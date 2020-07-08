use crate::apu::APU;
use crate::config::CONTROLLER_POLL_INTERVAL;
use crate::controller::Controller;
use crate::cpu::CPU;
use crate::mapper::{CpuMapper, Mapper0, Mapper1, Mapper3, Mapper4};
use crate::mem_location::*;
use crate::rom::Rom;
use crate::sdl_system::SDLSystem;
use rust_2c02::{PpuMapper, Display, PPU, PPURegInt};
use sdl2::audio::AudioSpecDesired;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::surface::Surface;
use sdl2::render::Canvas;
use std::time::Instant;
use std::cell::RefCell;
use std::path::PathBuf;
use std::io::Write;
use std::rc::Rc;
use std::convert::TryFrom;
use pixels::Pixels;

macro_rules! getter_body {
    ($self:expr, $addr:expr, $mapperfunc:ident, $func:ident, $($arg:expr)?) => {
        match $addr {
            0x0000..=0x1FFF => panic!(),
            0x2000..=0x3FFF => {
                PPURegister {
                    ppu: &mut $self.ppu/*.as_mut().unwrap()*/,
                    reg: PPURegInt::try_from((($addr - 0x2000) % 0x8) as u8).unwrap(),
                    mapper: &mut *$self.ppu_mapper.borrow_mut(),
                }
            }
            .$func($($arg)?),
            0x4000..=0x4013 => APURegister {
                apu: $self.apu.as_mut().unwrap(),
                register: ($addr - 0x4000) as usize,
            }
            .$func($($arg)?),
            0x4014..=0x4014 => panic!(),
            0x4015..=0x4015 => APURegister {
                apu: $self.apu.as_mut().unwrap(),
                register: 0x15,
            }
            .$func($($arg)?),
            0x4016..=0x4016 => CTLRegister {
                ctl: &mut $self.controller,
                register: 0,
            }
            .$func($($arg)?),
            0x4017..=0x4017 => APURegister {
                apu: $self.apu.as_mut().unwrap(),
                register: 0x17,
            }
            .$func($($arg)?),
            0x4018..=0x401F => unimplemented!(
                "Access to normally disabled APU or IO register at {:#04X}",
                $addr
            ),
            0x4020..=0xFFFF => {
                let cpu_mapper = $self.cpu_mapper.take().unwrap();
                let result = cpu_mapper.borrow_mut().$mapperfunc($addr, $($arg,)? $self);
                $self.cpu_mapper.replace(cpu_mapper);
                result
            }
        }
    };
}



pub struct SdlDisplay<'a> {
    //sdl_system: &'a mut SDLSystem,
    //surface: &'a mut Canvas<Surface<'static>>,
    pixels: &'a mut Pixels,
}

impl Display for SdlDisplay<'_> {
    fn set_pixel(&mut self, x: i32, y: i32, rgb: (u8, u8, u8)) {
        /*let surface = self.surface.surface_mut();

        let x = if 0 <= x && (x as u32) < surface.width() {
            x as usize
        } else {
            return
        };

        let y = if 0 <= y && (y as u32) < surface.height() {
            y as usize
        } else {
            return
        };

        let pitch = surface.pitch() as usize;
        let pixels = surface.without_lock_mut().unwrap();
        pixels[y * pitch + x * 4] = rgb.0;
        pixels[y * pitch + x * 4 + 1] = rgb.1;
        pixels[y * pitch + x * 4 + 2] = rgb.2;*/

        //self.surface.surface_mut().fill_rect(Rect::new(x, y, 1, 1), rgb.into()).unwrap();

        let x = if 0 <= x && (x as u32) < 1024 {
            x as usize
        } else {
            return
        };

        let y = if 0 <= y && (y as u32) < 512 {
            y as usize
        } else {
            return
        };

        let pixels = &mut self.pixels.get_frame()[(y * 1024 + x) * 4..][..4];

        pixels[0] = rgb.0;
        pixels[1] = rgb.1;
        pixels[2] = rgb.2;
    }

    fn draw_rect(&mut self, x: i32, y: i32, w: u32, h: u32, rgb: (u8, u8, u8)) {
        for x in x..=x + w as i32 {
            self.set_pixel(x, y, rgb);
            self.set_pixel(x, y + h as i32, rgb);
        }
        for y in y..=y + h as i32 {
            self.set_pixel(x, y, rgb);
            self.set_pixel(x + w as i32, y, rgb);
        }
    }
    
    fn commit(&mut self) {
        self.pixels.render().unwrap();
    }
}

pub struct CpuContext {
    pub ppu_mapper: Rc<RefCell<dyn PpuMapper>>,
    pub cpu_mapper: Option<Rc<RefCell<dyn CpuMapper<Context=CpuContext>>>>,
    pub ppu: PPU,
    pub apu: Option<APU>,
    pub controller: Controller,
    pub cycle: usize,
}

impl CpuContext {
    pub fn read(&mut self, addr: u16) -> u8 {
        getter_body! { self, addr, read_mem_cpu, read, }
    }

    pub fn write(&mut self, addr: u16, value: u8) {
        getter_body! { self, addr, write_mem_cpu, write, value }
    }
}

pub struct Context {
    pub cpu: CPU,
    pub inner: CpuContext,
    pub hit_breakpoint: bool,
    pub sdl_system: RefCell<SDLSystem>,
    pub pixels: Pixels,
    pub surface: Canvas<Surface<'static>>,
    controller_poll_timer: usize,
    pub prev_ppu_frame: usize,
    pub times: Vec<Instant>,
}

type CpuMapperPart<C> = Rc<RefCell<dyn CpuMapper<Context = C>>>;
type PpuMapperPart = Rc<RefCell<dyn PpuMapper>>;

fn to_dyn_pair<C, T: 'static + CpuMapper<Context=C> + PpuMapper>(mapper: T) ->
    (CpuMapperPart<C>, PpuMapperPart)
{
    let rc = Rc::new(RefCell::new(mapper));
    let rc2 = Rc::clone(&rc);
    (rc, rc2)
}

impl Context {
    pub fn new(rom: Rom, savedata: Option<Vec<u8>>) -> Context {
        let (cpu_mapper, ppu_mapper) = if rom.mapper == 0 {
            let chr = if rom.chr_rom.is_empty() {
                // TODO: Figure this out
                // let len = rom.chr_ram_len;
                let len = 0x2000;

                let mut chr = Vec::with_capacity(len);
                println!("CHR RAM length: {}", len);
                chr.resize(len, 0);
                chr
            } else {
                rom.chr_rom
            };

            to_dyn_pair(Mapper0::new(rom.prg_rom, chr))
        } else if rom.mapper == 1 {
            /*Box::new(RefCell::new(Mapper1::new(rom.prg_rom, rom.chr_rom)))*/
            let mut chr = rom.chr_rom;
            let chr_is_rom = !chr.is_empty();
            if !chr_is_rom {
                if rom.chr_ram_len == 0 {
                    println!("Assuming CHR RAM size of 8KiB");
                    chr.resize(0x2000, 0);
                } else {
                    println!("CHR RAM len: {:#X}", rom.chr_ram_len);
                    chr.resize(rom.chr_ram_len, 0);
                }
            } else {
                println!("Using CHR ROM of size {:#X}", chr.len());
            }
            to_dyn_pair(Mapper1::new(rom.prg_rom, savedata, chr, chr_is_rom))
        } else if rom.mapper == 3 {
            to_dyn_pair(Mapper3::new(rom.prg_rom, rom.chr_rom))
        } else if rom.mapper == 4 {
            to_dyn_pair(Mapper4::new(rom.prg_rom, rom.chr_rom, rom.prg_ram_len))
        } else {
            panic!()
        };

        let desired = AudioSpecDesired {
            freq: Some(16000),
            channels: Some(1),
            samples: None,
        };

        let (apu, apu_audio) = APU::new();

        let mut sdl_system = SDLSystem::new();
        sdl_system.audio_device = Some(
            sdl_system
                .audio_subsystem
                .open_playback(None, &desired, move |_spec| apu_audio)
                .unwrap(),
        );

        sdl_system.audio_device.as_ref().unwrap().resume();

        let pixels = Pixels::new(1024, 512, pixels::SurfaceTexture::new(1024, 512, pixels::wgpu::Surface::create(&sdl_system.window))).unwrap();

        Context {
            hit_breakpoint: false,
            cpu: CPU::new(),
            sdl_system: RefCell::new(sdl_system),
            pixels,
            surface: Surface::new(1024, 512, sdl2::pixels::PixelFormatEnum::RGBA32).unwrap().into_canvas().unwrap(),
            inner: CpuContext {
                ppu: PPU::new(),
                ppu_mapper,
                apu: Some(apu),
                cpu_mapper: Some(cpu_mapper),
                controller: Controller::new(),
                cycle: 0,
            },
            controller_poll_timer: CONTROLLER_POLL_INTERVAL,
            prev_ppu_frame: 0,
            times: Vec::new(),
        }
    }

    /*pub fn ppu_address(&self, addr: u16) -> AnyMemLocation {
        let addr = addr % 0x4000;
        match addr {
            0x0000..=0x1FFF => self.mapper.mem_ppu(addr),
            0x2000..=0x3EFF => PPUNametable {
                ppu: &self.ppu,
                addr: self.mapper.map_nametable(addr) as usize,
            }
            .into(),
            _ => PPUPalette {
                ppu: &self.ppu,
                addr: PPU::palette_idx(addr),
            }
            .into(),
        }
    }*/


    pub fn advance(&mut self) -> bool {
        let mut should_run = false;

        if self.controller_poll_timer > 0 {
            self.controller_poll_timer -= 1;
        } else {
            self.controller_poll_timer = CONTROLLER_POLL_INTERVAL;

            let mut key_events = Vec::new();

            for event in self.sdl_system.borrow_mut().event_pump.poll_iter() {
                key_events.push(event.clone());
                match event {
                    Event::Quit { .. }
                    | Event::KeyDown {
                        keycode: Some(Keycode::Escape),
                        ..
                    } => {
                        return true;
                    }
                    Event::KeyDown {
                        keycode: Some(Keycode::C),
                        ..
                    } => {
                        self.hit_breakpoint = false;
                    }
                    Event::KeyDown {
                        keycode: Some(Keycode::Space),
                        ..
                    } => {
                        should_run = true;
                    }
                    Event::KeyDown { .. } => {
                        key_events.push(event);
                    }
                    _ => {}
                }
            }

            self.inner.controller.update_from_keys(&key_events);
        }

        if !self.hit_breakpoint || should_run {
            //let instr = cpu::instruction::ARCH[self.cpu.borrow().read(self.cpu.borrow().pc, self) as usize].as_ref().unwrap();

            let cycles = self.cpu.next(&mut self.inner);
            self.inner.cycle += cycles;

            //println!("... with starting PPU position {}, {}", self.ppu.borrow().pixel(), self.ppu.borrow().scanline());

            let mut apu = self.inner.apu.take().unwrap();
            apu.next(cycles as usize, &mut self.inner, &mut self.cpu);
            self.inner.apu.replace(apu);

            self.inner.ppu.next(cycles as usize, &mut *self.inner.ppu_mapper.borrow_mut(), &mut SdlDisplay { pixels: &mut self.pixels, });

            if self.inner.ppu.frame() != self.prev_ppu_frame {
                self.prev_ppu_frame = self.inner.ppu.frame();
                self.inner.ppu.debug_render(&mut *self.inner.ppu_mapper.borrow_mut(), &mut SdlDisplay { pixels: &mut self.pixels });

                if self.times.len() == 10 {
                    self.times.remove(0);
                }

                self.times.push(Instant::now());

                if self.times.len() == 10 {               
                    let frame_duration = (self.times[9] - self.times[0]) / 10;
                    println!("Framerate: {}", 0.016666 / frame_duration.as_secs_f64());
                }
            }

            //println!("and ending PPU position {}, {}", self.ppu.borrow().pixel(), self.ppu.borrow().scanline());

            //println!("Mode: {:?}, cycles: {}", instr.mode, cycles);


            // TODO: ???????????????????

            let do_if = {
                let ppu = &mut self.inner.ppu;
                !(ppu.pixel() == 2 && ppu.scanline() == 241) && ppu.nmi_falling()
            };

            if do_if {
                self.cpu.trigger_nmi(&mut self.inner);
                self.inner.cycle += 7;

                let mut apu = self.inner.apu.take().unwrap();
                apu.next(7, &mut self.inner, &mut self.cpu);
                self.inner.apu.replace(apu);

                self.inner.ppu.next(7, &mut *self.inner.ppu_mapper.borrow_mut(), &mut SdlDisplay { pixels: &mut self.pixels });
            }
        }

        false
    }

    pub fn save(&mut self, path: PathBuf) -> Result<(), String> {
        if path.exists() {
            let backup_path = path.with_extension("save.bak");
            std::fs::copy(&path, backup_path).map_err(|e| format!("Failed to copy file: {}", e))?;
        }

        let mut result = [0; 0x2000];

        let cpu_mapper = self.inner.cpu_mapper.take().unwrap();
        for (idx, res) in result.iter_mut().enumerate() {
            *res = cpu_mapper.borrow_mut().read_mem_cpu((0x6000 + idx) as u16, &mut self.inner);
        }
        self.inner.cpu_mapper.replace(cpu_mapper);

        let mut savefile = std::fs::File::create(path).map_err(|e| format!("Failed to create file: {}", e))?;

        let bytes_written = savefile.write(&result).map_err(|e| format!("Failed to write file: {}", e))?;

        if bytes_written != result.len() {
            Err(format!("Expected {:#X} bytes to be written, instead got {:#X}", result.len(), bytes_written))
        } else {
            Ok(())
        }
    }
}
