use crate::apu::APU;
use crate::config::CONTROLLER_POLL_INTERVAL;
use crate::controller::Controller;
use crate::cpu::CPU;
use crate::mapper::{CpuMapper, Mapper0, Mapper1, Mapper3, Mapper4};
use crate::mem_location::*;
use crate::rom::Rom;
use crate::sdl_system::SDLSystem;
use rust_2c02::{PpuMapper, Display, PPU};
use num_traits::FromPrimitive;
use sdl2::audio::AudioSpecDesired;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::surface::Surface;
use std::cell::RefCell;
use std::path::PathBuf;
use std::io::Write;
use std::rc::Rc;

macro_rules! getter_body {
    ($self:expr, $addr:expr, $mapperfunc:ident, $func:ident, $($arg:expr)?) => {
        match $addr {
            0x0000..=0x1FFF => panic!(),
            0x2000..=0x3FFF => {
                PPURegister {
                    ppu: $self.ppu.as_mut().unwrap(),
                    reg: PPURegInt::from_usize((($addr - 0x2000) % 0x8) as usize).unwrap(),
                    mapper: &mut *$self.ppu_mapper.as_ref().unwrap().borrow_mut(),
                }
            }
            .$func($($arg)?),
            0x4000..=0x4013 => APURegister {
                apu: $self.apu.as_mut().unwrap(),
                register: ($addr - 0x4000) as usize,
            }
            .$func($($arg)?),
            0x4014..=0x4014 => {
                PPURegister {
                    ppu: $self.ppu.as_mut().unwrap(),
                    reg: PPURegInt::from_u8(14).unwrap(),
                    mapper: &mut *$self.ppu_mapper.as_ref().unwrap().borrow_mut(),
                }
            }
            .$func($($arg)?),
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
    sdl_system: &'a mut SDLSystem,
    surface: &'a mut Surface<'static>,
}

impl Display for SdlDisplay<'_> {
    fn set_pixel(&mut self, x: i32, y: i32, rgb: (u8, u8, u8)) {
        self.surface.fill_rect(sdl2::rect::Rect::new(x * 2, y * 2, 2, 2), rgb.into()).unwrap();
    }

    fn draw_rect(&mut self, x: i32, y: i32, w: u32, h: u32, rgb: (u8, u8, u8)) {
        self.surface.fill_rect(sdl2::rect::Rect::new(x * 2, y * 2, w * 2, h * 2), rgb.into()).unwrap();
    }
    
    fn commit(&mut self) {
        let creator = self.sdl_system.canvas.texture_creator();
        let texture = creator.create_texture_from_surface(&self.surface).unwrap();
        let w = texture.query().width;
        let h = texture.query().height;
        self.sdl_system.canvas.copy(&texture, None, sdl2::rect::Rect::new(0, 0, w, h)).unwrap();
        self.sdl_system.present();
    }
}

pub struct CpuContext {
    pub ppu_mapper: Option<Rc<RefCell<dyn PpuMapper>>>,
    pub cpu_mapper: Option<Rc<RefCell<dyn CpuMapper<Context=CpuContext>>>>,
    pub ppu: Option<PPU>,
    pub apu: Option<APU>,
    pub controller: Controller,
    pub cpu_pause: usize,
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
    pub surface: Surface<'static>,
    controller_poll_timer: usize,
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

        Context {
            hit_breakpoint: false,
            cpu: CPU::new(),
            sdl_system: RefCell::new(sdl_system),
            surface: Surface::new(1024, 512, sdl2::pixels::PixelFormatEnum::RGBA32).unwrap(),
            inner: CpuContext {
                ppu: Some(PPU::new()),
                apu: Some(apu),
                cpu_mapper: Some(cpu_mapper),
                ppu_mapper: Some(ppu_mapper),
                controller: Controller::new(),
                cpu_pause: 0,
                cycle: 0,
            },
            controller_poll_timer: CONTROLLER_POLL_INTERVAL,
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
            if self.inner.cpu_pause != 0 {

                let mut apu = self.inner.apu.take().unwrap();
                apu.next(self.inner.cpu_pause, &mut self.inner, &mut self.cpu);
                self.inner.apu.replace(apu);

                let mut ppu = self.inner.ppu.take().unwrap();
                let ppu_mapper = self.inner.ppu_mapper.take().unwrap();
                ppu.next(self.inner.cpu_pause, &mut *ppu_mapper.borrow_mut(), &mut SdlDisplay { sdl_system: &mut self.sdl_system.borrow_mut(), surface: &mut self.surface }, &mut self.cpu, &mut self.inner);
                self.inner.ppu.replace(ppu);
                self.inner.ppu_mapper.replace(ppu_mapper);

                self.inner.cycle += self.inner.cpu_pause;
                self.inner.cpu_pause = 0;
            }

            //let instr = cpu::instruction::ARCH[self.cpu.borrow().read(self.cpu.borrow().pc, self) as usize].as_ref().unwrap();

            let cycles = self.cpu.next(&mut self.inner);
            self.inner.cycle += cycles;

            //println!("... with starting PPU position {}, {}", self.ppu.borrow().pixel(), self.ppu.borrow().scanline());

            let mut apu = self.inner.apu.take().unwrap();
            apu.next(cycles as usize, &mut self.inner, &mut self.cpu);
            self.inner.apu.replace(apu);

            let mut ppu = self.inner.ppu.take().unwrap();
            let ppu_mapper = self.inner.ppu_mapper.take().unwrap();
            ppu.next(cycles as usize, &mut *ppu_mapper.borrow_mut(), &mut SdlDisplay { sdl_system: &mut self.sdl_system.borrow_mut(), surface: &mut self.surface }, &mut self.cpu, &mut self.inner);
            self.inner.ppu.replace(ppu);
            self.inner.ppu_mapper.replace(ppu_mapper);

            //println!("and ending PPU position {}, {}", self.ppu.borrow().pixel(), self.ppu.borrow().scanline());

            //println!("Mode: {:?}, cycles: {}", instr.mode, cycles);


            // TODO: ???????????????????

            let do_if = {
                let ppu = self.inner.ppu.as_mut().unwrap();
                !(ppu.pixel() == 2 && ppu.scanline() == 241) && ppu.nmi_falling()
            };

            if do_if {
                self.cpu.trigger_nmi(&mut self.inner);
                self.inner.cycle += 7;

                let mut apu = self.inner.apu.take().unwrap();
                apu.next(7, &mut self.inner, &mut self.cpu);
                self.inner.apu.replace(apu);

                let mut ppu = self.inner.ppu.take().unwrap();
                let ppu_mapper = self.inner.ppu_mapper.take().unwrap();
                ppu.next(7, &mut *ppu_mapper.borrow_mut(), &mut SdlDisplay { sdl_system: &mut self.sdl_system.borrow_mut(), surface: &mut self.surface }, &mut self.cpu, &mut self.inner);
                self.inner.ppu_mapper.replace(ppu_mapper);
                self.inner.ppu.replace(ppu);
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
