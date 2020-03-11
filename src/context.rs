use crate::apu::APU;
use crate::config::CONTROLLER_POLL_INTERVAL;
use crate::controller::Controller;
use crate::cpu::CPU;
use crate::mapper::{AnyMemLocation, Mapped, Mapper0, Mapper1, Mapper3, Mapper4};
use crate::mem_location::*;
use crate::ppu::PPU;
use crate::rom::Rom;
use crate::sdl_system::SDLSystem;
use num_traits::FromPrimitive;
use sdl2::audio::AudioSpecDesired;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use std::cell::RefCell;
use std::path::PathBuf;
use std::io::Write;

pub struct Context {
    pub ppu: RefCell<PPU>,
    pub apu: RefCell<APU>,
    pub cpu: RefCell<CPU>,
    pub controller: RefCell<Controller>,
    pub sdl_system: RefCell<SDLSystem>,
    pub mapper: Box<dyn Mapped>,
    pub hit_breakpoint: bool,
    pub cycle: usize,
    pub cpu_pause: RefCell<usize>,
    controller_poll_timer: usize,
}

impl Context {
    pub fn new(rom: Rom, savedata: Option<Vec<u8>>) -> Context {
        let mapper: Box<dyn Mapped> = if rom.mapper == 0 {
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

            Box::new(Mapper0::new(rom.prg_rom, chr))
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
            Box::new(Mapper1::new(rom.prg_rom, savedata, chr, chr_is_rom))
        } else if rom.mapper == 3 {
            Box::new(Mapper3::new(rom.prg_rom, rom.chr_rom))
        } else if rom.mapper == 4 {
            Box::new(Mapper4::new(rom.prg_rom, rom.chr_rom, rom.prg_ram_len))
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
            mapper,
            cycle: 0,
            cpu_pause: RefCell::new(0),
            ppu: RefCell::new(PPU::new(sdl_system.canvas().default_pixel_format())),
            cpu: RefCell::new(CPU::new()),
            apu: RefCell::new(apu),
            controller: RefCell::new(Controller::new()),
            sdl_system: RefCell::new(sdl_system),
            controller_poll_timer: CONTROLLER_POLL_INTERVAL,
        }
    }

    pub fn cpu_address(&self, addr: u16) -> AnyMemLocation {
        match addr {
            0x0000..=0x1FFF => CPURamLoc {
                cpu: &self.cpu,
                addr: (addr % 0x0800),
            }
            .into(),
            0x2000..=0x3FFF => PPURegister {
                context: self,
                ppu: &self.ppu,
                reg: PPURegInt::from_usize(((addr - 0x2000) % 0x8) as usize).unwrap(),
            }
            .into(),
            0x4000..=0x4013 => APURegister {
                apu: &self.apu,
                register: (addr - 0x4000) as usize,
            }
            .into(),
            0x4014..=0x4014 => PPURegister {
                context: self,
                ppu: &self.ppu,
                reg: PPURegInt::from_u8(14).unwrap(),
            }
            .into(),
            0x4015..=0x4015 => APURegister {
                apu: &self.apu,
                register: 0x15,
            }
            .into(),
            0x4016..=0x4016 => CTLRegister {
                ctl: &self.controller,
                register: 0,
            }
            .into(),
            0x4017..=0x4017 => APURegister {
                apu: &self.apu,
                register: 0x17,
            }
            .into(),
            0x4018..=0x401F => unimplemented!(
                "Access to normally disabled APU or IO register at {:#04X}",
                addr
            ),
            0x4020..=0xFFFF => self.mapper.mem_cpu(addr, self),
        }
    }

    pub fn ppu_address(&self, addr: u16) -> AnyMemLocation {
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
    }

    pub fn read(&self, addr: u16) -> u8 {
        self.cpu_address(addr).read()
    }

    pub fn write(&self, addr: u16, value: u8) {
        self.cpu_address(addr).write(value)
    }

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

            self.controller.borrow_mut().update_from_keys(&key_events);
        }

        if !self.hit_breakpoint || should_run {
            if *self.cpu_pause.borrow() != 0 {
                let c = *self.cpu_pause.borrow();
                self.apu.borrow_mut().next(c, self, &self.cpu.borrow());
                self.ppu.borrow_mut().next(c, self, &self.cpu.borrow());
                self.cycle += c;
                *self.cpu_pause.borrow_mut() = 0;
            }

            //let instr = cpu::instruction::ARCH[self.cpu.borrow().read(self.cpu.borrow().pc, self) as usize].as_ref().unwrap();

            let cycles = self.cpu.borrow_mut().next(self);
            self.cycle += cycles;

            //println!("... with starting PPU position {}, {}", self.ppu.borrow().pixel(), self.ppu.borrow().scanline());

            self.apu
                .borrow_mut()
                .next(cycles as usize, self, &self.cpu.borrow());
            self.ppu
                .borrow_mut()
                .next(cycles as usize, self, &self.cpu.borrow());

            //println!("and ending PPU position {}, {}", self.ppu.borrow().pixel(), self.ppu.borrow().scanline());

            //println!("Mode: {:?}, cycles: {}", instr.mode, cycles);

            // TODO: ???????????????????
            if !(self.ppu.borrow().pixel() == 2 && self.ppu.borrow().scanline() == 241)
                && self.ppu.borrow_mut().nmi_falling()
            {
                self.cpu.borrow_mut().trigger_nmi(self);
                self.cycle += 7;

                self.apu.borrow_mut().next(7, self, &self.cpu.borrow());
                self.ppu.borrow_mut().next(7, self, &self.cpu.borrow());
            }
        }

        false
    }

    pub fn save(&self, path: PathBuf) -> Result<(), String> {
        if path.exists() {
            let backup_path = path.with_extension("save.bak");
            std::fs::copy(&path, backup_path).map_err(|e| format!("Failed to copy file: {}", e))?;
        }

        let mut result = [0; 0x2000];

        for (idx, res) in result.iter_mut().enumerate() {
            *res = self.mapper.mem_cpu((0x6000 + idx) as u16, &self).read();
        }

        let mut savefile = std::fs::File::create(path).map_err(|e| format!("Failed to create file: {}", e))?;

        let bytes_written = savefile.write(&result).map_err(|e| format!("Failed to write file: {}", e))?;

        if bytes_written != result.len() {
            Err(format!("Expected {:#X} bytes to be written, instead got {:#X}", result.len(), bytes_written))
        } else {
            Ok(())
        }
    }
}
