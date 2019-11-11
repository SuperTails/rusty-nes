#![feature(bool_to_option)] // TODO: Figure out why I still need
#![allow(unused_parens)]

#[macro_use]
extern crate num_derive;

#[macro_use]
extern crate bitfield;

#[macro_use]
extern crate lazy_static;

extern crate enum_dispatch;

extern crate arrayvec;
extern crate sdl2;

pub mod apu;
pub mod controller;
pub mod cpu;
pub mod mapper;
mod mem_location;
pub mod ppu;
pub mod rom;

use mapper::AnyMemLocation;
use mem_location::*;

use apu::{APUAudio, APU};
use controller::Controller;
use cpu::CPU;
use mapper::{Mapped, Mapper0, Mapper1, Mapper3};
use num_traits::cast::FromPrimitive;
use ppu::PPU;
use rom::Rom;
use sdl2::audio::{AudioDevice, AudioSpecDesired};
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::render::WindowCanvas;
use sdl2::{AudioSubsystem, EventPump, Sdl, VideoSubsystem};
use std::cell::RefCell;
use std::path::PathBuf;

/*
 * Mapping 0:
 *
 * PRG ROM - 16KiB or 32KiB
 *  (not bankswitched)
 * PRG RAM -  2KiB or 4KiB
 *  (not bankswitched)
 * CHR CAP -  8KiB
 *
 */

#[derive(Debug, Clone)]
pub struct Config {
    pub rom_path: PathBuf,
}

#[derive(Debug, Clone)]
pub struct ConfigParseError {
    error: String,
}

impl Config {
    pub fn from_args(args: &[String]) -> Result<Config, ConfigParseError> {
        if args.len() == 2 {
            Ok(Config {
                rom_path: args[1].clone().into(),
            })
        } else {
            Err(ConfigParseError {
                error: "Wrong number of arguments provided".to_string(),
            })
        }
    }
}

const CONTROLLER_POLL_INTERVAL: usize = 1000;

pub struct Context {
    pub ppu: RefCell<PPU>,
    pub apu: RefCell<APU>,
    pub cpu: RefCell<CPU>,
    pub controller: RefCell<Controller>,
    pub sdl_system: RefCell<SDLSystem>,
    pub mapper: Box<dyn Mapped>,
    pub hit_breakpoint: bool,
    pub cycle: usize,
    controller_poll_timer: usize,
}

impl From<Rom> for Context {
    fn from(rom: Rom) -> Context {
        let mapper: Box<dyn Mapped> = if rom.mapper == 0 {
            let chr = if rom.chr_rom.len() == 0 {
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
            Box::new(Mapper1::new(rom.prg_rom, [0; 0x2000].to_vec()))
        } else if rom.mapper == 3 {
            Box::new(Mapper3::new(rom.prg_rom, rom.chr_rom))
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
            ppu: RefCell::new(PPU::new(&sdl_system.canvas().default_pixel_format())),
            cpu: RefCell::new(CPU::new()),
            apu: RefCell::new(apu),
            controller: RefCell::new(Controller::new()),
            sdl_system: RefCell::new(sdl_system),
            controller_poll_timer: CONTROLLER_POLL_INTERVAL,
        }
    }
}

pub struct SDLSystem {
    pub ctx: Sdl,
    pub video_subsystem: VideoSubsystem,
    pub audio_subsystem: AudioSubsystem,
    pub audio_device: Option<AudioDevice<APUAudio>>,
    pub canvas: WindowCanvas,
    pub event_pump: EventPump,
}

impl SDLSystem {
    pub fn new() -> SDLSystem {
        let ctx = sdl2::init().unwrap();
        let video_subsystem = ctx.video().unwrap();
        let audio_subsystem = ctx.audio().unwrap();
        let window = video_subsystem
            .window("Terrible NES", 1024 + 256, 480)
            .position_centered()
            .build()
            .unwrap();
        let canvas = window.into_canvas().build().unwrap();
        let event_pump = ctx.event_pump().unwrap();
        SDLSystem {
            ctx,
            video_subsystem,
            audio_subsystem,
            canvas,
            event_pump,
            audio_device: None,
        }
    }

    pub fn present(&mut self) {
        self.canvas.present();
    }

    pub fn canvas(&mut self) -> &mut WindowCanvas {
        &mut self.canvas
    }
}

impl Context {
    pub fn cpu_address<'a>(&'a self, addr: u16) -> AnyMemLocation<'a> {
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
            0x4020..=0xFFFF => self.mapper.mem_cpu(addr),
        }
    }

    pub fn ppu_address<'a>(&'a self, addr: u16) -> AnyMemLocation<'a> {
        let addr = addr % 0x4000;
        /* TODO: Vertical/Horizontal mirroring */
        match addr {
            0x0000..=0x1FFF => self.mapper.mem_ppu(addr),
            0x2000..=0x3EFF => PPUNametable {
                ppu: &self.ppu,
                addr: self.mapper.map_nametable(addr) as usize,
            }
            .into(),
            /* TODO: Do this better */
            0x3F10 => PPUPalette {
                ppu: &self.ppu,
                addr: 0x0,
            }
            .into(),
            0x3F14 => PPUPalette {
                ppu: &self.ppu,
                addr: 0x4,
            }
            .into(),
            0x3F18 => PPUPalette {
                ppu: &self.ppu,
                addr: 0x8,
            }
            .into(),
            0x3F1C => PPUPalette {
                ppu: &self.ppu,
                addr: 0xC,
            }
            .into(),
            0x3F00..=0x3FFF => PPUPalette {
                ppu: &self.ppu,
                addr: ((addr - 0x3F00) % 0x20) as usize,
            }
            .into(),
            _ => unreachable!(),
        }
    }

    pub fn read(&self, addr: u16) -> u8 {
        self.cpu_address(addr).read()
    }

    pub fn write(&self, addr: u16, value: u8) {
        self.cpu_address(addr).write(value)
    }

    pub fn next(&mut self) {
        let mut should_run = false;

        if self.controller_poll_timer > 0 {
            self.controller_poll_timer -= 1;
        }
        else {
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
                        std::process::exit(0);
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
                    Event::KeyDown {
                        ..
                    } => {
                        key_events.push(event);
                    }
                    _ => {}
                }
            }

            self.controller.borrow_mut().update_from_keys(&key_events);
        }

        if !self.hit_breakpoint || should_run {
            let cycles = self.cpu.borrow_mut().next(self);
            self.cycle += cycles;

            self.apu.borrow_mut().next(cycles as usize, self, &self.cpu.borrow());
            self.ppu
                .borrow_mut()
                .next(cycles as usize, self, &self.cpu.borrow());

            if self.ppu.borrow_mut().nmi_falling() {
                self.cpu.borrow_mut().trigger_nmi(self);
            }
        }
    }
}
