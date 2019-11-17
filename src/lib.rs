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
pub mod ppu;
pub mod rom;
pub mod config;
mod mem_location;
mod sdl_system;

use mapper::AnyMemLocation;
use mem_location::*;
use sdl_system::SDLSystem;

use apu::{APUAudio, APU};
use controller::Controller;
use cpu::CPU;
use mapper::{Mapped, Mapper0, Mapper1, Mapper3, Mapper4};
use num_traits::cast::FromPrimitive;
use ppu::PPU;
use rom::Rom;
use std::cell::RefCell;
use std::io::{Read, Write};
use config::{Config, CONTROLLER_POLL_INTERVAL};
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::audio::AudioSpecDesired;

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
            let mut chr = rom.chr_rom;
            let chr_is_rom = chr.len() != 0;
            if !chr_is_rom {
                if rom.chr_ram_len == 0 {
                    println!("Assuming CHR RAM size of 8KiB");
                    chr.resize(0x2000, 0);
                }
                else {
                    println!("CHR RAM len: {:#X}", rom.chr_ram_len);
                    chr.resize(rom.chr_ram_len, 0);
                }
            }
            else {
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
            ppu: RefCell::new(PPU::new(&sdl_system.canvas().default_pixel_format())),
            cpu: RefCell::new(CPU::new()),
            apu: RefCell::new(apu),
            controller: RefCell::new(Controller::new()),
            sdl_system: RefCell::new(sdl_system),
            controller_poll_timer: CONTROLLER_POLL_INTERVAL,
        }

    }

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
            0x4020..=0xFFFF => self.mapper.mem_cpu(addr, self),
        }
    }

    pub fn ppu_address<'a>(&'a self, addr: u16) -> AnyMemLocation<'a> {
        let addr = addr % 0x4000;
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

    pub fn next(&mut self) -> bool {
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
            if *self.cpu_pause.borrow() != 0 {
                let c = *self.cpu_pause.borrow();
                self.apu.borrow_mut().next(c, self, &self.cpu.borrow());
                self.ppu
                    .borrow_mut()
                    .next(c, self, &self.cpu.borrow());
                self.cycle += c;
                *self.cpu_pause.borrow_mut() = 0;
            }

            //let instr = cpu::instruction::ARCH[self.cpu.borrow().read(self.cpu.borrow().pc, self) as usize].as_ref().unwrap();

            let cycles = self.cpu.borrow_mut().next(self);
            self.cycle += cycles;

            //println!("... with starting PPU position {}, {}", self.ppu.borrow().pixel(), self.ppu.borrow().scanline());

            self.apu.borrow_mut().next(cycles as usize, self, &self.cpu.borrow());
            self.ppu
                .borrow_mut()
                .next(cycles as usize, self, &self.cpu.borrow());

            //println!("and ending PPU position {}, {}", self.ppu.borrow().pixel(), self.ppu.borrow().scanline());

            //println!("Mode: {:?}, cycles: {}", instr.mode, cycles);

            // TODO: ???????????????????
            if !(self.ppu.borrow().pixel() == 2 && self.ppu.borrow().scanline() == 241) {
                if self.ppu.borrow_mut().nmi_falling() {
                    self.cpu.borrow_mut().trigger_nmi(self);
                    self.cycle += 7;

                    self.apu.borrow_mut().next(7, self, &self.cpu.borrow());
                    self.ppu
                        .borrow_mut()
                        .next(7, self, &self.cpu.borrow());
                }
            }
        }

        return false;
    }
}

pub fn run(config: &Config) {
    let rom = Rom::new(&config.rom_path).unwrap_or_else(|err| {
        println!("Error loading ROM: {}", err);
        std::process::exit(1)
    });

    println!(
        "ROM has mapper {} and submapper {}",
        rom.mapper, rom.submapper
    );
    println!(
        "PRG ROM has size {:#X}, CHR ROM has size {:#X}, and misc ROM has size {}",
        rom.prg_rom.len(),
        rom.chr_rom.len(),
        rom.misc_rom.len()
    );

    let battery_backed = rom.battery_backed;

    let mem_path = config.rom_path.with_extension("save");

    let savedata = if mem_path.exists() {
        let mut savedata = Vec::new();
        std::fs::File::open(&mem_path).unwrap().read_to_end(&mut savedata).unwrap();
        Some(savedata)
    } else {
        None
    };

    let mut ctx = Context::new(rom, savedata);

    if config.verify {
        let log_path = config.rom_path.with_extension("log");

        let expected: Vec<_> = std::fs::read_to_string(log_path)
            .unwrap()
            .lines()
            .map(parse_log_line)
            .collect();

        {
            let mut cpu = ctx.cpu.borrow_mut();
            cpu.pc = expected[0].0;
            cpu.sp = expected[0].5;
            cpu.state = cpu::State::Run;
            cpu.status = expected[0].4;
            ctx.cycle = expected[0].6;
        }

        for expected in expected.iter() {
            /* Program Counter, Acc, X, Y, Status, Stack Pointer */
            if ctx.cycle == 27403 || ctx.cycle == 57183 {
                ctx.cpu.borrow_mut().acc = 128;
                ctx.cpu.borrow_mut().status = 164;
                ctx.ppu.borrow_mut().status &= !0x80;
            }

            {
                let mut cpu = ctx.cpu.borrow_mut();
                if cpu.read(cpu.pc - 1, &ctx) == 0x40 && cpu.read(cpu.pc - 2, &ctx) == 0x16 && cpu.read(cpu.pc - 3, &ctx) != 0x8D {
                    cpu.acc = expected.1;
                    cpu.status = expected.4;
                }

                if cpu.read(cpu.pc - 1, &ctx) == 0x20 && cpu.read(cpu.pc - 2, &ctx) == 0x02 && cpu.read(cpu.pc - 3, &ctx) == 0xAD && (1..=12).contains(&ctx.ppu.borrow().pixel()) && ctx.ppu.borrow().scanline() == 261 {
                    cpu.acc &= !64;
                    let acc = cpu.acc;
                    cpu.update_flags(acc);
                }
            }

            match ctx.cycle {
                118191 |
                118215 |
                118239 |
                118263 |
                118287 => {
                    ctx.cpu.borrow_mut().acc = 64;
                    ctx.cpu.borrow_mut().update_flags(64);
                }
                _ => {}
            }

            let actual = {
                let cpu = ctx.cpu.borrow();
                let ppu = ctx.ppu.borrow();
                let val = (cpu.pc, cpu.acc, cpu.x, cpu.y, cpu.status, cpu.sp, ctx.cycle, ppu.pixel(), ppu.scanline());
                val
            };
            assert_eq!(
                expected,
                &actual,
                "Frame: {:?}", ctx.ppu.borrow().frame()
            );

            while !ctx.next() {}
        }
    } else {
        while !ctx.next() {}

        if battery_backed {
            if mem_path.exists() {
                let backup_path = mem_path.to_owned().with_extension("save.bak");
                std::fs::copy(&mem_path, backup_path).unwrap();
            }

            let mut result = [0; 0x2000];

            for i in 0..result.len() {
                result[i] = ctx.mapper.mem_cpu((0x6000 + i) as u16, &ctx).read();
            }

            let mut savefile = std::fs::File::create(mem_path).unwrap();

            assert_eq!(savefile.write(&result).unwrap(), 0x2000);
        }
    }
}

fn parse_log_line(line: &str) -> (u16, u8, u8, u8, u8, u8, usize, usize, usize) {
    let pc = u16::from_str_radix(&line[0..4], 16).unwrap();
    let a = u8::from_str_radix(&line[50..52], 16).unwrap();
    let x = u8::from_str_radix(&line[55..57], 16).unwrap();
    let y = u8::from_str_radix(&line[60..62], 16).unwrap();
    let p = u8::from_str_radix(&line[65..67], 16).unwrap();
    let s = u8::from_str_radix(&line[71..73], 16).unwrap();
    let cyc = usize::from_str_radix(&line[90..], 10).unwrap();

    let pixel = usize::from_str_radix(&line[78..81].trim(), 10).unwrap();
    let scanline = usize::from_str_radix(&line[82..85].trim(), 10).unwrap();

    (pc, a, x, y, p, s, cyc, pixel, scanline)
}
