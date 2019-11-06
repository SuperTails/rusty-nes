#![feature(bool_to_option)]

// TODO: Figure out why I still need
#![allow(unused_parens)]

#[macro_use]
extern crate num_derive;

#[macro_use]
extern crate bitfield;

extern crate arrayvec;
extern crate sdl2;

pub mod rom;
pub mod ppu;
pub mod apu;
pub mod controller;
pub mod mapper;
pub mod instruction;

use rom::Rom;
use ppu::PPU;
use apu::APU;
use mapper::MappedLocation;
use controller::Controller;
use std::cell::RefCell;
use std::num::Wrapping;
use instruction::Instruction;
use sdl2::keyboard::Keycode;
use sdl2::event::Event;
use sdl2::{Sdl, VideoSubsystem, EventPump};
use sdl2::render::WindowCanvas;
use mapper::{Mapped, Mapper0, Mapper1, Mapper3};
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
            Ok(Config { rom_path: args[1].clone().into() })
        }
        else {
            Err(ConfigParseError { error: "Wrong number of arguments provided".to_string() })
        }
    }
}

type Arch = [Option<Instruction>; 256];

pub enum State {
    Reset,
    Irq,
    Nmi,
    Run,
}

pub struct Context {
	pub status: u8,
	pub pc: u16,
	pub acc: u8,
	pub x: u8,
	pub y: u8,
	pub sp: u8,
    pub native_ram: [u8; 0x800],
    pub state: State,
    pub ppu: RefCell<PPU>,
    pub apu: RefCell<APU>,
    pub controller: RefCell<Controller>,
    pub sdl_system: SDLSystem,
    pub mapper: Box<RefCell<dyn Mapped>>,
    pub hit_breakpoint: bool,
    pub cycle: usize,
}

impl From<Rom> for Context {
    fn from(rom: Rom) -> Context {
        let mapper: Box<RefCell<dyn Mapped>> = 
        if rom.mapper == 0 {
            Box::new(RefCell::new(Mapper0::new(rom.prg_rom, rom.chr_rom)))
        }
        else if rom.mapper == 1 {
            /*Box::new(RefCell::new(Mapper1::new(rom.prg_rom, rom.chr_rom)))*/
            Box::new(RefCell::new(Mapper1::new(rom.prg_rom, [0; 0x2000].to_vec())))
        }
        else if rom.mapper == 3 {
            Box::new(RefCell::new(Mapper3::new(rom.prg_rom, rom.chr_rom)))
        }
        else {
            panic!()
        };

        let ppu = RefCell::new(PPU::new());
        Context { 
            ppu,
            hit_breakpoint: false,
            mapper,
			status: 0,
			pc: 0xFFFC,
			acc: 0,
			x: 0,
			y: 0,
			sp: 0,
            cycle: 0,
            native_ram: [0; 0x800],
            state: State::Reset,
            apu: RefCell::new(APU::new()),
            controller: RefCell::new(Controller::new()),
            sdl_system: SDLSystem::new(),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
enum MemLocation<T> {
    Ram(T),
    Rom(T),
    Ppu(u8),
    Apu(u8),
    Controller(u8),
    Mapped(MappedLocation<T>),
}

impl<'a, T> From<MemLocation<&'a mut T>> for MemLocation<&'a T> {
    fn from(mem: MemLocation<&'a mut T>) -> MemLocation<&'a T> {
        match mem {
            MemLocation::Ram(t) => MemLocation::Ram(&(*t)),
            MemLocation::Rom(t) => MemLocation::Rom(&(*t)),
            MemLocation::Ppu(t) => MemLocation::Ppu(t),
            MemLocation::Apu(t) => MemLocation::Apu(t),
            MemLocation::Controller(t) => MemLocation::Controller(t),
            MemLocation::Mapped(t) => MemLocation::Mapped(t.into()),
        }
    }
}

// TODO: See if there is a better way to do this
macro_rules! get_mem {
    ($addr:ident, $mapper:ident, $ctx:ident, $($ref_t:tt)+) => ({
        match $addr {
            0x0000..=0x1FFF => MemLocation::Ram($($ref_t)+ $ctx.native_ram[($addr % 0x0800) as usize]),
            0x2000..=0x3FFF => MemLocation::Ppu((($addr - 0x2000) % 0x8) as u8),
            0x4000..=0x4013 => MemLocation::Apu(($addr - 0x4000) as u8),
            0x4014..=0x4014 => MemLocation::Ppu(14),
            0x4015..=0x4015 => MemLocation::Apu(15),
            0x4016..=0x4016 => MemLocation::Controller(0),
            0x4017..=0x4017 => MemLocation::Apu(17),
            0x4018..=0x401F => unimplemented!("Access to normally disabled APU or IO register at {:#04X}", $addr),
            0x4020..=0xFFFF => MemLocation::Mapped($mapper.mem_cpu($addr).into()),
        }
    });
}

pub struct SDLSystem {
    pub ctx: Sdl,
    pub video_subsystem: VideoSubsystem,
    pub canvas: WindowCanvas,
    pub event_pump: EventPump,
}

impl SDLSystem {
    pub fn new() -> SDLSystem {
        let ctx = sdl2::init().unwrap();
        let video_subsystem = ctx.video().unwrap();
        let window = video_subsystem.window("Terrible NES", 256 * 4, 240 * 2).position_centered().build().unwrap();
        let canvas = window.into_canvas().build().unwrap();
        let event_pump = ctx.event_pump().unwrap();
        SDLSystem { ctx, video_subsystem, canvas, event_pump }
    }

    pub fn present(&mut self) {
        self.canvas.present();
    }

    pub fn canvas(&mut self) -> &mut WindowCanvas {
        &mut self.canvas
    }
}



impl Context {
    pub fn push(&mut self, value: u8) {
        self.write(0x0100 + self.sp as u16, value);
        self.sp = (Wrapping(self.sp) - Wrapping(1)).0;
    }

    pub fn pop(&mut self) -> u8 {
        self.sp = (Wrapping(self.sp) + Wrapping(1)).0;
        let res = self.read(0x0100 + self.sp as u16);
        res
    }

    pub fn read_m(&self, addr: u16, mapper: &mut dyn Mapped) -> u8 {
        match get_mem!(addr, mapper, self, &) {
            MemLocation::Rom(val) => *val,
            MemLocation::Ram(val) => *val,
            MemLocation::Ppu(reg) => self.ppu.borrow_mut().read(reg, &mut *mapper),
            MemLocation::Apu(reg) => self.apu.borrow_mut().read(reg),
            MemLocation::Controller(reg) => self.controller.borrow_mut().read(reg),
            MemLocation::Mapped(_) => mapper.read_cpu(addr),
        }
    }

    pub fn read(&self, addr: u16) -> u8 {
        let mut mapper = self.mapper.borrow_mut();
        self.read_m(addr, &mut *mapper)
    }

    pub fn write(&mut self, addr: u16, value: u8) {
        let mut mapper = self.mapper.borrow_mut();
        let location = get_mem!(addr, mapper, self, &mut);
        match location {
            MemLocation::Rom(_val) => panic!("Attempt to write to ROM at {:#06?}", addr),
            MemLocation::Ram(val) => *val = value,
            MemLocation::Ppu(reg) => self.ppu.borrow_mut().write(reg, value, self, &mut *mapper),
            MemLocation::Apu(reg) => self.apu.borrow_mut().write(reg, value),
            MemLocation::Controller(reg) => self.controller.borrow_mut().write(reg, value),
            MemLocation::Mapped(_) => mapper.write_cpu(addr, value),
        }
    }

    pub fn read_wide_nowrap(&self, addr: u16) -> u16 {
        (self.read(addr + 1) as u16) << 8 | self.read(addr) as u16
    }

    pub fn read_wide(&self, addr: u16) -> u16 {
        let page = addr & 0xFF00;
        let idx = (Wrapping((addr & 0xFF) as u8) + Wrapping(1)).0 as u16;
        let addr_next = page | idx;
        (self.read(addr_next) as u16) << 8 | self.read(addr) as u16
    }

    fn set_status(&mut self, v: bool, b: u8) {
        assert!(b < 8);
        self.status &= !(1 << b);
        self.status |= (v as u8) << b;
    }

    fn get_status(&self, b: u8) -> bool {
        (self.status >> b) & 1 != 0
    }

	pub fn set_neg(&mut self, v: bool) {
        self.set_status(v, 7);
	}

    pub fn get_neg(&self) -> bool {
        self.get_status(7)
    }

    pub fn set_overflow(&mut self, v: bool) {
        self.set_status(v, 6);
    }

    pub fn get_overflow(&self) -> bool {
        self.get_status(6)
    }

    pub fn set_carry(&mut self, v: bool) {
        self.set_status(v, 0);
    }

    pub fn get_carry(&self) -> bool {
        self.get_status(0)
    }

    pub fn set_zero(&mut self, v: bool) {
        self.set_status(v, 1);
    }

    pub fn get_zero(&self) -> bool {
        self.get_status(1)
    }

    pub fn update_flags(&mut self, value: u8) {
        self.set_neg(value & (1 << 7) != 0);
        self.set_zero(value == 0);
    }

    pub fn trigger_nmi(&mut self) {
        self.push((self.pc >> 8) as u8);
        self.push((self.pc & 0xFF) as u8);
        self.push(self.status | 0b10 << 4);

        // Disable interrupts
        self.status |= 0b100;

        self.pc = (self.read(0xFFFB) as u16) << 8 | self.read(0xFFFA) as u16;
        self.state = State::Nmi;
        
        println!("NMI triggered, PC is now {:#06X} ({:#04X})", self.pc, self.read(self.pc));
    }

	pub fn try_irq(&mut self) -> bool {
		if self.status & 0b100 != 0 {
			return false;
		}

        self.push((self.pc >> 8) as u8);
        self.push((self.pc & 0xFF) as u8);
        self.push(self.status | 0b10 << 4);

        // Disable interrupts
        self.status |= 0b100;

        self.pc = (self.read(0xFFFF) as u16) << 8 | self.read(0xFFFE) as u16;
        self.state = State::Irq;

        true
	}

    pub fn next(&mut self, instrs: &Arch) {
        let mut key_events = Vec::new();

        let mut should_run = false;

        for event in self.sdl_system.event_pump.poll_iter() {
            key_events.push(event.clone());
            match event {
                Event::Quit {..} |
                Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
                    std::process::exit(0);
                },
                Event::KeyDown { keycode: Some(Keycode::C), .. } => {
                    self.hit_breakpoint = false;
                }
                Event::KeyDown { keycode: Some(Keycode::Space), .. } => {
                    should_run = true;
                }
                _ => {},
            }
        }

        self.controller.borrow_mut().update_from_keys(&key_events);

        if !self.hit_breakpoint || should_run { 
            let cycles = 
            match self.state {
                State::Reset => {
                    self.pc = (self.read(self.pc + 1) as u16) << 8 | self.read(self.pc) as u16;
                    println!("Reset vector was {:#06X}", self.pc);
                    self.state = State::Run;
                    1
                },
                State::Irq | 
                State::Nmi |
                State::Run => {
                    let id = self.read(self.pc);
                    let instr = instrs[id as usize].as_ref().unwrap_or_else(|| {self.print_stack(); panic!("Instruction {:#04X} does not exist", id) });

                    // TODO: Fix this
                    if id == 0x00 && self.try_irq() {
                        1
                    }
                    else {
                        //self.print_instr(instr);
                        instr.run(self)
                    }
                },
            };

            self.cycle += cycles as usize;

            /* TODO: Actually count the number of cycles properly */
            self.ppu.borrow_mut().next(cycles as usize, &mut self.sdl_system, &mut (*self.mapper.borrow_mut()));

            if self.ppu.borrow_mut().nmi_falling() {
                self.trigger_nmi();
            }
        }
    }

    fn print_instr(&self, instr: &Instruction) {
        print!("[PC: {:#06X}] A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X} {} {: >4?}:", self.pc, self.acc, self.x, self.y, self.status, self.sp, instr.opcode, instr.mode);
        for i in 0..instr.mode.len() {
            print!("{:#04X} ", self.read(self.pc + i as u16));
        }
        println!("");
    }

    fn print_stack(&self) {
        println!("SP: {:#04X}, Stack:", self.sp);
        for i in self.sp..=255 {
            println!("{:#04X}: {:#04X}", i, self.read(i as u16));
        }
        println!("Error code (If testing): {:#04X}, {:#04X}", self.read(2), self.read(3));
    }
}

