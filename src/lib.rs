#![feature(bool_to_option)]

// TODO: Figure out why I still need
#![allow(unused_parens)]

#[macro_use]
extern crate num_derive;

extern crate arrayvec;
extern crate sdl2;

pub mod rom;
pub mod ppu;
pub mod apu;
pub mod controller;
pub mod mapper;

use rom::Rom;
use ppu::PPU;
use apu::APU;
use mapper::MappedLocation;
use controller::Controller;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::num::Wrapping;
use sdl2::keyboard::Keycode;
use sdl2::event::Event;
use sdl2::{Sdl, VideoSubsystem, EventPump};
use sdl2::render::WindowCanvas;
use mapper::{Mapped, Mapper0, Mapper1};

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

type Arch = HashMap<u8, Instruction>;

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
        let mut canvas = window.into_canvas().build().unwrap();
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
            MemLocation::Rom(val) => panic!("Attempt to write to ROM at {:#06?}", addr),
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
            match self.state {
                State::Reset => {
                    self.pc = (self.read(self.pc + 1) as u16) << 8 | self.read(self.pc) as u16;
                    println!("Reset vector was {:#06X}", self.pc);
                    self.state = State::Run;
                },
                State::Irq => {
                    unimplemented!("IRQ not implemented");
                },
                State::Nmi |
                State::Run => {
                    let id = self.read(self.pc);
                    let instr = instrs.get(&id).unwrap_or_else(|| {self.print_stack(); panic!("Instruction {:#04X} does not exist", id) });
                    
                    // A starts as 0x0F
                    // writes to mmc at $8000, then shifts right
                    //self.print_instr(instr);
                    instr.run(self);
                },
            }

            /* TODO: Actually count the number of cycles properly */
            self.ppu.borrow_mut().next(3, &mut self.sdl_system, &mut (*self.mapper.borrow_mut()));

            if self.ppu.borrow_mut().nmi_falling() {
                self.trigger_nmi();
            }
        }
    }

    fn print_instr(&self, instr: &Instruction) {
        print!("[PC: {:#06X}] A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X} {}:", self.pc, self.acc, self.x, self.y, self.status, self.sp, instr.opcode);
        for i in 0..instr.length {
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

pub struct Instruction {
    pub opcode: String,
    pub length: usize,
    pub code: Box<dyn Fn(&mut Context)>,
}

impl Instruction {
    pub fn run(&self, ctx: &mut Context) {
        (self.code)(ctx);
        ctx.pc += self.length as u16;
    }
}

macro_rules! raw_inst {
	($v:ident $name:ident $val:tt $len:literal $exc:expr) => (
		assert!($v.insert(
            $val,
            crate::Instruction {
                opcode: stringify!($name).to_string(),
                length: $len,
                code: ::std::boxed::Box::new($exc),
            }
        ).is_none(), "Duplicate instruction {:#X}", $val);
	);
    ($v:ident $name:ident $val:tt $len:literal) => (
        assert!($v.insert(
                $val,
                crate::Instruction {
                    opcode: stringify!($name).to_string(),
                    length: $len, 
                    code: ::std::boxed::Box::new(|_| unimplemented!("Instruction {} ({:#X}) not implemented", stringify!($name), $val))
                }
        ).is_none(), "Duplicate instruction {:#X}", $val);
    );
}

macro_rules! inst {
    ($v:ident $name:ident $val:literal abs $($exc:expr)?) => (
		raw_inst!($v $name $val 3 $(|ctx: &mut crate::Context| { let addr = ctx.read_wide_nowrap(ctx.pc + 1); ($exc)(ctx, addr) })?)
    );
    ($v:ident $name:ident $val:tt abs,X $($exc:expr)?) => (
		raw_inst!($v $name $val 3 $(|ctx: &mut crate::Context| { let addr = (Wrapping(ctx.read_wide(ctx.pc + 1)) + Wrapping(ctx.x as u16)).0; ($exc)(ctx, addr) })?)
    );
    ($v:ident $name:ident $val:tt abs,Y $($exc:expr)?) => (
		raw_inst!($v $name $val 3 $(|ctx: &mut crate::Context| { let addr = (Wrapping(ctx.read_wide(ctx.pc + 1)) + Wrapping(ctx.y as u16)).0; ($exc)(ctx, addr) })?)
    );
    ($v:ident $name:ident $val:tt imm $($exc:expr)?) => (
		raw_inst!($v $name $val 2 $(|ctx: &mut crate::Context| { let val = ctx.read(ctx.pc + 1); ($exc)(ctx, val) })?)
    );
    ($v:ident $name:ident $val:tt ind $($exc:expr)?) => (
		raw_inst!($v $name $val 3 $(|ctx: &mut crate::Context| { let addr = ctx.read_wide(ctx.pc + 1); ($exc)(ctx, addr) })?)
    );
    ($v:ident $name:ident $val:tt X,ind ind,Y $($exc:expr)?) => (
        inst!($v $name $val X,ind $($exc)?);
        inst!($v $name ($val | 0x10) ind,Y $($exc)?);
    );
    ($v:ident $name:ident $val:tt X,ind $($exc:expr)?) => (
		raw_inst!($v $name $val 2 $(|ctx: &mut crate::Context| { let addr = ctx.read_wide((Wrapping(ctx.read(ctx.pc + 1)) + Wrapping(ctx.x)).0 as u16); ($exc)(ctx, addr) })?)
    );
    ($v:ident $name:ident $val:tt ind,Y $($exc:expr)?) => (
		raw_inst!($v $name $val 2 $(|ctx: &mut crate::Context| {
                let zpg_addr = ctx.read(ctx.pc + 1);
                let mut effective_addr = Wrapping(ctx.read_wide(zpg_addr as u16));
                effective_addr += Wrapping(ctx.y as u16);
                ($exc)(ctx, effective_addr.0)
            })?
        )
    );
    ($v:ident $name:ident $val:tt rel $($exc:expr)?) => (
		raw_inst!($v $name $val 2 $(|ctx: &mut crate::Context| { let offset = ctx.read(ctx.pc + 1); ($exc)(ctx, offset) })?)
    );
    ($v:ident $name:ident $val:tt zpg zpg,X $($exc:expr)?) => (
        inst!($v $name $val zpg $($exc)?);
        inst!($v $name ($val | 0x10) zpg,X $($exc)?);
    );
    ($v:ident $name:ident $val:tt zpg $($exc:expr)?) => (
		raw_inst!($v $name $val 2 $(|ctx: &mut crate::Context| { ($exc)(ctx, ctx.read(ctx.pc + 1) as u16) })?)
    );
    ($v:ident $name:ident $val:tt zpg,X $($exc:expr)?) => (
		raw_inst!($v $name $val 2 $(|ctx: &mut crate::Context| { ($exc)(ctx, (Wrapping(ctx.read(ctx.pc + 1)) + Wrapping(ctx.x)).0 as u16) })?)
    );
    ($v:ident $name:ident $val:tt zpg,Y $($exc:expr)?) => (
		raw_inst!($v $name $val 2 $(|ctx: &mut crate::Context| { ($exc)(ctx, (Wrapping(ctx.read(ctx.pc + 1)) + Wrapping(ctx.y)).0 as u16) })?)
    );
    ($v:ident $name:ident $val:tt $($exc:expr)?) => (
		raw_inst!($v $name $val 1 $($exc)?)
	);
}

macro_rules! inst_list {
    ($(
        { $($in:tt)+ }
    )*)
        =>
        (pub mod instructions {
            use super::*;
			pub fn gen_excs() -> std::collections::HashMap<u8, crate::Instruction> {
				let mut excs = std::collections::HashMap::new();
				$(
					inst!(excs $($in)+);
				)*

				excs
			}
        });
}

const BRANCH: &'static dyn Fn(&mut Context, u8, bool) = &|ctx: &mut Context, offset: u8, cond: bool| {
    if cond { 
        let neg = offset >> 7 != 0;
        if neg {
            ctx.pc -= (!offset + 1) as u16
        }
        else {
            ctx.pc += offset as u16
        }
    }
};

const COMPARE: &'static dyn Fn(&mut Context, u8, u8) = &|ctx: &mut Context, lhs: u8, rhs: u8| {
    let diff = (Wrapping(lhs) - Wrapping(rhs)).0;
    match lhs.cmp(&rhs) {
        Ordering::Less => {
            ctx.set_neg(diff >> 7 != 0);
            ctx.set_zero(false);
            ctx.set_carry(false);
        },
        Ordering::Equal => {
            ctx.set_neg(false);
            ctx.set_zero(true);
            ctx.set_carry(true);
        },
        Ordering::Greater => {
            ctx.set_neg(diff >> 7 != 0);
            ctx.set_zero(false);
            ctx.set_carry(true);
        }
    }
};

const ADD: &'static dyn Fn(&mut Context, u8) = &|ctx: &mut Context, rhs: u8| {
    let lhs = ctx.acc;
    let res1 = lhs as u16 + rhs as u16 + ctx.get_carry() as u16;
    let result = (res1 % 0x100) as u8;
    let did_carry = res1 & 0x100 != 0;
    let did_overflow = (lhs ^ result) & (rhs ^ result) & 0x80 != 0;
    ctx.update_flags(result);
    ctx.set_carry(did_carry);
    ctx.set_overflow(did_overflow);
    ctx.acc = result;
};

const SUB: &'static dyn Fn(&mut Context, u8) = &|ctx: &mut Context, rhs: u8| {
    let lhs = ctx.acc;
    let res1 = lhs as u16 + (!rhs) as u16 + ctx.get_carry() as u16;
    let result = (res1 % 0x100) as u8;
    let did_carry = res1 & 0x100 != 0;
    let did_overflow = (lhs ^ result) & (!rhs ^ result) & 0x80 != 0;
    ctx.update_flags(result);
    ctx.set_carry(did_carry);
    ctx.set_overflow(did_overflow);
    ctx.acc = result;
};

const ROR: &'static dyn Fn(&mut Context, u8) -> u8 = &|ctx: &mut Context, mut value: u8| {
    let carry_out = value & 1 != 0;
    value >>= 1;
    value |= (ctx.get_carry() as u8) << 7;
    ctx.update_flags(value);
    ctx.set_carry(carry_out);
    value
};

const ROL: &'static dyn Fn(&mut Context, u8) -> u8 = &|ctx: &mut Context, mut value: u8| {
    let carry_out = value & 0x80 != 0;
    value <<= 1;
    value |= ctx.get_carry() as u8;
    ctx.update_flags(value);
    ctx.set_carry(carry_out);
    value
};

const BIT: &'static dyn Fn(&mut Context, u8) = &|ctx: &mut Context, mut value: u8| {
    ctx.set_neg(value & 0x80 != 0);
    ctx.set_overflow(value & 0x40 != 0);
    ctx.set_zero(ctx.acc & value == 0);
};

const AAX: &'static dyn Fn(&mut Context, u16) = &|ctx: &mut Context, addr: u16| {
    let result = ctx.acc & ctx.x;
    ctx.write(addr, result);
};

const DCP: &'static dyn Fn(&mut Context, u16) = &|ctx: &mut Context, addr: u16| {
    ctx.write(addr, (Wrapping(ctx.read(addr)) - Wrapping(1)).0);
    COMPARE(ctx, ctx.acc, ctx.read(addr));
};

const ISC: &'static dyn Fn(&mut Context, u16) = &|ctx: &mut Context, addr: u16| {
    ctx.write(addr, (Wrapping(ctx.read(addr)) + Wrapping(1)).0);
    SUB(ctx, ctx.read(addr));
};

const SLO: &'static dyn Fn(&mut Context, u16) = &|ctx: &mut Context, addr: u16| {
    let mut m = ctx.read(addr);
    let c = m >> 7;
    m <<= 1;
    ctx.write(addr, m); 
    ctx.acc |= m;
    ctx.update_flags(ctx.acc);
    ctx.set_carry(c != 0);
};

const RLA: &'static dyn Fn(&mut Context, u16) = &|ctx: &mut Context, addr: u16| {
    let result = ROL(ctx, ctx.read(addr));
    ctx.write(addr, result);
    ctx.acc &= result;
    ctx.update_flags(ctx.acc);
};

const SRE: &'static dyn Fn(&mut Context, u16) = &|ctx: &mut Context, addr: u16| {
    let mut m = ctx.read(addr);
    let carry = m & 1 != 0;
    m >>= 1;
    ctx.write(addr, m);
    ctx.set_carry(carry);
    ctx.acc ^= m;
    ctx.update_flags(ctx.acc);
};

const RRA: &'static dyn Fn(&mut Context, u16) = &|ctx: &mut Context, addr: u16| {
    let result = ROR(ctx, ctx.read(addr));
    ctx.write(addr, result);
    ADD(ctx, result);
};

const NOP: &'static dyn Fn(&mut Context) = &|_: &mut Context| {};

inst_list! {
    { BRK 0x00 }
    { BPL 0x10 rel (|ctx: &mut crate::Context, offset: u8| BRANCH(ctx, offset, !ctx.get_neg())) }
    { JSR 0x20 abs (|ctx: &mut crate::Context, addr: u16| { ctx.push(((ctx.pc + 2) >> 8) as u8); ctx.push(((ctx.pc + 2) & 0xFF) as u8); ctx.pc = addr - 3; }) }
    { BMI 0x30 rel (|ctx: &mut crate::Context, offset: u8| BRANCH(ctx, offset,  ctx.get_neg())) }
    { RTI 0x40 (|ctx: &mut crate::Context| { ctx.status = (ctx.pop() & !(0b01 << 4)) | 0b10 << 4; ctx.pc = ctx.pop() as u16; ctx.pc |= (ctx.pop() as u16) << 8; ctx.pc -= 1; }) }
    { BVC 0x50 rel (|ctx: &mut crate::Context, offset: u8| BRANCH(ctx, offset, !ctx.get_overflow())) }
    { RTS 0x60 (|ctx: &mut crate::Context| { let lo = ctx.pop() as u16; let hi = ctx.pop() as u16; ctx.pc = hi << 8 | lo; }) }
    { BVS 0x70 rel (|ctx: &mut crate::Context, offset: u8| BRANCH(ctx, offset,  ctx.get_overflow())) }
    { DOP 0x80 imm (|ctx: &mut crate::Context, imm: u8| { }) }
    { BCC 0x90 rel (|ctx: &mut crate::Context, offset: u8| BRANCH(ctx, offset, !ctx.get_carry())) }
    { LDY 0xA0 imm (|ctx: &mut crate::Context, imm: u8| { ctx.y = imm; ctx.update_flags(ctx.y); }) }
    { BCS 0xB0 rel (|ctx: &mut crate::Context, offset: u8| BRANCH(ctx, offset,  ctx.get_carry())) }
    { CPY 0xC0 imm (|ctx: &mut crate::Context, imm: u8| { COMPARE(ctx, ctx.y, imm) }) }
    { BNE 0xD0 rel (|ctx: &mut crate::Context, offset: u8| BRANCH(ctx, offset, !ctx.get_zero())) }
    { CPX 0xE0 imm (|ctx: &mut crate::Context, imm: u8| { COMPARE(ctx, ctx.x, imm) }) }
    { BEQ 0xF0 rel (|ctx: &mut crate::Context, offset: u8| BRANCH(ctx, offset,  ctx.get_zero())) }
    { ORA 0x01 X,ind ind,Y (|ctx: &mut crate::Context, addr: u16| { ctx.acc |= ctx.read(addr); ctx.update_flags(ctx.acc); }) }
    { AND 0x21 X,ind ind,Y (|ctx: &mut crate::Context, addr: u16| { ctx.acc &= ctx.read(addr); ctx.update_flags(ctx.acc); }) }
    { EOR 0x41 X,ind ind,Y (|ctx: &mut crate::Context, addr: u16| { ctx.acc ^= ctx.read(addr); ctx.update_flags(ctx.acc); }) }
    { ADC 0x61 X,ind ind,Y (|ctx: &mut crate::Context, addr: u16| { ADD(ctx, ctx.read(addr)) }) }
    { STA 0x81 X,ind ind,Y (|ctx: &mut crate::Context, addr: u16| { ctx.write(addr, ctx.acc); }) }
    { LDA 0xA1 X,ind ind,Y (|ctx: &mut crate::Context, addr: u16| { ctx.acc = ctx.read(addr); ctx.update_flags(ctx.acc); }) }
    { CMP 0xC1 X,ind ind,Y (|ctx: &mut crate::Context, addr: u16| { COMPARE(ctx, ctx.acc, ctx.read(addr)); }) }
    { SBC 0xE1 X,ind ind,Y (|ctx: &mut crate::Context, addr: u16| { SUB(ctx, ctx.read(addr)); }) }
    { LDX 0xA2 imm (|ctx: &mut crate::Context, imm: u8| { ctx.x = imm; ctx.update_flags(ctx.x); }) }
    { SLO 0x03 X,ind ind,Y SLO }
    { RLA 0x23 X,ind ind,Y RLA }
    { SRE 0x43 X,ind ind,Y SRE }
    { RRA 0x63 X,ind ind,Y RRA }
    { AAX 0x83 X,ind (|ctx: &mut crate::Context, addr: u16| { AAX(ctx, addr); }) }
    { LAX 0xA3 X,ind ind,Y (|ctx: &mut crate::Context, addr: u16| { ctx.acc = ctx.read(addr); ctx.x = ctx.acc; ctx.update_flags(ctx.x); }) }
    { DCP 0xC3 X,ind ind,Y (|ctx: &mut crate::Context, addr: u16| { DCP(ctx, addr); }) }
    { ISC 0xE3 X,ind ind,Y (|ctx: &mut crate::Context, addr: u16| { ISC(ctx, addr); }) }
    { DOP 0x04 zpg zpg,X (|_ctx: &mut crate::Context, _addr: u16| { }) }
    { BIT 0x24 zpg (|ctx: &mut crate::Context, addr: u16| { BIT(ctx, ctx.read(addr)); }) }
    { DOP 0x34 zpg,X (|_ctx: &mut crate::Context, _addr: u16| { }) }
    { DOP 0x44 zpg zpg,X (|_ctx: &mut crate::Context, _addr: u16| { }) }
    { DOP 0x64 zpg zpg,X (|_ctx: &mut crate::Context, _addr: u16| { }) }
    { STY 0x84 zpg zpg,X (|ctx: &mut crate::Context, addr: u16| { ctx.write(addr, ctx.y); }) }
    { LDY 0xA4 zpg zpg,X (|ctx: &mut crate::Context, addr: u16| { ctx.y = ctx.read(addr); ctx.update_flags(ctx.y); }) }
    { CPY 0xC4 zpg (|ctx: &mut crate::Context, addr: u16| { COMPARE(ctx, ctx.y, ctx.read(addr)); }) }
    { DOP 0xD4 zpg,X (|_ctx: &mut crate::Context, _addr: u16| { }) }
    { CPX 0xE4 zpg (|ctx: &mut crate::Context, addr: u16| { COMPARE(ctx, ctx.x, ctx.read(addr)); }) }
    { DOP 0xF4 zpg,X (|_ctx: &mut crate::Context, _addr: u16| { }) }
    { ORA 0x05 zpg zpg,X (|ctx: &mut crate::Context, addr: u16| { ctx.acc |= ctx.read(addr); ctx.update_flags(ctx.acc) }) }
    { AND 0x25 zpg zpg,X (|ctx: &mut crate::Context, addr: u16| { ctx.acc &= ctx.read(addr); ctx.update_flags(ctx.acc); }) }
    { EOR 0x45 zpg zpg,X (|ctx: &mut crate::Context, addr: u16| { ctx.acc ^= ctx.read(addr); ctx.update_flags(ctx.acc) }) }
    { ADC 0x65 zpg zpg,X (|ctx: &mut crate::Context, addr: u16| { ADD(ctx, ctx.read(addr)); }) }
    { STA 0x85 zpg zpg,X (|ctx: &mut crate::Context, addr: u16| { ctx.write(addr, ctx.acc); }) }
    { LDA 0xA5 zpg zpg,X (|ctx: &mut crate::Context, addr: u16| { ctx.acc = ctx.read(addr); ctx.update_flags(ctx.acc) }) }
    { CMP 0xC5 zpg zpg,X (|ctx: &mut crate::Context, addr: u16| { COMPARE(ctx, ctx.acc, ctx.read(addr)); }) }
    { SBC 0xE5 zpg zpg,X (|ctx: &mut crate::Context, addr: u16| { SUB(ctx, ctx.read(addr)); }) }
    { ASL 0x06 zpg zpg,X (|ctx: &mut crate::Context, addr: u16| { let mut m = ctx.read(addr); let c = m >> 7; m <<= 1; ctx.update_flags(m); ctx.set_carry(c != 0); ctx.write(addr, m); }) }
    { ROL 0x26 zpg zpg,X (|ctx: &mut crate::Context, addr: u16| { let val = ROL(ctx, ctx.read(addr)); ctx.write(addr, val); }) }
    { LSR 0x46 zpg zpg,X (|ctx: &mut crate::Context, addr: u16| { let mut m = ctx.read(addr); let carry = m & 1 != 0; m >>= 1; ctx.update_flags(m); ctx.write(addr, m); ctx.set_carry(carry) }) }
    { ROR 0x66 zpg zpg,X (|ctx: &mut crate::Context, addr: u16| { let val = ROR(ctx, ctx.read(addr)); ctx.write(addr, val); }) }
    { STX 0x86 zpg (|ctx: &mut crate::Context, addr: u16| { ctx.write(addr, ctx.x); }) }
    { STX 0x96 zpg,Y (|ctx: &mut crate::Context, addr: u16| { ctx.write(addr, ctx.x); }) }
    { LDX 0xA6 zpg (|ctx: &mut crate::Context, addr: u16| { ctx.x = ctx.read(addr); ctx.update_flags(ctx.x) }) }
    { LDX 0xB6 zpg,Y (|ctx: &mut crate::Context, addr: u16| { ctx.x = ctx.read(addr); ctx.update_flags(ctx.x) }) }
    { DEC 0xC6 zpg zpg,X (|ctx: &mut crate::Context, addr: u16| { ctx.write(addr, (Wrapping(ctx.read(addr)) - Wrapping(1)).0); ctx.update_flags(ctx.read(addr)); }) }
    { INC 0xE6 zpg zpg,X (|ctx: &mut crate::Context, addr: u16| { ctx.write(addr, (Wrapping(ctx.read(addr)) + Wrapping(1)).0); ctx.update_flags(ctx.read(addr)); }) }
    { SLO 0x07 zpg zpg,X SLO }
    { RLA 0x27 zpg zpg,X RLA }
    { SRE 0x47 zpg zpg,X SRE }
    { RRA 0x67 zpg zpg,X RRA }
    { AAX 0x87 zpg AAX }
    { AAX 0x97 zpg,Y AAX }
    { LAX 0xA7 zpg (|ctx: &mut crate::Context, addr: u16| { ctx.acc = ctx.read(addr); ctx.x = ctx.acc; ctx.update_flags(ctx.x); }) }
    { LAX 0xB7 zpg,Y (|ctx: &mut crate::Context, addr: u16| { ctx.acc = ctx.read(addr); ctx.x = ctx.acc; ctx.update_flags(ctx.x); }) }
    { DCP 0xC7 zpg zpg,X DCP }
    { ISC 0xE7 zpg zpg,X ISC }
    { PHP 0x08 (|ctx: &mut crate::Context| { ctx.push(ctx.status | (0b11 << 4)) }) }
    { CLC 0x18 (|ctx: &mut crate::Context| { ctx.set_carry(false); }) }
    { PLP 0x28 (|ctx: &mut crate::Context| { ctx.status = (ctx.pop() & !(0b01 << 4)) | (0b10 << 4); }) }
    { SEC 0x38 (|ctx: &mut crate::Context| { ctx.set_carry(true); }) }
    { PHA 0x48 (|ctx: &mut crate::Context| { ctx.push(ctx.acc); }) }
    { PLA 0x68 (|ctx: &mut crate::Context| { ctx.acc = ctx.pop(); ctx.update_flags(ctx.acc); }) }
    { SEI 0x78 (|ctx: &mut crate::Context| { ctx.status |= 1 << 2; }) }
    { DEY 0x88 (|ctx: &mut crate::Context| { ctx.y = (Wrapping(ctx.y) - Wrapping(1)).0; ctx.update_flags(ctx.y); }) }
    { TYA 0x98 (|ctx: &mut crate::Context| { ctx.acc = ctx.y; ctx.update_flags(ctx.acc); }) }
    { TAY 0xA8 (|ctx: &mut crate::Context| { ctx.y = ctx.acc; ctx.update_flags(ctx.y); }) }
    { CLV 0xB8 (|ctx: &mut crate::Context| { ctx.set_overflow(false); }) }
    { INY 0xC8 (|ctx: &mut crate::Context| { ctx.y = (Wrapping(ctx.y) + Wrapping(1)).0; ctx.update_flags(ctx.y); }) }
    { CLD 0xD8 (|ctx: &mut crate::Context| { ctx.status &= !(1 << 3); }) }
    { INX 0xE8 (|ctx: &mut crate::Context| { ctx.x = (Wrapping(ctx.x) + Wrapping(1)).0; ctx.update_flags(ctx.x); }) }
    { SED 0xF8 (|ctx: &mut crate::Context| { ctx.status |= 1 << 3; }) }
    { ORA 0x09 imm (|ctx: &mut crate::Context, imm: u8| { ctx.acc |= imm; ctx.update_flags(ctx.acc); }) }
    { ORA 0x19 abs,Y (|ctx: &mut crate::Context, addr: u16| { ctx.acc |= ctx.read(addr); ctx.update_flags(ctx.acc); }) }
    { AND 0x29 imm (|ctx: &mut crate::Context, imm: u8| { ctx.acc &= imm; ctx.update_flags(ctx.acc); }) }
    { AND 0x39 abs,Y (|ctx: &mut crate::Context, addr: u16| { ctx.acc &= ctx.read(addr); ctx.update_flags(ctx.acc); }) }
    { EOR 0x49 imm (|ctx: &mut crate::Context, imm: u8| { ctx.acc ^= imm; ctx.update_flags(ctx.acc); }) }
    { EOR 0x59 abs,Y (|ctx: &mut crate::Context, addr: u16| { ctx.acc ^= ctx.read(addr); ctx.update_flags(ctx.acc); }) }
    { ADC 0x69 imm (|ctx: &mut crate::Context, imm: u8| { ADD(ctx, imm); }) }
    { ADC 0x79 abs,Y (|ctx: &mut crate::Context, addr: u16| { ADD(ctx, ctx.read(addr)); }) }
    { STA 0x99 abs,Y (|ctx: &mut crate::Context, addr: u16| { ctx.write(addr, ctx.acc); }) }
    { LDA 0xA9 imm (|ctx: &mut crate::Context, imm: u8| { ctx.acc = imm; ctx.update_flags(ctx.acc); }) }
    { LDA 0xB9 abs,Y (|ctx: &mut crate::Context, addr: u16| { ctx.acc = ctx.read(addr); ctx.update_flags(ctx.acc); }) }
    { CMP 0xC9 imm (|ctx: &mut crate::Context, imm: u8| { COMPARE(ctx, ctx.acc, imm) }) }
    { CMP 0xD9 abs,Y (|ctx: &mut crate::Context, addr: u16| { COMPARE(ctx, ctx.acc, ctx.read(addr)) }) }
    { SBC 0xE9 imm (|ctx: &mut crate::Context, imm: u8| { SUB(ctx, imm); }) }
    { SBC 0xF9 abs,Y (|ctx: &mut crate::Context, addr: u16| { SUB(ctx, ctx.read(addr)); }) }
    { ASL 0x0A (|ctx: &mut crate::Context| { let c = ctx.acc >> 7; ctx.acc <<= 1; ctx.update_flags(ctx.acc); ctx.set_carry(c != 0); }) }
    { NOP 0x1A NOP }
    { ROL 0x2A (|ctx: &mut crate::Context| { ctx.acc = ROL(ctx, ctx.acc); }) }
    { NOP 0x3A NOP }
    { LSR 0x4A (|ctx: &mut crate::Context| { let carry = ctx.acc & 1 != 0; ctx.acc >>= 1; ctx.update_flags(ctx.acc); ctx.set_carry(carry) }) }
    { NOP 0x5A NOP }
    { ROR 0x6A (|ctx: &mut crate::Context| { ctx.acc = ROR(ctx, ctx.acc); }) }
    { NOP 0x7A NOP }
    { TXA 0x8A (|ctx: &mut crate::Context| { ctx.acc = ctx.x; ctx.update_flags(ctx.acc); }) }
    { TXS 0x9A (|ctx: &mut crate::Context| { ctx.sp = ctx.x }) }
    { TAX 0xAA (|ctx: &mut crate::Context| { ctx.x = ctx.acc; ctx.update_flags(ctx.x); }) }
    { TSX 0xBA (|ctx: &mut crate::Context| { ctx.x = ctx.sp; ctx.update_flags(ctx.x); }) }
    { DEX 0xCA (|ctx: &mut crate::Context| { ctx.x = (Wrapping(ctx.x) - Wrapping(1)).0; ctx.update_flags(ctx.x); }) }
    { NOP 0xDA NOP }
    { NOP 0xEA NOP }
    { NOP 0xFA NOP }
    { SLO 0x1B abs,Y SLO }
    { RLA 0x3B abs,Y RLA }
    { SRE 0x5B abs,Y SRE }
    { RRA 0x7B abs,Y RRA }
    { DCP 0xDB abs,Y DCP }
    { SBC 0xEB imm (|ctx: &mut crate::Context, imm: u8| { SUB(ctx, imm); }) }
    { ISC 0xFB abs,Y (|ctx: &mut crate::Context, addr: u16| { ISC(ctx, addr); }) }
    { TOP 0x0C abs (|_ctx: &mut crate::Context, _addr: u16| { }) }
    { TOP 0x1C abs,X (|_ctx: &mut crate::Context, _addr: u16| { }) }
    { BIT 0x2C abs (|ctx: &mut crate::Context, addr: u16| { BIT(ctx, ctx.read(addr)); }) }
    { TOP 0x3C abs,X (|_ctx: &mut crate::Context, _addr: u16| { }) }
    { JMP 0x4C abs (|ctx: &mut crate::Context, addr: u16| { ctx.pc = addr - 3; }) }
    { TOP 0x5C abs,X (|_ctx: &mut crate::Context, _addr: u16| { }) }
    { JMP 0x6C ind (|ctx: &mut crate::Context, addr: u16| { ctx.pc = ctx.read_wide(addr) - 3; }) }
    { TOP 0x7C abs,X (|_ctx: &mut crate::Context, _addr: u16| { }) }
    { STY 0x8C abs (|ctx: &mut crate::Context, addr: u16| { ctx.write(addr, ctx.y); }) }
    { LDY 0xAC abs (|ctx: &mut crate::Context, addr: u16| { ctx.y = ctx.read(addr); ctx.update_flags(ctx.y); }) }
    { LDY 0xBC abs,X (|ctx: &mut crate::Context, addr: u16| { ctx.y = ctx.read(addr); ctx.update_flags(ctx.y); }) }
    { CPY 0xCC abs (|ctx: &mut crate::Context, addr: u16| { COMPARE(ctx, ctx.y, ctx.read(addr)); }) }
    { TOP 0xDC abs,X (|_ctx: &mut crate::Context, _addr: u16| { }) }
    { CPX 0xEC abs (|ctx: &mut crate::Context, addr: u16| { COMPARE(ctx, ctx.x, ctx.read(addr)); }) }
    { TOP 0xFC abs,X (|_ctx: &mut crate::Context, _addr: u16| { }) }
    { ORA 0x0D abs (|ctx: &mut crate::Context, addr: u16| { ctx.acc |= ctx.read(addr); ctx.update_flags(ctx.acc); }) }
    { ORA 0x1D abs,X (|ctx: &mut crate::Context, addr: u16| { ctx.acc |= ctx.read(addr); ctx.update_flags(ctx.acc); }) }
    { AND 0x2D abs (|ctx: &mut crate::Context, addr: u16| { ctx.acc &= ctx.read(addr); ctx.update_flags(ctx.acc); }) }
    { AND 0x3D abs,X (|ctx: &mut crate::Context, addr: u16| { ctx.acc &= ctx.read(addr); ctx.update_flags(ctx.acc); }) }
    { EOR 0x4D abs (|ctx: &mut crate::Context, addr: u16| { ctx.acc ^= ctx.read(addr); ctx.update_flags(ctx.acc); }) }
    { EOR 0x5D abs,X (|ctx: &mut crate::Context, addr: u16| { ctx.acc ^= ctx.read(addr); ctx.update_flags(ctx.acc); }) }
    { ADC 0x6D abs (|ctx: &mut crate::Context, addr: u16| { ADD(ctx, ctx.read(addr)); }) }
    { ADC 0x7D abs,X (|ctx: &mut crate::Context, addr: u16| { ADD(ctx, ctx.read(addr)); }) }
    { STA 0x8D abs (|ctx: &mut crate::Context, addr: u16| { ctx.write(addr, ctx.acc) }) }
    { STA 0x9D abs,X (|ctx: &mut crate::Context, addr: u16| { ctx.write(addr, ctx.acc) }) }
    { LDA 0xAD abs (|ctx: &mut crate::Context, addr: u16| { ctx.acc = ctx.read(addr); ctx.update_flags(ctx.acc) }) }
    { LDA 0xBD abs,X (|ctx: &mut crate::Context, addr: u16| { ctx.acc = ctx.read(addr); ctx.update_flags(ctx.acc) }) }
    { CMP 0xCD abs (|ctx: &mut crate::Context, addr: u16| { COMPARE(ctx, ctx.acc, ctx.read(addr)) }) }
    { CMP 0xDD abs,X (|ctx: &mut crate::Context, addr: u16| { COMPARE(ctx, ctx.acc, ctx.read(addr)) }) }
    { SBC 0xED abs (|ctx: &mut crate::Context, addr: u16| { SUB(ctx, ctx.read(addr)); }) }
    { SBC 0xFD abs,X (|ctx: &mut crate::Context, addr: u16| { SUB(ctx, ctx.read(addr)); }) }
    { ASL 0x0E abs (|ctx: &mut crate::Context, addr: u16| { let mut m = ctx.read(addr); let c = m >> 7; m <<= 1; ctx.update_flags(m); ctx.set_carry(c != 0); ctx.write(addr, m); }) }
    { ASL 0x1E abs,X (|ctx: &mut crate::Context, addr: u16| { let mut m = ctx.read(addr); let c = m >> 7; m <<= 1; ctx.update_flags(m); ctx.set_carry(c != 0); ctx.write(addr, m); }) }
    { ROL 0x2E abs (|ctx: &mut crate::Context, addr: u16| { let val = ROL(ctx, ctx.read(addr)); ctx.write(addr, val); }) }
    { ROL 0x3E abs,X (|ctx: &mut crate::Context, addr: u16| { let val = ROL(ctx, ctx.read(addr)); ctx.write(addr, val); }) }
    { LSR 0x4E abs (|ctx: &mut crate::Context, addr: u16| { let mut m = ctx.read(addr); let carry = m & 1 != 0; m >>= 1; ctx.update_flags(m); ctx.write(addr, m); ctx.set_carry(carry) }) }
    { LSR 0x5E abs,X (|ctx: &mut crate::Context, addr: u16| { let mut m = ctx.read(addr); let carry = m & 1 != 0; m >>= 1; ctx.update_flags(m); ctx.write(addr, m); ctx.set_carry(carry) }) }
    { ROR 0x6E abs (|ctx: &mut crate::Context, addr: u16| { let val = ROR(ctx, ctx.read(addr)); ctx.write(addr, val); }) }
    { ROR 0x7E abs,X (|ctx: &mut crate::Context, addr: u16| { let val = ROR(ctx, ctx.read(addr)); ctx.write(addr, val); }) }
    { STX 0x8E abs (|ctx: &mut crate::Context, addr: u16| { ctx.write(addr, ctx.x); }) }
    { LDX 0xAE abs (|ctx: &mut crate::Context, addr: u16| { ctx.x = ctx.read(addr); ctx.update_flags(ctx.x) }) }
    { LDX 0xBE abs,Y (|ctx: &mut crate::Context, addr: u16| { ctx.x = ctx.read(addr); ctx.update_flags(ctx.x) }) }
    { DEC 0xCE abs (|ctx: &mut crate::Context, addr: u16| { ctx.write(addr, (Wrapping(ctx.read(addr)) - Wrapping(1)).0); ctx.update_flags(ctx.read(addr)); }) }
    { DEC 0xDE abs,X (|ctx: &mut crate::Context, addr: u16| { ctx.write(addr, (Wrapping(ctx.read(addr)) - Wrapping(1)).0); ctx.update_flags(ctx.read(addr)); }) }
    { INC 0xEE abs (|ctx: &mut crate::Context, addr: u16| { ctx.write(addr, (Wrapping(ctx.read(addr)) + Wrapping(1)).0); ctx.update_flags(ctx.read(addr)); }) }
    { INC 0xFE abs,X (|ctx: &mut crate::Context, addr: u16| { ctx.write(addr, (Wrapping(ctx.read(addr)) + Wrapping(1)).0); ctx.update_flags(ctx.read(addr)); }) }
    { SLO 0x0F abs SLO }
    { SLO 0x1F abs,X SLO }
    { RLA 0x2F abs RLA }
    { RLA 0x3F abs,X RLA }
    { SRE 0x4F abs SRE }
    { SRE 0x5F abs,X SRE }
    { RRA 0x6F abs RRA }
    { RRA 0x7F abs,X RRA }
    { AAX 0x8F abs (|ctx: &mut crate::Context, addr: u16| { AAX(ctx, addr); }) }
    { LAX 0xAF abs (|ctx: &mut crate::Context, addr: u16| { ctx.acc = ctx.read(addr); ctx.x = ctx.acc; ctx.update_flags(ctx.x); }) }
    { LAX 0xBF abs,Y (|ctx: &mut crate::Context, addr: u16| { ctx.acc = ctx.read(addr); ctx.x = ctx.acc; ctx.update_flags(ctx.x); }) }
    { DCP 0xCF abs DCP }
    { DCP 0xDF abs,X DCP }
    { ISC 0xEF abs ISC }
    { ISC 0xFF abs,X ISC }
    /* to be continued... */
}
