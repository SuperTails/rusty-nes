pub mod instruction;

use instruction::Instruction;
use std::cell::RefCell;
use super::Context;

pub enum State {
    Reset,
    Irq,
    Nmi,
    Run,
}

pub struct CPU {
	pub status: RefCell<u8>,
	pub pc: u16,
	pub acc: RefCell<u8>,
	pub x: RefCell<u8>,
	pub y: RefCell<u8>,
	pub sp: RefCell<u8>,
    pub ram: [u8; 0x800],
    pub state: State,
}

impl CPU {
    pub fn new() -> CPU {
        CPU {
            status: RefCell::new(0),
            pc: 0xFFFC,
            acc: RefCell::new(0),
            x: RefCell::new(0),
            y: RefCell::new(0),
            sp: RefCell::new(0),
            ram: [0; 0x800],
            state: State::Reset,
        }
    }

    fn set_status(&self, v: bool, b: u8) {
        assert!(b < 8);
        let mut status = self.status.borrow_mut();
        *status &= !(1 << b);
        *status |= (v as u8) << b;
    }

    fn get_status(&self, b: u8) -> bool {
        (*self.status.borrow() >> b) & 1 != 0
    }

    pub fn read(&self, addr: u16, context: &Context) -> u8 {
        if addr < 0x2000 {
            self.ram[(addr % 0x800) as usize]
        }
        else {
            context.read(addr)
        }
    }

    pub fn write(&mut self, addr: u16, value: u8, context: &Context) {
        if addr < 0x2000 {
            self.ram[(addr % 0x800) as usize] = value;
        }
        else {
            context.write(addr, value);
        }
    }

    pub fn set_decimal(&self, v: bool) {
        self.set_status(v, 3);
    }

    pub fn get_decimal(&self) -> bool {
        self.get_status(3)
    }

    pub fn set_interrupt(&self, v: bool) {
        self.set_status(v, 2);
    }

    pub fn get_interrupt(&self) -> bool {
        self.get_status(2)
    }

	pub fn set_neg(&self, v: bool) {
        self.set_status(v, 7);
	}

    pub fn get_neg(&self) -> bool {
        self.get_status(7)
    }

    pub fn set_overflow(&self, v: bool) {
        self.set_status(v, 6);
    }

    pub fn get_overflow(&self) -> bool {
        self.get_status(6)
    }

    pub fn set_carry(&self, v: bool) {
        self.set_status(v, 0);
    }

    pub fn get_carry(&self) -> bool {
        self.get_status(0)
    }

    pub fn set_zero(&self, v: bool) {
        self.set_status(v, 1);
    }

    pub fn get_zero(&self) -> bool {
        self.get_status(1)
    }

    pub fn read_wide_nowrap(&self, addr: u16, ctx: &Context) -> u16 {
        (self.read(addr + 1, ctx) as u16) << 8 | self.read(addr, ctx) as u16
    }

    pub fn read_wide(&self, addr: u16, ctx: &Context) -> u16 {
        let page = addr & 0xFF00;
        let idx = (addr as u8).wrapping_add(1) as u16;
        let addr_next = page | idx;
        (self.read(addr_next, ctx) as u16) << 8 | self.read(addr, ctx) as u16
    }

    pub fn push(&mut self, value: u8) {
        let old_sp = *self.sp.borrow();
        self.ram[0x0100 + old_sp as usize] = value;
        *self.sp.borrow_mut() = old_sp.wrapping_sub(1);
    }

    pub fn pop(&mut self) -> u8 {
        let mut sp = self.sp.borrow_mut();
        *sp = sp.wrapping_add(1);
        self.ram[0x0100 + *sp as usize]
    }

    pub fn update_flags(&self, value: u8) {
        self.set_neg(value & (1 << 7) != 0);
        self.set_zero(value == 0);
    }

    pub fn trigger_nmi(&mut self, ctx: &Context) {
        self.push((self.pc >> 8) as u8);
        self.push((self.pc & 0xFF) as u8);
        let status = *self.status.borrow();
        self.push(status | 0b10 << 4);

        // Disable interrupts
        self.set_interrupt(true);

        self.pc = (self.read(0xFFFB, ctx) as u16) << 8 | self.read(0xFFFA, ctx) as u16;
        self.state = State::Nmi;
        
        println!("NMI triggered, PC is now {:#06X} ({:#04X})", self.pc, self.read(self.pc, ctx));
    }

	pub fn try_irq(&mut self, ctx: &Context) -> bool {
		if self.get_interrupt() {
			return false;
		}

        self.push((self.pc >> 8) as u8);
        self.push((self.pc & 0xFF) as u8);
        let status = *self.status.borrow();
        self.push(status | 0b10 << 4);

        // Disable interrupts
        self.set_interrupt(true);

        self.pc = (self.read(0xFFFF, ctx) as u16) << 8 | self.read(0xFFFE, ctx) as u16;
        self.state = State::Irq;

        true
	}

    pub fn print_stack(&self) {
        println!("SP: {:#04X}, Stack:", *self.sp.borrow());
        for i in *self.sp.borrow()..=255 {
            println!("{:#04X}: {:#04X}", i, self.ram[0x0100 + i as usize]);
        }
    }

    pub fn print_instr(&mut self, instr: &Instruction, ctx: &Context) {
        print!("[PC: {:#06X}] A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X} {} {: >4?}:", self.pc, *self.acc.borrow(), *self.x.borrow(), *self.y.borrow(), *self.status.borrow(), *self.sp.borrow(), instr.opcode, instr.mode);
        for i in 0..instr.mode.len() {
            print!("{:#04X} ", self.read(self.pc + i as u16, ctx));
        }
        println!("");
    }


    pub fn next(&mut self, ctx: &Context) -> usize {
        let cycles = match self.state {
            State::Reset => {
                self.pc = (self.read(self.pc + 1, ctx) as u16) << 8 | self.read(self.pc, ctx) as u16;
                println!("Reset vector was {:#06X}", self.pc);
                self.state = State::Run;
                1
            },
            State::Irq | 
            State::Nmi |
            State::Run => {
                let id = self.read(self.pc, ctx);
                let instr = instruction::ARCH[id as usize].as_ref().unwrap_or_else(|| { 
                    self.print_stack();
                    panic!("Instruction {:#04X} does not exist", id)
                });

                self.print_instr(instr, ctx);

                // TODO: Fix this
                if id == 0x00 && self.try_irq(ctx) {
                    1
                }
                else {
                    instr.run(ctx, self)
                }
            },
        };

        cycles
    }
}

