use super::MemLocation;
use std::num::Wrapping;
use crate::mapper::Mapped;

pub enum State {
    Reset,
    Irq,
    Nmi,
    Run,
}

pub struct CPU {
	pub status: u8,
	pub pc: u16,
	pub acc: u8,
	pub x: u8,
	pub y: u8,
	pub sp: u8,
    pub ram: [u8; 0x800],
    pub state: State,
}

// TODO: See if there is a better way to do this
macro_rules! get_mem {
    ($addr:ident, $mapper:ident, $ctx:ident, $($ref_t:tt)+) => ({
        match $addr {
            0x0000..=0x1FFF => MemLocation::Ram($($ref_t)+ $ctx.ram[($addr % 0x0800) as usize]),
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

impl CPU {
    pub fn push(&mut self, value: u8) {
        self.write(0x0100 + self.sp as u16, value);
        self.sp = (Wrapping(self.sp) - Wrapping(1)).0;
    }

    pub fn pop(&mut self) -> u8 {
        self.sp = (Wrapping(self.sp) + Wrapping(1)).0;
        let res = self.read(0x0100 + self.sp as u16);
        res
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

    pub fn next(&mut self) -> usize {
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
                    let instr = crate::instruction::ARCH[id as usize].as_ref().unwrap_or_else(|| {self.print_stack(); panic!("Instruction {:#04X} does not exist", id) });

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

            cycles
        }
    }

