pub mod instruction;

use crate::context::CpuContext;
use instruction::Instruction;

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

impl CPU {
    pub fn new() -> CPU {
        CPU {
            status: 0,
            pc: 0xFFFC,
            acc: 0,
            x: 0,
            y: 0,
            sp: 0,
            ram: [0; 0x800],
            state: State::Reset,
        }
    }

    fn set_status(&mut self, v: bool, b: u8) {
        assert!(b < 8);
        self.status &= !(1 << b);
        self.status |= (v as u8) << b;
    }

    fn get_status(&self, b: u8) -> bool {
        (self.status >> b) & 1 != 0
    }

    pub fn read(&self, addr: u16, context: &mut CpuContext) -> u8 {
        if addr < 0x2000 {
            self.ram[(addr % 0x800) as usize]
        } else {
            context.read(addr)
        }
    }

    pub fn write(&mut self, addr: u16, value: u8, context: &mut CpuContext) {
        if addr < 0x2000 {
            self.ram[(addr % 0x800) as usize] = value;
        } else {
            context.write(addr, value);
        }
    }

    pub fn set_decimal(&mut self, v: bool) {
        self.set_status(v, 3);
    }

    pub fn get_decimal(&self) -> bool {
        self.get_status(3)
    }

    pub fn set_interrupt(&mut self, v: bool) {
        self.set_status(v, 2);
    }

    pub fn get_interrupt(&self) -> bool {
        self.get_status(2)
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

    pub fn read_wide_nowrap(&self, addr: u16, ctx: &mut CpuContext) -> u16 {
        (self.read(addr + 1, ctx) as u16) << 8 | self.read(addr, ctx) as u16
    }

    pub fn read_wide(&self, addr: u16, ctx: &mut CpuContext) -> u16 {
        let page = addr & 0xFF00;
        let idx = (addr as u8).wrapping_add(1) as u16;
        let addr_next = page | idx;
        (self.read(addr_next, ctx) as u16) << 8 | self.read(addr, ctx) as u16
    }

    pub fn push(&mut self, value: u8) {
        let old_sp = self.sp;
        self.ram[0x0100 + old_sp as usize] = value;
        self.sp = old_sp.wrapping_sub(1);
    }

    pub fn pop(&mut self) -> u8 {
        self.sp = self.sp.wrapping_add(1);
        self.ram[0x0100 + self.sp as usize]
    }

    pub fn update_flags(&mut self, value: u8) {
        self.set_neg(value & (1 << 7) != 0);
        self.set_zero(value == 0);
    }

    pub fn trigger_nmi(&mut self, ctx: &mut CpuContext) {
        self.push((self.pc >> 8) as u8);
        self.push((self.pc & 0xFF) as u8);
        self.push(self.status | 0b10 << 4);

        // Disable interrupts
        self.set_interrupt(true);

        self.pc = (self.read(0xFFFB, ctx) as u16) << 8 | self.read(0xFFFA, ctx) as u16;
        self.state = State::Nmi;

        //println!("NMI triggered, PC is now {:#06X} ({:#04X})", self.pc, self.read(self.pc, ctx));
    }

    pub fn try_irq(&mut self, ctx: &mut CpuContext) -> bool {
        if self.get_interrupt() {
            return false;
        }

        self.push((self.pc >> 8) as u8);
        self.push((self.pc & 0xFF) as u8);
        self.push(self.status | 0b10 << 4);

        // Disable interrupts
        self.set_interrupt(true);

        self.pc = (self.read(0xFFFF, ctx) as u16) << 8 | self.read(0xFFFE, ctx) as u16;
        self.state = State::Irq;

        true
    }

    pub fn print_stack(&self) {
        println!("SP: {:#04X}, Stack:", self.sp);
        for i in self.sp..=255 {
            println!("{:#04X}: {:#04X}", i, self.ram[0x0100 + i as usize]);
        }
    }

    pub fn print_instr(&mut self, instr: &Instruction, ctx: &mut CpuContext) {
        print!(
            "[PC: {:#06X}] A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X} {} {: >4?}:",
            self.pc, self.acc, self.x, self.y, self.status, self.sp, instr.opcode, instr.mode
        );
        for i in 0..instr.mode.size() {
            print!("{:#04X} ", self.read(self.pc + i as u16, ctx));
        }
        println!();
    }

    pub fn next(&mut self, ctx: &mut CpuContext) -> usize {
        match self.state {
            State::Reset => {
                self.pc =
                    (self.read(self.pc + 1, ctx) as u16) << 8 | self.read(self.pc, ctx) as u16;
                println!("Reset vector was {:#06X}", self.pc);
                self.state = State::Run;
                1
            }
            State::Irq | State::Nmi | State::Run => {
                let id = self.read(self.pc, ctx);
                let instr = instruction::ARCH[id as usize].as_ref().unwrap_or_else(|| {
                    self.print_stack();
                    panic!("Instruction {:#04X} does not exist", id)
                });

                //self.print_instr(instr, ctx);

                // TODO: Fix this
                if id == 0x00 && self.try_irq(ctx) {
                    1
                } else {
                    instr.run(ctx, self)
                }
            }
        }
    }
}

impl rust_2c02::Cpu for CPU {
    type Context = CpuContext;

    fn pause(&mut self, cycles: usize, context: &mut Self::Context) {
        context.cpu_pause = cycles;
    }

    fn read(&mut self, addr: u16, context: &mut Self::Context) -> u8 {
        CPU::read(self, addr, context)
    }
}

impl Default for CPU {
    fn default() -> CPU {
        CPU::new()
    }
}
