#![allow(non_snake_case)]

use std::cmp::Ordering;
use std::num::Wrapping;
use crate::Context;
use std::str::FromStr;
use super::CPU;

lazy_static! {
    pub static ref ARCH: [Option<Instruction>; 256] = instructions::gen_excs();
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AddressingMode {
    Impl,
    Imm,
    Rel,
    Zpg,
    ZpgX,
    ZpgY,
    XInd,
    IndY,
    Ind,
    Abs,
    AbsX,
    AbsY,
}

pub enum Address {
    Implied,
    Immediate(u8),
    Addressed(u16),
}

impl AddressingMode {
    pub fn len(&self) -> usize {
        match self {
            AddressingMode::Impl => 1,
            AddressingMode::Imm => 2,
            AddressingMode::Zpg => 2,
            AddressingMode::ZpgX => 2,
            AddressingMode::ZpgY => 2,
            AddressingMode::XInd => 2,
            AddressingMode::Ind => 3,
            AddressingMode::IndY => 2,
            AddressingMode::Rel => 2,
            AddressingMode::Abs => 3,
            AddressingMode::AbsX => 3,
            AddressingMode::AbsY => 3,
        }
    }

    pub fn address(&self, ctx: &Context, cpu: &CPU) -> (Address, usize) {
        match self {
            AddressingMode::Impl => (Address::Implied, 0),
            AddressingMode::Imm => (Address::Immediate(cpu.read(cpu.pc + 1, ctx)), 0),
            AddressingMode::Zpg => {
                (Address::Addressed(cpu.read(cpu.pc + 1, ctx) as u16), 0)
            },
            AddressingMode::ZpgX => {
                (Address::Addressed(cpu.read(cpu.pc + 1, ctx).wrapping_add(cpu.x) as u16), 0)
            },
            AddressingMode::ZpgY => {
                (Address::Addressed(cpu.read(cpu.pc + 1, ctx).wrapping_add(cpu.y) as u16), 0)
            },
            AddressingMode::Ind => {
                (Address::Addressed(cpu.read_wide(cpu.pc + 1, ctx)), 0)
            },
            AddressingMode::XInd => {
                let zpg_addr = cpu.read(cpu.pc + 1, ctx);
                (Address::Addressed(cpu.read_wide(zpg_addr.wrapping_add(cpu.x) as u16, ctx)), 0)
            },
            AddressingMode::Rel => {
                (Address::Immediate(cpu.read(cpu.pc + 1, ctx)), 0)
            },
            AddressingMode::IndY => {
                let zpg_addr = cpu.read(cpu.pc + 1, ctx);
                let mut effective_addr = cpu.read_wide(zpg_addr as u16, ctx);
                let prev = effective_addr;
                effective_addr += cpu.y as u16;
                let cycles = if prev >> 8 != effective_addr >> 8 {
                    1
                }
                else {
                    0
                };
                (Address::Addressed(effective_addr), cycles)
            }
            AddressingMode::Abs => {
                (Address::Addressed(cpu.read_wide_nowrap(cpu.pc + 1, ctx)), 0)
            },
            AddressingMode::AbsX => {
                let addr = cpu.read_wide(cpu.pc + 1, ctx);
                let eff_addr = addr + cpu.x as u16;
                let cycles = if addr >> 8 != eff_addr >> 8 {
                    println!("{:#06X}, {:#06X}", addr, eff_addr);
                    1
                }
                else {
                    0
                };
                (Address::Addressed(eff_addr), cycles)
            },
            AddressingMode::AbsY => {
                let addr = cpu.read_wide(cpu.pc + 1, ctx);
                let eff_addr = addr + cpu.y as u16;
                let cycles = if addr >> 8 != eff_addr >> 8 {
                    1
                }
                else {
                    0
                };
                (Address::Addressed(eff_addr), cycles)

            },
        }
    }
}

impl FromStr for AddressingMode {
    type Err = String;

    fn from_str(s: &str) -> Result<AddressingMode, String> {
        match s {
            "" => Ok(AddressingMode::Impl),
            "imm" => Ok(AddressingMode::Imm),
            "zpg" => Ok(AddressingMode::Zpg),
            "zpg,X" => Ok(AddressingMode::ZpgX),
            "zpg,Y" => Ok(AddressingMode::ZpgY),
            "ind" => Ok(AddressingMode::Ind),
            "X,ind" => Ok(AddressingMode::XInd),
            "ind,Y" => Ok(AddressingMode::IndY),
            "abs" => Ok(AddressingMode::Abs),
            "abs,X" => Ok(AddressingMode::AbsX),
            "abs,Y" => Ok(AddressingMode::AbsY),
            "rel" => Ok(AddressingMode::Rel),
            _ => Err(s.to_string()),
        }
    }
}

enum Operation {
    Addressed(&'static (dyn Fn(&Context, &mut CPU, u16) + Sync)),
    Immediate(&'static (dyn Fn(&Context, &mut CPU, u8) + Sync)),
    Implied(&'static (dyn Fn(&Context, &mut CPU) + Sync)),
}

impl Operation {
    pub fn run(&self, ctx: &Context, cpu: &mut CPU, addr: &Address) {
        match self {
            Operation::Addressed(f) => {
                if let Address::Addressed(a) = addr {
                    f(ctx, cpu, *a)
                }
                else {
                    panic!()
                }
            },
            Operation::Immediate(f) => {
                if let Address::Immediate(a) = addr {
                    f(ctx, cpu, *a)
                }
                else {
                    panic!()
                }
            },
            Operation::Implied(f) => {
                if let Address::Implied = addr {
                    f(ctx, cpu)
                }
                else {
                    panic!()
                }
            },
        }
    }
}

pub struct Instruction {
    pub opcode: String,
    pub mode: AddressingMode,
    pub cycles: u8,
    operation: Operation,
}

impl Instruction {
    pub fn run(&self, ctx: &Context, cpu: &mut CPU) -> usize {
        let last_pc = cpu.pc;

        let (addr, addr_cycles) = self.mode.address(ctx, cpu);
        
        self.operation.run(ctx, cpu, &addr);

        let mut cycles = self.cycles as usize;

        if !self.opcode.starts_with("STA") {
            cycles += addr_cycles;
        }

        // Check for page boundary crossings
        // if this is a branch instruction
        if self.mode == AddressingMode::Rel {
            if cpu.pc >> 8 != last_pc >> 8 {
                cycles += 1;
            }
            else if cpu.pc != last_pc {
                cycles += 1;
                if (cpu.pc + self.mode.len() as u16) >> 8 != last_pc >> 8 {
                    cycles += 1;
                }
            }
        }

        cpu.pc += self.mode.len() as u16;

        cycles
    }
}

macro_rules! raw_inst {
	($v:ident $name:ident $val:tt $mode:literal $exc:expr) => (
		assert!($v.insert(
            $val,
            crate::cpu::instruction::Instruction {
                opcode: stringify!($name).to_string(),
                cycles: 2,
                mode: ($mode).parse().unwrap(),
                operation: $exc 
            }
        ).is_none(), "Duplicate instruction {:#X}", $val);
	);
    ($v:ident $name:ident $val:tt $len:literal) => (
        assert!($v.insert(
                $val,
                crate::cpu::instruction::Instruction {
                    opcode: stringify!($name).to_string(),
                    cycles: 2,
                    mode: ($mode).parse().unwrap()
                    operation: Operation::Implied(::std::boxed::Box::new(|_| unimplemented!("Instruction {} ({:#X}) not implemented", stringify!($name), $val)))
                }
        ).is_none(), "Duplicate instruction {:#X}", $val);
    );
}

macro_rules! inst {
	($v:ident $name:ident $val:literal cc_01_all_addressed $($exc:expr)?) => (
		inst!($v $name ($val | 0b00000) X,ind $($exc)?);
		inst!($v $name ($val | 0b00100) zpg $($exc)?);
		/* -- Immediate would go here -- */
		inst!($v $name ($val | 0b01100) abs $($exc)?);
		inst!($v $name ($val | 0b10000) ind,Y $($exc)?);
		inst!($v $name ($val | 0b10100) zpg,X $($exc)?);
		inst!($v $name ($val | 0b11000) abs,Y $($exc)?);
		inst!($v $name ($val | 0b11100) abs,X $($exc)?);
	);
    ($v:ident $name:ident $val:tt abs abs,X $($exc:expr)?) => (
        inst!($v $name $val abs $($exc)?);
        inst!($v $name ($val | 0x10) abs,X $($exc)?);
    );
    ($v:ident $name:ident $val:tt abs $($exc:expr)?) => (
		raw_inst!($v $name $val "abs" $(Operation::Addressed($exc))?)
    );
    ($v:ident $name:ident $val:tt abs,X $($exc:expr)?) => (
		raw_inst!($v $name $val "abs,X" $(Operation::Addressed($exc))?)
    );
    ($v:ident $name:ident $val:tt abs,Y $($exc:expr)?) => (
		raw_inst!($v $name $val "abs,Y" $(Operation::Addressed($exc))?)
    );
    ($v:ident $name:ident $val:tt imm $($exc:expr)?) => (
		raw_inst!($v $name $val "imm" $(Operation::Immediate($exc))?)
    );
    ($v:ident $name:ident $val:tt ind $($exc:expr)?) => (
		raw_inst!($v $name $val "ind" $(Operation::Addressed($exc))?)
    );
    ($v:ident $name:ident $val:tt X,ind ind,Y $($exc:expr)?) => (
        inst!($v $name $val X,ind $($exc)?);
        inst!($v $name ($val | 0x10) ind,Y $($exc)?);
    );
    ($v:ident $name:ident $val:tt X,ind $($exc:expr)?) => (
		raw_inst!($v $name $val "X,ind" $(Operation::Addressed($exc))?)
    );
    ($v:ident $name:ident $val:tt ind,Y $($exc:expr)?) => (
		raw_inst!($v $name $val "ind,Y" $(Operation::Addressed($exc))?)
    );
    ($v:ident $name:ident $val:tt rel $($exc:expr)?) => (
		raw_inst!($v $name $val "rel" $(Operation::Immediate($exc))?)
    );
    ($v:ident $name:ident $val:tt zpg zpg,X $($exc:expr)?) => (
        inst!($v $name $val zpg $($exc)?);
        inst!($v $name ($val | 0x10) zpg,X $($exc)?);
    );
    ($v:ident $name:ident $val:tt zpg $($exc:expr)?) => (
		raw_inst!($v $name $val "zpg" $(Operation::Addressed($exc))?)
    );
    ($v:ident $name:ident $val:tt zpg,X $($exc:expr)?) => (
		raw_inst!($v $name $val "zpg,X" $(Operation::Addressed($exc))?)
    );
    ($v:ident $name:ident $val:tt zpg,Y $($exc:expr)?) => (
		raw_inst!($v $name $val "zpg,Y" $(Operation::Addressed($exc))?)
    );
    ($v:ident $name:ident $val:tt $($exc:expr)?) => (
		raw_inst!($v $name $val "" $(Operation::Implied($exc))?)
	);
}

macro_rules! inst_list {
    ($(
        { $($in:tt)+ }
    )*)
        =>
        (pub mod instructions {
            use super::*;
			pub fn gen_excs() -> [Option<Instruction>; 256] {
				let mut excs = std::collections::HashMap::new();
				$(
					inst!(excs $($in)+);
				)*

                let parsed = parse_instruction_list();

                for (n, m, cycles) in parsed {
                    let i = excs.iter_mut().find(|(_, instr): &(_, &mut Instruction)| instr.opcode == n && instr.mode == m).unwrap_or_else(|| panic!("Couldn't find {}, {:?}", n, m));
                    i.1.cycles = cycles;
                }

                let mut result: [std::mem::MaybeUninit<Option<Instruction>>; 256] = unsafe {
                    std::mem::MaybeUninit::uninit().assume_init()
                };

                for elem in &mut result[..] {
                    *elem = std::mem::MaybeUninit::new(None);
                }

                let mut result = unsafe { std::mem::transmute::<_, [Option<Instruction>; 256]>(result) };

                for (k, v) in excs.into_iter() {
                    result[k] = Some(v);
                }

                let other_times = vec![
                    // DOP
                    (0x04, 3),
                    (0x14, 4),
                    (0x34, 4),
                    (0x44, 3),
                    (0x54, 4),
                    (0x64, 3),
                    (0x74, 4),
                    (0x80, 2),
                    (0xD4, 4),
                    (0xF4, 4),
                    // TOP
                    (0x0C, 4),
                    (0x1C, 4),
                    (0x3C, 4),
                    (0x5C, 4),
                    (0x7C, 4),
                    (0xDC, 4),
                    (0xFC, 4),
                    // LAX
                    (0xA7, 3),
                    (0xB7, 4),
                    (0xAF, 4),
                    (0xBF, 4),
                    (0xA3, 6),
                    (0xB3, 5),
                    // AAX
                    (0x87, 3),
                    (0x97, 4),
                    (0x83, 6),
                    (0x8F, 4),
                    // DCP
                    (0xC7, 5),
                    (0xD7, 6),
                    (0xCF, 6),
                    (0xDF, 6), // Should be 7?
                    (0xDB, 6), // Should be 7?
                    (0xC3, 8),
                    (0xD3, 7), // Should be 8?
                    // ISC
                    (0xE7, 5),
                    (0xF7, 6),
                    (0xEF, 6),
                    (0xFF, 6), // Should be 7?
                    (0xFB, 6), // Should be 7?
                    (0xE3, 8),
                    (0xF3, 7), // Should be 7?
                    // SLO
                    (0x07, 5),
                    (0x17, 6),
                    (0x0F, 6),
                    (0x1F, 6), // Should be 7?
                    (0x1B, 6), // Should be 7?
                    (0x03, 8),
                    (0x13, 7), // Should be 8?
                    // RLA
                    (0x27, 5),
                    (0x37, 6),
                    (0x2F, 6),
                    (0x3F, 6), // Should be 7?
                    (0x3B, 6), // Should be 7?
                    (0x23, 8),
                    (0x33, 7), // Should be 8?
                    // SRE
                    (0x47, 5),
                    (0x57, 6),
                    (0x4F, 6),
                    (0x5F, 6), // Should be 7?
                    (0x5B, 6), // Should be 7?
                    (0x43, 8),
                    (0x53, 7), // Should be 8?
                    // RRA
                    (0x67, 5),
                    (0x77, 6),
                    (0x6F, 6),
                    (0x7F, 6), // Should be 7?
                    (0x7B, 6), // Should be 7?
                    (0x63, 8),
                    (0x73, 7), // Should be 8?
                ];

                for (op, time) in other_times.iter() {
                    result[*op as usize].as_mut().unwrap_or_else(|| { println!("Instr {:#X} doesn't exist", *op); std::process::exit(1) }).cycles = *time;
                }

                result
			}
        });
}

fn parse_instruction_list() -> Vec<(String, AddressingMode, u8)> {
    let data = std::fs::read_to_string("./instructions.txt").unwrap();

    let mut result = Vec::new();

    for listing in data.split("\n\n\n") {
        let name = listing[0..3].to_owned();

        for line in listing.lines().skip(7) {
            let words: Vec<_> = line.split(' ').filter(|s| !s.is_empty()).collect();
            if words.len() == 0 {
                continue;
            }
            assert!(words.len() == 6 || words.len() == 5, "Line:\n{}\nName: {}", line, name);

            let mode = 
            match words[0] {
                "immidiate" => AddressingMode::Imm,
                "zeropage" => AddressingMode::Zpg,
                "zeropage,X" => AddressingMode::ZpgX,
                "zeropage,Y" => AddressingMode::ZpgY,
                "absolute" => AddressingMode::Abs,
                "absolute,X" => AddressingMode::AbsX,
                "absolute,Y" => AddressingMode::AbsY,
                "indirect" => AddressingMode::Ind,
                "(indirect,X)" => AddressingMode::XInd,
                "(indirect),Y" => AddressingMode::IndY,
                "implied" |
                "accumulator" => AddressingMode::Impl,
                "relative" => AddressingMode::Rel,
                a => panic!("{}", a),
            };

            // TODO: Page-crossing penalties
            let cycles = words[words.len() - 1].chars().next().unwrap().to_digit(10).unwrap() as u8;

            result.push((name.clone(), mode, cycles));
        }
    }

    result
}

fn BRANCH(_: &Context, cpu: &mut CPU, offset: u8, cond: bool) {
    if cond { 
        let neg = offset >> 7 != 0;
        if neg {
            cpu.pc -= (!offset + 1) as u16
        }
        else {
            cpu.pc += offset as u16
        }
    }
}

fn COMPARE(_: &Context, cpu: &mut CPU, lhs: u8, rhs: u8) {
    let diff = (Wrapping(lhs) - Wrapping(rhs)).0;
    match lhs.cmp(&rhs) {
        Ordering::Less => {
            cpu.set_neg(diff >> 7 != 0);
            cpu.set_zero(false);
            cpu.set_carry(false);
        },
        Ordering::Equal => {
            cpu.set_neg(false);
            cpu.set_zero(true);
            cpu.set_carry(true);
        },
        Ordering::Greater => {
            cpu.set_neg(diff >> 7 != 0);
            cpu.set_zero(false);
            cpu.set_carry(true);
        }
    }
}

fn ADC_imm(_: &Context, cpu: &mut CPU, rhs: u8) {
    let res1 = cpu.acc as u16 + rhs as u16 + cpu.get_carry() as u16;
    let result = (res1 % 0x100) as u8;
    let did_carry = res1 & 0x100 != 0;
    let did_overflow = (cpu.acc ^ result) & (rhs ^ result) & 0x80 != 0;
    cpu.update_flags(result);
    cpu.set_carry(did_carry);
    cpu.set_overflow(did_overflow);
    cpu.acc = result;
}

fn SUB(_: &Context, cpu: &mut CPU, rhs: u8) {
    let res1 = cpu.acc as u16 + (!rhs) as u16 + cpu.get_carry() as u16;
    let result = (res1 % 0x100) as u8;
    let did_carry = res1 & 0x100 != 0;
    let did_overflow = (cpu.acc ^ result) & (!rhs ^ result) & 0x80 != 0;
    cpu.update_flags(result);
    cpu.set_carry(did_carry);
    cpu.set_overflow(did_overflow);
    cpu.acc = result;
}

fn ror(_: &Context, cpu: &mut CPU, mut value: u8) -> u8 {
    let carry_out = value & 1 != 0;
    value >>= 1;
    value |= (cpu.get_carry() as u8) << 7;
    cpu.update_flags(value);
    cpu.set_carry(carry_out);
    value
}

fn rol(_: &Context, cpu: &mut CPU, mut value: u8) -> u8 {
    let carry_out = value & 0x80 != 0;
    value <<= 1;
    value |= cpu.get_carry() as u8;
    cpu.update_flags(value);
    cpu.set_carry(carry_out);
    value
}

fn lsr(_: &Context, cpu: &mut CPU, mut value: u8) -> u8 {
    let carry = value & 1 != 0;
    value >>= 1;
    cpu.update_flags(value);
    cpu.set_carry(carry);
    value
}

fn BIT(ctx: &Context, cpu: &mut CPU, addr: u16) {
    let value = cpu.read(addr, ctx);
    cpu.set_neg(value & 0x80 != 0);
    cpu.set_overflow(value & 0x40 != 0);
    cpu.set_zero(cpu.acc & value == 0);
}

fn AAX(ctx: &Context, cpu: &mut CPU, addr: u16) {
    cpu.write(addr, cpu.acc & cpu.x, ctx);
}

fn DCP(ctx: &Context, cpu: &mut CPU, addr: u16) {
    let v = (Wrapping(cpu.read(addr, ctx)) - Wrapping(1)).0;
    cpu.write(addr, v, ctx);
    COMPARE(ctx, cpu, cpu.acc, v);
}

fn ISC(ctx: &Context, cpu: &mut CPU, addr: u16) {
    let v = (Wrapping(cpu.read(addr, ctx)) + Wrapping(1)).0;
    cpu.write(addr, v, ctx);
    SUB(ctx, cpu, v);
}

fn SLO(ctx: &Context, cpu: &mut CPU, addr: u16) {
    let mut m = cpu.read(addr, ctx);
    let c = m >> 7;
    m <<= 1;
    cpu.write(addr, m, ctx); 
    cpu.acc |= m;
    cpu.update_flags(cpu.acc);
    cpu.set_carry(c != 0);
}

fn RLA(ctx: &Context, cpu: &mut CPU,  addr: u16) {
    let v = cpu.read(addr, ctx);
    let result = rol(ctx, cpu, v);
    cpu.write(addr, result, ctx);
    cpu.acc &= result;
    cpu.update_flags(cpu.acc);
}

fn SRE(ctx: &Context, cpu: &mut CPU, addr: u16) {
    let mut m = cpu.read(addr, ctx);
    let carry = m & 1 != 0;
    m >>= 1;
    cpu.write(addr, m, ctx);
    cpu.set_carry(carry);

    cpu.acc ^= m;
    cpu.update_flags(cpu.acc);
}

fn RRA(ctx: &Context, cpu: &mut CPU, addr: u16) {
    let read = cpu.read(addr, ctx);
    let result = ror(ctx, cpu, read);
    cpu.write(addr, result, ctx);
    ADC_imm(ctx, cpu, result);
}

fn asl(_: &Context, cpu: &mut CPU, mut val: u8) -> u8 {
    let c = val >> 7 != 0;
    val <<= 1;
    cpu.update_flags(val);
    cpu.set_carry(c);
    val
}

fn RTI(_: &Context, cpu: &mut CPU) {
    cpu.status = (cpu.pop() & !(0b01 << 4)) | 0b10 << 4;
    cpu.pc = cpu.pop() as u16;
    cpu.pc |= (cpu.pop() as u16) << 8;
    cpu.pc -= 1;
}
fn JSR(_: &Context, cpu: &mut CPU, addr: u16) {
    let pc = cpu.pc;
    cpu.push(((pc + 2) >> 8) as u8);
    cpu.push(((pc + 2) & 0xFF) as u8);
    cpu.pc = addr - 3;
}
fn RTS(_: &Context, cpu: &mut CPU) {
    let lo = cpu.pop() as u16;
    let hi = cpu.pop() as u16;
    cpu.pc = hi << 8 | lo;
}

macro_rules! make_branch_func {
    ($truename:ident, $falsename:ident, $cond:ident) => {
        fn $truename(ctx: &Context, cpu: &mut CPU, offset: u8) {
            BRANCH(ctx, cpu, offset, cpu.$cond())
        }

        fn $falsename(ctx: &Context, cpu: &mut CPU, offset: u8) {
            BRANCH(ctx, cpu, offset, !cpu.$cond())
        }
    };
}

make_branch_func!(BEQ, BNE, get_zero);
make_branch_func!(BMI, BPL, get_neg);
make_branch_func!(BVS, BVC, get_overflow);
make_branch_func!(BCS, BCC, get_carry);

macro_rules! make_arith_func {
    ($name:ident, $name2:ident, $op:tt) => {
        fn $name(_: &Context, cpu: &mut CPU, imm: u8) { 
            cpu.acc $op imm;
            cpu.update_flags(cpu.acc);
        }

        fn $name2(ctx: &Context, cpu: &mut CPU, addr: u16) {
            let value = cpu.read(addr, ctx);
            $name(ctx, cpu, value)
        }
    };
}

make_arith_func!(ORA_imm, ORA, |=);
make_arith_func!(AND_imm, AND, &=);
make_arith_func!(EOR_imm, EOR, ^=);

macro_rules! make_trans_func {
    ($name:ident, $lhs:ident, $rhs:ident) => {
        fn $name(_: &Context, cpu: &mut CPU) {
            cpu.$lhs = cpu.$rhs;
            cpu.update_flags(cpu.$rhs);
        }
    };
}

make_trans_func!(TYA, acc, y);
make_trans_func!(TXA, acc, x);
make_trans_func!(TAY, y, acc);
make_trans_func!(TAX, x, acc);
make_trans_func!(TSX, x, sp);

fn TXS(_: &Context, cpu: &mut CPU) {
    cpu.sp = cpu.x;
}

fn SBC(ctx: &Context, cpu: &mut CPU, addr: u16) { let v = cpu.read(addr, ctx); SBC_imm(ctx, cpu, v) }
fn SBC_imm(ctx: &Context, cpu: &mut CPU, imm: u8) { SUB(ctx, cpu, imm); }
fn ADC(ctx: &Context, cpu: &mut CPU, addr: u16) { let v = cpu.read(addr, ctx); ADC_imm(ctx, cpu, v); }

fn LAX(ctx: &Context, cpu: &mut CPU, addr: u16) { LDA(ctx, cpu, addr); TAX(ctx, cpu); }

macro_rules! make_compare {
    ($name:ident, $name2:ident, $reg:ident) => {
        fn $name(ctx: &Context, cpu: &mut CPU, imm: u8) {
            COMPARE(ctx, cpu, cpu.$reg, imm);
        }

        fn $name2(ctx: &Context, cpu: &mut CPU, addr: u16) {
            let val = cpu.read(addr, ctx);
            $name(ctx, cpu, val);
        }
    };
}

make_compare!(CMP_imm, CMP, acc);
make_compare!(CPX_imm, CPX, x);
make_compare!(CPY_imm, CPY, y);

macro_rules! make_shift {
    ($name:ident, $name2:ident, $func:ident) => {
        fn $name(ctx: &Context, cpu: &mut CPU) {
            let result = $func(ctx, cpu, cpu.acc);
            cpu.acc = result;
        }

        fn $name2(ctx: &Context, cpu: &mut CPU, addr: u16) {
            let value = cpu.read(addr, ctx);
            let result = $func(ctx, cpu, value);
            cpu.write(addr, result, ctx);
        }
    };
}

make_shift!(ASL_imp, ASL, asl);
make_shift!(ROL_imp, ROL, rol);
make_shift!(LSR_imp, LSR, lsr);
make_shift!(ROR_imp, ROR, ror);

macro_rules! make_load_store {
    ($loadname_imm:ident, $loadname:ident, $storename:ident, $reg:ident) => {
        fn $loadname_imm(_: &Context, cpu: &mut CPU, imm: u8) {
            cpu.update_flags(imm);
            cpu.$reg = imm;
        }

        fn $loadname(ctx: &Context, cpu: &mut CPU, addr: u16) {
            let value = cpu.read(addr, ctx);
            $loadname_imm(ctx, cpu, value)
        }

        fn $storename(ctx: &Context, cpu: &mut CPU, addr: u16) {
            let value = cpu.$reg;
            cpu.write(addr, value, ctx);
        }
    };
}

make_load_store!(LDA_imm, LDA, STA, acc);
make_load_store!(LDX_imm, LDX, STX, x);
make_load_store!(LDY_imm, LDY, STY, y);

fn DEC(ctx: &Context, cpu: &mut CPU, addr: u16) { let v = (Wrapping(cpu.read(addr, ctx)) - Wrapping(1)).0; cpu.write(addr, v, ctx); cpu.update_flags(v); }
fn INC(ctx: &Context, cpu: &mut CPU, addr: u16) { let v = (Wrapping(cpu.read(addr, ctx)) + Wrapping(1)).0; cpu.write(addr, v, ctx); cpu.update_flags(v); }

macro_rules! make_inc_dec {
    ($decname:ident, $incname:ident, $reg:ident) => {
        fn $decname(_: &Context, cpu: &mut CPU) {
            let result = (Wrapping(cpu.$reg) - Wrapping(1)).0;
            cpu.update_flags(result);
            cpu.$reg = result;
        }

        fn $incname(_: &Context, cpu: &mut CPU) {
            let result = (Wrapping(cpu.$reg) + Wrapping(1)).0;
            cpu.update_flags(result);
            cpu.$reg = result;
        }
    };
}

make_inc_dec!(DEX, INX, x);
make_inc_dec!(DEY, INY, y);

fn PHP(_: &Context, cpu: &mut CPU) {
    let status = cpu.status | (0b11 << 4);
    cpu.push(status)
}
fn PLP(_: &Context, cpu: &mut CPU) {
    let status = (cpu.pop() & !(0b01 << 4)) | (0b10 << 4);
    cpu.status = status;
}

fn PHA(_: &Context, cpu: &mut CPU) {
    cpu.push(cpu.acc);
}
fn PLA(_: &Context, cpu: &mut CPU) {
    let value = cpu.pop();
    cpu.update_flags(value);
    cpu.acc = value;
}

macro_rules! make_flag_setter {
    ($setflag:ident, $clearflag:ident, $setter_name:ident) => {
        fn $setflag(_: &Context, cpu: &mut CPU) {
            cpu.$setter_name(true);
        }

        fn $clearflag(_: &Context, cpu: &mut CPU) {
            cpu.$setter_name(false);
        }
    };
}

make_flag_setter!(SEC, CLC, set_carry);
make_flag_setter!(SEI, CLI, set_interrupt);
make_flag_setter!(_SEV, CLV, set_overflow);
make_flag_setter!(SED, CLD, set_decimal);

fn JMP_abs(_: &Context, cpu: &mut CPU, addr: u16) { cpu.pc = addr - 3; }
fn JMP_ind(ctx: &Context, cpu: &mut CPU, addr: u16) { cpu.pc = cpu.read_wide(addr, ctx) - 3; }

fn NOP(_: &Context, _: &mut CPU) {}
fn DOP_imm(_: &Context, _: &mut CPU, _: u8) {}
fn DOP(_: &Context, _: &mut CPU, _: u16) {}
fn TOP_addr(_: &Context, _: &mut CPU, _: u16) {}

inst_list! {
    { BRK 0x00 &NOP }
    { BPL 0x10 rel &BPL }
    { JSR 0x20 abs &JSR }
    { BMI 0x30 rel &BMI }
    { RTI 0x40 &RTI }
    { BVC 0x50 rel &BVC }
    { RTS 0x60 &RTS }
    { BVS 0x70 rel &BVS }
    { DOP 0x80 imm &DOP_imm }
    { BCC 0x90 rel &BCC }
    { LDY 0xA0 imm &LDY_imm }
    { BCS 0xB0 rel &BCS }
    { CPY 0xC0 imm &CPY_imm }
    { BNE 0xD0 rel &BNE }
    { CPX 0xE0 imm &CPX_imm }
    { BEQ 0xF0 rel &BEQ }
	{ ORA 0x01 cc_01_all_addressed &ORA }
    { AND 0x21 cc_01_all_addressed &AND }
    { EOR 0x41 cc_01_all_addressed &EOR }
    { ADC 0x61 cc_01_all_addressed &ADC }
    { STA 0x81 cc_01_all_addressed &STA }
    { LDA 0xA1 cc_01_all_addressed &LDA }
    { CMP 0xC1 cc_01_all_addressed &CMP }
    { SBC 0xE1 cc_01_all_addressed &SBC }
    { SKB 0x82 imm &DOP_imm }
    { LDX 0xA2 imm &LDX_imm }
    { SKB 0xC2 imm &DOP_imm }
    { SLO 0x03 cc_01_all_addressed &SLO }
    { RLA 0x23 cc_01_all_addressed &RLA }
    { SRE 0x43 cc_01_all_addressed &SRE }
    { RRA 0x63 cc_01_all_addressed &RRA }
    { AAX 0x83 X,ind &AAX }
    { LAX 0xA3 X,ind ind,Y &LAX }
    { DCP 0xC3 X,ind ind,Y &DCP }
    { ISC 0xE3 X,ind ind,Y &ISC }
    { DOP 0x04 zpg zpg,X &DOP }
    { BIT 0x24 zpg &BIT }
    { DOP 0x34 zpg,X &DOP }
    { DOP 0x44 zpg zpg,X &DOP }
    { DOP 0x64 zpg zpg,X &DOP }
    { STY 0x84 zpg zpg,X &STY }
    { LDY 0xA4 zpg zpg,X &LDY }
    { CPY 0xC4 zpg &CPY }
    { DOP 0xD4 zpg,X &DOP }
    { CPX 0xE4 zpg &CPX }
    { DOP 0xF4 zpg,X &DOP }
    { ASL 0x06 zpg zpg,X &ASL }
    { ROL 0x26 zpg zpg,X &ROL }
    { LSR 0x46 zpg zpg,X &LSR }
    { ROR 0x66 zpg zpg,X &ROR }
    { STX 0x86 zpg &STX }
    { STX 0x96 zpg,Y &STX }
    { LDX 0xA6 zpg &LDX }
    { LDX 0xB6 zpg,Y &LDX }
    { DEC 0xC6 zpg zpg,X &DEC }
    { INC 0xE6 zpg zpg,X &INC }
    { AAX 0x87 zpg &AAX }
    { AAX 0x97 zpg,Y &AAX }
    { LAX 0xA7 zpg &LAX }
    { LAX 0xB7 zpg,Y &LAX }
    { DCP 0xC7 zpg zpg,X &DCP }
    { ISC 0xE7 zpg zpg,X &ISC }
    { PHP 0x08 &PHP }
    { CLC 0x18 &CLC }
    { PLP 0x28 &PLP }
    { SEC 0x38 &SEC }
    { PHA 0x48 &PHA }
    { CLI 0x58 &CLI }
    { PLA 0x68 &PLA }
    { SEI 0x78 &SEI }
    { DEY 0x88 &DEY }
    { TYA 0x98 &TYA }
    { TAY 0xA8 &TAY }
    { CLV 0xB8 &CLV }
    { INY 0xC8 &INY }
    { CLD 0xD8 &CLD }
    { INX 0xE8 &INX }
    { SED 0xF8 &SED }
    { ORA 0x09 imm &ORA_imm }
    { AND 0x29 imm &AND_imm }
    { EOR 0x49 imm &EOR_imm }
    { ADC 0x69 imm &ADC_imm }
    { LDA 0xA9 imm &LDA_imm }
    { CMP 0xC9 imm &CMP_imm }
    { SBC 0xE9 imm &SUB }
    { ASL 0x0A &ASL_imp }
    { NOP 0x1A &NOP }
    { ROL 0x2A &ROL_imp }
    { NOP 0x3A &NOP }
    { LSR 0x4A &LSR_imp }
    { NOP 0x5A &NOP }
    { ROR 0x6A &ROR_imp }
    { NOP 0x7A &NOP }
    { TXA 0x8A &TXA }
    { TXS 0x9A &TXS }
    { TAX 0xAA &TAX }
    { TSX 0xBA &TSX }
    { DEX 0xCA &DEX }
    { NOP 0xDA &NOP }
    { NOP 0xEA &NOP }
    { NOP 0xFA &NOP }
    { DCP 0xDB abs,Y &DCP }
    { SBC 0xEB imm &SUB }
    { ISC 0xFB abs,Y &ISC }
    { TOP 0x0C abs abs,X &TOP_addr }
    { BIT 0x2C abs &BIT }
    { TOP 0x3C abs,X &TOP_addr }
    { JMP 0x4C abs &JMP_abs }
    { TOP 0x5C abs,X &TOP_addr }
    { JMP 0x6C ind &JMP_ind }
    { TOP 0x7C abs,X &TOP_addr }
    { STY 0x8C abs &STY }
    { LDY 0xAC abs abs,X &LDY }
    { CPY 0xCC abs &CPY }
    { TOP 0xDC abs,X &TOP_addr }
    { CPX 0xEC abs &CPX }
    { TOP 0xFC abs,X &TOP_addr }
    { ASL 0x0E abs abs,X &ASL }
    { ROL 0x2E abs abs,X &ROL }
    { LSR 0x4E abs abs,X &LSR }
    { ROR 0x6E abs abs,X &ROR }
    { STX 0x8E abs &STX }
    { LDX 0xAE abs &LDX }
    { LDX 0xBE abs,Y &LDX }
    { DEC 0xCE abs abs,X &DEC }
    { INC 0xEE abs abs,X &INC }
    { AAX 0x8F abs &AAX }
    { LAX 0xAF abs &LAX }
    { LAX 0xBF abs,Y &LAX }
    { DCP 0xCF abs abs,X &DCP }
    { ISC 0xEF abs abs,X &ISC }
}
