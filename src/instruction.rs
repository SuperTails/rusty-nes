use std::cmp::Ordering;
use std::num::Wrapping;
use crate::Context;
use std::str::FromStr;

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

    pub fn address(&self, ctx: &mut Context) -> Address {
        match self {
            AddressingMode::Impl => Address::Implied,
            AddressingMode::Imm => Address::Immediate(ctx.read(ctx.pc + 1)),
            AddressingMode::Zpg => {
                Address::Addressed(ctx.read(ctx.pc + 1) as u16)
            },
            AddressingMode::ZpgX => {
                Address::Addressed((Wrapping(ctx.read(ctx.pc + 1)) + Wrapping(ctx.x)).0 as u16)
            },
            AddressingMode::ZpgY => {
                Address::Addressed((Wrapping(ctx.read(ctx.pc + 1)) + Wrapping(ctx.y)).0 as u16)
            },
            AddressingMode::Ind => {
                Address::Addressed(ctx.read_wide(ctx.pc + 1))
            },
            AddressingMode::XInd => {
                let zpg_addr = ctx.read(ctx.pc + 1);
                Address::Addressed(ctx.read_wide((Wrapping(zpg_addr) + Wrapping(ctx.x)).0 as u16))
            },
            AddressingMode::Rel => {
                Address::Immediate(ctx.read(ctx.pc + 1))
            },
            AddressingMode::IndY => {
                let zpg_addr = ctx.read(ctx.pc + 1);
                let mut effective_addr = Wrapping(ctx.read_wide(zpg_addr as u16));
                effective_addr += Wrapping(ctx.y as u16);
                Address::Addressed(effective_addr.0)
            }
            AddressingMode::Abs => {
                Address::Addressed(ctx.read_wide_nowrap(ctx.pc + 1))
            },
            AddressingMode::AbsX => {
                Address::Addressed((Wrapping(ctx.read_wide(ctx.pc + 1)) + Wrapping(ctx.x as u16)).0)
            },
            AddressingMode::AbsY => {
                Address::Addressed((Wrapping(ctx.read_wide(ctx.pc + 1)) + Wrapping(ctx.y as u16)).0)
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
    Addressed(&'static dyn Fn(&mut Context, u16)),
    Immediate(&'static dyn Fn(&mut Context, u8)),
    Implied(&'static dyn Fn(&mut Context)),
}

impl Operation {
    pub fn run(&self, ctx: &mut Context, addr: &Address) {
        match self {
            Operation::Addressed(f) => {
                if let Address::Addressed(a) = addr {
                    f(ctx, *a)
                }
                else {
                    panic!()
                }
            },
            Operation::Immediate(f) => {
                if let Address::Immediate(a) = addr {
                    f(ctx, *a)
                }
                else {
                    panic!()
                }
            },
            Operation::Implied(f) => {
                if let Address::Implied = addr {
                    f(ctx)
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
    pub fn run(&self, ctx: &mut Context) -> usize {
        let last_pc = ctx.pc;

        let addr = self.mode.address(ctx);
        self.operation.run(ctx, &addr);

        let mut cycles = self.cycles as usize;

        // TODO: Addressing mode delays

        // Check for page boundary crossings
        // if this is a branch instruction
        if self.mode == AddressingMode::Rel {
            if ctx.pc >> 8 != last_pc >> 8 {
                cycles += 1;
            }
            else if ctx.pc != last_pc {
                cycles += 1;
            }
        }
        
        ctx.pc += self.mode.len() as u16;

        cycles
    }
}

macro_rules! raw_inst {
	($v:ident $name:ident $val:tt $mode:literal $exc:expr) => (
		assert!($v.insert(
            $val,
            crate::Instruction {
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
                crate::Instruction {
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
			pub fn gen_excs() -> crate::Arch {
				let mut excs = std::collections::HashMap::new();
				$(
					inst!(excs $($in)+);
				)*

                let parsed = parse_instruction_list();

                for (n, m, cycles) in parsed {
                    let i = excs.iter_mut().find(|(_, instr)| instr.opcode == n && instr.mode == m).unwrap_or_else(|| panic!("Couldn't find {}, {:?}", n, m));
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

fn BRANCH(ctx: &mut Context, offset: u8, cond: bool) {
    if cond { 
        let neg = offset >> 7 != 0;
        if neg {
            ctx.pc -= (!offset + 1) as u16
        }
        else {
            ctx.pc += offset as u16
        }
    }
}

fn COMPARE(ctx: &mut Context, lhs: u8, rhs: u8) {
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
}

fn ADC_imm(ctx: &mut Context, rhs: u8) {
    let lhs = ctx.acc;
    let res1 = lhs as u16 + rhs as u16 + ctx.get_carry() as u16;
    let result = (res1 % 0x100) as u8;
    let did_carry = res1 & 0x100 != 0;
    let did_overflow = (lhs ^ result) & (rhs ^ result) & 0x80 != 0;
    ctx.update_flags(result);
    ctx.set_carry(did_carry);
    ctx.set_overflow(did_overflow);
    ctx.acc = result;
}

fn SUB(ctx: &mut Context, rhs: u8) {
    let lhs = ctx.acc;
    let res1 = lhs as u16 + (!rhs) as u16 + ctx.get_carry() as u16;
    let result = (res1 % 0x100) as u8;
    let did_carry = res1 & 0x100 != 0;
    let did_overflow = (lhs ^ result) & (!rhs ^ result) & 0x80 != 0;
    ctx.update_flags(result);
    ctx.set_carry(did_carry);
    ctx.set_overflow(did_overflow);
    ctx.acc = result;
}

fn ror(ctx: &mut Context, mut value: u8) -> u8 {
    let carry_out = value & 1 != 0;
    value >>= 1;
    value |= (ctx.get_carry() as u8) << 7;
    ctx.update_flags(value);
    ctx.set_carry(carry_out);
    value
}

fn rol(ctx: &mut Context, mut value: u8) -> u8 {
    let carry_out = value & 0x80 != 0;
    value <<= 1;
    value |= ctx.get_carry() as u8;
    ctx.update_flags(value);
    ctx.set_carry(carry_out);
    value
}

fn lsr(ctx: &mut Context, mut value: u8) -> u8 {
    let carry = value & 1 != 0;
    value >>= 1;
    ctx.update_flags(value);
    ctx.set_carry(carry);
    value
}

fn BIT(ctx: &mut Context, addr: u16) {
    let value = ctx.read(addr);
    ctx.set_neg(value & 0x80 != 0);
    ctx.set_overflow(value & 0x40 != 0);
    ctx.set_zero(ctx.acc & value == 0);
}

fn AAX(ctx: &mut Context, addr: u16) {
    let result = ctx.acc & ctx.x;
    ctx.write(addr, result);
}

fn DCP(ctx: &mut Context, addr: u16) {
    ctx.write(addr, (Wrapping(ctx.read(addr)) - Wrapping(1)).0);
    COMPARE(ctx, ctx.acc, ctx.read(addr));
}

fn ISC(ctx: &mut Context, addr: u16) {
    ctx.write(addr, (Wrapping(ctx.read(addr)) + Wrapping(1)).0);
    SUB(ctx, ctx.read(addr));
}

fn SLO(ctx: &mut Context, addr: u16) {
    let mut m = ctx.read(addr);
    let c = m >> 7;
    m <<= 1;
    ctx.write(addr, m); 
    ctx.acc |= m;
    ctx.update_flags(ctx.acc);
    ctx.set_carry(c != 0);
}

fn RLA(ctx: &mut Context, addr: u16) {
    let result = rol(ctx, ctx.read(addr));
    ctx.write(addr, result);
    ctx.acc &= result;
    ctx.update_flags(ctx.acc);
}

fn SRE(ctx: &mut Context, addr: u16) {
    let mut m = ctx.read(addr);
    let carry = m & 1 != 0;
    m >>= 1;
    ctx.write(addr, m);
    ctx.set_carry(carry);
    ctx.acc ^= m;
    ctx.update_flags(ctx.acc);
}

fn RRA(ctx: &mut Context, addr: u16) {
    let result = ror(ctx, ctx.read(addr));
    ctx.write(addr, result);
    ADC_imm(ctx, result);
}

fn asl(ctx: &mut Context, mut val: u8) -> u8 {
    let c = val >> 7 != 0;
    val <<= 1;
    ctx.update_flags(val);
    ctx.set_carry(c);
    val
}

fn BPL(ctx: &mut Context, offset: u8) {
    BRANCH(ctx, offset, !ctx.get_neg())
}
fn BMI(ctx: &mut Context, offset: u8) {
    BRANCH(ctx, offset,  ctx.get_neg())
}
fn BVC(ctx: &mut Context, offset: u8) {
    BRANCH(ctx, offset, !ctx.get_overflow());
}
fn BVS(ctx: &mut Context, offset: u8) {
    BRANCH(ctx, offset,  ctx.get_overflow());
}
fn BCC(ctx: &mut Context, offset: u8) {
    BRANCH(ctx, offset, !ctx.get_carry());
}
fn BCS(ctx: &mut Context, offset: u8) {
    BRANCH(ctx, offset,  ctx.get_carry());
}
fn RTI(ctx: &mut Context) {
    ctx.status = (ctx.pop() & !(0b01 << 4)) | 0b10 << 4;
    ctx.pc = ctx.pop() as u16;
    ctx.pc |= (ctx.pop() as u16) << 8;
    ctx.pc -= 1;
}
fn JSR(ctx: &mut Context, addr: u16) {
    ctx.push(((ctx.pc + 2) >> 8) as u8);
    ctx.push(((ctx.pc + 2) & 0xFF) as u8);
    ctx.pc = addr - 3;
}
fn RTS(ctx: &mut crate::Context) {
    let lo = ctx.pop() as u16;
    let hi = ctx.pop() as u16;
    ctx.pc = hi << 8 | lo;
}
fn LDY_imm(ctx: &mut Context, imm: u8) { ctx.y = imm; ctx.update_flags(ctx.y); }
fn CPY_imm(ctx: &mut Context, imm: u8) { COMPARE(ctx, ctx.y, imm) }
fn BNE(ctx: &mut Context, offset: u8) { BRANCH(ctx, offset, !ctx.get_zero()) }
fn CPX_imm(ctx: &mut Context, imm: u8) { COMPARE(ctx, ctx.x, imm) }
fn BEQ(ctx: &mut Context, offset: u8) { BRANCH(ctx, offset,  ctx.get_zero()) }

fn ORA(ctx: &mut Context, addr: u16) { ORA_imm(ctx, ctx.read(addr)) }
fn ORA_imm(ctx: &mut Context, imm: u8) { ctx.acc |= imm; ctx.update_flags(ctx.acc); }
fn AND(ctx: &mut Context, addr: u16) { AND_imm(ctx, ctx.read(addr)) }
fn AND_imm(ctx: &mut Context, imm: u8) { ctx.acc &= imm; ctx.update_flags(ctx.acc); }
fn EOR(ctx: &mut Context, addr: u16) { EOR_imm(ctx, ctx.read(addr)) }
fn EOR_imm(ctx: &mut Context, imm: u8) { ctx.acc ^= imm; ctx.update_flags(ctx.acc); }
fn LDA(ctx: &mut Context, addr: u16) { LDA_imm(ctx, ctx.read(addr)) }
fn LDA_imm(ctx: &mut Context, imm: u8) { ctx.acc = imm; ctx.update_flags(ctx.acc); }
fn CMP(ctx: &mut Context, addr: u16) { CMP_imm(ctx, ctx.read(addr)) }
fn CMP_imm(ctx: &mut Context, imm: u8) { COMPARE(ctx, ctx.acc, imm); }
fn SBC(ctx: &mut Context, addr: u16) { SBC_imm(ctx, ctx.read(addr)) }
fn SBC_imm(ctx: &mut Context, imm: u8) { SUB(ctx, imm); }
fn ADC(ctx: &mut Context, addr: u16) { ADC_imm(ctx, ctx.read(addr)); }
fn STA(ctx: &mut Context, addr: u16) { ctx.write(addr, ctx.acc); }

fn LDX_imm(ctx: &mut Context, imm: u8) { ctx.x = imm; ctx.update_flags(ctx.x); }

fn LAX(ctx: &mut Context, addr: u16) { ctx.acc = ctx.read(addr); ctx.x = ctx.acc; ctx.update_flags(ctx.x); }

fn STY(ctx: &mut Context, addr: u16) { ctx.write(addr, ctx.y); }
fn LDY(ctx: &mut Context, addr: u16) { ctx.y = ctx.read(addr); ctx.update_flags(ctx.y); }
fn TYA(ctx: &mut Context) { ctx.acc = ctx.y; ctx.update_flags(ctx.acc); }
fn TXA(ctx: &mut Context) { ctx.acc = ctx.x; ctx.update_flags(ctx.acc); }
fn TAY(ctx: &mut Context) { ctx.y = ctx.acc; ctx.update_flags(ctx.y); }
fn TAX(ctx: &mut Context) { ctx.x = ctx.acc; ctx.update_flags(ctx.x); }
fn TSX(ctx: &mut Context) { ctx.x = ctx.sp; ctx.update_flags(ctx.x); }
fn TXS(ctx: &mut Context) { ctx.sp = ctx.x; }

fn CPY(ctx: &mut Context, addr: u16) { COMPARE(ctx, ctx.y, ctx.read(addr)); }
fn CPX(ctx: &mut Context, addr: u16) { COMPARE(ctx, ctx.x, ctx.read(addr)); }

fn ASL(ctx: &mut Context, addr: u16) { let val = asl(ctx, ctx.read(addr)); ctx.write(addr, val); }
fn ASL_imp(ctx: &mut Context) { ctx.acc = asl(ctx, ctx.acc); }
fn ROL(ctx: &mut Context, addr: u16) { let val = rol(ctx, ctx.read(addr)); ctx.write(addr, val); }
fn ROL_imp(ctx: &mut Context) { ctx.acc = rol(ctx, ctx.acc); }
fn LSR(ctx: &mut Context, addr: u16) { let val = lsr(ctx, ctx.read(addr)); ctx.write(addr, val); }
fn LSR_imp(ctx: &mut Context) { ctx.acc = lsr(ctx, ctx.acc); }
fn ROR(ctx: &mut Context, addr: u16) { let val = ror(ctx, ctx.read(addr)); ctx.write(addr, val); }
fn ROR_imp(ctx: &mut Context) { ctx.acc = ror(ctx, ctx.acc); }

fn STX(ctx: &mut crate::Context, addr: u16) { ctx.write(addr, ctx.x); }
fn LDX(ctx: &mut crate::Context, addr: u16) { ctx.x = ctx.read(addr); ctx.update_flags(ctx.x) }

fn DEC(ctx: &mut crate::Context, addr: u16) { ctx.write(addr, (Wrapping(ctx.read(addr)) - Wrapping(1)).0); ctx.update_flags(ctx.read(addr)); }
fn INC(ctx: &mut crate::Context, addr: u16) { ctx.write(addr, (Wrapping(ctx.read(addr)) + Wrapping(1)).0); ctx.update_flags(ctx.read(addr)); }
fn DEY(ctx: &mut crate::Context) { ctx.y = (Wrapping(ctx.y) - Wrapping(1)).0; ctx.update_flags(ctx.y); }
fn INY(ctx: &mut crate::Context) { ctx.y = (Wrapping(ctx.y) + Wrapping(1)).0; ctx.update_flags(ctx.y); }
fn INX(ctx: &mut crate::Context) { ctx.x = (Wrapping(ctx.x) + Wrapping(1)).0; ctx.update_flags(ctx.x); }
fn DEX(ctx: &mut crate::Context) { ctx.x = (Wrapping(ctx.x) - Wrapping(1)).0; ctx.update_flags(ctx.x); }

fn PHP(ctx: &mut crate::Context) { ctx.push(ctx.status | (0b11 << 4)) }
fn PLP(ctx: &mut crate::Context) { ctx.status = (ctx.pop() & !(0b01 << 4)) | (0b10 << 4); }
fn PHA(ctx: &mut crate::Context) { ctx.push(ctx.acc); }
fn PLA(ctx: &mut crate::Context) { ctx.acc = ctx.pop(); ctx.update_flags(ctx.acc); }

fn CLC(ctx: &mut Context) { ctx.set_carry(false); }
fn SEC(ctx: &mut Context) { ctx.set_carry(true); }
fn CLI(ctx: &mut Context) { ctx.status &= !(1 << 2); }
fn SEI(ctx: &mut Context) { ctx.status |= (1 << 2); }
fn CLV(ctx: &mut Context) { ctx.set_overflow(false); }
fn CLD(ctx: &mut Context) { ctx.status &= !(1 << 3); }
fn SED(ctx: &mut Context) { ctx.status |= 1 << 3; }

fn JMP_abs(ctx: &mut crate::Context, addr: u16) { ctx.pc = addr - 3; }
fn JMP_ind(ctx: &mut crate::Context, addr: u16) { ctx.pc = ctx.read_wide(addr) - 3; }

fn NOP(_: &mut Context) {}
fn DOP_IMM(_: &mut Context, _: u8) {}
fn DOP(_: &mut Context, _: u16) {}
fn TOP_ADDR(_: &mut Context, _: u16) {}

inst_list! {
    { BRK 0x00 &NOP }
    { BPL 0x10 rel &BPL }
    { JSR 0x20 abs &JSR }
    { BMI 0x30 rel &BMI }
    { RTI 0x40 &RTI }
    { BVC 0x50 rel &BVC }
    { RTS 0x60 &RTS }
    { BVS 0x70 rel &BVS }
    { DOP 0x80 imm &DOP_IMM }
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
    { LDX 0xA2 imm &LDX_imm }
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
    { TOP 0x0C abs abs,X &TOP_ADDR }
    { BIT 0x2C abs &BIT }
    { TOP 0x3C abs,X &TOP_ADDR }
    { JMP 0x4C abs &JMP_abs }
    { TOP 0x5C abs,X &TOP_ADDR }
    { JMP 0x6C ind &JMP_ind }
    { TOP 0x7C abs,X &TOP_ADDR }
    { STY 0x8C abs &STY }
    { LDY 0xAC abs abs,X &LDY }
    { CPY 0xCC abs &CPY }
    { TOP 0xDC abs,X &TOP_ADDR }
    { CPX 0xEC abs &CPX }
    { TOP 0xFC abs,X &TOP_ADDR }
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
