extern crate nes;

use nes::rom::Rom;
use nes::instruction::instructions;
use nes::Config;
use nes::Context;
use std::path::Path;

const VERIFY: bool = false;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    let config = Config::from_args(&args).unwrap_or_else(|err| {
        println!("Error parsing arguments: {:?}", err);
        std::process::exit(1)
    });

    let rom = Rom::new(&config.rom_path).unwrap_or_else(|err| {
        println!("Error loading ROM: {}", err);
        std::process::exit(1)
    });

    println!("ROM has mapper {} and submapper {}", rom.mapper, rom.submapper);
    println!("PRG ROM has size {}, CHR ROM has size {}, and misc ROM has size {}", rom.prg_rom.len(), rom.chr_rom.len(), rom.misc_rom.len());

    let mut ctx = Context::from(rom);

    if VERIFY {
        ctx.pc = 0xC000;
        ctx.sp = 0xFD;
        ctx.state = nes::State::Run;
        ctx.status = 0x24;
        ctx.cycle = 7;
        let expected: Vec<_> = std::fs::read_to_string("./nestest.log").unwrap().lines().map(parse_log_line).collect();

        for i in 0.. {
            /* Program Counter, Acc, X, Y, Status, Stack Pointer */
            let actual = (ctx.pc, ctx.acc, ctx.x, ctx.y, ctx.status, ctx.sp, ctx.cycle);
            assert_eq!(expected[i], actual);
            ctx.next();
        }

    }
    else {
        loop {
            ctx.next();
        }
    }
}

fn parse_log_line(line: &str) -> (u16, u8, u8, u8, u8, u8, usize) {
    let pc = u16::from_str_radix(&line[0..4], 16).unwrap();
    let a = u8::from_str_radix(&line[50..52], 16).unwrap();
    let x = u8::from_str_radix(&line[55..57], 16).unwrap();
    let y = u8::from_str_radix(&line[60..62], 16).unwrap();
    let p = u8::from_str_radix(&line[65..67], 16).unwrap();
    let s = u8::from_str_radix(&line[71..73], 16).unwrap();
    let cyc = usize::from_str_radix(&line[90..], 10).unwrap();

    (pc, a, x, y, p, s, cyc)
}
