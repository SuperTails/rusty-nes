extern crate nes;

use nes::rom::Rom;
use nes::instructions;
use nes::Context;
use std::path::Path;

fn main() {
    let rom = Rom::new(Path::new("./SMB.nes")).unwrap_or_else(|err| {
        println!("Error loading ROM: {}", err);
        std::process::exit(1);
    });

    println!("ROM has mapper {} and submapper {}", rom.mapper, rom.submapper);
    println!("PRG ROM has size {}, CHR ROM has size {}, and misc ROM has size {}", rom.prg_rom.len(), rom.chr_rom.len(), rom.misc_rom.len());

    let mut ctx = Context::from(rom);

    /*ctx.pc = 0xC000;
    ctx.sp = 0xFD;
    ctx.state = nes::State::Run;
    ctx.status = 0x24;*/

    let arch = instructions::gen_excs();

    //let mut expected: Vec<_> = std::fs::read_to_string("./nestest.log").unwrap().lines().map(parse_log_line).collect();

    for i in 0.. {
        /* Program Counter, Acc, X, Y, Status, Stack Pointer */
        //let actual = (ctx.pc, ctx.acc, ctx.x, ctx.y, ctx.status, ctx.sp);
        //assert_eq!(expected[i], actual);
        ctx.next(&arch);
    }
}

fn parse_log_line(s: &str) -> (u16, u8, u8, u8, u8, u8) {
    let pc = u16::from_str_radix(&s[0..4], 16).unwrap();
    let a = u8::from_str_radix(&s[50..52], 16).unwrap();
    let x = u8::from_str_radix(&s[55..57], 16).unwrap();
    let y = u8::from_str_radix(&s[60..62], 16).unwrap();
    let p = u8::from_str_radix(&s[65..67], 16).unwrap();
    let s = u8::from_str_radix(&s[71..73], 16).unwrap();

    (pc, a, x, y, p, s)
}
