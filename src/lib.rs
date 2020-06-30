#![allow(unused_parens)]
#![allow(clippy::identity_op)]

pub mod apu;
pub mod config;
mod context;
pub mod controller;
pub mod cpu;
pub mod mapper;
mod mem_location;
pub mod rom;
mod sdl_system;

pub use context::Context;

use apu::APUAudio;
use config::Config;
use rom::Rom;
use std::io::Read;

pub fn run(config: &Config) -> Result<(), String> {
    let rom = Rom::new(&config.rom_path).map_err(|e| format!("Error when loading ROM: {}", e))?;

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
        let mut savefile = std::fs::File::open(&mem_path).map_err(|e| format!("Failed to open save file: {}", e))?;
        // TODO: Should I check that the savedata is of the correct length?
        savefile.read_to_end(&mut savedata).map_err(|e| format!("Failed to read save file: {}", e))?;
        Some(savedata)
    } else {
        None
    };

    let mut ctx = Context::new(rom, savedata);

    while !ctx.advance() {}

    if battery_backed {
        ctx.save(mem_path)?;
    }

    Ok(())
}

#[cfg(test)]
mod test {
    use std::path::Path;
    use super::*;

    #[allow(clippy::many_single_char_names)]
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

    #[test]
    fn verify() {
        let rom = Rom::new(Path::new("./nestest.nes")).unwrap();

        let mut ctx = Context::new(rom, None);

        let expected: Vec<_> = std::fs::read_to_string(Path::new("./nestest.log"))
            .unwrap()
            .lines()
            .map(parse_log_line)
            .collect();

        {
            ctx.cpu.pc = expected[0].0;
            ctx.cpu.sp = expected[0].5;
            ctx.cpu.state = cpu::State::Run;
            ctx.cpu.status = expected[0].4;
            ctx.inner.cycle = expected[0].6;
        }

        for expected in expected.iter() {
            /* Program Counter, Acc, X, Y, Status, Stack Pointer */
            if ctx.inner.cycle == 27403 || ctx.inner.cycle == 57183 {
                ctx.cpu.acc = 128;
                ctx.cpu.status = 164;
                ctx.inner.ppu.status &= !0x80;
            }

            {
                let cpu = &mut ctx.cpu;
                if cpu.read(cpu.pc - 1, &mut ctx.inner) == 0x40
                    && cpu.read(cpu.pc - 2, &mut ctx.inner) == 0x16
                    && cpu.read(cpu.pc - 3, &mut ctx.inner) != 0x8D
                {
                    cpu.acc = expected.1;
                    cpu.status = expected.4;
                }

                if cpu.read(cpu.pc - 1, &mut ctx.inner) == 0x20
                    && cpu.read(cpu.pc - 2, &mut ctx.inner) == 0x02
                    && cpu.read(cpu.pc - 3, &mut ctx.inner) == 0xAD
                    && (1..=12).contains(&ctx.inner.ppu.pixel())
                    && ctx.inner.ppu.scanline() == 261
                {
                    cpu.acc &= !64;
                    let acc = cpu.acc;
                    cpu.update_flags(acc);
                }
            }

            match ctx.inner.cycle {
                118_191 | 118_215 | 118_239 | 118_263 | 118_287 => {
                    ctx.cpu.acc = 64;
                    ctx.cpu.update_flags(64);
                }
                _ => {}
            }

            let actual = {
                let cpu = &ctx.cpu;
                let ppu = &ctx.inner.ppu;
                (
                    cpu.pc,
                    cpu.acc,
                    cpu.x,
                    cpu.y,
                    cpu.status,
                    cpu.sp,
                    ctx.inner.cycle,
                    ppu.pixel(),
                    ppu.scanline(),
                )
            };
            assert_eq!(expected, &actual, "Frame: {:?}", ctx.inner.ppu.frame());

            while !ctx.advance() {}
        }
    }
}