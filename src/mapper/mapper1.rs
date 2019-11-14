use std::cell::RefCell;
use crate::Context;
use crate::mem_location::{RomLocation, MemLocation};
use super::{Mapped, MapperResult, MirrorMode};
use num_traits::cast::FromPrimitive;

/*
 * MAPPER 1
 *
 * CPU:
 * PRG-RAM: $6000..=$7FFF: 8KiB (Optional)
 *
 * PRG-ROM: $8000..=$BFFF: 16KiB (Switchable or fixed to first bank)
 *
 * PRG-ROM: $C000..=$FFFF: 16KiB (Switchable or fixed to last bank)
 *
 * PPU:
 * CHR-ROM: $0000..=$0FFF: 4KiB, Switchable
 *
 * CHR-ROM: $1000..=$1FFF: 4KiB, Switchable
 *
 */
// TODO: Variants of this
pub struct Mapper1 {
    prg_ram: RefCell<Vec<u8>>, // 8K, Optional

    prg_rom: RefCell<Vec<u8>>, // 32K (??)

    chr: RefCell<Vec<u8>>, // 8K (??)

    chr_is_rom: bool,

    data: RefCell<Mapper1Impl>,

    last_mmio_write: RefCell<usize>,
}

#[derive(FromPrimitive, Debug, Clone, Copy, PartialEq, Eq)]
enum PrgRomMode {
    DoubleSize = 1,
    FixFirst,
    FixLast,
}

#[derive(FromPrimitive, Debug, Clone, Copy, PartialEq, Eq)]
enum ChrMode {
    DoubleSize = 0,
    Individual,
}

struct Mapper1Impl {
    shifter: u8,
    shift_count: u8,

    mirror_mode: MirrorMode,
    prg_rom_mode: PrgRomMode,
    chr_mode: ChrMode,

    chr_bank_0: u8,
    chr_bank_1: u8,
    prg_bank: u8,
    ram_enable: bool,
}

impl Mapper1Impl {
    pub fn write_control(&mut self, value: u8) {
        let mut prg_rom_mode = (value >> 2) & 0b11;
        if prg_rom_mode == 0 {
            prg_rom_mode = 1
        }

        self.mirror_mode =
            MirrorMode::from_u8(value & 0b11).unwrap();
        self.prg_rom_mode =
            PrgRomMode::from_u8(prg_rom_mode).unwrap();
        self.chr_mode =
            ChrMode::from_u8((value >> 4) & 0b1).unwrap();
    }

    pub fn update_shifter(&mut self, value: u8) -> Option<u8> {
        if value & 0x80 != 0 {
            self.shifter = 0;
            self.shift_count = 0;
            // TODO: Is this correct? 
            self.prg_rom_mode = PrgRomMode::FixLast;
            None
        } else {
            self.shifter >>= 1;
            self.shifter |= (value & 1) << 4;

            self.shift_count += 1;

            // TODO: Ignore consecutive writes?
            if self.shift_count == 5 {
                self.shift_count = 0;
                Some(std::mem::replace(&mut self.shifter, 0))
            }
            else {
                None
            }
        }

    }
}

impl Mapper1 {
    pub fn new(prg_rom: Vec<u8>, chr: Vec<u8>, chr_is_rom: bool) -> Mapper1 {
        assert!(prg_rom.len() == 0x20000 || prg_rom.len() == 0x40000);

        let mut prg_ram = [90; 0x2000].to_vec();
        for val in prg_ram[10..=0x51D].iter_mut() { *val = 0; }
        for val in prg_ram[0x521..=0x525].iter_mut() { *val = 165; }
        for val in prg_ram[0x52A..=0x52F].iter_mut() { *val = 255; }
        for val in prg_ram[0x02..=0x19].iter_mut() { *val = 36; }
        prg_ram[0x1FFF] = 165;
        prg_ram[0x0524] = 1;
        prg_ram[0x0526] = 1;
        prg_ram[0x0528] = 1;
        prg_ram[0x525] = 32;
        prg_ram[0x527] = 32;
        prg_ram[0x529] = 32;

        Mapper1 {
            last_mmio_write: RefCell::new(0),
            prg_ram: RefCell::new(prg_ram),
            prg_rom: RefCell::new(prg_rom),
            chr: RefCell::new(chr),
            chr_is_rom,
            data: RefCell::new(Mapper1Impl {
                mirror_mode: MirrorMode::OneScreenLowerBank,
                prg_rom_mode: PrgRomMode::FixLast,
                chr_mode: ChrMode::DoubleSize,
                shifter: 0,
                shift_count: 0,
                chr_bank_0: 0,
                chr_bank_1: 0,
                prg_bank: 0,
                ram_enable: false,
            }),
        }
    }
}

// TODO: Use mirror mode
pub enum Mapper1Location<'a> {
    Ram((&'a RefCell<Vec<u8>>, usize)),
    Mmio((&'a Mapper1, usize, &'a Context)),
}

impl<'a> Mapper1Location<'a> {
    // PRG ROM is mapped in CPU space from $8000 to $FFFF
    fn get_prg_rom(mode: &PrgRomMode, addr: usize, prg_bank: u8, prg_rom: &Vec<u8>) -> u8 {
        match mode {
            PrgRomMode::DoubleSize => {
                // Switch 32KiB at $8000
                let bank_size = 0x8000; // 32KiB

                // Ignore lower bit since we're switching a larger size
                let selected = (prg_bank >> 1) as usize;

                let bank_start = (bank_size * selected) % prg_rom.len();
                let bank_end = bank_start + bank_size;
                let bank = &prg_rom[bank_start..bank_end];

                bank[(addr - 0x8000) as usize]
            }
            PrgRomMode::FixFirst => {
                // Fix first bank at $8000,
                // Switch 16KiB at $C000
                match addr {
                    0x8000..=0xBFFF => prg_rom[(addr - 0x8000) as usize],
                    _ => {
                        let bank_start =
                            (0x4000 * prg_bank as usize) % prg_rom.len();
                        prg_rom[bank_start + addr as usize - 0xC000]
                    }
                }
            }
            PrgRomMode::FixLast => {
                // Fix last bank at $C000,
                // Switch 16KiB at $8000
                match addr {
                    0xC000..=0xFFFF => {
                        let bank = &prg_rom[(prg_rom.len() - 0x4000)..];
                        bank[(addr - 0xC000) as usize]
                    }
                    _ => {
                        let bank_start =
                            (0x4000 * prg_bank as usize) % prg_rom.len();
                        let bank_end = bank_start + 0x4000;
                        let bank = &prg_rom[bank_start..bank_end];
                        bank[(addr - 0x8000) as usize]
                    }
                }
            }
        }
    }
}

impl<'a> MemLocation<'a> for Mapper1Location<'a> {
    fn read(&mut self) -> u8 {
        match self {
            Mapper1Location::Ram((t, addr)) => t.borrow()[*addr as usize],
            Mapper1Location::Mmio((mapper, addr, _)) => {
                Mapper1Location::get_prg_rom(
                    &mapper.data.borrow().prg_rom_mode,
                    *addr as usize,
                    mapper.data.borrow().prg_bank,
                    &mapper.prg_rom.borrow()
                )
            }
        }
    }

    fn write(&mut self, value: u8) {
        match self {
            Mapper1Location::Ram((t, addr)) => t.borrow_mut()[*addr as usize] = value,
            Mapper1Location::Mmio((mapper, addr, context)) => {
                // Ignore consecutive writes
                let ignoring = context.cycle == *mapper.last_mmio_write.borrow() + 1;
                *mapper.last_mmio_write.borrow_mut() = context.cycle;
                if ignoring { return };

                let shift_result = mapper.data.borrow_mut().update_shifter(value);

                if let Some(shifter) = shift_result {
                    let register = (*addr >> 13) & 0b11;

                    let mut data = mapper.data.borrow_mut();
                    
                    // Register is bits 14 and 13 of the address
                    match register {
                        0 => {
                            data.write_control(shifter);
                            println!(
                                "Modes are now: {:?}, {:?}, {:?}",
                                data.mirror_mode, data.prg_rom_mode, data.chr_mode
                            );
                        }
                        1 => {
                            data.chr_bank_0 = shifter;
                            println!("Set CHR bank 0 to {}", shifter);
                        }
                        2 => {
                            data.chr_bank_1 = shifter;
                            println!("Set CHR bank 1 to {}", shifter);
                        }
                        3 => {
                            data.prg_bank = shifter & 0b1111;
                            data.ram_enable = shifter & 0b10000 != 0;
                            println!("Set PRG bank to {}, RAM state is now {}", data.prg_bank, data.ram_enable);
                        }
                        _ => unreachable!(),
                    } 
                }
            }
        }
    }
}

impl Mapped for Mapper1 {
    fn mem_cpu<'a>(&'a self, addr: u16, context: &'a Context) -> MapperResult<'a> {
        match addr {
            0x4020..=0x7FFF => {
                if self.data.borrow().ram_enable {
                    println!("RAM is disabled");
                    RomLocation { mem: 0 }.into()
                }
                else {
                    let relative = (addr - 0x4000) as usize % self.prg_ram.borrow().len();
                    //println!("Reading RAM at {:#X} = {:#X}", relative, self.prg_ram.borrow()[relative]);
                    Mapper1Location::Ram((&self.prg_ram, relative)).into()
                }
            }
            0x8000..=0xFFFF => Mapper1Location::Mmio((self, addr as usize, context)).into(),
            _ => panic!(),
        }
    }

    fn mem_ppu<'a>(&'a self, addr: u16) -> MapperResult<'a> {
        if addr >= 0x2000 {
            panic!();
        }

        let data = self.data.borrow();

        let (chr, addr) = match data.chr_mode {
            ChrMode::Individual => {
                // Switch 1KiB banks at the first and second KiB of PPU memory space
                let bank_select = if (0x0000..=0x0FFF).contains(&addr) {
                    data.chr_bank_0
                } else {
                    data.chr_bank_1
                };

                let bank_start = 0x1000 * (bank_select as usize);
                (&self.chr, (addr % 0x1000) as usize + bank_start)
            }
            ChrMode::DoubleSize => {
                let bank_select = data.chr_bank_0 >> 1;

                let bank_start = 0x2000 * (bank_select as usize);

                (&self.chr, addr as usize + bank_start)
            }
        };

        if self.chr_is_rom {
            let chr = chr.borrow();
            let mem = chr[addr % chr.len()];
            RomLocation { mem }.into()
        }
        else {
            Mapper1Location::Ram((chr, addr)).into()
        }
    }

    // TODO: The one-screen modes
    fn mirror_mode(&self) -> MirrorMode {
        self.data.borrow().mirror_mode
    }
}


