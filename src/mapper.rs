use num_traits::cast::FromPrimitive;
use crate::mem_location::*;
use std::cell::RefCell;

pub trait Mapped {
    fn mem_cpu<'a>(&'a self, addr: u16) -> Box<dyn MemLocation + 'a>;

    fn mem_ppu<'a>(&'a self, addr: u16) -> Box<dyn MemLocation + 'a>;

    fn is_vert_mirrored(&self) -> bool;
}

/* 
 * MAPPER 0 
 *
 * PRG-ROM: 16KiB or 32KiB
 *  not bankswitched
 *
 * PRG-RAM:  2KiB or  4KiB
 *  not bankswitched
 *
 * CHR-ROM:  8Kib
 *  not bankswitched
 *
 */
pub struct Mapper0 {
    prg_rom: Vec<u8>,
    prg_ram: RefCell<Vec<u8>>,
    chr_rom: Vec<u8>,
}

impl Mapper0 {
    pub fn new(prg_rom: Vec<u8>, chr_rom: Vec<u8>) -> Mapper0 {
        assert!(prg_rom.len() == 0x4000 || prg_rom.len() == 0x8000);
        assert!(chr_rom.len() == 0x2000);

        /* TODO: Figure out how much RAM to allocate */
        Mapper0 {
            prg_rom,
            prg_ram: RefCell::new([0; 0x800].to_vec()),
            chr_rom,
        }
    }
}

enum Mapper0Location<'a> {
    Ram((&'a RefCell<Vec<u8>>, u16)),
    Rom(&'a u8)
}

impl MemLocation for Mapper0Location<'_> {
    fn read(&mut self) -> u8 {
        match self {
            Mapper0Location::Ram((data, addr)) => data.borrow()[*addr as usize],
            Mapper0Location::Rom(t) => **t,
        }
    }

    fn write(&mut self, value: u8) {
        match self {
            Mapper0Location::Ram((data, addr)) => data.borrow_mut()[*addr as usize] = value,
            Mapper0Location::Rom(_) => println!("Attempted write to ROM"),
        }
    }
}

impl Mapped for Mapper0 {
    fn mem_cpu<'a>(&'a self, addr: u16) -> Box<dyn MemLocation + 'a> {
        let prg_rom_len = self.prg_rom.len();
        match addr {
            0x4020..=0x5FFF => panic!("CPU Read from unmapped cart memory at {:#06X}", addr),
            0x6000..=0x7FFF => Box::new(Mapper0Location::<'a>::Ram((&self.prg_ram, (addr - 0x6000)))),
            0x8000..=0xFFFF => Box::new(Mapper0Location::<'a>::Rom(&self.prg_rom[(addr - 0x8000) as usize % prg_rom_len])),
            _ => panic!("CPU access to memory not in cart at {:#06X}", addr),
        }
    }

    fn mem_ppu<'a>(&'a self, addr: u16) -> Box<dyn MemLocation + 'a> {
        match addr {
            0x0000..=0x1FFF => Box::new(Mapper0Location::<'a>::Rom(&self.chr_rom[addr as usize])),
            _ => panic!("PPU access to memory not in cart at {:#06X}", addr),
        }
    }

    // TODO: Figure this out
    fn is_vert_mirrored(&self) -> bool {
        true
    }
}

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

#[derive(FromPrimitive, Debug, Clone, Copy, PartialEq, Eq)]
enum MirrorMode {
    OneScreenLowerBank = 0,
    OneScreenUpperBank,
    Vertical,
    Horizontal,
}

#[derive(FromPrimitive, Debug, Clone, Copy, PartialEq, Eq)]
enum PrgRomMode {
    DoubleSize = 1,
    FixFirst,
    FixLast
}

#[derive(FromPrimitive, Debug, Clone, Copy, PartialEq, Eq)]
enum ChrMode {
    DoubleSize = 0,
    Individual
}


// TODO: Variants of this
pub struct Mapper1 {
    prg_ram: RefCell<Vec<u8>>, // 8K, Optional

    prg_rom: RefCell<Vec<u8>>, // 32K (??)

    chr_rom: RefCell<Vec<u8>>, // 8K (??)

    data: RefCell<Mapper1Impl>
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
}

impl Mapper1 {
    pub fn new(prg_rom: Vec<u8>, chr_rom: Vec<u8>) -> Mapper1 {
        assert!(prg_rom.len() == 0x20000 || prg_rom.len() == 0x40000);
        assert_eq!(chr_rom.len(), 0x2000);

        Mapper1 {
            prg_ram: RefCell::new([0; 0x2000].to_vec()),
            prg_rom: RefCell::new(prg_rom),
            chr_rom: RefCell::new(chr_rom),
            data: RefCell::new(Mapper1Impl {
                mirror_mode: MirrorMode::OneScreenLowerBank,
                prg_rom_mode: PrgRomMode::FixLast,
                chr_mode: ChrMode::DoubleSize,
                shifter: 0,
                shift_count: 0,
                chr_bank_0: 0,
                chr_bank_1: 0,
                prg_bank: 0,
            }),
        }
    }
}

enum Mapper1Location<'a> {
    Ram((&'a RefCell<Vec<u8>>, u16)),
    Mmio((&'a Mapper1, u16))
}

impl MemLocation for Mapper1Location<'_> {
    fn read(&mut self) -> u8 {
        match self {
            Mapper1Location::Ram((t, addr)) => t.borrow()[*addr as usize],
            Mapper1Location::Mmio((mapper, addr)) => {
                let data = mapper.data.borrow();
                let prg_rom = mapper.prg_rom.borrow();
                // TODO: Use mirror mode
                match data.prg_rom_mode {
                    PrgRomMode::DoubleSize => {
                        // Switch 32KiB at $8000
                        let bank_start = (0x8000 * ((data.prg_bank & 0b01110) >> 1) as usize) % mapper.prg_rom.borrow().len();
                        let bank_end = bank_start + 0x8000;
                        let bank = &prg_rom[bank_start..bank_end];
                        bank[(*addr - 0x8000) as usize]
                    },
                    PrgRomMode::FixFirst => {
                        // Fix first bank at $8000,
                        // Switch 16KiB at $C000
                        match addr {
                            0x8000..=0xBFFF => {
                                mapper.prg_rom.borrow()[(*addr - 0x8000) as usize]
                            },
                            _ => {
                                let bank_start = (0x4000 * (data.prg_bank & 0b01111) as usize) % prg_rom.len();
                                prg_rom[bank_start + *addr as usize - 0x8000]
                            }
                        }
                    },
                    PrgRomMode::FixLast => {
                        // Fix last bank at $C000,
                        // Switch 16KiB at $8000
                        match addr {
                            0xC000..=0xFFFF => {
                                let bank = &mapper.prg_rom.borrow()[(mapper.prg_rom.borrow().len() - 0x4000)..];
                                bank[(*addr - 0xC000) as usize]
                            }
                            _ => {
                                let bank_start = (0x4000 * (data.prg_bank & 0b01111) as usize) % prg_rom.len();
                                let bank_end = bank_start + 0x4000;
                                let bank = &prg_rom[bank_start..bank_end];
                                bank[(*addr - 0x8000) as usize]
                            }
                        }
                    }
                }
            },
        }
    }

    fn write(&mut self, value: u8) {
        match self {
            Mapper1Location::Ram((t, addr)) => t.borrow_mut()[*addr as usize] = value,
            Mapper1Location::Mmio((mapper, addr)) => {
                let mut data = mapper.data.borrow_mut();
                if value & 0x80 != 0 {
					// TODO: Does this affect the prg_bank register?
                    data.shifter = 0;
                    data.shift_count = 0;
                }
                else {
                    data.shifter >>= 1;
                    data.shifter |= (value & 1) << 4;

                    data.shift_count += 1;

                    // TODO: Ignore consecutive writes?
                    if data.shift_count == 5 {
                        let register = (*addr >> 13) & 0b11;

                        println!("Writing {:#b} to mapper register {}", data.shifter, register);
						if register != 0 {
							println!("... with modes {:?}, {:?}, {:?}", data.mirror_mode, data.prg_rom_mode, data.chr_mode);
						}

                        // Register is bits 14 and 13 of the address
                        match register {
                            0 => {
                                let mut prg_rom_mode = (data.shifter >> 2 ) & 0b11;
                                if prg_rom_mode == 0 { prg_rom_mode = 1 }

                                data.mirror_mode = MirrorMode::from_u8(data.shifter & 0b11).unwrap();
                                data.prg_rom_mode = PrgRomMode::from_u8(prg_rom_mode).unwrap();
                                data.chr_mode = ChrMode::from_u8((data.shifter >> 4) & 0b1).unwrap();

                                println!("Modes are now: {:?}, {:?}, {:?}", data.mirror_mode, data.prg_rom_mode, data.chr_mode);
                            },
                            1 => data.chr_bank_0 = data.shifter,
                            2 => data.chr_bank_1 = data.shifter,
                            3 => data.prg_bank = data.shifter,
                            _ => unreachable!(),
                        }

                        data.shift_count = 0;
                        data.shifter = 0;
                    }
                }
            },
        }
    }
}

impl Mapped for Mapper1 {
    fn mem_cpu<'a>(&'a self, addr: u16) -> Box<dyn MemLocation + 'a> {
        match addr {
            0x4020..=0x5FFF => {
                println!("Returning wrapped from unmapped address {:#06X}", addr);
                self.mem_cpu(addr + 0x2000)
            },
            0x6000..=0x7FFF => {
                if self.data.borrow().prg_bank & (1 << 4) != 0 {
                    panic!("RAM is disabled")
                }
                Box::new(Mapper1Location::Ram((&self.prg_ram, (addr - 0x6000))))
            },
            0x8000..=0xFFFF => Box::new(Mapper1Location::Mmio((self, addr))),
            _ => panic!(),
        }
    }

    fn mem_ppu<'a>(&'a self, addr: u16) -> Box<dyn MemLocation + 'a> {
        if addr >= 0x2000 {
            panic!();
        }

        let data = self.data.borrow();

        match data.chr_mode {
            ChrMode::Individual => {
                let bank_select = 
                if (0x0000..=0x0FFF).contains(&addr) {
                    data.chr_bank_0
                }
                else {
                    data.chr_bank_1
                };

                let bank_start = 0x1000 * (bank_select as usize);
                Box::new(Mapper1Location::Ram((&self.chr_rom, addr + bank_start as u16)))
            },
            ChrMode::DoubleSize => {
                let bank_select = data.chr_bank_0 >> 1;

                let bank_start = 0x2000 * (bank_select as usize);

                Box::new(Mapper1Location::Ram((&self.chr_rom, addr + bank_start as u16)))
            },
        }
    }

    // TODO: The one-screen modes
    fn is_vert_mirrored(&self) -> bool {
        self.data.borrow().mirror_mode == MirrorMode::Vertical
    }
}

pub struct Mapper3 {
    prg_rom: Vec<u8>, // 16KiB or 32KiB, not bankswitched
    chr: RefCell<Vec<u8>>, // Up to 2048KiB, bank size 8KiB
    bank_select: RefCell<u8>,
}

impl Mapper3 {
    pub fn new(prg_rom: Vec<u8>, chr: Vec<u8>) -> Mapper3 {
        assert!(chr.len() <= 0x200000);
        assert_eq!(chr.len() % 0x2000, 0);
        assert!(prg_rom.len() == 0x4000 || prg_rom.len() == 0x8000);

        Mapper3 { prg_rom, chr: RefCell::new(chr), bank_select: RefCell::new(0) }
    }
}

enum Mapper3Location<'a> {
    CPU((&'a RefCell<u8>, u8)),
    PPU((&'a RefCell<Vec<u8>>, u16)),
}

impl MemLocation for Mapper3Location<'_> {
    fn read(&mut self) -> u8 {
        match self {
            Mapper3Location::CPU((_, d)) => *d,
            Mapper3Location::PPU((d, addr)) => d.borrow()[*addr as usize],
        }
    }

    fn write(&mut self, value: u8) {
        match self {
            Mapper3Location::CPU((bank_select, _)) => {
                assert!(value <= 3);
                *bank_select.borrow_mut() = value;
            },
            Mapper3Location::PPU((d, addr)) => {
                d.borrow_mut()[*addr as usize] = value;
            },
        }
    }
}

impl Mapped for Mapper3 {
    fn mem_cpu<'a>(&'a self, addr: u16) -> Box<dyn MemLocation + 'a> {
        if (0x8000..=0xFFFF).contains(&addr) {
            let d = self.prg_rom[(addr - 0x8000) as usize];
            Box::new(Mapper3Location::<'a>::CPU((&self.bank_select, d)))
        }
        else {
            panic!()
        }
    }

    fn mem_ppu<'a>(&'a self, addr: u16) -> Box<dyn MemLocation + 'a> {
        match addr {
            0x0000..=0x1FFF => {
                Box::new(Mapper3Location::<'a>::PPU((&self.chr, addr)))
            },
            _ => panic!(),
        }
    }

    // TODO: Is it?
    fn is_vert_mirrored(&self) -> bool {
        true
    }
}
