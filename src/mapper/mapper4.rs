use std::cell::RefCell;
use crate::Context;
use crate::mem_location::{MemLocation, RamLocation, RomLocation};
use super::{Mapped, MapperResult, MirrorMode};

/* From the nesdev wiki (https://wiki.nesdev.com/w/index.php/MMC3):
 *
 * CPU $6000-$7FFF: 8 KB PRG RAM bank (optional)
 * CPU $8000-$9FFF (or $C000-$DFFF): 8 KB switchable PRG ROM bank
 * CPU $A000-$BFFF: 8 KB switchable PRG ROM bank
 * CPU $C000-$DFFF (or $8000-$9FFF): 8 KB PRG ROM bank, fixed to the second-last bank
 * CPU $E000-$FFFF: 8 KB PRG ROM bank, fixed to the last bank
 * 
 * PPU $0000-$07FF (or $1000-$17FF): 2 KB switchable CHR bank
 * PPU $0800-$0FFF (or $1800-$1FFF): 2 KB switchable CHR bank
 * PPU $1000-$13FF (or $0000-$03FF): 1 KB switchable CHR bank
 * PPU $1400-$17FF (or $0400-$07FF): 1 KB switchable CHR bank
 * PPU $1800-$1BFF (or $0800-$0BFF): 1 KB switchable CHR bank
 * PPU $1C00-$1FFF (or $0C00-$0FFF): 1 KB switchable CHR bank
 *
 * Memory Mapping    ($8000/$8001, $A000/$A001)
 * Scanline Counting ($C000/$C001, $E000/$E001)
 */

// TODO: IRQ
pub struct Mapper4 {
    prg_ram: RefCell<Vec<u8>>, // Optional
    prg_rom: Vec<u8>,      // 16KiB or 32KiB, not bankswitched
    chr: RefCell<Vec<u8>>, // Up to 2048KiB, bank size 8KiB
    
    data: RefCell<Mapper4Data>,
}

pub struct Mapper4Data {
    next_destination: u8,
    prg_rom_mode: bool,
    chr_inversion: bool,

    doublesize_chr_banks: [u8; 2],  // R0, R1
    normalsize_chr_banks: [u8; 4],  // R2, R3, R4, R5
    switched_prg_bank: u8,          // R6
    middle_prg_bank: u8,            // R7

    deny_writes: bool,
    enable_ram: bool,

    horizontal_mirroring: bool,

    irq_counter_reload: u8,
    irq_counter: u8,
    irq_enable: bool,
}

impl Mapper4Data {
    pub fn new() -> Mapper4Data {
        Mapper4Data {
            next_destination: 0,
            prg_rom_mode: false,
            chr_inversion: false,
            doublesize_chr_banks: [0; 2],
            normalsize_chr_banks: [0; 4],
            switched_prg_bank: 0,
            middle_prg_bank: 0,
            deny_writes: false,
            enable_ram: false,
            horizontal_mirroring: false,
            irq_counter_reload: 0,
            irq_counter: 0,
            irq_enable: false,
        }
    }

	pub fn write_register(&mut self, addr: u16, value: u8) {
        let register_pair = (addr >> 13) & 0b11;
        let is_right = addr & 1 != 0;

        match (register_pair, is_right) {
            (0, false) => {
                self.next_destination = value & 0b111;
                self.prg_rom_mode = value & (1 << 6) != 0;
                self.chr_inversion = value & (1 << 7) != 0;
                /*println!("Next destination is now {}, PRG ROM mode is {}, CHR inversion state is {}",
                     self.next_destination,
                     self.prg_rom_mode,
                     self.chr_inversion,
                );*/
            }
            (0, true) => {
                match self.next_destination {
                    0..=1 => self.doublesize_chr_banks[self.next_destination as usize] = value & !0x1, // Ignore the bottom bit for doublesize
                    2..=5 => self.normalsize_chr_banks[self.next_destination as usize - 2] = value,
                    6..=6 => self.switched_prg_bank = value & 0x3F, // Ignore the top two bits for both PRG banks
                    7..=7 => self.middle_prg_bank = value & 0x3F,
                    _ => unreachable!(),
                }

                /*if self.next_destination <= 5 {
                    println!("Writing {:#X} to CHR register {}", 
                        value,
                        self.next_destination
                    );
                } else {
                    println!("Writing {:#X} to PRG ROM register {}",
                        value,
                        self.next_destination - 6
                    );
                }*/
            }
            (1, false) => {
                // TODO: Support hardwired 4-screen VRAM
                self.horizontal_mirroring = value & 1 != 0;

                //println!("Setting mirroring state to {}", self.horizontal_mirroring);
            }
            (1, true) => {
                self.deny_writes = value & (1 << 6) != 0;
                self.enable_ram = value & (1 << 7) != 0;
                //println!("Ignoring write to PRG RAM protection register");
            }
            (2, false) => {
                self.irq_counter_reload = value;
                //println!("Setting IRQ counter reload value to {:#X}", value);
            }
            (2, true) => {
                // TODO: Reload counter at *next* rising edge of PPU address
                // (usually at pixel 260?)
                self.irq_counter = self.irq_counter_reload;
                //println!("Reloading counter maybe");
            }
            (3, false) => {
                // TODO: Acknowledge pending interrupts
                self.irq_enable = false;
                //println!("Disabling IRQs");
            }
            (3, true) => {
                self.irq_enable = true;
                //println!("Enabling IRQs");
            }
            _ => unreachable!(),
        }
	}

    pub fn cpu_bank(&self, addr: u16) -> i8 {
        if self.prg_rom_mode {
            match addr {
                0x8000..=0x9FFF => -2,
                0xA000..=0xBFFF => self.middle_prg_bank as i8,
                0xC000..=0xDFFF => self.switched_prg_bank as i8,
                0xE000..=0xFFFF => -1,
                _ => panic!(),
            }
        } else {
            match addr {
                0x8000..=0x9FFF => self.switched_prg_bank as i8,
                0xA000..=0xBFFF => self.middle_prg_bank as i8,
                0xC000..=0xDFFF => -2,
                0xE000..=0xFFFF => -1,
                _ => panic!(),
            }
        }
    }

    pub fn ppu_bank_register(&self, addr: u16) -> (u8, bool) {
        if self.chr_inversion {
            match addr {
                0x0000..=0x0FFF => (self.normalsize_chr_banks[(addr - 0x0000) as usize / 0x400], false),
                0x1000..=0x1FFF => (self.doublesize_chr_banks[(addr - 0x1000) as usize / 0x800], true),
                _ => panic!(),
            }
        }
        else {
            match addr {
                0x0000..=0x0FFF => (self.doublesize_chr_banks[(addr - 0x0000) as usize / 0x800], true),
                0x1000..=0x1FFF => (self.normalsize_chr_banks[(addr - 0x1000) as usize / 0x400], false),
                _ => panic!(),
            }
        }
    }
}

impl Mapper4 {
    pub fn new(prg_rom: Vec<u8>, chr: Vec<u8>, prg_ram_len: usize) -> Mapper4 {
        assert!(chr.len() <= 0x200000);
        assert_eq!(chr.len() % 0x2000, 0);

        let mut prg_ram = Vec::with_capacity(prg_ram_len);
        prg_ram.resize(prg_ram_len, 0);

        Mapper4 {
            prg_rom,
            chr: RefCell::new(chr),
            prg_ram: RefCell::new(prg_ram),
            data: RefCell::new(Mapper4Data::new()),
        }
    }
}

pub enum Mapper4Location<'a> {
    CPU((&'a RefCell<Mapper4Data>, u16, u8)),
    PPU((&'a RefCell<Vec<u8>>, usize)),
}

impl<'a> MemLocation<'a> for Mapper4Location<'a> {
    fn read(&mut self) -> u8 {
        match self {
            Mapper4Location::CPU((_, _, d)) => *d,
            Mapper4Location::PPU((d, addr)) => d.borrow()[*addr as usize],
        }
    }

    fn write(&mut self, value: u8) {
        match self {
            Mapper4Location::CPU((data, addr, _)) => {
                data.borrow_mut().write_register(*addr, value);
            }
            Mapper4Location::PPU((d, addr)) => {
                d.borrow_mut()[*addr as usize] = value;
            }
        }
    }
}

impl Mapped for Mapper4 {
    fn mem_cpu<'a>(&'a self, addr: u16, _: &'a Context) -> MapperResult<'a> {
        match addr {
            0x4020..=0x7FFF => {
                if self.data.borrow().deny_writes {
                    RomLocation { mem: self.prg_ram.borrow()[addr as usize % 0x2000] }.into()
                } else {
                    RamLocation { mem: &self.prg_ram, addr: (addr % 0x2000) }.into()
                }
            },
            0x8000..=0xFFFF => Mapper4Location::CPU({
                let relative = (addr % 0x2000) as usize;
                let bank_select = {
                    let bank_count = self.prg_rom.len() / 0x2000;
                    let bank_select = self.data.borrow().cpu_bank(addr);
                    if bank_select == -1 {
                        bank_count - 1
                    } else if bank_select == -2 {
                        bank_count - 2
                    } else {
                        bank_select as usize % bank_count
                    }
                };
                let bank = &self.prg_rom[(bank_select * 0x2000)..((bank_select + 1) * 0x2000)];
                (&self.data, addr, bank[relative])
            }).into(),
            _ => panic!(),
        }
    }

    fn mem_ppu<'a>(&'a self, mut addr: u16) -> MapperResult<'a> {
        match addr {
            0x0000..=0x1FFF => {
                let (reg, double) = self.data.borrow().ppu_bank_register(addr);
                addr &= if double { 0x7FF } else { 0x3FF };

                let mapped_addr = (reg as usize * 0x400 + addr as usize) & (self.chr.borrow().len() - 1);
                Mapper4Location::PPU((&self.chr, mapped_addr)).into()
            }
            _ => panic!(),
        }
    }

    fn mirror_mode(&self) -> MirrorMode {
        if self.data.borrow().horizontal_mirroring {
            MirrorMode::Horizontal
        }
        else {
            MirrorMode::Vertical
        }
    }
}
