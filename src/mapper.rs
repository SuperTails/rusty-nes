use num_traits::cast::FromPrimitive;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MappedLocation<T> {
    Rom(T),
    Ram(T),
    Register(u16),
    Mmio(u16),
}

impl<'a, T> From<MappedLocation<&'a mut T>> for MappedLocation<&'a T> {
    fn from(l: MappedLocation<&'a mut T>) -> MappedLocation<&'a T> {
        match l {
            MappedLocation::Rom(t) => MappedLocation::Rom(&*t),
            MappedLocation::Ram(t) => MappedLocation::Ram(&*t),
            MappedLocation::Register(t) => MappedLocation::Register(t),
            MappedLocation::Mmio(t) => MappedLocation::Mmio(t),
        }
    }
}

pub trait Mapped {
    fn mem_cpu(&mut self, addr: u16) -> MappedLocation<&mut u8>;

    fn read_cpu(&mut self, addr: u16) -> u8 {
        match self.mem_cpu(addr) {
            MappedLocation::Ram(b) => *b,
            MappedLocation::Rom(b) => *b,
            _ => panic!(),
        }
    }

    fn write_cpu(&mut self, addr: u16, value: u8) {
        match self.mem_cpu(addr) {
            MappedLocation::Ram(b) => *b = value,
            MappedLocation::Rom(b) => panic!("CPU Write to cart ROM at {:#06X}", addr),
            _ => panic!(),
        }
    }

    fn mem_ppu(&mut self, addr: u16) -> MappedLocation<&mut u8>;

    fn read_ppu(&mut self, addr: u16) -> u8 {
        match self.mem_ppu(addr) {
            MappedLocation::Rom(b) => *b,
            _ => panic!(),
        }
    }

    fn write_ppu(&mut self, addr: u16, value: u8) {
        match self.mem_ppu(addr) {
            /*MappedLocation::Rom(b) => panic!("PPU write to cart ROM at {:#06X}", addr),*/
            MappedLocation::Rom(b) => { *b = value; println!("Writing to CHR ROM??") },
            _ => panic!(),
        }
    }
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
    prg_ram: Vec<u8>,
    chr_rom: Vec<u8>,
}

impl Mapper0 {
    pub fn new(prg_rom: Vec<u8>, chr_rom: Vec<u8>) -> Mapper0 {
        assert!(prg_rom.len() == 0x4000 || prg_rom.len() == 0x8000);
        assert!(chr_rom.len() == 0x2000);

        /* TODO: Figure out how much RAM to allocate */
        Mapper0 { prg_rom, prg_ram: [0; 0x800].to_vec(), chr_rom }
    }
}

impl Mapped for Mapper0 {
    fn mem_cpu(&mut self, addr: u16) -> MappedLocation<&mut u8> {
        let prg_ram_len = self.prg_ram.len();
        let prg_rom_len = self.prg_rom.len();
        match addr {
            0x4020..=0x5FFF => panic!("CPU Read from unmapped cart memory at {:#06X}", addr),
            0x6000..=0x7FFF => MappedLocation::Ram(&mut self.prg_ram[(addr - 0x6000) as usize % prg_ram_len]),
            0x8000..=0xFFFF => MappedLocation::Rom(&mut self.prg_rom[(addr - 0x8000) as usize % prg_rom_len]),
            _ => panic!("CPU access to memory not in cart at {:#06X}", addr),
        }
    }

    fn mem_ppu(&mut self, addr: u16) -> MappedLocation<&mut u8> {
        match addr {
            0x0000..=0x1FFF => MappedLocation::Rom(&mut self.chr_rom[addr as usize]),
            _ => panic!("PPU access to memory not in cart at {:#06X}", addr),
        }
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

#[derive(FromPrimitive, Debug, Clone, Copy)]
enum MirrorMode {
    OneScreenLowerBank,
    OneScreenUpperBank,
    Vertical,
    Horizontal,
}

#[derive(FromPrimitive, Debug, Clone, Copy)]
enum PrgRomMode {
    DoubleSize = 1,
    FixFirst,
    FixLast
}

#[derive(FromPrimitive, Debug, Clone, Copy)]
enum ChrMode {
    DoubleSize,
    Individual
}


// TODO: Variants of this
pub struct Mapper1 {
    prg_ram: Vec<u8>, // 8K, Optional

    prg_rom: Vec<u8>, // 32K (??)

    chr_rom: Vec<u8>, // 8K (??)

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
    pub fn new(mut prg_rom: Vec<u8>, chr_rom: Vec<u8>) -> Mapper1 {
        assert!(prg_rom.len() == 0x20000 || prg_rom.len() == 0x40000);
        assert_eq!(chr_rom.len(), 0x2000);

        Mapper1 {
            prg_ram: [0; 0x2000].to_vec(),
            prg_rom,
            chr_rom,
            mirror_mode: MirrorMode::OneScreenLowerBank,
            prg_rom_mode: PrgRomMode::FixLast,
            chr_mode: ChrMode::DoubleSize,
            shifter: 0,
            shift_count: 0,
            chr_bank_0: 0,
            chr_bank_1: 0,
            prg_bank: 0,
        }
    }
}

impl Mapped for Mapper1 {
    fn mem_cpu(&mut self, addr: u16) -> MappedLocation<&mut u8> {
        match addr {
            0x6000..=0x7FFF => {
                if self.prg_bank & (1 << 4) != 0 {
                    panic!("RAM is disabled")
                }
                let prg_ram = &mut self.prg_ram;
                MappedLocation::Ram(&mut prg_ram[(addr - 0x6000) as usize])
            },
            0x8000..=0xFFFF => MappedLocation::Mmio(addr),
            _ => panic!(),
        }
    }

    fn read_cpu(&mut self, addr: u16) -> u8 {
        match self.mem_cpu(addr) {
            MappedLocation::Ram(t) => *t,
            MappedLocation::Mmio(_) => {
                // TODO: Use mirror mode

                match self.prg_rom_mode {
                    PrgRomMode::DoubleSize => {
                        // Switch 32KiB at $8000
                        let bank_start = (0x8000 * ((self.prg_bank & 0b01110) >> 1) as usize) % self.prg_rom.len();
                        let bank_end = bank_start + 0x8000;
                        let bank = &self.prg_rom[bank_start..bank_end];
                        bank[(addr - 0x8000) as usize]
                    },
                    PrgRomMode::FixFirst => {
                        // Fix first bank at $8000,
                        // Switch 16KiB at $C000
                        match addr {
                            0x8000..=0xBFFF => {
                                let bank = &self.prg_rom[0..0x4000];
                                bank[(addr - 0x8000) as usize]
                            },
                            _ => {
                                let bank_start = (0x4000 * (self.prg_bank & 0b01111) as usize) % self.prg_rom.len();
                                let bank_end = bank_start + 0x4000;
                                let bank = &self.prg_rom[bank_start..bank_end];
                                bank[(addr - 0x8000) as usize]
                            }
                        }
                    },
                    PrgRomMode::FixLast => {
                        // Fix last bank at $C000,
                        // Switch 16KiB at $8000
                        match addr {
                            0xC000..=0xFFFF => {
                                let bank = &self.prg_rom[(self.prg_rom.len() - 0x4000)..];
                                bank[(addr - 0xC000) as usize]
                            }
                            _ => {
                                let bank_start = (0x4000 * (self.prg_bank & 0b01111) as usize) % self.prg_rom.len();
                                let bank_end = bank_start + 0x4000;
                                let bank = &self.prg_rom[bank_start..bank_end];
                                bank[(addr - 0x8000) as usize]

                            }
                        }
                    }
                }
            },
            _ => panic!(),
        }
    }

    fn write_cpu(&mut self, addr: u16, value: u8) {
        match self.mem_cpu(addr) {
            MappedLocation::Ram(t) => *t = value,
            MappedLocation::Mmio(_) => {
                if value & 0x80 != 0 {
                    // Clear shift register
                    self.shifter = 0;
                    self.shift_count = 0;
                }
                else {
                    self.shifter >>= 1;
                    self.shifter |= (value & 1) << 4;

                    self.shift_count += 1;

                    // TODO: Ignore consecutive writes?
                    if self.shift_count == 5 {
                        let register = (addr >> 13) & 0b11;

                        println!("Writing {:#b} to mapper register {}", self.shifter, register);

                        // Register is bits 14 and 13 of the address
                        match register {
                            0 => {
                                let mut prg_rom_mode = (self.shifter >> 2 ) & 0b11;
                                if prg_rom_mode == 0 { prg_rom_mode = 1 }

                                self.mirror_mode = MirrorMode::from_u8(self.shifter & 0b11).unwrap();
                                self.prg_rom_mode = PrgRomMode::from_u8((self.shifter >> 2) & 0b11).unwrap();
                                self.chr_mode = ChrMode::from_u8((self.shifter >> 4) & 0b1).unwrap();

                                println!("Modes are now: {:?}, {:?}, {:?}", self.mirror_mode, self.prg_rom_mode, self.chr_mode);
                            },
                            1 => self.chr_bank_0 = self.shifter,
                            2 => self.chr_bank_1 = self.shifter,
                            3 => self.prg_bank = self.shifter,
                            _ => unreachable!(),
                        }

                        // TODO: Does this affect the prg_bank register?
                        self.shift_count = 0;
                        self.shifter = 0;
                    }
                }
            },
            _ => panic!(),
        }
    }

    fn mem_ppu(&mut self, addr: u16) -> MappedLocation<&mut u8> {
        match addr {
            0x0000..=0x0FFF => MappedLocation::Mmio(addr),
            0x1000..=0x1FFF => MappedLocation::Mmio(addr),
            _ => panic!(),
        }
    }

    fn read_ppu(&mut self, addr: u16) -> u8 {
        if addr >= 0x2000 {
            panic!();
        }

        match self.chr_mode {
            ChrMode::Individual => {
                let bank_select = 
                if (0x0000..=0x0FFF).contains(&addr) {
                    self.chr_bank_0
                }
                else {
                    self.chr_bank_1
                };

                let bank_start = 0x1000 * (bank_select as usize);
                let bank_end = bank_start + 0x1000;
                let bank = &self.chr_rom[bank_start..bank_end];
                bank[(addr % 0x1000) as usize]
            },
            ChrMode::DoubleSize => {
                let bank_select = self.chr_bank_0 >> 1;

                let bank_start = 0x2000 * (bank_select as usize);
                let bank_end = bank_start + 0x2000;
                let bank = &self.chr_rom[bank_start..bank_end];

                bank[addr as usize]
            },
        }
    }

    fn write_ppu(&mut self, addr: u16, value: u8) {
        if addr >= 0x2000 {
            panic!();
        }

        println!("Writing to PPU rom...?");

        match self.chr_mode {
            ChrMode::Individual => {
                let bank_select = 
                if (0x0000..=0x0FFF).contains(&addr) {
                    self.chr_bank_0
                }
                else {
                    self.chr_bank_1
                };

                let bank_start = 0x1000 * (bank_select as usize);
                let bank_end = bank_start + 0x1000;
                let bank = &mut self.chr_rom[bank_start..bank_end];
                bank[addr as usize] = value;
            },
            ChrMode::DoubleSize => {
                let bank_select = self.chr_bank_0 >> 1;

                let bank_start = 0x2000 * (bank_select as usize);
                let bank_end = bank_start + 0x2000;
                let bank = &mut self.chr_rom[bank_start..bank_end];

                bank[addr as usize] = value;
            }
        }
    }

}
