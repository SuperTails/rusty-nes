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

use super::MemLocation;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MappedLocation<T> {
    Rom(T),
    Ram(T),
    Register(u16),
}

impl<'a, T> From<MappedLocation<&'a mut T>> for MappedLocation<&'a T> {
    fn from(l: MappedLocation<&'a mut T>) -> MappedLocation<&'a T> {
        match l {
            MappedLocation::Rom(t) => MappedLocation::Rom(&*t),
            MappedLocation::Ram(t) => MappedLocation::Ram(&*t),
            MappedLocation::Register(t) => MappedLocation::Register(t),
        }
    }
}

pub trait Mapped {
    fn mem_cpu(&mut self, addr: u16) -> MappedLocation<&mut u8>;

    fn read_cpu(&mut self, addr: u16) -> u8 {
        match self.mem_cpu(addr) {
            MappedLocation::Ram(b) => *b,
            MappedLocation::Rom(b) => *b,
            _ => unreachable!(),
        }
    }

    fn write_cpu(&mut self, addr: u16, value: u8) {
        match self.mem_cpu(addr) {
            MappedLocation::Ram(b) => *b = value,
            MappedLocation::Rom(b) => panic!("CPU Write to cart ROM at {:#06X}", addr),
            _ => unreachable!(),
        }
    }

    fn mem_ppu(&mut self, addr: u16) -> MappedLocation<&mut u8>;

    fn read_ppu(&mut self, addr: u16) -> u8 {
        match self.mem_ppu(addr) {
            MappedLocation::Rom(b) => *b,
            _ => unreachable!(),
        }
    }

    fn write_ppu(&mut self, addr: u16, value: u8) {
        match self.mem_ppu(addr) {
            /*MappedLocation::Rom(b) => panic!("PPU write to cart ROM at {:#06X}", addr),*/
            MappedLocation::Rom(b) => { *b = value; println!("Writing to CHR ROM??") },
            _ => unreachable!(),
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
