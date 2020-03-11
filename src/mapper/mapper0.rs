use super::{Mapped, MapperResult, MirrorMode};
use crate::mem_location::{MemLocation, RamLocation, RomLocation};
use crate::Context;
use std::cell::RefCell;

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
    chr_rom: RefCell<Vec<u8>>,
}

impl Mapper0 {
    pub fn new(prg_rom: Vec<u8>, chr_rom: Vec<u8>) -> Mapper0 {
        assert!(prg_rom.len() == 0x4000 || prg_rom.len() == 0x8000);
        assert_eq!(chr_rom.len() % 0x2000, 0);

        /* TODO: Figure out how much RAM to allocate */
        Mapper0 {
            prg_rom,
            prg_ram: RefCell::new([0; 0x800].to_vec()),
            chr_rom: RefCell::new(chr_rom),
        }
    }
}

pub struct Mapper0Ram<'a>(&'a RefCell<Vec<u8>>, u16);

impl<'a> MemLocation<'a> for Mapper0Ram<'_> {
    fn read(&mut self) -> u8 {
        self.0.borrow()[self.1 as usize]
    }

    fn write(&mut self, value: u8) {
        self.0.borrow_mut()[self.1 as usize] = value;
    }
}

impl Mapped for Mapper0 {
    fn mem_cpu<'a>(&'a self, addr: u16, _: &'a Context) -> MapperResult<'a> {
        let prg_rom_len = self.prg_rom.len();
        match addr {
            0x4020..=0x7FFF => Mapper0Ram(&self.prg_ram, (addr - 0x4000) % 0x800).into(),
            0x8000..=0xFFFF => RomLocation {
                mem: self.prg_rom[(addr - 0x8000) as usize % prg_rom_len],
            }
            .into(),
            _ => panic!("CPU access to memory not in cart at {:#06X}", addr),
        }
    }

    fn mem_ppu(&self, addr: u16) -> MapperResult {
        match addr {
            0x0000..=0x1FFF => RamLocation {
                addr,
                mem: &self.chr_rom,
            }
            .into(),
            _ => panic!("PPU access to memory not in cart at {:#06X}", addr),
        }
    }

    // TODO: Figure this out
    fn mirror_mode(&self) -> MirrorMode {
        MirrorMode::Vertical
    }
}
