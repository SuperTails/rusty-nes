use crate::context::CpuContext;
use super::CpuMapper;
use rust_2c02::{PpuMapper, MirrorMode};
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

impl CpuMapper for Mapper0 {
    type Context = CpuContext;

    fn read_mem_cpu(&mut self, addr: u16, _: &mut Self::Context) -> u8 {
        let prg_rom_len = self.prg_rom.len();
        match addr {
            // RAM for Mapper 0
            0x4020..=0x7FFF => self.prg_ram.borrow()[((addr - 0x4000) % 0x800) as usize],
            // ROM for Mapper 0
            0x8000..=0xFFFF => self.prg_rom[(addr - 0x8000) as usize % prg_rom_len],
            _ => panic!("CPU access to memory not in cart at {:#06X}", addr),
        }
    }

    fn write_mem_cpu(&mut self, addr: u16, value: u8, _: &mut Self::Context) {
        match addr {
            0x4020..=0x7FFF => self.prg_ram.borrow_mut()[((addr - 0x4000) % 0x800) as usize] = value,
            0x8000..=0xFFFF => eprintln!("Ignoring ROM write at {:06X}", addr),
            _ => panic!("CPU access to memory not in cart at {:#06X}", addr),
        }
    }
}

impl PpuMapper for Mapper0 {
    fn read_mem_ppu(&mut self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x1FFF => self.chr_rom.borrow()[addr as usize],
            _ => panic!("PPU access to memory not in cart at {:#06X}", addr),
        }
    }

    fn write_mem_ppu(&mut self, addr: u16, value: u8) {
        match addr {
            0x0000..=0x1FFF => self.chr_rom.borrow_mut()[addr as usize] = value,
            _ => panic!("PPU access to memory not in cart at {:#06X}", addr),
        }
    }

    // TODO: Figure this out
    fn mirror_mode(&self) -> MirrorMode {
        MirrorMode::Vertical
    }
}
