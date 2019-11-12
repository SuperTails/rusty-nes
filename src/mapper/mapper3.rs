use std::cell::RefCell;
use crate::Context;
use crate::mem_location::MemLocation;
use super::{Mapped, MapperResult, MirrorMode};

pub struct Mapper3 {
    prg_rom: Vec<u8>,      // 16KiB or 32KiB, not bankswitched
    chr: RefCell<Vec<u8>>, // Up to 2048KiB, bank size 8KiB
    bank_select: RefCell<u8>,
}

impl Mapper3 {
    pub fn new(prg_rom: Vec<u8>, chr: Vec<u8>) -> Mapper3 {
        assert!(chr.len() <= 0x200000);
        assert_eq!(chr.len() % 0x2000, 0);
        assert!(prg_rom.len() == 0x4000 || prg_rom.len() == 0x8000);

        Mapper3 {
            prg_rom,
            chr: RefCell::new(chr),
            bank_select: RefCell::new(0),
        }
    }
}

pub enum Mapper3Location<'a> {
    CPU((&'a RefCell<u8>, u8)),
    PPU((&'a RefCell<Vec<u8>>, u16)),
}

impl<'a> MemLocation<'a> for Mapper3Location<'a> {
    fn read(&mut self) -> u8 {
        match self {
            Mapper3Location::CPU((_, d)) => *d,
            Mapper3Location::PPU((d, addr)) => d.borrow()[*addr as usize % d.borrow().len()],
        }
    }

    fn write(&mut self, value: u8) {
        match self {
            Mapper3Location::CPU((bank_select, _)) => {
                *bank_select.borrow_mut() = value % 4;
            }
            Mapper3Location::PPU((d, addr)) => {
                d.borrow_mut()[*addr as usize] = value;
            }
        }
    }
}

impl Mapped for Mapper3 {
    fn mem_cpu<'a>(&'a self, addr: u16, _: &'a Context) -> MapperResult<'a> {
        // TODO: What do I do with 0x4020 to 0x7FFF?
        /*if (0x8000..=0xFFFF).contains(&addr) {
            let d = self.prg_rom[(addr - 0x8000) as usize];
            Mapper3Location::<'a>::CPU((&self.bank_select, d)).into()
        }
        else {
            panic!()
        }*/
        if (0x4020..=0x7FFF).contains(&addr) {
            let d = self.prg_rom[addr as usize];
            Mapper3Location::<'a>::CPU((&self.bank_select, d)).into()
        } else if (0x4020..=0xFFFF).contains(&addr) {
            let d = self.prg_rom[(addr - 0x8000) as usize];
            Mapper3Location::<'a>::CPU((&self.bank_select, d)).into()
        } else {
            panic!()
        }
    }

    fn mem_ppu<'a>(&'a self, addr: u16) -> MapperResult<'a> {
        match addr {
            0x0000..=0x1FFF => {
                let selected = *self.bank_select.borrow();
                let bank_start = selected as u16 * 0x2000;
                let mapped_addr = addr + bank_start;
                Mapper3Location::<'a>::PPU((&self.chr, mapped_addr)).into()
            }
            _ => panic!(),
        }
    }

    // TODO: Is it?
    fn mirror_mode(&self) -> MirrorMode {
        MirrorMode::Vertical
    }
}
