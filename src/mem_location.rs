use crate::ppu::PPU;
use crate::apu::APU;
use crate::controller::Controller;
use crate::Context;
use std::cell::RefCell;
use std::num::Wrapping;

pub trait MemLocation {
    fn read(&mut self) -> u8;

    fn write(&mut self, value: u8);
}

#[derive(Debug)]
pub struct RamLocation<'a> {
    pub mem: &'a RefCell<Vec<u8>>,
    pub addr: u16,
}

impl MemLocation for RamLocation<'_> {
    fn read(&mut self) -> u8 {
        self.mem.borrow()[self.addr as usize]
    }

    fn write(&mut self, value: u8) {
        self.mem.borrow_mut()[self.addr as usize] = value;
    }
}

#[derive(Debug, Clone)]
pub struct RomLocation<'a> {
    pub mem: &'a u8
}

impl MemLocation for RomLocation<'_> {
    fn read(&mut self) -> u8 {
        *self.mem
    }

    fn write(&mut self, _value: u8) {
        panic!("Attempted to write to a location in ROM!");
    }
}

pub struct PPUNametable<'a> {
    pub ppu: &'a RefCell<PPU>,
    pub addr: usize,
}

impl MemLocation for PPUNametable<'_> {
    fn read(&mut self) -> u8 {
        unimplemented!()
    }

    fn write(&mut self, value: u8) {
        self.ppu.borrow().name_tables.borrow_mut()[self.addr] = value;
    }

}

pub struct PPUPalette<'a> {
    pub ppu: &'a RefCell<PPU>,
    pub addr: usize,
}

impl MemLocation for PPUPalette<'_> {
    fn read(&mut self) -> u8 {
        self.ppu.borrow().palette_idxs.borrow()[self.addr]
    }

    fn write(&mut self, value: u8) {
        self.ppu.borrow().palette_idxs.borrow_mut()[self.addr] = value;
    }
}

pub struct PPURegister<'a> {
    pub context: &'a Context,
    pub ppu: &'a RefCell<PPU>,
    pub reg: PPURegInt,
}

#[derive(FromPrimitive)]
pub enum PPURegInt {
    Ctrl = 0,
    Mask,
    Status,
    Oamaddr,
    Oamdata,
    Scroll,
    Addr,
    Data = 7,
    Dma = 14,
}

impl MemLocation for PPURegister<'_> {
    fn read(&mut self) -> u8 {
        match self.reg {
            PPURegInt::Ctrl => self.ppu.borrow().ctrl.0,
            PPURegInt::Mask => self.ppu.borrow().mask,
            PPURegInt::Status => *self.ppu.borrow().status.borrow_mut(),
            PPURegInt::Oamaddr => unimplemented!(),
            PPURegInt::Oamdata => {
                let ppu = self.ppu.borrow();
                let mut oam_addr = ppu.oam_addr.borrow_mut();
                let array_addr = *oam_addr / 4;
                let oam = ppu.oam.borrow();
                let entry = &oam[array_addr as usize];
                let result = match *oam_addr % 4 {
                    0 => entry.y,
                    1 => entry.index,
                    2 => entry.attrs.0,
                    3 => entry.x,
                    _ => unreachable!(),
                };

                *oam_addr = (Wrapping(*oam_addr) + Wrapping(1)).0;

                result
            },
            PPURegInt::Scroll => {
                unimplemented!()
            },
            PPURegInt::Addr => {
                unimplemented!()
            },
            PPURegInt::Data => {
                let result = self.context.ppu_address(*self.ppu.borrow().address.borrow()).read();
                self.ppu.borrow_mut().incr_address();
                result
            },
            PPURegInt::Dma => {
                unimplemented!()
            },
        }
    }

    fn write(&mut self, value: u8) {
        match self.reg {
            PPURegInt::Ctrl => {
                self.ppu.borrow_mut().ctrl.0 = value;
            },
            PPURegInt::Mask => {
                self.ppu.borrow_mut().mask = value;
            },
            PPURegInt::Status => {
                println!("Attempt to write to status")
            },
            PPURegInt::Oamaddr => {
                *self.ppu.borrow().oam_addr.borrow_mut() = value;
            },
            PPURegInt::Oamdata => {
                let ppu = self.ppu.borrow();
                let mut oam_addr = ppu.oam_addr.borrow_mut();

                let array_addr = *oam_addr / 4;
                let mut oam = ppu.oam.borrow_mut();
                let entry = &mut oam[array_addr as usize];
                match *oam_addr % 4 {
                    0 => entry.y = value,
                    1 => entry.index = value,
                    2 => entry.attrs = crate::ppu::OAMEntryAttrs(value),
                    3 => entry.x = value,
                    _ => unreachable!(),
                };

                *oam_addr = (Wrapping(*oam_addr) + Wrapping(1)).0;
            },
            PPURegInt::Scroll => {
                let ppu = self.ppu.borrow();
                let mut scroll = ppu.scroll.borrow_mut();
                *scroll <<= 8;
                *scroll |= value as u16;
            },
            PPURegInt::Addr => {
                let ppu = self.ppu.borrow();
                let mut address = ppu.address.borrow_mut();
                *address <<= 8;
                *address |= value as u16;
            },
            PPURegInt::Dma => {
                self.ppu.borrow().dma(value, self.context);
            },
            PPURegInt::Data => {
                self.context.ppu_address(*self.ppu.borrow().address.borrow()).write(value);
                self.ppu.borrow_mut().incr_address();
            },
        }
    }
}

pub struct APURegister<'a> {
    pub apu: &'a RefCell<APU>,
    pub register: usize,
}

impl MemLocation for APURegister<'_> {
    fn read(&mut self) -> u8 {
        0
        //unimplemented!()
    }

    fn write(&mut self, _value: u8) {
        println!("Ignoring APU write");
        //unimplemented!()
    }
}

pub struct CTLRegister<'a> {
    pub ctl: &'a RefCell<Controller>,
    pub register: usize,
}

impl MemLocation for CTLRegister<'_> {
    fn read(&mut self) -> u8 {
        self.ctl.borrow_mut().read(self.register as u8)
    }

    fn write(&mut self, value: u8) {
        self.ctl.borrow_mut().write(self.register as u8, value);
    }
}
