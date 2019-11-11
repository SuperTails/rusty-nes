use crate::apu::APU;
use crate::controller::Controller;
use crate::cpu::CPU;
use crate::mapper::{AnyMemLocation, Mapper0Ram, Mapper1Location, Mapper3Location};
use crate::ppu::{PPU, nametable::NAMETABLE_SIZE};
use crate::Context;
use enum_dispatch::enum_dispatch;
use std::cell::RefCell;
use std::num::Wrapping;

#[enum_dispatch(AnyMemLocation)]
pub trait MemLocation<'a> {
    fn read(&'a mut self) -> u8;

    fn write(&'a mut self, value: u8);
}

pub struct CPURamLoc<'a> {
    pub cpu: &'a RefCell<CPU>,
    pub addr: u16,
}

impl<'a> MemLocation<'a> for CPURamLoc<'a> {
    fn read(&mut self) -> u8 {
        self.cpu.borrow().ram[self.addr as usize]
    }

    fn write(&mut self, value: u8) {
        self.cpu.borrow_mut().ram[self.addr as usize] = value;
    }
}

#[derive(Debug)]
pub struct RamLocation<'a> {
    pub mem: &'a RefCell<Vec<u8>>,
    pub addr: u16,
}

impl<'a> MemLocation<'a> for RamLocation<'a> {
    fn read(&mut self) -> u8 {
        self.mem.borrow()[self.addr as usize]
    }

    fn write(&mut self, value: u8) {
        self.mem.borrow_mut()[self.addr as usize] = value;
    }
}

#[derive(Debug, Clone)]
pub struct RomLocation<'a> {
    pub mem: &'a u8,
}

impl<'a> MemLocation<'a> for RomLocation<'a> {
    fn read(&mut self) -> u8 {
        *self.mem
    }

    fn write(&mut self, _value: u8) {
        println!("Write to ROM??");
    }
}

pub struct PPUNametable<'a> {
    pub ppu: &'a RefCell<PPU>,
    pub addr: usize,
}

impl<'a> MemLocation<'a> for PPUNametable<'a> {
    fn read(&mut self) -> u8 {
        unimplemented!()
    }

    fn write(&mut self, value: u8) {
        let mut ppu = self.ppu.borrow_mut();
        let table = self.addr / NAMETABLE_SIZE;
        let entry = self.addr % NAMETABLE_SIZE;
        ppu.nametables[table].write(entry, value);
    }
}

pub struct PPUPalette<'a> {
    pub ppu: &'a RefCell<PPU>,
    pub addr: usize,
}

impl<'a> MemLocation<'a> for PPUPalette<'a> {
    fn read(&mut self) -> u8 {
        self.ppu.borrow().palette_idxs[self.addr]
    }

    fn write(&mut self, value: u8) {
        self.ppu.borrow_mut().palette_idxs[self.addr] = value;
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

impl<'a> MemLocation<'a> for PPURegister<'a> {
    fn read(&mut self) -> u8 {
        let result = match self.reg {
            PPURegInt::Ctrl => self.ppu.borrow().ctrl.0,
            PPURegInt::Mask => self.ppu.borrow().mask,
            PPURegInt::Status => {
                let mut ppu = self.ppu.borrow_mut();
                    
                let new_status = ppu.status & !0x80;
                std::mem::replace(&mut ppu.status, new_status)
            }
            PPURegInt::Oamdata => {
                let ppu = self.ppu.borrow_mut();
                let entry = &ppu.oam[(ppu.oam_addr / 4) as usize];
                match ppu.oam_addr % 4 {
                    0 => entry.y,
                    1 => entry.index,
                    2 => entry.attrs.0,
                    3 => entry.x,
                    _ => unreachable!(),
                }
            }
            PPURegInt::Scroll | PPURegInt::Addr | PPURegInt::Oamaddr => {
                self.ppu.borrow().last_value
            }
            PPURegInt::Data => {
                let addr = self.ppu.borrow().address;
                let mut ppu = self.ppu.borrow_mut();
                let result = ppu.read_buffered(addr, self.context);
                ppu.incr_address();
                result
            }
            PPURegInt::Dma => unimplemented!(),
        };

        self.ppu.borrow_mut().last_value = result;

        result
    }

    fn write(&mut self, value: u8) {
        self.ppu.borrow_mut().last_value = value;

        match self.reg {
            PPURegInt::Ctrl => {
                self.ppu.borrow_mut().ctrl.0 = value;
            }
            PPURegInt::Mask => {
                self.ppu.borrow_mut().mask = value;
            }
            PPURegInt::Status => println!("Attempt to write to status"),
            PPURegInt::Oamaddr => {
                self.ppu.borrow_mut().oam_addr = value;
            }
            PPURegInt::Oamdata => {
                let mut ppu = self.ppu.borrow_mut();

                let entry = ppu.oam_addr / 4;
                let byte = ppu.oam_addr % 4;

                let entry = &mut ppu.oam[entry as usize];
                match byte {
                    0 => entry.y = value,
                    1 => entry.index = value,
                    2 => entry.attrs = crate::ppu::OAMEntryAttrs(value),
                    3 => entry.x = value,
                    _ => unreachable!(),
                };

                ppu.oam_addr = (Wrapping(ppu.oam_addr) + Wrapping(1)).0;
            }
            PPURegInt::Scroll => {
                let mut ppu = self.ppu.borrow_mut();
                ppu.scroll <<= 8;
                ppu.scroll |= value as u16;
            }
            PPURegInt::Addr => {
                let mut ppu = self.ppu.borrow_mut();
                ppu.address <<= 8;
                ppu.address |= value as u16;
            }
            PPURegInt::Dma => {
                self.ppu.borrow_mut().dma_request = Some(value);
            }
            PPURegInt::Data => {
                let addr = self.ppu.borrow().address;
                let mut ppu = self.ppu.borrow_mut();
                ppu.write(addr, value, self.context);
                ppu.incr_address();
            }
        }
    }
}

pub struct APURegister<'a> {
    pub apu: &'a RefCell<APU>,
    pub register: usize,
}

impl<'a> MemLocation<'a> for APURegister<'a> {
    fn read(&mut self) -> u8 {
        self.apu.borrow_mut().read(self.register as u8)
    }

    fn write(&mut self, value: u8) {
        self.apu.borrow_mut().write(self.register as u8, value);
    }
}

pub struct CTLRegister<'a> {
    pub ctl: &'a RefCell<Controller>,
    pub register: usize,
}

impl<'a> MemLocation<'a> for CTLRegister<'a> {
    fn read(&mut self) -> u8 {
        self.ctl.borrow_mut().read(self.register as u8)
    }

    fn write(&mut self, value: u8) {
        self.ctl.borrow_mut().write(self.register as u8, value);
    }
}
