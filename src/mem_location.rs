use crate::apu::APU;
use crate::controller::Controller;
use crate::cpu::CPU;
use rust_2c02::{PPU, nametable::NAMETABLE_SIZE, PpuMapper};
use num_derive::FromPrimitive;
use std::cell::RefCell;
use std::num::Wrapping;

pub trait MemLocation {
    fn read(&mut self) -> u8;

    fn write(&mut self, value: u8);
}

pub struct CPURamLoc<'a> {
    pub cpu: &'a RefCell<CPU>,
    pub addr: u16,
}

impl<'a> MemLocation for CPURamLoc<'a> {
    fn read(&mut self) -> u8 {
        self.cpu.borrow().ram[self.addr as usize]
    }

    fn write(&mut self, value: u8) {
        self.cpu.borrow_mut().ram[self.addr as usize] = value;
    }
}

#[derive(Debug)]
pub struct RamLocation<'a> {
    pub mem: &'a mut Vec<u8>,
    pub addr: u16,
}

impl<'a> MemLocation for RamLocation<'a> {
    fn read(&mut self) -> u8 {
        self.mem[self.addr as usize]
    }

    fn write(&mut self, value: u8) {
        self.mem[self.addr as usize] = value;
    }
}

#[derive(Debug, Clone)]
pub struct RomLocation {
    pub mem: u8,
}

impl MemLocation for RomLocation {
    fn read(&mut self) -> u8 {
        self.mem
    }

    fn write(&mut self, _value: u8) {}
}

pub struct PPUNametable<'a, > {
    pub ppu: &'a RefCell<PPU>,
    pub addr: usize,
}

impl<'a> MemLocation for PPUNametable<'a> {
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

impl<'a> MemLocation for PPUPalette<'a> {
    fn read(&mut self) -> u8 {
        self.ppu.borrow().palette_idxs[self.addr]
    }

    fn write(&mut self, value: u8) {
        self.ppu.borrow_mut().palette_idxs[self.addr] = value;
    }
}

pub struct PPURegister<'a> {
    pub ppu: &'a mut PPU,
    pub reg: PPURegInt,
    pub mapper: &'a mut dyn PpuMapper,
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

impl<'a> MemLocation for PPURegister<'a> {
    fn read(&mut self) -> u8 {
        let result = match self.reg {
            PPURegInt::Ctrl => self.ppu.decay,
            PPURegInt::Mask => self.ppu.decay,
            PPURegInt::Status => {
                self.ppu.write_second = false;
                let decay = self.ppu.decay & 0b0001_1111;
                let new_status = self.ppu.status & !0x80;
                std::mem::replace(&mut self.ppu.status, new_status) | decay
            }
            PPURegInt::Oamdata => {
                let entry = &self.ppu.oam[(self.ppu.oam_addr / 4) as usize];
                match self.ppu.oam_addr % 4 {
                    0 => entry.y,
                    1 => entry.index,
                    2 => entry.attrs.0,
                    3 => entry.x,
                    _ => unreachable!(),
                }
            }
            PPURegInt::Scroll | PPURegInt::Addr | PPURegInt::Oamaddr => self.ppu.decay,
            PPURegInt::Data => {
                let addr = self.ppu.address();
                let result = self.ppu.read_buffered(addr.0, self.mapper);
                self.ppu.incr_address();
                result
            }
            PPURegInt::Dma => unimplemented!(),
        };

        self.ppu.last_value = result;

        result
    }

    fn write(&mut self, value: u8) {
        self.ppu.last_value = value;
        self.ppu.decay = value;

        match self.reg {
            PPURegInt::Ctrl => {
                self.ppu.ctrl.0 = value;
                self.ppu
                    .temp_address
                    .set_nametable(value as u16 & 0b11);
            }
            PPURegInt::Mask => {
                self.ppu.mask.0 = value;
            }
            PPURegInt::Status => println!("Attempt to write to status"),
            PPURegInt::Oamaddr => {
                self.ppu.oam_addr = value;
            }
            PPURegInt::Oamdata => {
                let entry = self.ppu.oam_addr / 4;
                let byte = self.ppu.oam_addr % 4;

                let entry = &mut self.ppu.oam[entry as usize];
                match byte {
                    0 => entry.y = value,
                    1 => entry.index = value,
                    2 => entry.attrs = rust_2c02::oam::OAMEntryAttrs(value),
                    3 => entry.x = value,
                    _ => unreachable!(),
                };

                self.ppu.oam_addr = (Wrapping(self.ppu.oam_addr) + Wrapping(1)).0;
            }
            PPURegInt::Scroll => {
                if self.ppu.write_second {
                    self.ppu.temp_address.set_coarse_y((value / 8) as u16);
                    self.ppu.temp_address.set_fine_y((value % 8) as u16);
                } else {
                    self.ppu.temp_address.set_coarse_x((value / 8) as u16);
                    self.ppu.temp_fine_x = value % 8;
                }

                self.ppu.write_second = !self.ppu.write_second;
            }
            PPURegInt::Addr => {
                if self.ppu.write_second {
                    self.ppu.temp_address.0 &= 0xFF00;
                    self.ppu.temp_address.0 |= (value as u16) << 0;
                    self.ppu.reload_address();
                } else {
                    self.ppu.temp_address.0 &= 0x00FF;
                    self.ppu.temp_address.0 |= (0x7F & value as u16) << 8;
                }

                self.ppu.write_second = !self.ppu.write_second;
            }
            PPURegInt::Dma => {
                self.ppu.dma_request = Some(value);
            }
            PPURegInt::Data => {
                let addr = self.ppu.address();
                self.ppu.write(addr.0, value, &mut self.mapper);
                self.ppu.incr_address();
            }
        }
    }
}

pub struct APURegister<'a> {
    pub apu: &'a mut APU,
    pub register: usize,
}

impl<'a> MemLocation for APURegister<'a> {
    fn read(&mut self) -> u8 {
        self.apu.read(self.register as u8)
    }

    fn write(&mut self, value: u8) {
        self.apu.write(self.register as u8, value);
    }
}

pub struct CTLRegister<'a> {
    pub ctl: &'a mut Controller,
    pub register: usize,
}

impl<'a> MemLocation for CTLRegister<'a> {
    fn read(&mut self) -> u8 {
        self.ctl.read(self.register as u8)
    }

    fn write(&mut self, value: u8) {
        self.ctl.write(self.register as u8, value);
    }
}
