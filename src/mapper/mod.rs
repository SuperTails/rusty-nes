use super::Context;
use crate::mem_location::*;
use num_derive::FromPrimitive;

mod mapper0;
mod mapper1;
mod mapper3;
mod mapper4;

pub use mapper0::*;
pub use mapper1::*;
pub use mapper3::*;
pub use mapper4::*;

#[derive(FromPrimitive, Debug, Clone, Copy, PartialEq, Eq)]
pub enum MirrorMode {
    OneScreenLowerBank = 0,
    OneScreenUpperBank,
    Vertical,
    Horizontal,
}

type MapperResult<'a> = AnyMemLocation<'a>;

pub trait Mapped {
    fn mem_cpu<'a>(&'a self, addr: u16, context: &'a Context) -> MapperResult<'a>;

    fn mem_ppu(&self, addr: u16) -> MapperResult;

    fn map_nametable_index(&self, mut index: usize) -> usize {
        assert!(index < 8);

        index %= 4;

        match self.mirror_mode() {
            MirrorMode::OneScreenLowerBank => 0,
            MirrorMode::OneScreenUpperBank => 1,
            MirrorMode::Vertical => index % 2,
            MirrorMode::Horizontal => {
                if index < 2 {
                    0
                } else {
                    1
                }
            }
        }
    }

    fn map_nametable_relative(&self, mut addr: u16) -> u16 {
        assert!(addr < 0x2000);

        addr %= 0x1000;

        let index = (addr / 0x400) as usize;
        let entry = (addr % 0x400) as usize;

        (self.map_nametable_index(index) * 0x400 + entry) as u16
    }

    fn map_nametable(&self, addr: u16) -> u16 {
        self.map_nametable_relative(addr - 0x2000) + 0x2000
    }

    fn mirror_mode(&self) -> MirrorMode;
}

// why can't the enum_dispatch crate just work with rls, ughhh
macro_rules! mem_loc_dispatch {
    (pub enum $enumname:ident<'a> { $($varname:ident($structname:ident $(<$l:lifetime>)?),)* }) => {
        pub enum $enumname<'a> {
            $($varname($crate::mapper::$structname $(<$l>)?),)*
        }

        impl<'a> MemLocation<'a> for $enumname<'a> {
            fn read(&'a mut self) -> u8 {
                match self {
                    $(
                        Self::$varname(v) => v.read(),
                    )*
                }
            }

            fn write(&'a mut self, value: u8) {
                match self {
                    $(
                        Self::$varname(v) => v.write(value),
                    )*
                }
            }
        }

        $(
        impl<'a> From<$structname $(<$l>)?> for $enumname<'a> {
            fn from(f: $structname $(<$l>)?) -> Self {
                Self::$varname(f)
            }
        }
        )*
    }
}

mem_loc_dispatch! {
    pub enum AnyMemLocation<'a> {
        CPURamLoc(CPURamLoc<'a>),
        RamLocation(RamLocation<'a>),
        RomLocation(RomLocation),
        PPUNametable(PPUNametable<'a>),
        PPUPalette(PPUPalette<'a>),
        PPURegister(PPURegister<'a>),
        APURegister(APURegister<'a>),
        CTLRegister(CTLRegister<'a>),
        Mapper0Ram(Mapper0Ram<'a>),
        Mapper1Location(Mapper1Location<'a>),
        Mapper3Location(Mapper3Location<'a>),
        Mapper4Location(Mapper4Location<'a>),
    }
}
