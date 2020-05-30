use crate::mem_location::*;

mod mapper0;
mod mapper1;
mod mapper3;
mod mapper4;

pub use mapper0::*;
pub use mapper1::*;
pub use mapper3::*;
pub use mapper4::*;

type MapperResult<'a> = AnyMemLocation<'a>;

// why can't the enum_dispatch crate just work with rls, ughhh
macro_rules! mem_loc_dispatch {
    (pub enum $enumname:ident<'a> { $($varname:ident($structname:ident $(<$l:lifetime>)?),)* }) => {
        pub enum $enumname<'a> {
            $($varname($crate::mapper::$structname $(<$l>)?),)*
        }

        impl<'a> crate::mem_location::MemLocation for $enumname<'a> {
            fn read(&mut self) -> u8 {
                match self {
                    $(
                        Self::$varname(v) => v.read(),
                    )*
                }
            }

            fn write(&mut self, value: u8) {
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
        Mapper1Location(Mapper1Location<'a>),
        Mapper3Location(Mapper3Location<'a>),
        Mapper4Location(Mapper4Location<'a>),
    }
}

pub trait CpuMapper {
    type Context;

    fn read_mem_cpu(&mut self, addr: u16, context: &mut Self::Context) -> u8;
    fn write_mem_cpu(&mut self, addr: u16, value: u8, context: &mut Self::Context);
}