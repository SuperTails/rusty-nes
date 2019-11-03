use std::error::Error;
use std::io::prelude::*;
use std::path::Path;
use std::fs::File;
use std::fmt;

pub struct Rom {
    pub trainer: Option<[u8; 512]>,
    pub prg_rom: Vec<u8>,
    pub chr_rom: Vec<u8>,
    pub misc_rom: Vec<u8>,
    pub mapper: u16,
    pub submapper: u8,
}

#[derive(Debug)]
pub struct RomReadError {
    pub error: String,
}

impl fmt::Display for RomReadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "RomReadError: {}", self.error)
    }
}

impl Error for RomReadError {}

impl Rom {
    pub fn read_area(size_upper: u8, size_lower: u8, file: &mut File, prg: bool) -> Vec<u8> {
        assert!(size_upper <= 0xF);

        let chunk_size = if prg { 0x4000 } else { 0x2000 };

        let size =
            if size_upper == 0xF {
                // Exponent-multiplier notation
                let exp = size_lower >> 2;
                let mul = (size_lower & 0x3) * 2 + 1;

                mul as usize * (2 << exp as usize)
            }
            else {
                ((size_upper as usize) << 4 | (size_lower as usize)) * chunk_size
            };

        let mut result = Vec::with_capacity(size);
        result.resize(size, 0);

        file.read_exact(&mut result).unwrap_or_else(|err| {
            println!("Failed to fill buffer of size {}KiB", size / chunk_size);  
            std::process::exit(1);
        });

        result
    }

    pub fn new(path: &Path) -> Result<Rom, Box<dyn Error>> {
        let mut file = File::open(path)?;

        let mut header = [0; 16];
        file.read_exact(&mut header)?;

        if &header[0..4] != b"NES\x1A" {
            return Err(Box::new(RomReadError { error: format!("Incorrect file marker {:?}", &header[0..4]) }));
        }

        let mapper = (header[8] as u16 & 0x0F) << 8 | ((header[7] & 0xF0)| (header[6] >> 4)) as u16;
        let submapper = header[8] >> 4;
        let fs_mode = (header[6] >> 3) & 1 != 0;
        let trainer = (header[6] >> 2) & 1 != 0;
        let battery = (header[6] >> 1) & 1 != 0;
        let mirro_t = (header[6] >> 0) & 1 != 0;

        if header[7] & 0b11 != 0b00 {
            return Err(Box::new(RomReadError { error: format!("Unsupported console {}", header[7] & 0x3) }));
        }

        let version = header[7] >> 2;

        if version != 3 && version != 0 {
            return Err(Box::new(RomReadError { error: format!("Incorrect file version {}", (header[7] >> 2) & 0x3) }));
        }

        let prg_ram_eeprom_size = header[10];
        let chr_ram_size = header[11];

        let timing = header[12];

        let misc_roms = header[14] & 0x03;

        let def_exp_device = header[15] & 0x3F;

        let trainer = trainer.then_with(|| {
           let mut trainer_buf = [0; 512]; 

           file.read_exact(&mut trainer_buf);

           trainer_buf
        });

        let prg_rom = Rom::read_area(header[9] & 0x0F, header[4], &mut file, true);
        let chr_rom = Rom::read_area((header[9] & 0xF0) >> 4, header[5], &mut file, false);

        let mut misc_rom = Vec::new();

        file.read_to_end(&mut misc_rom)?;

        Ok(Rom{ trainer, prg_rom, chr_rom, misc_rom, mapper, submapper })
    }
}


