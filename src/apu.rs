pub struct APU {
    pub status: u8,
}

impl APU {
    pub fn new() -> APU {
        APU { status: 0 }
    }

    pub fn read(&mut self, reg: u8) -> u8 {
        match reg {
            15 => self.status,
            17 => { println!("Returning 0 from APU frame counter"); 0 } 
            _ => panic!("Attempt to read invalid or write-only APU register {}", reg),
        }
    }

    pub fn write(&mut self, reg: u8, value: u8) {
        println!("Ignoring write of {:#04X} to APU register {}", value, reg);
    }
}
