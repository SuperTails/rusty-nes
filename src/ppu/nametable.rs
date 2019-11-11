pub enum NametableEntry {
    PatternIndex(u8),
    PatternAttribute(u8),
}

impl Into<u8> for NametableEntry {
    fn into(self) -> u8 {
        match self {
            NametableEntry::PatternIndex(value) => value,
            NametableEntry::PatternAttribute(value) => value,
        }
    }
}

pub const NAMETABLE_SIZE: usize = 0x400;

#[derive(Clone)]
pub struct Nametable {
    pub data: [[u8; 32]; 30],
    pub attribute_table: [u8; 64],
}

impl Nametable {
    pub fn index(&self, i: usize) -> NametableEntry {
        if i < 0x3C0 {
            let row = i / 32;
            let col = i % 32;
            NametableEntry::PatternIndex(self.data[row][col])
        }
        else if i < 0x400 {
            NametableEntry::PatternAttribute(self.attribute_table[i - 0x3C0])
        }
        else {
            panic!("Index {:#X} out of range for nametable", i);
        }
    }

    pub fn pattern_at(&self, row: usize, col: usize) -> u8 {
        assert!(row < 0x30);
        assert!(col < 0x32);

        self.data[row][col]
    }

    fn index_mut(&mut self, i: usize) -> &mut u8 {
        if i < 0x3C0 {
            let row = i / 32;
            let col = i % 32;
            &mut self.data[row][col]
        }
        else if i < 0x400 {
            &mut self.attribute_table[i - 0x3C0]
        }
        else {
            panic!("Index {:#X} out of range for nametable", i);
        }
       
    }

    pub fn write(&mut self, i: usize, value: u8) {
        *self.index_mut(i) = value;
    }
}

impl Default for Nametable {
    fn default() -> Nametable {
        Nametable {
            data: [[0; 32]; 30],
            attribute_table: [0; 64],
        }
    }
}
