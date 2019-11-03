type PatternTile = [u8; 16];
type PatternTable = [PatternTile; 256];
type NameTable = [u8; 0x400];

use sdl2::pixels::Color;
use sdl2::rect::Point;
use arrayvec::ArrayVec;
use super::SDLSystem;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct PalettedColor(pub u8);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct NTSCColor(pub u8);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct OAMEntry {
    pub y: u8,
    pub index: u8,
    pub attrs: u8,
    pub x: u8,
}

impl Default for OAMEntry {
    fn default() -> OAMEntry {
        OAMEntry {
            y: 0,
            index: 0,
            attrs: 0,
            x: 0,
        }
    }
}

pub struct PPU {
    pub ctrl: u8,
    pub mask: u8,

    /* TODO: Sprite zero hit, Sprite Overflow, Proper VBlank */
    pub status: u8,

    pub cycles: u128,
    pub address: u16,
    pub scroll: u16,

    pub pattern_tables: [PatternTable; 2],
    pub name_tables: [NameTable; 4],

    pub palette_idxs: [u8; 0x20],

    pub oam_addr: u8,
    pub oam: [OAMEntry; 64],

    cycle: usize,
    scanline: usize,
    pixel: usize,

    colors: Vec<Color>,

}

macro_rules! get_mem {
    ($addr:expr, $ctx:ident, $($ref_t:tt)+) => ({
        if $addr > 0x3FFF {
            println!("\n\nWARNING: ACCESSING OUT OF BOUNDS PPU DATA AT {:#06X}!! \n\n", $addr);
        }
        let addr = $addr % 0x4000;
        /* TODO: Vertical/Horizontal mirroring */
        match addr {
            0x0000..=0x1FFF => $($ref_t)+ $ctx.pattern_tables[(addr / 0x1000) as usize][((addr % 0x1000) / 0x10) as usize][(addr % 0x10) as usize],
            0x2000..=0x3EFF => {
                let mut canon = (addr - 0x2000) % 0x1000;

                if true /* Vertical mirroring */ {
                    canon %= 0x800;
                }
                $($ref_t)+ $ctx.name_tables[(canon / 0x400) as usize][(canon % 0x400) as usize]
            }, 
            /* TODO: Do this better */
            0x3F10 => $($ref_t)+ $ctx.palette_idxs[0x0],
            0x3F14 => $($ref_t)+ $ctx.palette_idxs[0x4],
            0x3F18 => $($ref_t)+ $ctx.palette_idxs[0x8],
            0x3F1C => $($ref_t)+ $ctx.palette_idxs[0xC],
            0x3F00..=0x3FFF => $($ref_t)+ $ctx.palette_idxs[((addr - 0x3F00) % 0x20) as usize],
            _ => panic!("Attempt to access invalid PPU memory location {:#06X}", $addr),
        }
    });
}

impl PPU {
    pub fn new(chr_rom: Vec<u8>) -> PPU {
        assert!(chr_rom.len() == 0x1000 || chr_rom.len() == 0x2000);
        let mut pattern_tables = [[[0; 16]; 256]; 2];
        let loaded: Vec<PatternTable> = {
            chr_rom.chunks(0x1000).map(|d| -> PatternTable {
                d.chunks(16).map(|chunk| chunk.iter().copied().collect::<ArrayVec<[u8; 16]>>().into_inner().unwrap()).collect::<ArrayVec<[_; 256]>>().into_inner().unwrap()
            }).collect()
        };

        for (i, l) in loaded.into_iter().enumerate() {
            pattern_tables[i] = l;
        }

        PPU {
            ctrl: 0,
            scanline: 0,
            pixel: 0,
            mask: 0,
            status: 0,
            cycles: 0,
            address: 0,
            scroll: 0,
            pattern_tables: pattern_tables,
            name_tables: [[0; 0x400]; 4],
            palette_idxs: [0; 0x20],
            oam_addr: 0,
            cycle: 0,
            colors: PPU::get_colors(),
            oam: [OAMEntry::default(); 64],
        }
    }

    /* 
     *  CPU clock speed is 1.789773 MHz
     *  PPU clock speed is 5.369 MHz 
     *
     *  262 scanlines per frame
     *  Each scanline is 341 PPU clock cycles, each clock cycle produces one pixel
     *  1 CPU cycle = 3 PPU cycles
     *
     */
    pub fn next(&mut self, cpu_cycles: usize, sdl_system: &mut SDLSystem) {
        for _ in 0..(3 * cpu_cycles) {
            if self.pixel >= 341 {
                self.scanline += 1;
                self.pixel = 0;
            }
            else {
                self.pixel += 1;
            }

            match self.scanline {
                0..=0       => { /* Pre-render scanline */
                    // TODO: Clear sprite-0 hit at the right time
                    self.status &= !(1 << 6);
                },
                1..=240     => { /* Render scanline */
                    // Rendering
                    self.for_each_pixel(sdl_system);
                },
                241..=241   => { /* Idle scanline */ },
                242..=261   => { /* Vertical blanking line */
                    // Trigger NMI
                    if self.scanline == 242 && self.pixel == 1 {
                        println!("VBlank");
                        self.render_pattern_tables(sdl_system);
                        self.render_oam(sdl_system);
                        sdl_system.present();
                        self.status |= 1 << 7;
                    }
                },
                /* 262 */ _ => { /* Pre-render scanline */ self.scanline = 0; },
            }
        }
    }

    fn get_palette_index(attr_table: &[u8], tile_row: u8, tile_col: u8) -> u8 {
        assert!(attr_table.len() == 64);

        let attr = attr_table[(tile_row / 4 * 8 + tile_col / 4) as usize];

        // attribute = (bottom right << 6) | (bottom left << 4) | (top right << 2) | (top left << 0)
        
        match (tile_row % 2, tile_col % 2) {
            (0, 0) => (attr >> 0) & 0x3, // Top left
            (0, 1) => (attr >> 2) & 0x3, // Top right
            (1, 0) => (attr >> 4) & 0x3, // Bottom left
            (1, 1) => (attr >> 6) & 0x3, // Bottom right
            _ => unreachable!(),
        }
    }

    fn render_pattern_tables(&self, sdl_system: &mut SDLSystem) {
        for (idx, table) in self.pattern_tables.iter().enumerate() {
            for row in 0..16 {
                for col in 0..16 {
                    let entry = &table[row * 16 + col];
                    for y in 0..8 {
                        for x in 0..8 {
                            let mut color = PPU::color_in_pattern(entry, y as u8, x as u8).0 * 85;
                            let p_x = x + col * 8 + idx * 128 + 256;
                            let p_y = y + row * 8;

                            sdl_system.canvas().set_draw_color(Color::RGB(color, color, color));
                            sdl_system.canvas().draw_point(Point::new(p_x as i32, p_y as i32));
                        }
                    }
                }
            }
        }
    }

    fn render_oam(&self, sdl_system: &mut SDLSystem) {
        for row in 0..8 {
            for col in 0..8 {
                let entry = &self.oam[row * 8 + col];
                for y in 0..8 {
                    for x in 0..8 {
                        let (_, color, _) = self.get_sprite_pixel(entry, x as u8, y as u8);

                        sdl_system.canvas().set_draw_color(color);
                        let p_x = x + col * 8 + 256;
                        let p_y = y + row * 8 + 128;
                        sdl_system.canvas().draw_point(Point::new(p_x as i32, p_y as i32)).unwrap();
                    }
                }
            }
        }
    }

    fn get_color(&self, tile_color: PalettedColor, palette_idx: u8, sprite: bool) -> u8 {
        assert!(tile_color.0 <= 0b11);
        assert!(palette_idx <= 0b11);

        let idx = tile_color.0 | palette_idx << 2 | (sprite as u8) << 4;

        self.palette_idxs[idx as usize]
    }

    fn get_colors() -> Vec<Color> {
        std::fs::read_to_string("./palette.txt").unwrap()
            .lines()
            .map(|l| &l[1..])
            .map(|c| (u8::from_str_radix(&c[0..2], 16).unwrap(), u8::from_str_radix(&c[2..4], 16).unwrap(), u8::from_str_radix(&c[4..6], 16).unwrap()))
            .map(|t| Color::from(t))
            .collect()
    }

    fn selected_name_table(&self) -> &NameTable {
        /* TODO: Vertical/horizontal mirroring */
        let selected = self.ctrl & /*3*/ 1;
        &self.name_tables[selected as usize]
    }

    fn selected_attr_table(&self) -> &[u8] {
        let name_table = self.selected_name_table();
        &name_table[(name_table.len() - 64)..]
    }

    fn selected_patt_table_bg(&self) -> &PatternTable {
        &self.pattern_tables[((self.ctrl >> 4) & 1) as usize]
    }

    fn selected_patt_table_sp(&self) -> &PatternTable {
        &self.pattern_tables[((self.ctrl >> 3) & 1) as usize]
    }

    fn color_in_pattern(pattern: &[u8], row: u8, col: u8) -> PalettedColor {
        assert_eq!(pattern.len(), 16);
        assert!(row < 8);
        assert!(col < 8);

        let lo_color = pattern[row as usize];
        let hi_color = pattern[(row + 8) as usize];

        PalettedColor(((hi_color >> (7 - col)) & 1) << 1 | ((lo_color >> (7 - col)) & 1))
    }

    fn get_background_color(&mut self) -> (PalettedColor, Color) {
        let row = (self.scanline - 1) / 8;
        let col = self.pixel / 8;

        let name_table = self.selected_name_table();
        let attr_table = self.selected_attr_table();
        let pattern_table = self.selected_patt_table_bg();

        let tile_name = name_table[row * 32 + col];

        let palette_idx = PPU::get_palette_index(attr_table, row as u8, col as u8);

        let pattern = &pattern_table[tile_name as usize];

        let color = PPU::color_in_pattern(pattern, ((self.scanline - 1) % 8) as u8, (self.pixel % 8) as u8);

        let c = self.get_color(color, palette_idx, false);

        (color, self.colors[c as usize])
    }

    fn get_sprite_pixel(&self, object: &OAMEntry, mut s_x: u8, mut s_y: u8) -> (PalettedColor, Color, bool) {
        assert!(s_x < 8);
        assert!(s_y < 8);

        let palette_idx = object.attrs & 0b11;
            
        let priority = (object.attrs >> 5) & 1 != 0;
        let horiz_flip = (object.attrs >> 6) & 1 == 0;
        let vert_flip = (object.attrs >> 7) & 1 == 0;

        if horiz_flip {
            s_x = 7 - s_x;
        }

        if vert_flip {
            s_y = 7 - s_y;
        }

        let sprite_color = PPU::color_in_pattern(&self.selected_patt_table_sp()[object.index as usize], s_y, s_x);

        let color = self.colors[self.get_color(sprite_color, palette_idx, true) as usize];

        (sprite_color, color, priority)
    }

    fn for_each_pixel(&mut self, sdl_system: &mut SDLSystem) {
        let row = (self.scanline - 1) / 8;
        let col = self.pixel / 8;

        if self.pixel >= 256 {
            return;
        }

        let bg_color = self.get_background_color();

        sdl_system.canvas().set_scale(2.0, 2.0);
        let mut sprite_colors = Vec::new();

        for object in self.oam.iter() {
            if self.pixel <= object.x as usize && (object.x as usize) < self.pixel + 8 {
                if self.scanline - 1 <= object.y as usize && (object.y as usize) < self.scanline + 7 {
                    let s_x = object.x - self.pixel as u8;
                    let s_y = object.y - (self.scanline - 1) as u8;
                    sprite_colors.push(self.get_sprite_pixel(object, s_x, s_y))
                    // TODO: Properly implement 8x16 sprites I think
                }
            }
        }
    
        // TODO: Properly sort sprites
        let output = if sprite_colors.len() == 0 {
            if (bg_color.0).0 == 0 {
                self.colors[self.palette_idxs[0] as usize]
            }
            else {
                bg_color.1
            }
        }
        else {
            let sprite_color = &sprite_colors[0];

            /*if (sprite_color.0).0 != 0 {
                println!("Beep boop {:?}", sprite_color);
                println!("At scanline {} and pixel {}", self.scanline, self.pixel);
            }*/

            // TODO: Check sprite-0 hit conditions properly
            if /*(bg_color.0).0 != 0 &&*/ (sprite_color.0).0 != 0 {
                println!("Sprite-0 hit");
                self.status |= 1 << 6;
            }

            match ((bg_color.0).0, (sprite_color.0).0, sprite_color.2) {
                (0, 0, _)     => self.colors[self.palette_idxs[0] as usize],
                (0, _, _)     => sprite_color.1,
                (_, 0, _)     => bg_color.1,
                (_, _, false) => sprite_color.1,
                (_, _, true)  => bg_color.1,

            }
        };

        sdl_system.canvas().set_draw_color(output);
        sdl_system.canvas().draw_point(Point::new(self.pixel as i32, (self.scanline - 1) as i32)).unwrap();
    }

    fn incr_address(&mut self) {
        if (self.ctrl >> 2) & 1 == 1 {
            self.address += 32;
        }
        else {
            self.address += 1;
        }
    }


    fn dma(&mut self, addr: u8, ctx: &crate::Context) {
        /* TODO: Determine if this should depend on oam_addr */
        print!("OAM: [");
        for idx in 0..64 {
            let full_addr = (addr as u16) << 8 | (idx * 4);

            self.oam[idx as usize] = OAMEntry {
                y: ctx.read(full_addr + 0),
                index: ctx.read(full_addr + 1),
                attrs: ctx.read(full_addr + 2),
                x: ctx.read(full_addr + 3),
            };
            
            print!("{:?}, ", self.oam[idx as usize]);
        }

        println!("]");
    }

    pub fn read(&mut self, reg: u8) -> u8 {
        match reg {
            0 => panic!("Cannot read from controller register"),
            1 => panic!("Cannot read from mask register"),
            2 => { let result = self.status; self.status &= !(1 << 7); result } /* TODO: Reset writes for 5 and 6*/,
            3 => panic!("Cannot read from OAM address register"),
            4 => unimplemented!("OAM data read"),
            5 => panic!("Cannot read from scroll register"),
            6 => panic!("Cannot read from address register"),
            7 => { let result = *get_mem!(self.address, self, &); self.incr_address(); result },
            _ => panic!("Unknown register {}", reg),
        }
    }

    pub fn write(&mut self, reg: u8, value: u8, ctx: &crate::Context) {
        match reg {
            0 => self.ctrl = value,
            1 => self.mask = value,
            2 => panic!("Cannot write to status register"),
            3 => self.oam_addr = value,
            4 => unimplemented!("OAM data write"),
            5 => { self.scroll  <<= 8; self.scroll  |= value as u16; println!("Wrote {:#04X} to PPU scroll", value); }
            6 => { self.address <<= 8; self.address |= value as u16; println!("Wrote {:#04X} to PPU address", value); }
            7 => { *get_mem!(self.address, self, &mut) = value; self.incr_address(); },
            14 => self.dma(value, ctx),
            _ => panic!("Unknown register {}", reg),
        }
    }
}
