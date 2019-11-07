use sdl2::pixels::Color;
use sdl2::rect::Point;
use super::SDLSystem;
use crate::mapper::Mapped;
use std::cell::RefCell;
use std::num::Wrapping;
use crate::Context;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct PalettedColor(pub u8);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct NTSCColor(pub u8);

bitfield!{
    #[derive(Clone, Copy)]
    pub struct OAMEntryAttrs(u8);
    impl Debug;

    palette_idx, set_palette_idx: 1, 0;

    priority, set_priority: 5;

    horiz_flip, set_horiz_flip: 6;

    vert_flip, set_vert_flip: 7;
}

#[derive(Clone, Copy, Debug)]
pub struct OAMEntry {
    pub y: u8,
    pub index: u8,
    pub attrs: OAMEntryAttrs,
    pub x: u8,
}

impl Default for OAMEntry {
    fn default() -> OAMEntry {
        OAMEntry {
            y: 0,
            index: 0,
            attrs: OAMEntryAttrs(0),
            x: 0,
        }
    }
}

bitfield! {
    pub struct PPUCtrl(u8);
    impl Debug;

    nametable_addr, set_nametable_addr: 1, 0;
    
    increment_mode, set_incr_mode: 2;

    sp_table_addr, set_sp_table_addr: 3;
    bg_table_addr, set_bg_table_addr: 4;

    sprite_size, set_sprite_size: 5;
    is_master, set_master: 6;

    nmi_enabled, set_nmi_enabled: 7;
}

fn get_palette_index(attr_table: &[u8], tile_row: u8, tile_col: u8) -> u8 {
    assert!(attr_table.len() == 64);

    let attr = attr_table[(tile_row / 4 * 8 + tile_col / 4) as usize];

    // attribute = (bottom right << 6) | (bottom left << 4) | (top right << 2) | (top left << 0)
    
    match ((tile_row / 2) % 2, (tile_col / 2) % 2) {
        (0, 0) => (attr >> 0) & 0x3, // Top left
        (0, 1) => (attr >> 2) & 0x3, // Top right
        (1, 0) => (attr >> 4) & 0x3, // Bottom left
        (1, 1) => (attr >> 6) & 0x3, // Bottom right
        _ => unreachable!(),
    }
}

fn render_pattern_tables(sdl_system: &mut SDLSystem, mapper: &mut dyn Mapped) {
    for table in 0..2 {
        for row in 0..16 {
            for col in 0..16 {
                // TODO: Don't read the whole thing
                let mut entry = Vec::with_capacity(16);
                for i in 0..16 {
                    entry.push(mapper.mem_ppu(i + (row * 16 + col) * 16 + 0x1000 * table).read());
                }
                for y in 0..8 {
                    for x in 0..8 {
                        let color = color_in_pattern(&entry, y as u8, x as u8).0 * 85;
                        let p_x = x + col * 8 + table * 128 + 256;
                        let p_y = y + row * 8;

                        sdl_system.canvas().set_draw_color(Color::RGB(color, color, color));
                        sdl_system.canvas().draw_point(Point::new(p_x as i32, p_y as i32)).unwrap();
                    }
                }
            }
        }
    }
}

fn color_in_pattern(pattern: &[u8], row: u8, col: u8) -> PalettedColor {
    assert_eq!(pattern.len(), 16);
    assert!(row < 8);
    assert!(col < 8);

    let lo_color = pattern[row as usize];
    let hi_color = pattern[(row + 8) as usize];

    PalettedColor(((hi_color >> (7 - col)) & 1) << 1 | ((lo_color >> (7 - col)) & 1))
}

pub struct PPU {
    pub ctrl: PPUCtrl,

    pub mask: u8,

    /* TODO: Sprite zero hit, Sprite Overflow, Proper VBlank */
    pub status: RefCell<u8>,

    pub cycles: u128,
    pub address: RefCell<u16>,
    pub scroll: RefCell<u16>,

    pub name_tables: RefCell<Vec<u8>>,

    pub palette_idxs: RefCell<[u8; 0x20]>,

    pub oam_addr: RefCell<u8>,
    pub oam: RefCell<[OAMEntry; 64]>,

    prev_nmi_state: bool,

    scanline: usize,
    pixel: usize,

    colors: Vec<Color>,
}

impl PPU {
    pub fn new() -> PPU {
        PPU {
            prev_nmi_state: false,
            ctrl: PPUCtrl(0),
            scanline: 0,
            pixel: 0,
            mask: 0,
            status: RefCell::new(0),
            cycles: 0,
            address: RefCell::new(0),
            scroll: RefCell::new(0),
            name_tables: RefCell::new([0; 0x800].to_vec()),
            palette_idxs: RefCell::new([0; 0x20]),
            oam_addr: RefCell::new(0),
            colors: PPU::get_colors(),
            oam: RefCell::new([OAMEntry::default(); 64]),
        }
    }

    pub fn nmi_falling(&mut self) -> bool {
        let nmi_occurred = *self.status.borrow() & 0x80 != 0;

        let new_state = self.ctrl.nmi_enabled() && nmi_occurred;

        let falling = new_state && !self.prev_nmi_state;

        self.prev_nmi_state = new_state;

        falling
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
    pub fn next(&mut self, cpu_cycles: usize, sdl_system: &mut SDLSystem, mapper: &mut dyn Mapped) {
        for _ in 0..(3 * cpu_cycles) {
            if self.pixel >= 341 {
                self.scanline += 1;
                self.pixel = 0;
            }
            else {
                self.pixel += 1;
            }

            if (0..=240).contains(&self.scanline) {
                if (257..=320).contains(&self.pixel) {
                    *self.oam_addr.borrow_mut() = 0;
                }
            }

            match self.scanline {
                0..=0       => { /* Pre-render scanline */
                    // TODO: Clear sprite-0 hit at the right time
                    *self.status.borrow_mut() &= !(1 << 6);
                },
                1..=240     => { /* Render scanline */
                    // Rendering
                    self.for_each_pixel(sdl_system, mapper);
                },
                241..=241   => { /* Idle scanline */ },
                242..=261   => { /* Vertical blanking line */
                    // Trigger NMI
                    if self.scanline == 242 && self.pixel == 1 {
                        println!("VBlank");
                        render_pattern_tables(sdl_system, mapper);
                        self.render_oam(sdl_system, mapper);
                        /*for y in (0..(240 / 32)).map(|y| y * 32) {
                            for x in (0..(256 / 32)).map(|x| x * 32) {
                                sdl_system.canvas().draw_rect(sdl2::rect::Rect::new(x as i32, y as i32, 32, 32)).unwrap();
                            }
                        }*/
                        sdl_system.present();
                        *self.status.borrow_mut() |= 1 << 7;
                    }
                },
                /* 262 */ _ => { /* Pre-render scanline */
                    self.scanline = 0;
                    /* TODO: Check if this is the right time to clear nmi_occurred */
                    *self.status.borrow_mut() &= !0x80;
                },
            }
        }
    }

    fn render_oam(&self, sdl_system: &mut SDLSystem, mapper: &mut dyn Mapped) {
        for row in 0..8 {
            for col in 0..8 {
                let entry = &self.oam.borrow()[row * 8 + col];
                let height = if self.ctrl.sprite_size() { 16 } else { 8 };
                for y in 0..height {
                    for x in 0..8 {
                        let (_, color, _) = self.get_sprite_pixel(entry, x as u8, y as u8, mapper);

                        sdl_system.canvas().set_draw_color(color);
                        let p_x = x + col * 8 + 256;
                        let p_y = y + row * height + 128;
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

        self.palette_idxs.borrow()[idx as usize]
    }

    fn get_colors() -> Vec<Color> {
        std::fs::read_to_string("./palette.txt").unwrap()
            .lines()
            .map(|l| &l[1..])
            .map(|c| (u8::from_str_radix(&c[0..2], 16).unwrap(), u8::from_str_radix(&c[2..4], 16).unwrap(), u8::from_str_radix(&c[4..6], 16).unwrap()))
            .map(|t| Color::from(t))
            .collect()
    }

    fn selected_name_table(&self) -> usize {
        /* TODO: Vertical/horizontal mirroring */
        let x_scroll = (*self.scroll.borrow() >> 8) as usize;
        let y_scroll = (*self.scroll.borrow() & 0xFF) as usize;
        let selected = (self.ctrl.nametable_addr() & /*3*/ 1) as usize * 0x400 + 32 * (y_scroll / 8) + (x_scroll / 8);
        selected
    }

    fn selected_attr_table(&self) -> usize {
        self.selected_name_table() + 0x400 - 64
    }

    fn selected_patt_table_bg(&self) -> u16 {
        0x1000 * self.ctrl.bg_table_addr() as u16
    }

    fn selected_patt_table_sp(&self) -> u16 {
        0x1000 * self.ctrl.sp_table_addr() as u16
    }


    fn get_background_color(&self, mapper: &mut dyn Mapped, x: usize, y: usize) -> (PalettedColor, Color) {
        let row = y / 8;
        let col = x / 8;

        let name_tables = self.name_tables.borrow();
        let name_table = &name_tables[self.selected_name_table()..self.selected_name_table()+0x400];
        let attr_table = self.selected_attr_table();
        let pattern_table = self.selected_patt_table_bg();

        let tile_name = name_table[row * 32 + col];

        let palette_idx = get_palette_index(&name_table[attr_table..attr_table+64], row as u8, col as u8);

        let mut pattern = Vec::with_capacity(16);
        for i in 0..16 {
            pattern.push(mapper.mem_ppu(i + pattern_table + 16 * tile_name as u16).read());
        }

        let color = color_in_pattern(&pattern, (y % 8) as u8, (x % 8) as u8);

        let c = self.get_color(color, palette_idx, false);

        (color, self.colors[c as usize])
    }

    fn get_sprite_pixel(&self, object: &OAMEntry, mut s_x: u8, mut s_y: u8, mapper: &mut dyn Mapped) -> (PalettedColor, Color, bool) {
        let max_height = if self.ctrl.sprite_size() {
            16
        }
        else {
            8
        };

        assert!(s_x < 8);
        assert!(s_y < max_height);

        let mut is_bottom = s_y >= 8;
        s_y %= 8;

        if object.attrs.horiz_flip() {
            s_x = 7 - s_x;
        }

        if object.attrs.vert_flip() {
            is_bottom = !is_bottom;
            s_y = 7 - s_y;
        }

        let selected_patt_table = if self.ctrl.sprite_size() {
            0x1000 * ((object.index & 1) as u16)
        }
        else {
            self.selected_patt_table_sp()
        };

        let index = if self.ctrl.sprite_size() {
            (object.index & !1) + is_bottom as u8
        }
        else {
            object.index
        };

        let mut pattern = Vec::with_capacity(16);
        for i in 0..16 {
            pattern.push(mapper.mem_ppu(i + selected_patt_table + 16 * index as u16).read());
        }

        let sprite_color = color_in_pattern(&pattern, s_y, s_x);

        let color = self.colors[self.get_color(sprite_color, object.attrs.palette_idx(), true) as usize];

        (sprite_color, color, object.attrs.priority())
    }

    fn for_each_pixel(&mut self, sdl_system: &mut SDLSystem, mapper: &mut dyn Mapped) {
        let scroll = self.scroll.borrow();

        if self.pixel + ((*scroll >> 8) % 8) as usize >= 256 {
            return;
        }

        let bg_color = self.get_background_color(mapper, self.pixel + ((*scroll >> 8) % 8) as usize, self.scanline - 1 + (*scroll as usize & 0xFF) % 8);

        sdl_system.canvas().set_scale(2.0, 2.0).unwrap();
        let mut sprite_colors = Vec::new();

        for object in self.oam.borrow().iter() {
            if self.scanline < 2 {
                break;
            }

            let max_height = if self.ctrl.sprite_size() { 16 } else { 8 };

            if (object.x as usize..object.x as usize + 8).contains(&self.pixel) {
                    if (object.y as usize..object.y as usize + max_height).contains(&(self.scanline - 2)) {
                        let s_x = self.pixel as u8 - object.x;
                        let s_y = (self.scanline - 2) as u8 - object.y;
                        sprite_colors.push(self.get_sprite_pixel(object, s_x, s_y, mapper))
                    }
                }
            }
    
        // TODO: Properly sort sprites
        let output = if sprite_colors.len() == 0 {
            if (bg_color.0).0 == 0 {
                self.colors[self.palette_idxs.borrow()[0] as usize]
            }
            else {
                bg_color.1
            }
        }
        else {
            let sprite_color = &sprite_colors[0];

            // TODO: I think this should only be triggered once
            if (bg_color.0).0 != 0 && (sprite_color.0).0 != 0 {
                *self.status.borrow_mut() |= 1 << 6;
            }

            // TODO: Check sprite-0 hit conditions properly
            if (bg_color.0).0 != 0 && (sprite_color.0).0 != 0 {
                *self.status.borrow_mut() |= 1 << 6;
            }

            match ((bg_color.0).0, (sprite_color.0).0, sprite_color.2) {
                (0, 0, _)     => self.colors[self.palette_idxs.borrow()[0] as usize],
                (0, _, _)     => sprite_color.1,
                (_, 0, _)     => bg_color.1,
                (_, _, false) => sprite_color.1,
                (_, _, true)  => bg_color.1,

            }
        };

        sdl_system.canvas().set_draw_color(output);
        sdl_system.canvas().draw_point(Point::new(self.pixel as i32, (self.scanline - 1) as i32)).unwrap();
    }

    pub fn incr_address(&mut self) {
        if self.ctrl.increment_mode() {
            *self.address.borrow_mut() += 32;
        }
        else {
            *self.address.borrow_mut() += 1;
        }
    }


    pub fn dma(&self, addr: u8, ctx: &crate::Context) {
        /* TODO: Determine if this should depend on oam_addr */
        //print!("{:#06X}, OAM: [", addr);
        for idx in 0..64 {
            let full_addr = (addr as u16) << 8 | (idx * 4);

            // TODO: Access properly
            self.oam.borrow_mut()[idx as usize] = OAMEntry {
                y: ctx.native_ram.borrow()[(full_addr + 0) as usize],
                index: ctx.native_ram.borrow()[(full_addr + 1) as usize],
                attrs: OAMEntryAttrs(ctx.native_ram.borrow()[(full_addr + 2) as usize]),
                x: ctx.native_ram.borrow()[(full_addr + 3) as usize],
            };
            
            /*if self.oam[idx as usize].y != 248 {
                print!("{:02X}: {:?}, ", idx, self.oam[idx as usize]);
            }*/
        }

        //println!("]");
    }

    /*pub fn read(&mut self, reg: u8, context: &Context) -> u8 {
        /* TODO: Figure out which of these you *can* read from*/
        match reg {
            0 => self.ctrl.0,
            1 => self.mask,
            2 => { let result = self.status; self.status &= !(1 << 7); result }, /* TODO: figure out resetting address latches */
            /*3 => { println!("Cannot read from OAM address register"); 0 },*/
            4 => {
                let array_addr = self.oam_addr / 4;
                let entry = &self.oam[array_addr as usize];
                let result = match self.oam_addr % 4 {
                    0 => entry.y,
                    1 => entry.index,
                    2 => entry.attrs.0,
                    3 => entry.x,
                    _ => unreachable!(),
                };

                self.oam_addr = (Wrapping(self.oam_addr) + Wrapping(1)).0;

                result
            },
            /*5 => { println!("Cannot read from scroll register"); 0 },
            6 => { println!("Cannot read from address register"); 0 },*/
            3|5|6 => 0,
            7 => {
                let result = context.ppu_address(self.address).read();
                self.incr_address();
                result
            },
            _ => panic!("Unknown register {}", reg),
        }
    }

    pub fn write(&mut self, reg: u8, value: u8, ctx: &crate::Context) {
        //println!("Writing {} to reg {}", value, reg);
        match reg {
            0 => self.ctrl = PPUCtrl(value),
            1 => self.mask = value,
            2 => println!("Cannot write to status register"),
            3 => *self.oam_addr.borrow_mut() = value,
            4 => {
                let array_addr = self.oam_addr / 4;
                let entry = &mut self.oam[array_addr as usize];
                match self.oam_addr % 4 {
                    0 => entry.y = value,
                    1 => entry.index = value,
                    2 => entry.attrs = OAMEntryAttrs(value),
                    3 => entry.x = value,
                    _ => unreachable!(),
                };

                self.oam_addr = (Wrapping(self.oam_addr) + Wrapping(1)).0;
            },
            5 => {
                self.scroll  <<= 8;
                self.scroll  |= value as u16;
                //println!("Wrote {:#04X} to PPU scroll", value);
                if self.scroll != 0 {
                    println!("Ignoring scroll {}", self.scroll);
                    //unimplemented!("Scrolling");
                }
            },
            6 => {
                self.address <<= 8;
                self.address |= value as u16;
                //println!("Wrote {:#04X} to PPU address", value);
            },
            7 => { 
                ctx.ppu_address(self.address).write(value);
                self.incr_address();
            },
            14 => self.dma(value, ctx),
            _ => panic!("Unknown register {}", reg),
        }
    }*/
}
