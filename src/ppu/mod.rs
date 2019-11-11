use crate::cpu::CPU;
use crate::mem_location::MemLocation;
use crate::Context;
use sdl2::pixels::{Color, PixelFormatEnum};
use sdl2::rect::Rect;
use sdl2::surface::Surface;
use std::cell::RefCell;
use std::time::Instant;

pub mod nametable;

use nametable::{Nametable, NAMETABLE_SIZE};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct PalettedColor(pub u8);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct NTSCColor(pub u8);

bitfield! {
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

pub struct PPU {
    pub ctrl: PPUCtrl,

    pub mask: u8,

    pub last_value: u8,

    /* TODO: Sprite zero hit, Sprite Overflow, Proper VBlank */
    pub status: u8,

    pub cycles: u128,
    pub address: u16,
    pub scroll: u16,

    pub nametables: Vec<Nametable>,

    pub palette_idxs: [u8; 0x20],

    pub oam_addr: u8,
    pub oam: [OAMEntry; 64],

    pub dma_request: Option<u8>,

    pub target: Surface<'static>,

    frame: usize,

    read_buffer: RefCell<u8>,

    prev_nmi_state: bool,

    vblank_occurred: bool,

    last_frames: Vec<Instant>,

    scanline: usize,
    pixel: usize,

    colors: Vec<Color>,
}

impl PPU {
    fn make_target(pixel_format: &PixelFormatEnum) -> Surface<'static> {
        Surface::new(1024, 480, *pixel_format).unwrap()
    }

    pub fn new(pixel_format: &PixelFormatEnum) -> PPU {
        PPU {
            dma_request: None,
            prev_nmi_state: false,
            ctrl: PPUCtrl(0),
            frame: 0,
            scanline: 0,
            pixel: 0,
            mask: 0,
            last_value: 0,
            last_frames: [Instant::now(); 10].to_vec(),
            target: PPU::make_target(pixel_format),
            status: 0,
            vblank_occurred: false,
            cycles: 0,
            address: 0,
            scroll: 0,
            read_buffer: RefCell::new(0),
            nametables: vec![Nametable::default(), Nametable::default()],
            palette_idxs: [
                1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23,
                24, 25, 26, 27, 28, 29, 30, 31, 32,
            ],
            oam_addr: 0,
            colors: PPU::get_colors(),
            oam: [OAMEntry::default(); 64],
        }
    }

    fn render_pattern_tables(&mut self, context: &Context) {
        for table in 0..2 {
            for row in 0..16 {
                for col in 0..16 {
                    let base_addr = (row * 16 + col) * 16 + 0x1000 * table;
                    for y in 0..8 {
                        for x in 0..8 {
                            let color = self
                                .color_pattern_mapped(base_addr, y as u8, x as u8, context)
                                .0
                                * 85;

                            let p_x = x + col * 8 + table * 128 + 512;
                            let p_y = y + row * 8;

                            self.set_pixel_positioned(p_y as usize, p_x as usize, &Color::RGB(color, color, color));
                        }
                    }
                }
            }
        }
    }

    fn color_pattern_mapped(
        &self,
        base_addr: u16,
        row: u8,
        col: u8,
        context: &Context,
    ) -> PalettedColor {
        assert!(row < 8);
        assert!(col < 8);

        let lo_color = self.read(base_addr + row as u16, context);
        let hi_color = self.read(base_addr + (row + 8) as u16, context);

        PalettedColor(((hi_color >> (7 - col)) & 1) << 1 | ((lo_color >> (7 - col)) & 1))
    }

    fn read_with_shadowed(&self, addr: u16, context: &Context) -> u8 {
        let addr = addr % 0x4000;
        match addr {
            0x0000..=0x1FFF => context.mapper.mem_ppu(addr).read(),
            0x2000..=0x3FFF => {
                let relative = context.mapper.map_nametable_relative(addr - 0x2000) as usize;
                let table = relative / NAMETABLE_SIZE;
                let entry = relative % NAMETABLE_SIZE;
                self.nametables[table].index(entry).into()
            }
            _ => unreachable!(),
        }
    }

    pub fn read(&self, addr: u16, context: &Context) -> u8 {
        match addr % 0x4000 {
            0x0000..=0x3EFF => self.read_with_shadowed(addr, context),
            0x3F10 => self.palette_idxs[0x0],
            0x3F14 => self.palette_idxs[0x4],
            0x3F18 => self.palette_idxs[0x8],
            0x3F1C => self.palette_idxs[0xC],
            0x3F00..=0x3FFF => self.palette_idxs[((addr - 0x3F00) % 0x20) as usize],
            _ => unreachable!(),
        }
    }

    pub fn read_buffered(&self, addr: u16, context: &Context) -> u8 {
        let shadowed = self.read_with_shadowed(addr, context);

        let old_buffer = *self.read_buffer.borrow();

        *self.read_buffer.borrow_mut() = shadowed;

        match addr % 0x4000 {
            0x0000..=0x3EFF => old_buffer,
            0x3F10 => self.palette_idxs[0x0],
            0x3F14 => self.palette_idxs[0x4],
            0x3F18 => self.palette_idxs[0x8],
            0x3F1C => self.palette_idxs[0xC],
            0x3F00..=0x3FFF => self.palette_idxs[((addr - 0x3F00) % 0x20) as usize],
            _ => unreachable!(),
        }
    }

    pub fn write(&mut self, mut addr: u16, value: u8, context: &Context) {
        addr %= 0x4000;

        match addr {
            0x0000..=0x1FFF => context.mapper.mem_ppu(addr).write(value),
            0x2000..=0x3EFF => {
                let relative = context.mapper.map_nametable_relative(addr - 0x2000) as usize;
                let table = relative / NAMETABLE_SIZE;
                let entry = relative % NAMETABLE_SIZE;
                self.nametables[table].write(entry, value);
            }
            0x3F10 => self.palette_idxs[0x0] = value,
            0x3F14 => self.palette_idxs[0x4] = value,
            0x3F18 => self.palette_idxs[0x8] = value,
            0x3F1C => self.palette_idxs[0xC] = value,
            0x3F00..=0x3FFF => self.palette_idxs[((addr - 0x3F00) % 0x20) as usize] = value,
            _ => unreachable!(),
        }
    }

    pub fn vblank_occurred(&mut self) -> bool {
        std::mem::replace(&mut self.vblank_occurred, false)
    }

    pub fn nmi_falling(&mut self) -> bool {
        let nmi_occurred = self.status & 0x80 != 0;

        let new_state = self.ctrl.nmi_enabled() && nmi_occurred;

        !std::mem::replace(&mut self.prev_nmi_state, new_state) && new_state
    }

    fn on_vblank_start(&mut self, context: &Context) {
        while self.last_frames[0].elapsed().as_micros() / 10 < 16_666 {
            std::thread::sleep(std::time::Duration::from_micros(100));
        }

        let frame_time = (self.last_frames[0].elapsed().as_micros() as f64
            / self.last_frames.len() as f64)
            / 1000.0;

        self.last_frames.remove(0);
        self.last_frames.push(Instant::now());

        let framerate = 1000.0 / frame_time;

        self.render_pattern_tables(context);
        self.render_oam(context);
        //self.render_nametables(context);
        /*for y in (0..(240 / 32)).map(|y| y * 32) {
            for x in (0..(256 / 32)).map(|x| x * 32) {
                sdl_system.canvas().draw_rect(sdl2::rect::Rect::new(x as i32, y as i32, 32, 32)).unwrap();
            }
        }*/
        let mut sdl_system = context.sdl_system.borrow_mut();
        let creator = sdl_system.canvas().texture_creator();
        let tex = creator
            .create_texture_from_surface(
                &mut self.target
                
            )
            .unwrap();
        sdl_system
            .canvas()
            .copy(&tex, None, Rect::new(0, 0, self.target.width(), self.target.height()))
            .unwrap();
        sdl_system.present();
        sdl_system.canvas().set_draw_color(Color::from((0, 0, 0)));
        sdl_system.canvas().clear();

        //println!("VBlank occurred");

        self.status |= 0x80;
        self.vblank_occurred = true;
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
    pub fn next(&mut self, cpu_cycles: usize, context: &Context, cpu: &CPU) {
        for _ in 0..(3 * cpu_cycles) {
            if let Some(addr) = self.dma_request.take() {
                self.dma(addr, cpu, context);
            }

            if (0..=240).contains(&self.scanline) {
                if (257..=320).contains(&self.pixel) {
                    self.oam_addr = 0;
                }
            }

            match self.scanline {
                0..=0 => {
                    /* Pre-render scanline */
                    // TODO: Clear sprite-0 hit at the right time
                    self.status &= !0x40;
                }
                1..=240 => {
                    /* Render scanline */
                    self.for_each_pixel(context);
                }
                241..=241 => { /* Idle scanline */ }
                242..=261 => {
                    /* Vertical blanking line */
                    // Trigger NMI
                    if self.scanline == 242 && self.pixel == 1 {
                        self.on_vblank_start(context);
                    }

                    // This may be off by a cycle or two
                    if self.scanline == 261 && self.pixel == 290 {
                        self.status &= !0x80;
                    }
                }
                262 => {

                    /* Pre-render scanline */
                    let last_pixel = if self.frame % 2 == 0 { 341 } else { 340 };

                    if self.pixel == last_pixel {
                        self.scanline = 0;
                        self.pixel = 0;
                        self.frame += 1;
                    }
                }
                _ => unreachable!(),
            }

            if self.pixel >= 341 {
                self.scanline += 1;
                self.pixel = 0;
            } else {
                self.pixel += 1;
            }
        }
    }

    fn render_oam(&mut self, context: &Context) {
        let height = if self.ctrl.sprite_size() { 16 } else { 8 };
        for row in 0..8 {
            for y in 0..height {
                for col in 0..8 {
                    for x in 0..8 {
                        let (_, color, _) = self.get_sprite_pixel(&self.oam[row * 8 + col], x as u8, y as u8, context);

                        let p_x = x + col * 8 + 512 + 256;
                        let p_y = y + row * height;

                        self.set_pixel_positioned(p_y as usize, p_x as usize, &color);
                    }
                }
            }
        }
    }

    fn render_nametables(&mut self, context: &Context) {
        for row in 0..60 {
            for y in 0..8 {
                for col in 0..64 {
                    for x in 0..8 {
                        let p_x = x + col * 8;
                        let p_y = y + row * 8;

                        let addr = col + row * 32;

                        let addr = context.mapper.map_nametable_relative(addr);
                        
                        let color = self.color_pattern_mapped(addr, (y % 8) as u8, (x % 8) as u8, context).0 * 85;

                        self.set_pixel_positioned(p_y as usize, p_x as usize, &Color::RGB(color, color, color));
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
        std::fs::read_to_string("./palette.txt")
            .unwrap()
            .lines()
            .map(|l| &l[1..])
            .map(|c| {
                (
                    u8::from_str_radix(&c[0..2], 16).unwrap(),
                    u8::from_str_radix(&c[2..4], 16).unwrap(),
                    u8::from_str_radix(&c[4..6], 16).unwrap(),
                )
            })
            .map(|t| Color::from(t))
            .collect()
    }

    fn selected_name_table(&self) -> usize {
        /* TODO: Vertical/horizontal mirroring */
        /*let x_scroll = (self.scroll >> 8) as usize;
        let y_scroll = (self.scroll & 0xFF) as usize;
        let selected = (self.ctrl.nametable_addr() & /*3*/ 1) as usize * 0x400 + 32 * (y_scroll / 8) + (x_scroll / 8);
        selected*/
        (self.ctrl.nametable_addr() & 1) as usize * 0x400
    }

    fn selected_patt_table_bg(&self) -> u16 {
        0x1000 * self.ctrl.bg_table_addr() as u16
    }

    fn selected_patt_table_sp(&self) -> u16 {
        0x1000 * self.ctrl.sp_table_addr() as u16
    }

    fn get_background_color(
        &self,
        mut x: usize,
        mut y: usize,
        context: &Context,
    ) -> (PalettedColor, Color) {
        let wrap = x >= 256 || y >= 256;
        x %= 256;
        y %= 240;

        let row = y / 8;
        let col = x / 8;

        let selected =
            (self.selected_name_table() + wrap as usize * NAMETABLE_SIZE) as usize % (self.nametables.len() * NAMETABLE_SIZE);

        let name_table = &self.nametables[selected / NAMETABLE_SIZE];
        let pattern_table = self.selected_patt_table_bg();

        let tile_name = name_table.pattern_at(row, col);

        let palette_idx = get_palette_index(&name_table.attribute_table, row as u8, col as u8);

        let base_addr = pattern_table + 16 * tile_name as u16;

        let color = self.color_pattern_mapped(base_addr, (y % 8) as u8, (x % 8) as u8, context);

        let c = self.get_color(color, palette_idx, false);

        (color, self.colors[c as usize % self.colors.len()])
    }

    fn get_sprite_pixel(
        &self,
        object: &OAMEntry,
        mut s_x: u8,
        mut s_y: u8,
        context: &Context,
    ) -> (PalettedColor, Color, bool) {
        let max_height = if self.ctrl.sprite_size() { 16 } else { 8 };

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
        } else {
            self.selected_patt_table_sp()
        };

        let index = if self.ctrl.sprite_size() {
            (object.index & !1) + is_bottom as u8
        } else {
            object.index
        };

        let base_addr = selected_patt_table + 16 * index as u16;
        let sprite_color = self.color_pattern_mapped(base_addr, s_y, s_x, context);

        let color = self.colors[self.get_color(sprite_color, object.attrs.palette_idx(), true)
            as usize
            % self.colors.len()];

        (sprite_color, color, object.attrs.priority())
    }

    fn for_each_pixel(&mut self, context: &Context) {
        let x_scroll = self.scroll >> 8;
        let y_scroll = self.scroll & 0xFF;

        /*if self.pixel + ((scroll >> 8) % 8) as usize >= 256 {
            return;
        }*/

        if self.pixel as usize >= 256 {
            return;
        }

        let bg_color = self.get_background_color(
            self.pixel + x_scroll as usize,
            self.scanline - 1 + y_scroll as usize,
            context,
        );

        let mut sprite_color: Option<(PalettedColor, Color, bool)> = None;

        if self.scanline >= 2 {
            let max_height = if self.ctrl.sprite_size() { 16 } else { 8 };
            for object in self.oam.iter() {
                if object.x as usize <= self.pixel && self.pixel < object.x as usize + 8 {
                    if object.y as usize <= self.scanline - 2
                        && self.scanline - 2 < object.y as usize + max_height
                    {
                        let s_x = self.pixel as u8 - object.x;
                        let s_y = (self.scanline - 2) as u8 - object.y;
                        sprite_color = Some(self.get_sprite_pixel(object, s_x, s_y, context));
                        break;
                    }
                }
            }
        }

        // TODO: Properly sort sprites
        let output = if let Some(sprite_color) = sprite_color {
            // TODO: I think this should only be triggered once
            if (bg_color.0).0 != 0 && (sprite_color.0).0 != 0 {
                self.status |= 1 << 6;
            }

            // TODO: Check sprite-0 hit conditions properly
            if (bg_color.0).0 != 0 && (sprite_color.0).0 != 0 {
                self.status |= 1 << 6;
            }

            match ((bg_color.0).0, (sprite_color.0).0, sprite_color.2) {
                (0, 0, _) => self.colors[self.palette_idxs[0] as usize % self.colors.len()],
                (0, _, _) => sprite_color.1,
                (_, 0, _) => bg_color.1,
                (_, _, false) => sprite_color.1,
                (_, _, true) => bg_color.1,
            }
        } else {
            if (bg_color.0).0 == 0 {
                self.colors[self.palette_idxs[0] as usize % self.colors.len()]
            } else {
                bg_color.1
            }
        };

        self.set_output_pixel(&output);
    }

    fn set_output_pixel(&mut self, color: &Color) {
        self.set_pixel_positioned(((self.scanline - 1) * 2) as usize, self.pixel * 2, color);
        self.set_pixel_positioned(((self.scanline - 1) * 2) as usize, self.pixel * 2 + 1, color);
        self.set_pixel_positioned(((self.scanline - 1) * 2) as usize + 1, self.pixel * 2, color);
        self.set_pixel_positioned(((self.scanline - 1) * 2) as usize + 1, self.pixel * 2 + 1, color);
    }

    fn set_pixel_positioned(&mut self, row: usize, col: usize, color: &Color) {
        let pixel_format = self.target.pixel_format_enum();
        let size_per_pixel = pixel_format.byte_size_per_pixel();
        let base = (col + row * self.target.width() as usize) * size_per_pixel;
        self.target.with_lock_mut(|data| {
            let pix = &mut data[base..base + size_per_pixel];
            pix[0] = color.b;
            pix[1] = color.g;
            pix[2] = color.r;
        });
    }

    pub fn incr_address(&mut self) {
        if self.ctrl.increment_mode() {
            self.address += 32;
        } else {
            self.address += 1;
        }
    }

    pub fn dma(&mut self, addr: u8, cpu: &CPU, context: &Context) {
        for idx in 0..64 {
            let index = ((idx * 4 + self.oam_addr as u16) % (64 * 4));
            let full_addr = (addr as u16) << 8 | index;

            self.oam[(index / 4) as usize] = OAMEntry {
                y: cpu.read((full_addr + 0), context),
                index: cpu.read((full_addr + 1), context),
                attrs: OAMEntryAttrs(cpu.read((full_addr + 2), context)),
                x: cpu.read((full_addr + 3), context),
            };
        }
    }
}
