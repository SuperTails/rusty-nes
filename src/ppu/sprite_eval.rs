use super::OAMEntry;
use arrayvec::ArrayVec;

pub struct Evaluator {
    sprites: ArrayVec<[(usize, OAMEntry); 8]>,

    pub next_sprites: ArrayVec<[(usize, OAMEntry); 8]>,

    current_index: usize,
}

impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {
            sprites: ArrayVec::new(),
            next_sprites: ArrayVec::new(),
            current_index: 0,
        }
    }

    pub fn next(&mut self, pixel: usize, scanline: usize, oam: &[OAMEntry], max_height: usize) {
        if scanline > 239 {
            self.next_sprites = ArrayVec::new();
            self.sprites = ArrayVec::new();
            return;
        }

        match pixel {
            1..=64 => {
                // PPU does some writes here
                self.sprites = ArrayVec::new();
            }
            65..=256 => {
                if pixel % 2 == 1 && scanline >= 1 && self.current_index < 64 {
                    if self.sprites.len() < 8 {
                        let entry = &oam[self.current_index];
                        if entry.y as usize <= scanline && scanline < entry.y as usize + max_height
                        {
                            self.sprites.push((self.current_index, *entry));
                        }
                    } else {
                        // TODO: Sprite overflow
                    }

                    self.current_index += 1;
                }
            }
            257..=320 | 321..=340 => {
                self.next_sprites = self.sprites.clone();
                self.current_index = 0;
            }
            0..=0 => {
                // Idle
            }
            _ => panic!(),
        }
    }
}
