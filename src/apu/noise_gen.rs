use super::envelope::Envelope;

const PERIODS: [u16; 0x10] = [
    4, 8, 16, 32, 64, 96, 128, 160, 202, 254, 380, 508, 762, 1016, 2034, 4068,
];

pub struct NoiseGen {
    ctrl: Envelope,
    length_count: u8,
    mode: bool,
    period: u8,
    timer: u16,

    shift_register: u16,
}

impl NoiseGen {
    pub fn new() -> NoiseGen {
        NoiseGen {
            ctrl: Envelope::new(),
            length_count: 0,
            mode: false,
            period: 0,
            timer: 0,
            shift_register: 1,
        }
    }

    fn next_shift(mut reg: u16, mode: bool) -> u16 {
        let feedback = {
            let feedback_rhs = if mode { reg >> 6 } else { reg >> 1 };
            (reg & 1) ^ (feedback_rhs & 1)
        };

        reg >>= 1;

        reg |= feedback << 14;

        reg
    }

    pub fn output(&self) -> u8 {
        if self.length_count != 0 {
            let raw = (!self.shift_register & 1) as u8;
            raw * self.ctrl.volume()
        } else {
            0
        }
    }

    pub fn on_disable(&mut self) {
        self.length_count = 0;
    }

    pub fn on_clock(&mut self, is_half_frame: bool) {
        if !self.ctrl.do_loop() && self.length_count > 0 && is_half_frame {
            self.length_count -= 1;
        }

        self.ctrl.on_clock();
    }

    pub fn on_apu_cycle(&mut self) {
        if self.timer != 0 {
            self.timer -= 1;
        } else {
            self.timer = PERIODS[self.period as usize];

            self.shift_register = NoiseGen::next_shift(self.shift_register, self.mode);
        }
    }

    pub fn write_reg(&mut self, reg: usize, value: u8) {
        match reg {
            0xC => self.ctrl.write(value & 0x3F),
            0xD => {}
            0xE => {
                self.mode = value & 0x80 != 0;
                self.period = value & 0b1111;
            }
            0xF => {
                self.length_count = value >> 3;
                self.ctrl.start();
            }
            _ => panic!(),
        }
    }
}
