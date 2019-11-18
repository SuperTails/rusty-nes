use super::get_length;
use super::envelope::Envelope;

pub mod sweep;

use sweep::Sweep;

pub struct PulseGen {
    pub envelope: Envelope,
    pub sweep: Sweep,

    duty: u8,

    timer: u16,

    period: u16,

    length_count: u8,

    step: u8,
}

impl PulseGen {
    pub fn new(second: bool) -> PulseGen {
        PulseGen {
            envelope: Envelope::new(),
            duty: 0,
            sweep: Sweep::new(second),
            timer: 0,
            length_count: 0,
            step: 0,
            period: 0,
        }
    }

    pub fn write_reg(&mut self, reg: usize, value: u8) {
        match reg {
            0 => {
                self.envelope.write(value & 0x3F);
                self.duty = value >> 6;
            }
            1 => self.sweep.write_reg(value),
            2 => {
                self.period &= !0xFF;
                self.period |= value as u16;
            }
            3 => {
                self.period &= 0xFF;
                self.period |= ((value & 7) as u16) << 8;
                self.length_count = get_length(value >> 3);

                self.step = 0;
            }
            _ => panic!(),
        }
    }

    pub fn on_disable(&mut self) {
        self.length_count = 0;
    }

    pub fn on_clock(&mut self, is_half_frame: bool) {
        if self.length_count > 0 && is_half_frame {
            self.length_count -= 1;
        }

        self.period = self.sweep.on_clock(is_half_frame, self.period);

        self.envelope.on_clock();
    }

    pub fn on_apu_cycle(&mut self) {
        if self.timer != 0 {
            self.timer -= 1;
        } else {
            self.step += 1;
            self.step %= 8;

            self.timer = self.period;
        }
    }

    pub fn output(&self) -> u8 {
        if self.period < 8 || self.length_count == 0 {
            0
        } else {
            let sequence: [u8; 8] = match self.duty {
                0 => [0, 1, 0, 0, 0, 0, 0, 0],
                1 => [0, 1, 1, 0, 0, 0, 0, 0],
                2 => [0, 1, 1, 1, 1, 0, 0, 0],
                3 => [1, 0, 0, 1, 1, 1, 1, 1],
                _ => unreachable!(),
            };

            self.envelope.volume() * sequence[self.step as usize]
        }
    }
}
