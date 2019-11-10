bitfield! {
    #[derive(Clone, Copy)]
    pub struct NoiseGenCtrl(u8);
    impl Debug;

    volume, set_volume: 3, 0;

    constant_volume, set_constant_volume: 4;

    halt_length_count, set_halt: 5;
}

const PERIODS: [u16; 0x10] = [
    4, 8, 16, 32, 64, 96, 128, 160, 202, 254, 380, 508, 762, 1016, 2034, 4068,
];

pub struct NoiseGen {
    ctrl: NoiseGenCtrl,
    length_count: u8,
    mode: bool,
    period: u8,
    timer: u16,

    shift_register: u16,
}

impl NoiseGen {
    pub fn new() -> NoiseGen {
        NoiseGen {
            ctrl: NoiseGenCtrl(0),
            length_count: 0,
            mode: false,
            period: 0,
            timer: 0,
            shift_register: 1,
        }
    }

    fn next_shift(mut reg: u16, mode: bool) -> u16 {
        let feedback = {
            let feedback_rhs = (if mode { reg >> 6 } else { reg >> 1 }) & 1;
            (reg & 1) ^ feedback_rhs
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

    pub fn on_clock(&mut self) {
        if !self.ctrl.halt_length_count() && self.length_count > 0 {
            self.length_count -= 1;
        }
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
            0xC => self.ctrl = NoiseGenCtrl(value),
            0xD => {}
            0xE => {
                self.mode = value & 0x80 != 0;
                self.period = value & 0b1111;
            }
            0xF => self.length_count = value >> 3,
            _ => panic!(),
        }
    }
}
