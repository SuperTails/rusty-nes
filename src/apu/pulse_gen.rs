use super::get_length;

bitfield! {
    #[derive(Clone, Copy)]
    pub struct PulseGenCtrl1(u8);
    impl Debug;

    volume, set_volume: 3, 0;

    constant_volume, set_constant_volume: 4;

    halt_length_count, set_halt: 5;

    duty, set_duty: 7, 6;
}

bitfield! {
    #[derive(Clone, Copy)]
    pub struct PulseGenCtrl2(u8);
    impl Debug;

    shift, set_shift: 2, 0;

    negate, set_negate: 3;

    period, set_period: 6, 4;

    enable_sweep, set_sweep_enable: 7;
}

pub struct PulseGen {
    pub ctrl1: PulseGenCtrl1,
    pub ctrl2: PulseGenCtrl2,

    timer: u16,

    period: u16,

    length_count: u8,

    step: u8,
}

impl PulseGen {
    pub fn new() -> PulseGen {
        PulseGen {
            ctrl1: PulseGenCtrl1(0),
            ctrl2: PulseGenCtrl2(0),
            timer: 0,
            length_count: 0,
            step: 0,
            period: 0,
        }
    }

    pub fn write_reg(&mut self, reg: usize, value: u8) {
        match reg {
            0 => self.ctrl1 = PulseGenCtrl1(value),
            1 => self.ctrl2 = PulseGenCtrl2(value),
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

    pub fn on_clock(&mut self) {
        if self.length_count > 0 {
            self.length_count -= 1;
        }
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
            let sequence: [u8; 8] = match self.ctrl1.duty() {
                0 => [0, 1, 0, 0, 0, 0, 0, 0],
                1 => [0, 1, 1, 0, 0, 0, 0, 0],
                2 => [0, 1, 1, 1, 1, 0, 0, 0],
                3 => [1, 0, 0, 1, 1, 1, 1, 1],
                _ => unreachable!(),
            };

            self.ctrl1.volume() * sequence[self.step as usize]
        }
    }
}
