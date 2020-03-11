bitfield! {
    #[derive(Clone, Copy)]
    pub struct SweepCtrl(u8);
    impl Debug;

    shift, set_shift: 2, 0;

    negate, set_negate: 3;

    period, set_period: 6, 4;

    enable_sweep, set_sweep_enable: 7;
}

pub struct Sweep {
    ctrl: SweepCtrl,
    timer: u8,
    second: bool,
    reload: bool,
}

impl Sweep {
    pub fn new(second: bool) -> Sweep {
        Sweep {
            ctrl: SweepCtrl(0),
            timer: 0,
            reload: false,
            second,
        }
    }

    pub fn write_reg(&mut self, value: u8) {
        self.ctrl = SweepCtrl(value);
        self.reload = true;
    }

    pub fn on_clock(&mut self, is_half_frame: bool, timer_period: u16) -> u16 {
        if is_half_frame {
            if self.timer == 0 && self.ctrl.enable_sweep() && self.is_muting(timer_period) {
                self.timer = self.ctrl.period();
                self.next_period(timer_period)
            } else if self.timer == 0 || self.reload {
                self.reload = false;
                self.timer = self.ctrl.period();
                timer_period
            } else {
                self.timer -= 1;
                timer_period
            }
        } else {
            timer_period
        }
    }

    pub fn is_muting(&self, timer_period: u16) -> bool {
        timer_period < 8 || self.next_period(timer_period) == 0x7FF
    }

    pub fn next_period(&self, timer_period: u16) -> u16 {
        let mut shifted = (timer_period >> self.ctrl.shift()) as i16;

        if self.ctrl.negate() {
            shifted = if self.second {
                // Pulse 2  uses two's complement
                -shifted
            } else {
                // Pulse 1 uses one's complement
                -shifted - 1
            }
        }

        let new_period = ((timer_period as i16) + shifted) as u16;

        if new_period > 0x7FF {
            0x7FF
        } else if timer_period < 8 {
            timer_period
        } else {
            new_period
        }
    }
}
