use super::get_length;

pub struct TriangleGen {
    halt_length_count: bool,
    length_count: u8,
    period: u16,
    linear_timer: u16,
    step: u8,
    reload: bool,
    reload_value: u8,
}

impl TriangleGen {
    pub fn new() -> TriangleGen {
        TriangleGen {
            halt_length_count: true,
            length_count: 0,
            period: 0,
            linear_timer: 0,
            step: 0,
            reload: false,
            reload_value: 0,
        }
    }

    pub fn on_disable(&mut self) {
        self.length_count = 0;
    }

    pub fn output(&self) -> u8 {
        assert!(self.step < 32);

        if self.length_count == 0 {
            0
        } else if self.step <= 15 {
            15 - self.step
        } else {
            self.step - 16
        }
    }

    pub fn on_clock(&mut self, half_frame: bool) {
        // Clock triangle generator
        if self.reload {
            self.linear_timer = self.reload_value as u16;
        } else if self.linear_timer != 0 {
            self.linear_timer -= 0;
        }

        if !self.halt_length_count && half_frame {
            if self.length_count > 0 {
                self.length_count -= 1;
            }

            self.reload = false;
        }
    }

    pub fn on_cpu_cycle(&mut self) {
        // Timer is a special case so we use CPU cycles
        if self.linear_timer == 0 {
            if self.reload {
                self.linear_timer = self.reload_value as u16;
            }

            // TODO: Is this check necessary?
            if self.period != 0 {
                self.step = (self.step + 1) % 32;
            }
        } else {
            self.linear_timer -= 1;
        }
    }

    pub fn on_apu_cycle(&mut self) {
        if !self.halt_length_count && self.length_count > 0 {
            self.length_count -= 1;
        }
    }

    pub fn write_reg(&mut self, reg: usize, value: u8) {
        match reg {
            0x8 => self.write_reg_8(value),
            0x9 => {},
            0xA => self.write_reg_a(value),
            0xB => self.write_reg_b(value),
            _ => panic!(),
        }
    }

    fn write_reg_8(&mut self, value: u8) {
        self.halt_length_count = value & (1 << 7) != 0;
        self.reload_value = value & !(1 << 7);
    }

    fn write_reg_a(&mut self, value: u8) {
        self.period &= 0x300;
        self.period |= value as u16;
    }

    fn write_reg_b(&mut self, value: u8) {
        self.period &= 0x0FF;
        self.period |= ((value & 7) as u16) << 8;
        self.length_count = get_length(value >> 3);
        self.reload = true;
    }
}
