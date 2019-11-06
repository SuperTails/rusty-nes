pub struct APU {
    step_mode: bool,
    irq_inhibit: bool,
    cycle: usize,
    frame_counter: u8,

    dmc_enable: bool,
    noise_enable: bool,
    triangle_enable: bool,
    pulse_2_enable: bool,
    pulse_1_enable: bool,
}

impl APU {
    fn set_counter_state(&mut self, status: u8) {
        self.step_mode = status & 0x80 != 0;
        self.irq_inhibit = status & 0x40 != 0;
    }

    fn get_counter_state(&self) -> u8 {
        (self.step_mode as u8) << 7 | (self.irq_inhibit as u8) << 6
    }

    pub fn new() -> APU {
        APU {
            step_mode: false,
            irq_inhibit: false,
            cycle: 0,
            frame_counter: 0,
            dmc_enable: false,
            noise_enable: false,
            triangle_enable: false,
            pulse_2_enable: false,
            pulse_1_enable: false,
        }
    }

    // TODO: Delay in settings changing
    pub fn next(&mut self, cpu_cycles: usize) {
        for _ in 0..cpu_cycles {
            self.cycle += 1;

            // TODO: One cpu cycle delay
            if self.cycle % 2 == 0 {
            }
        }
    }

    pub fn read(&mut self, reg: u8) -> u8 {
        match reg {
            17 => self.get_counter_state(),
            _ => panic!("Attempt to read invalid or write-only APU register {}", reg),
        }
    }

    pub fn write(&mut self, reg: u8, value: u8) {
        match reg {
            15 => {
                self.pulse_1_enable  = value & (1 << 0) != 0;
                self.pulse_2_enable  = value & (1 << 1) != 0;
                self.triangle_enable = value & (1 << 2) != 0;
                self.noise_enable    = value & (1 << 3) != 0;
                self.dmc_enable      = value & (1 << 4) != 0;
            },
            17 => self.set_counter_state(value),
            _ => println!("Ignoring write of {:#04X} to APU register {}", value, reg),
        }
    }
}
