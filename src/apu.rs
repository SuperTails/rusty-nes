use sdl2::audio::AudioCallback;
use std::sync::{Arc, Mutex};

// APU clock rate is 894886.5Hz
// About once every 56 cycles @ 16KHz

fn get_length(idx: u8) -> u8 {
    let lengths = [
        10, 254, 20, 2, 40, 4, 80, 6, 160, 8, 60, 10, 14, 12, 26, 14,
        12, 16, 24, 18, 48, 20, 96, 22, 192, 24, 72, 26, 16, 28, 32, 30
    ];
    lengths[idx as usize]
}

struct TriangleGen {
    halt_length_count: bool,
    length_count: u8,
    period: u16,
    linear_timer: u16,
    step: u8,
    reload: bool,
    reload_value: u8,
}

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

struct PulseGen {
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
            },
            3 => {
                self.period &=  0xFF;
                self.period |= ((value & 7) as u16) << 8;
                self.length_count = get_length(value >> 3);
                
                self.step = 0;
            },
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
        }
        else {
            self.step += 1;
            self.step %= 8;

            self.timer = self.period;
        }
    }

    pub fn output(&self) -> u8 {
        if self.period < 8 || self.length_count == 0 {
            0
        }
        else {
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
    
    pub fn output(&self) -> u8 {
        assert!(self.step < 32);

        if self.length_count == 0 {
            0
        }
        else if self.step <= 15 {
            15 - self.step
        }
        else {
            self.step - 16
        }
    }

    pub fn on_clock(&mut self, half_frame: bool) {
        // Clock triangle generator 
        if self.reload {
            self.linear_timer = self.reload_value as u16;
        }
        else if self.linear_timer != 0 {
            self.linear_timer -= 0;
        }

        if !self.halt_length_count {
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
        }
        else {
            self.linear_timer -= 1;
        }
    }

    pub fn on_apu_cycle(&mut self) {
        if !self.halt_length_count && self.length_count > 0 {
            self.length_count -= 1;
        }
    }

    pub fn write_reg_8(&mut self, value: u8) {
        self.halt_length_count = value & (1 << 7) != 0;
        self.reload_value = value & !(1 << 7);
    }

    pub fn write_reg_A(&mut self, value: u8) {
        self.period &= 0x300;
        self.period |= value as u16;
    }

    pub fn write_reg_B(&mut self, value: u8) {
        self.period &= 0x0FF;
        self.period |= ((value & 7) as u16) << 8;
        self.length_count = get_length(value >> 3);
        self.reload = true;
    }
}

pub struct APU {
    step_mode: bool,
    irq_inhibit: bool,
    cycle: usize,

    dmc_enable: bool,
    noise_enable: bool,

    pulse_2_enable: bool,
    pulse_2: PulseGen,

    pulse_1_enable: bool,
    pulse_1: PulseGen,

    triangle_enable: bool,
    triangle_gen: TriangleGen,

    audio_data: Arc<Mutex<[f32; 1024]>>,
    audio_data_idx: usize,
}

pub struct APUAudio {
    audio_data: Arc<Mutex<[f32; 1024]>>,
}

impl AudioCallback for APUAudio {
    type Channel = f32;

    fn callback(&mut self, out: &mut [f32]) {
        out.copy_from_slice(&*self.audio_data.lock().unwrap());
    }
}

impl APU {
    fn set_counter_state(&mut self, status: u8) {
        self.step_mode = status & 0x80 != 0;
        self.irq_inhibit = status & 0x40 != 0;
    }


    fn get_counter_state(&self) -> u8 {
        (self.step_mode as u8) << 7 | (self.irq_inhibit as u8) << 6
    }

    fn get_status(&self) -> u8 {
        // TODO: DMC or frame interrupt
        let mut result = 0;
        
        result <<= (self.dmc_enable as u8) << 4;
        result <<= (self.noise_enable as u8) << 3;
        result <<= (self.triangle_enable as u8) << 2;
        result <<= (self.pulse_2_enable as u8) << 1;
        result <<= (self.pulse_1_enable as u8) << 0;

        result
    }

    pub fn new() -> (APU, APUAudio) {
        let audio_data = Arc::new(Mutex::new([0.0; 1024]));

        (APU {
            step_mode: false,
            irq_inhibit: false,
            cycle: 0,
            dmc_enable: false,
            noise_enable: false,
            pulse_2_enable: false,
            pulse_2: PulseGen::new(),
            pulse_1_enable: false,
            pulse_1: PulseGen::new(),
            triangle_enable: false,
            triangle_gen: TriangleGen::new(),
            audio_data: Arc::clone(&audio_data),
            audio_data_idx: 0,
        },
        APUAudio {
            audio_data,
        })
    }

    // TODO: Mixing
    pub fn output(&self) -> u8 {
        let mut total = 0;

        if self.triangle_enable {
            total += self.triangle_gen.output();
        }

        if self.pulse_1_enable {
            total += self.pulse_1.output();
        }

        if self.pulse_2_enable {
            total += self.pulse_2.output();
        }
        
        total
    }

    // TODO: Delay in settings changing
    pub fn next(&mut self, cpu_cycles: usize) {
        for _ in 0..cpu_cycles {
            self.cycle += 1;
            if self.step_mode {
                self.cycle %= 18641 * 2;
            }
            else {
                self.cycle %= 14915 * 2;
            }

            if self.triangle_enable {
                self.triangle_gen.on_cpu_cycle();
            }

            if self.step_mode {
                // 5-Step sequence
                if self.cycle == 3728 * 2 + 1 || self.cycle == 7456 * 2 + 1 || self.cycle == 11185 * 2 + 1 || self.cycle == 18640 * 2 + 1 {
                    let is_half_frame = self.cycle == 7456 * 2 + 1 || self.cycle == 18640 * 2 + 1;
                    self.triangle_gen.on_clock(is_half_frame);
                    if is_half_frame {
                        self.pulse_1.on_clock();
                        self.pulse_2.on_clock();
                    }
                }
            }
            else {
                // 4-Step sequence
                if self.cycle == 3728 * 2 + 1 || self.cycle == 7456 * 2 + 1 || self.cycle == 11185 * 2 + 1 || self.cycle == 14914 * 2 + 1 {
                    let is_half_frame = self.cycle == 7456 * 2 + 1 || self.cycle == 14914 * 2 + 1;
                    self.triangle_gen.on_clock(is_half_frame);
                    if is_half_frame {
                        self.pulse_1.on_clock();
                        self.pulse_2.on_clock();
                    }
                }
            }

            // TODO: One cpu cycle delay
            if self.cycle % 2 == 0 {
                if self.triangle_enable {
                    self.triangle_gen.on_apu_cycle();
                }

                if self.pulse_1_enable {
                    self.pulse_1.on_apu_cycle();
                }

                if self.pulse_2_enable {
                    self.pulse_2.on_apu_cycle();
                }
            }

            if self.cycle % 56 == 0 {
                {
                    self.audio_data.lock().unwrap()[self.audio_data_idx] = self.output() as f32 / 64.0;
                }

                self.audio_data_idx += 1;
                if self.audio_data_idx == 1024 {
                    self.audio_data_idx = 0;
                }
            }
        }
    }

    pub fn read(&mut self, reg: u8) -> u8 {
        match reg {
            0x15 => self.get_status(),
            0x17 => self.get_counter_state(),
            _ => panic!("Attempt to read invalid or write-only APU register {:#X}", reg),
        }
    }

    pub fn write(&mut self, reg: u8, value: u8) {
        match reg {
            0x8 => {
                self.triangle_gen.write_reg_8(value);
              },
            0xA => {
                self.triangle_gen.write_reg_A(value);
            },
            0xB => {
                self.triangle_gen.write_reg_B(value);
            },
            0x0..=0x3 => {
                self.pulse_1.write_reg(reg as usize, value);
            },
            0x4..=0x7 => {
                self.pulse_2.write_reg(reg as usize - 0x4, value);
            }
            0x15 => {
                self.pulse_1_enable  = value & (1 << 0) != 0;
                self.pulse_2_enable  = value & (1 << 1) != 0;
                self.triangle_enable = value & (1 << 2) != 0;
                self.noise_enable    = value & (1 << 3) != 0;
                self.dmc_enable      = value & (1 << 4) != 0;
            },
            0x17 => {
                // TODO: Side effects
                self.set_counter_state(value)
            },
            _ => println!("Ignoring write of {:#04X} to APU register {:#X}", value, reg),
        }
    }
}
