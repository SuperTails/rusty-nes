use sdl2::audio::AudioCallback;
use std::sync::{Arc, Mutex};
use crate::cpu::CPU;
use crate::Context;

mod noise_gen;
mod pulse_gen;
mod triangle_gen;
mod dmc;

use noise_gen::NoiseGen;
use pulse_gen::PulseGen;
use triangle_gen::TriangleGen;
use dmc::DMC;

// APU clock rate is 894886.5Hz
// About once every 56 cycles @ 16KHz

fn get_length(idx: u8) -> u8 {
    let lengths = [
        10, 254, 20, 2, 40, 4, 80, 6, 160, 8, 60, 10, 14, 12, 26, 14, 12, 16, 24, 18, 48, 20, 96,
        22, 192, 24, 72, 26, 16, 28, 32, 30,
    ];
    lengths[idx as usize]
}
pub struct APU {
    step_mode: bool,
    irq_inhibit: bool,
    cycle: usize,

    dmc_enable: bool,
    dmc: DMC,

    noise_enable: bool,
    noise: NoiseGen,

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

        result <<= ((self.dmc_enable && self.dmc.is_running()) as u8) << 4;
        result <<= (self.noise_enable as u8) << 3;
        result <<= (self.triangle_enable as u8) << 2;
        result <<= (self.pulse_2_enable as u8) << 1;
        result <<= (self.pulse_1_enable as u8) << 0;

        result
    }

    pub fn new() -> (APU, APUAudio) {
        let audio_data = Arc::new(Mutex::new([0.0; 1024]));

        (
            APU {
                step_mode: false,
                irq_inhibit: false,
                cycle: 0,
                dmc_enable: false,
                dmc: DMC::new(),
                noise_enable: false,
                noise: NoiseGen::new(),
                pulse_2_enable: false,
                pulse_2: PulseGen::new(),
                pulse_1_enable: false,
                pulse_1: PulseGen::new(),
                triangle_enable: false,
                triangle_gen: TriangleGen::new(),
                audio_data: Arc::clone(&audio_data),
                audio_data_idx: 0,
            },
            APUAudio { audio_data },
        )
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

        if self.noise_enable {
            total += self.noise.output();
        }

        /* TODO: Reenable
        if self.dmc_enable {
            total += self.dmc.output();
        }*/

        total
    }

    // TODO: Delay in settings changing
    pub fn next(&mut self, cpu_cycles: usize, context: &Context, cpu: &CPU) {
        for _ in 0..cpu_cycles {
            self.cycle += 1;
            if self.step_mode {
                self.cycle %= 18641 * 2;
            } else {
                self.cycle %= 14915 * 2;
            }

            if self.triangle_enable {
                self.triangle_gen.on_cpu_cycle();
            }

            if self.step_mode {
                // 5-Step sequence
                if self.cycle == 3728 * 2 + 1
                    || self.cycle == 7456 * 2 + 1
                    || self.cycle == 11185 * 2 + 1
                    || self.cycle == 18640 * 2 + 1
                {
                    let is_half_frame = self.cycle == 7456 * 2 + 1 || self.cycle == 18640 * 2 + 1;
                    self.triangle_gen.on_clock(is_half_frame);
                    if is_half_frame {
                        self.pulse_1.on_clock();
                        self.pulse_2.on_clock();
                        self.noise.on_clock();
                    }
                }
            } else {
                // 4-Step sequence
                if self.cycle == 3728 * 2 + 1
                    || self.cycle == 7456 * 2 + 1
                    || self.cycle == 11185 * 2 + 1
                    || self.cycle == 14914 * 2 + 1
                {
                    let is_half_frame = self.cycle == 7456 * 2 + 1 || self.cycle == 14914 * 2 + 1;
                    self.triangle_gen.on_clock(is_half_frame);
                    if is_half_frame {
                        self.pulse_1.on_clock();
                        self.pulse_2.on_clock();
                        self.noise.on_clock();
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

                if self.noise_enable {
                    self.noise.on_apu_cycle();
                }

                if self.dmc_enable {
                    self.dmc.on_apu_cycle(cpu, context);
                }
            }

            if self.cycle % 56 == 0 {
                {
                    self.audio_data.lock().unwrap()[self.audio_data_idx] =
                        self.output() as f32 / 64.0;
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
            _ => panic!(
                "Attempt to read invalid or write-only APU register {:#X}",
                reg
            ),
        }
    }

    pub fn write(&mut self, reg: u8, value: u8) {
        match reg {
            0x8..=0xB => {
                self.triangle_gen.write_reg(reg as usize, value);
            }
            0xC..=0xF => {
                self.noise.write_reg(reg as usize, value);
            }
            0x10..=0x13 => {
                self.dmc.write_reg(reg as usize, value);
            }
            0x0..=0x3 => {
                self.pulse_1.write_reg(reg as usize, value);
            }
            0x4..=0x7 => {
                self.pulse_2.write_reg(reg as usize - 0x4, value);
            }
            0x15 => {
                self.pulse_1_enable = value & (1 << 0) != 0;
                self.pulse_2_enable = value & (1 << 1) != 0;
                self.triangle_enable = value & (1 << 2) != 0;
                self.noise_enable = value & (1 << 3) != 0;
                self.dmc_enable = value & (1 << 4) != 0;
            }
            0x17 => {
                // TODO: Side effects
                self.set_counter_state(value)
            }
            _ => println!(
                "Ignoring write of {:#04X} to APU register {:#X}",
                value, reg
            ),
        }
    }
}
