use crate::cpu::CPU;
use crate::Context;
use bitfield::bitfield;

const RATES: [usize; 0x10] = [
    428, 380, 340, 320, 286, 254, 226, 214, 190, 160, 142, 128, 106, 84, 72, 54,
];

bitfield! {
    #[derive(Clone, Copy)]
    pub struct DMCCtrl(u8);

    rate, set_rate: 3, 0;

    do_loop, set_loop: 6;

    irq_enable, set_irq_enable: 7;
}

pub struct DMC {
    pub ctrl: DMCCtrl,

    pub level: u8,

    pub sample_address: u16,

    pub sample_bit: u8,

    pub sample_length: u16,

    pub timer: usize,
}

impl DMC {
    pub fn new() -> DMC {
        DMC {
            ctrl: DMCCtrl(0),
            level: 0,
            sample_address: 0xC000,
            sample_length: 1,
            sample_bit: 0,
            timer: 0,
        }
    }

    pub fn write_reg(&mut self, reg: usize, value: u8) {
        match reg {
            0x10 => self.ctrl = DMCCtrl(value),
            0x11 => self.level = value,
            0x12 => self.sample_address = 0xC000 + value as u16 * 64,
            0x13 => self.sample_length = value as u16 * 16 + 1,
            _ => panic!(),
        }
    }

    pub fn output(&self) -> u8 {
        self.level
    }

    pub fn on_apu_cycle(&mut self, cpu: &CPU, context: &Context) {
        let period = RATES[self.ctrl.rate() as usize] / 2;

        if self.timer > 0 {
            self.timer -= 1;
        } else {
            self.timer = period;

            let value = cpu.read(self.sample_address, context);

            let bit = (value >> self.sample_bit) & 1 != 0;

            if bit {
                if self.level <= 125 {
                    self.level += 2;
                }
            } else if self.level >= 2 {
                self.level -= 2;
            }

            self.sample_bit += 1;

            if self.sample_bit == 8 {
                self.sample_bit = 0;

                if self.sample_address == 0xFFFF {
                    if self.ctrl.do_loop() {
                        self.sample_address = 0x8000;
                    } else {
                        // TODO: IRQ
                    }
                } else {
                    self.sample_address += 1;
                }
            }
        }
    }

    pub fn is_running(&self) -> bool {
        self.sample_address != 0xFFFF || self.ctrl.do_loop()
    }
}
