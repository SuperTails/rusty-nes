pub struct Envelope {
    start: bool,
    timer: u8,
    decay_level: u8,
    period: u8,
    do_loop: bool,
    constant_volume: bool,
}

impl Envelope {
    pub fn volume(&self) -> u8 {
        self.decay_level
    }

    pub fn do_loop(&self) -> bool {
        self.do_loop
    }

    pub fn new() -> Envelope {
        Envelope {
            start: true,
            timer: 0,
            decay_level: 15,
            period: 0,
            do_loop: false,
            constant_volume: true,
        }
    }

    pub fn start(&mut self) {
        self.start = true;
    }

    pub fn write(&mut self, value: u8) {
        assert_eq!(value & 0xC0, 0);
        self.constant_volume = (value & 0x10) != 0;
        self.do_loop = (value & 0x20) != 0;

        if self.constant_volume {
            self.decay_level = value & 0xF;
        } else {
            self.period = value & 0xF;
        }
    }

    // Quarter frame
    pub fn on_clock(&mut self) {
        if self.start {
            self.decay_level = 15;
            self.timer = self.period;
            self.start = false;
        } else if self.timer == 0 {
            self.timer = self.period;
            if self.decay_level > 0 {
                self.decay_level -= 1;
            } else if self.do_loop {
                self.decay_level = 15;
            }
        } else {
            self.timer -= 1;
        }
    }
}
