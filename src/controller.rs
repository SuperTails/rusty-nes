use sdl2::event::Event;
use sdl2::keyboard::Keycode;

#[derive(Default, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Controller {
    state: u8,
    shift: u8,
    strobe: bool,
}

impl Controller {
    pub fn new() -> Controller {
        Controller::default()
    }

    pub fn update_from_keys(&mut self, keys: &[Event]) {
        for event in keys {
            if let Event::KeyDown {
                keycode: Some(k), ..
            } = event
            {
                match k {
                    Keycode::A => self.state |= 1 << 0,
                    Keycode::S => self.state |= 1 << 1,
                    Keycode::RShift => self.state |= 1 << 2,
                    Keycode::Return => self.state |= 1 << 3,
                    Keycode::Up => self.state |= 1 << 4,
                    Keycode::Down => self.state |= 1 << 5,
                    Keycode::Left => self.state |= 1 << 6,
                    Keycode::Right => self.state |= 1 << 7,
                    _ => {}
                }
            }
            if let Event::KeyUp {
                keycode: Some(k), ..
            } = event
            {
                match k {
                    Keycode::A => self.state &= !(1 << 0),
                    Keycode::S => self.state &= !(1 << 1),
                    Keycode::RShift => self.state &= !(1 << 2),
                    Keycode::Return => self.state &= !(1 << 3),
                    Keycode::Up => self.state &= !(1 << 4),
                    Keycode::Down => self.state &= !(1 << 5),
                    Keycode::Left => self.state &= !(1 << 6),
                    Keycode::Right => self.state &= !(1 << 7),
                    _ => {}
                }
            }
        }
    }

    pub fn read(&mut self, reg: u8) -> u8 {
        match reg {
            0 => {
                if self.strobe {
                    self.shift = self.state;
                }

                let result = self.shift & 1;

                self.shift >>= 1;

                result
            }
            _ => panic!("Unrecognized controller register {}", reg),
        }
    }

    pub fn write(&mut self, reg: u8, value: u8) {
        match reg {
            0 => {
                self.strobe = (value & 1) != 0;
                if self.strobe {
                    self.shift = self.state;
                }
            }
            _ => panic!("Unrecognized controller register {}", reg),
        }
    }
}
