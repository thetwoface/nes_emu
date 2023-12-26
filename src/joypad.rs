bitflags! {
    // https://wiki.nesdev.com/w/index.php/Controller_reading_code
    #[derive(Clone, Copy)]
    pub struct JoypadButton: u8 {
        const RIGHT             = 0b1000_0000;
        const LEFT              = 0b0100_0000;
        const DOWN              = 0b0010_0000;
        const UP                = 0b0001_0000;
        const START             = 0b0000_1000;
        const SELECT            = 0b0000_0100;
        const BUTTON_B          = 0b0000_0010;
        const BUTTON_A          = 0b0000_0001;
    }
}

pub struct Joypad {
    /// strobe mode - on/off
    strobe: bool,
    /// an index of a button to be reported on the next read
    button_index: u8,
    /// the status of all buttons
    button_status: JoypadButton,
}

impl Joypad {
    pub fn new() -> Self {
        Self {
            strobe: false,
            button_index: 0,
            button_status: JoypadButton::from_bits_truncate(0),
        }
    }

    pub fn write(&mut self, data: u8) {
        self.strobe = data & 1 == 1;
        if self.strobe {
            self.button_index = 0;
        }
    }

    pub fn read(&mut self) -> u8 {
        if self.button_index > 7 {
            return 1;
        }

        let response = (self.button_status.bits() & (1 << self.button_index)) >> self.button_index;
        if !self.strobe && self.button_index <= 7 {
            self.button_index += 1;
        }

        response
    }

    pub fn set_button_pressed_status(&mut self, button: JoypadButton, pressed: bool) {
        self.button_status.set(button, pressed);
    }
}

impl Default for Joypad {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_strobe_mode() {
        let mut joypad = Joypad::new();
        joypad.write(1);
        joypad.set_button_pressed_status(JoypadButton::BUTTON_A, true);
        for _x in 0..10 {
            assert_eq!(joypad.read(), 1);
        }
    }

    #[test]
    fn test_strobe_mode_on_off() {
        let mut joypad = Joypad::new();

        joypad.write(0);
        joypad.set_button_pressed_status(JoypadButton::RIGHT, true);
        joypad.set_button_pressed_status(JoypadButton::LEFT, true);
        joypad.set_button_pressed_status(JoypadButton::SELECT, true);
        joypad.set_button_pressed_status(JoypadButton::BUTTON_B, true);

        for _ in 0..=1 {
            assert_eq!(joypad.read(), 0);
            assert_eq!(joypad.read(), 1);
            assert_eq!(joypad.read(), 1);
            assert_eq!(joypad.read(), 0);
            assert_eq!(joypad.read(), 0);
            assert_eq!(joypad.read(), 0);
            assert_eq!(joypad.read(), 1);
            assert_eq!(joypad.read(), 1);

            for _x in 0..10 {
                assert_eq!(joypad.read(), 1);
            }
            joypad.write(1);
            joypad.write(0);
        }
    }
}
