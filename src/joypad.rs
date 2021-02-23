#[derive(Default)]
pub struct Joypad {
    a: bool,
    b: bool,
    up: bool,
    down: bool,
    left: bool,
    right: bool,
    start: bool,
    select: bool,

    direction: bool,
    buttons: bool,

    request_interrupt: bool,
}

impl Joypad {
    pub fn poll(&mut self) -> bool {
        if self.request_interrupt && (self.buttons || self.direction) {
            self.request_interrupt = false;
            true
        } else {
            false
        }
    }

    pub fn is_valid_key(&self, key: &String) -> bool {
        match &key[..] {
            "P" | "L" | ";" | "\'" | "X" | "Z" | "Return" | "Backspace" => true,
            _ => false,
        }
    }

    pub fn update_key(&mut self, key: &String, pressed: bool) {
        assert!(self.is_valid_key(key));
        match &key[..] {
            "P" => self.up = pressed,
            "L" => self.left = pressed,
            ";" => self.down = pressed,
            "'" => self.right = pressed,
            "X" => self.a = pressed,
            "Z" => self.b = pressed,
            "Return" => self.start = pressed,
            "Backspace" => self.select = pressed,
            _ => {}
        }

        self.request_interrupt = pressed;
    }

    pub fn read(&self) -> u8 {
        let mut nibble = 0;
        if self.direction {
            nibble |= ((self.down as u8) << 3)
                | ((self.up as u8) << 2)
                | ((self.left as u8) << 1)
                | (self.right as u8);
        }
        if self.buttons {
            nibble |= ((self.start as u8) << 3)
                | ((self.select as u8) << 2)
                | ((self.b as u8) << 1)
                | (self.a as u8);
        }
        nibble = !nibble & 0xF;
        0xC0 | ((!self.buttons as u8) << 5) | ((!self.direction as u8) << 4) | nibble
    }

    pub fn write(&mut self, val: u8) {
        self.buttons = val & (1 << 5) == 0;
        self.direction = val & (1 << 4) == 0;
    }
}
