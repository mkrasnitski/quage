use crate::hotkeys::JoypadKey;
use crate::utils::*;
use serde::{Deserialize, Serialize};

#[derive(Default, Serialize, Deserialize)]
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

    request_buttons_interrupt: bool,
    request_direction_interrupt: bool,
}

impl Joypad {
    pub fn new() -> Self {
        Joypad {
            direction: true,
            buttons: true,
            ..Default::default()
        }
    }

    pub fn poll(&mut self) -> bool {
        if self.request_buttons_interrupt && self.buttons {
            self.request_buttons_interrupt = false;
            true
        } else if self.request_direction_interrupt && self.direction {
            self.request_direction_interrupt = false;
            true
        } else {
            false
        }
    }

    pub fn update_key(&mut self, key: JoypadKey, pressed: bool) {
        match key {
            JoypadKey::Up => self.up = pressed,
            JoypadKey::Down => self.down = pressed,
            JoypadKey::Left => self.left = pressed,
            JoypadKey::Right => self.right = pressed,
            JoypadKey::A => self.a = pressed,
            JoypadKey::B => self.b = pressed,
            JoypadKey::Start => self.start = pressed,
            JoypadKey::Select => self.select = pressed,
        }

        match key {
            JoypadKey::Up | JoypadKey::Down | JoypadKey::Left | JoypadKey::Right => {
                self.request_direction_interrupt = pressed
            }
            JoypadKey::A | JoypadKey::B | JoypadKey::Start | JoypadKey::Select => {
                self.request_buttons_interrupt = pressed
            }
        }
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
        self.buttons = !val.bit(5);
        self.direction = !val.bit(4);
    }
}
