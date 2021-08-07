use crate::hotkeys::Hotkey;
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

    pub fn update_key(&mut self, key: Hotkey, pressed: bool) {
        match key {
            Hotkey::Up => self.up = pressed,
            Hotkey::Down => self.down = pressed,
            Hotkey::Left => self.left = pressed,
            Hotkey::Right => self.right = pressed,
            Hotkey::A => self.a = pressed,
            Hotkey::B => self.b = pressed,
            Hotkey::Start => self.start = pressed,
            Hotkey::Select => self.select = pressed,
            _ => {}
        }

        match key {
            Hotkey::Up | Hotkey::Down | Hotkey::Left | Hotkey::Right => {
                self.request_direction_interrupt = pressed
            }
            Hotkey::A | Hotkey::B | Hotkey::Start | Hotkey::Select => {
                self.request_buttons_interrupt = pressed
            }
            _ => {}
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
