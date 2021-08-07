use anyhow::Result;
use maplit::hashmap;
use sdl2::keyboard::Keycode;
use serde::Deserialize;
use std::collections::HashMap;
use std::fs::File;
use std::io::Read;
use std::path::Path;

use crate::utils::*;

/// A wrapper type for SDL2's Keycode enum that lets us deserialize it.
#[derive(Debug, Eq, PartialEq, Hash, Deserialize)]
struct Key(#[serde(with = "keycode_serde")] Keycode);

#[derive(Deserialize)]
struct JoypadBindings {
    up: Key,
    down: Key,
    left: Key,
    right: Key,
    a: Key,
    b: Key,
    start: Key,
    select: Key,
}

#[derive(Deserialize)]
struct EmuBindings {
    toggle_frame_limiter: Key,
}

#[derive(Deserialize)]
/// The struct that `hotkeys.toml` gets deserialized into.
struct Keybinds {
    joypad: JoypadBindings,
    emu: EmuBindings,
}

/// By default the D-pad is mapped to the Arrow Keys, A to X, B to Z,
/// Start to Enter/Return, and Select to Tab.
impl Default for Keybinds {
    fn default() -> Self {
        Keybinds {
            joypad: JoypadBindings {
                up: Key(Keycode::Up),
                down: Key(Keycode::Down),
                left: Key(Keycode::Left),
                right: Key(Keycode::Right),
                a: Key(Keycode::X),
                b: Key(Keycode::Z),
                start: Key(Keycode::Return),
                select: Key(Keycode::Tab),
            },
            emu: EmuBindings {
                toggle_frame_limiter: Key(Keycode::Space),
            },
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Hotkey {
    Up,
    Down,
    Left,
    Right,
    A,
    B,
    Start,
    Select,
    ToggleFrameLimiter,
}

/// A Hashmap between SDL Keycodes and relevant Hotkeys
pub struct Keymap {
    map: HashMap<Key, Hotkey>,
}

impl Keymap {
    /// Initializes from the provided TOML file; initializes with
    /// default values if the file doesn't exist or otherwise fails to open.
    pub fn new(path: &Path) -> Result<Self> {
        let keys = if let Ok(mut file) = File::open(&path) {
            let mut toml = String::new();
            file.read_to_string(&mut toml)?;
            toml::from_str(&toml)?
        } else {
            Keybinds::default()
        };
        let map = hashmap! {
            keys.joypad.up => Hotkey::Up,
            keys.joypad.down => Hotkey::Down,
            keys.joypad.left => Hotkey::Left,
            keys.joypad.right => Hotkey::Right,
            keys.joypad.a => Hotkey::A,
            keys.joypad.b => Hotkey::B,
            keys.joypad.start => Hotkey::Start,
            keys.joypad.select => Hotkey::Select,
            keys.emu.toggle_frame_limiter => Hotkey::ToggleFrameLimiter,
        };
        Ok(Keymap { map })
    }

    pub fn get_hotkey(&self, key: Keycode) -> Option<Hotkey> {
        self.map.get(&Key(key)).copied()
    }
}
