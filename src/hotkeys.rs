use anyhow::Result;
use maplit::hashmap;
use sdl2::keyboard::Keycode;
use serde::de::{self, Deserializer, Visitor};
use serde::Deserialize;
use std::collections::HashMap;
use std::fmt;
use std::fs::File;
use std::io::Read;
use std::path::Path;

/// A wrapper type for SDL2's Keycode enum that lets us implement Deserialize for it.
#[derive(Debug, Eq, PartialEq, Hash)]
struct Key(Keycode);

/// Deserialize a string into an Keycode by parsing the string as a Keycode name.
impl<'de> Deserialize<'de> for Key {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct KeyVisitor;
        impl<'de> Visitor<'de> for KeyVisitor {
            type Value = Key;
            fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(formatter, "a string representing a keycode")
            }

            fn visit_str<E>(self, s: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(Key(Keycode::from_name(s)
                    .ok_or(format!("invalid hotkey value \"{}\"", s))
                    .map_err(de::Error::custom)?))
            }
        }
        deserializer.deserialize_string(KeyVisitor)
    }
}

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
