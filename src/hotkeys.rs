use anyhow::Result;
use maplit::hashmap;
use sdl2::keyboard::{Keycode, Mod};
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
struct SavestateBindings {
    load_state: Key,
    save_state: Key,
}

#[derive(Deserialize, Default)]
/// The struct that `hotkeys.toml` gets deserialized into.
struct Keybinds {
    joypad: JoypadBindings,
    emu: EmuBindings,
    savestate: SavestateBindings,
}

impl Default for JoypadBindings {
    fn default() -> Self {
        JoypadBindings {
            up: Key(Keycode::Up),
            down: Key(Keycode::Down),
            left: Key(Keycode::Left),
            right: Key(Keycode::Right),
            a: Key(Keycode::X),
            b: Key(Keycode::Z),
            start: Key(Keycode::Return),
            select: Key(Keycode::Tab),
        }
    }
}

impl Default for EmuBindings {
    fn default() -> Self {
        EmuBindings {
            toggle_frame_limiter: Key(Keycode::Space),
        }
    }
}

impl Default for SavestateBindings {
    fn default() -> Self {
        SavestateBindings {
            load_state: Key(Keycode::F1),
            save_state: Key(Keycode::F2),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum JoypadKey {
    Up,
    Down,
    Left,
    Right,
    A,
    B,
    Start,
    Select,
}

#[derive(Copy, Clone, Debug)]
pub enum Hotkey {
    Joypad(JoypadKey),
    ToggleFrameLimiter,
    LoadState,
    SaveState,
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
            keys.joypad.up => Hotkey::Joypad(JoypadKey::Up),
            keys.joypad.down => Hotkey::Joypad(JoypadKey::Down),
            keys.joypad.left => Hotkey::Joypad(JoypadKey::Left),
            keys.joypad.right => Hotkey::Joypad(JoypadKey::Right),
            keys.joypad.a => Hotkey::Joypad(JoypadKey::A),
            keys.joypad.b => Hotkey::Joypad(JoypadKey::B),
            keys.joypad.start => Hotkey::Joypad(JoypadKey::Start),
            keys.joypad.select => Hotkey::Joypad(JoypadKey::Select),
            keys.emu.toggle_frame_limiter => Hotkey::ToggleFrameLimiter,
            keys.savestate.load_state => Hotkey::LoadState,
            keys.savestate.save_state => Hotkey::SaveState,
        };
        Ok(Keymap { map })
    }

    pub fn get_hotkey(&self, key: Keycode, mods: Mod) -> Option<Hotkey> {
        self.map.get(&Key(key)).copied()
    }
}
