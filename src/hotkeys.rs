use anyhow::Result;
use sdl2::keyboard::{Keycode, Mod};
use serde::{de, Deserialize, Deserializer};
use std::collections::{BTreeSet, HashMap};
use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::str::FromStr;

macro_rules! keycombo {
    ($keycode:ident) => {
        KeyCombo {
            key: Keycode::$keycode,
            mods: BTreeSet::new()
        }
    };
    ($($mod:ident)-+;$keycode:ident) => {{
        KeyCombo {
            key: Keycode::$keycode,
            mods: BTreeSet::from([$(Modifier::$mod),*]),
        }
    }};
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Modifier {
    Alt,
    Ctrl,
    Shift,
    Super,
}

impl FromStr for Modifier {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Alt" => Ok(Modifier::Alt),
            "Ctrl" => Ok(Modifier::Ctrl),
            "Shift" => Ok(Modifier::Shift),
            "Super" => Ok(Modifier::Super),
            _ => Err(format!("invalid modifier: \"{}\"", s)),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct KeyCombo {
    key: Keycode,
    mods: BTreeSet<Modifier>,
}

impl KeyCombo {
    fn from_sdl(key: Keycode, sdl_mods: Mod) -> Self {
        let mut mods = BTreeSet::new();
        let mod_sets = [
            (Mod::LALTMOD | Mod::RALTMOD, Modifier::Alt),
            (Mod::LCTRLMOD | Mod::RCTRLMOD, Modifier::Ctrl),
            (Mod::LSHIFTMOD | Mod::RSHIFTMOD, Modifier::Shift),
            (Mod::LGUIMOD | Mod::RGUIMOD, Modifier::Super),
        ];
        for (sdl_mod, my_mod) in mod_sets {
            if sdl_mods.intersects(sdl_mod) {
                mods.insert(my_mod);
            }
        }
        KeyCombo { key, mods }
    }
}

impl FromStr for KeyCombo {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let err = || format!("invalid keycode: \"{}\"", s);
        let mut strs = s.split('+').collect::<Vec<_>>();
        let keycode_str = strs.pop().ok_or_else(err)?;
        let keycode = Keycode::from_name(keycode_str).ok_or_else(err)?;
        let mut mods = BTreeSet::new();
        for mod_str in strs {
            mods.insert(Modifier::from_str(mod_str)?);
        }
        Ok(KeyCombo { key: keycode, mods })
    }
}

impl<'de> Deserialize<'de> for KeyCombo {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        FromStr::from_str(&s).map_err(de::Error::custom)
    }
}

#[derive(Deserialize)]
struct JoypadBindings {
    up: KeyCombo,
    down: KeyCombo,
    left: KeyCombo,
    right: KeyCombo,
    a: KeyCombo,
    b: KeyCombo,
    start: KeyCombo,
    select: KeyCombo,
}

#[derive(Deserialize)]
struct EmuBindings {
    toggle_frame_limiter: KeyCombo,
    reset: KeyCombo,
}

#[derive(Deserialize)]
struct SavestateBindings {
    slot: u8,
    load: KeyCombo,
    save: KeyCombo,
}

#[derive(Deserialize, Default)]
/// The struct that `hotkeys.toml` gets deserialized into.
struct Keybinds {
    joypad: JoypadBindings,
    emu: EmuBindings,
    savestate: Vec<SavestateBindings>,
}

impl Default for JoypadBindings {
    fn default() -> Self {
        JoypadBindings {
            up: keycombo!(Up),
            down: keycombo!(Down),
            left: keycombo!(Left),
            right: keycombo!(Right),
            a: keycombo!(X),
            b: keycombo!(Z),
            start: keycombo!(Return),
            select: keycombo!(Tab),
        }
    }
}

impl Default for EmuBindings {
    fn default() -> Self {
        EmuBindings {
            toggle_frame_limiter: keycombo!(Space),
            reset: keycombo!(Ctrl; R),
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
    Reset,
    LoadState(u8),
    SaveState(u8),
}

/// A Hashmap between SDL Keycodes and relevant Hotkeys
pub struct Keymap {
    map: HashMap<KeyCombo, Hotkey>,
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
        let mut map = HashMap::from([
            (keys.joypad.up, Hotkey::Joypad(JoypadKey::Up)),
            (keys.joypad.down, Hotkey::Joypad(JoypadKey::Down)),
            (keys.joypad.left, Hotkey::Joypad(JoypadKey::Left)),
            (keys.joypad.right, Hotkey::Joypad(JoypadKey::Right)),
            (keys.joypad.a, Hotkey::Joypad(JoypadKey::A)),
            (keys.joypad.b, Hotkey::Joypad(JoypadKey::B)),
            (keys.joypad.start, Hotkey::Joypad(JoypadKey::Start)),
            (keys.joypad.select, Hotkey::Joypad(JoypadKey::Select)),
            (keys.emu.toggle_frame_limiter, Hotkey::ToggleFrameLimiter),
            (keys.emu.reset, Hotkey::Reset),
        ]);
        for state in keys.savestate {
            map.insert(state.load, Hotkey::LoadState(state.slot));
            map.insert(state.save, Hotkey::SaveState(state.slot));
        }
        Ok(Keymap { map })
    }

    pub fn get_hotkey(&self, key: Keycode, mods: Mod) -> Option<Hotkey> {
        self.map.get(&KeyCombo::from_sdl(key, mods)).copied()
    }
}
