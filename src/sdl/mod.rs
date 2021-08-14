mod display;

use anyhow::{Error, Result};
use sdl2::event::Event;
use sdl2::keyboard::Keycode;

use crate::config::Config;
use crate::hotkeys::{Hotkey, Keymap};
use display::Display;

pub const W_WIDTH: usize = 160;
pub const W_HEIGHT: usize = 144;

pub enum SDLEvent {
    HotkeyEvent((Hotkey, bool)),
    Quit,
    None,
}

pub struct SDLManager {
    pub display: Display<W_WIDTH, W_HEIGHT>,
    pub tile_display: Option<Display<128, 192>>,
    event_pump: sdl2::EventPump,
    hotkey_map: Keymap,
}

impl SDLManager {
    pub fn new(config: &Config) -> Result<Self> {
        let context = sdl2::init().map_err(Error::msg)?;
        let video = context.video().map_err(Error::msg)?;
        let event_pump = context.event_pump().map_err(Error::msg)?;
        let display = Display::new(&video, None, config.show_fps)?;
        let tile_display = config
            .dump_tiles
            .then(|| Display::new(&video, Some((1220, 250)), false))
            .transpose()?;
        Ok(SDLManager {
            display,
            tile_display,
            event_pump,
            hotkey_map: Keymap::new(&config.hotkey_file)?,
        })
    }

    pub fn toggle_frame_limiter(&mut self) {
        self.display.toggle_frame_limiter();
        if let Some(tile_display) = self.tile_display.as_mut() {
            tile_display.toggle_frame_limiter();
        }
    }

    pub fn poll_event(&mut self) -> SDLEvent {
        for event in self.event_pump.poll_iter() {
            match event {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => return SDLEvent::Quit,
                Event::KeyDown {
                    keycode: Some(k),
                    keymod: mods,
                    repeat: false,
                    ..
                } => {
                    if let Some(hotkey) = self.hotkey_map.get_hotkey(k, mods) {
                        return SDLEvent::HotkeyEvent((hotkey, true));
                    }
                }
                Event::KeyUp {
                    keycode: Some(k),
                    keymod: mods,
                    repeat: false,
                    ..
                } => {
                    // This has a limitation, in that KeyUp events also have to have
                    // the same modifiers active in order to register the same Hotkey.
                    // Moral of the story: Don't use modifiers for Joypad bindings,
                    // where we care about KeyUp.
                    if let Some(hotkey) = self.hotkey_map.get_hotkey(k, mods) {
                        return SDLEvent::HotkeyEvent((hotkey, false));
                    }
                }
                _ => continue,
            }
        }
        SDLEvent::None
    }
}
