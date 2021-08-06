use anyhow::{Context, Result};
use std::fs;
use std::path::PathBuf;

use crate::config::Config;
use crate::cpu::CPU;
use crate::display::*;
use crate::hotkeys::*;

pub struct GameBoy {
    cpu: CPU,
    sdl_manager: SDLManager,
    save_path: PathBuf,
}

impl GameBoy {
    pub fn new(config: Config) -> Result<Self> {
        let bootrom = fs::read(&config.bootrom_path)
            .context(format!("Bootrom {:?} not found", &config.bootrom_path))?;
        let cartridge = fs::read(&config.cartridge_path)
            .context(format!("Cartridge {:?} not found", &config.cartridge_path))?;

        let mut save_path = config.saves_dir.clone();
        save_path.push(config.cartridge_path.file_stem().unwrap());
        save_path.set_extension("sav");

        let mut gb = GameBoy {
            cpu: CPU::new(bootrom, cartridge, &config)?,
            sdl_manager: SDLManager::new(&config)?,
            save_path,
        };
        gb.cpu.bus.cartridge.load_external_ram(&gb.save_path)?;
        Ok(gb)
    }

    pub fn run(&mut self) -> Result<()> {
        loop {
            self.cpu.step()?;
            if self.cpu.bus.ppu.draw_frame {
                self.cpu.bus.ppu.paint_display(&mut self.sdl_manager);
                if self.handle_display_events()? {
                    break;
                }
            }
        }
        Ok(())
    }

    fn handle_display_events(&mut self) -> Result<bool> {
        match self.sdl_manager.display_manager.poll_event() {
            DisplayEvent::KeyEvent((key, pressed)) => {
                if let Some(key) = key {
                    if let Hotkey::ToggleFrameLimiter = key {
                        if pressed {
                            self.sdl_manager.toggle_frame_limiter();
                        }
                    } else {
                        self.cpu.bus.joypad.update_key(key, pressed);
                    }
                }
            }
            DisplayEvent::Quit => {
                self.cpu.bus.cartridge.save_external_ram(&self.save_path)?;
                return Ok(true);
            }
            DisplayEvent::None => {}
        };
        Ok(false)
    }
}
