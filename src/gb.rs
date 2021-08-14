use anyhow::{Context, Result};
use std::fs::{self, File};
use std::io::{Read, Write};
use std::path::PathBuf;

use crate::config::Config;
use crate::cpu::CPU;
use crate::display::*;
use crate::hotkeys::*;

pub struct GameBoy {
    cpu: CPU,
    sdl_manager: SDLManager,
    save_path: PathBuf,
    savestates_path: PathBuf,
}

impl GameBoy {
    pub fn new(config: Config) -> Result<Self> {
        let bootrom = fs::read(&config.bootrom_path)
            .context(format!("Bootrom {:?} not found", &config.bootrom_path))?;
        let cartridge = fs::read(&config.cartridge_path)
            .context(format!("Cartridge {:?} not found", &config.cartridge_path))?;

        let cartridge_name = config.cartridge_path.file_stem().unwrap();

        let mut save_path = config.saves_dir.clone();
        save_path.push(cartridge_name);
        save_path.set_extension("sav");

        let mut savestates_path = config.savestates_dir.clone();
        savestates_path.push(cartridge_name);

        let mut gb = GameBoy {
            cpu: CPU::new(bootrom, cartridge, &config)?,
            sdl_manager: SDLManager::new(&config)?,
            save_path,
            savestates_path,
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
        match self.sdl_manager.poll_event() {
            DisplayEvent::HotkeyEvent((key, pressed)) => match key {
                Hotkey::Joypad(key) => self.cpu.bus.joypad.update_key(key, pressed),
                Hotkey::ToggleFrameLimiter => {
                    if pressed {
                        self.sdl_manager.toggle_frame_limiter();
                    }
                }
                Hotkey::LoadState(slot) => {
                    if pressed {
                        self.load_state(slot)?;
                    }
                }
                Hotkey::SaveState(slot) => {
                    if pressed {
                        self.save_state(slot)?;
                    }
                }
            },
            DisplayEvent::Quit => {
                self.cpu.bus.cartridge.save_external_ram(&self.save_path)?;
                return Ok(true);
            }
            DisplayEvent::None => {}
        };
        Ok(false)
    }

    fn load_state(&mut self, slot: u8) -> Result<()> {
        let mut path = self.savestates_path.clone();
        path.push(format!("{}.state", slot));
        if let Ok(mut file) = File::open(path) {
            let mut data = Vec::new();
            file.read_to_end(&mut data)?;
            self.cpu = bincode::deserialize(&data)?;
            println!("Loaded slot {}", slot);
        } else {
            println!("Slot {} is empty", slot);
        }
        Ok(())
    }

    fn save_state(&mut self, slot: u8) -> Result<()> {
        let mut path = self.savestates_path.clone();
        fs::create_dir_all(&path)?;
        path.push(format!("{}.state", slot));
        File::create(path)?.write_all(&bincode::serialize(&self.cpu)?)?;
        println!("Saved slot {}", slot);
        Ok(())
    }
}
