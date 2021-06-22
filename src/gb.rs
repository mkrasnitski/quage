use anyhow::{Context, Result};
use std::fs;
use std::path::PathBuf;

use crate::config::Config;
use crate::cpu::CPU;
use crate::display::DisplayEvent;

pub struct GameBoy {
    cpu: CPU,
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
            save_path,
        };
        gb.cpu.bus.cartridge.load_external_ram(&gb.save_path)?;
        Ok(gb)
    }

    pub fn run(&mut self) -> Result<()> {
        loop {
            self.cpu.step()?;
            if let DisplayEvent::Quit = self.cpu.bus.poll_display_event() {
                self.cpu.bus.cartridge.save_external_ram(&self.save_path)?;
                break;
            }
        }
        Ok(())
    }
}
