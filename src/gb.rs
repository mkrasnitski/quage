use anyhow::{Context, Result};
use std::fs;
use std::path::*;

use crate::cpu::*;
use crate::display::*;

pub struct GameBoy {
    cpu: CPU,
    save_path: PathBuf,
}

impl GameBoy {
    pub fn new(bootrom_path: &str, cartridge_path: &str) -> Result<Self> {
        let bootrom = fs::read(bootrom_path)
            .with_context(|| format!("Couldn't read bootrom `{}`", bootrom_path))?;
        let cartridge = fs::read(cartridge_path)
            .with_context(|| format!("Couldn't read cartridge `{}`", cartridge_path))?;

        let cart_path = PathBuf::from(cartridge_path);
        let mut save_path = PathBuf::from("saves");
        save_path.push(cart_path.file_stem().unwrap());
        save_path.set_extension("sav");

        let mut gb = GameBoy {
            cpu: CPU::new(bootrom, cartridge, true)?,
            save_path,
        };
        gb.cpu.bus.load_external_ram(&gb.save_path);
        Ok(gb)
    }

    pub fn run(&mut self) -> Result<()> {
        loop {
            self.cpu.step()?;
            if let DisplayEvent::Quit = self.cpu.bus.poll_display_event() {
                self.cpu.bus.save_external_ram(self.save_path.as_path());
                break;
            }
        }
        Ok(())
    }
}
