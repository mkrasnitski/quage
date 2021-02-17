use anyhow::{Context, Result};
use std::fs;

use crate::cpu::*;
use crate::display::*;

pub struct GameBoy {
    cpu: CPU,
}

impl GameBoy {
    pub fn new(bootrom_path: &str, cartridge_path: &str) -> Result<Self> {
        let bootrom = fs::read(bootrom_path)
            .with_context(|| format!("Couldn't read bootrom `{}`", bootrom_path))?;
        let cartridge = fs::read(cartridge_path)
            .with_context(|| format!("Couldn't read cartridge `{}`", cartridge_path))?;

        Ok(GameBoy {
            cpu: CPU::new(bootrom, cartridge, true)?,
        })
    }

    pub fn run(&mut self) -> Result<()> {
        loop {
            self.cpu.step()?;
            if let DisplayEvent::Quit = self.cpu.poll_display_event() {
                break;
            }
        }
        Ok(())
    }
}
