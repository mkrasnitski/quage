use anyhow::{Context, Result};
use std::fs;

use crate::cpu::*;

pub struct GameBoy {
    cpu: CPU,
}

impl GameBoy {
    pub fn new(bootrom_path: &str, cartridge_path: &str) -> Result<Self> {
        let bootrom = fs::read(bootrom_path)
            .with_context(|| format!("Couldn't read bootrom `{}`", bootrom_path))?;
        let cartridge = fs::read(cartridge_path)
            .with_context(|| format!("Couldn't read cartridge `{}`", cartridge_path))?;

        let gb = GameBoy {
            cpu: CPU::new(bootrom, cartridge, true),
        };
        Ok(gb)
    }

    pub fn run(&mut self) -> Result<()> {
        loop {
            self.cpu.step()?;
        }
    }
}
