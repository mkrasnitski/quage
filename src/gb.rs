use anyhow::{Context, Result};
use std::fs;
use std::path::Path;

use crate::cpu::*;

pub struct GameBoy {
    cpu: CPU,
}

impl GameBoy {
    pub fn new() -> Result<Self> {
        let bootrom_path = Path::new("dmg_boot.bin");
        let bootrom = fs::read(bootrom_path)
            .with_context(|| format!("Couldn't read file `{}`", bootrom_path.to_str().unwrap()))?;
        // let bootrom: Vec<u8> = [0x00].to_vec();
        let gb = GameBoy {
            cpu: CPU::new(bootrom),
        };
        Ok(gb)
    }

    pub fn run(&mut self) -> Result<()> {
        loop {
            self.cpu.step()?;
        }
    }
}
