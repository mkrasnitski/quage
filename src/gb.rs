use anyhow::{Context, Result};
use std::fs;
use std::time::{Duration, Instant};

use crate::cpu::*;
use crate::display::*;

pub struct GameBoy {
    cpu: CPU,
    display: Display,
}

impl GameBoy {
    pub fn new(bootrom_path: &str, cartridge_path: &str) -> Result<Self> {
        let bootrom = fs::read(bootrom_path)
            .with_context(|| format!("Couldn't read bootrom `{}`", bootrom_path))?;
        let cartridge = fs::read(cartridge_path)
            .with_context(|| format!("Couldn't read cartridge `{}`", cartridge_path))?;

        let gb = GameBoy {
            cpu: CPU::new(bootrom, cartridge, true)?,
            display: Display::new(160, 144)?,
        };
        Ok(gb)
    }

    pub fn run(&mut self) -> Result<()> {
        // let mut frames = 0;
        // let mut now = Instant::now();
        loop {
            self.cpu.step()?;
            if self.cpu.check_draw_call() {
                self.display.draw();

                // frames += 1;
                // let time_elapsed = Instant::now().duration_since(now);
                // if time_elapsed > Duration::from_secs(1) {
                //     println!("{}", frames);
                //     frames = 0;
                //     now = Instant::now();
                // }

                if let DisplayEvent::Quit = self.display.poll_events() {
                    break;
                }
            }
        }
        Ok(())
    }
}
