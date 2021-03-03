mod bus;
mod cpu;
mod display;
mod flags;
mod gb;
mod instruction;
mod joypad;
mod ppu;
mod rtc;
mod timers;
use anyhow::Result;
use std::env;

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        anyhow::bail!("Please provide a cartridge.")
    }
    let mut gb = gb::GameBoy::new("dmg_boot.bin", &args[1])?;
    gb.run()
}
