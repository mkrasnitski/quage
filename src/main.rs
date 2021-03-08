mod bus;
mod cpu;
mod display;
mod flags;
mod gb;
mod instruction;
mod joypad;
mod ppu;
mod rtc;
mod sound;
mod timers;
use anyhow::Result;
use std::env;

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        anyhow::bail!("Please provide a bootrom and cartridge.")
    }
    let mut gb = gb::GameBoy::new(&args[1], &args[2])?;
    gb.run()
}
