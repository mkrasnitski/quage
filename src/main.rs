mod bus;
mod cpu;
mod flags;
mod gb;
mod gpu;
mod instruction;
use anyhow::{bail, Result};
use std::env;

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        bail!("Please provide a cartridge.")
    }
    let mut gb = gb::GameBoy::new("dmg_boot.bin", &args[1])?;
    gb.run()
}
