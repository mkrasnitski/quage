mod cpu;
mod flags;
mod gb;
mod gpu;
mod instruction;
use anyhow::{bail, Result};
use std::env;

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        bail!("Please provide a bootrom and cartridge.")
    }
    let mut gb = gb::GameBoy::new(&args[1], &args[2])?;
    gb.run()
}
