mod cpu;
mod gb;
mod gpu;
mod instruction;
use anyhow::Result;

fn main() -> Result<()> {
    let mut gb = gb::GameBoy::new()?;
    gb.run()
}
