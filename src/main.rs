mod cpu;
mod flags;
mod gb;
mod gpu;
mod instruction;
use anyhow::Result;

fn main() -> Result<()> {
    let mut gb = gb::GameBoy::new()?;
    gb.run()
    // println!(
    //     "{:?}",
    //     crate::instruction::Instruction::from_byte(0x1a, false)
    // );
    // Ok(())
}
