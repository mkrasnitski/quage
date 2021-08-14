mod bus;
mod config;
mod cpu;
mod gb;
mod hotkeys;
mod ppu;
mod sdl;
mod utils;
use anyhow::Result;
use structopt::StructOpt;

use crate::config::Config;
use crate::gb::GameBoy;

fn main() -> Result<()> {
    GameBoy::new(Config::from_args())?.run()
}
