mod bus;
mod cartridge;
mod config;
mod cpu;
mod debug;
mod display;
mod flags;
mod gb;
mod hotkeys;
mod instruction;
mod joypad;
mod ppu;
mod rtc;
mod sound;
mod timers;
use anyhow::Result;
use structopt::StructOpt;

use crate::config::Config;
use crate::gb::GameBoy;

fn main() -> Result<()> {
    GameBoy::new(Config::from_args())?.run()
}
