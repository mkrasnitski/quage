use clap::AppSettings;
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(setting = AppSettings::DeriveDisplayOrder)]
pub struct Config {
    pub cartridge_path: PathBuf,

    #[structopt(long = "bootrom", default_value = "./dmg_boot.bin")]
    pub bootrom_path: PathBuf,

    #[structopt(long, default_value = "./saves")]
    pub saves_dir: PathBuf,

    #[structopt(long, default_value = "./hotkeys.toml")]
    pub hotkey_file: PathBuf,

    #[structopt(short, long, help = "Print CPU state after each instruction")]
    pub debug: bool,

    #[structopt(long, help = "Display the current tileset in a second window")]
    pub dump_tiles: bool,

    #[structopt(long)]
    pub show_fps: bool,

    #[structopt(long)]
    pub skip_bootrom: bool,
}
