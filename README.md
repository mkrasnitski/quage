# quage - The Questionably Accurate Gameboy Emulator

Written in 100% Rust, using SDL2 for graphics. Targets Linux for lack of Windows machines to test on.

## Features

 - [x] Playing of original DMG games
 - [x] Battery saves
 - [x] Decent Accuracy (?)
 - [x] Configurable Hotkeys
 - [ ] Audio
 - [ ] Savestates
 - [ ] Gameboy Color (CGB) support

## Build Requirements
Rust >= 1.51.0 is required, as well as libSDL2.

## Usage
```
quage 0.5.0

USAGE:
    quage [FLAGS] [OPTIONS] <cartridge-path>

FLAGS:
    -d, --debug           Print CPU state after each instruction
        --dump-tiles      Display the current tileset in a second window
        --show-fps
        --skip-bootrom
    -h, --help            Prints help information
    -V, --version         Prints version information

OPTIONS:
        --bootrom <bootrom-path>        [default: ./dmg_boot.bin]
        --saves-dir <saves-dir>         [default: ./saves]
        --hotkey-file <hotkey-file>     [default: ./hotkeys.toml]

ARGS:
    <cartridge-path>
```
