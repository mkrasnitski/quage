#![allow(non_snake_case)]
mod cartridge;
mod joypad;
mod rtc;
mod sound;
mod timers;

use anyhow::Result;
use serde::{Deserialize, Serialize};
use serde_big_array::BigArray;

use crate::ppu::PPU;
use cartridge::Cartridge;
use joypad::Joypad;
use sound::Sound;
use timers::Timers;

#[derive(Default, Serialize, Deserialize)]
struct DMA {
    base: u8,
    next_base: u8,
    byte: u8,
    cycles: u8,
    countdown: u8,
    running: bool,
}

#[derive(Serialize, Deserialize)]
pub struct MemoryBus {
    pub ppu: PPU,
    pub timers: Timers,
    pub joypad: Joypad,
    pub sound: Sound,
    pub cartridge: Cartridge,
    pub bootrom: Vec<u8>,
    bootrom_switch: bool,
    #[serde(with = "BigArray")]
    work_ram: [u8; 0x2000],
    #[serde(with = "BigArray")]
    hram: [u8; 0x7f],
    dma: DMA,
    pub IE: u8,
    pub IF: u8,
}

impl MemoryBus {
    pub fn new(bootrom: Vec<u8>, cartridge: Vec<u8>) -> Result<Self> {
        Ok(MemoryBus {
            ppu: PPU::new(),
            timers: Timers::new(),
            joypad: Joypad::new(),
            sound: Sound::new(),
            cartridge: Cartridge::new(cartridge)?,
            work_ram: rand::random(),
            dma: DMA::default(),
            hram: [0; 0x7f],
            bootrom,
            bootrom_switch: false,
            IE: 0,
            IF: 0xE0,
        })
    }

    pub fn check_interrupts(&mut self) {
        let (vblank, stat) = self.ppu.check_interrupts();
        let joypad = self.joypad.poll();
        self.IF |= (vblank as u8) | ((stat as u8) << 1) | ((joypad as u8) << 4);
    }

    pub fn increment_dma(&mut self) {
        if self.dma.countdown > 0 {
            self.dma.countdown -= 1;
            if self.dma.countdown == 0 {
                self.dma.base = self.dma.next_base;
                self.dma.cycles = 0;
                self.dma.running = true;
            }
        }
        if self.dma.running {
            let i = self.dma.cycles as u16;
            let base = match self.dma.base {
                0xFE | 0xFF => self.dma.base as u16 - 0x20,
                _ => self.dma.base as u16,
            };
            self.dma.byte = self.read_byte_direct((base << 8) + i);
            self.write_byte_direct(0xFE00 + i, self.dma.byte);
            if self.dma.cycles == 160 {
                self.dma.cycles = 0;
                self.dma.running = false;
            } else {
                self.dma.cycles += 1;
            }
        }
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
        if self.dma_conflict(addr) {
            self.dma.byte
        } else {
            self.read_byte_direct(addr)
        }
    }

    pub fn write_byte(&mut self, addr: u16, val: u8) {
        if !self.dma_conflict(addr) {
            self.write_byte_direct(addr, val);
        }
    }

    fn read_byte_direct(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x7FFF => {
                if addr < 0x100 && !self.bootrom_switch {
                    return self.bootrom[addr as usize];
                }
                self.cartridge.read_rom_byte(addr)
            }
            0x8000..=0x9FFF => self.ppu.read_byte(addr),
            0xA000..=0xBFFF => self.cartridge.read_ram_byte(addr),
            0xC000..=0xDFFF => self.work_ram[addr as usize - 0xC000],
            0xE000..=0xFDFF => self.work_ram[addr as usize - 0xE000],

            0xFE00..=0xFE9F => {
                if self.dma.running {
                    0xFF
                } else {
                    self.ppu.read_byte(addr)
                }
            }
            0xFEA0..=0xFEFF => 0x00,
            0xFF80..=0xFFFE => self.hram[addr as usize - 0xFF80],

            0xFF00 => self.joypad.read(),
            0xFF04..=0xFF07 => self.timers.read_byte(addr),
            0xFF10..=0xFF14 | 0xFF16..=0xFF1E | 0xFF20..=0xFF26 | 0xFF30..=0xFF3F => {
                self.sound.read_byte(addr)
            }
            0xFF40..=0xFF45 | 0xFF47..=0xFF4B => self.ppu.read_byte(addr),
            0xFF46 => self.dma.base,
            0xFF50 => 0xFF, // read-only
            0xFF0F => self.IF,
            0xFFFF => self.IE,

            // serial
            0xFF01 => 0x00,
            0xFF02 => 0x7E,

            // unused on DMG
            0xFF03
            | 0xFF08..=0xFF0E
            | 0xFF15
            | 0xFF1F
            | 0xFF27..=0xFF2F
            | 0xFF4C..=0xFF4F
            | 0xFF51..=0xFF7F => 0xFF,
        }
    }

    fn write_byte_direct(&mut self, addr: u16, val: u8) {
        match addr {
            0x0000..=0x7FFF => self.cartridge.write_rom_byte(addr, val),
            0x8000..=0x9FFF => self.ppu.write_byte(addr, val),
            0xA000..=0xBFFF => self.cartridge.write_ram_byte(addr, val),
            0xC000..=0xDFFF => self.work_ram[addr as usize - 0xC000] = val,
            0xE000..=0xFDFF => self.work_ram[addr as usize - 0xE000] = val,

            0xFE00..=0xFE9F => self.ppu.write_byte(addr, val),
            0xFEA0..=0xFEFF => {}
            0xFF80..=0xFFFE => self.hram[addr as usize - 0xFF80] = val,

            0xFF00 => self.joypad.write(val),
            0xFF04..=0xFF07 => self.timers.write_byte(addr, val),
            0xFF10..=0xFF14 | 0xFF16..=0xFF1E | 0xFF20..=0xFF26 | 0xFF30..=0xFF3F => {
                self.sound.write_byte(addr, val)
            }
            0xFF40..=0xFF45 | 0xFF47..=0xFF4B => self.ppu.write_byte(addr, val),
            0xFF46 => {
                self.dma.next_base = val;
                self.dma.countdown = 2;
            }
            0xFF50 => {
                // Only written to once
                if !self.bootrom_switch && val & 1 == 1 {
                    self.bootrom_switch = true
                }
            }
            0xFF0F => self.IF = val | 0xE0,
            0xFFFF => self.IE = val,

            // serial
            0xFF01 | 0xFF02 => {}

            // unused on DMG
            0xFF03
            | 0xFF08..=0xFF0E
            | 0xFF15
            | 0xFF1F
            | 0xFF27..=0xFF2F
            | 0xFF4C..=0xFF4F
            | 0xFF51..=0xFF7F => {}
        }
    }

    fn dma_conflict(&self, addr: u16) -> bool {
        self.dma.running
            && match addr {
                0x0000..=0x7FFF | 0xA000..=0xFDFF => {
                    self.dma.base < 0x7F || (0xA0..=0xFD).contains(&self.dma.base)
                }
                0x8000..=0x9FFF => (0x80..=0x9F).contains(&self.dma.base),
                0xFE00..=0xFFFF => false,
            }
    }
}
