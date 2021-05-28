#![allow(non_snake_case)]
use anyhow::Result;

use crate::cartridge::Cartridge;
use crate::display::DisplayEvent;
use crate::joypad::Joypad;
use crate::ppu::PPU;
use crate::sound::Sound;
use crate::timers::Timers;

pub struct MemoryBus {
    pub ppu: PPU,
    pub timers: Timers,
    pub joypad: Joypad,
    pub sound: Sound,
    pub cartridge: Cartridge,
    work_ram: [u8; 0x2000],
    hram: [u8; 0x7f],
    bootrom: Vec<u8>,
    IE: u8,
    IF: u8,
    dma_start: u8,
    dma_byte: u8,
    dma_cycles: u8,
    dma_running: bool,
    bootrom_switch: u8,
}

impl MemoryBus {
    pub fn new(bootrom: Vec<u8>, cartridge: Vec<u8>) -> Result<Self> {
        Ok(MemoryBus {
            ppu: PPU::new()?,
            timers: Timers::new(),
            joypad: Joypad::new(),
            sound: Sound::new(),
            cartridge: Cartridge::new(cartridge)?,
            work_ram: rand::random(),
            hram: [0; 0x7f],
            bootrom,
            IE: 0,
            IF: 0xE0,
            dma_start: 0,
            dma_byte: 0,
            dma_cycles: 0,
            dma_running: false,
            bootrom_switch: 0,
        })
    }

    pub fn check_interrupts(&mut self) {
        let (vblank, stat) = self.ppu.check_interrupts();
        let joypad = self.joypad.poll();
        self.IF |= (vblank as u8) | ((stat as u8) << 1) | ((joypad as u8) << 4);
    }

    pub fn increment_dma(&mut self) {
        if self.dma_running {
            if self.dma_cycles > 0 {
                let i = self.dma_cycles as u16 - 1;
                self.dma_byte = self.read_byte_direct(((self.dma_start as u16) << 8) + i);
                self.write_byte_direct(0xFE00 + i, self.dma_byte);
            }
            if self.dma_cycles == 160 {
                self.dma_cycles = 0;
                self.dma_running = false;
            } else {
                self.dma_cycles += 1;
            }
        }
    }

    pub fn poll_display_event(&mut self) -> DisplayEvent {
        let event = self.ppu.poll_display_event();
        if let DisplayEvent::KeyEvent((key, pressed)) = &event {
            if self.joypad.is_valid_key(key) {
                self.joypad.update_key(key, *pressed);
            } else if key == "Space" && *pressed {
                self.ppu.toggle_frame_limiter();
            }
        }
        event
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
        if self.dma_conflict(addr) {
            // println!("DMA Conflict! {:04x} {:02x}", addr, self.dma_start);
            self.dma_byte
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
                if addr < 0x100 && self.bootrom_switch == 0 {
                    return self.bootrom[addr as usize];
                }
                self.cartridge.read_rom_byte(addr)
            }
            0x8000..=0x9FFF => self.ppu.read_byte(addr),
            0xA000..=0xBFFF => self.cartridge.read_ram_byte(addr),
            0xC000..=0xDFFF => self.work_ram[addr as usize - 0xC000],
            0xE000..=0xFDFF => self.work_ram[addr as usize - 0xE000],

            0xFE00..=0xFE9F => self.ppu.read_byte(addr),
            0xFEA0..=0xFEFF => 0x00,
            0xFF80..=0xFFFE => self.hram[addr as usize - 0xFF80],

            0xFF00 => self.joypad.read(),
            0xFF04..=0xFF07 => self.timers.read_byte(addr),
            0xFF10..=0xFF14 | 0xFF16..=0xFF1E | 0xFF20..=0xFF26 | 0xFF30..=0xFF3F => {
                self.sound.read_byte(addr)
            }
            0xFF40..=0xFF45 | 0xFF47..=0xFF4B => self.ppu.read_byte(addr),
            0xFF46 => self.dma_start,
            0xFF50 => 0xFF, // read-only
            0xFF0F => self.IF,
            0xFFFF => self.IE,

            // stubs
            0xFF01 => 0x00, // serial
            0xFF02 => 0x7E,

            // unused on DMG:
            // 0xFF03
            // 0xFF08..=0xFF0E
            // 0xFF15
            // 0xFF1F
            // 0xFF27..=0xFF2F
            // 0xFF4C..=0xFF4F
            // 0xFF51..=0xFF7F
            _ => 0xFF,
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
                self.dma_start = val;
                self.dma_cycles = 0;
                self.dma_running = true;
            }
            0xFF50 => {
                // Only written to once
                if self.bootrom_switch == 0 {
                    self.bootrom_switch = val
                }
            }
            0xFF0F => self.IF = val | 0xE0,
            0xFFFF => self.IE = val,

            // stubs
            0xFF01 | 0xFF02 => {} // serial

            // unused
            _ => {}
        }
    }

    fn dma_conflict(&self, addr: u16) -> bool {
        self.dma_running
            && match addr {
                0x0000..=0x7FFF | 0xA000..=0xFDFF => {
                    self.dma_start < 0x7F || (0xA0..=0xFD).contains(&self.dma_start)
                }
                0x8000..=0x9FFF => (0x80..=0x9F).contains(&self.dma_start),
                0xFE00..=0xFFFF => false,
            }
    }
}
