#![allow(non_snake_case)]
use anyhow::{bail, Result};
use enum_primitive_derive::Primitive;
use num_traits::FromPrimitive;

use crate::display::*;
use crate::joypad::*;
use crate::ppu::*;
use crate::timers::*;

#[derive(Primitive)]
pub enum MapperType {
    ROM = 0x00,
    MBC1 = 0x01,
    MBC1Ram = 0x02,
    MBC1BattRam = 0x03,
    MBC3 = 0x11,
    MBC3Ram = 0x12,
    MBC3BattRam = 0x13,
    MBC5 = 0x19,
    MBC5Ram = 0x1A,
    MBC5BattRam = 0x1B,
}

pub struct Mapper {
    mapper_type: MapperType,
    num_rom_banks: u16,
    ram_size: u32,
    low_bank: u8,
    high_bank: u16,
    ram_bank: u8,
    ram_enabled: bool,
    flag: bool,
}

impl Mapper {
    fn write_byte(&mut self, addr: u16, val: u8) {
        let rom_mask = (1u16 << (((self.num_rom_banks - 1) as f32).log2() as u8 + 1)) - 1;
        let ram_mask = if self.ram_size <= 8192 {
            0
        } else {
            (1 << (((self.ram_size - 1) as f32 / 8192.0).log2() as u8 + 1)) - 1
        };
        match self.mapper_type {
            MapperType::ROM => (),
            MapperType::MBC1 | MapperType::MBC1Ram | MapperType::MBC1BattRam => match addr {
                0x0000..=0x1FFF => self.ram_enabled = (val & 0xf) == 0xA,
                0x2000..=0x3FFF => {
                    let val = val as u16 & 0x1f;
                    let bank_num = if val == 0 { 1 } else { val & rom_mask };
                    self.high_bank = (self.high_bank & 0x60) | bank_num as u16;
                }
                0x4000..=0x5FFF => {
                    let val = val & 0b11;
                    self.high_bank = (((val << 5) as u16) | (self.high_bank & 0x1f)) & rom_mask;
                    if self.flag {
                        self.ram_bank = val & ram_mask;
                        self.low_bank = (val << 5) & rom_mask as u8;
                    }
                }
                0x6000..=0x7FFF => self.flag = val != 0,
                _ => panic!("Invalid MBC1 write addr: {:#04x}", addr),
            },
            MapperType::MBC3 | MapperType::MBC3Ram | MapperType::MBC3BattRam => match addr {
                0x0000..=0x1FFF => self.ram_enabled = (val & 0xf) == 0xA,
                0x2000..=0x3FFF => {
                    let val = val as u16 & 0x7f;
                    self.high_bank = if val == 0 { 1 } else { val & rom_mask };
                }
                0x4000..=0x5FFF => self.ram_bank = val & ram_mask,
                0x6000..=0x7FFF => {} // RTC
                _ => panic!("Invalid MBC3 write addr: {:#04x}", addr),
            },
            MapperType::MBC5 | MapperType::MBC5Ram | MapperType::MBC5BattRam => match addr {
                0x0000..=0x1FFF => self.ram_enabled = (val & 0xf) == 0xA,
                0x2000..=0x2FFF => {
                    self.high_bank = ((self.high_bank & 0x100) | val as u16) & rom_mask
                }
                0x3000..=0x3FFF => {
                    self.high_bank = (((val as u16 & 1) << 8) | (self.high_bank & 0xff)) & rom_mask
                }
                0x4000..=0x5FFF => self.ram_bank = val & ram_mask,
                0x6000..=0x7FFF => {}
                _ => panic!("Invalid MBC5 write addr: {:#04x}", addr),
            },
        }
    }

    fn get_high_bank(&self) -> u16 {
        self.high_bank
    }

    fn get_low_bank(&self) -> u8 {
        if self.flag {
            self.low_bank
        } else {
            0
        }
    }

    fn get_ram_bank(&self) -> u8 {
        if self.flag {
            self.ram_bank
        } else {
            0
        }
    }
}

pub struct Cartridge {
    contents: Vec<u8>,
    ram: Vec<u8>,
    mapper: Mapper,
}

impl Cartridge {
    fn new(cart: Vec<u8>) -> Result<Self> {
        let mapper = cart[0x147];
        let size = cart[0x148];
        let ram = cart[0x149];
        let num_rom_banks = match size {
            0x00..=0x08 => 2 << size,
            _ => bail!("Invalid ROM Size: {:#02x}", size),
        };
        let ram_size = 1024
            * match ram {
                0x00 => 0,
                0x01 => 2,
                0x02 => 8,
                0x03 => 32,
                0x04 => 128,
                0x05 => 64,
                _ => bail!("Invalid RAM Size: {:#02x}", ram),
            };
        Ok(Cartridge {
            contents: cart,
            ram: vec![0; ram_size as usize],
            mapper: Mapper {
                mapper_type: MapperType::from_u8(mapper)
                    .ok_or_else(|| anyhow::anyhow!("Invalid Mapper: {:#02x}", mapper))?,
                num_rom_banks,
                ram_size,
                low_bank: 0,
                high_bank: 1,
                ram_bank: 0,
                ram_enabled: false,
                flag: false,
            },
        })
    }

    pub fn read_rom_byte(&self, addr: u16) -> u8 {
        if addr < 0x4000 {
            self.contents[self.mapper.get_low_bank() as usize * 0x4000 + addr as usize]
        } else {
            self.contents[self.mapper.get_high_bank() as usize * 0x4000 + addr as usize - 0x4000]
        }
    }

    pub fn read_ram_byte(&self, addr: u16) -> u8 {
        if self.mapper.ram_enabled {
            self.ram[self.mapper.get_ram_bank() as usize * 0x2000 + addr as usize - 0xA000]
        } else {
            0xFF
        }
    }

    pub fn write_ram_byte(&mut self, addr: u16, val: u8) {
        let addr = addr as usize - 0xA000;
        if self.mapper.ram_enabled && addr < self.ram.len() {
            self.ram[self.mapper.get_ram_bank() as usize * 0x2000 + addr] = val
        }
    }
}

pub struct MemoryBus {
    pub ppu: PPU,
    pub timers: Timers,
    pub joypad: Joypad,
    work_ram: [u8; 0x2000],
    hram: [u8; 0x7f],
    bootrom: Vec<u8>,
    cartridge: Cartridge,
    IE: u8,
    IF: u8,
    dma_start: u8,
    bootrom_switch: u8,
}

impl MemoryBus {
    pub fn new(bootrom: Vec<u8>, cartridge: Vec<u8>) -> Result<Self> {
        Ok(MemoryBus {
            ppu: PPU::new()?,
            timers: Timers::default(),
            work_ram: [0; 0x2000],
            hram: [0; 0x7f],
            bootrom,
            cartridge: Cartridge::new(cartridge)?,
            joypad: Joypad::default(),
            IE: 0,
            IF: 0xE0,
            dma_start: 0,
            bootrom_switch: 0,
        })
    }

    pub fn check_interrupts(&mut self) {
        let (vblank, stat) = self.ppu.check_interrupts();
        let joypad = self.joypad.poll();
        self.IF = (self.IF & 0xC) | (vblank as u8) | ((stat as u8) << 1) | ((joypad as u8) << 4);
    }

    pub fn poll_display_event(&mut self) -> DisplayEvent {
        let event = self.ppu.poll_display_event();
        if let DisplayEvent::KeyEvent((key, pressed)) = &event {
            if self.joypad.is_valid_key(key) {
                self.joypad.update_key(key, *pressed);
            }
        }
        event
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
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
            0xFEA0..=0xFEFF => 0xFF,
            0xFF80..=0xFFFE => self.hram[addr as usize - 0xFF80],

            0xFF00 => self.joypad.read(),
            0xFF04..=0xFF07 => self.timers.read_byte(addr),
            0xFF40..=0xFF45 | 0xFF47..=0xFF4B => self.ppu.read_byte(addr),
            0xFF46 => self.dma_start,
            0xFF50 => self.bootrom_switch,
            0xFF0F => self.IF,
            0xFFFF => self.IE,

            // stubs
            0xFF01..=0xFF02 => 0x00, // serial
            0xFF10..=0xFF26 => 0x00, // sound
            0xFF30..=0xFF3F => 0x00, // waveform RAM

            // unused on DMG:
            // 0xFF03
            // 0xFF08..=0xFF0E
            // 0xFF27..=0xFF2F
            // 0xFF4C..=0xFF4F
            // 0xFF51..=0xFF7F
            _ => 0xFF,
        }
    }

    pub fn write_byte(&mut self, addr: u16, val: u8) {
        match addr {
            0x0000..=0x7FFF => self.cartridge.mapper.write_byte(addr, val),
            0x8000..=0x9FFF => self.ppu.write_byte(addr, val),
            0xA000..=0xBFFF => self.cartridge.write_ram_byte(addr, val),
            0xC000..=0xDFFF => self.work_ram[addr as usize - 0xC000] = val,
            0xE000..=0xFDFF => self.work_ram[addr as usize - 0xE000] = val,
            0xFE00..=0xFE9F => self.ppu.write_byte(addr, val),
            0xFEA0..=0xFEFF => {}
            0xFF80..=0xFFFE => self.hram[addr as usize - 0xFF80] = val,

            0xFF00 => self.joypad.write(val),
            0xFF04..=0xFF07 => self.timers.write_byte(addr, val),
            0xFF40..=0xFF45 | 0xFF47..=0xFF4B => self.ppu.write_byte(addr, val),
            0xFF46 => {
                self.dma_start = val;
                self.run_dma_transfer();
            }
            0xFF50 => self.bootrom_switch = val,
            0xFF0F => self.IF = val | 0xE0,
            0xFFFF => self.IE = val,

            // stubs
            0xFF01..=0xFF02 => {} // serial
            0xFF10..=0xFF26 => {} // sound
            0xFF30..=0xFF3F => {} // waveform RAM

            // unused
            _ => {}
        }
    }

    fn run_dma_transfer(&mut self) {
        for i in 0..160 {
            self.write_byte(
                0xFE00 + i,
                self.read_byte(((self.dma_start as u16) << 8) + i),
            )
        }
    }

    #[allow(dead_code)]
    pub fn read_word(&self, addr: u16) -> u16 {
        ((self.read_byte(addr + 1) as u16) << 8) | (self.read_byte(addr) as u16)
    }

    pub fn write_word(&mut self, addr: u16, val: u16) {
        self.write_byte(addr, val as u8);
        self.write_byte(addr + 1, (val >> 8) as u8)
    }
}
