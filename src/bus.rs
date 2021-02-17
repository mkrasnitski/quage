#![allow(non_snake_case)]
use anyhow::{bail, Result};
use enum_primitive_derive::Primitive;
use num_traits::FromPrimitive;

use crate::ppu::*;

#[derive(Primitive)]
pub enum MapperType {
    ROM = 0,
    MBC1 = 1,
    MBC1Ram = 2,
    MBC1BattRam = 3,
}

pub struct Mapper {
    mapper_type: MapperType,
    num_rom_banks: u16,
    ram_size: u32,
    low_bank: u8,
    high_bank: u8,
    ram_bank: u8,
    ram_enabled: bool,
    flag: bool,
}

impl Mapper {
    fn write_byte(&mut self, addr: u16, val: u8) {
        let rom_mask = (1 << (((self.num_rom_banks - 1) as f32).log2() as u8 + 1)) - 1;
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
                    let val = val & 0x1f;
                    let bank_num = if val == 0 { 1 } else { val & rom_mask };
                    self.high_bank = (self.high_bank & 0x60) | bank_num;
                }
                0x4000..=0x5FFF => {
                    let val = val & 0b11;
                    self.high_bank = ((val << 5) | (self.high_bank & 0x1f)) & rom_mask;
                    if self.flag {
                        self.ram_bank = val & ram_mask;
                        self.low_bank = (val << 5) & rom_mask;
                    }
                }
                0x6000..=0x7FFF => self.flag = val != 0,
                _ => panic!("Invalid write addr: {}", addr),
            },
        }
    }

    fn get_high_bank(&self) -> u8 {
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
                    .ok_or_else(|| anyhow::anyhow!("Invalid Mapper: {}", mapper))?,
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
        if self.mapper.ram_enabled {
            self.ram[self.mapper.get_ram_bank() as usize * 0x2000 + addr as usize - 0xA000] = val
        }
    }
}

pub struct MemoryBus {
    pub ppu: PPU,
    memory: [u8; 0x10000],
    work_ram: [u8; 0x2000],
    hram: [u8; 0x7f],
    bootrom: Vec<u8>,
    cartridge: Cartridge,
    IE: u8,
    IF: u8,
}

impl MemoryBus {
    pub fn new(bootrom: Vec<u8>, cartridge: Vec<u8>) -> Result<Self> {
        Ok(MemoryBus {
            ppu: PPU::new()?,
            memory: [0; 0x10000],
            work_ram: [0; 0x2000],
            hram: [0; 0x7f],
            bootrom,
            cartridge: Cartridge::new(cartridge)?,
            IE: 0,
            IF: 0,
        })
    }

    pub fn check_interrupts(&mut self) {
        let (vblank, stat) = self.ppu.check_interrupts();
        self.IF |= vblank as u8;
        self.IF |= (stat as u8) << 1;
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x7FFF => {
                if addr < 0x100 && self.read_byte(0xff50) == 0 {
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
            0xFF80..=0xFFEF => self.hram[addr as usize - 0xFF80],

            0xFF40..=0xFF45 | 0xFF47..=0xFF4B => self.ppu.read_byte(addr),
            0xFF4D => 0xFF,
            0xFF0F => self.IF,
            0xFFFF => self.IE,
            _ => self.memory[addr as usize],
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
            0xFF80..=0xFFEF => self.hram[addr as usize - 0xFF80] = val,

            0xFF40..=0xFF45 | 0xFF47 => self.ppu.write_byte(addr, val),
            0xFF0F => self.IF = val,
            0xFFFF => self.IE = val,
            _ => self.memory[addr as usize] = val,
        }
    }

    pub fn read_word(&self, addr: u16) -> u16 {
        ((self.read_byte(addr + 1) as u16) << 8) | (self.read_byte(addr) as u16)
    }

    pub fn write_word(&mut self, addr: u16, val: u16) {
        self.write_byte(addr, val as u8);
        self.write_byte(addr + 1, (val >> 8) as u8)
    }
}
