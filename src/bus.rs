use anyhow::{anyhow, bail, Result};
use enum_primitive_derive::Primitive;
use num_traits::FromPrimitive;

use crate::gpu::*;

#[derive(Primitive)]
pub enum Mapper {
    ROM = 0,
    MBC1 = 1,
}

pub struct Cartridge {
    pub contents: Vec<u8>,
    pub mapper: Mapper,
    pub num_rom_banks: u16,
    pub ram_size_kb: u8,
}

impl Cartridge {
    fn new(cart: Vec<u8>) -> Result<Self> {
        let mapper = cart[0x147];
        let size = cart[0x148];
        let ram = cart[0x149];
        Ok(Cartridge {
            contents: cart,
            mapper: Mapper::from_u8(mapper).ok_or(anyhow!("Invalid Mapper: {}", mapper))?,
            num_rom_banks: match size {
                0x00..=0x08 => 2 << size,
                0x52 => 72,
                0x53 => 80,
                0x54 => 96,
                _ => bail!("Invalid ROM Size: {:#02x}", size),
            },
            ram_size_kb: match ram {
                0x00 => 0,
                0x01 => 2,
                0x02 => 8,
                0x03 => 32,
                0x04 => 128,
                0x05 => 64,
                _ => bail!("Invalid RAM Size: {:#02x}", ram),
            },
        })
    }
}

pub struct MemoryBus {
    pub memory: [u8; 0x10000],
    pub bootrom: Vec<u8>,
    pub cartridge: Cartridge,
    pub gpu: GPU,
}

impl MemoryBus {
    pub fn new(bootrom: Vec<u8>, cartridge: Vec<u8>) -> Result<Self> {
        Ok(MemoryBus {
            memory: [0; 0x10000],
            bootrom: bootrom,
            cartridge: Cartridge::new(cartridge)?,
            gpu: GPU {},
        })
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
        if addr < 0x8000 {
            if addr < 0x100 && self.read_byte(0xff50) == 0 {
                return self.bootrom[addr as usize];
            }
            self.cartridge.contents[addr as usize]
        } else {
            self.memory[addr as usize]
        }
    }

    pub fn write_byte(&mut self, addr: u16, val: u8) {
        if addr >= 0x8000 {
            self.memory[addr as usize] = val
        }
    }

    pub fn read_bytes(&self, addr: u16, len: u16) -> Vec<u8> {
        let mut slice: Vec<u8> = vec![0; len as usize];
        for i in 0..len {
            slice[i as usize] = self.read_byte(addr + i);
        }
        slice
    }

    pub fn read_word(&self, addr: u16) -> u16 {
        ((self.read_byte(addr + 1) as u16) << 8) | (self.read_byte(addr) as u16)
    }

    pub fn write_word(&mut self, addr: u16, val: u16) {
        self.write_byte(addr, val as u8);
        self.write_byte(addr + 1, (val >> 8) as u8)
    }
}
