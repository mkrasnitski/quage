#![allow(non_snake_case)]
use anyhow::{bail, Result};
use enum_primitive_derive::Primitive;
use num_traits::FromPrimitive;
use std::fs::File;
use std::io::{Read, Write};
use std::path::Path;

use crate::display::*;
use crate::joypad::*;
use crate::ppu::*;
use crate::rtc::*;
use crate::sound::*;
use crate::timers::*;

#[derive(Primitive)]
enum MapperType {
    ROM = 0x00,
    MBC1 = 0x01,
    MBC1Ram = 0x02,
    MBC1BattRam = 0x03,
    MBC2 = 0x05,
    MBC2Batt = 0x06,
    MBC3RTC = 0x0F,
    MBC3RamRTC = 0x10,
    MBC3 = 0x11,
    MBC3Ram = 0x12,
    MBC3BattRam = 0x13,
    MBC5 = 0x19,
    MBC5Ram = 0x1A,
    MBC5BattRam = 0x1B,
}

struct Mapper {
    mapper_type: MapperType,
    num_rom_banks: u16,
    ram_size: u32,
    low_bank: u8,
    high_bank: u16,
    ram_bank: u8,
    ram_enabled: bool,
    flag: bool,

    rtc: RTC,
    rtc_enabled: bool,
    rtc_register: u8,
    prepare_rtc_latch: bool,
}

impl Mapper {
    fn write_byte(&mut self, addr: u16, val: u8) {
        let num_ram_banks = (self.ram_size as f64 / 8192.0).ceil() as u8;
        match self.mapper_type {
            MapperType::ROM => (),
            MapperType::MBC1 | MapperType::MBC1Ram | MapperType::MBC1BattRam => match addr {
                0x0000..=0x1FFF => self.ram_enabled = (val & 0xf) == 0xA,
                0x2000..=0x3FFF => {
                    let val = val as u16 & 0x1f;
                    let bank_num = if val == 0 {
                        1
                    } else {
                        val % self.num_rom_banks
                    };
                    self.high_bank = (self.high_bank & 0x60) | bank_num as u16;
                }
                0x4000..=0x5FFF => {
                    let val = val & 0b11;
                    self.high_bank =
                        (((val << 5) as u16) | (self.high_bank & 0x1f)) % self.num_rom_banks;
                    if self.flag {
                        if self.ram_size > 0 {
                            self.ram_bank = val % num_ram_banks;
                        }
                        self.low_bank = (val << 5) % self.num_rom_banks as u8;
                    }
                }
                0x6000..=0x7FFF => self.flag = val != 0,
                _ => panic!("Invalid MBC1 write addr: {:#04x}", addr),
            },
            MapperType::MBC2 | MapperType::MBC2Batt => match addr {
                0x0000..=0x3FFF => {
                    if (addr >> 8) & 1 == 0 {
                        self.ram_enabled = (val & 0xf) == 0xA;
                    } else {
                        self.high_bank = if val & 0xF == 0 {
                            1
                        } else {
                            (val as u16) % self.num_rom_banks
                        };
                    }
                }
                0x4000..=0x7FFF => {}
                _ => panic!("Invalid MBC2 write addr: {:#04x}", addr),
            },
            MapperType::MBC3
            | MapperType::MBC3Ram
            | MapperType::MBC3BattRam
            | MapperType::MBC3RTC
            | MapperType::MBC3RamRTC => match addr {
                0x0000..=0x1FFF => self.ram_enabled = (val & 0xf) == 0xA,
                0x2000..=0x3FFF => {
                    let val = (val as u16 & 0x7f) % self.num_rom_banks;
                    self.high_bank = if val == 0 { 1 } else { val };
                }
                0x4000..=0x5FFF => {
                    if val < 0x04 {
                        self.ram_bank = val;
                        self.rtc_enabled = false;
                    } else if val >= 0x08 && val <= 0x0C {
                        self.rtc_register = val;
                        self.rtc_enabled = true;
                    } else {
                        panic!("Invalid ram bank {:02x}", val);
                    }
                }
                0x6000..=0x7FFF => {
                    if val == 0 && !self.prepare_rtc_latch {
                        self.prepare_rtc_latch = true;
                    }
                    if val == 1 && self.prepare_rtc_latch {
                        self.prepare_rtc_latch = false;
                        self.rtc.update_latched_state();
                    }
                }
                _ => panic!("Invalid MBC3 write addr: {:#04x}", addr),
            },
            MapperType::MBC5 | MapperType::MBC5Ram | MapperType::MBC5BattRam => match addr {
                0x0000..=0x1FFF => self.ram_enabled = (val & 0xf) == 0xA,
                0x2000..=0x2FFF => {
                    self.high_bank = ((self.high_bank & 0x100) | val as u16) % self.num_rom_banks
                }
                0x3000..=0x3FFF => {
                    self.high_bank =
                        (((val as u16 & 1) << 8) | (self.high_bank & 0xff)) % self.num_rom_banks
                }
                0x4000..=0x5FFF => self.ram_bank = val % num_ram_banks,
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

struct Cartridge {
    contents: Vec<u8>,
    ram: Vec<u8>,
    mapper: Mapper,
}

impl Cartridge {
    fn new(cart: Vec<u8>) -> Result<Self> {
        let mapper = cart[0x147];
        let size = cart[0x148];
        let ram = cart[0x149];

        let mapper_type = MapperType::from_u8(mapper)
            .ok_or_else(|| anyhow::anyhow!("Invalid Mapper: {:#02x}", mapper))?;
        let num_rom_banks = match size {
            0x00..=0x08 => 2 << size,
            _ => bail!("Invalid ROM Size: {:#02x}", size),
        };

        let mut ram_init_val = 0;
        let mut ram_size = 1024
            * match ram {
                0x00 => 0,
                0x01 => 2,
                0x02 => 8,
                0x03 => 32,
                0x04 => 128,
                0x05 => 64,
                _ => bail!("Invalid RAM Size: {:#02x}", ram),
            };
        if let MapperType::MBC2 | MapperType::MBC2Batt = mapper_type {
            ram_init_val = 0xf0;
            ram_size = 4096;
        };
        Ok(Cartridge {
            contents: cart,
            ram: vec![ram_init_val; ram_size as usize],
            mapper: Mapper {
                mapper_type,
                num_rom_banks,
                ram_size,
                low_bank: 0,
                high_bank: 1,
                ram_bank: 0,
                flag: false,

                rtc: RTC::default(),
                ram_enabled: false,
                rtc_enabled: false,
                rtc_register: 0,
                prepare_rtc_latch: false,
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
        let addr = addr as usize - 0xA000;
        if self.mapper.ram_enabled {
            if self.mapper.rtc_enabled {
                self.mapper.rtc.read_byte(self.mapper.rtc_register)
            } else if let MapperType::MBC2 | MapperType::MBC2Batt = self.mapper.mapper_type {
                self.ram[addr % 0x200]
            } else if addr < self.mapper.ram_size as usize {
                self.ram[self.mapper.get_ram_bank() as usize * 0x2000 + addr]
            } else {
                0xFF
            }
        } else {
            0xFF
        }
    }

    pub fn write_ram_byte(&mut self, addr: u16, val: u8) {
        let addr = addr as usize - 0xA000;
        if self.mapper.ram_enabled {
            if self.mapper.rtc_enabled {
                self.mapper.rtc.write_byte(self.mapper.rtc_register, val);
            } else if let MapperType::MBC2 | MapperType::MBC2Batt = self.mapper.mapper_type {
                self.ram[addr % 0x200] = val | 0xf0;
            } else if addr < self.mapper.ram_size as usize {
                self.ram[self.mapper.get_ram_bank() as usize * 0x2000 + addr] = val;
            }
        }
    }
}

pub struct MemoryBus {
    pub ppu: PPU,
    pub timers: Timers,
    pub joypad: Joypad,
    pub sound: Sound,
    work_ram: [u8; 0x2000],
    hram: [u8; 0x7f],
    bootrom: Vec<u8>,
    cartridge: Cartridge,
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

    pub fn increment_rtc(&mut self) {
        match self.cartridge.mapper.mapper_type {
            MapperType::MBC3RTC | MapperType::MBC3RamRTC => self.cartridge.mapper.rtc.increment(),
            _ => {}
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

    pub fn load_external_ram(&mut self, filename: &Path) {
        match self.cartridge.mapper.mapper_type {
            MapperType::MBC1BattRam
            | MapperType::MBC2Batt
            | MapperType::MBC3BattRam
            | MapperType::MBC3RamRTC
            | MapperType::MBC5BattRam => {
                if let Ok(mut file) = File::open(filename) {
                    file.read_exact(&mut self.cartridge.ram).unwrap();
                }
            }
            _ => {}
        }
    }

    pub fn save_external_ram(&self, filename: &Path) {
        match self.cartridge.mapper.mapper_type {
            MapperType::MBC1BattRam
            | MapperType::MBC2Batt
            | MapperType::MBC3BattRam
            | MapperType::MBC3RamRTC
            | MapperType::MBC5BattRam => {
                File::create(filename)
                    .unwrap()
                    .write_all(&self.cartridge.ram)
                    .unwrap();
            }
            _ => {}
        }
    }
}
