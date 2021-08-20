use anyhow::{bail, Result};
use serde::{Deserialize, Serialize};
use std::fs::File;
use std::io::{Read, Write};
use std::path::Path;

use super::rtc::RTC;

#[derive(Serialize, Deserialize)]
pub enum MapperType {
    ROM,
    MBC1,
    MBC1Ram,
    MBC1BattRam,
    MBC2,
    MBC2Batt,
    MBC3,
    MBC3Ram,
    MBC3BattRam,
    MBC3RTC,
    MBC3RamRTC,
    MBC30,
    MBC5,
    MBC5Ram,
    MBC5BattRam,
}

impl MapperType {
    fn from_u8(mapper: u8) -> Result<Self> {
        Ok(match mapper {
            0x00 => MapperType::ROM,
            0x01 => MapperType::MBC1,
            0x02 => MapperType::MBC1Ram,
            0x03 => MapperType::MBC1BattRam,
            0x05 => MapperType::MBC2,
            0x06 => MapperType::MBC2Batt,
            0x0F => MapperType::MBC3RTC,
            0x10 => MapperType::MBC3RamRTC,
            0x11 => MapperType::MBC3,
            0x12 => MapperType::MBC3Ram,
            0x13 => MapperType::MBC3BattRam,
            0x19 => MapperType::MBC5,
            0x1A => MapperType::MBC5Ram,
            0x1B => MapperType::MBC5BattRam,
            _ => bail!("Invalid Mapper: {:#02x}", mapper),
        })
    }
}

#[derive(Serialize, Deserialize)]
pub struct Mapper {
    mapper_type: MapperType,
    num_rom_banks: u16,
    ram_size: u32,
    low_bank: u8,
    high_bank: u16,
    ram_bank: u8,
    ram_enabled: bool,
    flag: bool,

    pub rtc: RTC,
    rtc_enabled: bool,
    prepare_rtc_latch: bool,
}

impl Mapper {
    pub fn write_byte(&mut self, addr: u16, val: u8) {
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
            | MapperType::MBC3RamRTC
            | MapperType::MBC30 => match addr {
                0x0000..=0x1FFF => self.ram_enabled = (val & 0xf) == 0xA,
                0x2000..=0x3FFF => {
                    self.high_bank = if val == 0 { 1 } else { val as u16 };
                }
                0x4000..=0x5FFF => {
                    // MBC3 only uses 32KB of SRAM at most, but MBC30 can use up to 64KB
                    let max_ram_banks = match self.mapper_type {
                        MapperType::MBC30 => 0x08,
                        _ => 0x04,
                    };
                    if val < max_ram_banks {
                        self.ram_bank = val;
                        self.rtc_enabled = false;
                    } else if val >= 0x08 && val <= 0x0C {
                        self.ram_bank = val;
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

#[derive(Serialize, Deserialize)]
pub struct Cartridge {
    pub contents: Vec<u8>,
    pub ram: Vec<u8>,
    pub mapper: Mapper,
}

impl Cartridge {
    pub fn new(cart: Vec<u8>) -> Result<Self> {
        let mapper = cart[0x147];
        let size = cart[0x148];
        let ram = cart[0x149];

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

        let mut mapper_type = MapperType::from_u8(mapper)?;
        match mapper_type {
            MapperType::MBC2 | MapperType::MBC2Batt => {
                ram_init_val = 0xf0;
                ram_size = 4096;
            }
            MapperType::MBC3
            | MapperType::MBC3Ram
            | MapperType::MBC3BattRam
            | MapperType::MBC3RTC
            | MapperType::MBC3RamRTC => {
                if num_rom_banks > 128 || ram_size > 0x8000 {
                    mapper_type = MapperType::MBC30;
                }
            }
            _ => {}
        }
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
                self.mapper.rtc.read_byte(self.mapper.ram_bank)
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
                self.mapper.rtc.write_byte(self.mapper.ram_bank, val);
            } else if let MapperType::MBC2 | MapperType::MBC2Batt = self.mapper.mapper_type {
                self.ram[addr % 0x200] = val | 0xf0;
            } else if addr < self.mapper.ram_size as usize {
                self.ram[self.mapper.get_ram_bank() as usize * 0x2000 + addr] = val;
            }
        }
    }

    pub fn write_rom_byte(&mut self, addr: u16, val: u8) {
        self.mapper.write_byte(addr, val);
    }

    pub fn increment_rtc(&mut self) {
        match self.mapper.mapper_type {
            MapperType::MBC3RTC | MapperType::MBC3RamRTC => self.mapper.rtc.increment(),
            _ => {}
        }
    }

    pub fn load_external_ram(&mut self, filename: &Path) -> Result<()> {
        match self.mapper.mapper_type {
            MapperType::MBC1BattRam
            | MapperType::MBC2Batt
            | MapperType::MBC3BattRam
            | MapperType::MBC3RamRTC
            | MapperType::MBC5BattRam => {
                if let Ok(mut file) = File::open(filename) {
                    file.read_exact(&mut self.ram)?;
                    let mut rtc_data = Vec::new();
                    file.read_to_end(&mut rtc_data)?;
                    self.mapper.rtc = bincode::deserialize(&rtc_data)?;
                }
            }
            _ => {}
        };
        Ok(())
    }

    pub fn save_external_ram(&self, filename: &Path) -> Result<()> {
        match self.mapper.mapper_type {
            MapperType::MBC1BattRam
            | MapperType::MBC2Batt
            | MapperType::MBC3BattRam
            | MapperType::MBC3RamRTC
            | MapperType::MBC5BattRam => {
                let dir = filename.parent().unwrap();
                std::fs::create_dir_all(dir)?;
                let mut file = File::create(filename)?;
                file.write_all(&self.ram)?;
                file.write_all(&bincode::serialize(&self.mapper.rtc)?)?;
            }
            _ => {}
        };
        Ok(())
    }
}
