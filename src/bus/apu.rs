use crate::utils::*;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
struct Channel1 {
    nr10: u8,
    nr11: u8,
    nr12: u8,
    nr13: u8,
    nr14: u8,
}

#[derive(Serialize, Deserialize)]
struct Channel2 {
    nr21: u8,
    nr22: u8,
    nr23: u8,
    nr24: u8,
}

#[derive(Serialize, Deserialize)]
struct Channel3 {
    nr30: u8,
    nr31: u8,
    nr32: u8,
    nr33: u8,
    nr34: u8,
}

#[derive(Serialize, Deserialize)]
struct Channel4 {
    nr41: u8,
    nr42: u8,
    nr43: u8,
    nr44: u8,
}

#[derive(Serialize, Deserialize)]
struct APURegisters {
    ch1: Channel1,
    ch2: Channel2,
    ch3: Channel3,
    ch4: Channel4,
    nr50: u8,
    nr51: u8,
    nr52: u8,
}

impl Default for APURegisters {
    fn default() -> Self {
        APURegisters {
            ch1: Channel1 {
                nr10: 0x80,
                nr11: 0x00,
                nr12: 0x00,
                nr13: 0x00,
                nr14: 0x38,
            },
            ch2: Channel2 {
                nr21: 0x00,
                nr22: 0x00,
                nr23: 0x00,
                nr24: 0x38,
            },
            ch3: Channel3 {
                nr30: 0x7F,
                nr31: 0x00,
                nr32: 0x9F,
                nr33: 0x00,
                nr34: 0x38,
            },
            ch4: Channel4 {
                nr41: 0xC0,
                nr42: 0x00,
                nr43: 0x00,
                nr44: 0x3F,
            },
            nr50: 0x00,
            nr51: 0x00,
            nr52: 0x70,
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct APU {
    registers: APURegisters,
    wave_ram: [u8; 0x10],
}

impl APU {
    pub fn new() -> Self {
        APU {
            registers: APURegisters::default(),
            wave_ram: [0; 0x10],
        }
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
        match addr {
            0xFF10 => self.registers.ch1.nr10,
            0xFF11 => self.registers.ch1.nr11 | 0x3F,
            0xFF12 => self.registers.ch1.nr12,
            0xFF13 => self.registers.ch1.nr13 | 0xFF,
            0xFF14 => self.registers.ch1.nr14 | 0x87,

            0xFF16 => self.registers.ch2.nr21 | 0x3F,
            0xFF17 => self.registers.ch2.nr22,
            0xFF18 => self.registers.ch2.nr23 | 0xFF,
            0xFF19 => self.registers.ch2.nr24 | 0x87,

            0xFF1A => self.registers.ch3.nr30,
            0xFF1B => self.registers.ch3.nr31 | 0xFF,
            0xFF1C => self.registers.ch3.nr32,
            0xFF1D => self.registers.ch3.nr33 | 0xFF,
            0xFF1E => self.registers.ch3.nr34 | 0x87,

            0xFF20 => self.registers.ch4.nr41 | 0x3F,
            0xFF21 => self.registers.ch4.nr42,
            0xFF22 => self.registers.ch4.nr43,
            0xFF23 => self.registers.ch4.nr44 | 0x80,

            0xFF24 => self.registers.nr50,
            0xFF25 => self.registers.nr51,
            0xFF26 => self.registers.nr52,

            0xFF30..=0xFF3F => self.wave_ram[addr as usize - 0xFF30],
            _ => panic!("Invalid APU register read: {:04x}", addr),
        }
    }

    pub fn write_byte(&mut self, addr: u16, val: u8) {
        if self.registers.nr52.bit(7) || addr == 0xFF26 {
            match addr {
                0xFF10 => self.registers.ch1.nr10 = val | 0x80,
                0xFF11 => self.registers.ch1.nr11 = val,
                0xFF12 => self.registers.ch1.nr12 = val,
                0xFF13 => self.registers.ch1.nr13 = val,
                0xFF14 => self.registers.ch1.nr14 = val | 0x38,

                0xFF16 => self.registers.ch2.nr21 = val,
                0xFF17 => self.registers.ch2.nr22 = val,
                0xFF18 => self.registers.ch2.nr23 = val,
                0xFF19 => self.registers.ch2.nr24 = val | 0x38,

                0xFF1A => self.registers.ch3.nr30 = val | 0x7F,
                0xFF1B => self.registers.ch3.nr31 = val,
                0xFF1C => self.registers.ch3.nr32 = val | 0x9F,
                0xFF1D => self.registers.ch3.nr33 = val,
                0xFF1E => self.registers.ch3.nr34 = val | 0x38,

                0xFF20 => self.registers.ch4.nr41 = val | 0xC0,
                0xFF21 => self.registers.ch4.nr42 = val,
                0xFF22 => self.registers.ch4.nr43 = val,
                0xFF23 => self.registers.ch4.nr44 = val | 0x3F,

                0xFF24 => self.registers.nr50 = val,
                0xFF25 => self.registers.nr51 = val,
                0xFF26 => {
                    if !val.bit(7) {
                        self.registers = APURegisters::default();
                    }
                    self.registers.nr52 = (val | 0x70) & 0xF0;
                }

                0xFF30..=0xFF3F => self.wave_ram[addr as usize - 0xFF30] = val,
                _ => panic!("Invalid APU register write: {:04x} {:02x}", addr, val),
            }
        }
    }
}
