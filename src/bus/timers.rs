#![allow(non_snake_case)]
use crate::utils::*;
use serde::{Deserialize, Serialize};

#[derive(Default, Serialize, Deserialize)]
pub struct Timers {
    pub DIV: u16,
    TIMA: u8,
    TMA: u8,
    TAC: u8,
    and_result: bool,
    queue_overflow: bool,
    overflow_write: bool,
}

impl Timers {
    pub fn new() -> Self {
        Timers {
            TAC: 0xf8,
            ..Default::default()
        }
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
        match addr {
            0xFF04 => (self.DIV >> 8) as u8,
            0xFF05 => self.TIMA,
            0xFF06 => self.TMA,
            0xFF07 => self.TAC,
            _ => panic!("Invalid timer register read: {:02x}", addr),
        }
    }

    pub fn write_byte(&mut self, addr: u16, val: u8) {
        match addr {
            0xFF04 => self.DIV = 0,
            0xFF05 => {
                if !self.overflow_write {
                    self.TIMA = val;
                    self.queue_overflow = false;
                }
            }
            0xFF06 => self.TMA = val,
            0xFF07 => self.TAC = val | 0xf8,
            _ => panic!("Invalid timer register write: {:02x}", addr),
        }
    }

    // Increment DIV by 4 T-cycles = 1 M-cycle
    pub fn increment(&mut self) -> bool {
        let mut interrupt = false;
        self.DIV = self.DIV.wrapping_add(4);
        let bit_position = match self.TAC & 0b11 {
            0 => 9,
            1 => 3,
            2 => 5,
            3 => 7,
            _ => unreachable!(),
        };
        let new_and_result = self.DIV.bit(bit_position) && self.TAC.bit(2);

        if self.queue_overflow {
            self.queue_overflow = false;
            self.TIMA = self.TMA;
            self.overflow_write = true;
        }
        // increment TIMA on falling edge of AND result
        if self.and_result && !new_and_result {
            let (new_TIMA, c) = self.TIMA.overflowing_add(1);
            self.TIMA = new_TIMA;
            if c {
                self.queue_overflow = true;
                interrupt = true;
            } else {
                self.overflow_write = false;
            }
        }
        self.and_result = new_and_result;
        interrupt
    }
}
