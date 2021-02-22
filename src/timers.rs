#![allow(non_snake_case)]

#[derive(Default)]
pub struct Timers {
    pub DIV: u8,
    pub TIMA: u8,
    pub TMA: u8,
    pub TAC: u8,
    cycles: u64,
}

impl Timers {
    pub fn read_byte(&self, addr: u16) -> u8 {
        match addr {
            0xFF04 => self.DIV,
            0xFF05 => self.TIMA,
            0xFF06 => self.TMA,
            0xFF07 => self.TAC,
            _ => panic!("Invalid timer register read: {:02x}", addr),
        }
    }

    pub fn write_byte(&mut self, addr: u16, val: u8) {
        match addr {
            0xFF04 => self.DIV = val,
            0xFF05 => self.TIMA = val,
            0xFF06 => self.TMA = val,
            0xFF07 => self.TAC = val,
            _ => panic!("Invalid timer register write: {:02x}", addr),
        }
    }

    pub fn increment(&mut self, cycles_passed: u64) -> bool {
        let mut interrupt = false;
        let total_cycles = self.cycles + cycles_passed;
        let div_clocks = (total_cycles / 256 - self.cycles / 256) as u8;
        self.DIV = self.DIV.wrapping_add(div_clocks);
        if self.TAC & 0b100 != 0 {
            let clock = match self.TAC & 0b11 {
                0 => 1024,
                1 => 16,
                2 => 64,
                3 => 256,
                _ => unreachable!(),
            };

            let timer_clocks = (total_cycles / clock - self.cycles / clock) as u8;
            let (new_TIMA, c) = self.TIMA.overflowing_add(timer_clocks);
            if c {
                self.TIMA = self.TMA;
                interrupt = true;
            } else {
                self.TIMA = new_TIMA;
            }
        }
        self.cycles += cycles_passed;
        interrupt
    }
}
