#![allow(non_snake_case)]

#[derive(Default)]
pub struct Timers {
    pub DIV: u16,
    pub TIMA: u8,
    pub TMA: u8,
    pub TAC: u8,
    and_result: bool,
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
            0xFF04 => (self.DIV >> 6) as u8,
            0xFF05 => self.TIMA,
            0xFF06 => self.TMA,
            0xFF07 => self.TAC,
            _ => panic!("Invalid timer register read: {:02x}", addr),
        }
    }

    pub fn write_byte(&mut self, addr: u16, val: u8) {
        match addr {
            0xFF04 => self.DIV = 0,
            0xFF05 => self.TIMA = val,
            0xFF06 => self.TMA = val,
            0xFF07 => self.TAC = val | 0xf8,
            _ => panic!("Invalid timer register write: {:02x}", addr),
        }
    }

    // Increment DIV by 1 M-cycle
    pub fn increment(&mut self) -> bool {
        let mut interrupt = false;
        self.DIV = self.DIV.wrapping_add(1);
        let bit_position = match self.TAC & 0b11 {
            0 => 7,
            1 => 1,
            2 => 3,
            3 => 5,
            _ => unreachable!(),
        };
        let bit = self.DIV & (1 << bit_position) != 0;
        let new_and_result = bit && (self.TAC & 0b100 != 0);

        // increment TIMA falling edge of AND result
        if self.and_result && !new_and_result {
            let (new_TIMA, c) = self.TIMA.overflowing_add(1);
            self.TIMA = new_TIMA;
            if c {
                self.TIMA = self.TMA;
                interrupt = true;
            }
        }
        self.and_result = new_and_result;
        interrupt
    }
}
