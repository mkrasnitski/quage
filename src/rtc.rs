#[derive(Default, Debug, Clone)]
struct RTCState {
    seconds: u8,
    minutes: u8,
    hours: u8,
    days: u16,
}

#[derive(Default)]
pub struct RTC {
    carry: bool,
    halted: bool,
    cycles: u8,
    ticks: u16,
    internal_state: RTCState,
    latched_state: RTCState,
}

impl RTC {
    pub fn read_byte(&self, reg: u8) -> u8 {
        match reg {
            0x08 => self.latched_state.seconds,
            0x09 => self.latched_state.minutes,
            0x0A => self.latched_state.hours,
            0x0B => self.latched_state.days as u8,
            0x0C => {
                ((self.carry as u8) << 7)
                    | ((self.halted as u8) << 6)
                    | ((self.latched_state.days >> 8) as u8)
            }
            _ => panic!("Invalid RTC read: {}", reg),
        }
    }

    pub fn write_byte(&mut self, reg: u8, val: u8) {
        match reg {
            0x08 => {
                self.cycles = 0;
                self.ticks = 0;
                self.latched_state.seconds = val & 0x3F;
            }
            0x09 => self.latched_state.minutes = val & 0x3F,
            0x0A => self.latched_state.hours = val & 0x1F,
            0x0B => self.latched_state.days = (self.latched_state.days & 0b100000000) | val as u16,
            0x0C => {
                self.latched_state.days =
                    (self.latched_state.days & 0xff) | ((val as u16 & 1) << 8);
                self.halted = val & (1 << 6) != 0;
                self.carry = val & (1 << 7) != 0;
            }
            _ => panic!("Invalid RTC write: {}", reg),
        };
        self.internal_state = self.latched_state.clone();
    }

    pub fn update_latched_state(&mut self) {
        self.latched_state = self.internal_state.clone();
    }

    pub fn increment(&mut self) {
        if !self.halted {
            self.cycles += 4;
            if self.cycles == 128 {
                self.cycles = 0;
                self.ticks += 1;
            };

            if self.ticks == 32768 {
                self.ticks = 0;
                self.internal_state.seconds = (self.internal_state.seconds + 1) & 0x3F;
                if self.internal_state.seconds == 60 {
                    self.internal_state.seconds = 0;
                    self.internal_state.minutes = (self.internal_state.minutes + 1) & 0x3F;
                    if self.internal_state.minutes == 60 {
                        self.internal_state.minutes = 0;
                        self.internal_state.hours = (self.internal_state.hours + 1) & 0x1F;
                        if self.internal_state.hours == 24 {
                            self.internal_state.hours = 0;
                            self.internal_state.days += 1;
                            if self.internal_state.days == 512 {
                                self.internal_state.days = 0;
                                self.carry = true;
                            };
                        };
                    };
                };
            };
        }
    }
}
