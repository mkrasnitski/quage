#[derive(Default)]
struct SoundRegisters {
    ch1_freq_sweep: u8,
    ch1_length_duty: u8,
    ch1_volume: u8,
    ch1_freq_lo: u8,
    ch1_freq_hi: u8,

    ch2_length_duty: u8,
    ch2_volume: u8,
    ch2_freq_lo: u8,
    ch2_freq_hi: u8,

    ch3_enable: u8,
    ch3_length: u8,
    ch3_level: u8,
    ch3_freq_lo: u8,
    ch3_freq_hi: u8,

    ch4_length: u8,
    ch4_volume: u8,
    ch4_poly_ctr: u8,
    ch4_ctr_cons: u8,

    ch_control: u8,
    output_select: u8,
    sound_enable: u8,
}

pub struct Sound {
    registers: SoundRegisters,
    wave_ram: [u8; 0x10],
}

impl Sound {
    pub fn new() -> Self {
        Sound {
            registers: SoundRegisters {
                ch1_freq_sweep: 0x80,
                ch3_enable: 0x7F,
                ch3_level: 0x9F,
                ch4_length: 0xC0,
                ch4_ctr_cons: 0x3F,
                sound_enable: 0x70,
                ..Default::default()
            },
            wave_ram: [0; 0x10],
        }
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
        match addr {
            0xFF10 => self.registers.ch1_freq_sweep,
            0xFF11 => self.registers.ch1_length_duty,
            0xFF12 => self.registers.ch1_volume,
            0xFF13 => self.registers.ch1_freq_lo,
            0xFF14 => self.registers.ch1_freq_hi,

            0xFF16 => self.registers.ch2_length_duty,
            0xFF17 => self.registers.ch2_volume,
            0xFF18 => self.registers.ch2_freq_lo,
            0xFF19 => self.registers.ch2_freq_hi,

            0xFF1A => self.registers.ch3_enable,
            0xFF1B => self.registers.ch3_length,
            0xFF1C => self.registers.ch3_level,
            0xFF1D => self.registers.ch3_freq_lo,
            0xFF1E => self.registers.ch3_freq_hi,

            0xFF20 => self.registers.ch4_length,
            0xFF21 => self.registers.ch4_volume,
            0xFF22 => self.registers.ch4_poly_ctr,
            0xFF23 => self.registers.ch4_ctr_cons,

            0xFF24 => self.registers.ch_control,
            0xFF25 => self.registers.output_select,
            0xFF26 => self.registers.sound_enable,

            0xFF30..=0xFF3F => self.wave_ram[addr as usize - 0xFF30],
            _ => panic!("Invalid Sound register read: {:04x}", addr),
        }
    }

    pub fn write_byte(&mut self, addr: u16, val: u8) {
        match addr {
            0xFF10 => self.registers.ch1_freq_sweep = val | 0x80,
            0xFF11 => self.registers.ch1_length_duty = val,
            0xFF12 => self.registers.ch1_volume = val,
            0xFF13 => self.registers.ch1_freq_lo = val,
            0xFF14 => self.registers.ch1_freq_hi = val,

            0xFF16 => self.registers.ch2_length_duty = val,
            0xFF17 => self.registers.ch2_volume = val,
            0xFF18 => self.registers.ch2_freq_lo = val,
            0xFF19 => self.registers.ch2_freq_hi = val,

            0xFF1A => self.registers.ch3_enable = val | 0x7F,
            0xFF1B => self.registers.ch3_length = val,
            0xFF1C => self.registers.ch3_level = val | 0x9F,
            0xFF1D => self.registers.ch3_freq_lo = val,
            0xFF1E => self.registers.ch3_freq_hi = val,

            0xFF20 => self.registers.ch4_length = val | 0xC0,
            0xFF21 => self.registers.ch4_volume = val,
            0xFF22 => self.registers.ch4_poly_ctr = val,
            0xFF23 => self.registers.ch4_ctr_cons = val | 0x3F,

            0xFF24 => self.registers.ch_control = val,
            0xFF25 => self.registers.output_select = val,
            0xFF26 => self.registers.sound_enable = val | 0x70,

            0xFF30..=0xFF3F => self.wave_ram[addr as usize - 0xFF30] = val,
            _ => panic!("Invalid Sound register write: {:04x} {:02x}", addr, val),
        }
    }
}
