#![allow(non_snake_case)]
use anyhow::Result;
use sdl2::pixels::Color;

use crate::display::*;

struct PPUInterrupts {
    vblank: bool,
    stat: bool,
}

struct PPURegisters {
    LCDC: u8,
    LY: u8,
    LYC: u8,
    STAT: u8,
    SCY: u8,
    SCX: u8,
    WY: u8,
    WX: u8,
    WC: u8,
    BGP: u8,
    OBP0: u8,
    OBP1: u8,
    interrupts: PPUInterrupts,
}

impl PPURegisters {
    pub fn new() -> Self {
        PPURegisters {
            LCDC: 0,
            LY: 0,
            LYC: 0,
            STAT: 0x80,
            SCY: 0,
            SCX: 0,
            WY: 0,
            WX: 0,
            WC: 0,
            BGP: 0,
            OBP0: 0,
            OBP1: 0,
            interrupts: PPUInterrupts {
                vblank: false,
                stat: false,
            },
        }
    }
}

pub struct PPU {
    pub memory: [u8; 0x2000],
    pub oam: [u8; 0xA0],
    viewport: [[Color; W_WIDTH]; W_HEIGHT],
    registers: PPURegisters,
    display: Display,
    enable_display_events: bool,
    block_stat_irqs: bool,
    cycles: u64,
}

impl PPU {
    pub fn new() -> Result<Self> {
        Ok(PPU {
            memory: [0; 0x2000],
            oam: [0; 0xA0],
            viewport: [[Color::WHITE; W_WIDTH]; W_HEIGHT],
            registers: PPURegisters::new(),
            display: Display::new()?,
            enable_display_events: false,
            block_stat_irqs: false,
            cycles: 0,
        })
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
        match addr {
            0x8000..=0x9FFF => self.memory[addr as usize - 0x8000],
            0xFE00..=0xFE9F => self.oam[addr as usize - 0xFE00],
            0xFF40 => self.registers.LCDC,
            0xFF41 => self.registers.STAT,
            0xFF42 => self.registers.SCY,
            0xFF43 => self.registers.SCX,
            0xFF44 => self.registers.LY,
            0xFF45 => self.registers.LYC,
            0xFF47 => self.registers.BGP,
            0xFF48 => self.registers.OBP0,
            0xFF49 => self.registers.OBP1,
            0xFF4A => self.registers.WY,
            0xFF4B => self.registers.WX,
            _ => panic!("Invalid PPU Register read: {:04x}", addr),
        }
    }

    pub fn write_byte(&mut self, addr: u16, val: u8) {
        match addr {
            0x8000..=0x9FFF => self.memory[addr as usize - 0x8000] = val,
            0xFE00..=0xFE9F => self.oam[addr as usize - 0xFE00] = val,
            0xFF40 => self.registers.LCDC = val,
            0xFF41 => {
                self.registers.STAT |= val & !7;
                // println!("STAT {:08b} {:08b}", val, self.registers.STAT);
            }
            0xFF42 => self.registers.SCY = val,
            0xFF43 => self.registers.SCX = val,
            0xFF45 => self.registers.LYC = val,
            0xFF47 => self.registers.BGP = val,
            0xFF48 => self.registers.OBP0 = val,
            0xFF49 => self.registers.OBP1 = val,
            0xFF4A => {
                self.registers.WY = val;
                // println!("WY {}", val);
            }
            0xFF4B => {
                self.registers.WX = val;
                // println!("WX {}", val);
            }
            _ => panic!("Invalid PPU Register write: {:04x}", addr),
        }
    }

    pub fn poll_display_event(&mut self) -> DisplayEvent {
        if self.enable_display_events {
            self.enable_display_events = false;
            self.display.poll_event()
        } else {
            DisplayEvent::None
        }
    }

    pub fn check_interrupts(&mut self) -> (bool, bool) {
        let res = (
            self.registers.interrupts.vblank,
            self.registers.interrupts.stat,
        );
        self.registers.interrupts.vblank = false;
        self.registers.interrupts.stat = false;
        res
    }

    pub fn draw(&mut self, cycles_passed: u64) {
        for _ in 0..cycles_passed / 4 {
            self.cycles += 4;
            if self.cycles > 70224 {
                self.cycles -= 70224;
                self.enable_display_events = true;
                self.display.draw(self.viewport);
            }
            self.step();
        }
    }

    fn step(&mut self) {
        let clocks = self.cycles % 456;
        let scanline = (self.cycles / 456) as u8;

        // Start of a line
        if scanline != self.registers.LY {
            self.block_stat_irqs = false;
        }
        self.registers.LY = scanline;

        // Check for LY = LYC
        let coincidence = self.registers.LY == self.registers.LYC;
        if coincidence {
            self.req_stat_interrupt(6);
        }
        self.registers.STAT |= (coincidence as u8) << 2;

        // PPU Mode switching
        if self.registers.LY < 144 {
            match clocks {
                0 => self.set_mode(2), // OAM Search
                80 => {
                    self.set_mode(3); // Drawing
                    self.draw_line();
                }
                252 => self.set_mode(0), // H-blank
                _ => {}
            }
        }
        // V-blank
        else if self.registers.LY == 144 && clocks == 0 {
            self.registers.interrupts.vblank = true;
            self.set_mode(1);
        }
    }

    fn set_mode(&mut self, mode: u8) {
        self.registers.STAT &= !0b11;
        self.registers.STAT |= mode & 0b11;
        if mode < 3 {
            self.req_stat_interrupt(mode + 3);
        }
    }

    fn get_mode(&self) -> u8 {
        self.registers.STAT & 0b11
    }

    fn req_stat_interrupt(&mut self, bit: u8) {
        if !self.block_stat_irqs
            && (self.registers.STAT & (1 << bit)) != 0
            && (3..=6).contains(&bit)
        {
            self.block_stat_irqs = true;
            self.registers.interrupts.stat = true;
        }
    }

    fn draw_line(&mut self) {
        // LCD Enable
        if self.registers.LCDC & (1 << 7) != 0 {
            // bg/win enable
            if self.registers.LCDC & 1 != 0 {
                let bg_tilemap = match self.registers.LCDC & (1 << 3) != 0 {
                    true => 0x9C00,
                    false => 0x9800,
                };
                let bg_y = self.registers.SCY.wrapping_add(self.registers.LY);
                for i in 0u8..32 {
                    let bg_tile_num =
                        self.read_byte(bg_tilemap + 32 * ((bg_y / 8) as u16) + i as u16);
                    let tile_row = self.decode_tile_row(bg_tile_num, bg_y % 8);
                    for j in 0u8..8 {
                        let x = (8 * i + j).wrapping_sub(self.registers.SCX) as usize;
                        if x < W_WIDTH {
                            self.viewport[self.registers.LY as usize][x] =
                                self.decode_palette(tile_row[j as usize]);
                        }
                    }
                }
            }
            if self.registers.LCDC & (1 << 5) != 0 {
                // TODO: Window rendering
            }
        }
        // TODO: Sprite rendering
    }

    fn decode_tile_row(&self, tile_num: u8, row_num: u8) -> [u8; 8] {
        let mut row = [0; 8];
        let tile_row_offset = if self.registers.LCDC & (1 << 4) == 0 && tile_num <= 0x80 {
            0x9000
        } else {
            0x8000
        } + tile_num as u16 * 16
            + 2 * row_num as u16;
        let hi = self.read_byte(tile_row_offset + 1);
        let lo = self.read_byte(tile_row_offset);
        for i in 0..8 {
            row[7 - i] = (((hi >> i) & 1) << 1) | ((lo >> i) & 1);
        }
        row
    }

    fn decode_palette(&self, color: u8) -> Color {
        let color = (self.registers.BGP >> (2 * color)) & 0b11;
        match color {
            0 => Color::WHITE,
            1 => Color::RGB(0xaa, 0xaa, 0xaa),
            2 => Color::RGB(0x55, 0x55, 0x55),
            3 => Color::BLACK,
            _ => panic!("Incorrect palette color: {}", color),
        }
    }
}
