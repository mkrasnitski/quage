#![allow(non_snake_case)]
use anyhow::Result;
use sdl2::pixels::Color;

use crate::display::*;

struct PPURegisters {
    LCDC: u8,
    LY: u8,
    LYC: u8,
    STAT: u8,
    SCY: u8,
    SCX: u8,
    BGP: u8,
}

impl PPURegisters {
    fn new() -> Self {
        PPURegisters {
            LCDC: 0,
            LY: 0,
            LYC: 0,
            STAT: 0,
            SCY: 0,
            SCX: 0,
            BGP: 0,
        }
    }
}

pub struct PPU {
    pub memory: [u8; 0x2000],
    registers: PPURegisters,
    display: Display,
    draw_call: bool,
    cycles: u64,
}

impl PPU {
    pub fn new() -> Result<Self> {
        Ok(PPU {
            memory: [0; 0x2000],
            registers: PPURegisters::new(),
            display: Display::new()?,
            draw_call: false,
            cycles: 0,
        })
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
        match addr {
            0x8000..=0x9FFF => self.memory[addr as usize - 0x8000],
            0xFF40 => self.registers.LCDC,
            0xFF41 => self.registers.STAT,
            0xFF42 => self.registers.SCY,
            0xFF43 => self.registers.SCX,
            0xFF44 => self.registers.LY,
            0xFF45 => self.registers.LYC,
            0xFF47 => self.registers.BGP,
            _ => panic!("Invalid PPU Register read: {:04x}", addr),
        }
    }

    pub fn write_byte(&mut self, addr: u16, val: u8) {
        match addr {
            0x8000..=0x9FFF => self.memory[addr as usize - 0x8000] = val,
            0xFF40 => self.registers.LCDC = val,
            0xFF41 => self.registers.STAT = val,
            0xFF42 => self.registers.SCY = val,
            0xFF43 => self.registers.SCX = val,
            0xFF45 => self.registers.LYC = val,
            0xFF47 => self.registers.BGP = val,
            _ => panic!("Invalid PPU Register write: {:04x}", addr),
        }
    }

    pub fn draw(&mut self, cycles_passed: u64) {
        self.cycles += cycles_passed;
        if self.cycles > 70224 {
            self.cycles -= 70224;
            self.draw_call = true;
            self.render_display();
        } else {
            self.draw_call = false;
        }
        self.registers.LY = (self.cycles / 456) as u8;
    }

    pub fn poll_display_event(&mut self) -> DisplayEvent {
        if self.draw_call {
            self.display.poll_event()
        } else {
            DisplayEvent::None
        }
    }

    fn render_display(&mut self) {
        let mut colors = [[Color::WHITE; 256]; 256];
        for x in 0..32 {
            for y in 0..32 {
                let tile = self.decode_tile(self.read_byte(0x9800 + 32 * x as u16 + y as u16));
                for i in 0..8 {
                    for j in 0..8 {
                        let color = (self.registers.BGP >> 2 * tile[i][j]) & 0b11;
                        colors[8 * x + i][8 * y + j] = self.decode_palette(color);
                    }
                }
            }
        }
        let mut viewport = [[Color::WHITE; W_WIDTH]; W_HEIGHT];
        for i in 0..W_WIDTH {
            for j in 0..W_HEIGHT {
                viewport[j][i] = colors[(self.registers.SCY as usize + j) % 256]
                    [(self.registers.SCX as usize + i) % 256];
            }
        }
        self.display.draw(viewport);
    }

    fn decode_tile(&self, tile_num: u8) -> [[u8; 8]; 8] {
        let mut tile = [[0; 8]; 8];
        let base = 0x8000;
        let tile_offset = base + tile_num as u16 * 16;
        for i in 0..8 {
            let hi = self.read_byte(tile_offset + 2 * i + 1);
            let lo = self.read_byte(tile_offset + 2 * i);
            for j in 0..8 {
                tile[i as usize][7 - j] = (((hi >> j) & 1) << 1) | ((lo >> j) & 1);
            }
        }
        tile
    }

    fn decode_palette(&self, color: u8) -> Color {
        match color {
            0 => Color::WHITE,
            1 => Color::RGB(0x55, 0x55, 0x55),
            2 => Color::RGB(0xaa, 0xaa, 0xaa),
            3 => Color::BLACK,
            _ => panic!("Incorrect palette color: {}", color),
        }
    }
}
