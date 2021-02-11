use anyhow::Result;
use sdl2::pixels::Color;

use crate::display::*;

pub struct PPU {
    pub memory: [u8; 0x2000],
    display: Display,
    draw_call: bool,
    cycles: u64,
}

impl PPU {
    pub fn new() -> Result<Self> {
        Ok(PPU {
            memory: [0; 0x2000],
            display: Display::new()?,
            draw_call: false,
            cycles: 0,
        })
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
        self.memory[addr as usize - 0x8000]
    }

    pub fn write_byte(&mut self, addr: u16, val: u8) {
        self.memory[addr as usize - 0x8000] = val
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
    }

    pub fn poll_display_event(&mut self) -> DisplayEvent {
        if self.draw_call {
            self.display.poll_event()
        } else {
            DisplayEvent::None
        }
    }

    fn render_display(&mut self) {
        let mut colors = [[Color::WHITE; W_WIDTH]; W_HEIGHT];
        for x in 0..32 {
            for y in 0..32 {
                let tile = self.decode_tile(self.read_byte(0x9800 + 32 * x as u16 + y as u16));
                for i in 0..8 {
                    for j in 0..8 {
                        colors[8 * x + i][8 * y + j] = self.decode_palette(tile[i][j]);
                    }
                }
            }
        }
        self.display.draw(colors);
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

    fn decode_palette(&self, num: u8) -> Color {
        match num {
            0 => Color::WHITE,
            1 => Color::RGB(0x55, 0x55, 0x55),
            2 => Color::RGB(0xaa, 0xaa, 0xaa),
            3 => Color::BLACK,
            _ => panic!("Incorrect palette color: {}", num),
        }
    }
}
