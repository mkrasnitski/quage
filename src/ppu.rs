#![allow(non_snake_case)]
use anyhow::Result;
use sdl2::pixels::Color;

use crate::config::Config;
use crate::display::*;

#[derive(Default)]
struct PPUInterrupts {
    vblank: bool,
    stat: bool,
}

#[derive(Default)]
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
}

impl PPURegisters {
    pub fn new() -> Self {
        PPURegisters {
            STAT: 0x80,
            ..Default::default()
        }
    }
}

pub struct PPU {
    pub memory: [u8; 0x2000],
    pub oam: [u8; 0xA0],
    pub display_manager: DisplayManager,
    registers: PPURegisters,
    interrupts: PPUInterrupts,
    viewport: [[Color; W_WIDTH]; W_HEIGHT],
    fifo_viewport: [[Color; W_WIDTH]; W_HEIGHT],
    sprite_indices: Vec<usize>,
    display: Display<W_WIDTH, W_HEIGHT>,
    fifo_display: Display<W_WIDTH, W_HEIGHT>,
    tile_display: Option<Display<128, 192>>,
    enable_display_events: bool,
    block_stat_irqs: bool,
    dots: u64,
    cycles: u64,
}

impl PPU {
    pub fn new(config: &Config) -> Result<Self> {
        let display_manager = DisplayManager::new()?;
        let display = display_manager.new_display((468, 350), config.show_fps)?;
        let fifo_display = display_manager.new_display((968, 350), false)?;
        let tile_display = if config.dump_tiles {
            Some(display_manager.new_display((1468, 280), false)?)
        } else {
            None
        };
        Ok(PPU {
            memory: [0; 0x2000],
            oam: [0; 0xA0],
            sprite_indices: Vec::new(),
            viewport: [[Color::WHITE; W_WIDTH]; W_HEIGHT],
            fifo_viewport: [[Color::WHITE; W_WIDTH]; W_HEIGHT],
            registers: PPURegisters::new(),
            interrupts: PPUInterrupts::default(),
            display_manager,
            display,
            fifo_display,
            tile_display,
            enable_display_events: false,
            block_stat_irqs: false,
            dots: 0,
            cycles: 0,
        })
    }

    #[allow(dead_code)]
    fn memory_lock(&self, addr: u16) -> bool {
        let mode = self.registers.STAT & 0b11;
        if (0x8000..=0x9FFF).contains(&addr) {
            mode < 3
        } else if (0xFE00..=0xFE9F).contains(&addr) {
            mode < 2
        } else {
            false
        }
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
            0xFF40 => {
                self.registers.LCDC = val;
                if val & (1 << 7) == 0 {
                    self.dots = 0;
                    self.registers.LY = 0;
                    self.set_mode(0);
                }
            }
            0xFF41 => self.registers.STAT |= val & !7,
            0xFF42 => self.registers.SCY = val,
            0xFF43 => self.registers.SCX = val,
            0xFF45 => self.registers.LYC = val,
            0xFF47 => self.registers.BGP = val,
            0xFF48 => self.registers.OBP0 = val,
            0xFF49 => self.registers.OBP1 = val,
            0xFF4A => self.registers.WY = val,
            0xFF4B => self.registers.WX = val,
            _ => panic!("Invalid PPU Register write: {:04x}", addr),
        }
    }

    pub fn poll_display_event(&mut self) -> DisplayEvent {
        if self.enable_display_events {
            self.enable_display_events = false;
            self.display_manager.poll_event()
        } else {
            DisplayEvent::None
        }
    }

    pub fn check_interrupts(&mut self) -> (bool, bool) {
        let res = (self.interrupts.vblank, self.interrupts.stat);
        self.interrupts.vblank = false;
        self.interrupts.stat = false;
        res
    }

    pub fn toggle_frame_limiter(&mut self) {
        self.display.toggle_frame_limiter();
        self.fifo_display.toggle_frame_limiter();
        if let Some(tile_display) = self.tile_display.as_mut() {
            tile_display.toggle_frame_limiter();
        }
    }

    pub fn draw(&mut self) {
        if self.cycles == 17556 {
            self.cycles = 0;
            self.enable_display_events = true;
            self.display.draw(&self.viewport);
            self.fifo_display.draw(&self.fifo_viewport);
            let tiles = self.dump_tiles(0x8000);
            if let Some(tile_display) = self.tile_display.as_mut() {
                tile_display.draw(&tiles);
            }
        }

        // Keep drawing a blank screen while the LCD is turned off
        // This means occasionally decoupling the display "cycles"
        // from the number of LCD dots that have passed.
        if self.registers.LCDC & (1 << 7) != 0 {
            self.step();
            self.cycles = self.dots + 1;
            self.dots = (self.dots + 1) % 17556;
        } else {
            self.cycles += 1;
        }
    }

    fn step(&mut self) {
        let clocks = self.dots % 114;
        let scanline = (self.dots / 114) as u8;

        // Start of a line
        if clocks == 0 {
            self.block_stat_irqs = false;
            if scanline == 0 {
                self.registers.WC = 0;
            }
            self.registers.LY = scanline;
            self.check_LY_coincidence(self.registers.LY);
        }
        if self.registers.LY == 153 && clocks == 1 {
            self.check_LY_coincidence(0);
        }

        // PPU Mode switching
        if self.registers.LY < 144 {
            match clocks {
                0 => {
                    self.set_mode(2);
                    self.oam_scan();
                }
                20 => {
                    self.set_mode(3); // Drawing
                    self.draw_line();
                }
                64 => self.set_mode(0), // H-blank
                _ => {}
            }
        } else {
            if self.registers.LY == 144 && clocks == 0 {
                self.interrupts.vblank = true;
            }
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

    fn check_LY_coincidence(&mut self, line: u8) {
        let coincidence = line == self.registers.LYC;
        if coincidence {
            self.req_stat_interrupt(6);
        }
        self.registers.STAT &= !(1 << 2);
        self.registers.STAT |= (coincidence as u8) << 2;
    }

    fn req_stat_interrupt(&mut self, bit: u8) {
        if !self.block_stat_irqs
            && (self.registers.STAT & (1 << bit)) != 0
            && (3..=6).contains(&bit)
        {
            self.block_stat_irqs = true;
            self.interrupts.stat = true;
        }
    }

    fn oam_scan(&mut self) {
        let sprite_height = ((self.registers.LCDC & (1 << 2)) >> 2) as u8 + 1;
        self.sprite_indices.clear();
        for i in 0..40 {
            if self.sprite_indices.len() < 10 {
                let sprite_y = self.oam[4 * i];
                let sprite_x = self.oam[4 * i + 1];
                if sprite_x > 0
                    && self.registers.LY + 16 >= sprite_y
                    && self.registers.LY + 16 < sprite_y + 8 * sprite_height
                {
                    let get_sprite_x = |j| self.oam[4 * j as usize + 1];
                    let idx = self
                        .sprite_indices
                        .binary_search_by(|&j| sprite_x.cmp(&get_sprite_x(j)))
                        .unwrap_or_else(|j| j);
                    self.sprite_indices.insert(idx, i);
                }
            }
        }
    }

    fn draw_line(&mut self) {
        let mut scanline = [0; W_WIDTH];
        let mut palettes = [0; W_WIDTH];
        // LCD Enable
        if self.registers.LCDC & (1 << 7) != 0 {
            if self.registers.LCDC & 1 != 0 {
                // Background
                let bg_tilemap = match self.registers.LCDC & (1 << 3) != 0 {
                    true => 0x9C00,
                    false => 0x9800,
                };
                let bg_y = self.registers.SCY.wrapping_add(self.registers.LY);
                for i in 0u8..32 {
                    let bg_tile_num =
                        self.read_byte(bg_tilemap + 32 * ((bg_y / 8) as u16) + i as u16);
                    let bg_tile_row = self.decode_tile_row(bg_tile_num, bg_y % 8, false);
                    for j in 0u8..8 {
                        let bg_x = (8 * i + j).wrapping_sub(self.registers.SCX) as usize;
                        if bg_x < W_WIDTH {
                            scanline[bg_x] = bg_tile_row[j as usize];
                            palettes[bg_x] = self.registers.BGP;
                        }
                    }
                }

                // Window
                if self.registers.LCDC & (1 << 5) != 0 && self.registers.LY >= self.registers.WY {
                    let win_tilemap = match self.registers.LCDC & (1 << 6) != 0 {
                        true => 0x9C00,
                        false => 0x9800,
                    };
                    let mut window_visible = false;
                    for i in 0u8..32 {
                        let win_tile_num = self.read_byte(
                            win_tilemap + 32 * ((self.registers.WC / 8) as u16) + i as u16,
                        );
                        let win_tile_row =
                            self.decode_tile_row(win_tile_num, self.registers.WC % 8, false);
                        for j in 0..8 {
                            let win_x = 8 * i as usize + j + self.registers.WX as usize - 7;
                            if (0..W_WIDTH).contains(&win_x) {
                                window_visible = true;
                                scanline[win_x] = win_tile_row[j as usize];
                                palettes[win_x] = self.registers.BGP;
                            }
                        }
                    }
                    if window_visible {
                        self.registers.WC += 1
                    }
                }
            }

            // Sprites
            let sprite_height = ((self.registers.LCDC & (1 << 2)) >> 2) as u8 + 1;
            if self.registers.LCDC & (1 << 1) != 0 {
                for sprite_index in self.sprite_indices.iter() {
                    let sprite_y = self.oam[4 * sprite_index];
                    let sprite_x = self.oam[4 * sprite_index + 1];
                    let sprite_tile_num =
                        self.oam[4 * sprite_index + 2] & (0xFF - sprite_height + 1);
                    let sprite_flags = self.oam[4 * sprite_index + 3];

                    let sprite_palette = if sprite_flags & (1 << 4) != 0 {
                        self.registers.OBP1
                    } else {
                        self.registers.OBP0
                    };

                    let mut row_num = self.registers.LY + 16 - sprite_y;
                    if sprite_flags & (1 << 6) != 0 {
                        row_num = 8 * sprite_height - 1 - row_num
                    };
                    let sprite_tile_row = self.decode_tile_row(sprite_tile_num, row_num, true);
                    for j in 0..8 {
                        let col_num = if sprite_flags & (1 << 5) != 0 {
                            7 - j
                        } else {
                            j
                        };
                        let obj_x = (sprite_x as usize + j).wrapping_sub(8);
                        let color_num = sprite_tile_row[col_num];
                        if (0..W_WIDTH).contains(&obj_x)
                            && color_num != 0
                            && (sprite_flags & (1 << 7) == 0 || scanline[obj_x] == 0)
                        {
                            scanline[obj_x] = color_num;
                            palettes[obj_x] = sprite_palette;
                        }
                    }
                }
            }
        }

        for i in 0..160 {
            self.viewport[self.registers.LY as usize][i] =
                self.decode_palette(palettes[i], scanline[i]);
        }
    }

    fn dump_tiles(&self, base: u16) -> [[Color; 128]; 192] {
        let mut bg = [[Color::WHITE; 128]; 192];
        for i in 0..384 {
            let tile_addr = base + i * 16;
            let tile_y = i / 16;
            let tile_x = i % 16;
            for j in 0..8 {
                let hi = self.read_byte(tile_addr + 2 * j + 1);
                let lo = self.read_byte(tile_addr + 2 * j);
                for k in 0..8 {
                    bg[(8 * tile_y + j) as usize][(8 * tile_x + 7 - k) as usize] = self
                        .decode_palette(
                            self.registers.BGP,
                            (((hi >> k) & 1) << 1) | ((lo >> k) & 1),
                        );
                }
            }
        }
        bg
    }

    fn decode_tile_row(&self, tile_num: u8, row_num: u8, is_sprite: bool) -> [u8; 8] {
        let mut row = [0; 8];
        let tile_row_offset =
            if self.registers.LCDC & (1 << 4) == 0 && tile_num < 0x80 && !is_sprite {
                0x9000
            } else {
                0x8000
            } + 16 * tile_num as u16
                + 2 * row_num as u16;
        let hi = self.read_byte(tile_row_offset + 1);
        let lo = self.read_byte(tile_row_offset);
        for i in 0..8 {
            row[7 - i] = (((hi >> i) & 1) << 1) | ((lo >> i) & 1);
        }
        row
    }

    fn decode_palette(&self, palette: u8, color: u8) -> Color {
        let color = (palette >> (2 * color)) & 0b11;
        match color {
            0 => Color::WHITE,
            1 => Color::RGB(0xaa, 0xaa, 0xaa),
            2 => Color::RGB(0x55, 0x55, 0x55),
            3 => Color::BLACK,
            _ => panic!("Incorrect palette color: {}", color),
        }
    }
}
