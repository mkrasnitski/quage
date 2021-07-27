#![allow(non_snake_case)]
use anyhow::Result;
use sdl2::pixels::Color;

use crate::config::Config;
use crate::display::*;

fn is_bit_set(val: u8, bit: u8) -> bool {
    val & (1 << bit) != 0
}

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

#[derive(Copy, Clone, Default)]
struct Pixel {
    color: u8,
    palette: u8,
}

impl Pixel {
    fn decode(&self) -> Color {
        let color = (self.palette >> (2 * self.color)) & 0b11;
        match color {
            0 => Color::WHITE,
            1 => Color::RGB(0xaa, 0xaa, 0xaa),
            2 => Color::RGB(0x55, 0x55, 0x55),
            3 => Color::BLACK,
            _ => panic!("Incorrect palette color: {}", color),
        }
    }
}

#[derive(Copy, Clone)]
struct Sprite {
    tile_num: u8,
    x: u8,
    y: u8,
    priority: bool,
    x_flip: bool,
    y_flip: bool,
    palette: bool,
}

impl Sprite {
    fn from_oam_data(data: &[u8]) -> Self {
        Sprite {
            tile_num: data[2],
            x: data[1],
            y: data[0],
            priority: is_bit_set(data[3], 7),
            x_flip: is_bit_set(data[3], 5),
            y_flip: is_bit_set(data[3], 6),
            palette: is_bit_set(data[3], 4),
        }
    }
}

pub struct PPU {
    pub memory: [u8; 0x2000],
    pub oam: [u8; 0xA0],
    pub display_manager: DisplayManager,
    registers: PPURegisters,
    interrupts: PPUInterrupts,
    viewport: Box<[[Color; W_WIDTH]; W_HEIGHT]>,
    oam_sprites: Vec<Sprite>,
    oam_index: usize,
    display: Display<W_WIDTH, W_HEIGHT>,
    tile_display: Option<Display<128, 192>>,

    mode: u8,
    dots: u64,
    cycles: u64,

    enable_display_events: bool,
    stat_condition: bool,
    ly_coincidence: bool,
    wy_ly_latch: bool,
    lcd_startup: bool,
}

impl PPU {
    pub fn new(config: &Config) -> Result<Self> {
        let display_manager = DisplayManager::new(&config)?;
        let display = display_manager.new_display(None, config.show_fps)?;
        let tile_display = if config.dump_tiles {
            Some(display_manager.new_display(Some((1220, 250)), false)?)
        } else {
            None
        };
        Ok(PPU {
            memory: [0; 0x2000],
            oam: [0; 0xA0],
            display_manager,
            registers: PPURegisters::new(),
            interrupts: PPUInterrupts::default(),
            viewport: Box::new([[Color::WHITE; W_WIDTH]; W_HEIGHT]),
            oam_sprites: Vec::new(),
            oam_index: 0,
            display,
            tile_display,

            mode: 0,
            dots: 0,
            cycles: 0,

            enable_display_events: false,
            stat_condition: false,
            ly_coincidence: false,
            wy_ly_latch: false,
            lcd_startup: false,
        })
    }

    // fn memory_lock(&self, addr: u16) -> bool {
    //     if !is_bit_set(self.registers.LCDC, 7) {
    //         false
    //     } else if (0x8000..=0x9FFF).contains(&addr) {
    //         self.mode == 3
    //     } else if (0xFE00..=0xFE9F).contains(&addr) {
    //         self.mode >= 2
    //     } else {
    //         false
    //     }
    // }

    fn memory_lock(&self, _addr: u16) -> bool {
        false
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
        if self.memory_lock(addr) {
            0xFF
        } else {
            self.read_byte_direct(addr)
        }
    }

    fn read_byte_direct(&self, addr: u16) -> u8 {
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
        if !self.memory_lock(addr) {
            match addr {
                0x8000..=0x9FFF => self.memory[addr as usize - 0x8000] = val,
                0xFE00..=0xFE9F => self.oam[addr as usize - 0xFE00] = val,
                0xFF40 => {
                    let old_val = self.registers.LCDC;
                    self.registers.LCDC = val;
                    if !is_bit_set(self.registers.LCDC, 7) {
                        self.dots = 0;
                        self.registers.LY = 0;
                        self.set_mode(0);
                    } else if !is_bit_set(old_val, 7) {
                        self.lcd_startup = true;
                    }
                }
                0xFF41 => {
                    self.registers.STAT &= 0b10000111; // these bits are read-only
                    self.registers.STAT |= val & 0b01111000;
                }
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
        if let Some(tile_display) = self.tile_display.as_mut() {
            tile_display.toggle_frame_limiter();
        }
    }

    pub fn draw(&mut self) {
        if self.cycles == 17556 {
            self.cycles = 0;
            self.enable_display_events = true;
            self.display.draw(&self.viewport);
            if self.tile_display.is_some() {
                let tiles = self.dump_tiles();
                self.tile_display.as_mut().unwrap().draw(&tiles);
            }
        }

        // Keep drawing a blank screen while the LCD is turned off
        // This means occasionally decoupling the display "cycles"
        // from the number of LCD dots that have passed.
        if is_bit_set(self.registers.LCDC, 7) {
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
            if scanline == 0 {
                self.registers.WC = 0;
            }
            self.registers.LY = scanline;
        }
        self.check_LY_coincidence();
        if self.registers.WY == self.registers.LY {
            self.wy_ly_latch = true;
        }

        // PPU Mode switching
        if self.registers.LY < 144 {
            let oam_length = if self.lcd_startup { 19 } else { 20 };
            if clocks < oam_length {
                if clocks == 0 {
                    self.oam_sprites.clear();
                    self.oam_index = 0;
                    self.set_mode(2);
                    if self.lcd_startup {
                        self.registers.STAT &= 0b11111100;
                    }
                }
                self.oam_scan();
            } else if self.mode != 0 {
                if clocks == oam_length {
                    if self.lcd_startup {
                        self.lcd_startup = false;
                    }
                    self.set_mode(3);
                    self.draw_line();
                }
                if clocks == 63 {
                    self.set_mode(0);
                }
            }
        } else {
            if self.registers.LY == 144 && clocks == 0 {
                self.interrupts.vblank = true;
                if is_bit_set(self.registers.STAT, 5) {
                    self.interrupts.stat = true;
                }
            }
            self.wy_ly_latch = false;
            self.set_mode(1);
        }
    }

    fn draw_line(&mut self) {
        let mut scanline = [Pixel::default(); W_WIDTH];
        if is_bit_set(self.registers.LCDC, 7) {
            if is_bit_set(self.registers.LCDC, 0) {
                // Background
                let bg_tilemap = match is_bit_set(self.registers.LCDC, 3) {
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
                            scanline[bg_x] = Pixel {
                                color: bg_tile_row[j as usize],
                                palette: self.registers.BGP,
                            }
                        }
                    }
                }

                // Window
                if is_bit_set(self.registers.LCDC, 5) && self.registers.LY >= self.registers.WY {
                    let win_tilemap = match is_bit_set(self.registers.LCDC, 6) {
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
                                scanline[win_x] = Pixel {
                                    color: win_tile_row[j as usize],
                                    palette: self.registers.BGP,
                                }
                            }
                        }
                    }
                    if window_visible {
                        self.registers.WC += 1
                    }
                }
            }
        }

        // Sprites
        let sprite_height = match is_bit_set(self.registers.LCDC, 2) {
            true => 16,
            false => 8,
        };
        if is_bit_set(self.registers.LCDC, 1) {
            for sprite in &self.oam_sprites {
                let mut row_num = self.registers.LY + 16 - sprite.y;
                if sprite.y_flip {
                    row_num = sprite_height - row_num - 1;
                }
                let palette = if sprite.palette {
                    self.registers.OBP1
                } else {
                    self.registers.OBP0
                };
                let sprite_tile_row = self.decode_tile_row(sprite.tile_num, row_num, true);
                for j in 0..8 {
                    let col_num = if sprite.x_flip { 7 - j } else { j };
                    let sprite_x = (sprite.x as usize + j).wrapping_sub(8);
                    let color = sprite_tile_row[col_num];
                    if (0..W_WIDTH).contains(&sprite_x)
                        && color != 0
                        && (!sprite.priority || scanline[sprite_x].color == 0)
                    {
                        scanline[sprite_x] = Pixel { color, palette }
                    }
                }
            }
        }

        for i in 0..160 {
            self.viewport[self.registers.LY as usize][i] = scanline[i].decode();
        }
    }

    fn set_mode(&mut self, mode: u8) {
        self.mode = mode;
        self.registers.STAT &= 0b11111100;
        self.registers.STAT |= self.mode & 0b11;
        if self.mode < 3 {
            self.update_stat_condition();
        }
    }

    fn check_LY_coincidence(&mut self) {
        self.ly_coincidence = if self.registers.LY == 153 && self.dots % 114 >= 1 {
            self.registers.LYC == 0
        } else {
            self.registers.LYC == self.registers.LY
        };
        self.registers.STAT &= 0b11111011;
        self.registers.STAT |= (self.ly_coincidence as u8) << 2;
        self.update_stat_condition();
    }

    fn update_stat_condition(&mut self) {
        let old_val = self.stat_condition;
        self.stat_condition = is_bit_set(self.registers.STAT, 6) && self.ly_coincidence;
        for i in 0..3 {
            if is_bit_set(self.registers.STAT, i + 3) {
                self.stat_condition |= self.mode == i;
            }
        }
        if self.stat_condition && !old_val {
            self.interrupts.stat = true;
        }
    }

    fn oam_scan(&mut self) {
        let sprite_height = match is_bit_set(self.registers.LCDC, 2) {
            true => 16,
            false => 8,
        };
        for i in self.oam_index..self.oam_index + 2 {
            if self.oam_sprites.len() < 10 {
                let sprite = Sprite::from_oam_data(&self.oam[4 * i..4 * i + 4]);
                let pos = self.registers.LY + 16;
                if sprite.x > 0 && (sprite.y..sprite.y + sprite_height).contains(&pos) {
                    self.oam_sprites.push(sprite);
                }
            }
        }
        self.oam_index += 2;
    }

    fn decode_tile_row(&self, tile_num: u8, row_num: u8, is_sprite: bool) -> [u8; 8] {
        let mut row = [0; 8];
        let tile_row_offset =
            if !is_bit_set(self.registers.LCDC, 4) && tile_num < 0x80 && !is_sprite {
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

    fn dump_tiles(&self) -> [[Color; 128]; 192] {
        let mut bg = [[Color::WHITE; 128]; 192];
        for i in 0..384 {
            let tile_addr = 0x8000 + i * 16;
            let tile_y = i as usize / 16;
            let tile_x = i as usize % 16;
            for j in 0..8 {
                let hi = self.read_byte_direct(tile_addr + 2 * j as u16 + 1);
                let lo = self.read_byte_direct(tile_addr + 2 * j as u16);
                for k in 0..8 {
                    let x = 8 * tile_x + 7 - k;
                    let y = 8 * tile_y + j;
                    let pixel = Pixel {
                        color: (((hi >> k) & 1) << 1) | ((lo >> k) & 1),
                        palette: self.registers.BGP,
                    };
                    bg[y][x] = pixel.decode();
                }
            }
        }
        bg
    }
}
