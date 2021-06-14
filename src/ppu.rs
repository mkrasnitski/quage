#![allow(non_snake_case)]
use anyhow::Result;
use sdl2::pixels::Color;
use std::collections::VecDeque;

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

#[derive(Copy, Clone, Debug)]
enum FetcherState {
    GetTileNum,
    GetLoByte,
    GetHiByte,
    Sleep,
}

impl Default for FetcherState {
    fn default() -> Self {
        Self::GetTileNum
    }
}

#[derive(Default)]
struct Fetcher {
    state: FetcherState,
    cycles: u8,
    tile_num: u8,
    row_num: u8,
    tile_addr: u16,
    tile_lo: u8,
    tile_hi: u8,
    push: bool,
}

impl Fetcher {
    pub fn step_state(&mut self) -> Option<FetcherState> {
        let new_state = match self.cycles {
            0 => Some(FetcherState::GetTileNum),
            2 => Some(FetcherState::GetLoByte),
            4 => Some(FetcherState::GetHiByte),
            6 => Some(FetcherState::Sleep),
            _ => None,
        };
        self.cycles += 1;
        if self.cycles >= 8 && !self.push {
            self.push = true;
        }
        if let Some(state) = new_state {
            self.state = state;
            new_state
        } else {
            None
        }
    }

    pub fn reset(&mut self) {
        self.cycles = 0;
        self.push = false;
    }
}

struct FIFO {
    fetcher: Fetcher,
    queue: VecDeque<Pixel>,
    num_pixels: u8,
    num_tiles: u8,
    thrown_away: u8,
}

impl FIFO {
    pub fn new() -> Self {
        FIFO {
            fetcher: Fetcher::default(),
            queue: VecDeque::with_capacity(8),
            num_pixels: 0,
            num_tiles: 0,
            thrown_away: 0,
        }
    }

    pub fn clear(&mut self) {
        *self = FIFO::new();
    }
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

pub struct PPU {
    pub memory: [u8; 0x2000],
    pub oam: [u8; 0xA0],
    pub display_manager: DisplayManager,
    registers: PPURegisters,
    interrupts: PPUInterrupts,
    viewport: [[Color; W_WIDTH]; W_HEIGHT],
    oam_sprites: Vec<usize>,
    oam_index: usize,
    display: Display<W_WIDTH, W_HEIGHT>,
    tile_display: Option<Display<128, 192>>,

    bg_fifo: FIFO,
    fifo_display: Display<W_WIDTH, W_HEIGHT>,
    fifo_viewport: [[Color; W_WIDTH]; W_HEIGHT],

    enable_display_events: bool,
    stat_condition: bool,
    ly_coincidence: bool,
    drawing_window: bool,
    wy_ly_latch: bool,
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
            display_manager,
            registers: PPURegisters::new(),
            interrupts: PPUInterrupts::default(),
            viewport: [[Color::WHITE; W_WIDTH]; W_HEIGHT],
            oam_sprites: Vec::new(),
            oam_index: 0,
            display,
            tile_display,

            bg_fifo: FIFO::new(),
            fifo_viewport: [[Color::WHITE; W_WIDTH]; W_HEIGHT],
            fifo_display,

            enable_display_events: false,
            stat_condition: false,
            ly_coincidence: false,
            drawing_window: false,
            wy_ly_latch: false,
            dots: 0,
            cycles: 0,
        })
    }

    #[allow(dead_code)]
    fn memory_lock(&self, addr: u16) -> bool {
        let mode = self.get_mode();
        if !is_bit_set(self.registers.LCDC, 7) {
            false
        } else if (0x8000..=0x9FFF).contains(&addr) {
            mode == 3
        } else if (0xFE00..=0xFE9F).contains(&addr) {
            mode >= 2
        } else {
            false
        }
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
                    self.registers.LCDC = val;
                    if !is_bit_set(self.registers.LCDC, 7) {
                        self.dots = 0;
                        self.registers.LY = 0;
                        self.set_mode(0);
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
            if clocks < 20 {
                if clocks == 0 {
                    self.set_mode(2);
                    self.oam_sprites.clear();
                    self.oam_index = 0;
                }
                self.oam_scan();
            } else if self.get_mode() != 0 {
                if clocks == 20 {
                    self.set_mode(3);
                    self.draw_line();
                    self.bg_fifo.clear();
                }
                self.draw_fifo();
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

    fn set_mode(&mut self, mode: u8) {
        self.registers.STAT = (self.registers.STAT & !0b11) | (mode & 0b11);
        if mode < 3 {
            self.update_stat_condition();
        }
    }

    fn get_mode(&self) -> u8 {
        self.registers.STAT & 0b11
    }

    fn check_LY_coincidence(&mut self) {
        self.ly_coincidence = if self.registers.LY == 153 && self.dots % 114 >= 1 {
            self.registers.LYC == 0
        } else {
            self.registers.LYC == self.registers.LY
        };
        self.registers.STAT &= !(1 << 2);
        self.registers.STAT |= (self.ly_coincidence as u8) << 2;
        self.update_stat_condition();
    }

    fn update_stat_condition(&mut self) {
        let old_val = self.stat_condition;
        self.stat_condition = is_bit_set(self.registers.STAT, 6) && self.ly_coincidence;
        for i in 0..3 {
            if self.registers.STAT & (1 << (i + 3)) != 0 {
                self.stat_condition |= self.get_mode() == i;
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
                let sprite_y = self.oam[4 * i];
                let sprite_x = self.oam[4 * i + 1];
                let sprite_pos = self.registers.LY + 16 - sprite_y;
                if sprite_x > 0 && (0..sprite_height).contains(&sprite_pos) {
                    let get_sprite_x = |j| self.oam[4 * j as usize + 1];
                    let idx = self
                        .oam_sprites
                        .binary_search_by(|&j| sprite_x.cmp(&get_sprite_x(j)))
                        .unwrap_or_else(|j| j);
                    self.oam_sprites.insert(idx, i);
                }
            }
        }
        self.oam_index += 2;
    }

    fn draw_fifo(&mut self) {
        // FIFO pushes pixels out every T-cycle
        for _ in 0..4 {
            // Throw away pixels that would normally be off screen
            // This includes the first SCX % 8 pixels of the background,
            // and the first pixels of the window if WX < 7
            let num_to_throw = match self.drawing_window {
                true => match self.registers.WX {
                    0..=6 => 7 - self.registers.WX,
                    _ => 0,
                },
                false => self.registers.SCX % 8,
            };
            if self.bg_fifo.num_pixels == 0 && self.bg_fifo.thrown_away < num_to_throw {
                if self.bg_fifo.queue.pop_front().is_some() {
                    self.bg_fifo.thrown_away += 1;
                }
                self.step_fetcher();
            } else if self.bg_fifo.num_pixels < 160 {
                // Pop pixels until we have 160 on the line, then mode 3 is done
                if is_bit_set(self.registers.LCDC, 7) {
                    if is_bit_set(self.registers.LCDC, 0) {
                        if let Some(pixel) = self.bg_fifo.queue.pop_front() {
                            let x = self.bg_fifo.num_pixels as usize;
                            let y = self.registers.LY as usize;
                            self.fifo_viewport[y][x] = pixel.decode();
                            self.bg_fifo.num_pixels += 1;
                        }
                    }
                    // Check if we're inside the window, and reset the FIFO if we are
                    let window_x = self.bg_fifo.num_pixels + 7;
                    if is_bit_set(self.registers.LCDC, 5)
                        && (self.registers.WX..167).contains(&window_x)
                        && self.wy_ly_latch
                        && !self.drawing_window
                    {
                        self.drawing_window = true;
                        self.bg_fifo.fetcher.reset();
                        self.bg_fifo.queue.clear();
                    }
                    self.step_fetcher();
                }
            } else {
                self.set_mode(0);
                if self.drawing_window {
                    self.drawing_window = false;
                    self.registers.WC += 1;
                }
            }
        }
    }

    fn step_fetcher(&mut self) {
        let fifo_state = self.bg_fifo.fetcher.step_state();
        if let Some(state) = fifo_state {
            match state {
                FetcherState::GetTileNum => {
                    // BG and Window use different tilemap flip-bits
                    let tilemap_bit = match self.drawing_window {
                        true => 6,
                        false => 3,
                    };
                    let tilemap = match is_bit_set(self.registers.LCDC, tilemap_bit) {
                        true => 0x9C00,
                        false => 0x9800,
                    };
                    // Figure out what tile we're drawing
                    let (x, y) = match self.drawing_window {
                        true => (
                            8 * self.bg_fifo.num_tiles
                                - (self.registers.SCX % 8)
                                - (self.registers.WX - 7),
                            self.registers.WC,
                        ),
                        false => (
                            8 * self.bg_fifo.num_tiles + self.registers.SCX,
                            self.registers.LY.wrapping_add(self.registers.SCY),
                        ),
                    };
                    // Fetch the tile number from the tilemap
                    let tile_addr = tilemap + 32 * (y / 8) as u16 + (x / 8) as u16;
                    self.bg_fifo.fetcher.tile_num = self.read_byte_direct(tile_addr);
                    self.bg_fifo.fetcher.row_num = y % 8;
                }
                FetcherState::GetLoByte => {
                    // Fetch the actual graphics data
                    let tile_num = self.bg_fifo.fetcher.tile_num;
                    let row_num = self.bg_fifo.fetcher.row_num;
                    self.bg_fifo.fetcher.tile_addr =
                        match !is_bit_set(self.registers.LCDC, 4) && tile_num < 0x80 {
                            true => 0x9000,
                            false => 0x8000,
                        } + 16 * tile_num as u16
                            + 2 * row_num as u16;
                    self.bg_fifo.fetcher.tile_lo =
                        self.read_byte_direct(self.bg_fifo.fetcher.tile_addr);
                }
                FetcherState::GetHiByte => {
                    self.bg_fifo.fetcher.tile_hi =
                        self.read_byte_direct(self.bg_fifo.fetcher.tile_addr + 1);
                }
                FetcherState::Sleep => {}
            }
        }
        // If the FIFO is empty, decode the row of 8 pixels and push them onto it
        if self.bg_fifo.fetcher.push && self.bg_fifo.queue.is_empty() {
            let bg_tile_row =
                self.decode_tile_bytes(self.bg_fifo.fetcher.tile_hi, self.bg_fifo.fetcher.tile_lo);
            for &color in bg_tile_row.iter() {
                self.bg_fifo.queue.push_back(Pixel {
                    color,
                    palette: self.registers.BGP,
                })
            }
            self.bg_fifo.num_tiles += 1;
            self.bg_fifo.fetcher.reset();
        }
    }

    fn draw_line(&mut self) {
        let mut scanline = [Pixel::default(); W_WIDTH];
        // LCD Enable
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
                        self.read_byte_direct(bg_tilemap + 32 * ((bg_y / 8) as u16) + i as u16);
                    let bg_tile_row = self.decode_tile_row(bg_tile_num, bg_y % 8, false);
                    for j in 0u8..8 {
                        let bg_x = (8 * i + j).wrapping_sub(self.registers.SCX) as usize;
                        if bg_x < W_WIDTH {
                            scanline[bg_x] = Pixel {
                                color: bg_tile_row[j as usize],
                                palette: self.registers.BGP,
                            };
                        }
                    }
                }

                // Window
                if is_bit_set(self.registers.LCDC, 5) && self.wy_ly_latch {
                    let win_tilemap = match is_bit_set(self.registers.LCDC, 6) {
                        true => 0x9C00,
                        false => 0x9800,
                    };
                    for i in 0u8..32 {
                        let win_tile_num = self.read_byte_direct(
                            win_tilemap + 32 * ((self.registers.WC / 8) as u16) + i as u16,
                        );
                        let win_tile_row =
                            self.decode_tile_row(win_tile_num, self.registers.WC % 8, false);
                        for j in 0..8 {
                            let win_x = 8 * i as usize + j + self.registers.WX as usize - 7;
                            if (0..W_WIDTH).contains(&win_x) {
                                scanline[win_x] = Pixel {
                                    color: win_tile_row[j as usize],
                                    palette: self.registers.BGP,
                                };
                            }
                        }
                    }
                }
            }

            // Sprites
            let sprite_height = ((self.registers.LCDC & (1 << 2)) >> 2) as u8 + 1;
            if is_bit_set(self.registers.LCDC, 1) {
                for sprite_index in self.oam_sprites.iter() {
                    let sprite_y = self.oam[4 * sprite_index];
                    let sprite_x = self.oam[4 * sprite_index + 1];
                    let sprite_tile_num =
                        self.oam[4 * sprite_index + 2] & (0xFF - sprite_height + 1);
                    let sprite_flags = self.oam[4 * sprite_index + 3];

                    let sprite_palette = if is_bit_set(sprite_flags, 4) {
                        self.registers.OBP1
                    } else {
                        self.registers.OBP0
                    };

                    let mut row_num = self.registers.LY + 16 - sprite_y;
                    if is_bit_set(sprite_flags, 6) {
                        row_num = 8 * sprite_height - 1 - row_num
                    };
                    let sprite_tile_row = self.decode_tile_row(sprite_tile_num, row_num, true);
                    for j in 0..8 {
                        let col_num = if is_bit_set(sprite_flags, 5) {
                            7 - j
                        } else {
                            j
                        };
                        let obj_x = (sprite_x as usize + j).wrapping_sub(8);
                        let color_num = sprite_tile_row[col_num];
                        if (0..W_WIDTH).contains(&obj_x)
                            && color_num != 0
                            && (!is_bit_set(sprite_flags, 7) || scanline[obj_x].color == 0)
                        {
                            scanline[obj_x] = Pixel {
                                color: color_num,
                                palette: sprite_palette,
                            };
                        }
                    }
                }
            }
        }

        for i in 0..160 {
            self.viewport[self.registers.LY as usize][i] = scanline[i].decode();
        }
    }

    fn dump_tiles(&self, base: u16) -> [[Color; 128]; 192] {
        let mut bg = [[Color::WHITE; 128]; 192];
        for i in 0..384 {
            let tile_addr = base + i * 16;
            let tile_y = i / 16;
            let tile_x = i % 16;
            for j in 0..8 {
                let hi = self.read_byte_direct(tile_addr + 2 * j + 1);
                let lo = self.read_byte_direct(tile_addr + 2 * j);
                for k in 0..8 {
                    let pixel = Pixel {
                        color: (((hi >> k) & 1) << 1) | ((lo >> k) & 1),
                        palette: self.registers.BGP,
                    };
                    bg[(8 * tile_y + j) as usize][(8 * tile_x + 7 - k) as usize] = pixel.decode();
                }
            }
        }
        bg
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
        let hi = self.read_byte_direct(tile_row_offset + 1);
        let lo = self.read_byte_direct(tile_row_offset);
        for i in 0..8 {
            row[7 - i] = (((hi >> i) & 1) << 1) | ((lo >> i) & 1);
        }
        row
    }

    fn decode_tile_bytes(&self, hi: u8, lo: u8) -> [u8; 8] {
        let mut row = [0; 8];
        for i in 0..8 {
            row[7 - i] = (((hi >> i) & 1) << 1) | ((lo >> i) & 1);
        }
        row
    }
}
