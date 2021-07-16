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

#[derive(Copy, Clone)]
struct Pixel {
    color: u8,
    palette: u8,
    priority: bool,
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

#[derive(Copy, Clone, Debug)]
enum Fetch {
    TileNum,
    LowByte,
    HighByte,
}

impl Default for Fetch {
    fn default() -> Self {
        Fetch::TileNum
    }
}

#[derive(Default)]
struct Fetcher {
    state: Fetch,
    cycles: u32,
    tile_num: u8,
    row_num: u8,
    tile_addr: u16,
    tile_lo: u8,
    tile_hi: u8,
    push: bool,
}

impl Fetcher {
    pub fn step_state(&mut self) -> Option<Fetch> {
        let new_state = match self.cycles {
            0 => Some(Fetch::TileNum),
            2 => Some(Fetch::LowByte),
            4 => Some(Fetch::HighByte),
            _ => None,
        };
        self.cycles += 1;
        if self.cycles >= 6 && !self.push {
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

    pub fn decode_tile_row(&self) -> [u8; 8] {
        let mut row = [0; 8];
        for i in 0..8 {
            row[7 - i] = (((self.tile_hi >> i) & 1) << 1) | ((self.tile_lo >> i) & 1);
        }
        row
    }
}

struct FIFO {
    fetcher: Fetcher,
    queue: VecDeque<Pixel>,
    thrown_away: u8,
}

impl FIFO {
    pub fn new() -> Self {
        FIFO {
            fetcher: Fetcher::default(),
            queue: VecDeque::with_capacity(8),
            thrown_away: 0,
        }
    }

    pub fn clear(&mut self) {
        *self = FIFO::new();
    }

    pub fn pop(&mut self) -> Option<Pixel> {
        self.queue.pop_front()
    }

    pub fn push(&mut self, pixel: Pixel) {
        self.queue.push_back(pixel);
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

    bg_fifo: FIFO,
    sprite_fifo: FIFO,
    current_sprite: Option<Sprite>,

    mode: u8,
    dots: u64,
    cycles: u64,

    num_pixels_drawn: u8,
    num_tiles_fetched: u8,

    enable_display_events: bool,
    stat_condition: bool,
    ly_coincidence: bool,
    drawing_window: bool,
    wy_ly_latch: bool,
    lcd_just_turned_on: bool,
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

            bg_fifo: FIFO::new(),
            sprite_fifo: FIFO::new(),
            current_sprite: None,

            num_pixels_drawn: 0,
            num_tiles_fetched: 0,

            mode: 0,
            dots: 0,
            cycles: 0,

            enable_display_events: false,
            stat_condition: false,
            ly_coincidence: false,
            drawing_window: false,
            wy_ly_latch: false,
            lcd_just_turned_on: false,
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
                        self.lcd_just_turned_on = true;
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
            let oam_length = if self.lcd_just_turned_on { 19 } else { 20 };
            if clocks < oam_length {
                if clocks == 0 {
                    self.oam_sprites.clear();
                    self.oam_index = 0;
                    self.set_mode(2);
                    if self.lcd_just_turned_on {
                        self.registers.STAT &= 0b11111100;
                    }
                }
                self.oam_scan();
            } else if self.mode != 0 {
                if clocks == oam_length {
                    if self.lcd_just_turned_on {
                        self.lcd_just_turned_on = false;
                    }
                    self.set_mode(3);
                    self.clear_fifos();
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
            if self.registers.STAT & (1 << (i + 3)) != 0 {
                self.stat_condition |= self.mode == i;
            }
        }
        if self.stat_condition && !old_val {
            self.interrupts.stat = true;
        }
    }

    fn clear_fifos(&mut self) {
        self.bg_fifo.clear();
        self.sprite_fifo.clear();
        self.num_pixels_drawn = 0;
        self.num_tiles_fetched = 0;
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

    fn draw_fifo(&mut self) {
        // FIFO pushes pixels out every T-cycle
        for _ in 0..4 {
            if is_bit_set(self.registers.LCDC, 1) {
                for i in 0..self.oam_sprites.len() {
                    let sprite = self.oam_sprites[i];
                    if sprite.x <= self.num_pixels_drawn + 8 && self.current_sprite.is_none() {
                        self.current_sprite = Some(sprite);
                        self.sprite_fifo.fetcher.reset();
                        self.oam_sprites.remove(i);
                        break;
                    }
                }
            }
            if self.current_sprite.is_some() {
                self.step_sprite_fetcher();
            } else {
                // Throw away pixels that would normally be off screen
                // This includes the first SCX % 8 pixels of the background,
                // and the first pixels of the window if WX < 7
                let left_edge = match self.drawing_window {
                    true => match self.registers.WX {
                        0..=6 => 7 - self.registers.WX,
                        _ => 0,
                    },
                    false => self.registers.SCX % 8,
                };
                if self.bg_fifo.thrown_away < left_edge {
                    if self.bg_fifo.pop().is_some() {
                        self.bg_fifo.thrown_away += 1;
                    }
                    self.step_bg_fetcher();
                } else if self.num_pixels_drawn < 160 {
                    // Pop pixels until we have 160 on the line, then mode 3 is done
                    if is_bit_set(self.registers.LCDC, 7) {
                        if is_bit_set(self.registers.LCDC, 0) {
                            if let Some(pixel) = self.pop_mix_fifos() {
                                let x = self.num_pixels_drawn as usize;
                                let y = self.registers.LY as usize;
                                self.viewport[y][x] = pixel.decode();
                                self.num_pixels_drawn += 1;
                            }
                        }
                        // Check if we're inside the window, and reset the FIFO if we are
                        let window_x = self.num_pixels_drawn + 7;
                        if is_bit_set(self.registers.LCDC, 5)
                            && (self.registers.WX..167).contains(&window_x)
                            && self.wy_ly_latch
                            && !self.drawing_window
                        {
                            self.drawing_window = true;
                            self.bg_fifo.fetcher.reset();
                            self.bg_fifo.queue.clear();
                        }
                        self.step_bg_fetcher();
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
    }

    fn pop_mix_fifos(&mut self) -> Option<Pixel> {
        let bg_pixel = self.bg_fifo.pop()?;
        match self.sprite_fifo.pop() {
            Some(sprite_pixel) => {
                match (sprite_pixel.priority && bg_pixel.color != 0) || sprite_pixel.color == 0 {
                    true => Some(bg_pixel),
                    false => Some(sprite_pixel),
                }
            }
            None => Some(bg_pixel),
        }
    }

    fn step_bg_fetcher(&mut self) {
        let fifo_state = self.bg_fifo.fetcher.step_state();
        if let Some(state) = fifo_state {
            match state {
                Fetch::TileNum => {
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
                            (8 * self.num_tiles_fetched)
                                .wrapping_sub(self.registers.SCX % 8)
                                .wrapping_sub(self.registers.WX)
                                .wrapping_add(7),
                            self.registers.WC,
                        ),
                        false => (
                            (8 * self.num_tiles_fetched).wrapping_add(self.registers.SCX),
                            self.registers.LY.wrapping_add(self.registers.SCY),
                        ),
                    };
                    // Fetch the tile number from the tilemap
                    let tile_addr = tilemap + 32 * (y / 8) as u16 + (x / 8) as u16;
                    self.bg_fifo.fetcher.tile_num = self.read_byte_direct(tile_addr);
                    self.bg_fifo.fetcher.row_num = y % 8;
                }
                // Fetch the actual graphics data
                Fetch::LowByte => {
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
                Fetch::HighByte => {
                    self.bg_fifo.fetcher.tile_hi =
                        self.read_byte_direct(self.bg_fifo.fetcher.tile_addr + 1);
                }
            }
        }
        // If the FIFO is empty, decode the row of 8 pixels and push them onto it
        if self.bg_fifo.fetcher.push && self.bg_fifo.queue.is_empty() {
            for (i, &color) in self.bg_fifo.fetcher.decode_tile_row().iter().enumerate() {
                self.bg_fifo.push(Pixel {
                    color,
                    palette: self.registers.BGP,
                    priority: false,
                })
            }
            self.num_tiles_fetched += 1;
            self.bg_fifo.fetcher.reset();
        }
    }

    fn step_sprite_fetcher(&mut self) {
        if let Some(state) = self.sprite_fifo.fetcher.step_state() {
            match state {
                Fetch::TileNum => {
                    let sprite = self.current_sprite.unwrap();
                    let sprite_height = match is_bit_set(self.registers.LCDC, 2) {
                        true => 2,
                        false => 1,
                    };
                    self.sprite_fifo.fetcher.tile_num =
                        sprite.tile_num & (0xFF - sprite_height + 1);
                    let row_num = self.registers.LY + 16 - sprite.y;
                    self.sprite_fifo.fetcher.row_num = match sprite.y_flip {
                        true => 8 * sprite_height - 1 - row_num,
                        false => row_num,
                    }
                }
                Fetch::LowByte => {
                    self.sprite_fifo.fetcher.tile_addr = 0x8000
                        + 16 * self.sprite_fifo.fetcher.tile_num as u16
                        + 2 * self.sprite_fifo.fetcher.row_num as u16;
                    self.sprite_fifo.fetcher.tile_lo =
                        self.read_byte_direct(self.sprite_fifo.fetcher.tile_addr);
                }
                Fetch::HighByte => {
                    self.sprite_fifo.fetcher.tile_hi =
                        self.read_byte_direct(self.sprite_fifo.fetcher.tile_addr + 1);
                }
            }
        }
        if self.sprite_fifo.fetcher.push {
            let sprite = self.current_sprite.unwrap();
            let sprite_tile_row = self.sprite_fifo.fetcher.decode_tile_row();
            let tile_row_iter: Box<dyn Iterator<Item = _>> = match sprite.x_flip {
                true => Box::new(sprite_tile_row.iter().rev()),
                false => Box::new(sprite_tile_row.iter()),
            };
            let palette = match sprite.palette {
                true => self.registers.OBP1,
                false => self.registers.OBP0,
            };
            for (i, &color) in tile_row_iter.enumerate() {
                let pixel = Pixel {
                    color,
                    palette,
                    priority: sprite.priority,
                };
                let throw_away = if sprite.x < 8 { 8 - sprite.x } else { 0 };
                if i < throw_away as usize {
                    continue;
                }
                let i = i - throw_away as usize;
                if i < self.sprite_fifo.queue.len() {
                    if self.sprite_fifo.queue[i].color == 0 {
                        self.sprite_fifo.queue[i] = pixel;
                    }
                } else {
                    self.sprite_fifo.push(pixel)
                }
            }
            self.current_sprite = None;
            self.sprite_fifo.fetcher.reset();
        }
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
                        priority: false,
                    };
                    bg[y][x] = pixel.decode();
                }
            }
        }
        bg
    }
}
