use anyhow::{Error, Result};
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::Color;
use sdl2::rect::Rect;
use sdl2::render::Canvas;
use sdl2::video::Window;
use std::time::{Duration, Instant};

// pub const W_WIDTH: usize = 256;
// pub const W_HEIGHT: usize = 256;
pub const W_WIDTH: usize = 160;
pub const W_HEIGHT: usize = 144;
const W_SCALE: usize = 2;
const LIMIT_FRAMERATE: bool = true;

pub enum DisplayEvent {
    KeyEvent((String, bool)),
    Quit,
    None,
}

pub struct Display {
    frames: u64,
    time: Instant,
    last_frame: Instant,
    event_pump: sdl2::EventPump,
    canvas: Canvas<Window>,
}

impl Display {
    pub fn new() -> Result<Self> {
        let context = sdl2::init().map_err(Error::msg)?;
        Ok(Display {
            frames: 0,
            time: Instant::now(),
            last_frame: Instant::now(),
            event_pump: context.event_pump().map_err(Error::msg)?,
            canvas: context
                .video()
                .map_err(Error::msg)?
                .window(
                    "gb-emu",
                    (W_WIDTH * W_SCALE) as u32,
                    (W_HEIGHT * W_SCALE) as u32,
                )
                .position_centered()
                .build()?
                .into_canvas()
                .build()?,
        })
    }

    pub fn draw(&mut self, pixels: [[Color; W_WIDTH]; W_HEIGHT]) {
        self.canvas.set_draw_color(Color::WHITE);
        self.canvas.clear();
        for (i, row) in pixels.iter().enumerate() {
            for (j, &color) in row.iter().enumerate() {
                match color {
                    Color::WHITE => continue,
                    _ => {
                        self.canvas.set_draw_color(color);
                        self.canvas
                            .fill_rect(Rect::new(
                                (j * W_SCALE) as i32,
                                (i * W_SCALE) as i32,
                                W_SCALE as u32,
                                W_SCALE as u32,
                            ))
                            .unwrap()
                    }
                }
            }
        }
        self.canvas.present();
        self.frames += 1;
        if LIMIT_FRAMERATE {
            while Instant::now().duration_since(self.last_frame)
                < Duration::from_secs_f64(70224.0 / 4194304.0)
            {
                continue;
            }
        }
        self.last_frame = Instant::now();
        let time_elapsed = Instant::now().duration_since(self.time);
        if time_elapsed > Duration::from_secs(1) {
            // println!("{}", self.frames);
            self.frames = 0;
            self.time = Instant::now();
        }
    }

    pub fn poll_event(&mut self) -> DisplayEvent {
        for event in self.event_pump.poll_iter() {
            match event {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => return DisplayEvent::Quit,
                Event::KeyDown {
                    keycode: Some(x),
                    repeat: false,
                    ..
                } => return DisplayEvent::KeyEvent((x.name(), true)),
                Event::KeyUp {
                    keycode: Some(x), ..
                } => return DisplayEvent::KeyEvent((x.name(), false)),
                _ => continue,
            }
        }
        DisplayEvent::None
    }
}
