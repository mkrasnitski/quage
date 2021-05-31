use anyhow::{Error, Result};
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::Color;
use sdl2::rect::Rect;
use sdl2::render::Canvas;
use sdl2::video::Window;
use std::time::{Duration, Instant};

pub const W_WIDTH: usize = 160;
pub const W_HEIGHT: usize = 144;
const W_SCALE: usize = 3;

pub struct DisplayManager {
    context: sdl2::Sdl,
    event_pump: sdl2::EventPump,
}

impl DisplayManager {
    pub fn new() -> Result<Self> {
        let context = sdl2::init().map_err(Error::msg)?;
        let event_pump = context.event_pump().map_err(Error::msg)?;
        Ok(DisplayManager {
            context,
            event_pump,
        })
    }

    pub fn new_display<const W: usize, const H: usize>(
        &self,
        position: (i32, i32),
        show_fps: bool,
    ) -> Result<Display<W, H>> {
        Display::new(&self.context, position, show_fps)
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

pub enum DisplayEvent {
    KeyEvent((String, bool)),
    Quit,
    None,
}

pub struct Display<const W: usize, const H: usize> {
    frames: u64,
    time: Instant,
    last_frame: Instant,
    limit_framerate: bool,
    show_fps: bool,
    canvas: Canvas<Window>,
}

impl<const W: usize, const H: usize> Display<W, H> {
    pub fn new(context: &sdl2::Sdl, position: (i32, i32), show_fps: bool) -> Result<Self> {
        let (x, y) = position;
        Ok(Display {
            frames: 0,
            time: Instant::now(),
            last_frame: Instant::now(),
            limit_framerate: true,
            show_fps,
            canvas: context
                .video()
                .map_err(Error::msg)?
                .window("gb-emu", (W * W_SCALE) as u32, (H * W_SCALE) as u32)
                .position(x, y)
                .build()?
                .into_canvas()
                .build()?,
        })
    }

    pub fn toggle_frame_limiter(&mut self) {
        self.limit_framerate = !self.limit_framerate;
    }

    pub fn draw(&mut self, pixels: &[[Color; W]; H]) {
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
        if self.limit_framerate {
            let passed = Instant::now().duration_since(self.last_frame);
            let frame_time = Duration::from_secs_f64(70224.0 / 4194304.0);
            if passed < frame_time {
                std::thread::sleep(frame_time - passed);
            }
        }
        self.last_frame = Instant::now();
        let time_elapsed = Instant::now().duration_since(self.time);
        if time_elapsed > Duration::from_secs(1) {
            if self.show_fps {
                println!("{}", self.frames);
            }
            self.frames = 0;
            self.time = Instant::now();
        }
    }
}
