use anyhow::{Error, Result};
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::{Color, PixelFormatEnum};
use sdl2::render::Canvas;
use sdl2::video::Window;
use spin_sleep::LoopHelper;

pub const W_WIDTH: usize = 160;
pub const W_HEIGHT: usize = 144;
const W_SCALE: usize = 3;
const FRAMERATE: f64 = 4194304.0 / 70224.0;

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
        position: Option<(i32, i32)>,
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
    limit_framerate: bool,
    show_fps: bool,
    canvas: Canvas<Window>,
    frame_limiter: LoopHelper,
}

impl<const W: usize, const H: usize> Display<W, H> {
    pub fn new(context: &sdl2::Sdl, position: Option<(i32, i32)>, show_fps: bool) -> Result<Self> {
        let mut window = context.video().map_err(Error::msg)?.window(
            "gb-emu",
            (W * W_SCALE) as u32,
            (H * W_SCALE) as u32,
        );
        Ok(Display {
            limit_framerate: true,
            show_fps,
            canvas: match position {
                Some((x, y)) => window.position(x, y),
                None => window.position_centered(),
            }
            .build()?
            .into_canvas()
            .build()?,
            frame_limiter: LoopHelper::builder()
                .report_interval_s(1.0)
                .build_with_target_rate(FRAMERATE),
        })
    }

    pub fn toggle_frame_limiter(&mut self) {
        self.limit_framerate = !self.limit_framerate;
    }

    pub fn draw(&mut self, pixels: &[[Color; W]; H]) {
        let texture_creator = self.canvas.texture_creator();
        let mut texture = texture_creator
            .create_texture_streaming(PixelFormatEnum::RGB24, W as u32, H as u32)
            .unwrap();
        texture
            .with_lock(None, |buffer: &mut [u8], pitch: usize| {
                for (i, row) in pixels.iter().enumerate() {
                    for (j, &color) in row.iter().enumerate() {
                        let offset = i * pitch + j * 3;
                        let (r, g, b) = color.rgb();
                        buffer[offset] = r;
                        buffer[offset + 1] = g;
                        buffer[offset + 2] = b;
                    }
                }
            })
            .unwrap();
        self.canvas.clear();
        self.canvas.copy(&texture, None, None).unwrap();
        self.canvas.present();
        if let Some(fps) = self.frame_limiter.report_rate() {
            if self.show_fps {
                println!("{:.4}", fps);
            }
        }
        if self.limit_framerate {
            self.frame_limiter.loop_sleep();
        }
        self.frame_limiter.loop_start();
    }
}
