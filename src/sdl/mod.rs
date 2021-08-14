use anyhow::{Error, Result};
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::{Color, PixelFormatEnum};
use sdl2::render::Canvas;
use sdl2::video::Window;
use sdl2::VideoSubsystem as Video;
use spin_sleep::LoopHelper;

use crate::config::Config;
use crate::hotkeys::{Hotkey, Keymap};

pub const W_WIDTH: usize = 160;
pub const W_HEIGHT: usize = 144;
const W_SCALE: u32 = 3;
const FRAMERATE: f64 = 4194304.0 / 70224.0;

pub enum SDLEvent {
    HotkeyEvent((Hotkey, bool)),
    Quit,
    None,
}

pub struct SDLManager {
    pub display: Display<W_WIDTH, W_HEIGHT>,
    pub tile_display: Option<Display<128, 192>>,
    event_pump: sdl2::EventPump,
    hotkey_map: Keymap,
}

impl SDLManager {
    pub fn new(config: &Config) -> Result<Self> {
        let context = sdl2::init().map_err(Error::msg)?;
        let video = context.video().map_err(Error::msg)?;
        let event_pump = context.event_pump().map_err(Error::msg)?;
        let display = Display::new(&video, None, config.show_fps)?;
        let tile_display = config
            .dump_tiles
            .then(|| Display::new(&video, Some((1220, 250)), false))
            .transpose()?;
        Ok(SDLManager {
            display,
            tile_display,
            event_pump,
            hotkey_map: Keymap::new(&config.hotkey_file)?,
        })
    }

    pub fn toggle_frame_limiter(&mut self) {
        self.display.toggle_frame_limiter();
        if let Some(tile_display) = self.tile_display.as_mut() {
            tile_display.toggle_frame_limiter();
        }
    }

    pub fn poll_event(&mut self) -> SDLEvent {
        for event in self.event_pump.poll_iter() {
            match event {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => return SDLEvent::Quit,
                Event::KeyDown {
                    keycode: Some(k),
                    keymod: mods,
                    repeat: false,
                    ..
                } => {
                    if let Some(hotkey) = self.hotkey_map.get_hotkey(k, mods) {
                        return SDLEvent::HotkeyEvent((hotkey, true));
                    }
                }
                Event::KeyUp {
                    keycode: Some(k),
                    keymod: mods,
                    repeat: false,
                    ..
                } => {
                    // This has a limitation, in that KeyUp events also have to have
                    // the same modifiers active in order to register the same Hotkey.
                    // Moral of the story: Don't use modifiers for Joypad bindings,
                    // where we care about KeyUp.
                    if let Some(hotkey) = self.hotkey_map.get_hotkey(k, mods) {
                        return SDLEvent::HotkeyEvent((hotkey, false));
                    }
                }
                _ => continue,
            }
        }
        SDLEvent::None
    }
}

pub struct Display<const W: usize, const H: usize> {
    limit_framerate: bool,
    show_fps: bool,
    canvas: Canvas<Window>,
    frame_limiter: LoopHelper,
}

impl<const W: usize, const H: usize> Display<W, H> {
    pub fn new(video: &Video, position: Option<(i32, i32)>, show_fps: bool) -> Result<Self> {
        let mut window = video.window("quage", W_SCALE * W as u32, W_SCALE * H as u32);
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
