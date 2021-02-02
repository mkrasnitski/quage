use anyhow::{Error, Result};
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::Color;
use sdl2::render::Canvas;
use sdl2::video::Window;

pub enum DisplayEvent {
    KeyEvent((String, bool)),
    Quit,
    None,
}

pub struct Display {
    event_pump: sdl2::EventPump,
    pub canvas: Canvas<Window>,
}

impl Display {
    pub fn new(w: u32, h: u32) -> Result<Self> {
        let context = sdl2::init().map_err(Error::msg)?;
        Ok(Display {
            event_pump: context.event_pump().map_err(Error::msg)?,
            canvas: context
                .video()
                .map_err(Error::msg)?
                .window("gb-emu", w, h)
                .position_centered()
                .build()?
                .into_canvas()
                .build()?,
        })
    }

    pub fn draw(&mut self) {
        self.canvas.set_draw_color(Color::WHITE);
        self.canvas.clear();
        self.canvas.present();
    }

    pub fn poll_events(&mut self) -> DisplayEvent {
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
                _ => {}
            }
        }
        DisplayEvent::None
    }
}

// fn decode_tile_row(hi: u8, lo: u8) -> [u8; 8] {
//     let mut res = [0; 8];
//     for i in 0..8 {
//         res[i] = (((hi >> i) & 1) << 1) | ((lo >> i) & 1)
//     }
//     res
// }
