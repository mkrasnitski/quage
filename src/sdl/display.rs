use anyhow::Result;
use sdl2::pixels::{Color, PixelFormatEnum};
use sdl2::render::Canvas;
use sdl2::video::Window;
use sdl2::VideoSubsystem as Video;
use spin_sleep::LoopHelper;

const W_SCALE: u32 = 3;
const FRAMERATE: f64 = 4194304.0 / 70224.0;

pub struct Display<const W: usize, const H: usize> {
    limit_framerate: bool,
    speed: f64,
    show_fps: bool,
    canvas: Canvas<Window>,
    frame_limiter: LoopHelper,
}

impl<const W: usize, const H: usize> Display<W, H> {
    pub fn new(video: &Video, position: Option<(i32, i32)>, show_fps: bool) -> Result<Self> {
        let mut window = video.window("quage", W_SCALE * W as u32, W_SCALE * H as u32);
        let mut canvas = match position {
            Some((x, y)) => window.position(x, y),
            None => window.position_centered(),
        }
        .build()?
        .into_canvas()
        .build()?;
        canvas.set_logical_size(W as u32, H as u32)?;
        Ok(Display {
            limit_framerate: true,
            speed: 1.0,
            show_fps,
            canvas,
            frame_limiter: LoopHelper::builder()
                .report_interval_s(1.0)
                .build_with_target_rate(FRAMERATE),
        })
    }

    pub fn toggle_frame_limiter(&mut self) {
        self.limit_framerate = !self.limit_framerate;
    }

    pub fn scale_framerate(&mut self, delta: f64) {
        self.speed += delta;
        println!("Speed {:.0}%", 100.0 * self.speed);
        self.frame_limiter.set_target_rate(FRAMERATE * self.speed);
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
