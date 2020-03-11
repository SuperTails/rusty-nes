use crate::APUAudio;
use sdl2::audio::AudioDevice;
use sdl2::render::WindowCanvas;
use sdl2::{AudioSubsystem, EventPump, Sdl, VideoSubsystem};

pub struct SDLSystem {
    pub ctx: Sdl,
    pub video_subsystem: VideoSubsystem,
    pub audio_subsystem: AudioSubsystem,
    pub audio_device: Option<AudioDevice<APUAudio>>,
    pub canvas: WindowCanvas,
    pub event_pump: EventPump,
}

impl SDLSystem {
    pub fn new() -> SDLSystem {
        let ctx = sdl2::init().unwrap();
        let video_subsystem = ctx.video().unwrap();
        let audio_subsystem = ctx.audio().unwrap();
        let window = video_subsystem
            .window("Terrible NES", 1024 + 256, 480)
            .position_centered()
            .build()
            .unwrap();
        let canvas = window.into_canvas().build().unwrap();
        let event_pump = ctx.event_pump().unwrap();
        SDLSystem {
            ctx,
            video_subsystem,
            audio_subsystem,
            canvas,
            event_pump,
            audio_device: None,
        }
    }

    pub fn present(&mut self) {
        self.canvas.present();
    }

    pub fn canvas(&mut self) -> &mut WindowCanvas {
        &mut self.canvas
    }
}

impl Default for SDLSystem {
    fn default() -> SDLSystem {
        SDLSystem::new()
    }
}
