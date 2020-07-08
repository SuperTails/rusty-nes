use crate::APUAudio;
use sdl2::audio::AudioDevice;
use sdl2::video::Window;
use sdl2::{AudioSubsystem, EventPump, Sdl, VideoSubsystem};

pub struct SDLSystem {
    pub ctx: Sdl,
    pub video_subsystem: VideoSubsystem,
    pub audio_subsystem: AudioSubsystem,
    pub audio_device: Option<AudioDevice<APUAudio>>,
    pub window: Window,
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
        let event_pump = ctx.event_pump().unwrap();
        SDLSystem {
            ctx,
            video_subsystem,
            audio_subsystem,
            window,
            event_pump,
            audio_device: None,
        }
    }
}

impl Default for SDLSystem {
    fn default() -> SDLSystem {
        SDLSystem::new()
    }
}
