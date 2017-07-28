extern crate glium;
extern crate glium_text_rusttype as glium_text;
extern crate cgmath;

use std::path::Path;
use std::thread;
use std::time::Duration;
use glium::Surface;
use glium::Display;
use glium::glutin::{ WindowBuilder, ContextBuilder, EventsLoop };
use glium::glutin::WindowEvent::{ Closed, ReceivedCharacter };
use glium::glutin::Event::WindowEvent;

fn main() {
    use std::fs::File;

    let mut events_loop = EventsLoop::new();
    let window = WindowBuilder::new().with_dimensions(1024, 768);
    let context = ContextBuilder::new();
    let display = Display::new(window, context, &events_loop).unwrap();

    let system = glium_text::TextSystem::new(&display);

    let font = match std::env::args().nth(1) {
        Some(file) => glium_text::FontTexture::new(&display, File::open(&Path::new(&file)).unwrap(), 70, glium_text::FontTexture::ascii_character_list()),
        None => glium_text::FontTexture::new(&display, &include_bytes!("font.ttf")[..], 70, glium_text::FontTexture::ascii_character_list()),
    }.unwrap();

    let mut buffer = String::new();

    let sleep_duration = Duration::from_millis(17);

    println!("Type with your keyboard");

    'main: loop {
        let text = glium_text::TextDisplay::new(&system, &font, &buffer);

        let (w, h) = display.get_framebuffer_dimensions();

        let matrix:[[f32; 4]; 4] = cgmath::Matrix4::new(
            0.1, 0.0, 0.0, 0.0,
            0.0, 0.1 * (w as f32) / (h as f32), 0.0, 0.0,
            0.0, 0.0, 1.0, 0.0,
            -0.9, 0.0, 0.0, 1.0f32,
        ).into();

        let mut target = display.draw();
        target.clear_color(0.0, 0.0, 0.0, 1.0);
        glium_text::draw(&text, &system, &mut target, matrix, (1.0, 1.0, 0.0, 1.0)).unwrap();
        target.finish().unwrap();

        thread::sleep(sleep_duration);

        let mut closing = false;
        events_loop.poll_events(|event| {
            if let WindowEvent { event, .. } = event {
                match event {
                    ReceivedCharacter('\r') => buffer.clear(),
                    ReceivedCharacter(c) if c as u32 == 8 => { buffer.pop(); },
                    ReceivedCharacter(chr) => buffer.push(chr),
                    Closed => {
                        closing = true;
                    },
                    _ => ()
                }
            }
        });
        if closing {
            break;
        }
    }
}
