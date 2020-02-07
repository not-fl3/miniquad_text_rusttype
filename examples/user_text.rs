use miniquad::{conf, Context, EventHandler, KeyCode, KeyMods};
use miniquad_text_rusttype as quad_text;

struct Stage {
    buffer: String,
    system: quad_text::TextSystem,
    font: quad_text::FontTexture,
}

impl EventHandler for Stage {
    fn update(&mut self, _ctx: &mut Context) {}

    fn key_down_event(&mut self, _: &mut Context, character: KeyCode, _: KeyMods, _: bool) {
        if character == KeyCode::Backspace {
            self.buffer.pop();
        }
    }

    fn char_event(&mut self, _: &mut Context, character: char, _: KeyMods, _: bool) {
        self.buffer.push(character);
    }

    fn draw(&mut self, ctx: &mut Context) {
        let (w, h) = ctx.screen_size();

        ctx.clear(Some((0., 0., 0., 1.)), None, None);

        let text = quad_text::TextDisplay::new(
            ctx,
            &self.system,
            &self.font,
            &format!("> {}", self.buffer),
        );

        for i in 0..200 {
            #[rustfmt::skip]
            let matrix:[[f32; 4]; 4] = glam::Mat4::from_cols_array(&[
                0.1, 0.0, 0.0, 0.0,
                0.0, 0.1 * (w as f32) / (h as f32), 0.0, 0.0,
                0.0, 0.0, 1.0, 0.0,
                -0.9, -1. + i as f32 / 100., 0.0, 1.0f32,
            ]).to_cols_array_2d();

            quad_text::draw(ctx, &text, &self.system, matrix, (1.0, 1.0, 0.0, 1.0));
        }
    }
}

fn main() {
    miniquad::start(conf::Conf::default(), |ctx| {
        let system = quad_text::TextSystem::new(ctx);
        let font = quad_text::FontTexture::new(
            ctx,
            &include_bytes!("font.ttf")[..],
            70,
            quad_text::FontTexture::cyrllic_character_list(),
        )
        .unwrap();

        let buffer = String::new();

        Box::new(Stage {
            buffer,
            font,
            system,
        })
    });
}
