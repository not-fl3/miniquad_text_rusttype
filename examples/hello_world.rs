use miniquad::{conf, Context, EventHandler};
use miniquad_text_rusttype as quad_text;
use std::rc::Rc;

struct Stage {
    system: quad_text::TextSystem,
    text: quad_text::TextDisplay<Rc<quad_text::FontTexture>>,
}

impl EventHandler for Stage {
    fn update(&mut self, _ctx: &mut Context) {}

    fn draw(&mut self, ctx: &mut Context) {
        let (w, h) = ctx.screen_size();

        ctx.clear(Some((0., 0., 0., 1.)), None, None);

        let text_width = self.text.get_width();
        #[rustfmt::skip]
        let matrix:[[f32; 4]; 4] = glam::Mat4::from_cols_array(&[
            2.0 / text_width, 0.0, 0.0, 0.0,
            0.0, 2.0 * (w as f32) / (h as f32) / text_width, 0.0, 0.0,
            0.0, 0.0, 1.0, 0.0,
            -1.0, -1.0, 0.0, 1.0f32,
        ]).to_cols_array_2d();
        quad_text::draw(ctx, &self.text, &self.system, matrix, (1.0, 1.0, 0.0, 1.0));
    }
}

fn main() {
    miniquad::start(conf::Conf::default(), |mut ctx| {
        let system = quad_text::TextSystem::new(&mut ctx);
        let font = quad_text::FontTexture::new(
            &mut ctx,
            &include_bytes!("font.ttf")[..],
            70,
            quad_text::FontAtlas::ascii_character_list(),
        )
        .unwrap();
        let text = quad_text::TextDisplay::new(&mut ctx, &system, Rc::new(font), "Hello world!");

        let text_width = text.get_width();
        println!("Text width: {:?}", text_width);

        miniquad::UserData::owning(Stage { system, text }, ctx)
    });
}
