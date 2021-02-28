use std::{io::Read, ops::Deref};

use crate::{AtlasCharacterInfos, Error, FontAtlas};

use miniquad::{
    Bindings, BlendFactor, BlendState, BlendValue, Buffer, BufferLayout, BufferType, Context,
    Equation, PassAction, Pipeline, PipelineParams, Shader, Texture, VertexAttribute, VertexFormat,
};

/// Font texture loaded to the GPU
pub struct FontTexture {
    texture: Texture,
    character_infos: AtlasCharacterInfos,
}

impl FontTexture {
    #[deprecated(
        since = "0.1.0",
        note = "Please use FontAtlas::ascii_character_list instead"
    )]
    pub fn ascii_character_list() -> Vec<char> {
        FontAtlas::ascii_character_list()
    }

    #[deprecated(
        since = "0.1.0",
        note = "Please use FontAtlas::cyrllic_character_list instead"
    )]
    pub fn cyrllic_character_list() -> Vec<char> {
        FontAtlas::cyrllic_character_list()
    }

    #[deprecated(
        since = "0.1.0",
        note = "Please use FontAtlas::thai_character_list instead"
    )]
    pub fn thai_character_list() -> Vec<char> {
        FontAtlas::thai_character_list()
    }

    #[deprecated(
        since = "0.1.0",
        note = "Please use FontAtlas::vietnamese_character_list instead"
    )]
    pub fn vietnamese_character_list() -> Vec<char> {
        FontAtlas::vietnamese_character_list()
    }

    pub fn new<R, I>(
        context: &mut Context,
        font: R,
        font_size: u32,
        characters_list: I,
    ) -> Result<FontTexture, Error>
    where
        R: Read,
        I: IntoIterator<Item = char>,
    {
        let atlas = FontAtlas::new(font, font_size, characters_list)?;

        let texture = Texture::from_rgba8(
            context,
            atlas.texture.width as u16,
            atlas.texture.height as u16,
            &atlas.texture.data[..],
        );

        Ok(FontTexture {
            texture,
            character_infos: atlas.character_infos,
        })
    }
}
#[derive(Copy, Clone, Debug)]
#[repr(C)]
struct Vertex {
    position: [f32; 2],
    tex_coords: [f32; 2],
}

/// Object that contains the elements shared by all `TextDisplay` objects.
///
/// Required to create a `TextDisplay`.
pub struct TextSystem {
    pipeline: Pipeline,
}

/// Object that will allow you to draw a text.
pub struct TextDisplay<F>
where
    F: Deref<Target = FontTexture>,
{
    texture: F,
    bindings: Option<Bindings>,
    total_text_width: f32,
    text_top: f32,
    text_bottom: f32,
    is_empty: bool,
}

mod shader {
    use miniquad::{ShaderMeta, UniformBlockLayout, UniformDesc, UniformType};

    pub const VERTEX: &str = r#"#version 100
    attribute lowp vec2 position;
    attribute lowp vec2 tex_coords;
    varying lowp vec2 v_tex_coords;
    uniform lowp mat4 matrix;

    void main() {
        gl_Position = matrix * vec4(position.x, position.y, 0.0, 1.0);
        v_tex_coords = tex_coords;

    }"#;

    pub const FRAGMENT: &str = r#"#version 100
    varying lowp vec2 v_tex_coords;
    uniform lowp vec4 color;
    uniform sampler2D tex;

    void main() {
        gl_FragColor = vec4(color.rgb, color.a * texture2D(tex, v_tex_coords));
        if (gl_FragColor.a <= 0.01) {
            discard;
        }
    }"#;

    pub fn meta() -> ShaderMeta {
        ShaderMeta {
            images: vec!["tex".to_string()],
            uniforms: UniformBlockLayout {
                uniforms: vec![
                    UniformDesc::new("matrix", UniformType::Mat4),
                    UniformDesc::new("color", UniformType::Float4),
                ],
            },
        }
    }

    #[repr(C)]
    pub struct Uniforms {
        pub matrix: [[f32; 4]; 4],
        pub color: (f32, f32, f32, f32),
    }
}

impl TextSystem {
    /// Builds a new text system that must be used to build `TextDisplay` objects.
    pub fn new(ctx: &mut Context) -> TextSystem {
        let shader = Shader::new(ctx, shader::VERTEX, shader::FRAGMENT, shader::meta()).unwrap();

        let pipeline = Pipeline::with_params(
            ctx,
            &[BufferLayout::default()],
            &[
                VertexAttribute::new("position", VertexFormat::Float2),
                VertexAttribute::new("tex_coords", VertexFormat::Float2),
            ],
            shader,
            PipelineParams {
                color_blend: Some(BlendState::new(
                    Equation::Add,
                    BlendFactor::Value(BlendValue::SourceAlpha),
                    BlendFactor::OneMinusValue(BlendValue::SourceAlpha),
                )),
                ..Default::default()
            },
        );

        TextSystem { pipeline }
    }
}

impl<F> TextDisplay<F>
where
    F: Deref<Target = FontTexture>,
{
    /// Builds a new text display that allows you to draw text.
    pub fn new(ctx: &mut Context, system: &TextSystem, texture: F, text: &str) -> TextDisplay<F> {
        let mut text_display = TextDisplay {
            texture,
            total_text_width: 0.0,
            bindings: None,
            text_top: 0.0,
            text_bottom: 0.0,
            is_empty: true,
        };

        text_display.set_text(ctx, text);

        text_display
    }

    /// Returns the width in GL units of the text.
    pub fn get_width(&self) -> f32 {
        self.total_text_width
    }

    /// Returns the height in GL units of the text.
    pub fn get_height(&self) -> f32 {
        self.text_top
    }

    /// Returns the bottom point in GL units of the text.
    pub fn get_bottom(&self) -> f32 {
        self.text_bottom
    }
    /// Modifies the text on this display.
    pub fn set_text(&mut self, ctx: &mut Context, text: &str) {
        self.is_empty = true;
        self.total_text_width = 0.0;
        self.clear();

        // returning if no text
        if text.is_empty() {
            return;
        }

        // these arrays will contain the vertex buffer and index buffer data
        let mut vertex_buffer_data = Vec::with_capacity(text.len() * 4 * 4);
        let mut index_buffer_data = Vec::with_capacity(text.len() * 6);

        // iterating over the characters of the string
        for character in text.chars() {
            let infos = match self.texture.character_infos.get(&character) {
                Some(infos) => infos,
                None => continue,
            };

            self.is_empty = false;

            // adding the quad in the index buffer
            {
                let first_vertex_offset = vertex_buffer_data.len() as u16;
                index_buffer_data.push(first_vertex_offset);
                index_buffer_data.push(first_vertex_offset + 1);
                index_buffer_data.push(first_vertex_offset + 2);
                index_buffer_data.push(first_vertex_offset + 2);
                index_buffer_data.push(first_vertex_offset + 1);
                index_buffer_data.push(first_vertex_offset + 3);
            }

            //
            self.total_text_width += infos.left_padding;

            // calculating coords
            let left_coord = self.total_text_width;
            let right_coord = left_coord + infos.size.0;
            let top_coord = infos.height_over_line;
            let bottom_coord = infos.height_over_line - infos.size.1;

            // top-left vertex
            vertex_buffer_data.push(Vertex {
                position: [left_coord, top_coord],
                tex_coords: [infos.tex_coords.0, infos.tex_coords.1],
            });

            // top-right vertex
            vertex_buffer_data.push(Vertex {
                position: [right_coord, top_coord],
                tex_coords: [infos.tex_coords.0 + infos.tex_size.0, infos.tex_coords.1],
            });

            // bottom-left vertex
            vertex_buffer_data.push(Vertex {
                position: [left_coord, bottom_coord],
                tex_coords: [infos.tex_coords.0, infos.tex_coords.1 + infos.tex_size.1],
            });

            // bottom-right vertex
            vertex_buffer_data.push(Vertex {
                position: [right_coord, bottom_coord],
                tex_coords: [
                    infos.tex_coords.0 + infos.tex_size.0,
                    infos.tex_coords.1 + infos.tex_size.1,
                ],
            });

            // going to next char
            self.total_text_width = right_coord + infos.right_padding;

            if top_coord > self.text_top {
                self.text_top = top_coord;
            }

            if bottom_coord < self.text_bottom {
                self.text_bottom = bottom_coord
            }
        }

        if !vertex_buffer_data.len() != 0 {
            // building the vertex buffer
            let vertex_buffer =
                Buffer::immutable(ctx, BufferType::VertexBuffer, &vertex_buffer_data);

            // building the index buffer
            let index_buffer = Buffer::immutable(ctx, BufferType::IndexBuffer, &index_buffer_data);

            self.bindings = Some(Bindings {
                vertex_buffers: vec![vertex_buffer],
                index_buffer: index_buffer,
                images: vec![self.texture.texture],
            });
        }
    }

    fn clear(&mut self) {
        if let Some(bindings) = self.bindings.take() {
            bindings.vertex_buffers[0].delete();
            bindings.index_buffer.delete();
        }
    }
}

impl<F> std::ops::Drop for TextDisplay<F>
where
    F: Deref<Target = FontTexture>,
{
    fn drop(&mut self) {
        self.clear();
    }
}

/// Draws linear-filtered text.
///
/// ## About the matrix
///
/// The matrix must be column-major post-muliplying (which is the usual way to do in OpenGL).
///
/// One unit in height corresponds to a line of text, but the text can go above or under.
/// The bottom of the line is at `0.0`, the top is at `1.0`.
/// You need to adapt your matrix by taking these into consideration.
pub fn draw<F, M>(
    ctx: &mut Context,
    text: &TextDisplay<F>,
    system: &TextSystem,
    matrix: M,
    color: (f32, f32, f32, f32),
) where
    M: Into<[[f32; 4]; 4]>,
    F: Deref<Target = FontTexture>,
{
    if let Some(ref bindings) = text.bindings {
        ctx.begin_default_pass(PassAction::Nothing);
        ctx.apply_pipeline(&system.pipeline);
        ctx.apply_bindings(bindings);
        ctx.apply_uniforms(&shader::Uniforms {
            matrix: matrix.into(),
            color,
        });
        ctx.draw(0, bindings.index_buffer.size() as i32 / 2, 1);
        ctx.end_render_pass();
    }
}
