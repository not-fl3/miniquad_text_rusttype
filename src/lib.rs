/*!

This crate allows you to easily write text.

Usage:

```no_run
# extern crate glium;
# extern crate glium_text_rusttype as glium_text;
# extern crate cgmath;
# fn main() {
# let display: glium::Display = unsafe { std::mem::uninitialized() };
// The `TextSystem` contains the shaders and elements used for text display.
let system = glium_text::TextSystem::new(&display);

// Creating a `FontTexture`, which a regular `Texture` which contains the font.
// Note that loading the systems fonts is not covered by this library.
let font = glium_text::FontTexture::new(&display, std::fs::File::open(&std::path::Path::new("my_font.ttf")).unwrap(), 24, glium_text::FontTexture::ascii_character_list()).unwrap();

// Creating a `TextDisplay` which contains the elements required to draw a specific sentence.
let text = glium_text::TextDisplay::new(&system, &font, "Hello world!");

// Finally, drawing the text is done like this:
let matrix = [[1.0, 0.0, 0.0, 0.0],
              [0.0, 1.0, 0.0, 0.0],
              [0.0, 0.0, 1.0, 0.0],
              [0.0, 0.0, 0.0, 1.0]];
glium_text::draw(&text, &system, &mut display.draw(), matrix, (1.0, 1.0, 0.0, 1.0));
# }
```

*/

#![warn(missing_docs)]

extern crate rusttype;
#[macro_use]
extern crate glium;

use std::borrow::Cow;
use std::collections::HashMap;
use std::default::Default;
use std::io::Read;
use std::ops::Deref;
use std::rc::Rc;

use glium::DrawParameters;
use glium::backend::Context;
use glium::backend::Facade;

/// Texture which contains the characters of the font.
pub struct FontTexture {
    texture: glium::texture::Texture2d,
    character_infos: HashMap<char, CharacterInfos>,
}

/// Object that contains the elements shared by all `TextDisplay` objects.
///
/// Required to create a `TextDisplay`.
pub struct TextSystem {
    context: Rc<Context>,
    program: glium::Program,
}

/// Object that will allow you to draw a text.
pub struct TextDisplay<F> where F: Deref<Target=FontTexture> {
    context: Rc<Context>,
    texture: F,
    vertex_buffer: Option<glium::VertexBuffer<VertexFormat>>,
    index_buffer: Option<glium::IndexBuffer<u16>>,
    total_text_width: f32,
    text_height: f32,
    is_empty: bool,
}

// structure containing informations about a character of a font
#[derive(Copy, Clone, Debug)]
struct CharacterInfos {
    // coordinates of the character top-left hand corner on the font's texture
    tex_coords: (f32, f32),

    // width and height of character in texture units
    tex_size: (f32, f32),

    // size of the character in EMs
    size: (f32, f32),

    // number of EMs between the bottom of the character and the base line of text
    height_over_line: f32,

    // number of EMs at the left of the character
    left_padding: f32,

    // number of EMs at the right of the character
    right_padding: f32,
}

struct TextureData {
    data: Vec<f32>,
    width: u32,
    height: u32,
}

impl<'a> glium::texture::Texture2dDataSource<'a> for &'a TextureData {
    type Data = f32;

    fn into_raw(self) -> glium::texture::RawImage2d<'a, f32> {
        glium::texture::RawImage2d {
            data: Cow::Borrowed(&self.data),
            width: self.width,
            height: self.height,
            format: glium::texture::ClientFormat::F32,
        }
    }
}

#[derive(Copy, Clone)]
struct VertexFormat {
    position: [f32; 2],
    tex_coords: [f32; 2],
}

implement_vertex!(VertexFormat, position, tex_coords);

impl FontTexture {
    /// Vec<char> of complete ASCII range (from 0 to 255 bytes)
    pub fn ascii_character_list() -> Vec<char> {
        (0 .. 255).filter_map(|c| ::std::char::from_u32(c)).collect()
    }

    /// Creates a new texture representing a font stored in a `FontTexture`.
    pub fn new<R, F, I>(facade: &F, font: R, font_size: u32, characters_list: I)
                        -> Result<FontTexture, ()>
        where R: Read, F: Facade, I: IntoIterator<Item=char>
    {

        // building the freetype face object
        let font: Vec<u8> = font.bytes().map(|c| c.unwrap()).collect();

        let collection = ::rusttype::FontCollection::from_bytes(&font[..]);
        let font = collection.into_font().unwrap();

        // building the infos
        let (texture_data, chr_infos) =
            build_font_image(font, characters_list.into_iter().collect(), font_size);

        // we load the texture in the display
        let texture = glium::texture::Texture2d::new(facade, &texture_data).unwrap();

        Ok(FontTexture {
            texture: texture,
            character_infos: chr_infos.into_iter().collect(),
        })
    }
}

/*impl glium::uniforms::AsUniformValue for FontTexture {
    fn as_uniform_value(&self) -> glium::uniforms::UniformValue {
        glium::uniforms::AsUniformValue::as_uniform_value(&self.texture)
    }
}*/

impl TextSystem {
    /// Builds a new text system that must be used to build `TextDisplay` objects.
    pub fn new<F>(facade: &F) -> TextSystem where F: Facade {
        TextSystem {
            context: facade.get_context().clone(),
            program: program!(facade,
                140 => {
                    vertex: "
                        #version 140

                        uniform mat4 matrix;
                        in vec2 position;
                        in vec2 tex_coords;

                        out vec2 v_tex_coords;

                        void main() {
                            gl_Position = matrix * vec4(position, 0.0, 1.0);
                            v_tex_coords = tex_coords;
                        }
                    ",
                    fragment: "
                        #version 140
                        in vec2 v_tex_coords;
                        out vec4 f_color;
                        uniform vec4 color;
                        uniform sampler2D tex;
                        void main() {
                            vec4 c = vec4(color.rgb, color.a * texture(tex, v_tex_coords));
                            if (c.a <= 0.01) {
                                discard;
                            } else {
                                f_color = c;
                            }
                        }
                    "
                },

                110 => {
                    vertex: "
                        #version 110

                        attribute vec2 position;
                        attribute vec2 tex_coords;
                        varying vec2 v_tex_coords;
                        uniform mat4 matrix;

                        void main() {
                            gl_Position = matrix * vec4(position.x, position.y, 0.0, 1.0);
                            v_tex_coords = tex_coords;
                        }
                    ",
                    fragment: "
                        #version 110

                        varying vec2 v_tex_coords;
                        uniform vec4 color;
                        uniform sampler2D tex;

                        void main() {
                            gl_FragColor = vec4(color.rgb, color.a * texture2D(tex, v_tex_coords));
                            if (gl_FragColor.a <= 0.01) {
                                discard;
                            }
                        }
                    "
                },

            ).unwrap()
        }
    }
}

impl<F> TextDisplay<F> where F: Deref<Target=FontTexture> {
    /// Builds a new text display that allows you to draw text.
    pub fn new(system: &TextSystem, texture: F, text: &str) -> TextDisplay<F> {
        let mut text_display = TextDisplay {
            context: system.context.clone(),
            texture: texture,
            vertex_buffer: None,
            index_buffer: None,
            total_text_width: 0.0,
            text_height: 0.0,
            is_empty: true,
        };

        text_display.set_text(text);

        text_display
    }

    /// Returns the width in GL units of the text.
    pub fn get_width(&self) -> f32 {
        self.total_text_width
    }

    /// Returns the height in GL units of the text.
    pub fn get_height(&self) -> f32 {
        self.text_height
    }

    /// Modifies the text on this display.
    pub fn set_text(&mut self, text: &str) {
        self.is_empty = true;
        self.total_text_width = 0.0;
        self.vertex_buffer = None;
        self.index_buffer = None;

        // returning if no text
        if text.len() == 0 {
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
            vertex_buffer_data.push(VertexFormat {
                position: [left_coord, top_coord],
                tex_coords: [infos.tex_coords.0, infos.tex_coords.1],
            });

            // top-right vertex
            vertex_buffer_data.push(VertexFormat {
                position: [right_coord, top_coord],
                tex_coords: [infos.tex_coords.0 + infos.tex_size.0, infos.tex_coords.1],
            });

            // bottom-left vertex
            vertex_buffer_data.push(VertexFormat {
                position: [left_coord, bottom_coord],
                tex_coords: [infos.tex_coords.0, infos.tex_coords.1 + infos.tex_size.1],
            });

            // bottom-right vertex
            vertex_buffer_data.push(VertexFormat {
                position: [right_coord, bottom_coord],
                tex_coords: [
                    infos.tex_coords.0 + infos.tex_size.0,
                    infos.tex_coords.1 + infos.tex_size.1
                ],
            });

            // going to next char
            self.total_text_width = right_coord + infos.right_padding;

            if top_coord > self.text_height {
                self.text_height = top_coord;
            }
        }

        if !vertex_buffer_data.len() != 0 {
            // building the vertex buffer
            self.vertex_buffer = Some(glium::VertexBuffer::new(&self.context,
                                                               &vertex_buffer_data).unwrap());

            // building the index buffer
            self.index_buffer = Some(glium::IndexBuffer::new(&self.context,
                                     glium::index::PrimitiveType::TrianglesList,
                                     &index_buffer_data).unwrap());
        }
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
pub fn draw<F, S: ?Sized, M>(
    text: &TextDisplay<F>,
    system: &TextSystem,
    target: &mut S,
    matrix: M,
    color: (f32, f32, f32, f32)
) -> Result<(), glium::DrawError>
    where S: glium::Surface,
          M: Into<[[f32; 4]; 4]>,
          F: Deref<Target=FontTexture>
{
    let behavior = glium::uniforms::SamplerBehavior {
        magnify_filter: glium::uniforms::MagnifySamplerFilter::Linear,
        minify_filter: glium::uniforms::MinifySamplerFilter::Linear,
        .. Default::default()
    };

    let params = {
        use glium::BlendingFunction::Addition;
        use glium::LinearBlendingFactor::*;

        let blending_function = Addition {
            source: SourceAlpha,
            destination: OneMinusSourceAlpha
        };

        let blend = glium::Blend {
            color: blending_function,
            alpha: blending_function,
            constant_value: (1.0, 1.0, 1.0, 1.0),
        };

        DrawParameters {
            blend: blend,
            .. Default::default()
        }
    };
    draw_with_params(text, system, target, matrix, color, behavior, params)
}

/// More advanced variant of `draw` which also takes sampler behavior and draw
/// parameters.
pub fn draw_with_params<F, S: ?Sized, M>(
    text: &TextDisplay<F>,
    system: &TextSystem,
    target: &mut S,
    matrix: M,
    color: (f32, f32, f32, f32),
    sampler_behavior: glium::uniforms::SamplerBehavior,
    parameters: DrawParameters
) -> Result<(), glium::DrawError>
    where S: glium::Surface,
          M: Into<[[f32; 4]; 4]>,
          F: Deref<Target=FontTexture>
{
    let matrix = matrix.into();

    let &TextDisplay {
        ref vertex_buffer,
        ref index_buffer,
        ref texture,
        is_empty,
        ..
    } = text;
    let color = [color.0, color.1, color.2, color.3];

    // returning if nothing to draw
    if is_empty || vertex_buffer.is_none() || index_buffer.is_none() {
        return Ok(());
    }

    let vertex_buffer = vertex_buffer.as_ref().unwrap();
    let index_buffer = index_buffer.as_ref().unwrap();

    let uniforms = uniform! {
        matrix: matrix,
        color: color,
        tex: glium::uniforms::Sampler(&texture.texture, sampler_behavior)
    };

    target.draw(vertex_buffer, index_buffer, &system.program, &uniforms, &parameters)
}

fn build_font_image(font: rusttype::Font, characters_list: Vec<char>, font_size: u32)
                    -> (TextureData, Vec<(char, CharacterInfos)>)
{
    use std::iter;

    // a margin around each character to prevent artifacts
    const MARGIN: u32 = 2;

    // this variable will store the texture data
    // we set an arbitrary capacity that we think will match what we will need
    let mut texture_data: Vec<f32> = Vec::with_capacity(characters_list.len() *
                                                        font_size as usize * font_size as usize);

    // the width is chosen more or less arbitrarily, because we can store everything as long as
    //  the texture is at least as wide as the widest character
    // we just try to estimate a width so that width ~= height
    let texture_width = get_nearest_po2(std::cmp::max(font_size * 2 as u32,
        ((((characters_list.len() as u32) * font_size * font_size) as f32).sqrt()) as u32));

    // we store the position of the "cursor" in the destination texture
    // this cursor points to the top-left pixel of the next character to write on the texture
    let mut cursor_offset = (0u32, 0u32);

    // number of rows to skip at next carriage return
    let mut rows_to_skip = 0u32;

    // now looping through the list of characters, filling the texture and returning the informations
    let mut em_pixels = font_size as f32;
    let mut characters_infos: Vec<(char, CharacterInfos)> = characters_list.into_iter().filter_map(|character| {
        struct Bitmap {
            rows   : i32,
            width  : i32,
            buffer : Vec<u8>
        }
        // loading wanted glyph in the font face
        // hope scale will set the right pixel size
        let scaled_glyph = font.glyph(character).expect("no glyph for char")
            .scaled(::rusttype::Scale {x : font_size as f32, y : font_size as f32 });
        let h_metrics = scaled_glyph.h_metrics();
        let glyph = scaled_glyph
            .positioned(::rusttype::Point {x : 0.0, y : 0.0 });

        let bb = glyph.pixel_bounding_box();
        // if no bounding box - its now valid glyph?
        let bb = if let Some(bb) = bb { bb } else { return None;};

        let mut buffer = vec![0; (bb.height() * bb.width()) as usize];

        glyph.draw(|x, y, v| {
            let x = x;
            let y = y;
            buffer[(y * bb.width() as u32 + x) as usize] = (v * 255.0) as u8;
        });
        let bitmap : Bitmap = Bitmap {
            rows   : bb.height(),
            width  : bb.width(),
            buffer : buffer
        };

        // adding a left margin before our character to prevent artifacts
        cursor_offset.0 += MARGIN;

        // carriage return our cursor if we don't have enough room to write the next caracter
        // we add a margin to prevent artifacts
        if cursor_offset.0 + (bitmap.width as u32) + MARGIN >= texture_width {
            assert!(bitmap.width as u32 <= texture_width);       // if this fails, we should increase texture_width
            cursor_offset.0 = 0;
            cursor_offset.1 += rows_to_skip;
            rows_to_skip = 0;
        }

        // if the texture data buffer has not enough lines, adding some
        if rows_to_skip < MARGIN + bitmap.rows as u32 {
            let diff = MARGIN + (bitmap.rows as u32) - rows_to_skip;
            rows_to_skip = MARGIN + bitmap.rows as u32;
            texture_data.extend(iter::repeat(0.0).take((diff * texture_width) as usize));
        }

        // copying the data to the texture
        let offset_x_before_copy = cursor_offset.0;
        if bitmap.rows >= 1 {
            let destination = &mut texture_data[(cursor_offset.0 + cursor_offset.1 * texture_width) as usize ..];
            let source = &bitmap.buffer;
            //ylet source = std::slice::from_raw_parts(source, destination.len());

            for y in 0 .. bitmap.rows as u32 {
                let source = &source[(y * bitmap.width as u32) as usize ..];
                let destination = &mut destination[(y * texture_width) as usize ..];

                for x in 0 .. bitmap.width {
                    // the values in source are bytes between 0 and 255, but we want floats between 0 and 1
                    let val: u8 = *source.get(x as usize).unwrap();
                    let val = (val as f32) / (std::u8::MAX as f32);
                    let dest = destination.get_mut(x as usize).unwrap();
                    *dest = val;
                }
            }

            cursor_offset.0 += bitmap.width as u32;
            debug_assert!(cursor_offset.0 <= texture_width);
        }

        // filling infos about that character
        // tex_size and tex_coords are in pixels for the moment ; they will be divided
        // by the texture dimensions later
        Some((character, CharacterInfos {
            tex_size: (bitmap.width as f32, bitmap.rows as f32),
            tex_coords: (offset_x_before_copy as f32, cursor_offset.1 as f32),
            size: (bitmap.width as f32, bitmap.rows as f32),
            left_padding: h_metrics.left_side_bearing as f32,
            right_padding: (h_metrics.advance_width
                            - bitmap.width as f32
                            - h_metrics.left_side_bearing as f32) as f32 / 64.0,
            height_over_line: -bb.min.y as f32,
        }))
    }).collect();

    // adding blank lines at the end until the height of the texture is a power of two
    {
        let current_height = texture_data.len() as u32 / texture_width;
        let requested_height = get_nearest_po2(current_height);
        texture_data.extend(iter::repeat(0.0).take((texture_width * (requested_height - current_height)) as usize));
    }

    // now our texture is finished
    // we know its final dimensions, so we can divide all the pixels values into (0,1) range
    assert!((texture_data.len() as u32 % texture_width) == 0);
    let texture_height = (texture_data.len() as u32 / texture_width) as f32;
    let float_texture_width = texture_width as f32;
    for chr in characters_infos.iter_mut() {
        chr.1.tex_size.0 /= float_texture_width;
        chr.1.tex_size.1 /= texture_height;
        chr.1.tex_coords.0 /= float_texture_width;
        chr.1.tex_coords.1 /= texture_height;
        chr.1.size.0 /= em_pixels;
        chr.1.size.1 /= em_pixels;
        chr.1.left_padding /= em_pixels;
        chr.1.right_padding /= em_pixels;
        chr.1.height_over_line /= em_pixels;
    }

    // returning
    (TextureData {
        data: texture_data,
        width: texture_width,
        height: texture_height as u32,
    }, characters_infos)
}

/// Function that will calculate the nearest power of two.
fn get_nearest_po2(mut x: u32) -> u32 {
    assert!(x > 0);
    x -= 1;
    x = x | (x >> 1);
    x = x | (x >> 2);
    x = x | (x >> 4);
    x = x | (x >> 8);
    x = x | (x >> 16);
    x + 1
}
