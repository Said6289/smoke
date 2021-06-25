package main

import "core:os"
import "core:fmt"
import gl "shared:odin-gl"

WORK_GROUP_SIZE :: 32;

stbtt_bakedchar :: struct {
   x0, y0, x1, y1: u16,
   xoff, yoff, xadvance: f32,
}

stbtt_aligned_quad :: struct {
    x0, y0, s0, t0: f32,
    x1, y1, s1, t1: f32,
}

// TODO(said): Add windows version
foreign import stbtt "./stb_truetype.a"
@(default_calling_convention = "c")
foreign stbtt {
    stbtt_BakeFontBitmap :: proc (data: ^u8, offset: i32, pixel_height: f32, pixels: ^u8, pw, ph, first_char, num_chars: i32, chardata: ^stbtt_bakedchar) -> i32 ---;
    stbtt_GetBakedQuad :: proc (chardata: ^stbtt_bakedchar, pw, ph, char_index: i32, xpos, ypos: ^f32, q: ^stbtt_aligned_quad, opengl_fillrule: i32) ---;
    stbtt_GetScaledFontVMetrics :: proc (fontdata: ^u8, index: i32, size: f32, ascent, descent, line_gap: ^f32) ---;
}

V2 :: struct {
    x: f32,
    y: f32,
}

FontInfo :: struct {
    chars: []stbtt_bakedchar,
    font_bitmap: []u8,
    width: i32,
    height: i32,
    ascent: f32,
    pixel_height: f32,
}

Vertex :: struct {
    p: V2,
    uv: V2,
}

GradientProgram :: struct {
    handle: u32,
    field_uniform: i32,
    pressure_uniform: i32,
}

JacobiProgram :: struct {
    handle: u32,
    pressure_uniform: i32,
    divergence_uniform: i32,
}

DivergenceProgram :: struct {
    handle: u32,
    field_uniform: i32,
}

AdvectionProgram :: struct {
    handle: u32,
    field_uniform: i32,
    velocity_uniform: i32,
    output_texture: u32,
}

Texture :: struct {
    handle: u32,
    width: i32,
    height: i32,
}

OpenGL :: struct {
    query: u32,

    rk4_advection: AdvectionProgram,
    euler_advection: AdvectionProgram,
    divergence: DivergenceProgram,
    jacobi: JacobiProgram,
    gradient: GradientProgram,

    text_program: u32,
    texture_program: u32,
    field_texture_program: u32,

    resolution_uniform: i32,

    vao: u32,
    vbo: u32,
    display_width: i32,
    display_height: i32,

    font_texture: u32,

    current_texture: int,
    texture: [10]Texture,

    vertices: []Vertex,
    vertex_capacity: u16,
    vertex_size: u16,

    font: FontInfo,
}

init_font_texture :: proc(opengl: ^OpenGL) -> FontInfo
{
    result: FontInfo;

    file_path := "NotoSerif-Regular.ttf";
    file_buffer, ok := os.read_entire_file(file_path);
    if !ok do return result;

    start_char : i32 = 0x20;
    char_count : i32 = 0x7E - 0x20;

    result.pixel_height = 32;
    result.width = 256;
    result.height = 256;
    result.font_bitmap = make([]u8, result.width * result.height);
    result.chars = make([]stbtt_bakedchar, char_count);

    rows := stbtt_BakeFontBitmap(
            &file_buffer[0], 0, result.pixel_height, 
            &result.font_bitmap[0], result.width, result.height,
            start_char, char_count,
            &result.chars[0]);

    descent: f32;
    line_gap: f32;
    stbtt_GetScaledFontVMetrics(&file_buffer[0], 0, result.pixel_height, &result.ascent, &descent, &line_gap);

    assert(rows > 0);

    gl.ActiveTexture(gl.TEXTURE0);
    gl.GenTextures(1, &opengl.font_texture);
    gl.BindTexture(gl.TEXTURE_2D, opengl.font_texture);
    gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
    gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
    gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.REPEAT);
    gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.REPEAT);
    gl.TexImage2D(gl.TEXTURE_2D, 0, gl.RGBA8, result.width, result.height, 0, gl.RED, gl.UNSIGNED_BYTE, &result.font_bitmap[0]);

    return result;
}

compile_compute_shader :: proc(program: $program_type, code: string)
{
    ok := false;
    program.handle, ok = gl.load_compute_source(code);
    assert(ok);
}

bind_input_texture :: proc(texture: Texture, texture_unit: u32)
{
    gl.ActiveTexture(gl.TEXTURE0 + texture_unit);
    gl.BindTexture(gl.TEXTURE_2D, texture.handle);
}

bind_output_texture :: proc(texture: Texture)
{
    gl.BindImageTexture(0, texture.handle, 0, 0, 0, gl.WRITE_ONLY, gl.RGBA32F);
}

dispatch_compute :: proc(texture: Texture)
{
    assert(texture.width % WORK_GROUP_SIZE == 0);
    assert(texture.height % WORK_GROUP_SIZE == 0);

    gl.DispatchCompute(u32(texture.width) / WORK_GROUP_SIZE, u32(texture.height) / WORK_GROUP_SIZE, 1);
    gl.MemoryBarrier(gl.ALL_BARRIER_BITS);
}

init_advection_program :: proc(program: ^AdvectionProgram, width, height: i32)
{
    compile_compute_shader(program, advection_shader);

    program.field_uniform = gl.GetUniformLocation(program.handle, "field");
    program.velocity_uniform = gl.GetUniformLocation(program.handle, "velocity");
}

run_advection_program :: proc(program: ^AdvectionProgram, in_texture: Texture, velocity_texture: Texture, out_texture: Texture)
{
    gl.UseProgram(program.handle);

    bind_input_texture(in_texture, 0);
    bind_input_texture(velocity_texture, 1);

    gl.Uniform1i(program.field_uniform, 0);
    gl.Uniform1i(program.velocity_uniform, 1);

    bind_output_texture(out_texture);

    dispatch_compute(out_texture);
}

init_divergence_program :: proc(program: ^DivergenceProgram)
{
    compile_compute_shader(program, divergence_shader);

    program.field_uniform = gl.GetUniformLocation(program.handle, "Field");
}

run_divergence_program :: proc(program: ^DivergenceProgram, in_texture: Texture, out_texture: Texture)
{
    gl.UseProgram(program.handle);

    bind_input_texture(in_texture, 0);

    gl.Uniform1i(program.field_uniform, 0);

    bind_output_texture(out_texture);

    dispatch_compute(out_texture);
}

init_gradient_program :: proc(program: ^GradientProgram)
{
    compile_compute_shader(program, gradient_shader);

    program.field_uniform = gl.GetUniformLocation(program.handle, "velocity");
    program.pressure_uniform = gl.GetUniformLocation(program.handle, "pressure");
}

run_gradient_program :: proc(program: ^GradientProgram, pressure: Texture, field: Texture, out_texture: Texture)
{
    gl.UseProgram(program.handle);

    bind_input_texture(pressure, 0);
    bind_input_texture(field, 1);

    gl.Uniform1i(program.pressure_uniform, 0);
    gl.Uniform1i(program.field_uniform, 1);

    bind_output_texture(out_texture);

    dispatch_compute(out_texture);
}

init_jacobi_program :: proc(program: ^JacobiProgram)
{
    compile_compute_shader(program, jacobi_shader);

    program.pressure_uniform = gl.GetUniformLocation(program.handle, "pressure");
    program.divergence_uniform = gl.GetUniformLocation(program.handle, "divergence");
}

run_jacobi_program :: proc(program: ^JacobiProgram, pressure: Texture, divergence: Texture, out_texture: Texture)
{
    gl.UseProgram(program.handle);

    bind_input_texture(divergence, 0);
    bind_input_texture(pressure, 1);

    gl.Uniform1i(program.divergence_uniform, 0);
    gl.Uniform1i(program.pressure_uniform, 1);

    bind_output_texture(out_texture);

    dispatch_compute(out_texture);
}

init_texture :: proc(width: i32, height: i32, pixels: []f32) -> Texture
{
    texture := Texture{
        width=width,
        height=height,
    };

    gl.GenTextures(1, &texture.handle);
    gl.BindTexture(gl.TEXTURE_2D, texture.handle);
    gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
    gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
    gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.REPEAT);
    gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.REPEAT);

    gl.TexStorage2D(gl.TEXTURE_2D, 1, gl.RGBA32F, texture.width, texture.height);
    if pixels != nil {
        gl.TexSubImage2D(gl.TEXTURE_2D, 0, 0, 0, texture.width, texture.height, gl.RGBA, gl.FLOAT, &pixels[0]);
    }

    return texture;
}

init_opengl :: proc(opengl: ^OpenGL)
{
    gl.GenQueries(1, &opengl.query);

    gl.Enable(gl.BLEND);
    gl.BlendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);

    gl.PixelStorei(gl.UNPACK_ALIGNMENT, 1);
    gl.PixelStorei(gl.PACK_ALIGNMENT, 1);

    ok := false;

    opengl.texture_program, ok = gl.load_shaders_source(ndc_xy_vert_shader, texture_frag_shader);
    assert(ok);
    opengl.field_texture_program, ok = gl.load_shaders_source(ndc_xy_vert_shader, field_texture_frag_shader);
    assert(ok);
    opengl.text_program, ok = gl.load_shaders_source(text_vert_shader_code, text_frag_shader_code);
    assert(ok);

    gl.GenVertexArrays(1, &opengl.vao);
    gl.GenBuffers(1, &opengl.vbo);

    gl.BindVertexArray(opengl.vao);
    gl.BindBuffer(gl.ARRAY_BUFFER, opengl.vbo);

    gl.VertexAttribPointer(0, 2, gl.FLOAT, gl.FALSE, size_of(Vertex), rawptr(offset_of(Vertex, p)));
    gl.EnableVertexAttribArray(0);

    gl.VertexAttribPointer(1, 2, gl.FLOAT, gl.FALSE, size_of(Vertex), rawptr(offset_of(Vertex, uv)));
    gl.EnableVertexAttribArray(1);

    opengl.vertex_capacity = 6 * 8192;
    opengl.vertex_size = 0;
    opengl.vertices = make([]Vertex, opengl.vertex_capacity);

    opengl.resolution_uniform = gl.GetUniformLocation(opengl.text_program, "Resolution");

    opengl.font = init_font_texture(opengl);

    init_divergence_program(&opengl.divergence);
    init_jacobi_program(&opengl.jacobi);
    init_gradient_program(&opengl.gradient);

    grid_w := 256;
    grid_h := 256;
    pixels := make([]f32, 4 * grid_w * grid_h);

    init_advection_program(&opengl.rk4_advection, i32(grid_w), i32(grid_h));

    square :: 32;
    startx := (grid_w - square) / 2;
    endx := startx + square;
    starty := (grid_h - square) / 2;
    endy := starty + square;

    for y := 0; y < grid_w; y += 1 {
        for x := 0; x < grid_h; x += 1 {
            c: f32 = 0.0;

            yoff := y - starty;
            xoff := x - startx;

            if xoff >= 0 && xoff < square && y >= starty && y < endy {
                c = 30.0;
            }

            pixels[4 * (x + y * grid_w) + 0] = 0;
            pixels[4 * (x + y * grid_w) + 1] = c;
            pixels[4 * (x + y * grid_w) + 2] = 0;
            pixels[4 * (x + y * grid_w) + 3] = 0;
        }
    }

    opengl.texture[0] = init_texture(i32(grid_w), i32(grid_w), pixels);
    for i := 1; i < len(opengl.texture); i += 1 {
        opengl.texture[i] = init_texture(i32(grid_w), i32(grid_h), nil);
    }

    grid_w = 512;
    grid_h = 512;

    delete(pixels);
    pixels = make([]f32, 4 * grid_w * grid_h);

    checker_size := 64;
    checkers_in_a_row := (grid_w + checker_size - 1) / checker_size;

    for y := 0; y < grid_h; y += 1 {
        for x := 0; x < grid_w; x += 1 {
            checker_x := x / checker_size;
            checker_y := y / checker_size;

            c := f32(((checker_x % 2) + (checker_y % 2)) % 2);

            pixels[4 * (x + y * grid_w) + 0] = c;
            pixels[4 * (x + y * grid_w) + 1] = c;
            pixels[4 * (x + y * grid_w) + 2] = c;
            pixels[4 * (x + y * grid_w) + 3] = 1;
        }
    }

    opengl.texture[6] = init_texture(i32(grid_w), i32(grid_h), pixels);
    opengl.texture[7] = init_texture(i32(grid_w), i32(grid_h), nil);

    opengl.current_texture = 0;

    delete(pixels);
}

push_vertex :: proc(opengl: ^OpenGL, p: V2, uv := V2{})
{
    has_space := opengl.vertex_size < opengl.vertex_capacity;
    if (has_space) {
        opengl.vertices[opengl.vertex_size].p = p;
        opengl.vertices[opengl.vertex_size].uv = uv;
        opengl.vertex_size += 1;
    }
}

push_quad :: proc(opengl: ^OpenGL, min_corner: V2, max_corner: V2, min_uv: V2, max_uv: V2)
{
    p0 := V2{min_corner.x, min_corner.y};
    p1 := V2{max_corner.x, min_corner.y};
    p2 := V2{max_corner.x, max_corner.y};
    p3 := V2{min_corner.x, max_corner.y};

    push_vertex(opengl, p0, V2{min_uv.x, min_uv.y});
    push_vertex(opengl, p1, V2{max_uv.x, min_uv.y});
    push_vertex(opengl, p2, V2{max_uv.x, max_uv.y});

    push_vertex(opengl, p2, V2{max_uv.x, max_uv.y});
    push_vertex(opengl, p3, V2{min_uv.x, max_uv.y});
    push_vertex(opengl, p0, V2{min_uv.x, min_uv.y});
}

push_text :: proc(opengl: ^OpenGL, _p: V2, text: string)
{
    if opengl.font.chars == nil do return;

    p := _p;

    for c in text {
        quad: stbtt_aligned_quad;
        stbtt_GetBakedQuad(
            &opengl.font.chars[0], opengl.font.width, opengl.font.height,
            i32(c - 0x20),
            &p.x, &p.y, &quad,
            1);
        push_quad(
            opengl,
            V2{quad.x0, quad.y0 + opengl.font.ascent}, V2{quad.x1, quad.y1 + opengl.font.ascent},
            V2{quad.s0, quad.t0}, V2{quad.s1, quad.t1});
    }
}

send_quad :: proc(min_corner := V2{-1, -1}, max_corner := V2{1, 1})
{
    verts: [6]Vertex;

    verts[0].p = V2{min_corner.x, min_corner.y};
    verts[0].uv = V2{0,0};

    verts[1].p = V2{max_corner.x, min_corner.y};
    verts[1].uv = V2{1, 0};

    verts[2].p = V2{max_corner.x, max_corner.y};
    verts[2].uv = V2{1, 1};

    verts[3].p = V2{max_corner.x, max_corner.y};
    verts[3].uv = V2{1, 1};

    verts[4].p = V2{min_corner.x, max_corner.y};
    verts[4].uv = V2{0, 1};

    verts[5].p = V2{min_corner.x, min_corner.y};
    verts[5].uv = V2{0, 0};

    gl.BufferData(gl.ARRAY_BUFFER, 6 * size_of(Vertex), &verts[0], gl.STREAM_DRAW);
}

slap_texture :: proc(opengl: ^OpenGL, min_corner: V2, max_corner: V2, texture_handle: u32)
{
    send_quad(min_corner, max_corner);

    gl.UseProgram(opengl.texture_program);

    gl.ActiveTexture(gl.TEXTURE0);
    gl.BindTexture(gl.TEXTURE_2D, texture_handle);

    gl.DrawArrays(gl.TRIANGLES, 0, 6);
}

slap_field_texture :: proc(opengl: ^OpenGL, min_corner: V2, max_corner: V2, texture_handle: u32)
{
    send_quad(min_corner, max_corner);

    gl.UseProgram(opengl.field_texture_program);

    gl.ActiveTexture(gl.TEXTURE0);
    gl.BindTexture(gl.TEXTURE_2D, texture_handle);

    gl.DrawArrays(gl.TRIANGLES, 0, 6);
}

begin_frame :: proc(opengl: ^OpenGL, width, height: i32)
{
    gl.Viewport(0, 0, width, height);
    gl.Clear(gl.COLOR_BUFFER_BIT);

    gl.BindVertexArray(opengl.vao);
    gl.BindBuffer(gl.ARRAY_BUFFER, opengl.vbo);

    opengl.display_width = width;
    opengl.display_height = height;
}

end_frame :: proc(opengl: ^OpenGL)
{
    gl.BufferData(gl.ARRAY_BUFFER, int(opengl.vertex_size * size_of(Vertex)), &opengl.vertices[0], gl.STREAM_DRAW);

    gl.ActiveTexture(gl.TEXTURE0);
    gl.BindTexture(gl.TEXTURE_2D, opengl.font_texture);

    gl.UseProgram(opengl.text_program);
    gl.Uniform2f(opengl.resolution_uniform, f32(opengl.display_width), f32(opengl.display_height));

    gl.DrawArrays(gl.TRIANGLES, 0, i32(opengl.vertex_size));
}

render :: proc(opengl: ^OpenGL, width, height: i32)
{
    begin_frame(opengl, width, height);

    iterations := 1000;
    run_divergence_program(&opengl.divergence, opengl.texture[0], opengl.texture[2]);
    {
        query_block(.Pressure);
        for i := 0; i < iterations; i += 1 {
            run_jacobi_program(&opengl.jacobi, opengl.texture[3 + (i % 2)], opengl.texture[2], opengl.texture[4 - (i % 2)]);
        }
    }
    run_gradient_program(&opengl.gradient, opengl.texture[3], opengl.texture[0], opengl.texture[1]);
    run_advection_program(&opengl.rk4_advection, opengl.texture[6 + opengl.current_texture], opengl.texture[0], opengl.texture[7 - opengl.current_texture]);
    run_advection_program(&opengl.rk4_advection, opengl.texture[1], opengl.texture[1], opengl.texture[0]);

    slap_field_texture(opengl, V2{-1, -1}, V2{0, 1}, opengl.texture[1].handle);
    slap_texture(opengl, V2{0, -1}, V2{1, 1}, opengl.texture[7 - opengl.current_texture].handle);

    opengl.current_texture = 1 - opengl.current_texture;

    // NOTE(said): Text
    {
        time, bandwidth := get_query_averages(.Pressure, int(opengl.texture[4].width * opengl.texture[4].height * 4 * 4 * 3 * i32(iterations)));

        opengl.vertex_size = 0;

        push_text(opengl, V2{0, 0}, "Projection");
        push_text(opengl, V2{0, opengl.font.pixel_height}, fmt.tprintf("Time: %f, Bandwidth: %f", time, bandwidth));
        push_text(opengl, V2{512, 0}, "Color");
    }

    end_frame(opengl);
}
