package main

import "core:os"
import "core:math"
import "core:intrinsics"
import "core:math/linalg"
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

V2 :: [2]f32;

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
    dx_sq_uniform: i32,
    pressure_uniform: i32,
    divergence_uniform: i32,
}

ResidualProgram :: struct {
    handle: u32,
    dx_uniform: i32,
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
}

ResampleProgram :: struct {
    handle: u32,
    input_uniform: i32,
}

CorrectionProgram :: struct {
    handle: u32,
    pressure_uniform: i32,
    error_uniform: i32,
}

Texture :: struct {
    handle: u32,
    width: i32,
    height: i32,
}

GridLevel :: struct {
    y0, y1, y2, b, r_down, e_down: Texture,
}

MultiGridSolver :: struct {
    opengl: ^OpenGL,
    levels: [4]GridLevel,
    dx: f32,
    cycles: u32,
}

OpenGL :: struct {
    rk4_advection: AdvectionProgram,
    euler_advection: AdvectionProgram,
    divergence: DivergenceProgram,
    jacobi: JacobiProgram,
    residual: ResidualProgram,
    resample: ResampleProgram,
    correction: CorrectionProgram,
    gradient: GradientProgram,
    zero_program: u32,
    smoke_emit_program: u32,

    multigrid_solver: MultiGridSolver,

    text_program: u32,
    texture_program: u32,

    resolution_uniform: i32,
    color_coding_uniform: i32,
    camera_p_uniform: i32,
    camera_scale_uniform: i32,

    camera_p: V2,
    camera_scale: f32,

    vao: u32,
    vbo: u32,
    display_width: i32,
    display_height: i32,

    font_texture: u32,

    initial_velocity_texture: Texture,
    projected_velocity_texture: Texture,
    divergence_texture: Texture,

    color_textures: [2]Texture,

    current_color_texture: int,

    vertices: []Vertex,
    vertex_capacity: u16,
    vertex_size: u16,

    font: FontInfo,
}

init_multigrid_solver :: proc(opengl: ^OpenGL, grid_w, grid_h: i32, b: Texture, dx: f32)
{
    solver := &opengl.multigrid_solver;
    solver.opengl = opengl;
    solver.dx = dx;

    solver.levels[0].b      = b;
    solver.levels[0].y0     = init_texture(grid_w, grid_h, nil, gl.R32F);
    solver.levels[0].y1     = init_texture(grid_w, grid_h, nil, gl.R32F);
    solver.levels[0].y2     = init_texture(grid_w, grid_h, nil, gl.R32F);

    solver.levels[0].r_down = init_texture(grid_w / 2, grid_h / 2, nil, gl.R32F);
    solver.levels[0].e_down = init_texture(grid_w / 2, grid_h / 2, nil, gl.R32F);
    solver.levels[1].y1     = init_texture(grid_w / 2, grid_h / 2, nil, gl.R32F);
    solver.levels[1].y2     = init_texture(grid_w / 2, grid_h / 2, nil, gl.R32F);

    solver.levels[1].r_down = init_texture(grid_w / 4, grid_h / 4, nil, gl.R32F);
    solver.levels[1].e_down = init_texture(grid_w / 4, grid_h / 4, nil, gl.R32F);
    solver.levels[2].y1     = init_texture(grid_w / 4, grid_h / 4, nil, gl.R32F);
    solver.levels[2].y2     = init_texture(grid_w / 4, grid_h / 4, nil, gl.R32F);

    solver.levels[2].r_down = init_texture(grid_w / 8, grid_h / 8, nil, gl.R32F);
    solver.levels[2].e_down = init_texture(grid_w / 8, grid_h / 8, nil, gl.R32F);
    solver.levels[3].y1     = init_texture(grid_w / 8, grid_h / 8, nil, gl.R32F);

    zero_texture(opengl, solver.levels[0].y0);
}

// TODO(said): Make this take pointers to textures so it always puts the final
// iteration back in guess0 so caller doesn't have to pass an even number to
// make sure an iteration is not wasted or have to figure out where the final
// iteration result is based on whether iter_count is odd or even
run_n_jacobi_iterations :: proc (opengl: ^OpenGL, iter_count: int, dx: f32, guess0: Texture, guess1: Texture, b: Texture)
{
    dx_sq := dx*dx;

    texture0: Texture = guess0;
    texture1: Texture = guess1;

    for i in 0..<iter_count {
        run_jacobi_program(&opengl.jacobi, dx_sq, texture0, b, texture1);
        texture0, texture1 = texture1, texture0;
    }
}

get_level :: proc(solver: ^MultiGridSolver, index: int) -> GridLevel
{
    result: GridLevel;

    if index == 0 {
        result.y0     = solver.levels[0].y0;
        result.b      = solver.levels[0].b;
    } else {
        result.y0     = solver.levels[index - 1].e_down;
        result.b      = solver.levels[index - 1].r_down;
    }

    result.y1     = solver.levels[index].y1;
    result.y2     = solver.levels[index].y2;
    result.r_down = solver.levels[index].r_down;
    result.e_down = solver.levels[index].e_down;

    return result;
}

vcycle :: proc(solver: ^MultiGridSolver, index: int)
{
    dx := solver.dx;
    opengl := solver.opengl;

    l := get_level(solver, 0);
    run_n_jacobi_iterations(opengl, 2, dx, l.y0, l.y1, l.b);

    {
        run_residual_program(&opengl.residual, dx, l.y0, l.b, l.y1);
        run_resample_program(&opengl.resample, l.y1, l.r_down);

        zero_texture(opengl, l.e_down);

        l = get_level(solver, 1);
        run_n_jacobi_iterations(opengl, 4, dx*2, l.y0, l.y1, l.b);
        {
            run_residual_program(&opengl.residual, dx*2, l.y0, l.b, l.y1);
            run_resample_program(&opengl.resample, l.y1, l.r_down);

            zero_texture(opengl, l.e_down);

            l = get_level(solver, 2);
            run_n_jacobi_iterations(opengl, 8, dx*4, l.y0, l.y1, l.b);
            {
                run_residual_program(&opengl.residual, dx*4, l.y0, l.b, l.y1);
                run_resample_program(&opengl.resample, l.y1, l.r_down);

                zero_texture(opengl, l.e_down);

                l = get_level(solver, 3);
                run_n_jacobi_iterations(opengl, 100, dx*8, l.y0, l.y1, l.b);
                l = get_level(solver, 2);

                run_resample_program(&opengl.resample, l.e_down, l.y2);
                run_correction_program(&opengl.correction, l.y0, l.y2, l.y1);
                run_n_jacobi_iterations(opengl, 8 + 1, dx*4, l.y1, l.y0, l.b);
            }
            l = get_level(solver, 1);

            run_resample_program(&opengl.resample, l.e_down, l.y2);
            run_correction_program(&opengl.correction, l.y0, l.y2, l.y1);
            run_n_jacobi_iterations(opengl, 4 + 1, dx*2, l.y1, l.y0, l.b);
        }
        l = get_level(solver, 0);

        run_resample_program(&opengl.resample, l.e_down, l.y2);
        run_correction_program(&opengl.correction, l.y0, l.y2, l.y1);
        run_n_jacobi_iterations(opengl, 2 + 1, dx, l.y1, l.y0, l.b);
    }
}

vcycle_recursive :: proc(solver: ^MultiGridSolver, index: int)
{
    if index == 0 do solver.cycles += 1;

    dx := math.pow(2, f32(index)) * solver.dx;
    opengl := solver.opengl;

    pre_smooth := 2;
    post_smooth := 2;

    // NOTE(said): The numbers should be configurable
    switch index {
        case 0:
            pre_smooth  = 2;
            post_smooth = 2;

        case 1:
            pre_smooth  = 4;
            post_smooth = 4;

        case 2:
            pre_smooth  = 8;
            post_smooth = 8;

        case 3:
            pre_smooth  = 100;
            post_smooth = 100;
    }

    l := get_level(solver, index);
    run_n_jacobi_iterations(opengl, pre_smooth, dx, l.y0, l.y1, l.b);

    if index < 3 {
        run_residual_program(&opengl.residual, dx, l.y0, l.b, l.y1);
        run_resample_program(&opengl.resample, l.y1, l.r_down);

        zero_texture(opengl, l.e_down);
        vcycle_recursive(solver, index + 1);

        run_resample_program(&opengl.resample, l.e_down, l.y2);
        run_correction_program(&opengl.correction, l.y0, l.y2, l.y1);
        run_n_jacobi_iterations(opengl, post_smooth + 1, dx, l.y1, l.y0, l.b);
    }
}

init_font_texture :: proc(opengl: ^OpenGL) -> FontInfo
{
    result: FontInfo;

    file_path := "NotoSerif-Regular.ttf";
    file_buffer, ok := os.read_entire_file(file_path);
    if !ok do return result;

    start_char : i32 = 0x20;
    char_count : i32 = 0x7E - 0x20;

    result.pixel_height = 24;
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

bind_output_texture :: proc(texture: Texture, internal_format: u32 = gl.RGBA32F, read_write: bool = false)
{
    access_mode : u32 = gl.WRITE_ONLY;
    if read_write do access_mode = gl.READ_WRITE;
    gl.BindImageTexture(0, texture.handle, 0, 0, 0, access_mode, internal_format);
}

dispatch_compute :: proc(texture: Texture)
{
    assert(texture.width % WORK_GROUP_SIZE == 0);
    assert(texture.height % WORK_GROUP_SIZE == 0);

    gl.DispatchCompute(u32(texture.width) / WORK_GROUP_SIZE, u32(texture.height) / WORK_GROUP_SIZE, 1);
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

    bind_output_texture(out_texture, gl.R32F);

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

init_residual_program :: proc(program: ^ResidualProgram)
{
    compile_compute_shader(program, residual_shader);

    program.dx_uniform = gl.GetUniformLocation(program.handle, "dx");
    program.pressure_uniform = gl.GetUniformLocation(program.handle, "pressure");
    program.divergence_uniform = gl.GetUniformLocation(program.handle, "divergence");
}

run_residual_program :: proc(program: ^ResidualProgram, dx: f32, pressure: Texture, divergence: Texture, out_texture: Texture)
{
    gl.UseProgram(program.handle);

    bind_input_texture(divergence, 0);
    bind_input_texture(pressure, 1);

    gl.Uniform1i(program.divergence_uniform, 0);
    gl.Uniform1i(program.pressure_uniform, 1);
    gl.Uniform1f(program.dx_uniform, dx);

    bind_output_texture(out_texture, gl.R32F);

    dispatch_compute(out_texture);
}

init_resample_program :: proc(program: ^ResampleProgram)
{
    compile_compute_shader(program, resample_shader);

    program.input_uniform = gl.GetUniformLocation(program.handle, "in_texture");
}

run_resample_program :: proc(program: ^ResampleProgram, input: Texture, out_texture: Texture)
{
    gl.UseProgram(program.handle);

    bind_input_texture(input, 0);

    gl.Uniform1i(program.input_uniform, 0);

    bind_output_texture(out_texture, gl.R32F);

    dispatch_compute(out_texture);
}

init_correction_program :: proc(program: ^CorrectionProgram)
{
    compile_compute_shader(program, correction_shader);

    program.pressure_uniform = gl.GetUniformLocation(program.handle, "pressure");
    program.error_uniform = gl.GetUniformLocation(program.handle, "error");
}

run_correction_program :: proc(program: ^CorrectionProgram, pressure: Texture, error: Texture, out_texture: Texture)
{
    gl.UseProgram(program.handle);

    bind_input_texture(pressure, 0);
    bind_input_texture(error, 1);

    gl.Uniform1i(program.pressure_uniform, 0);
    gl.Uniform1i(program.error_uniform, 1);

    bind_output_texture(out_texture, gl.R32F);

    dispatch_compute(out_texture);
}

init_jacobi_program :: proc(program: ^JacobiProgram)
{
    compile_compute_shader(program, jacobi_shader);

    program.dx_sq_uniform = gl.GetUniformLocation(program.handle, "dx_sq");
    program.pressure_uniform = gl.GetUniformLocation(program.handle, "pressure");
    program.divergence_uniform = gl.GetUniformLocation(program.handle, "divergence");
}

run_jacobi_program :: proc(program: ^JacobiProgram, dx_sq: f32, pressure: Texture, divergence: Texture, out_texture: Texture)
{
    gl.UseProgram(program.handle);

    bind_input_texture(divergence, 0);
    bind_input_texture(pressure, 1);

    gl.Uniform1f(program.dx_sq_uniform, dx_sq);
    gl.Uniform1i(program.divergence_uniform, 0);
    gl.Uniform1i(program.pressure_uniform, 1);

    bind_output_texture(out_texture, gl.R32F);

    dispatch_compute(out_texture);
}

zero_texture :: proc(opengl: ^OpenGL, texture: Texture)
{
    gl.UseProgram(opengl.zero_program);
    bind_output_texture(texture, gl.R32F);
    dispatch_compute(texture);
}

init_texture :: proc(width: i32, height: i32, pixels: []f32, internal_format: u32 = gl.RGBA32F) -> Texture
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

    gl.TexStorage2D(gl.TEXTURE_2D, 1, internal_format, texture.width, texture.height);
    if pixels != nil {
        gl.TexSubImage2D(gl.TEXTURE_2D, 0, 0, 0, texture.width, texture.height, gl.RGBA, gl.FLOAT, &pixels[0]);
    }

    return texture;
}

init_solution :: proc(width, height: int, pixels: []f32)
{
    center: [2]f32;
    center[0] = f32(width) * 0.5;
    center[1] = f32(height) * 0.5;

    for y := 0; y < height; y += 1 {
        for x := 0; x < width; x += 1 {
            xn := (f32(x) / f32(width))  * 2 - 1;
            yn := (f32(y) / f32(height)) * 2 - 1;

            c := math.sin(math.PI*xn) * math.sin(math.PI*yn);

            pixels[4 * (x + y * width) + 0] = c;
            pixels[4 * (x + y * width) + 1] = 0;
            pixels[4 * (x + y * width) + 2] = 0;
            pixels[4 * (x + y * width) + 3] = 0;
        }
    }
}

init_b :: proc(width, height: int, pixels: []f32)
{
    center: [2]f32;
    center[0] = f32(width) * 0.5;
    center[1] = f32(height) * 0.5;

    for y := 0; y < height; y += 1 {
        for x := 0; x < width; x += 1 {
            xn := (f32(x) / f32(width))  * 2 - 1;
            yn := (f32(y) / f32(height)) * 2 - 1;

            c := -2 * math.PI * math.PI * math.sin(math.PI*xn) * math.sin(math.PI*yn);

            pixels[4 * (x + y * width) + 0] = c;
            pixels[4 * (x + y * width) + 1] = 0;
            pixels[4 * (x + y * width) + 2] = 0;
            pixels[4 * (x + y * width) + 3] = 0;
        }
    }
}

init_velocity_field :: proc(width, height: int, pixels: []f32)
{
    square :: 64;
    startx := (width - square) / 2;
    endx := startx + square;
    starty := (height - square) / 2;
    endy := starty + square;

    for y := 0; y < width; y += 1 {
        for x := 0; x < height; x += 1 {
            c: f32 = 0.0;

            yoff := y - starty;
            xoff := x - startx;

            _x := f32(xoff) - f32(square / 2);
            _y := f32(yoff) - f32(square / 2);

            if xoff >= 0 && xoff < square && yoff >= 0 && yoff < square {
                c = 10.0;
            }

            pixels[4 * (x + y * width) + 0] = 0;
            pixels[4 * (x + y * width) + 1] = c;
            pixels[4 * (x + y * width) + 2] = 0;
            pixels[4 * (x + y * width) + 3] = 0;
        }
    }
}

emit_smoke :: proc(opengl: ^OpenGL, p: V2, radius: f32)
{
    gl.UseProgram(opengl.smoke_emit_program);
    texture := opengl.color_textures[opengl.current_color_texture];
    bind_output_texture(texture, gl.RGBA32F, true);
    dispatch_compute(texture);
}

init_opengl :: proc(opengl: ^OpenGL)
{
    gl.Enable(gl.BLEND);
    gl.BlendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);

    gl.PixelStorei(gl.UNPACK_ALIGNMENT, 1);
    gl.PixelStorei(gl.PACK_ALIGNMENT, 1);

    ok := false;

    opengl.texture_program, ok = gl.load_shaders_source(ndc_xy_vert_shader, texture_frag_shader);
    assert(ok);
    opengl.text_program, ok = gl.load_shaders_source(text_vert_shader, text_frag_shader);
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

    opengl.camera_p_uniform = gl.GetUniformLocation(opengl.texture_program, "camera_p");
    opengl.camera_scale_uniform = gl.GetUniformLocation(opengl.texture_program, "camera_scale");
    opengl.color_coding_uniform = gl.GetUniformLocation(opengl.texture_program, "color_coding");

    opengl.camera_p = {0.5, -0.5};
    opengl.camera_scale = 0.25;

    opengl.font = init_font_texture(opengl);

    init_divergence_program(&opengl.divergence);
    init_jacobi_program(&opengl.jacobi);
    init_residual_program(&opengl.residual);
    init_resample_program(&opengl.resample);
    init_correction_program(&opengl.correction);
    init_gradient_program(&opengl.gradient);
    opengl.zero_program, ok = gl.load_compute_source(zero_shader);
    assert(ok);
    opengl.smoke_emit_program, ok = gl.load_compute_source(smoke_emit_shader);
    assert(ok);

    grid_w := 512;
    grid_h := 512;
    pixels := make([]f32, 4 * grid_w * grid_h);

    init_advection_program(&opengl.rk4_advection, i32(grid_w), i32(grid_h));

    init_velocity_field(grid_w, grid_h, pixels);
    opengl.initial_velocity_texture   = init_texture(i32(grid_w), i32(grid_h), pixels, gl.RGBA32F);

    opengl.projected_velocity_texture = init_texture(i32(grid_w), i32(grid_h), nil, gl.RGBA32F);
    opengl.divergence_texture         = init_texture(i32(grid_w), i32(grid_h), nil, gl.R32F);


    init_multigrid_solver(opengl, i32(grid_w), i32(grid_h), opengl.divergence_texture, 2);

    delete(pixels);

    opengl.color_textures[0] = init_texture(i32(grid_w), i32(grid_h), nil, gl.RGBA32F);
    opengl.color_textures[1] = init_texture(i32(grid_w), i32(grid_h), nil, gl.RGBA32F);
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

slap_texture :: proc(opengl: ^OpenGL, min_corner: V2, max_corner: V2, texture_handle: u32, color_coding: int = 0)
{
    send_quad(min_corner, max_corner);

    gl.UseProgram(opengl.texture_program);

    gl.ActiveTexture(gl.TEXTURE0);
    gl.BindTexture(gl.TEXTURE_2D, texture_handle);

    gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
    gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);

    gl.Uniform1i(opengl.color_coding_uniform, i32(color_coding));
    gl.Uniform2f(opengl.camera_p_uniform, opengl.camera_p.x, opengl.camera_p.y);
    gl.Uniform1f(opengl.camera_scale_uniform, opengl.camera_scale);

    gl.DrawArrays(gl.TRIANGLES, 0, 6);

    gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
    gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
}

begin_frame :: proc(opengl: ^OpenGL, width, height: i32)
{
    gl.ClearColor(0, 0, 0, 1);

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

sim_step :: proc(opengl: ^OpenGL)
{
    run_divergence_program(&opengl.divergence, opengl.initial_velocity_texture, opengl.divergence_texture);

    for i in 0..<8 do vcycle(&opengl.multigrid_solver, 0);

    run_gradient_program(&opengl.gradient, opengl.multigrid_solver.levels[0].y0, opengl.initial_velocity_texture, opengl.projected_velocity_texture);

    run_advection_program(&opengl.rk4_advection, opengl.color_textures[opengl.current_color_texture], opengl.projected_velocity_texture, opengl.color_textures[1 - opengl.current_color_texture]);
    run_advection_program(&opengl.rk4_advection, opengl.projected_velocity_texture, opengl.projected_velocity_texture, opengl.initial_velocity_texture);
    opengl.current_color_texture = 1 - opengl.current_color_texture;
}

render :: proc(opengl: ^OpenGL, width, height: i32)
{
    begin_frame(opengl, width, height);

    sim_step(opengl);

    slap_texture(opengl, V2{ 0, -1}, V2{1,  1}, opengl.projected_velocity_texture.handle, 1);
    slap_texture(opengl, V2{ 1, -1}, V2{2,  1}, opengl.multigrid_solver.levels[0].r_down.handle, 2);
    slap_texture(opengl, V2{ 0, -3}, V2{1, -1}, opengl.color_textures[opengl.current_color_texture].handle, 0);

    // NOTE(said): Text
    {
        opengl.vertex_size = 0;

        push_text(opengl, V2{0, 0*opengl.font.pixel_height}, fmt.tprintf("cycles: %d", opengl.multigrid_solver.cycles));
    }

    end_frame(opengl);
}
