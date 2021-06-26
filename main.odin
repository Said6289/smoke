package main

import "core:runtime"
import "core:fmt"
import gl "shared:odin-gl"
import glfw "shared:odin-glfw"
import glfw_bindings "shared:odin-glfw/bindings"

WINDOW_WIDTH :: 1280;
WINDOW_HEIGHT :: 720;

WindowData :: struct {
    opengl: ^OpenGL,
    lmb_pressed: bool,
    last_cursor_p: [2]f32,
}

main :: proc() {
    OPENGL_MAJOR_VERSION :: 4;
    OPENGL_MINOR_VERSION :: 4;

    if !glfw.init() do panic("Failed to init GLFW.");
	defer glfw.terminate();

    glfw.window_hint(.CONTEXT_VERSION_MAJOR, OPENGL_MAJOR_VERSION);
    glfw.window_hint(.CONTEXT_VERSION_MINOR, OPENGL_MINOR_VERSION);
	glfw.window_hint(.OPENGL_PROFILE, int(glfw.OpenGL_Profile.OPENGL_CORE_PROFILE));

	window_handle := glfw.create_window(WINDOW_WIDTH, WINDOW_HEIGHT, "smoke", nil, nil);
	if window_handle == nil do panic("Failed to create window.");

	glfw.make_context_current(window_handle);
	glfw.swap_interval(1);

    gl.load_up_to(4, 4, glfw.set_proc_address);

    opengl: OpenGL;
    init_opengl(&opengl);

    window_data: WindowData;
    window_data.opengl = &opengl;

    glfw.set_window_user_pointer(window_handle, &window_data);

    init_queries();

    glfw.set_cursor_pos_callback(window_handle, cursor_position_callback);
    glfw.set_scroll_callback(window_handle, scroll_callback);
    glfw.set_mouse_button_callback(window_handle, mouse_button_callback);

    for !glfw.window_should_close(window_handle) {
        glfw.poll_events();

        width, height := glfw.get_window_size(window_handle);
        render(&opengl, i32(width), i32(height));
        check_queries();

        glfw.swap_buffers(window_handle);
    }

    glfw.destroy_window(window_handle);
}

clamp_camera_p :: proc "contextless" (opengl: ^OpenGL)
{
    opengl.camera_p.x = clamp(opengl.camera_p.x, -5, 5);
    opengl.camera_p.y = clamp(opengl.camera_p.y, -5, 5);
}

cursor_position_callback :: proc "c" (window: glfw.Window_Handle, x, y: f64)
{
    window_data := cast(^WindowData)glfw_bindings.GetWindowUserPointer(window);
    opengl := window_data.opengl;
    lmb_pressed := window_data.lmb_pressed;
    last_cursor_p := &window_data.last_cursor_p;

    width, height: i32;
    glfw_bindings.GetWindowSize(window, &width, &height);

    cursor_p := [2]f32{f32(x) / f32(width), f32(y) / f32(height)} * 2 - 1;
    cursor_p.y *= -1;

    if lmb_pressed {
        delta := (cursor_p - last_cursor_p^) / opengl.camera_scale;
        opengl.camera_p.x -= delta[0];
        opengl.camera_p.y -= delta[1];
        clamp_camera_p(opengl);
    }

    last_cursor_p^ = cursor_p;
}

scroll_callback :: proc "c" (window: glfw.Window_Handle, x, y: f64)
{
    window_data := cast(^WindowData)glfw_bindings.GetWindowUserPointer(window);
    opengl := window_data.opengl;
    last_cursor_p := window_data.last_cursor_p;

    cam_p := [2]f32{opengl.camera_p.x, opengl.camera_p.y};

    initial_scale := opengl.camera_scale;

    opengl.camera_scale += f32(y / 10);
    opengl.camera_scale = max(opengl.camera_scale, 0.05);

    // Cursor positions before and after the scale change in
    // world positions
    cursor_world_p0 := last_cursor_p / initial_scale       + cam_p;
    cursor_world_p1 := last_cursor_p / opengl.camera_scale + cam_p;

    // This is the change in cursor position in world space due to
    // the change in the camera scale due to the scroll.
    cursor_delta := cursor_world_p1 - cursor_world_p0;

    // Adjust the camera position by the opposite of the delta so that
    // the point under the cursor remains at the same position in screen
    // space.
    opengl.camera_p.x -= cursor_delta[0];
    opengl.camera_p.y -= cursor_delta[1];
    clamp_camera_p(opengl);
}

mouse_button_callback :: proc "c" (window: glfw.Window_Handle, button, action, mods: i32)
{
    window_data := cast(^WindowData)glfw_bindings.GetWindowUserPointer(window);
    opengl := window_data.opengl;
    lmb_pressed := &window_data.lmb_pressed;

    if button == i32(glfw.MOUSE_BUTTON_LEFT) {
        if      action == i32(glfw.PRESS)   do lmb_pressed^ = true;
        else if action == i32(glfw.RELEASE) do lmb_pressed^ = false;
    }
}
