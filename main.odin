package main

import "core:fmt"
import gl "shared:odin-gl"
import glfw "shared:odin-glfw"

WINDOW_WIDTH :: 1024;
WINDOW_HEIGHT :: 512;

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

    init_queries();

    for !glfw.window_should_close(window_handle) {
        glfw.poll_events();

        gl.ClearColor(0, 0, 0, 1);

        render(&opengl, WINDOW_WIDTH, WINDOW_HEIGHT);
        check_queries();

        glfw.swap_buffers(window_handle);
    }

    glfw.destroy_window(window_handle);
}
