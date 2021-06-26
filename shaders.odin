package main

ndc_xy_vert_shader := `
    #version 330

    layout(location = 0) in vec2 in_Position;
    layout(location = 1) in vec2 in_UV;

    smooth out vec2 UV;

    uniform vec2 camera_p;
    uniform float camera_scale;

    void main()
    {
        UV = in_UV;
        vec2 p = (in_Position - camera_p) * camera_scale;
        gl_Position = vec4(p, 0, 1);
    }
`;

text_vert_shader := `
    #version 330

    layout(location = 0) in vec2 in_Position;
    layout(location = 1) in vec2 in_UV;

    smooth out vec2 UV;

    uniform vec2 Resolution;

    void main()
    {
        UV = in_UV;
        vec2 P = 2.0 * (in_Position / Resolution) - 1.0;
        P.y = -P.y;
        gl_Position = vec4(P, 0.0, 1.0);
    }
`;

text_frag_shader := `
    #version 330

    smooth in vec2 UV;
    out vec4 FragColor;

    uniform sampler2D Font;

    void main()
    {
        vec4 Alpha = texture(Font, UV);
        FragColor = vec4(1, 1, 1, Alpha.r);
    }
`;

texture_frag_shader := `
    #version 330

    smooth in vec2 UV;
    out vec4 frag_color;

    uniform sampler2D in_texture;
    uniform bool use_color_coding;

    void main()
    {
        if (use_color_coding) {
            vec2 c = texture(in_texture, UV).xy;
            c = (c + 1) / 2;
            frag_color = vec4(c.x, c.y, 0, 1);
        } else {
            frag_color = vec4(texture(in_texture, UV).rgb, 1);
        }
    }
`;

advection_shader := `
    #version 430

    layout(local_size_x = 32, local_size_y = 32, local_size_z = 1) in;
    layout(rgba32f, binding = 0) uniform writeonly image2D new_field;

    uniform sampler2D field;
    uniform sampler2D velocity;

    float dt = 0.016;

    vec2 compute_velocity(vec2 p)
    {
        return texture(velocity, p).xy;
    }

    void main()
    {
        ivec2 p = ivec2(gl_WorkGroupID.xy * gl_WorkGroupSize.xy + gl_LocalInvocationID.xy);
        vec2 size = vec2(gl_NumWorkGroups.xy * gl_WorkGroupSize.xy);

        vec2 uv = (vec2(p) + 0.5) / size;

        vec2 k1 = compute_velocity(uv);
        vec2 k2 = compute_velocity(uv - dt * k1 * 0.5);
        vec2 k3 = compute_velocity(uv - dt * k2 * 0.5);
        vec2 k4 = compute_velocity(uv - dt * k3 * 1.0);

        uv -= dt * (1.0 / 6.0) * (k1 + 2.0*(k2 + k3) + k4);

        imageStore(new_field, p, vec4(texture(field, uv).rgb, 1));
    }
`;

divergence_shader := `
    #version 430

    layout(local_size_x = 32, local_size_y = 32, local_size_z = 1) in;
    layout(r32f, binding = 0) uniform writeonly image2D divergence;

    uniform sampler2D field;

    void main()
    {
        ivec2 p = ivec2(gl_WorkGroupID.xy * gl_WorkGroupSize.xy + gl_LocalInvocationID.xy);
        ivec2 size = ivec2(gl_NumWorkGroups.xy * gl_WorkGroupSize.xy);

        ivec2 pL = p + ivec2(-1,  0);
        ivec2 pT = p + ivec2( 0,  1);
        ivec2 pR = p + ivec2( 1,  0);
        ivec2 pB = p + ivec2( 0, -1);

        pL = clamp(pL, ivec2(0), size - 1);
        pT = clamp(pT, ivec2(0), size - 1);
        pR = clamp(pR, ivec2(0), size - 1);
        pB = clamp(pB, ivec2(0), size - 1);

        vec4 l = texelFetch(field, pL, 0);
        vec4 t = texelFetch(field, pT, 0);
        vec4 r = texelFetch(field, pR, 0);
        vec4 b = texelFetch(field, pB, 0);

        float div = (r.x - l.x + t.y - b.y) * 0.5 * 0.5;

        imageStore(divergence, p, vec4(div, 0, 0, 1));
    }
`;

jacobi_shader := `
    #version 430

    layout(local_size_x = 32, local_size_y = 32, local_size_z = 1) in;
    layout(r32f, binding = 0) uniform writeonly image2D new_pressure;

    uniform sampler2D pressure;
    uniform sampler2D divergence;

    float pressure_sample(float x, float y)
    {
        return texelFetch(pressure, ivec2(x, y), 0).r;
    }

    void main()
    {
        ivec2 p = ivec2(gl_WorkGroupID.xy * gl_WorkGroupSize.xy + gl_LocalInvocationID.xy);
        ivec2 size = ivec2(gl_NumWorkGroups.xy * gl_WorkGroupSize.xy);

        float x = p.x;
        float y = p.y;

        float l, t, r, b;

        if (x == 0)          l = pressure_sample(x + 1, y);
        else                 l = pressure_sample(x - 1, y);

        if (y == size.y - 1) t = pressure_sample(x, y - 1);
        else                 t = pressure_sample(x, y + 1);

        if (x == size.x - 1) r = pressure_sample(x - 1, y);
        else                 r = pressure_sample(x + 1, y);

        if (y == 0)          b = pressure_sample(x, y + 1);
        else                 b = pressure_sample(x, y - 1);

        float div = texelFetch(divergence, p, 0).r;

        float result = (l + t + r + b - 4*div) * 0.25;
        imageStore(new_pressure, p, vec4(result, 0, 0, 1));
    }
`;

gradient_shader := `
    #version 430

    layout(local_size_x = 32, local_size_y = 32, local_size_z = 1) in;
    layout(rgba32f, binding = 0) uniform writeonly image2D out_velocity;

    uniform sampler2D velocity;
    uniform sampler2D pressure;

    void main()
    {
        ivec2 p = ivec2(gl_WorkGroupID.xy * gl_WorkGroupSize.xy + gl_LocalInvocationID.xy);
        ivec2 size = ivec2(gl_NumWorkGroups.xy * gl_WorkGroupSize.xy);

        ivec2 pL = p + ivec2(-1,  0);
        ivec2 pT = p + ivec2( 0,  1);
        ivec2 pR = p + ivec2( 1,  0);
        ivec2 pB = p + ivec2( 0, -1);

        pL = clamp(pL, ivec2(0), size - 1);
        pT = clamp(pT, ivec2(0), size - 1);
        pR = clamp(pR, ivec2(0), size - 1);
        pB = clamp(pB, ivec2(0), size - 1);

        float l = texelFetch(pressure, pL, 0).r;
        float t = texelFetch(pressure, pT, 0).r;
        float r = texelFetch(pressure, pR, 0).r;
        float b = texelFetch(pressure, pB, 0).r;

        vec2 grad = vec2(r - l, t - b) / (2.0 * 2.0);
        vec2 vel = texelFetch(velocity, p, 0).xy;

        if (p.x == 0 || p.x == size.x - 1) vel.x = 0;
        if (p.y == 0 || p.y == size.y - 1) vel.y = 0;

        imageStore(out_velocity, p, vec4(vel - grad, 0, 1));
    }
`;
