package main

ndc_xy_vert_shader := `
    #version 330

    layout(location = 0) in vec2 in_Position;
    layout(location = 1) in vec2 in_UV;

    smooth out vec2 UV;

    void main()
    {
        UV = in_UV;
        gl_Position = vec4(in_Position, 0.0, 1.0);
    }
`;

text_vert_shader_code := `
    #version 330

    layout(location = 0) in vec2 in_Position;
    layout(location = 1) in vec2 in_UV;

    uniform vec2 Resolution;

    smooth out vec2 UV;

    void main()
    {
        UV = in_UV;
        vec2 P = 2.0 * (in_Position / Resolution) - 1.0;
        P.y = -P.y;
        gl_Position = vec4(P, 0.0, 1.0);
    }
`;

text_frag_shader_code := `
    #version 330

    smooth in vec2 UV;
    out vec4 FragColor;

    uniform sampler2D Font;

    void main()
    {
        vec4 Alpha = texture(Font, UV);
        FragColor = vec4(0.3, 0.3, 1.0, Alpha.r);
    }
`;

texture_frag_shader := `
    #version 330

    smooth in vec2 UV;
    out vec4 FragColor;

    uniform sampler2D Texture;

    void main()
    {
        FragColor = vec4(texture(Texture, UV).rgb, 1);
    }
`;

advection_shader := `
    #version 330

    uniform sampler2D Color;
    uniform sampler2D Velocity;
    uniform bool UseRK4;
    uniform ivec2 Size;

    out vec4 FragColor;

    float dt = 0.016;

    vec2
    compute_velocity(vec2 P)
    {
        return texture(Velocity, P).xy;
    }

    void main()
    {
        vec2 P = gl_FragCoord.xy / Size;

        vec2 k1 = compute_velocity(P);

        if (true) {
            vec2 k2 = compute_velocity(P - dt * k1 * 0.5);
            vec2 k3 = compute_velocity(P - dt * k2 * 0.5);
            vec2 k4 = compute_velocity(P - dt * k3 * 1.0);

            P = P - dt * (1.0 / 6.0) * (k1 + 2.0*(k2 + k3) + k4);
        } else {
            P = P - dt * k1;
        }

        FragColor = vec4(texture(Color, P).rgb, 1);
    }
`;

divergence_shader := `
    #version 330

    uniform sampler2D Field;
    uniform ivec2 Size;

    out vec4 Output;

    void main()
    {
        ivec2 P = ivec2(gl_FragCoord.xy - 0.5);

        ivec2 PL = P + ivec2(-1,  0);
        ivec2 PT = P + ivec2( 0,  1);
        ivec2 PR = P + ivec2( 1,  0);
        ivec2 PB = P + ivec2( 0, -1);

        PL = clamp(PL, ivec2(0), Size - 1);
        PT = clamp(PT, ivec2(0), Size - 1);
        PR = clamp(PR, ivec2(0), Size - 1);
        PB = clamp(PB, ivec2(0), Size - 1);

        vec4 L = texelFetch(Field, PL, 0);
        vec4 T = texelFetch(Field, PT, 0);
        vec4 R = texelFetch(Field, PR, 0);
        vec4 B = texelFetch(Field, PB, 0);

        float Div = (R.x - L.x + T.y - B.y) * 0.5;

        Output = vec4(Div, 0, 0, 1);
    }
`;

jacobi_shader := `
    #version 330

    uniform sampler2D Pressure;
    uniform sampler2D Divergence;

    uniform ivec2 Size;

    out vec4 Output;

    void main()
    {
        ivec2 P = ivec2(gl_FragCoord.xy - 0.5);

        ivec2 PL = P + ivec2(-1,  0);
        ivec2 PT = P + ivec2( 0,  1);
        ivec2 PR = P + ivec2( 1,  0);
        ivec2 PB = P + ivec2( 0, -1);

        PL = clamp(PL, ivec2(0), Size - 1);
        PT = clamp(PT, ivec2(0), Size - 1);
        PR = clamp(PR, ivec2(0), Size - 1);
        PB = clamp(PB, ivec2(0), Size - 1);

        vec3 L = texelFetch(Pressure, PL, 0).rgb;
        vec3 T = texelFetch(Pressure, PT, 0).rgb;
        vec3 R = texelFetch(Pressure, PR, 0).rgb;
        vec3 B = texelFetch(Pressure, PB, 0).rgb;
        vec3 Div = texelFetch(Divergence, P, 0).rgb;

        vec3 Result = (L + T + R + B - Div) * 0.25;
        Output = vec4(Result, 1);
    }
`;

gradient_shader := `
    #version 330

    uniform sampler2D Field;
    uniform sampler2D Pressure;
    uniform ivec2 Size;

    out vec4 Output;

    void main()
    {
        ivec2 P = ivec2(gl_FragCoord.xy - 0.5);

        ivec2 PL = P + ivec2(-1,  0);
        ivec2 PT = P + ivec2( 0,  1);
        ivec2 PR = P + ivec2( 1,  0);
        ivec2 PB = P + ivec2( 0, -1);

        PL = clamp(PL, ivec2(0), Size - 1);
        PT = clamp(PT, ivec2(0), Size - 1);
        PR = clamp(PR, ivec2(0), Size - 1);
        PB = clamp(PB, ivec2(0), Size - 1);

        float L = texelFetch(Pressure, PL, 0).r;
        float T = texelFetch(Pressure, PT, 0).r;
        float R = texelFetch(Pressure, PR, 0).r;
        float B = texelFetch(Pressure, PB, 0).r;

        vec2 Grad = vec2(R - L, T - B) / (2.0);
        vec2 F = texelFetch(Field, P, 0).xy;

        Output = vec4(F - Grad, 0, 1);
    }
`;

field_texture_frag_shader := `
    #version 330

    smooth in vec2 UV;
    out vec4 FragColor;

    uniform sampler2D Texture;

    void main()
    {
        vec2 F = texture(Texture, UV).xy;
        F = (F + 1) / 2;
        FragColor = vec4(F.x, F.y, 0, 1);
    }
`;
