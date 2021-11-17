#version 410 core
layout(location = 0) in vec2 in_position;
layout(location = 1) in float radius;
layout(location = 2) in int state;

out VS_OUT {
     flat float hex_radius;
     flat int hex_state;
} vs_out;


uniform mat4 obj_transform;
uniform mat4 view_transform;

void main()
{
     gl_Position = vec4(in_position.xy, 0, 1.0);
     vs_out.hex_radius = radius;
     vs_out.hex_state = state;
}
