#version 410 core

layout(location = 0) in vec2 in_position;

uniform mat4 view_transform = mat4(1);

out VS_OUT {
     vec4 color;
     mat4 final_transform;
} vs_out;

void main()
{
     vs_out.final_transform = view_transform;
     gl_Position = vec4(in_position, 0.0, 1.0);
     vs_out.color = vec4(0, 0.8, 0.8, 0.1);
}
