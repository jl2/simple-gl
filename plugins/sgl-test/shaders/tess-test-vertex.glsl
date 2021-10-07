#version 410 core

layout(location = 0) in vec2 in_position;
layout(location = 1) in mat4 obj_transform;

uniform float inSize;
uniform vec4 in_color;
uniform mat4 view_transform;

out Quad {
     float size;
     vec4 diffuse_color;
} outQuad;

void main()
{
     vec4 real_position = vec4(in_position.x,
                               0.0,
                               in_position.y,
                               1.0);
     outQuad.size = inSize;
     outQuad.diffuse_color = in_color;

     mat4 final_transform = view_transform * obj_transform;
     gl_Position = final_transform * real_position;
}
