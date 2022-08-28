#version 410 core

layout(location = 0) in vec3 in_position;
layout(location = 1) in vec2 in_uv;

uniform mat4 view_transform;

out vec3 position;
out vec2 complexCoordinate;

void main(void)
{
     vec4 pos4 = view_transform * vec4(in_position, 1.0);
     gl_Position = vec4(pos4);

     position = vec3(pos4);
     complexCoordinate = in_uv;
}
