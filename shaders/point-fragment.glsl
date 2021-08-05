#version 410 core

uniform mat4 view_transform;

in vec3 normal;
in vec3 position;
in vec4 diffuse_color;

out vec4 out_color;

void main()
{
    out_color = diffuse_color;
}
