#version 410 core

in vec4 diffuse_color;
in vec4 position;
layout (location = 0) out vec4 out_color;


void main()
{
    out_color = diffuse_color;
}
