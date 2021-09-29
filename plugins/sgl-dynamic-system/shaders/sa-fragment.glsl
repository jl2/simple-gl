#version 410 core

in vec4 diffuse_color;
out vec4 out_color;

void main()
{
    out_color = diffuse_color;
}
