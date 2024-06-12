#version 410 core

layout(location = 0) in vec3 in_position;
layout(location = 1) in vec2 in_tex;

in vec2 tex_coord;

out vec4 color;
uniform sampler2D image;

void main() {
     color = texture(image, tex_coord);
}
