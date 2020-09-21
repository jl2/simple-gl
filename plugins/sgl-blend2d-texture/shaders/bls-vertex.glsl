#version 330 core

layout(location = 0) in vec3 position;
layout(location = 1) in vec2 uv;

uniform mat4 transform;

out vec2 texCoordOut;

void main()
{
    texCoordOut = uv;
    gl_Position = transform * vec4(position.xyz, 1.0);
}
