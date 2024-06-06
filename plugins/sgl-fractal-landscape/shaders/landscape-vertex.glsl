#version 460 core

layout(location = 0) in vec3 in_position;

uniform mat4 view_transform = mat4(1);

void main()
{
     gl_Position = vec4(in_position, 1.0);
}
