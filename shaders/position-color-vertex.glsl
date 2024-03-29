#version 410 core

layout(location = 0) in vec3 in_position;
layout(location = 1) in vec4 in_color;

uniform mat4 obj_transform = mat4(1);
uniform mat4 view_transform = mat4(1);

out vec4 diffuse_color;

void main()
{
     mat4 final_transform = view_transform * obj_transform;
     vec4 pos4 = final_transform * vec4(in_position, 1.0);

     gl_Position = pos4;
     diffuse_color = in_color;
}
