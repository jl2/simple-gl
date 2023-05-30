#version 410 core

layout(location = 0) in vec3 in_position;
layout(location = 1) in vec4 in_color;
layout(location = 2) in vec3 translation;

uniform mat4 view_transform;

out vec4 diffuse_color;
out vec4 position;

void main()
{
     mat4 final_transform = view_transform;
     vec3 real_translation = vec3(translation.x, translation.y, 0);
     vec4 pos4 = final_transform * vec4(in_position + real_translation, 1.0);

     gl_Position = pos4;
     position = pos4;
     diffuse_color = (1-log(0.75*translation.z)) * in_color;
}
