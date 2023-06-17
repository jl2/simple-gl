#version 410 core

layout(location = 0) in vec3 in_position;
layout(location = 1) in vec4 in_color;
layout(location = 2) in mat4 obj_transform;

uniform mat4 view_transform = mat4(1);

// out VS_OUT {
//      vec4 color;
//      mat4 final_transform;
// } vs_out;

out vec4 diffuse_color;

void main()
{
     mat4 final_transform = view_transform * obj_transform;
     gl_Position = final_transform * vec4(in_position, 1);
     diffuse_color = in_color;
}
