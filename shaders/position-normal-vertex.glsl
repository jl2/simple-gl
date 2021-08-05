#version 410 core

layout(location = 0) in vec3 in_position;
layout(location = 1) in vec3 in_normal;
layout(location = 2) in mat4 obj_color;
layout(location = 3) in mat4 obj_transform;

uniform mat4 view_transform;

out vec3 normal;
out vec3 obj_position;
out vec3 position;

void main()
{
     mat4 final_transform = view_transform * obj_transform;
     mat3 norm_view_transform = mat3(transpose(inverse(obj_transform)));

     vec4 pos4 = final_transform * vec4(in_position, 1.0);
     obj_position = in_position;
     gl_Position = pos4;
     position = vec3(pos4);
     normal = normalize(norm_view_transform * in_normal);
}
