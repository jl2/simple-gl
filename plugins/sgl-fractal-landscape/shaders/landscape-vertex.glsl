#version 460 core

layout(location = 0) in vec3 in_position;

uniform mat4 view_transform = mat4(1);

out mat3 vNormal_view_transform;

void main()
{
     mat4 final_transform = view_transform;
     vNormal_view_transform = mat3(transpose(inverse(final_transform)));

     vec4 pos4 = final_transform * vec4(in_position, 1.0);

     gl_Position = pos4;
}
