#version 460 core

layout(location = 0) in vec3 in_position;

uniform mat4 view_transform = mat4(1);

out vec3 normal;
out vec3 obj_position;
out vec3 position;

void main()
{
     mat4 final_transform = view_transform;
     mat3 norm_view_transform = mat3(transpose(inverse(final_transform)));

     vec4 pos4 = final_transform * vec4(in_position, 1.0);
     obj_position = vec3(pos4);
     position = vec3(pos4);
     normal = vec3(0,0.0,1.0);
     //normal = normalize(norm_view_transform * vec3(0,0.0,1.0));
     gl_Position = pos4;
}
