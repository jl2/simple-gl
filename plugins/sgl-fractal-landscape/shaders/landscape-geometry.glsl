#version 460 core

layout(triangles) in;
layout(triangle_strip, max_vertices = 4) out;

uniform mat4 view_transform = mat4(1);

in vec3 teNormal[];
in vec3 tePosition[];

out vec3 gPosition;
out vec3 gNormal;

void main()
{
     mat3 norm_view_transform = mat3(transpose(inverse(view_transform)));
     for(int i = 0; i < gl_in.length(); ++i) {
          gl_Position = gl_in[i].gl_Position.xyzw;
          gPosition = gl_Position.xyz;
          gNormal = teNormal[i];
          EmitVertex();
     }
     EndPrimitive();
}
