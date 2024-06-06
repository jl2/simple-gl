#version 460 core

layout(triangles) in;
layout(triangle_strip, max_vertices = 3) out;

uniform mat4 view_transform = mat4(1);

in vec3 teNormal[];
in vec3 tePosition[];

out vec3 gPosition;
out vec3 gNormal;

void main()
{
     for(int i = 0; i < gl_in.length(); ++i) {
          gl_Position = gl_in[i].gl_Position;
          gPosition = gl_Position.xyz;
          gNormal = teNormal[i];
          EmitVertex();
     }
     EndPrimitive();
}
