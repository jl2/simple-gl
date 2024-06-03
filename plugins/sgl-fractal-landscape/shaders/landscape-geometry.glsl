#version 460 core

layout(triangles) in;
layout(line_strip, max_vertices = 2) out;

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
          gl_Position = gl_in[i].gl_Position + vec4(teNormal[i], 0.0);
          gPosition = gl_Position.xyz;
          gNormal = teNormal[i];
          EmitVertex();
          EndPrimitive();
     }
}
