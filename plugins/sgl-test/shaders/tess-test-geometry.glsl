#version 410 core

layout(triangles) in;
layout(triangle_strip, max_vertices = 128) out;
in vec3 teNormal[];
void main()
{
     for(int i = 0; i < gl_in.length(); ++i)
          {
               gl_Position = gl_in[i].gl_Position;
               EmitVertex();
          }
     EndPrimitive();
}
