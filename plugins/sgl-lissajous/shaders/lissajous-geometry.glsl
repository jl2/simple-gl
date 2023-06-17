#version 410 core

layout (points) in;
layout (points, max_vertices = 2) out;

in VS_OUT {
     vec4 color;
     mat4 final_transform;
} gs_in[];


out vec4 diffuse_color;

void main() {
     vec4 pt = gl_in[0].gl_Position;
     diffuse_color = gs_in[0].color;
     gl_Position = gs_in[0].final_transform * pt;
     EmitVertex();

     // pt = gl_in[1].gl_Position;
     // diffuse_color = gs_in[1].color;
     // gl_Position = gs_in[1].final_transform * pt;
     // EmitVertex();

     EndPrimitive();
}
