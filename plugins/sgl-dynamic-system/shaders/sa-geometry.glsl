#version 410 core

layout (points) in;
layout (points, max_vertices = 128) out;

in VS_OUT {
     vec4 color;
     mat4 final_transform;
} gs_in[];

uniform float a = 0.125;
uniform float b = 0.125;
uniform float c = 0.125;
uniform float d = 0.125;
uniform float e = 0.125;

out vec4 diffuse_color;

vec4 fpt(vec4 pt) {
     return vec4( sin(a * pt.y) - pt.z * cos(b * pt.x),
                  cos(c * pt.x) - cos(d * pt.y),
                  e * sin(pt.x),
                  1.0);
}

void main() {
     vec4 pt = fpt(gl_in[0].gl_Position);
     diffuse_color = gs_in[0].color;
     gl_Position = gs_in[0].final_transform * pt;
     EmitVertex();
     for (int i=0; i<127; ++i) {
          pt = fpt(pt);
          gl_Position = gs_in[0].final_transform * pt;
          EmitVertex();
     }

     EndPrimitive();
}
