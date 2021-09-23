#version 410 core

layout (points) in;
layout (line_strip, max_vertices = 128) out;

in VS_OUT {
     vec4 color;
     mat4 final_transform;
} gs_in[];

uniform float dh = 0.125;

out vec4 diffuse_color;

vec4 fpt(vec4 pt) {
     float divisor = 0.2 * 3.1415;
     return vec4(pt.x, cos(pt.y + cos(pt.x)), pt.y, pt.w);
}

float fx(float xv) {
     // return sin(xv * xv + sin(3 * xv * xv)) + cos(xv);
     return sin(xv + sin(xv * xv)) + cos(xv);
}

void main() {
     int i;
     float h = 3.141592543 / 512;
     vec4 pt = gl_in[0].gl_Position;
     diffuse_color = gs_in[0].color;
     gl_Position = gs_in[0].final_transform * pt;
     vec4 oldpt = pt;
     EmitVertex();
     for (i=0; i<127; ++i) {
          pt = vec4(oldpt.x - h * fx(oldpt.z),
                    oldpt.y,
                    oldpt.z + h * fx(oldpt.x),
                    oldpt.w);
          gl_Position = gs_in[0].final_transform * pt;
          EmitVertex();
          oldpt = pt;
     }

     EndPrimitive();
}
