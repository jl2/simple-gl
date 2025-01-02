#version 410 core

layout (points) in;
layout (line_strip, max_vertices = 128) out;

in VS_OUT {
     vec4 color;
     mat4 final_transform;
} gs_in[];

uniform float power = 2.028;
uniform vec2 c = vec2(0.98, 0.0229);

out vec4 diffuse_color;

vec2 mandel_iterate(vec2 pt) {
     float r = sqrt(pt.x * pt.x + pt.y * pt.y);
     float theta = atan(pt.y, pt.x);
     float new_r = pow(r, power);
     
     return c + vec2(new_r * cos(power * (theta)),
                     new_r * sin(power * (theta)));
}
vec4 fpt(vec4 pt) {
     float divisor = 0.2 * 3.1415;
     return vec4(sin(pt.x), cos(pt.y * cos(pt.x)), pt.y, pt.w);
}

float fx(float xv) {
    // return sin(xv  + sin(1 * xv * xv)) + cos(xv);
    // return sin(xv + sin( xv * sin(xv) - cos( xv * sin(xv))));
    return 6.0*sin(xv + xv * sin( xv - xv * cos(xv) * cos( xv * sin(xv))));
}



void main() {
     int i;
     vec4 pt = gl_in[0].gl_Position;
     diffuse_color = vec4(0,0.5,0, 0.8);
     gl_Position = gs_in[0].final_transform * pt;
     int max_steps = 20;
     float blue = 1.0;
     float red = 0.0;
     float dc = 1.0/max_steps;
     vec4 oldpt = pt;
     EmitVertex();
     for (i=0; i<max_steps; ++i) {
          
          pt = vec4(mandel_iterate(pt.xy), 0,1);
          if (pt.x*pt.x + pt.y * pt.y > 64) {
               break;
          }
          float mag = pt.x*pt.x + pt.y * pt.y;
          diffuse_color = vec4( (dc * i) + (mag/48.0),0, 1.0 - (dc * i), 0.4);
          gl_Position = gs_in[0].final_transform * pt;
          EmitVertex();
          oldpt = pt;
     }

     EndPrimitive();
}
