#version 410 core

layout (points) in;
layout (line_strip, max_vertices = 128) out;

in VS_OUT {
     vec4 color;
     mat4 final_transform;
} gs_in[];

uniform vec3 position = vec3(0.98, 0.0229, 2.028);

out vec4 diffuse_color;

vec3 mandel_iterate(vec3 pt) {
     float power = position.z + pt.z;
     float r = sqrt(pt.x*pt.x + pt.y*pt.y);
     float theta = atan(pt.y, pt.x);
     float new_r = pow(r, power);
     
     return vec3(position.xy, pt.z) + vec3(new_r * cos(power * theta),
                                           new_r * sin(power * theta),
                                           0.0);
}


void main() {
     int i;
     vec4 pt = gl_in[0].gl_Position;
     diffuse_color = vec4(0, 0.5, 0, 0.6);
     gl_Position = gs_in[0].final_transform * pt;
     int max_steps = 20;
     float dc = 1.0/max_steps;
     vec4 oldpt = pt;
     EmitVertex();
     for (i=0; i<max_steps; ++i) {
          
          pt = vec4(mandel_iterate(pt.xyz).xy, 0,1);
          if (pt.x*pt.x + pt.y * pt.y > 32) {
               break;
          }
          float mag = pt.x*pt.x + pt.y * pt.y;
          diffuse_color = vec4( (0.8* dc * i) + (mag/32.0),
                                mag/127.0,
                                0.9 - 0.8* (dc * i),
                                ( (0.8 * i)/max_steps ));
          gl_Position = gs_in[0].final_transform * pt;
          EmitVertex();
          oldpt = pt;
     }

     EndPrimitive();
}
