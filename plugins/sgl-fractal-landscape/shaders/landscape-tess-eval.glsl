#version 460 core

layout( quads, equal_spacing, ccw ) in;

uniform mat4 view_transform;
uniform float time = 0.0;
out vec3 teNormal;
out vec3 tePosition;

vec3 fuv(float u, float v) {
     return vec3(u,
                 v,
                 0.4 * sin(0.5* sin(2 * u + v) * 4 * u) * cos(2 * cos(u) * v));
}
void main() {
     float pi = 3.141592654;
     vec4 p00 = gl_in[0].gl_Position;
     vec4 p10 = gl_in[1].gl_Position;
     vec4 p20 = gl_in[2].gl_Position;
     vec4 p30 = gl_in[3].gl_Position;
     float u = (p10 - p00).x + p00.x + gl_TessCoord.x;
     u *=  2 * pi;
     u +=  sin(0.01 * time);
     //u -=  pi  + pi/2;



     float v = (p20 - p00).y + p00.y + gl_TessCoord.y;
     v *=  2 * pi;
     // v += pi/2;
     v += cos(0.01 * time);
     vec3 pt0 = fuv(u, v);

     float u1 = u + 0.01;
     float v1 = v + 0.01;
     vec3 pt1 = fuv(u1, v);
     vec3 pt2 = fuv(u, v1);

     gl_Position = view_transform * vec4(pt0, 1.0);
     tePosition = gl_Position.xyz;
     teNormal = normalize(cross(pt1-pt0, pt2-pt0));
}
