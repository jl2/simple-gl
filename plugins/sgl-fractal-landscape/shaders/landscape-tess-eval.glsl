#version 460 core

layout( quads, equal_spacing, ccw ) in;

uniform mat4 view_transform;

out vec3 teNormal;
out vec3 tePosition;

void main() {
     float pi = 3.141592654;
     vec4 p00 = gl_in[0].gl_Position;
     vec4 p10 = gl_in[1].gl_Position;
     vec4 p20 = gl_in[2].gl_Position;
     vec4 p30 = gl_in[3].gl_Position;
     float u = (p10 - p00).x + p00.x + gl_TessCoord.x;
     u *=  pi;
     u -=  pi  + pi/2;
     float v = (p20 - p00).y + p00.y + gl_TessCoord.y;
     v *=  pi;
     v += pi/2;
     vec3 pt0 = vec3(u,
                     v,
                      0.4 * sin(2* u) * cos(2 * v));
     float u1 = u + 0.0001;
     float v1 = v + 0.0001;
     vec3 pt1 = vec3( u1,
                      v,
                      0.4 * sin(82 * u1) * cos( pi * pi * pi * pi * v));
     vec3 pt2 = vec3( u,
                      v1,
                      sin(37 * u) * cos( 32 * v1));
     
     gl_Position = view_transform * vec4(pt0, 1.0);
     tePosition = gl_Position.xyz;
     teNormal = normalize(cross(pt1-pt0, pt2-pt0));
}


