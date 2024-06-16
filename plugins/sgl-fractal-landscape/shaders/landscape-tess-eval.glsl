#version 460 core

layout( quads, equal_spacing, ccw ) in;

uniform mat4 view_transform;

out vec3 teNormal;
out vec3 tePosition;

void main() {
     vec4 p00 = gl_in[ 0].gl_Position;
     vec4 p10 = gl_in[ 1].gl_Position;
     vec4 p20 = gl_in[ 2].gl_Position;
     vec4 p30 = gl_in[ 3].gl_Position;
     float u = (p10 - p00).x + p00.x + gl_TessCoord.x;
     u *= 3.141592654;
     float v = (p20 - p00).y + p00.y + gl_TessCoord.y;
     v *= 2 * 3.141592654;
     vec3 pt0 = vec3(2 * sin(u) * cos(v),
                     2 * sin(u) * sin(v),
                     2 * cos(u));
     float u1 = u + 0.001;
     float v1 = v + 0.001;
     vec3 pt1 = vec3(2 * sin(u1) * cos(v),
                     2 * sin(u1) * sin(v),
                     2 * cos(u1));
     vec3 pt2 = vec3(2 * sin(u) * cos(v1),
                     2 * sin(u) * sin(v1),
                     2 * cos(u));
     
     gl_Position = view_transform * vec4(pt0,
                                         1.0);
     tePosition = p00.xyz + gl_Position.xyz;
     teNormal = normalize(cross(pt1-pt0, pt0-pt2));
}


