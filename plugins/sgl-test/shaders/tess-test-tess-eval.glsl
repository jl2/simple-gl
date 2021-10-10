#version 460 core
layout( triangles, equal_spacing, cw ) in;
uniform mat4 obj_transform;
uniform mat4 view_transform;

void main() {
     vec4 pt = (gl_TessCoord.x * gl_in[0].gl_Position) +
          (gl_TessCoord.y * gl_in[1].gl_Position) +
          (gl_TessCoord.z * gl_in[2].gl_Position);
     gl_Position = (obj_transform * view_transform) *
          vec4(pt.x, pt.y, pt.z, 1.0);
}
