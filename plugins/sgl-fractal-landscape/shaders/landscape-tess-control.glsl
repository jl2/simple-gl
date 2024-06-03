#version 460 core

// Input from vertex shader
layout (vertices = 16) out;

uniform float inner = 16;
uniform float outer = 32;

in mat3 vNormal_view_transform[];
out mat3 tcNormal_view_transform[];

// Per-vertex output
// out float tcFoo;

// Per-patch output
// patch out float tcSharedFoo;

void main()
{
     bool cull = false;

     tcNormal_view_transform[gl_InvocationID] = vNormal_view_transform[gl_InvocationID];

     if (cull) {
          gl_TessLevelOuter[0] = 0.0;
          gl_TessLevelOuter[1] = 0.0;
          gl_TessLevelOuter[2] = 0.0;
          gl_TessLevelOuter[3] = 0.0;

     } else {
          gl_TessLevelInner[0] = inner;   //Inside tessellation factor
          gl_TessLevelInner[1] = inner;   //Inside tessellation factor

          gl_TessLevelOuter[0] = outer;   //Edge tessellation factor
          gl_TessLevelOuter[1] = outer;   //Edge tessellation factor
          gl_TessLevelOuter[2] = outer;   //Edge tessellation factor
          gl_TessLevelOuter[3] = outer;   //Edge tessellation factor
     }
     gl_out[gl_InvocationID].gl_Position = gl_in[gl_InvocationID].gl_Position;
}
