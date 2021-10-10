#version 410 core

layout (vertices = 3) out;
uniform float inner = 8;
uniform float outer = 8;

void main()
{
     gl_TessLevelInner[0] = inner;   //Inside tessellation factor
     gl_TessLevelInner[1] = inner;   //Inside tessellation factor

     gl_TessLevelOuter[0] = outer;   //Edge tessellation factor
     gl_TessLevelOuter[1] = outer;   //Edge tessellation factor
     gl_TessLevelOuter[2] = outer;   //Edge tessellation factor
     gl_TessLevelOuter[3] = outer;   //Edge tessellation factor

     gl_out[gl_InvocationID].gl_Position = gl_in[gl_InvocationID].gl_Position;
}
