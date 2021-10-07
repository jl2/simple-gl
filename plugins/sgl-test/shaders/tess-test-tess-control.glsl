#version 410 core

layout (vertices = 3) out;

uniform float _factor;

void main()
{
     if (gl_InvocationID == 0)
          {
               gl_TessLevelInner[0] = _factor;   //Inside tessellation factor
               gl_TessLevelInner[1] = _factor;   //Inside tessellation factor

               gl_TessLevelOuter[0] = _factor;   //Edge tessellation factor
               gl_TessLevelOuter[1] = _factor;   //Edge tessellation factor
               gl_TessLevelOuter[2] = _factor;   //Edge tessellation factor
               gl_TessLevelOuter[3] = _factor;   //Edge tessellation factor
          }
     gl_out[gl_InvocationID].gl_Position = gl_in[gl_InvocationID].gl_Position;
}
