#version 410 core

layout (points) in;
layout (line_strip, max_vertices = 2) out;
// in VS_IN {
//      vec4 color;
// }
void main() {
     gl_Position = gl_in[0].gl_Position + vec4(-0.1, 0.0, 0.0, 0.0);
     EmitVertex();

     gl_Position = gl_in[0].gl_Position + vec4( 0.1, 0.0, 0.0, 0.0);
     EmitVertex();
     EndPrimitive();
}
