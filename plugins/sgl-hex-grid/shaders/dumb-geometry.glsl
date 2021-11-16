#version 450 core

layout (points) in;
layout (points, max_vertices = 7) out;

in VS_OUT {
     flat float hex_radius;
     flat int hex_state;
} gs_in[];

uniform mat4 obj_transform = mat4(1);
uniform mat4 view_transform = mat4(1);;
uniform float time = 0.0;

flat out vec4 diffuse_color;

void main() {
     mat4 final_transform = view_transform * obj_transform;
     vec2 pos = gl_in[0].gl_Position.xy;
     gl_Position = final_transform * vec4(gs_in[0].hex_radius * cos(pos.x + 0.125 * time), gs_in[0].hex_radius * sin(pos.y + 0.125 * time), 0, 1);
     gl_PointSize = 8.0;
     diffuse_color = vec4(1,0,0,1);
     EmitVertex();
     gl_Position = final_transform * vec4(gs_in[0].hex_state * cos(pos.x + 0.25 * time), gs_in[0].hex_state * sin(pos.y + 0.15 * time), 0, 1);
     gl_PointSize = 16.0;
     diffuse_color = vec4(0,1,0,1);
     EmitVertex();
     EndPrimitive();
}
