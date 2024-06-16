#version 460 core

layout (location = 0) out vec4 out_color;

in vec3 gNormal;
in vec3 gPosition;

uniform vec3 cam_position = vec3(0.0,0.0,-10.0);
uniform vec4 light_pos = vec4(-3.0, 3.0, -20.0, 1.0);
uniform vec3 light_color = vec3(1.0, 1.0, 1.0);
uniform float light_power = 90.0;

uniform vec4 Kd = vec4(0.1, 0.9, 0.2, 1.0);
uniform vec3 Ka = vec3(0.03, 0.013, 0.03);
uniform vec3 Ks = vec3(0.0, 0.0, 0.0);

uniform float Ns = 20.0;
uniform vec3 Ke = vec3(0,0,0);

uniform float d = 1.0;
uniform float illum = 2;

const float screen_gamma = 1.3;

void main() {

     if (gPosition.z > 0.8) {
          out_color = mix(vec4(0,1,0,1),
                          vec4(1,1,1,1),
                          gPosition.z);
     }
     else if (gPosition.z > 0.2) {
          out_color = mix(vec4(0.8,0.8,0,1),
                          vec4(0,1,0,1),
                          gPosition.z);
     }
     else if (gPosition.z <= -0.2) {
          out_color = vec4(0,0,0.7,1);
     }
     else if (gPosition.z <= 0.2) {
          out_color = mix(vec4(0, 0, 1, 1),
                          vec4(0.8,0.8,0,1),
                          gPosition.z);
     }

     vec3 light_dir = gPosition.xyz - light_pos.xyz;
     float distance = length(light_dir);
     distance = distance * distance;
     light_dir = normalize(light_dir);

     float lambertian = dot(light_dir,
                            gNormal);

     vec3 view_dir = -normalize(gPosition.xyz - cam_position);

     // Blinn-Phong
     vec3 half_dir = normalize(light_dir - view_dir);
     float spec_angle = max(dot( gNormal, half_dir), 0.0);
     float specular = pow(spec_angle, Ns);


     vec3 color_linear =
          Ke +
          Ka +
          out_color.rgb * lambertian * light_color * light_power / distance +
          Ks.rgb * specular * light_color * light_power / distance;
     // apply gamma correction (assume ambient_color, diffuseColor and spec_color
     // have been linearized, i.e. have no gamma correction in them)
     vec3 color_gamma_corrected = pow(color_linear, vec3(1.0 / screen_gamma));
     // use the gamma corrected color in the fragment
     out_color = vec4(color_gamma_corrected, Kd.a);
}

