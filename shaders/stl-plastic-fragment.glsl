#version 410 core

uniform mat4 view_transform;

in vec3 normal;
in vec3 obj_position;
in vec3 position;
in vec4 diffuse_color;
in vec3 cam_position;

layout (location = 0) out vec4 out_color;

const vec4 light_pos = vec4(-10.0, 10.0, -20.0, 1.0);
const vec3 light_color = vec3(1.0, 1.0, 1.0);
const float light_power = 190.0;
const vec3 ambient_color = vec3(0.03, 0.03, 0.03);
const vec3 spec_color = vec3(1.0, 1.0, 1.0);
const float shininess = 128.0;
const float screen_gamma = 1.3;

void main() {
     out_color = diffuse_color;
     vec3 light_dir = obj_position - light_pos.xyz;
     float distance = length(light_dir);
     distance = distance * distance;
     light_dir = normalize(light_dir);

     float lambertian = dot(light_dir,
                                normal);

     float specular = 0.0;



     vec3 view_dir = -normalize(obj_position - cam_position);

     // Blinn-Phong
     vec3 half_dir = normalize(light_dir - view_dir);
     float spec_angle = max(dot( normal, half_dir), 0.0);
     specular = pow(spec_angle, shininess);


     vec3 color_linear = ambient_color +
          diffuse_color.rgb * lambertian * light_color * light_power / distance +
          spec_color * specular * light_color * light_power / distance;
     // apply gamma correction (assume ambient_color, diffuseColor and spec_color
     // have been linearized, i.e. have no gamma correction in them)
     vec3 color_gamma_corrected = pow(color_linear, vec3(1.0 / screen_gamma));
     // use the gamma corrected color in the fragment
     out_color = vec4(color_gamma_corrected, diffuse_color.a);
}
