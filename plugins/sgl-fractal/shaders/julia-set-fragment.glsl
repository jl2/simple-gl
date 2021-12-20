#version 400 core

in vec3 position;
in vec2 complexCoordinate;

uniform int maxIterations=1000;
uniform float cReal=0.324;
uniform float cImag=-0.2345;

out vec4 outColor;

vec4 juliaSetColor(int maxIter, vec2 pos) {
     int iter;
     float tempzx, tempzy;
     float r2 = 0.0;

     float zx = pos.x;
     float zy = pos.y;

     for (iter = 0; iter < maxIterations; iter++)
     {
          // z = z^2 + c
          tempzx = zx;
          tempzy = zy;
          zx = (tempzx * tempzx) - (tempzy * tempzy);
          zy = 2 * tempzx * tempzy;
          zx += cReal;
          zy += cImag;
          r2 = (zx * zx) + (zy * zy);
          if (r2 >= 4)
               break;
     }
     if (r2 < 4) {
          return vec4(0.0, 0.0, 0.0, 1.0);
     }

     float tmpval, tmpval2, red, green, blue, pi, fi;
     pi = 3.141592654;

     tmpval = fract(iter / 8422.0);
     tmpval2 = fract(iter / 11133.0);

     fi = (0.5 + sin(pi * (iter/200.0))) / 2.0;
     red =   clamp(pow((1.0 + fi), (zy)), 0.0, 1.0);
     green = clamp(pow(fi, abs(sin(fi*zy))), 0.0, 1.0);
     blue =  clamp(abs(tan(fi - sin(fi * zy))), 0.0, 1.0);

     return vec4(0.5, green, blue, 1.0);
}

void main() {
     vec4 diffuseColor = juliaSetColor(maxIterations, complexCoordinate);
     outColor = diffuseColor;
}
