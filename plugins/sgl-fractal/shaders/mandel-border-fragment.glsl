#version 410 core

in vec3 position;
in vec2 complexCoordinate;

uniform int maxIterations=100;

out vec4 outColor;

vec4 mandelbrotColor(int maxIter, vec2 pos) {
     int iter;
     float tempzx, tempzy;
     float r2 = 0.0;

     float zx = pos.x;
     float zy = pos.y;

     float ox = zx;
     float oy = zy;
     for (iter = 0; iter < maxIter; iter++)
     {
          tempzx = zx;
          tempzy = zy;
          zx = (tempzx * tempzx) - (tempzy * tempzy);
          zy = 2 * tempzx * tempzy;
          zx += ox;
          zy += oy;
          r2 = r2 * r2;
          r2 = (zx * zx) + (zy * zy);
          if (r2 >= 4)
               break;
     }
     if (r2 < 4) {
          return vec4(0.0, 0.0, 0.0, 1.0); // black
     }
     return vec4(1.0, 1.0, 1.0, 1.0);
}

void main() {
     vec4 diffuseColor = mandelbrotColor(maxIterations, complexCoordinate);
     outColor = diffuseColor;
}
