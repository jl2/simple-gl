#version 400 core

in vec3 position;
in vec2 complexCoordinate;

uniform int maxIterations=100;
uniform float cReal=-1.5057239992026854;
uniform float cImag=-0.4609143758687352;

out vec4 outColor;

vec4 bsJsColor(int maxIter, vec2 pos) {
     int iter;
     float tempreal, tempimag;
     float r2 = 0.0;

     float zx = pos.x;
     float ox = zx;

     float zy = -pos.y;
     float oy = zy;

     for (iter = 0; iter < maxIterations; iter++)
     {
          float xtemp = abs(zx*zx*zx) - abs(3 * zx * zy * zy) + cReal;
          zy = (abs(3 * zx * zx * zy) - abs(zy * zy * zy)) + cImag;
          zx = xtemp;
          r2 = (zx * zx) + (zy * zy);
          if (r2 >= 4)
               break;
     }
     if (r2 < 4) {
          return vec4(0.0, 0.0, 0.0, 0.0); // black
     }
     float tmpval, tmpval2, red, green, blue, pi, fi;
     pi = 3.141592654;
     red = clamp(abs(sin(20*pi*iter/maxIterations)), 0.0, 1.0);
     green = clamp(abs(cos(zx*20*iter/maxIterations)), 0.0, 1.0);
     blue = clamp(abs(cos(zy*20*iter/maxIterations)), 0.0, 1.0);
     // fi = (0.5 + sin(pi * (iter/20.0))) / 2.0;
     // red =   clamp(pow((1.0 - fi), (zx*zy)), 0.0, 1.0);
     // green = clamp(pow(fi, abs(sin(fi+zy))), 0.0, 1.0);
     // blue =  clamp(abs(tan(fi - sin(fi + zx))), 0.0, 1.0);
     // fi = (0.5 + sin(pi * (iter/20.0))) / 2.0;
     // red =   clamp(pow((1.0 - fi), zy), 0.0, 1.0);
     // green = clamp(pow(fi, abs(sin(zy))), 0.0, 1.0);
     // blue =  clamp(abs(tan(fi - sin( zx))), 0.0, 1.0);

     return vec4(red, green, blue, 1.0);
}

void main() {
     vec4 diffuseColor = bsJsColor(maxIterations, complexCoordinate);
     outColor = diffuseColor;
}
