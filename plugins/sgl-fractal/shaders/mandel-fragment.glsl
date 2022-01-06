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
     float tmpval, tmpval2, red, green, blue, pi, fi;
     pi = 3.141592654;

     // fi = (0.5 + sin(pi * (iter/200.0))) / 2.0;
     // red =   clamp(pow((1.0 - fi), (zx*zy)), 0.0, 1.0);
     // green = clamp(pow(fi, abs(sin(fi+zy))), 0.0, 1.0);
     // blue =  clamp(abs(tan(fi - sin(fi + zx))), 0.0, 1.0);
     // tmpval = fract(iter / 8422.0);
     // tmpval2 = fract(iter / 11133.0);
     // red = tmpval2;
     // green = (1.0 - tmpval);
     // blue = tmpval;
     //      tmpval = fract(iter / 8422.0);
     //      tmpval2 = fract(iter / 11133.0);
     //      red = tmpval2;
     //      green = (1.0 - tmpval);
     //      blue = tmpval;

     // red = clamp(zy * zx * (iter % 67)/66.0, 0.0, 1.0);
     // green = clamp(zy * (iter % 47)/46.0, 0.0, 1.0);
     // blue = clamp(zx * zx * (iter % 24)/23.0, 0.0, 1.0);

     // red = clamp(zy * zx * zx * (iter % 67)/66.0, 0.0, 1.0);
     // green = clamp(zy * zy * (iter % 47)/46.0, 0.0, 1.0);
     // blue = clamp(zx * zx * (iter % 24)/23.0, 0.0, 1.0);

     // red = clamp(zy * sin(3.1415*zx) * (iter % 67)/66.0, 0.0, 1.0);
     // green = clamp(zx * cos(3.1415*zy) * (iter % 47)/46.0, 0.0, 1.0);
     // blue = clamp(zx * zy * (iter % 24)/23.0, 0.0, 1.0);

     // red = clamp(cos(0.3*zy) * sin(0.7*zx) + (iter % 7000)/6999.0, 0.0, 1.0);
     // green = clamp(abs(sin(6*zx) * cos(0.5*zy)) * (1.0 - (iter % 1000)/999.0), 0.0, 1.0);
     // blue = clamp(abs(cos(4*zx + zy)) * (1.0 - (iter % 240)/239.0), 0.0, 1.0);

     // red = fract(cos(0.25*zy) * sin(0.2*zx) * (iter/299.0));
     // green = fract(sin((iter/999.0)* zy * zx) + (iter/999.0));
     // blue = fract(cos(zx * iter));

     pi = 3.141592654;
     // fi = sin(0.5*(iter/24.0));
     // red =   clamp(fi * abs(sin(pow(fi, float(pos.x)))), 0.0, 1.0);
     // green = clamp(fi * abs(sin(pow(fi, float(zx)))), 0.0, 1.0);
     // blue =  clamp(fi * abs(cos(pow(fi, fi)) * sin(pow(float(zx), fi))), 0.0, 1.0);

     // pi = 3.141592654;

     // fi = (0.5 + sin(pi * (iter/2400.0))) / 2.0;
     // red =   clamp(pow((1.0 - fi), (zx*zy)), 0.0, 1.0);
     // green = clamp(pow(fi, abs(sin(fi+zy))), 0.0, 1.0);
     // blue =  clamp(abs(tan(fi - sin(fi + zx))), 0.0, 1.0);

     // pi = 3.141592654;
     fi = (0.85 + sin(pi * (iter/maxIterations))) / 2.0;
     // // red =   clamp(abs(/(1.0+4*fi)), 0.0, 1.0);
     // // green = clamp(, 0.0, 1.0);
     // // blue =  clamp(, 0.0, 1.0);

     red = clamp(abs(sin(20*pi*iter/maxIterations)), 0.0, 1.0);
     green = clamp(abs(cos(zx*20*iter/maxIterations)), 0.0, 1.0);
     blue = clamp(abs(cos(zy*20*iter/maxIterations)), 0.0, 1.0);
     // red = clamp(zx * fi, 0.0, 1.0);
     // green = clamp(abs(sin(zy * fi)), 0.0, 1.0) ;
     // blue = clamp(abs(sin(zx * zy * fi)), 0.0, 1.0);
     // red = abs(sin(dist*maxIterations));
     // green = abs(cos(dist * maxIterations));
     // blue = abs(cos(dist * maxIterations));
//          color = vec4(red, green, blue, 1.0);

     return vec4(red, green, blue, 1.0);
}

void main() {
     vec4 diffuseColor = mandelbrotColor(maxIterations, complexCoordinate);
     outColor = diffuseColor;
}
