/*  img.c - (c) rohan drape, 2005-2006 */

#include <stdio.h>
#include <string.h>
#include <math.h>

#include "failure.h"
#include "memory.h"
#include "int.h"
#include "float.h"

/* data = pixel map, w = width, x = column number, y = row number,
   pixel = RGB */

void img_set_pixel(u8 *data, i32 w, i32 x, i32 y, const u8 *pixel)
{
  i32 n = ((w * y) + x) * 3;
  memcpy(data + n, pixel, 3);
}

i32 signal_x_to_screen_x(f32 x, i32 width)
{
  f32 xx = (x / 2.0) + 0.5;
  return floorf(xx * (f32)width);
}

i32 signal_y_to_screen_y(f32 y, i32 height)
{
  f32 yy = 1.0 - ((y / 2.0) + 0.5);
  return floorf(yy * (f32)height);
}

i32 normal_x_to_screen_x(f32 x, i32 width)
{
  return floorf(x * (f32)width);
}

i32 normal_y_to_screen_y(f32 y, i32 height)
{
  return floorf((1.0 - y) * (f32)height);
}
