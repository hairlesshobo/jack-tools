#ifndef _COMMON_IMG_H
#define _COMMON_IMG_H

#include "int.h"
#include "float.h"

void img_set_pixel(u8 *data, i32 w, i32 x, i32 y, const u8 *pixel);
i32 signal_x_to_screen_x(f32 x, i32 width);
i32 signal_y_to_screen_y(f32 y, i32 height);
i32 normal_x_to_screen_x(f32 x, i32 width);
i32 normal_y_to_screen_y(f32 y, i32 height);

#endif
