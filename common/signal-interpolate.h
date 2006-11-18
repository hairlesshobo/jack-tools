#ifndef _COMMON_SIGNAL_INTERPOLATE_H
#define _COMMON_SIGNAL_INTERPOLATE_H

#include "float.h"

f32 signal_interpolate(const f32 *signal, int signal_n, f32 index);
f32 signal_interpolate_safe(const f32 *signal, int signal_n, f32 index);

#endif
