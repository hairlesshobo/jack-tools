/*  signal-interpolate.c - (c) rohan drape, 2005-2006 */

#include <math.h>

#include "int.h"
#include "float.h"
#include "signal-interpolate.h"

f32 signal_interpolate (const f32 *signal, int signal_n, f32 index)
{
  i32 index_i = (i32)floorf(index);
  f32 index_f = index - (f32)index_i;
  if (index_i == signal_n - 1) {
    return (signal[index_i] * (1.0 - index_f)) + (signal[0] * index_f);
  } else {
    return (signal[index_i] * (1.0 - index_f)) + (signal[index_i+1] * index_f);
  }
}

f32 signal_interpolate_safe (const f32 *signal, int signal_n, f32 index)
{
  if (index < 0 || index >= (f32) signal_n) {
    return 0.0;
  }
  return signal_interpolate (signal, signal_n, index);
}
