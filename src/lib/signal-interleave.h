#ifndef _COMMON_SIGNAL_INTERLEAVE_H
#define _COMMON_SIGNAL_INTERLEAVE_H

#include "float.h"

void signal_interleave(f32 *dst, const f32 *src, int f, int c);
void signal_uninterleave(f32 *dst, const f32 *src, int f, int c);
void signal_interleave_to(f32 *dst, const f32 **src, int f, int c);
void signal_uninterleave_to(f32 **dst, const f32 *src, int f, int c);

#endif
