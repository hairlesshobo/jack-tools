#include "signal-interleave.h"
#include "float.h"

/* dst = frame-order data, src = channel-order data, f = number of frames, c = number of channels */
void signal_interleave(f32 *dst, const f32 *src, int f, int c)
{
	int i, k = 0;
	for (i = 0; i < f; i++) {
		int j;
		for (j = 0; j < c; j++) {
			dst[k++] = src[(j * f) + i];
		}
	}
}

/* dst = channel-order data, src = frame-order data, f = number of frames, c = number of channels */
void signal_uninterleave(f32 *dst, const f32 *src, int f, int c)
{
	int i, k = 0;
	for (i = 0; i < f; i++) {
		int j;
		for (j = 0; j < c; j++) {
			dst[(j * f) + i] = src[k++];
		}
	}
}

void signal_interleave_to(f32 *dst, const f32 **src, int f, int c)
{
	int i, k = 0;
	for (i = 0; i < f; i++) {
		int j;
		for (j = 0; j < c; j++) {
			dst[k++] = src[j][i];
		}
	}
}

void signal_uninterleave_to(f32 **dst, const f32 *src, int f, int c)
{
	int i, k = 0;
	for (i = 0; i < f; i++) {
		int j;
		for (j = 0; j < c; j++) {
			dst[j][i] = src[k++];
		}
	}
}
