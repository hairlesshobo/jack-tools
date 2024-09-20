#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "failure.h"
#include "memory.h"

extern inline void *xmalloc(size_t size);
extern inline void *xcalloc(size_t count, size_t eltsize);
extern inline void *xrealloc(void *p, size_t size);
extern inline void *xmemcpy(void *dst, const void *src, size_t n);
extern inline void *xmemset(void *s, int c, size_t n);

extern inline float *fmalloc(size_t n);
extern inline void fmemset(float *data, int n, float value);
