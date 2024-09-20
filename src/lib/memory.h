#ifndef _COMMON_MEMORY_H
#define _COMMON_MEMORY_H

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "failure.h"

inline void *xmalloc(size_t size)
{
	void *p = malloc(size);
	if (p == NULL) {
		fprintf(stderr, "malloc() failed: %ld\n", (long)size);
		FAILURE;
	}
	return p;
}

inline void *xcalloc(size_t count, size_t eltsize)
{
	void *p = calloc(count, eltsize);
	if (p == NULL) {
		perror("calloc() failed");
		FAILURE;
	}
	return p;
}

inline void *xrealloc(void *p, size_t size)
{
	p = realloc(p, size);
	if (p == NULL) {
		perror("realloc() failed");
		FAILURE;
	}
	return p;
}

inline void *xmemcpy(void *dst, const void *src, size_t n)
{
	if (dst == NULL || src == NULL) {
		perror("xmemcpy() failed");
		FAILURE;
	} else {
		return memcpy(dst, src, n);
	}
}

inline void *xmemset(void *s, int c, size_t n)
{
	if (s == NULL) {
		perror("xmemset() failed");
		FAILURE;
	} else {
		return memset(s, c, n);
	}
}

inline float *fmalloc(size_t n)
{
	float *d = (float *)xmalloc(n * sizeof(float));
	size_t i;
	for (i = 0; i < n; i++) {
		d[i] = 0.0;
	}
	return d;
}

inline void fmemset(float *data, int n, float value)
{
	if (data == NULL) {
		perror("fmemset() failed");
		FAILURE;
	}
	{
		int i;
		for (i = 0; i < n; i++) {
			data[i] = value;
		}
	}
}

#endif
