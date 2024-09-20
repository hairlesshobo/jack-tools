#ifndef _COMMON_FAILURE_H
#define _COMMON_FAILURE_H

#include <stdlib.h>

#ifndef FAILURE
#define FAILURE exit(1)
#endif

#define die_when(x, ...) \
	if (x) { \
		fprintf(stderr, __VA_ARGS__); \
		exit(1); \
	}

#endif
