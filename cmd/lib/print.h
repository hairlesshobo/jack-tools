#ifndef _COMMON_PRINT_H
#define _COMMON_PRINT_H

#include <stdio.h>

/* error-printer */
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

/* verbose-printer */
#define vprintf(_verbose, ...) \
	if (_verbose) { \
		fprintf(stderr, __VA_ARGS__); \
	}

/* debug-printer */
#ifdef DEBUG
#define dprintf(...) fprintf(stderr, __VA_ARGS__)
#else
#define dprintf(...)
#endif

#endif
