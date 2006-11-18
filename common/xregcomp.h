#ifndef _COMMON_XREGCOMP_H
#define _COMMON_XREGCOMP_H

#include <regex.h>

int xregcomp(regex_t *preg, const char *regex, int cflags);

#endif

