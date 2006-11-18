/*  xregcomp.c - (c) rohan drape, 2005-2006 */

#include <stdio.h>

#include "failure.h"
#include "xregcomp.h"

int xregcomp(regex_t *preg, const char *regex, int cflags)
{
  int err = regcomp(preg, regex, cflags);
  if(err) {
    char str[64];
    regerror(err, preg, str, 64);
    fprintf(stderr, "regcomp() failed on `%s' with `%d': %s\n", 
	      regex, cflags, str);
    FAILURE;
  }
  return err;
}
