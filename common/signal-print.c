/*  signal-print.c - (c) rohan drape, 2005 */

#include <stdio.h>

#include "float.h"
#include "signal-print.h"

void signal_print(FILE *fp, const char *prefix, const f32 *f, int n)
{
  int i;
  fprintf(fp, "%s", prefix);
  for(i = 0; i < n; i++) {
    fprintf(fp, "%2.1f ", f[i]);
  }
  fprintf(fp, "\n");
}
