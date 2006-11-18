/*  time-timespec.c - (c) rohan drape, 2005-2006 */

#include <stdlib.h>

#include "time-timespec.h"

struct timespec usec_to_timespec(unsigned long usec)
{
  struct timespec t;
  long nsec = usec * 1000;
  ldiv_t d = ldiv(nsec, 1000000000L);
  t.tv_sec = d.quot;
  t.tv_nsec = d.rem;
  return t;
}
