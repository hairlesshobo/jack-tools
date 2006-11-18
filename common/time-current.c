/*  time-current.c - (c) rohan drape, 2005-2006 */

#include <stdlib.h>

#include <sys/time.h>

#include "float.h"
#include "time-timeval.h"
#include "time-current.h"

/* Get the current time as a UTC double precision value. */

f64 current_time_as_utc_real(void) 
{
  struct timeval current;
  gettimeofday(&current, NULL); 
  return timeval_to_real(current);
}
  
struct timeval current_time_as_utc_timeval(void)
{
  struct timeval tv;
  gettimeofday(&tv, 0);
  return tv;
}
