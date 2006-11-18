/*  time-ntp.c - (c) rohan drape, 2005-2006 */

#include <stdlib.h>
#include <stdint.h>

#include <sys/time.h>

#include "int.h"
#include "float.h"
#include "time-timeval.h"
#include "time-ntp.h"

u64 utc_timeval_to_ntp(struct timeval tv)
{
  const u64 secdif = (u64)2208988800UL; /* seconds from 1900 to 1970 */
  const f64 ms_to_ntp = (f64)4294.967296;
  const u64 a = (u64)tv.tv_sec + secdif;
  const u64 b = (u64) ((f64)tv.tv_usec * ms_to_ntp);
  return (a << 32) +  b;
}

u64 utc_real_to_ntp(f64 utc)
{
  return utc_timeval_to_ntp(real_to_timeval(utc));
}

/* This routine uses the factorization 2^32/10^6 = 4096 + 256 -
   1825/32 which results in a max conversion error of 3 * 10^-7 and an
   average error of half that. */

u32 utc_usec_to_ntp_approx(u32 usec)
{
  u32 t = (usec * 1825) >> 5;
  return (usec << 12) + (usec << 8) - t;
}

u64 utc_timeval_to_ntp_approx(struct timeval tv) 
{
  const u64 secdif = (u64)2208988800UL; /* seconds from 1900 to 1970 */
  return ((u64)tv.tv_sec + (secdif << 32)) + ((u64)utc_usec_to_ntp_approx (tv.tv_usec));
}
