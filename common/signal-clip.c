/*  signal-clip.c - (c) rohan drape, 2005-2006 */

#include "signal-clip.h"

/* l = left, r = right, n = length of signal */

void signal_clip(float *s, int n, float l, float r)
{
  int i;
  for(i = 0; i < n; i++) {
    if(s[i] < l) {
      s[i] = l;
    } else if(s[i] > r) {
      s[i] = r;
    }
  }
}
