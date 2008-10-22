/* gcc -shared -I.. -I/home/rohan/include sin.c -o sin.so */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "jack.dl.h"

struct sinosc {
  float phase;                  /* oscillator phase */
  float incr;                   /* osillator increment */
}; 

#define TWO_PI (2 * M_PI)

float hz_to_incr(float hz, float sr)
{
  return (TWO_PI / sr) * hz;
}

float step_phasor(float *phase, float incr)
{
  *phase += incr;
  if(*phase > TWO_PI) {
    *phase -= TWO_PI;
  }
}
  
void *dsp_init(struct world *w)
{
  struct sinosc *s = malloc(sizeof(s));
  s->phase = 0.0;
  w_cset(w, 0, 440.0);
  w_cset(w, 1, 0.1);
  w_cset(w, 2, 0.5);
  return (void*)s;
}

void dsp_step(struct world *w, void *ptr, int nf)
{
  int i;
  float n;
  struct sinosc *s = (struct sinosc *)ptr;
  for(i = 0; i < nf; i++) {
    s->incr = hz_to_incr(w_cget(w, 0), w_sr(w));
    step_phasor(&(s->phase), s->incr);
    float g = w_cget(w, 1);
    float n = sinf(s->phase) * g;
    float p = w_cget(w, 2);
    w_out2(w, i, n * p, n * (1 - p));
  }
}
