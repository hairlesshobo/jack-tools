/* gcc -shared -I ~/include sin.c -o sin.so */

#include <stdlib.h>
#include <math.h>
#include <jack.dl.h>

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
  
void *dsp_init(struct world *w, int g)
{
  struct sinosc *s = malloc(sizeof(s));
  s->phase = 0.0;
  w_p_set1(w, g, 0, 440.0);
  w_p_set1(w, g, 1, 0.1);
  w_p_set1(w, g, 2, 0.5);
  return (void*)s;
}

void dsp_step(struct world *w, int g, void *ptr, int nf)
{
  int i;
  struct sinosc *s = (struct sinosc *)ptr;
  for(i = 0; i < nf; i++) {
    float f = w_p_get1(w, g, 0);
    float a = w_p_get1(w, g, 1);
    float p = w_p_get1(w, g, 2);
    s->incr = hz_to_incr(f, w_sr(w));
    step_phasor(&(s->phase), s->incr);
    float n = sinf(s->phase) * a;
    w_out2(w, i, n * p, n * (1 - p));
  }
}
