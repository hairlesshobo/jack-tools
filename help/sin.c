/* gcc -shared -I ~/include sin.c -o sin.so */

#include <stdlib.h>
#include <math.h>
#include <jack.dl.h>

struct sinosc {
  float phase;                  /* oscillator phase */
  float f, a, p;                /* frequency, amplitude, pan */
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
  struct sinosc *s = malloc(sizeof(struct sinosc));
  s->phase = 0.0;
  s->f = 440.0;
  s->a = 0.1;
  s->p = 0.5;
  w_p_set1(w, g, 0, s->f);
  w_p_set1(w, g, 1, s->a);
  w_p_set1(w, g, 2, s->p);
  return (void*)s;
}

void dsp_step(struct world *w, int g, void *ptr, int nf)
{
  int i;
  struct sinosc *s = (struct sinosc *)ptr;
  float f = s->f;
  float a = s->a;
  float p = s->p;
  float fe = w_p_get1(w, g, 0);
  float ae = w_p_get1(w, g, 1);
  float pe = w_p_get1(w, g, 2);
  float fi = (fe - f) / (float)nf;
  float ai = (ae - a) / (float)nf;
  float pi = (pe - p) / (float)nf;
  for(i = 0; i < nf; i++) {
    float incr = hz_to_incr(f, w_sr(w));
    step_phasor(&(s->phase), incr);
    float n = sinf(s->phase) * a;
    w_out2(w, i, n * p, n * (1 - p));
    f += fi;
    a += ai;
    p += pi;
  }
  s->f = fe;
  s->a = ae;
  s->p = pe;
}
