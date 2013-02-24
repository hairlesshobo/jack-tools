/* gcc -shared -I ~/opt/include sin.c -o sin.so */

#include <stdlib.h>
#include <math.h>
#include <jack-dl.h>

struct sinosc {
  float f;                      /* frequency */
  float a;                      /* amplitude */
  float p;                      /* pan */
  float phase;                  /* oscillator phase */
};

#define TWO_PI (2 * M_PI)

/* convert frequency in hertz to phase increment in radians */
float hz_to_incr(float hz, float sr)
{
  return (TWO_PI / sr) * hz;
}

/* increment phase within (0,2pi) */
float step_phasor(float *phase, float incr)
{
  *phase += incr;
  if(*phase > TWO_PI) {
    *phase -= TWO_PI;
  }
}

size_t dsp_memreq()
{
  return sizeof(struct sinosc);
}

/* allocate state data and initialize control data */
void dsp_init(void *p)
{
  struct sinosc *s = p;
  s->phase = 0.0;
  s->f = 440.0;
  s->a = 0.1;
  s->p = 0.5;
}

/* process nf frames of data */
void dsp_step(struct world *w, int g, int nf)
{
  int i;
  struct sinosc *s = w_state(w,g);
  /* load state */
  float f = s->f;
  float a = s->a;
  float p = s->p;
  float phase = s->phase;
  /* read control values */
  float fe = w_c_get1(w, (g * 3) + 0);
  float ae = w_c_get1(w, (g * 3) + 1);
  float pe = w_c_get1(w, (g * 3) + 2);
  /* calculate control increments */
  float fi = (fe - f) / (float)nf;
  float ai = (ae - a) / (float)nf;
  float pi = (pe - p) / (float)nf;
  for(i = 0; i < nf; i++) {
    /* algorithm */
    float n = sinf(phase) * a;
    w_out2(w, i, n * p, n * (1 - p));
    float incr = hz_to_incr(f, w_sr(w));
    step_phasor(&phase, incr);
    /* apply control increments */
    f += fi;
    a += ai;
    p += pi;
  }
  /* store state */
  s->f = fe;
  s->a = ae;
  s->p = pe;
  s->phase = phase;
}
