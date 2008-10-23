#include <stdbool.h>
#include <jack/jack.h>

struct world {
  void* (*dsp_init)(struct world *);
  void (*dsp_step)(struct world *, void *, int);
  jack_client_t *c;             /* client */
  jack_port_t **ip;             /* input ports */
  jack_port_t **op;             /* output ports */
  int nc;                       /* number of channels */
  float sr;                     /* sample rate */
  float **in;                   /* input data */
  float **out;                  /* output data */
  float ctl[64];                /* control data */
  void *st;                     /* state */
  bool ef;                      /* exit flag */
};

#define w_sr(w) (w)->sr
#define w_cget(w,i) (w)->ctl[(i)]
#define w_cset(w,i,n) (w)->ctl[(i)]=(n)
#define w_out1(w,i,n) (w)->out[0][(i)]=(n)
#define w_out2(w,i,n1,n2) {(w)->out[0][(i)]=(n1);(w)->out[1][(i)]=(n2);}
