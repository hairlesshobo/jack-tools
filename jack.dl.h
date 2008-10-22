#include <jack/jack.h>

#define MAX_NC 2

struct world {
  void* (*dsp_init)(struct world *);
  void (*dsp_step)(struct world *, void *, int);
  jack_client_t *c;             /* client */
  jack_port_t *op[MAX_NC];      /* output ports */
  int nc;                       /* number of channels */
  float sr;                     /* sample rate */
  float *in[MAX_NC];            /* input data */
  float *out[MAX_NC];           /* output data */
  float ctl[64];                /* control data */
  void *st;                     /* state */
  int ef;                       /* exit flag */
};

#define w_sr(w) (w)->sr
#define w_cget(w,i) (w)->ctl[(i)]
#define w_cset(w,i,n) (w)->ctl[(i)]=(n)
#define w_out1(w,i,n) (w)->out[0][(i)]=(n)
#define w_out2(w,i,n1,n2) {(w)->out[0][(i)]=(n1);(w)->out[1][(i)]=(n1);}
