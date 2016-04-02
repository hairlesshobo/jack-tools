#include <stdbool.h>
#include <jack/jack.h>

struct world {
  size_t (*dsp_memreq)();
  void (*dsp_init)(void *);
  void (*dsp_step)(struct world *, int);
  void *st;                    /* graph state */
  bool ga;                     /* graph active */
  void *gh;                    /* graph shared library handle */
  jack_client_t *c;            /* client */
  char cn[64];                 /* client name */
  jack_port_t **ip;            /* input ports */
  jack_port_t **op;            /* output ports */
  int nc;                      /* number of channels */
  int nk;                      /* number of controls */
  int nb;                      /* number of buffers */
  float sr;                    /* sample rate */
  float **in;                  /* input data */
  float **out;                 /* output data */
  float *ctl;                  /* (shared) control data */
  float **bd;                  /* buffer data */
  int *bl;                     /* buffer sizes (0 == not-ready) */
  bool ef;                     /* exit flag */
};

#define df_world struct world
#define w_state(w) (w)->st
#define w_sr(w) (w)->sr
#define w_c_get1(w,i) (w)->ctl[(i)]
#define w_c_set1(w,i,n) (w)->ctl[(i)]=(n)
#define w_in1(w,i) (w)->in[0][(i)]
#define w_out1(w,i,n) (w)->out[0][(i)]=(n)
#define w_out2(w,i,n1,n2) {(w)->out[0][(i)]=(n1);(w)->out[1][(i)]=(n2);}
/* run-time check... */
#define w_b_read1(w,b,i) (w)->bl[(b)] > (i) ? (w)->bd[(b)][(i)] : 0.0
#define w_b_write1(w,b,i,n) if((w)->bl[(b)] > (i)) {(w)->bd[(b)][(i)]=(n);}
