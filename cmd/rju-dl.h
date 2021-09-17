#include <stdbool.h>
#include <stdint.h>
#include <jack/jack.h>

struct world {
  size_t (*dsp_memreq)();
  void (*dsp_init)(void *);
  void (*dsp_step)(struct world *, int32_t);
  void *st;                    /* graph state */
  bool ga;                     /* graph active */
  void *gh;                    /* graph shared library handle */
  jack_client_t *c;            /* client */
  char cn[64];                 /* client name */
  jack_port_t **ip;            /* input ports */
  jack_port_t **op;            /* output ports */
  int32_t nc;                  /* number of channels */
  int32_t nk;                  /* number of controls */
  int32_t nb;                  /* number of buffers */
  double sr;                   /* sample rate */
  float **in;                  /* input data - jackd allocated */
  float **out;                 /* output data - jackd allocated */
  double *ctl;                 /* (shared) control data */
  float **bd;                  /* buffer data */
  int64_t *bl;                 /* buffer sizes (0 == not-ready) */
  bool ef;                     /* exit flag */
  bool vb;                     /* verbose */
  void* rng_st;                /* random number generator state */
  double (*rng_gen)(struct world *, double, double);
};

#define df_world struct world
#define w_state(w) (w)->st
#define w_sr(w) (w)->sr
#define w_c_get1(w,i) (w)->ctl[(i)]
#define w_c_set1(w,i,n) (w)->ctl[(i)]=(n)
#define w_in1(w,c,i) (double)((w)->in[(c)][(i)])
#define w_out1(w,c,i,n) (w)->out[(c)][(i)]=(float)(n)
#define w_out2(w,c,i,n1,n2) {(w)->out[(c)][(i)]=(float)(n1);(w)->out[(c)+1][(i)]=(float)(n2);}
#define w_frand(w,n1,n2) ((w)->rng_gen((w),(n1),(n2)))
/* run-time check... */
#define w_buf_read1(w,b,i) (w)->bl[(b)] > (i) ? (w)->bd[(b)][(i)] : 0.0
#define w_buf_write1(w,b,i,n) if((w)->bl[(b)] > (i)) {(w)->bd[(b)][(i)]=(n);}
