#include <stdbool.h>
#include <jack/jack.h>

#define USE_P_CTL 0

struct world {
  void* (**dsp_init)(struct world *, int);
  void (**dsp_step)(struct world *, int, void *, int);
  void **st;                    /* graph state */
  bool *ga;                     /* graph active */
  void **gh;                    /* graph shared library handle */
  jack_client_t *c;             /* client */
  char cn[64];                  /* client name */
  jack_port_t **ip;             /* input ports */
  jack_port_t **op;             /* output ports */
  int nc;                       /* number of channels */
  int ng;                       /* number of graphs */
  int nk;                       /* number of controls */
  int nb;                       /* number of buffers */
  float sr;                     /* sample rate */
  float **in;                   /* input data */
  float **out;                  /* output data */
  float *ctl;                   /* shared control data */
  float **bd;                   /* buffer data */
  int *bl;                      /* buffer sizes */
#if USE_P_CTL
  float **p_ctl;                /* private control data */
#endif
  bool ef;                      /* exit flag */
};

#define w_sr(w) (w)->sr
#define w_c_get1(w,i) (w)->ctl[(i)]
#define w_c_set1(w,i,n) (w)->ctl[(i)]=(n)
#if USE_P_CTL
#define w_p_get1(w,g,i) (w)->p_ctl[(g)][(i)]
#define w_p_set1(w,g,i,n) (w)->p_ctl[(g)][(i)]=(n)
#endif
#define w_out1(w,i,n) (w)->out[0][(i)]+=(n)
#define w_out2(w,i,n1,n2) {(w)->out[0][(i)]+=(n1);(w)->out[1][(i)]+=(n2);}
#define w_b_read1(w,b,i) (w)->bd[(b)][(i)]
#define w_b_write1(w,b,i,n) (w)->bd[(b)][(i)]=(n)
