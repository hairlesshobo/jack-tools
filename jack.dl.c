/***** jack.dl.c - (c) rohan drape, 2003-2008 *****/

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <time.h>
#include <dlfcn.h>

#include <jack/jack.h>
#include <jack/thread.h>
#include <lo/lo.h>

#include "common/jack-port.h"
#include "jack.dl.h"

int dsp_run(jack_nframes_t nf, void *ptr)
{
  struct world *w = (struct world *)ptr;
  for(int i = 0; i < w->nc; i++) {
    w->out[i] = (float *)jack_port_get_buffer(w->op[i], nf);
  }
  w->dsp_step(w, w->st, nf);
  return 0;
}

void osc_error(int n, const char *m, const char *p)
{
  fprintf(stderr,"jack.dl: error %d in path %s: %s\n", n, p, m);
}

int osc_cset(const char *p, const char *t, lo_arg **a, int n, void *d, void *u)
{
  struct world *w = (struct world *)u;
  w_cset(w, a[0]->i, a[1]->f);
  fprintf(stderr,"jack.dl: c_set: %d, %f\n", a[0]->i, a[1]->f);
  return 0;
}

int osc_quit(const char *p, const char *t, lo_arg **a, int n, void *d, void *u)
{
  struct world *w = (struct world *)u;
  w->ef = true;
  return 0;
}

void fail(char *s)
{
  fprintf(stderr, s);
  exit(EXIT_FAILURE);
}

void world_init(struct world *w, int nc)
{
  w->nc = nc;
  w->ip = malloc(w->nc * sizeof(jack_port_t *));
  w->op = malloc(w->nc * sizeof(jack_port_t *));
  w->in = malloc(w->nc * sizeof(float *));
  w->out = malloc(w->nc * sizeof(float *));
  w->ef = false;
  w->c = jack_client_new("jack.dl");
  if(!w->c) fail("jack.dl: could not create jack client\n");
  jack_set_process_callback(w->c, dsp_run, w);
  w->sr = (float)jack_get_sample_rate(w->c);
  jack_port_make_standard(w->c, w->ip, w->nc, false);
  jack_port_make_standard(w->c, w->op, w->nc, true);
}

int main(int argc, char **argv)
{
  struct world w;
  void *h;
  lo_server_thread osc;
  h = dlopen(argv[1], RTLD_LAZY);
  if(!h) fail(dlerror());
  w.dsp_init = dlsym(h, "dsp_init");
  w.dsp_step = dlsym(h, "dsp_step");
  world_init(&w, 8);
  osc = lo_server_thread_new("57190", osc_error);
  lo_server_thread_add_method(osc, "/c_set", "if", osc_cset, &w);
  lo_server_thread_add_method(osc, "/quit", NULL, osc_quit, &w);
  lo_server_thread_start(osc);
  if(jack_activate(w.c)) fail("jack.dl: jack_activate() failed\n");
  w.st = w.dsp_init(&w);
  while(!w.ef) {
    struct timespec t = {0, 100000000};
    nanosleep(&t, NULL);
  }
  jack_client_close(w.c);
  lo_server_thread_free(osc);
  return EXIT_SUCCESS;
}
