/***** jack.dl.c - (c) rohan drape, 2003-2008 *****/

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <time.h>
#include <dlfcn.h>

#include <jack/jack.h>
#include <jack/thread.h>
#include <lo/lo.h>

#include "common/jack-port.h"
#include "jack.dl.h"

void fail(char *s)
{
  fprintf(stderr, s);
  exit(EXIT_FAILURE);
}

int dsp_run(jack_nframes_t nf, void *ptr)
{
  struct world *w = (struct world *)ptr;
  for(int i = 0; i < w->nc; i++) {
    w->in[i] = (float *)jack_port_get_buffer(w->ip[i], nf);
  }
  for(int i = 0; i < w->nc; i++) {
    w->out[i] = (float *)jack_port_get_buffer(w->op[i], nf);
    memset(w->out[i], 0, nf * sizeof(float));
  }
  for(int i = 0; i < w->ng; i++) {
    if(w->ga[i]) {
      w->dsp_step[i](w, i, w->st[i], nf);
    }
  }
  return 0;
}

void osc_error(int n, const char *m, const char *p)
{
  fprintf(stderr,"jack.dl: error %d in path %s: %s\n", n, p, m);
}

int osc_g_load(const char *p, const char *t, lo_arg **a, int n, void *d, void *u)
{
  struct world *w = (struct world *)u;
  int g = a[0]->i;
  if(g >= w->ng) {
    fprintf(stderr,"g_load: illegal graph number: %d\n", g);
    return 0;
  }
  w->ga[g] = false;
  char *s = &a[1]->s;
  void *h = dlopen(s, RTLD_LAZY);
  if(!h) {
    fprintf(stderr,"g_load: dlopen failed: %s\n", dlerror());
    return 0;
  }
  w->dsp_init[g] = dlsym(h, "dsp_init");
  w->dsp_step[g] = dlsym(h, "dsp_step");
  w->st[g] = w->dsp_init[g](w, g);
  w->ga[g] = true;
  fprintf(stderr,"jack.dl: g_load: %d, %s\n", g, s);
  return 0;
}

int osc_c_set1(const char *p, const char *t, lo_arg **a, int n, void *d, void *u)
{
  struct world *w = (struct world *)u;
  w_c_set1(w, a[0]->i, a[1]->f);
  fprintf(stderr,"jack.dl: c_set1: %d, %f\n", a[0]->i, a[1]->f);
  return 0;
}

int osc_p_set1(const char *p, const char *t, lo_arg **a, int n, void *d, void *u)
{
  struct world *w = (struct world *)u;
  w_p_set1(w, a[0]->i, a[1]->i, a[2]->f);
  fprintf(stderr,"jack.dl: p_set1: %d, %d, %f\n", a[0]->i, a[1]->i, a[2]->f);
  return 0;
}

int osc_quit(const char *p, const char *t, lo_arg **a, int n, void *d, void *u)
{
  struct world *w = (struct world *)u;
  w->ef = true;
  return 0;
}

void world_init(struct world *w, int ng, int nc, int nk)
{
  w->ng = ng;
  w->nc = nc;
  w->nk = nk;
  w->dsp_init = calloc(w->ng, sizeof(void *));
  w->dsp_step = calloc(w->ng, sizeof(void *));
  w->st = calloc(w->ng, sizeof(void *));
  w->p_ctl = malloc(w->ng * sizeof(float *));
  for(int i = 0; i < w->ng; i++) {
    w->p_ctl[i] = calloc(w->nk, sizeof(float));
  }
  w->ga = calloc(w->ng, sizeof(bool));
  w->ip = malloc(w->nc * sizeof(jack_port_t *));
  w->op = malloc(w->nc * sizeof(jack_port_t *));
  w->in = malloc(w->nc * sizeof(float *));
  w->out = malloc(w->nc * sizeof(float *));
  w->ctl = calloc(w->nk, sizeof(float));
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
  lo_server_thread osc;
  world_init(&w, 4, 8, 64);
  osc = lo_server_thread_new("57190", osc_error);
  lo_server_thread_add_method(osc, "/c_set1", "if", osc_c_set1, &w);
  lo_server_thread_add_method(osc, "/p_set1", "iif", osc_p_set1, &w);
  lo_server_thread_add_method(osc, "/g_load", "is", osc_g_load, &w);
  lo_server_thread_add_method(osc, "/quit", NULL, osc_quit, &w);
  lo_server_thread_start(osc);
  if(jack_activate(w.c)) fail("jack.dl: jack_activate() failed\n");
  while(!w.ef) {
    struct timespec t = {0, 100000000};
    nanosleep(&t, NULL);
  }
  jack_client_close(w.c);
  lo_server_thread_free(osc);
  return EXIT_SUCCESS;
}
