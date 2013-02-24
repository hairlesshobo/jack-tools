#include <dlfcn.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include <jack/jack.h>
#include <jack/thread.h>
#include <lo/lo.h>

#include "c-common/jack-port.h"
#include "c-common/print.h"
#include "jack-dl.h"

void fail(char *s)
{
  fprintf(stderr, "%s", s);
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
      w->dsp_step[i](w, i, nf);
    }
  }
  return 0;
}

void osc_error(int n, const char *m, const char *p)
{
  fprintf(stderr,"jack-dl: error %d in path %s: %s\n", n, p, m);
}

#define break_on(x,s)                                                   \
  if(x){                                                                \
    fprintf(stderr,"jack-dl: %s: %s, %d\n", s, __FILE__, __LINE__);     \
    return 0;                                                           \
  }

int osc_g_load(const char *p, const char *t, lo_arg **a, int n, void *d, void *u)
{
  struct world *w = (struct world *)u;
  int g = a[0]->i;
  break_on(g >= w->ng, "graph index");
  w->ga[g] = false;
  char *s = &a[1]->s;
  if(w->gh[g]) break_on(dlclose(w->gh[g]), dlerror());
  w->gh[g] = dlopen(s, RTLD_LAZY);
  break_on(!w->gh[g], dlerror());
  w->dsp_memreq[g] = dlsym(w->gh[g], "dsp_memreq");
  w->dsp_init[g] = dlsym(w->gh[g], "dsp_init");
  w->dsp_step[g] = dlsym(w->gh[g], "dsp_step");
  size_t k = w->dsp_memreq[g]();
  w->st[g] = malloc(k);
  w->dsp_init[g](w->st[g]);
  w->ga[g] = true;
  fprintf(stderr,"g_load: %d, %s\n", g, s);
  return 0;
}

int osc_g_unload(const char *p, const char *t, lo_arg **a, int n, void *d, void *u)
{
  struct world *w = (struct world *)u;
  int g = a[0]->i;
  break_on(g >= w->ng, "graph index");
  w->ga[g] = false;
  if(w->gh[g]) break_on(dlclose(w->gh[g]), dlerror());
  w->gh[g] = NULL;
  if(w->st[g]) free(w->st[g]);
  fprintf(stderr,"g_unload: %d\n", g);
  return 0;
}

int osc_c_set(const char *p, const char *t, lo_arg **a, int n, void *d, void *u)
{
  struct world *w = (struct world *)u;
  int i = a[0]->i;
  break_on(i >= w->nk, "control index");
  w_c_set1(w, i, a[1]->f);
  fprintf(stderr,"c_set: %d, %f\n", i, a[1]->f);
  return 0;
}

int osc_b_alloc(const char *p, const char *t, lo_arg **a, int n, void *d, void *u)
{
  struct world *w = (struct world *)u;
  int i = a[0]->i;
  break_on(i >= w->nb, "buffer index");
  /*if(w->bd[i]) free(w->bd[i]);*/
  int l = a[1]->i;
  int c = a[2]->i;
  break_on(c != 1, "buffer not single channel...");
  w->bl[i] = 0;
  w->bd[i] = calloc(l, sizeof(float));
  w->bl[i] = l;
  fprintf(stderr,"b_alloc: %d, %d, %d\n", i, l, c);
  return 0;
}

#if USE_P_CTL
int osc_p_set1(const char *p, const char *t, lo_arg **a, int n, void *d, void *u)
{
  struct world *w = (struct world *)u;
  int g = a[0]->i;
  break_on(g >= w->ng, "graph index");
  int i = a[1]->i;
  break_on(i >= w->nk, "control index");
  w_p_set1(w, g, i, a[2]->f);
  fprintf(stderr,"p_set1: %d, %d, %f\n", g, i, a[2]->f);
  return 0;
}
#endif

int osc_quit(const char *p, const char *t, lo_arg **a, int n, void *d, void *u)
{
  struct world *w = (struct world *)u;
  w->ef = true;
  return 0;
}

void world_init(struct world *w, int ng, int nc, int nk, int nb)
{
  w->ng = ng;
  w->nc = nc;
  w->nk = nk;
  w->nb = nb;
  w->dsp_memreq = calloc(w->ng, sizeof(void *));
  w->dsp_init = calloc(w->ng, sizeof(void *));
  w->dsp_step = calloc(w->ng, sizeof(void *));
  w->st = calloc(w->ng, sizeof(void *));
#if USE_P_CTL
  w->p_ctl = malloc(w->ng * sizeof(float *));
  for(int i = 0; i < w->ng; i++) {
    w->p_ctl[i] = calloc(w->nk, sizeof(float));
  }
#endif
  w->ga = calloc(w->ng, sizeof(bool));
  w->gh = calloc(w->ng, sizeof(void *));
  w->ip = malloc(w->nc * sizeof(jack_port_t *));
  w->op = malloc(w->nc * sizeof(jack_port_t *));
  w->in = malloc(w->nc * sizeof(float *));
  w->out = malloc(w->nc * sizeof(float *));
  w->ctl = calloc(w->nk, sizeof(float));
  w->bl = calloc(w->nb, sizeof(int));
  w->bd = calloc(w->ng, sizeof(float*));
  w->ef = false;
  strncpy(w->cn,"jack-dl",64);
  w->c = jack_client_open(w->cn,JackNullOption,NULL);
  if(!w->c) fail("could not create jack client\n");
  jack_set_process_callback(w->c, dsp_run, w);
  w->sr = (float)jack_get_sample_rate(w->c);
  jack_port_make_standard(w->c, w->ip, w->nc, false);
  jack_port_make_standard(w->c, w->op, w->nc, true);
}

void usage(void)
{
  eprintf("Usage: jack-dl [ options ]\n");
  eprintf("    -b N : Number of buffers (default=8).\n");
  eprintf("    -c N : Number of channels (default=8).\n");
  eprintf("    -g N : Number of graphs (default=8).\n");
  eprintf("    -k N : Number of controls (default=64).\n");
  exit(EXIT_SUCCESS);
}

int main(int argc, char **argv)
{
  struct world w;
  lo_server_thread osc;
  int c;
  int nb = 8, nc = 8, ng = 8, nk = 64;
  while((c = getopt(argc, argv, "b:c:hg:k:")) != -1) {
    switch(c) {
    case 'b': nb = (int)strtol(optarg, NULL, 0); break;
    case 'c': nc = (int)strtol(optarg, NULL, 0); break;
    case 'g': ng = (int)strtol(optarg, NULL, 0); break;
    case 'k': nk = (int)strtol(optarg, NULL, 0); break;
    case 'h': usage(); break;
    }
  }
  world_init(&w, ng, nc, nk, nb);
  osc = lo_server_thread_new("57190", osc_error);
  lo_server_thread_add_method(osc, "/c_set", "if", osc_c_set, &w);
#if USE_P_CTL
  lo_server_thread_add_method(osc, "/p_set1", "iif", osc_p_set1, &w);
#endif
  lo_server_thread_add_method(osc, "/g_load", "is", osc_g_load, &w);
  lo_server_thread_add_method(osc, "/g_unload", "i", osc_g_unload, &w);
  lo_server_thread_add_method(osc, "/b_alloc", "iii", osc_b_alloc, &w);
  lo_server_thread_add_method(osc, "/quit", NULL, osc_quit, &w);
  lo_server_thread_start(osc);
  if(jack_activate(w.c)) fail("jack-dl: jack_activate() failed\n");
  char *dst_pattern = getenv("JACK_DL_CONNECT_TO");
  if (dst_pattern) {
    char src_pattern[128];
    snprintf(src_pattern, 128, "%s:out_%%d", w.cn);
    jack_port_connect_pattern(w.c, w.nc, 0, src_pattern, dst_pattern);
  }
  while(!w.ef) {
    struct timespec t = {0, 100000000};
    nanosleep(&t, NULL);
  }
  jack_client_close(w.c);
  lo_server_thread_free(osc);
  return EXIT_SUCCESS;
}
