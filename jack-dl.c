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
  if(w->ga) {
    for(int i = 0; i < w->nc; i++) {
      w->in[i] = (float *)jack_port_get_buffer(w->ip[i], nf);
      w->out[i] = (float *)jack_port_get_buffer(w->op[i], nf);
      memset(w->out[i],0,nf * sizeof(float));
    }
    w->dsp_step(w, nf);
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
  w->ga = false;
  char *s = &a[0]->s;
  if(w->gh) break_on(dlclose(w->gh), dlerror());
  w->gh = dlopen(s, RTLD_LAZY);
  break_on(!w->gh, dlerror());
  w->dsp_memreq = dlsym(w->gh, "dsp_memreq");
  w->dsp_init = dlsym(w->gh, "dsp_init");
  w->dsp_step = dlsym(w->gh, "dsp_step");
  size_t k = w->dsp_memreq();
  w->st = malloc(k);
  w->dsp_init(w->st);
  w->ga = true;
  fprintf(stderr,"g_load: %s\n", s);
  return 0;
}

int osc_g_unload(const char *p, const char *t, lo_arg **a, int n, void *d, void *u)
{
  struct world *w = (struct world *)u;
  w->ga = false;
  if(w->gh) break_on(dlclose(w->gh), dlerror());
  w->gh = NULL;
  if(w->st) free(w->st);
  fprintf(stderr,"g_unload\n");
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
  if(w->bd[i]) {
    w->bd[i] = realloc(w->bd[i],l * sizeof(float));
  } else {
    w->bd[i] = calloc(l, sizeof(float));
  }
  w->bl[i] = l;
  fprintf(stderr,"b_alloc: %d, %d, %d\n", i, l, c);
  return 0;
}

int osc_quit(const char *p, const char *t, lo_arg **a, int n, void *d, void *u)
{
  struct world *w = (struct world *)u;
  w->ef = true;
  return 0;
}

void world_init(struct world *w, int nc, int nk, int nb)
{
  w->nc = nc;
  w->nk = nk;
  w->nb = nb;
  w->dsp_memreq = NULL;
  w->dsp_init = NULL;
  w->dsp_step = NULL;
  w->st = NULL;
  w->ga = false;
  w->gh = NULL;
  w->ip = malloc(w->nc * sizeof(jack_port_t *));
  w->op = malloc(w->nc * sizeof(jack_port_t *));
  w->in = malloc(w->nc * sizeof(float *));
  w->out = malloc(w->nc * sizeof(float *));
  w->ctl = calloc(w->nk, sizeof(float));
  w->bl = calloc(w->nb, sizeof(int));
  w->bd = calloc(w->nb, sizeof(float*));
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
  eprintf("    -k N : Number of controls (default=64).\n");
  exit(EXIT_SUCCESS);
}

int main(int argc, char **argv)
{
  struct world w;
  lo_server_thread osc;
  int c;
  int nb = 8, nc = 8, nk = 64;
  while((c = getopt(argc, argv, "b:c:hk:")) != -1) {
    switch(c) {
    case 'b': nb = (int)strtol(optarg, NULL, 0); break;
    case 'c': nc = (int)strtol(optarg, NULL, 0); break;
    case 'h': usage(); break;
    case 'k': nk = (int)strtol(optarg, NULL, 0); break;
    }
  }
  world_init(&w, nc, nk, nb);
  osc = lo_server_thread_new("57190", osc_error);
  lo_server_thread_add_method(osc, "/c_set", "if", osc_c_set, &w);
  lo_server_thread_add_method(osc, "/g_load", "s", osc_g_load, &w);
  lo_server_thread_add_method(osc, "/g_unload", "", osc_g_unload, &w);
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
