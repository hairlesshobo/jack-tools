#include <math.h> /* C99 */
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <dlfcn.h> /* POSIX */
#include <unistd.h>

#include <jack/jack.h>
#include <jack/thread.h>

#include <lo/lo.h>

#include "c-common/jack-port.h"
#include "c-common/failure.h"
#include "c-common/print.h"

#include "jack-dl.h"

int dsp_run(jack_nframes_t nf, void *ptr)
{
  struct world *w = (struct world *)ptr;
  if(w->ga) {
    for(int32_t i = 0; i < w->nc; i++) {
      w->in[i] = (float *)jack_port_get_buffer(w->ip[i], nf);
      w->out[i] = (float *)jack_port_get_buffer(w->op[i], nf);
      memset(w->out[i],0,nf * sizeof(float));
    }
    w->dsp_step(w, (int32_t)nf);
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
  vprintf(w->vb, "g_load: %s\n", s);
  return 0;
}

int osc_g_unload(const char *p, const char *t, lo_arg **a, int n, void *d, void *u)
{
  struct world *w = (struct world *)u;
  w->ga = false;
  if(w->gh) break_on(dlclose(w->gh), dlerror());
  w->gh = NULL;
  if(w->st) free(w->st);
  vprintf(w->vb, "g_unload\n");
  return 0;
}

int osc_c_set(const char *p, const char *t, lo_arg **a, int n, void *d, void *u)
{
  struct world *w = (struct world *)u;
  int32_t i = a[0]->i;
  break_on(i >= w->nk, "c_set: control index");
  w_c_set1(w, i, a[1]->f);
  vprintf(w->vb, "c_set: %d, %f\n", i, a[1]->f);
  return 0;
}

int osc_c_setn(const char *p, const char *t, lo_arg **a, int n, void *d, void *u)
{
  struct world *w = (struct world *)u;
  if(strlen(t) >= 3 && strncmp(t,"iif",3) == 0) {
      int32_t c_0 = a[0]->i;
      int32_t c_n = a[1]->i;
      break_on(c_0 + c_n >= w->nk, "c_setn: control index");
      for(int32_t i = 0; i < c_n; i++) {
          w_c_set1(w, c_0 + i, a[i + 2]->f);
      }
      vprintf(w->vb, "c_setn: %d, %d, %f...\n", c_0, c_n, a[2]->f);
  } else {
      fprintf(stderr,"c_setn: error: %s, %s, %d\n", p, t, n);
  }
  return 0;
}

int osc_b_alloc(const char *p, const char *t, lo_arg **a, int n, void *d, void *u)
{
  struct world *w = (struct world *)u;
  int32_t i = a[0]->i;
  break_on(i >= w->nb, "buffer index");
  /*if(w->bd[i]) free(w->bd[i]);*/
  int32_t l = a[1]->i;
  int32_t c = a[2]->i;
  break_on(c != 1, "buffer not single channel...");
  w->bl[i] = 0;
  if(w->bd[i]) {
    w->bd[i] = realloc(w->bd[i],l * sizeof(float));
  } else {
    w->bd[i] = calloc(l, sizeof(float));
  }
  w->bl[i] = l;
  vprintf(w->vb, "b_alloc: %d, %d, %d\n", i, l, c);
  return 0;
}

int osc_quit(const char *p, const char *t, lo_arg **a, int n, void *d, void *u)
{
  struct world *w = (struct world *)u;
  w->ef = true;
  return 0;
}

int osc_print(const char *p, const char *t, lo_arg **a, int n, void *d, void *u)
{
    int i;
    printf("p=%s, t=%s, n=%d a=", p, t, n);
    for(i = 0; i < n; i++) {
        lo_arg_pp((lo_type)t[i], a[i]);
        printf(i < n - 1 ? "," : "\n");
    }
    return 1;
}

void world_init(struct world *w, int nc, int nk, int nb, bool vb)
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
  w->vb = vb;
  strncpy(w->cn,"jack-dl",64);
  w->c = jack_client_open(w->cn,JackNullOption,NULL);
  die_when(!w->c,"could not create jack client\n");
  jack_set_process_callback(w->c, dsp_run, w);
  w->sr = (float)jack_get_sample_rate(w->c);
  jack_port_make_standard(w->c, w->ip, w->nc, false, false);
  jack_port_make_standard(w->c, w->op, w->nc, true, false);
}

void usage(void)
{
  eprintf("Usage: jack-dl [ options ]\n");
  eprintf("    -b N : Number of buffers (default=1024).\n");
  eprintf("    -c N : Number of channels (default=8).\n");
  eprintf("    -k N : Number of controls (default=16384).\n");
  eprintf("    -u N : UDP port number (default=57190).\n");
  exit(EXIT_SUCCESS);
}

int main(int argc, char **argv)
{
  struct world w;
  lo_server_thread osc;
  int c;
  bool vb = false;
  int32_t nb = 1024, nc = 8, nk = 16384;
  char udp_port[6] = "57190";
  while((c = getopt(argc, argv, "b:c:hk:u:v")) != -1) {
    switch(c) {
    case 'b': nb = (int32_t)strtol(optarg, NULL, 0); break;
    case 'c': nc = (int32_t)strtol(optarg, NULL, 0); break;
    case 'h': usage(); break;
    case 'k': nk = (int32_t)strtol(optarg, NULL, 0); break;
    case 'u': strncpy(udp_port,optarg,5); break;
    case 'v': vb = true;
    }
  }
  world_init(&w, nc, nk, nb, vb);
  osc = lo_server_thread_new(udp_port, osc_error);
  if(vb) {
      lo_server_thread_add_method(osc, NULL, NULL, osc_print, NULL);
  }
  lo_server_thread_add_method(osc, "/b_alloc", "iii", osc_b_alloc, &w);
  lo_server_thread_add_method(osc, "/c_set", "if", osc_c_set, &w);
  lo_server_thread_add_method(osc, "/c_setn", NULL, osc_c_setn, &w); /* NULL matches variable arg */
  lo_server_thread_add_method(osc, "/g_load", "s", osc_g_load, &w);
  lo_server_thread_add_method(osc, "/g_unload", "", osc_g_unload, &w);
  lo_server_thread_add_method(osc, "/quit", "", osc_quit, &w);
  lo_server_thread_start(osc);
  die_when(jack_activate(w.c),"jack-dl: jack_activate() failed\n");
  jack_port_connect_to_env(w.c, w.nc, 0, "JACK_DL_CONNECT_TO");
  jack_port_connect_from_env(w.c, w.nc, 0, "JACK_DL_CONNECT_FROM");
  while(!w.ef) {
    struct timespec t = {0, 100000000};
    nanosleep(&t, NULL);
  }
  jack_client_close(w.c);
  lo_server_thread_free(osc);
  return EXIT_SUCCESS;
}
