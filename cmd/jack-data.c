#include <math.h>		/* C99 */
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include <pthread.h>		/* POSIX */
#include <unistd.h>

#include "c-common/failure.h"
#include "c-common/file.h"
#include "c-common/int.h"
#include "c-common/jack-client.h"
#include "c-common/jack-port.h"
#include "c-common/memory.h"
#include "c-common/network.h"
#include "c-common/numeric-type.h"
#include "c-common/observe-signal.h"
#include "c-common/osc.h"
#include "c-common/print.h"
#include "c-common/resample-src.h"

#define MAX_NC 8
#define OSC_HDR_SZ 40

struct jackdata
{
  i32 nc;            /* number of channels */
  i32 read_sz;       /* read block size (frames) */
  i32 read_k;        /* number of frames read */
  i32 write_sz;      /* write block size (frames) */
  enum numeric_type write_ty;
  float *data;       /* jack thread (read_sz) */
  float *src;        /* osc thread (read_sz) */
  float *dst;        /* osc thread (write_sz) */
  int udp_port;      /* UDP port number to send to */
  pthread_t osc_thread;
  i32 osc_data_sz;   /* OSC packet data size (bytes) */
  u8 *osc_data;      /* OSC packet data store */
  int pipe[2];
  int fd;
  jack_port_t *port[MAX_NC];
};


i8 f32_to_i8(f32 n) {return n * 127.0;}
u8 f32_to_u8(f32 n) {return fabs(n) * 255.0;}
i16 f32_to_i16(f32 n) {return n * 32767.0;}

void f32_to_u8_seq(void *ptr,i32 n)
{
  f32 *src = ptr;
  u8* dst = ptr;
  for(i32 i = 0;i < n; i++) {
      dst[i] = f32_to_u8(src[i]);
  }
}

void *jackdata_osc_thread_procedure(void *ptr)
{
  struct jackdata *d = (struct jackdata *)ptr;
  osc_data_t o[5];
  osc_blob_t b;
  char ty[8];
  o[0].i = d->nc;
  o[1].i = d->read_sz;
  o[2].i = d->write_sz;
  numeric_type_pp(d->write_ty,ty);
  o[3].s = ty;
  b.data = (const u8 *)d->dst;
  b.size = d->write_sz * d->nc * numeric_type_sz(d->write_ty);
  o[4].b = b;
  struct sockaddr_in addr;
  init_sockaddr_in(&addr, "127.0.0.1", d->udp_port);
  while (!observe_end_of_process()) {
    char b;
    xread(d->pipe[0], &b, 1);
    if(!src_resample_block(d->dst,d->write_sz,d->src,d->read_sz,d->nc)) {
      printf("src_resample_block failed\n");
      FAILURE;
    }
    switch(d->write_ty) {
      case u8_ty: f32_to_u8_seq(d->dst,d->write_sz * d->nc); break;
      case f32_ty: break;
      default: FAILURE;
    }
    i32 pkt_sz = osc_construct_message("/data",",iiisb",o,d->osc_data,d->osc_data_sz);
    if(d->osc_data_sz != pkt_sz) {
      printf("osc_construct_message failed: (%d,%d)\n",d->osc_data_sz,pkt_sz);
      FAILURE;
    }
    sendto_exactly(d->fd,d->osc_data,d->osc_data_sz,addr);
  }
  return NULL;
}

int jackdata_process(jack_nframes_t nframes, void *ptr)
{
  struct jackdata *d = (struct jackdata *)ptr;
  float *in[MAX_NC];
  for (i32 i = 0; i < d->nc; i++) {
    in[i] = (float *) jack_port_get_buffer(d->port[i], nframes);
  }
  for (i32 i = 0; i < nframes; i++) {
    for (i32 j = 0; j < d->nc; j++) {
      d->data[(d->read_k * d->nc) + j] = (float) in[j][i];
    }
    d->read_k++;
    if (d->read_k == d->read_sz) {
        d->read_k = 0;
        xmemcpy(d->src, d->data, d->read_sz * d->nc * sizeof(float));
        char b = 1;
        xwrite(d->pipe[1], &b, 1);
    }
  }
  return 0;
}

void
jackdata_usage(void)
{
  eprintf("Usage: jack-data nc:int read:int write:int type:str udp:int\n");
  FAILURE;
}

int
main(int argc, char **argv)
{
  observe_signals();
  struct jackdata d;
  if(argc != 6) {
    jackdata_usage();
  }
  d.nc = strtod(argv[1], NULL);
  d.read_sz = strtod(argv[2], NULL);
  d.write_sz = strtod(argv[3], NULL);
  d.write_ty = numeric_type_parse(argv[4]);
  d.udp_port = strtod(argv[5], NULL);
  d.read_k = 0;
  size_t read_bytes = (size_t)d.read_sz * d.nc * sizeof(float);
  size_t write_bytes = (size_t)d.write_sz * d.nc * numeric_type_sz(d.write_ty);
  d.data = xmalloc(read_bytes);
  d.src = xmalloc(read_bytes);
  d.dst = xmalloc(write_bytes);
  d.osc_data_sz = OSC_HDR_SZ + write_bytes;
  d.osc_data = xmalloc(d.osc_data_sz);
  d.fd = socket_udp(0);
  xpipe(d.pipe);
  pthread_create(&(d.osc_thread), NULL, jackdata_osc_thread_procedure, &d);
  char nm[64] = "jack-data";
  jack_client_t *c = jack_client_unique_store(nm);
  jack_set_error_function(jack_client_minimal_error_handler);
  jack_on_shutdown(c, jack_client_minimal_shutdown_handler, 0);
  jack_set_process_callback(c, jackdata_process, &d);
  jack_port_make_standard(c, d.port, d.nc, false, false);
  if (jack_client_activate(c)) {
    eprintf("jack-data: jack_activate() failed\n");
    FAILURE;
  }
  char *p = getenv("JACK_DATA_CONNECT_TO");
  if (p) {
    char q[128];
    snprintf(q, 128, "%s:in_%%d", nm);
    jack_port_connect_pattern(c, d.nc, 0, p, q);
  }
  pthread_join(d.osc_thread, NULL);
  jack_client_close(c);
  close(d.pipe[0]);
  close(d.pipe[1]);
  free(d.data);
  /* these are causing segfaults.... */
  free(d.src);
  free(d.dst);
  return EXIT_SUCCESS;
}
