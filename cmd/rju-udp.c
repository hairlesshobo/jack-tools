#include <stdio.h> /* C99 */
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <stdint.h>

#include <unistd.h> /* POSIX */
#include <pthread.h>

#include "r-common/c/byte-order.h"
#include "r-common/c/failure.h"
#include "r-common/c/file.h"
#include "r-common/c/jack-client.h"
#include "r-common/c/jack-port.h"
#include "r-common/c/memory.h"
#include "r-common/c/network.h"
#include "r-common/c/print.h"
#include "r-common/c/ringbuffer.h"
#include "r-common/c/ringbuffer-fd.h"

#define MAX_CHANNELS      32
#define PAYLOAD_SAMPLES   256
#define PAYLOAD_BYTES     (PAYLOAD_SAMPLES*sizeof(f32))

typedef struct
{
  int buffer_size;
  f32 *j_buffer;
  int fd;
  struct sockaddr_in address;
  int channels;
  jack_port_t *j_port[MAX_CHANNELS];
  ringbuffer_t *rb;
  pthread_t c_thread;
  int pipe[2];
  char *name;
} jackudp_t;

static void jackudp_init(jackudp_t *d)
{
  d->buffer_size = 4096;
  d->channels = 2;
  d->name = NULL;
}

typedef struct
{
  u32 index;
  u16 channels;
  u16 frames;
  u8 data[PAYLOAD_BYTES];
} packet_t;

void packet_ntoh(packet_t *p)
{
  p->index = ntoh_i32(p->index);
  p->channels = ntoh_i16(p->channels);
  p->frames = ntoh_i16(p->frames);
  u8 *d = p->data;
  u32 i = p->channels * p->frames;
  while(i--) {
    ntoh32_buf(d, d);
    d += 4;
  }
}

void packet_hton(packet_t *p)
{
  u8 *d = p->data;
  int i = p->channels * p->frames;
  while(i--){
    hton32_buf(d, d);
    d += 4;
  }
  p->index = hton_i32(p->index);
  p->channels = hton_i16(p->channels);
  p->frames = hton_i16(p->frames);
}

void packet_sendto(int fd, packet_t *p, struct sockaddr_in address)
{
  packet_hton(p);
  sendto_exactly(fd, (unsigned char *)p, sizeof(packet_t), address);
}

void packet_recv(int fd, packet_t *p, int flags)
{
  recv_exactly(fd, (char *)p, sizeof(packet_t), flags);
  packet_ntoh(p);
}

/* Read data from udp port and write to ring buffer. */

void *jackudp_recv_thread(void *PTR)
{
  jackudp_t *d = (jackudp_t *) PTR;
  packet_t p;
  int next_packet = -1;
  while(1) {
    packet_recv(d->fd, &p, 0);
    if(p.index != next_packet && next_packet != -1) {
      eprintf("rju-udp recv: out of order packet arrival (%d, %d)\n",
	      next_packet, (int)p.index);
      FAILURE;
    }
    if(p.channels != d->channels) {
      eprintf("rju-udp recv: channel mismatch packet arrival (%d != %d)\n",
	      p.channels, d->channels);
      FAILURE;
    }
    int bytes_available = (int)ringbuffer_write_space(d->rb);
    if(PAYLOAD_BYTES > bytes_available) {
      eprintf("rju-udp recv: buffer overflow (%d > %d)\n",
	      (int) PAYLOAD_BYTES, bytes_available);
    } else {
      ringbuffer_write_exactly(d->rb,
				    (char *) p.data,
				    (size_t) PAYLOAD_BYTES);
    }
    next_packet = p.index + 1;
    next_packet %= INT32_MAX;
  }
  return NULL;
}

/* Write data from ring buffer to JACK output ports. */

int jackudp_recv (jack_nframes_t nframes, void *PTR)
{
  jackudp_t *d = (jackudp_t *) PTR;
  if(nframes >= d->buffer_size) {
    eprintf("rju-udp recv: JACK buffer size exceeds limit\n");
    return -1;
  }

  int i, j;
  float *out[MAX_CHANNELS];
  for(i = 0; i < d->channels; i++) {
    out[i] = (float *) jack_port_get_buffer(d->j_port[i], nframes);
  }

  int nsamples = nframes * d->channels;
  int nbytes = nsamples * sizeof(f32);
  int bytes_available = (int)ringbuffer_read_space(d->rb);
  if(nbytes > bytes_available) {
    eprintf("rju-udp recv: buffer underflow (%d > %d)\n",
	    nbytes, bytes_available);
    for(i = 0; i < nframes; i++) {
      for(j = 0; j < d->channels; j++) {
	out[j][i] = (float) 0.0;
      }
    }
  } else {
    ringbuffer_read_exactly(d->rb, (char *)d->j_buffer, nbytes);
    for(i = 0; i < nframes; i++) {
      for(j = 0; j < d->channels; j++) {
	out[j][i] = (float) d->j_buffer[(i * d->channels) + j];
      }
    }
  }
  return 0;
}

/* Read data from ring buffer and write to udp port. Packets are
   always sent with a full payload. */

void *jackudp_send_thread(void *PTR)
{
  jackudp_t *d = (jackudp_t *) PTR;
  packet_t p;
  p.index = 0;
  while(1) {
    ringbuffer_wait_for_read(d->rb, PAYLOAD_BYTES, d->pipe[0]);
    p.index += 1;
    p.index %= INT32_MAX;
    p.channels = d->channels;
    p.frames = PAYLOAD_SAMPLES / d->channels;
    ringbuffer_read_exactly(d->rb, (char *)&(p.data), PAYLOAD_BYTES);
    packet_sendto(d->fd,  &p, d->address);
  }
  return NULL;
}

/* Write data from the JACK input ports to the ring buffer. */

int jackudp_send(jack_nframes_t n, void *PTR )
{
  jackudp_t *d = (jackudp_t *) PTR;
  float *in[MAX_CHANNELS];
  int i, j;
  for(i = 0; i < d->channels; i++) {
    in[i] = (float *) jack_port_get_buffer(d->j_port[i], n);
  }
  for(i = 0; i < n; i++) {
    for(j = 0; j < d->channels; j++) {
      d->j_buffer[(i*d->channels)+j] = (f32) in[j][i];
    }
  }

  int bytes_available = (int) ringbuffer_write_space(d->rb);
  int bytes_to_write = n * sizeof(f32) * d->channels;
  if(bytes_to_write > bytes_available) {
    eprintf ("rju-udp send: buffer overflow error (UDP thread late)\n");
  } else {
    ringbuffer_write_exactly(d->rb,
				    (char *) d->j_buffer, bytes_to_write );
  }

  char b = 1;
  if(write(d->pipe[1], &b, 1)== -1) {
    eprintf ("rju-udp send: error writing communication pipe.\n");
    FAILURE;
  }
  return 0;
}

void jackudp_usage (void)
{
  eprintf("Usage: rju-udp [ options ] mode\n");
  eprintf("   -b  Set the ring buffer size in frames (default=4096).\n");
  eprintf("   -c  Set the client name (default=\"rju-udp-PID\").\n");
  eprintf("   -p  Set the port number (default=57160).\n");
  eprintf("   -n  Set the number of channels (default=2).\n");
  eprintf("   -r  Set the remote addrress to send to (default=\"127.0.0.1\").\n");
  FAILURE;
}

int main (int argc, char **argv)
{
  jackudp_t d;
  int c;
  int port_n = 57160;
  char *hostname =  NULL;
  jackudp_init(&d);
  while((c = getopt(argc, argv, "b:hn:p:r:c:")) != -1) {
    switch(c) {
    case 'b':
      d.buffer_size = atoi(optarg);
      break;
    case 'c':
      d.name = optarg;
      break;
    case 'h':
      jackudp_usage ();
      break;
    case 'n':
      d.channels = atoi (optarg);
      break;
    case 'p':
      port_n = atoi (optarg);
      break;
    case 'r':
      hostname = optarg;
      break;
    default:
      eprintf ("rju-udp: Illegal option %c.\n", c);
      jackudp_usage ();
      break;
    }
  }
  if (optind != argc - 1) {
    jackudp_usage ();
  }
  if(d.channels < 1 || d.channels > MAX_CHANNELS) {
    eprintf("rju-udp: illegal number of channels: %d\n", d.channels);
    FAILURE;
  }
  int recv_mode = (strcmp(argv[optind], "recv") == 0);
  d.fd = socket_udp(0);
  if(recv_mode) {
    bind_inet(d.fd, NULL, port_n);
  } else {
    init_sockaddr_in(&(d.address),
		     hostname ? hostname : "127.0.0.1",
		     port_n);
  }
  d.buffer_size *= d.channels * sizeof(f32);
  d.j_buffer = xmalloc(d.buffer_size);
  d.rb = ringbuffer_create(d.buffer_size);
  xpipe(d.pipe);
  jack_client_t *client = NULL;
  if(d.name) {
    client = jack_client_open(d.name,JackNullOption,NULL);
  } else {
    client = jack_client_unique("rju-udp");
  }
  jack_set_error_function(jack_client_minimal_error_handler);
  jack_on_shutdown(client, jack_client_minimal_shutdown_handler, 0);
  jack_set_process_callback(client,
			    recv_mode ? jackudp_recv : jackudp_send,
			    &d);
  jack_port_make_standard(client, d.j_port, d.channels, recv_mode, false);
  jack_client_activate(client);
  pthread_create(&(d.c_thread),
		 NULL,
		 recv_mode ? jackudp_recv_thread : jackudp_send_thread,
		 &d);
  pthread_join(d.c_thread, NULL);
  close(d.fd);
  ringbuffer_free(d.rb);
  jack_client_close(client);
  close(d.pipe[0]);
  close(d.pipe[1]);
  free(d.j_buffer);
  return EXIT_SUCCESS;
}
