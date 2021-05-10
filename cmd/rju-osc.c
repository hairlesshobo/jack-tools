#include <math.h> /* C99 */
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <pthread.h> /* POSIX */
#include <sys/time.h>
#include <unistd.h>

#include "r-common/c/byte-order.h"
#include "r-common/c/client.h"
#include "r-common/c/failure.h"
#include "r-common/c/jack-client.h"
#include "r-common/c/jack-port.h"
#include "r-common/c/memory.h"
#include "r-common/c/network.h"
#include "r-common/c/observe-signal.h"
#include "r-common/c/osc.h"
#include "r-common/c/print.h"
#include "r-common/c/time-current.h"
#include "r-common/c/time-ntp.h"
#include "r-common/c/time-timeval.h"

#define REQUEST_TICK        0x00000001
#define REQUEST_PULSE       0x00000002
#define REQUEST_CORRECTION  0x00000004
#define REQUEST_TRANSPORT   0x00000008
#define REQUEST_TIME        0x00000010
#define REQUEST_ALL         0xFFFFFFFF

struct jackosc
{
  f64 fps;			/* Frames per second (ie. sample rate) */
  f64 spf;			/* Seconds per frame */
  f64 ppc;			/* Pulses per cycle */
  f64 pt;			/* Pulse type */
  f64 ppm;			/* Pulses per minute */
  f64 ppf;			/* Pulses per frame */
  f64 tpf;			/* Ticks per frame */
  f64 pulse;			/* Pulse clock */
  i64 frm;			/* Frame clock (rju-osc) */
  i64 j_frm;			/* Frame clock (jackd) */
  u64 ntp;			/* NTP clock */
  f64 utc;			/* UTC clock */
  f64 tm;                       /* Transport time */
  f64 tm_q;                     /* time-quanta */
  i8 roll;                      /*  */
  i32 correct_interval;         /*  */
  i32 correct_n;                /*  */
  bool fps_alt;                 /* true if sample-rate is altered in this block */
  jack_client_t *client;        /*  */
  int fd;                       /*  */
  client_register_t *cr;        /*  */
  pthread_t osc_thread;         /*  */
};

#define COMMON_SETUP(n)				\
  u8 packet[256];				\
  osc_data_t o[n];				\
  o[0].t = ntp;					\
  o[1].d = utc;					\
  o[2].h = frm;

#define COMMON_SEND(addr,dsc,incl)					\
  int packet_sz;							\
  packet_sz = osc_construct_message(addr, dsc, o, packet, 256);		\
  sendto_client_register(d->fd, d->cr, packet, packet_sz, incl);

void send_jck_time(struct jackosc *d)
{
  u8 packet[256];
  int packet_sz;							\
  osc_data_t o[1];
  o[0].d = d->tm;
  packet_sz = osc_construct_message("/time", ",d", o, packet, 256);		\
  sendto_client_register(d->fd, d->cr, packet, packet_sz, REQUEST_TIME);
  dprintf("send_jck_time: %f\n",d->tm);
}

void send_jck_drift(struct jackosc *d,
		    u64 ntp, f64 utc, i64 frm,
		    i64 ntp_dif, f64 utc_dif)
{
  COMMON_SETUP(5);
  o[3].h = ntp_dif;
  o[4].d = utc_dif;
  COMMON_SEND("/drift", ",tdhhd", REQUEST_CORRECTION);
  dprintf("send_jck_drift\n");
}

void send_jck_tick(struct jackosc *d,
		   u64 ntp, f64 utc, i64 frm,
		   i64 frame, f64 pulse)
{
  COMMON_SETUP(5);
  o[3].h = frame;
  o[4].d = pulse;
  COMMON_SEND("/tick", ",tdhhd", REQUEST_TICK);
  dprintf("send_jck_tick\n");
}

void send_jck_current(int fd, struct sockaddr_in address,
		      u64 ntp, f64 utc, i64 frm,
		      i64 frame, f64 pulse)
{
  COMMON_SETUP(5);
  o[3].h = frame;
  o[4].d = pulse;
  int packet_sz;
  packet_sz = osc_construct_message("/current.reply", ",tdhhd",
				    o, packet, 256);
  if(packet_sz) {
    sendto_exactly(fd, packet, packet_sz, address);
  }
}

void send_jck_pulse(struct jackosc *d,
		    u64 ntp, f64 utc, i64 frm,
		    u64 p_ntp, f64 p_utc, i64 p_frm, i32 pulse)
{
  COMMON_SETUP(7);
  o[3].t = p_ntp;
  o[4].d = p_utc;
  o[5].h = p_frm;
  o[6].i = pulse;
  COMMON_SEND("/pulse", ",tdhtdhi", REQUEST_PULSE);
  dprintf("send_jck_pulse\n");
}

void send_jck_transport(struct jackosc *d,
			u64 ntp, f64 utc, i64 frm,
			f64 fps, f64 ppm, f64 ppc, f64 pt,
			i32 rolling)
{
  COMMON_SETUP(8);
  o[3].d = fps;
  o[4].d = ppm;
  o[5].d = ppc;
  o[6].d = pt;
  o[7].i = rolling;
  COMMON_SEND("/transport", ",tdhddddi", REQUEST_TRANSPORT);
  dprintf("send_jck_transport\n");
}

void send_jck_status(int fd, struct sockaddr_in address,
		     f64 fps, f64 ppm, f64 ppc, f64 pt,
		     i32 rolling)
{
  u8 packet[256];
  osc_data_t o[5];
  int packet_sz;
  o[0].d = fps;
  o[1].d = ppm;
  o[2].d = ppc;
  o[3].d = pt;
  o[4].i = rolling;
  packet_sz = osc_construct_message("/status.reply", ",ddddi",
				    o, packet, 256);
  if(packet_sz) {
    sendto_exactly(fd, packet, packet_sz, address);
  }
  dprintf("send_jck_status\n");
}

#define OSC_PARSE_MSG(command,types)				\
  osc_parse_message(command, types, packet, packet_sz, o)

void *jackosc_osc_thread_procedure(void *PTR)
{
  struct jackosc *d = (struct jackosc *) PTR;
  while(!observe_end_of_process()) {
    if(fd_wait(d->fd, 500000)) {
      struct sockaddr_in addr;
      socklen_t addr_len = sizeof(addr);
      const int packet_extent = 64;
      u8 packet[packet_extent];
      i32 packet_sz = xrecvfrom(d->fd, packet, 64, 0,
				(struct sockaddr *)&addr, &addr_len);
      osc_data_t o[4];
      if(OSC_PARSE_MSG("/receive", ",i")) {
	edit_client_register(d->cr, addr, o[0].i);
      } else if(OSC_PARSE_MSG("/receive_at", ",iis")) {
	struct sockaddr_in ra_addr;
	init_sockaddr_in(&ra_addr, o[2].s, (i16)o[1].i);
	edit_client_register(d->cr, ra_addr, o[0].i);
      } else if(OSC_PARSE_MSG("/status", ",")) {
	send_jck_status(d->fd, addr,
			d->fps, d->ppm, d->ppc, d->pt, d->roll);
      } else if(OSC_PARSE_MSG("/current", ",")) {
	send_jck_current(d->fd, addr,
			 d->ntp, d->utc, d->frm, d->j_frm, d->pulse);
      } else if(OSC_PARSE_MSG("/start", ",")) {
	jack_transport_start(d->client);
      } else if(OSC_PARSE_MSG("/stop", ",")) {
	jack_transport_stop(d->client);
      } else if(OSC_PARSE_MSG("/locate", ",f")) {
	jack_transport_locate(d->client, o[0].f * d->fps);
      } else if(OSC_PARSE_MSG("/connect", ",ss")) {
	jack_port_connect_named(d->client, o[0].s, o[1].s);
      } else if(OSC_PARSE_MSG("/disconnect", ",ss")) {
	jack_port_disconnect_named(d->client, o[0].s, o[1].s);
      } else {
	eprintf("%s: dropped packet: %8s\n", __func__, packet);
      }
    }
  }
  return NULL;
}

f64 get_ticks_per_frame(f64 ticks_per_pulse,
			f64 pulses_per_minute,
			f64 frames_per_second)
{
  f64 pulses_per_second = pulses_per_minute / 60.0;
  f64 ticks_per_second = ticks_per_pulse * pulses_per_second;
  return ticks_per_second / frames_per_second;
}

f64 get_pulses_per_frame(f64 pulses_per_minute,
			 f64 frames_per_second)
{
  f64 pulses_per_second = pulses_per_minute / 60.0;
  return pulses_per_second / frames_per_second;
}

int jackosc_process(jack_nframes_t nframes, void *PTR)
{
  struct jackosc *d = (struct jackosc *)PTR;
  if(d->correct_n >= d->correct_interval) {
    struct timeval t = current_time_as_utc_timeval();
    u64 c_ntp = utc_timeval_to_ntp(t);
    f64 c_utc = timeval_to_real(t);
    i64 c_ntp_dif = c_ntp - d->ntp;
    f64 c_utc_dif = c_utc - d->utc;
    send_jck_drift(d, d->ntp, d->utc, d->frm, c_ntp_dif, c_utc_dif);
    d->ntp = c_ntp;
    d->utc = c_utc;
    d->correct_n = 0;
  }
  jack_position_t p;
  jack_transport_state_t s = jack_transport_query(d->client, &p);
  f64 tm = (f64)p.frame / (f64)p.frame_rate;
  if(fabs(tm - d->tm) >= d->tm_q) {
    d->tm = tm;
    send_jck_time(d);
  }
  if((s & JackTransportRolling) != d->roll) {
    d->roll = s & JackTransportRolling;
    send_jck_transport(d, d->ntp, d->utc, d->frm,
		       d->fps, d->ppm, d->ppc, d->pt, d->roll);
  }
  if(d->fps_alt) {
    d->ppf = get_pulses_per_frame(d->ppm, d->fps);
    d->tpf = get_ticks_per_frame(p.ticks_per_beat, d->ppm, d->fps);
    send_jck_transport(d, d->ntp, d->utc, d->frm,
		       d->fps, d->ppm, d->ppc, d->pt, d->roll);
    d->fps_alt = false;
  }
  if((p.valid & JackPositionBBT)) {
    if(d->ppm != p.beats_per_minute) {
      d->ppm = p.beats_per_minute;
      d->ppf = get_pulses_per_frame(d->ppm, d->fps);
      d->tpf = get_ticks_per_frame(p.ticks_per_beat, d->ppm, d->fps);
      send_jck_transport(d, d->ntp, d->utc, d->frm,
			 d->fps, d->ppm, d->ppc, d->pt, d->roll);
    }
    if(d->ppc != p.beats_per_bar || d->pt != p.beat_type) {
      d->ppc = p.beats_per_bar;
      d->pt = p.beat_type;
      send_jck_transport(d, d->ntp, d->utc, d->frm,
			 d->fps, d->ppm, d->ppc, d->pt, d->roll);
    }
    if(d->roll &&
       (p.tick == 0 ||
	((f64)p.tick + (floorf((f64)nframes * d->tpf))) > (f64)p.ticks_per_beat)) {
      i32 p_tick_off =(p.tick == 0)? 0 : p.ticks_per_beat - p.tick;
      f64 p_frm_off = (f64)p_tick_off * d->tpf;
      f64 p_frm = (f64)d->frm + p_frm_off;
      f64 p_utc = d->utc +(p_frm_off * d->spf);
      u64 p_ntp = utc_real_to_ntp(p_utc);
      i32 p_pulse =(p.tick == 0)? p.beat : p.beat + 1;
      if((f64)p_pulse > d->ppc) {
	p_pulse = 1;
      }
      send_jck_pulse(d, d->ntp, d->utc, d->frm, p_ntp, p_utc, (i64)floor(p_frm), p_pulse);
      /* Correct pulse accumulator. */
      d->pulse = (f64)p_pulse -(ceil(p_frm_off)* d->ppf);
      if(d->pulse < 1.0) {
	d->pulse += d->ppc;
      }
    }
    /* jck_tk is sent after jck_pl to report the corrected pulse. */
    send_jck_tick(d, d->ntp, d->utc, d->frm, (i64)p.frame, d->pulse);
  }
  d->frm += nframes;
  d->j_frm = (i64)p.frame;
  d->utc += (f64)nframes * d->spf;
  d->ntp = utc_real_to_ntp(d->utc);
  d->pulse += (f64)nframes * d->ppf;
  if(d->pulse > d->ppc + 1.0) {
    d->pulse -= d->ppc;
  }
  d->correct_n += 1;
  return 0;
}

int jackosc_fps_handler(jack_nframes_t fps, void *PTR)
{
  struct jackosc *d =(struct jackosc *) PTR;
  d->fps = (f64) fps;
  d->fps_alt = true;
  return 0;
}

void jackosc_usage(void)
{
  eprintf("Usage: rju-osc [ options ]\n");
  eprintf("   -c  Drift correction interval in periods (default=64)\n");
  eprintf("   -p  Port number (default=57130)\n");
  eprintf("   -q  Time quanta (default=0.1)\n");
  FAILURE;
}

int main(int argc, char *argv[])
{
  observe_signals();
  struct jackosc d;
  d.ppc = 0.0;
  d.pt = 0.0;
  d.ppm = 0.0;
  d.ppf = 0.0;
  d.tpf = 0.0;
  d.pulse = 0.0;
  d.frm = 0;
  d.ntp = 0;
  d.utc = 0.0;
  d.tm = 0.0;
  d.tm_q = 0.1;
  d.roll = 0;
  /* Ensure that a correction occurs when the process starts. */
  d.correct_interval = 64;
  d.correct_n = d.correct_interval;
  d.cr = alloc_client_register(16);
  int port_n = 57130;
  int c;
  while(( c = getopt(argc, argv, "c:hp:q:")) != -1) {
    switch(c) {
    case 'c':
      d.correct_interval = atoi(optarg);
      d.correct_n = d.correct_interval;
      break;
    case 'h':
      jackosc_usage();
      break;
    case 'p':
      port_n = atoi(optarg);
      break;
    case 'q':
      d.tm_q = atof(optarg);
      break;
    default:
      eprintf("%s: Illegal option %c.\n", __func__, c);
      jackosc_usage();
      break;
    }
  }
  d.fd = socket_udp(0);
  bind_inet(d.fd, NULL, port_n);
  d.client = jack_client_unique("rju-osc");
  jack_set_error_function(jack_client_minimal_error_handler);
  jack_set_sample_rate_callback(d.client, jackosc_fps_handler, &d);
  jack_on_shutdown(d.client, jack_client_minimal_shutdown_handler, 0);
  jack_set_process_callback(d.client, jackosc_process, &d);
  d.fps = jack_get_sample_rate(d.client);
  d.fps_alt = false;
  d.spf = 1.0 / d.fps;
  jack_client_activate(d.client);
  pthread_create(&(d.osc_thread), NULL, jackosc_osc_thread_procedure, &d);
  pthread_join(d.osc_thread, NULL);
  jack_client_close(d.client);
  free_client_register(d.cr);
  exit(0);
  return 0;
}
