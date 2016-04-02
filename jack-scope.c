#include <errno.h> /* C99 */
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <pthread.h> /* POSIX */
#include <semaphore.h>
#include <sys/types.h>
#include <unistd.h>

#include "c-common/byte-order.h"
#include "c-common/failure.h"
#include "c-common/file.h"
#include "c-common/img.h"
#include "c-common/img-ppm.h"
#include "c-common/jack-client.h"
#include "c-common/jack-port.h"
#include "c-common/memory.h"
#include "c-common/network.h"
#include "c-common/observe-signal.h"
#include "c-common/osc.h"
#include "c-common/print.h"
#include "c-common/signal-copy.h"
#include "c-common/signal-clip.h"
#include "c-common/signal-interleave.h"
#include "c-common/signal-interpolate.h"
#include "c-common/ximg.h"

typedef void (*draw_fn_t) (u8 *, i32, const f32 *, i32, i32, i32, void *);

typedef bool(*control_fn_t) (const u8 *, i32, void *);

#define SIGNAL_MODE 0
#define EMBED_MODE 1
#define MODE_COUNT 2

#define MAX_CHANNELS 4

struct scope {
  i32 channels;
  i32 window_size;
  i32 data_frames;
  i32 data_samples;
  i32 draw_frames;
  i32 data_location;
  float *data;
  float *share;
  jack_port_t *port[MAX_CHANNELS];
  pthread_t draw_thread;
  pthread_t osc_thread;
  float fps;
  float delay_msec;
  i32 delay_frames;
  i32 delay_location;
  i32 mode;
  float input_gain;
  int pipe[2];
  char *image_directory;
  i32 image_cnt;
  int fd;
  draw_fn_t child_draw[MODE_COUNT];
  control_fn_t child_control[MODE_COUNT];
  void *child_data[MODE_COUNT];
  bool zero_crossing;
};

void jackscope_print(struct scope *d) {
  eprintf("channels          : %d\n", d->channels);
  eprintf("window_size       : %d\n", d->window_size);
  eprintf("data_frames       : %d\n", d->data_frames);
  eprintf("data_samples      : %d\n", d->data_samples);
  eprintf("draw_frames       : %d\n", d->draw_frames);
  eprintf("data_location     : %d\n", d->data_location);
  eprintf("frames_per_second : %f\n", d->fps);
  eprintf("delay_msec        : %f\n", d->delay_msec);
  eprintf("delay_frames      : %d\n", d->delay_frames);
  eprintf("delay_location    : %d\n", d->delay_location);
  eprintf("mode              : %d\n", d->mode);
  eprintf("input_gain        : %f\n", d->input_gain);
  eprintf("zero_crossing     : %s\n", d->zero_crossing ? "true" : "false");
}

#define OSC_PARSE_MSG(command,types)				\
  osc_parse_message(command, types, packet, packet_sz, o)

struct embed {
  i32 embed;
  f32 incr;
};

static void embed_draw_grid(u8 * image, i32 size) {
  u8 half[3] = { 128, 128, 128 };
  u8 feint[3] = { 192, 192, 192 };
  for (i32 i = 0; i < size; i += 3) {
    img_set_pixel(image, size, i, size / 2, half);
    img_set_pixel(image, size, i, size / 6, feint);
    img_set_pixel(image, size, i, size - (size / 6), feint);
  }
  for (i32 i = 0; i < size; i += 3) {
    img_set_pixel(image, size, size / 2, i, half);
    img_set_pixel(image, size, size / 6, i, feint);
    img_set_pixel(image, size, size - (size / 6), i, feint);
  }
}

static void
embed_draw_data(u8 * image, i32 size,
                const f32 * signal, i32 n, const u8 * color, i32 embed, f32 incr) {
  f32 xindex = 0.0;
  f32 yindex = (f32) embed;
  if (incr <= 0.0) {
    incr = 1.0;
  }
  while (yindex < n) {
    f32 xi = signal_interpolate_safe(signal, n, xindex);
    i32 x = signal_x_to_screen_x(xi, size);
    f32 yi = signal_interpolate_safe(signal, n, yindex);
    i32 y = signal_y_to_screen_y(yi, size);
    xindex += incr;
    yindex += incr;
    img_set_pixel(image, size, x, y, color);
  }
}

void *embed_init(void) {
  struct embed *e = malloc(sizeof(struct embed));
  e->embed = 6;
  e->incr = 1.0;
  return e;
}

void embed_draw(u8 * image, i32 size, const f32 * signal, i32 f, i32 d, i32 c, void *PTR) {
  struct embed *e = (struct embed *) PTR;
  embed_draw_grid(image, size);
  for (i32 i = 0; i < c; i++) {
    u8 color[12] = { 128, 32, 32, 32, 32, 128, 128, 224, 224, 224, 224, 128 };
    embed_draw_data(image, size, signal + (i * f), d, color + (i * 3), e->embed, e->incr);
  }
}

bool embed_control(const u8 * packet, i32 packet_sz, void *PTR) {
  struct embed *e = (struct embed *) PTR;
  osc_data_t o[1];
  if (OSC_PARSE_MSG("/embed", ",i")) {
    e->embed = o[0].i;
    return true;
  } else if (OSC_PARSE_MSG("/incr", ",f")) {
    e->incr = o[0].f;
    return true;
  }
  return false;
}

#define DOT_STYLE 0
#define FILL_STYLE 1
#define LINE_STYLE 2

struct signal {
  i32 style;
};

static void signal_draw_grid(u8 * image, i32 size) {
  for (i32 i = 0; i < size; i += 3) {
    u8 half[3] = { 128, 128, 128 };
    u8 feint[3] = { 192, 192, 192 };
    img_set_pixel(image, size, i, size / 2, half);
    img_set_pixel(image, size, i, size / 6, feint);
    img_set_pixel(image, size, i, size - (size / 6), feint);
  }
}

static void signal_draw_data(u8 * image, i32 size, const f32 * signal, i32 n, const u8 * color,
                             i32 style) {
  f32 incr = (f32) n / (f32) size;
  f32 index = 0.0;
  for (i32 i = 0; i < size; i++) {
    f32 s = signal_interpolate_safe(signal, n, index);
    i32 y = signal_y_to_screen_y(s, size);
    index += incr;
    img_set_pixel(image, size, i, y, color);
    if (style == DOT_STYLE) {
      img_set_pixel(image, size, i, y, color);
    } else if (style == FILL_STYLE) {
      i32 m = size / 2;
      i32 l = y > m ? m : y;
      i32 r = y > m ? y : m;
      for (i32 j = l; j < r; j++) {
        img_set_pixel(image, size, i, j, color);
      }
    } else if (style == LINE_STYLE) {
      f32 ss = signal_interpolate_safe(signal, n, index);
      i32 yy = signal_y_to_screen_y(ss, size);
      i32 l = yy > y ? y : yy;
      i32 r = yy > y ? yy : y;
      for (i32 j = l; j < r; j++) {
        img_set_pixel(image, size, i, j, color);
      }
    }
  }
}

static void signal_set_style(struct signal *s, const char *style) {
  if (strcmp("dot", style) == 0) {
    s->style = DOT_STYLE;
  } else if (strcmp("fill", style) == 0) {
    s->style = FILL_STYLE;
  } else if (strcmp("line", style) == 0) {
    s->style = LINE_STYLE;
  } else {
    eprintf("signal_set_style: illegal style, %s\n", style);
  }
}

void *signal_init(void) {
  struct signal *s = malloc(sizeof(struct signal));
  s->style = DOT_STYLE;
  return s;
}

void signal_draw(u8 * image, i32 size, const f32 * signal, i32 f, i32 d, i32 c, void *PTR) {
  struct signal *s = (struct signal *) PTR;
  signal_draw_grid(image, size);
  for (i32 i = 0; i < c; i++) {
    u8 color[12] = { 128, 32, 32,
      32, 32, 128,
      128, 224, 224,
      224, 224, 128
    };
    signal_draw_data(image, size, signal + (i * f), d, color + (i * 3), s->style);
  }
}

bool signal_control(const u8 * packet, i32 packet_sz, void *PTR) {
  struct signal *s = (struct signal *) PTR;
  osc_data_t o[1];
  if (OSC_PARSE_MSG("/style", ",s")) {
    signal_set_style(s, o[0].s);
    return true;
  }
  return false;
}

void set_mode(struct scope *d, const char *mode) {
  if (strncmp("signal", mode, 6) == 0) {
    d->mode = SIGNAL_MODE;
  } else if (strncmp("embed", mode, 5) == 0) {
    d->mode = EMBED_MODE;
  } else {
    eprintf("jack-scope: illegal mode, %s\n", mode);
  }
}

void *jackscope_osc_thread_procedure(void *PTR) {
  struct scope *d = (struct scope *) PTR;
  while (1) {
    const int packet_extent = 32;
    uint8_t packet[packet_extent];
    int packet_sz = xrecv(d->fd, packet, 32, 0);
    osc_data_t o[1];
    if (!(d->child_control[0] (packet, packet_sz, d->child_data[0]) ||
          d->child_control[1] (packet, packet_sz, d->child_data[1]))) {
      if (OSC_PARSE_MSG("/frames", ",i")) {
        d->draw_frames = o[0].i > d->data_frames ? d->data_frames : o[0].i;
      } else if (OSC_PARSE_MSG("/mode", ",s")) {
        set_mode(d, o[0].s);
      } else if (OSC_PARSE_MSG("/input-gain", ",f")) {
        d->input_gain = o[0].f;
      } else if (OSC_PARSE_MSG("/delay", ",f")) {
        d->delay_msec = o[0].f;
        d->delay_frames = floorf((d->delay_msec / 1000.0) * d->fps);
      } else {
        eprintf("jack-scope: dropped packet: %8s\n", packet);
      }
    }
  }
  return NULL;
}

/* The data is channel separated into a local buffer, 'local'.  The
   image data 'image' is cleared and a user selected drawing procedure
   is invoked.  The draw procedure must accept any combination of
   window size and frame count, and any number of channels.  */

void *jackscope_draw_thread_procedure(void *PTR) {
  struct scope *d = (struct scope *) PTR;
  Ximg_t *x = ximg_open(d->window_size, d->window_size, "jack-scope");
  i32 image_n = d->window_size * d->window_size * 3;
  u8 *image = xmalloc(image_n);
  float *local = xmalloc(d->data_samples * sizeof(float));
  while (!observe_end_of_process()) {
    char b;
    xread(d->pipe[0], &b, 1);
    signal_uninterleave(local, d->share, d->data_frames, d->channels);
    signal_clip(local, d->data_frames * d->channels, -1.0, 1.0);
    memset(image, 255, image_n);
    d->child_draw[d->mode] (image, d->window_size,
                            local, d->data_frames,
                            d->draw_frames, d->channels, d->child_data[d->mode]);
    ximg_blit(x, image);
    if (d->image_directory) {
      char name[256];
      snprintf(name, 256, "%s/jack-scope.%06d.ppm", d->image_directory, d->image_cnt);
      img_write_ppm_file(image, d->window_size, d->window_size, name);
    }
    d->image_cnt++;
  }
  ximg_close(x);
  free(image);
  free(local);
  return NULL;
}

/* Accumulate the input signal at `d->data'.  When an image is due to
   be displayed copy the accumulator into `d->share' and poke the
   drawing routine. */

int jackscope_process(jack_nframes_t nframes, void *PTR) {
  struct scope *d = (struct scope *) PTR;
  float *in[MAX_CHANNELS];
  for (i32 i = 0; i < d->channels; i++) {
    in[i] = (float *) jack_port_get_buffer(d->port[i], nframes);
  }
  i32 k = d->data_location;
  i32 l = d->delay_location;
  for (i32 i = 0; i < nframes; i++) {
    for (i32 j = 0; j < d->channels; j++) {
      d->data[k++] = (float) in[j][i] * d->input_gain;
      if (k >= d->data_samples) {
        k = 0;
      }
    }
    l++;
    if (l >= d->delay_frames && (!d->zero_crossing ||
                                 (i > 0 && in[0][i] > 0.0 && in[0][i - 1] <= 0.0) ||
                                 l >= d->delay_frames * 2)) {
      signal_copy_circular(d->share, d->data, d->data_samples, k);
      l = 0;
      char b = 1;
      xwrite(d->pipe[1], &b, 1);
    }
  }
  d->data_location = k;
  d->delay_location = l;
  return 0;
}

void jackscope_usage(void) {
  eprintf("Usage: jack-scope [options] sound-file\n");
  eprintf(" -b INT  : Scope size in frames (default=512)\n");
  eprintf(" -d REAL : Delay time in ms between scope updates (default=100)\n");
  eprintf(" -f STR  : Request images be stored at location (default=NULL)\n");
  eprintf(" -m STR  : Scope operating mode (default=signal)\n");
  eprintf(" -n INT  : Number of channels (default=1)\n");
  eprintf(" -p STR  : Jack port pattern to connect to (default=nil)\n");
  eprintf(" -s STR  : Drawing style for signal mode (default=dot)\n");
  eprintf(" -u INT  : UDP port number for OSC packets (default=57140)\n");
  eprintf(" -w INT  : Scope size in pixels (default=512)\n");
  eprintf(" -z      : Do not align to zero-crossing\n");
  FAILURE;
}

int main(int argc, char **argv) {
  observe_signals();
  struct scope d;
  d.window_size = 512;
  d.data_frames = 512;
  d.data_samples = 0;
  d.data_location = 0;
  d.draw_frames = 0;
  d.delay_msec = 100.0;
  d.delay_location = 0;
  d.channels = 1;
  d.mode = SIGNAL_MODE;
  d.input_gain = 1.0;
  d.child_data[0] = signal_init();
  d.child_draw[0] = signal_draw;
  d.child_control[0] = signal_control;
  d.child_data[1] = embed_init();
  d.child_draw[1] = embed_draw;
  d.child_control[1] = embed_control;
  d.image_directory = NULL;
  d.image_cnt = 0;
  d.zero_crossing = true;
  int port_n = 57140;
  int o;
  char *p = NULL;
  while ((o = getopt(argc, argv, "b:d:e:f:hi:m:n:p:s:u:w:z")) != -1) {
    switch (o) {
    case 'b':
      d.data_frames = strtol(optarg, NULL, 0);
      break;
    case 'd':
      d.delay_msec = strtod(optarg, NULL);
      break;
    case 'f':
      d.image_directory = optarg;
      break;
    case 'h':
      jackscope_usage();
      break;
    case 'm':
      set_mode(&d, optarg);
      break;
    case 'n':
      d.channels = strtol(optarg, NULL, 0);
      if (d.channels > MAX_CHANNELS) {
        eprintf("illegal channel count: %d", d.channels);
        FAILURE;
      }
      break;
    case 'p':
      p = malloc(128);
      strncpy(p, optarg, 128);
      break;
    case 's':
      signal_set_style(d.child_data[0], optarg);
      break;
    case 'u':
      port_n = strtol(optarg, NULL, 0);
      break;
    case 'w':
      d.window_size = strtol(optarg, NULL, 0);
      break;
    case 'z':
      d.zero_crossing = false;
      break;
    default:
      eprintf("jack-scope: illegal option, %c\n", (char) o);
      jackscope_usage();
      break;
    }
  }
  d.draw_frames = d.data_frames;
  d.data_samples = d.data_frames * d.channels;
  d.data = xmalloc(d.data_samples * sizeof(float));
  d.share = xmalloc(d.data_samples * sizeof(float));
  d.fd = socket_udp(0);
  bind_inet(d.fd, NULL, port_n);
  xpipe(d.pipe);
  pthread_create(&(d.osc_thread), NULL, jackscope_osc_thread_procedure, &d);
  pthread_create(&(d.draw_thread), NULL, jackscope_draw_thread_procedure, &d);
  char nm[64] = "jack-scope";
  jack_client_t *c = jack_client_unique_store(nm);
  jack_set_error_function(jack_client_minimal_error_handler);
  jack_on_shutdown(c, jack_client_minimal_shutdown_handler, 0);
  jack_set_process_callback(c, jackscope_process, &d);
  d.fps = (float) jack_get_sample_rate(c);
  d.delay_frames = floorf((d.delay_msec / 1000.0) * d.fps);
  jack_port_make_standard(c, d.port, d.channels, false, false);
  if (jack_client_activate(c)) {
    eprintf("jack-scope: jack_activate() failed\n");
    FAILURE;
  }
  if (!p) {
    p = getenv("JACK_SCOPE_CONNECT_TO");
  }
  if (p) {
    char q[128];
    snprintf(q, 128, "%s:in_%%d", nm);
    jack_port_connect_pattern(c, d.channels, 0, p, q);
  }
  pthread_join(d.draw_thread, NULL);
  jack_client_close(c);
  close(d.pipe[0]);
  close(d.pipe[1]);
  free(d.data);
  free(d.share);
  return EXIT_SUCCESS;
}
