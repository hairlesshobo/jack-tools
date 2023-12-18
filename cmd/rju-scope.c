#include <errno.h> /* C99 */
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include <pthread.h> /* Posix */
#include <semaphore.h>
#include <string.h> /* strdup */
#include <sys/types.h>
#include <unistd.h>

#include "r-common/c/colour.h"
#include "r-common/c/failure.h"
#include "r-common/c/file.h"
#include "r-common/c/img-ppm.h"
#include "r-common/c/img.h"
#include "r-common/c/int.h"
#include "r-common/c/memory.h"
#include "r-common/c/network.h"
#include "r-common/c/observe-signal.h"
#include "r-common/c/osc.h"
#include "r-common/c/signal-clip.h"
#include "r-common/c/signal-copy.h"
#include "r-common/c/signal-interleave.h"
#include "r-common/c/signal-interpolate.h"

#include "r-common/c/img-png.c"
#include "r-common/c/jack-client.c"
#include "r-common/c/jack-port.c"
#include "r-common/c/resample-src.c"
#include "r-common/c/x11.c"
#include "r-common/c/ximg.c"

#define SIGNAL_MODE 0
#define EMBED_MODE 1
#define HLINE_MODE 2
#define HSCAN_MODE 3
#define MODE_COUNT 4

#define DOT_STYLE 0
#define FILL_STYLE 1
#define LINE_STYLE 2

#define IMG_GREY 0
#define IMG_EGA_64 1

#define MAX_CHANNELS 8
#define MAX_WINDOW_SIZE 4096

struct scope {
	i32 channels; /* INIT, <= MAX_CHANNELS */
	i32 img_w; /* image width, INIT, <= MAX_WINDOW_SIZE */
	i32 img_h; /* image height, INIT, <= MAX_WINDOW_SIZE */
	i32 data_frames; /* number of frames per block, INIT */
	i32 data_samples; /* data_frames * channels */
	i32 draw_frames; /* VARIABLE, <= data_frames */
	i32 data_location; /* write index to data, interleaved sample count */
	float sample_rate; /* audio sample-rate (ie. not video frame rate) */
	float delay_msec; /* video frame delay (ms) */
	i32 delay_frames; /* delay_msec * sample_rate */
	i32 delay_location; /* delay_frames counter */
	float input_gain; /* input signal multiplier */
	bool zero_crossing; /* sync display to zero crossing (oddly rhs) */
	i32 mode; /* drawing mode */
	i32 signal_style; /* drawing style (signal mode) */
	i32 embed_n; /* frame delay (embed mode) */
	f32 embed_incr; /* interpolation increment (embed mode) */
	char *img_bg_fn; /* image ground file name | NULL */
	u8 *img_bg; /* the image that is masked, plain white by default */
	u8 img_colour_mode; /* GREY | EGA_64 */
	char *image_directory; /* OUTPUT image directory | NULL */
	i32 image_cnt; /* OUTPUT image frame counter (for file name) */
	float *data; /* data_samples store */
	float *share_il; /* data, draw thread copy */
	float *share_ul; /* de-interleaved share */
	float *hline_src_dst; /* resampled audio data (if required) */
	float *hscan_src_dst; /* resampled audio data (if required) */
	jack_port_t *port[MAX_CHANNELS];
	pthread_t draw_thread;
	pthread_t osc_thread;
	int pipe[2]; /* file-descriptors for IPC between audio and drawing threads */
	int fd;
};

void scope_init(struct scope *s)
{
	s->channels = 1;
	s->img_w = 512;
	s->img_h = 512;
	s->data_frames = 512;
	s->data_samples = 512;
	s->draw_frames = 0;
	s->data_location = 0;
	s->sample_rate = -1.0;
	s->delay_msec = 100.0;
	s->delay_frames = -1.0;
	s->delay_location = 0;
	s->input_gain = 1.0;
	s->zero_crossing = true;
	s->mode = SIGNAL_MODE;
	s->signal_style = DOT_STYLE;
	s->embed_n = 6;
	s->embed_incr = 1.0;
	s->img_bg_fn = NULL;
	s->img_bg = NULL;
	s->img_colour_mode = IMG_GREY;
	s->image_directory = NULL;
	s->image_cnt = 0;
	s->data = NULL;
	s->share_il = NULL;
	s->share_ul = NULL;
	s->hline_src_dst = NULL;
	s->hscan_src_dst = NULL;
}

void scope_print(struct scope *d)
{
	printf("channels            : %d\n", d->channels);
	printf("img_w               : %d\n", d->img_w);
	printf("img_h               : %d\n", d->img_h);
	printf("data_frames         : %d\n", d->data_frames);
	printf("data_samples        : %d\n", d->data_samples);
	printf("draw_frames         : %d\n", d->draw_frames);
	printf("data_location       : %d\n", d->data_location);
	printf("sample_rate         : %f\n", d->sample_rate);
	printf("delay_msec          : %f\n", d->delay_msec);
	printf("delay_frames        : %d\n", d->delay_frames);
	printf("delay_location      : %d\n", d->delay_location);
	printf("input_gain          : %f\n", d->input_gain);
	printf("zero_crossing       : %s\n", d->zero_crossing ? "true" : "false");
	printf("mode                : %d\n", d->mode);
}

#define OSC_PARSE_MSG(command, types) \
	osc_parse_message(command, types, packet, packet_sz, o)

void embed_draw_grid(u8 *img, i32 w, i32 h)
{
	u8 half[3] = { 128, 128, 128 };
	u8 feint[3] = { 192, 192, 192 };
	for (i32 i = 0; i < w; i += 3) {
		img_set_pixel(img, w, 3, i, h / 2, half);
		img_set_pixel(img, w, 3, i, h / 6, feint);
		img_set_pixel(img, w, 3, i, h - (h / 6), feint);
	}
	for (i32 i = 0; i < h; i += 3) {
		img_set_pixel(img, w, 3, w / 2, i, half);
		img_set_pixel(img, w, 3, w / 6, i, feint);
		img_set_pixel(img, w, 3, w - (w / 6), i, feint);
	}
}

void embed_draw_data(u8 *img, const f32 *data, const u8 *color, struct scope *s)
{
	i32 n = s->draw_frames;
	f32 xindex = 0.0;
	f32 yindex = (f32)s->embed_n;
	if (s->embed_incr <= 0.0) { /* SANITY */
		s->embed_incr = 1.0;
	}
	while (yindex < n) {
		f32 xi = signal_interpolate_circular_safe(data, n, xindex);
		i32 x = signal_x_to_screen_x(xi, s->img_w);
		f32 yi = signal_interpolate_circular_safe(data, n, yindex);
		i32 y = signal_y_to_screen_y(yi, s->img_h);
		xindex += s->embed_incr;
		yindex += s->embed_incr;
		img_set_pixel(img, s->img_w, 3, x, y, color);
	}
}

u8 c4_colors[12] = { 128, 32, 32,
	32, 32, 128,
	128, 224, 224,
	224, 224, 128 };

void embed_draw(u8 *img, struct scope *s)
{
	embed_draw_grid(img, s->img_w, s->img_h);
	for (i32 i = 0; i < s->channels; i++) {
		u8 *color = c4_colors + ((i % 4) * 3);
		embed_draw_data(img, s->share_ul + (i * s->data_frames), color, s);
	}
}

void signal_draw_grid(u8 *img, i32 w, i32 h)
{
	for (i32 i = 0; i < h; i += 3) {
		u8 half[3] = { 128, 128, 128 };
		u8 feint[3] = { 192, 192, 192 };
		img_set_pixel(img, w, 3, i, h / 2, half);
		img_set_pixel(img, w, 3, i, h / 6, feint);
		img_set_pixel(img, w, 3, i, h - (h / 6), feint);
	}
}

void signal_draw_data(u8 *img, const f32 *data, i32 n, const u8 *color, struct scope *s)
{
	i32 w = s->img_w;
	i32 h = s->img_h;
	f32 incr = (f32)n / (f32)w;
	f32 index = 0.0;
	for (i32 i = 0; i < w; i++) {
		f32 x = signal_interpolate_circular_safe(data, n, index);
		i32 y = signal_y_to_screen_y(x, h);
		index += incr;
		img_set_pixel(img, w, 3, i, y, color);
		if (s->signal_style == DOT_STYLE) {
			img_set_pixel(img, w, 3, i, y, color);
		} else if (s->signal_style == FILL_STYLE) {
			i32 m = h / 2;
			i32 l = y > m ? m : y;
			i32 r = y > m ? y : m;
			for (i32 j = l; j < r; j++) {
				img_set_pixel(img, w, 3, i, j, color);
			}
		} else if (s->signal_style == LINE_STYLE) {
			f32 xx = signal_interpolate_circular_safe(data, n, index);
			i32 yy = signal_y_to_screen_y(xx, h);
			i32 l = yy > y ? y : yy;
			i32 r = yy > y ? yy : y;
			for (i32 j = l; j < r; j++) {
				img_set_pixel(img, w, 3, i, j, color);
			}
		}
	}
}

i32 img_colour_mode_parse(const char *s)
{
	i32 e = IMG_GREY;
	if (strcmp("ega64", s) == 0) {
		e = IMG_EGA_64;
	} else if (strcmp("grey", s) == 0) {
		e = IMG_GREY;
	} else {
		fprintf(stderr, "img_colour_mode_parse: %s\n", s);
	}
	return e;
}

i32 signal_style_parse(const char *style)
{
	i32 e = DOT_STYLE;
	if (strcmp("dot", style) == 0) {
		e = DOT_STYLE;
	} else if (strcmp("fill", style) == 0) {
		e = FILL_STYLE;
	} else if (strcmp("line", style) == 0) {
		e = LINE_STYLE;
	} else {
		fprintf(stderr, "signal_style_parse: illegal style, %s\n", style);
	}
	return e;
}

void signal_draw(u8 *img, struct scope *s)
{
	signal_draw_grid(img, s->img_w, s->img_h);
	for (i32 i = 0; i < s->channels; i++) {
		u8 *color = c4_colors + ((i % 4) * 3);
		signal_draw_data(img, s->share_ul + (i * s->data_frames), s->draw_frames, color, s);
	}
}

void hline_init(struct scope *s)
{
	size_t signal_bytes = s->img_h * s->channels * sizeof(float);
	s->hline_src_dst = xmalloc(signal_bytes);
	if (s->img_bg_fn) {
		i32 png_w, png_h;
		load_png_rgb8(s->img_bg_fn, &png_w, &png_h, &(s->img_bg));
		if (png_w == s->img_w && png_h == s->img_h) {
			return;
		} else {
			fprintf(stderr, "img_bg_fn: incorrect size: (%d,%d)/(%d,%d)\n", png_w, png_h, s->img_w, s->img_h);
			free(s->img_bg);
		}
	}
	size_t image_bytes = s->img_w * s->img_h * 3;
	s->img_bg = xmalloc(image_bytes);
	xmemset(s->img_bg, 255, image_bytes);
}

void hline_draw(u8 *img, struct scope *s)
{
	src_resample_block(s->hline_src_dst, (long)s->img_h,
		(float *)s->share_il, (long)s->draw_frames,
		(int)s->channels);
	int c_width = s->img_w / s->channels;
	for (i32 c = 0; c < s->channels; c++) { /* c = channel */
		for (i32 i = 0; i < s->img_h; i++) { /* i = row */
			float z = fabsf(s->hline_src_dst[(i * s->channels) + c]);
			u8 color[3];
			switch (s->img_colour_mode) {
			case IMG_GREY:
				for (i32 j = c * c_width; j < (c + 1) * c_width; j++) { /* j = column */
					img_get_pixel(s->img_bg, s->img_w, 3, j, i, color);
					rgb_mul(color, z);
					img_set_pixel(img, s->img_w, 3, j, i, color);
				}
				break;
			case IMG_EGA_64:
				ega_rgb_lookup_f32(z, color);
				for (i32 j = c * c_width; j < (c + 1) * c_width; j++) { /* j = column */
					img_set_pixel(img, s->img_w, 3, j, i, color);
				}
				break;
			}
		}
	}
}

void hscan_init(struct scope *s)
{
	size_t n = s->img_w * s->img_h * sizeof(float);
	s->hscan_src_dst = xmalloc(n);
}

/* j = column, i = row, z = (0.0,1.0) */
void js_img_mod_plain(u8 *img, struct scope *s, i32 j, i32 i, f32 z)
{
	u8 color[3];
	switch (s->img_colour_mode) {
	case IMG_GREY:
		img_get_pixel(s->img_bg, s->img_w, 3, j, i, color);
		rgb_mul(color, z);
		img_set_pixel(img, s->img_w, 3, j, i, color);
		break;
	case IMG_EGA_64:
		ega_rgb_lookup_f32(z, color);
		img_set_pixel(img, s->img_w, 3, j, i, color);
		break;
	}
}

void hscan_draw(u8 *img, struct scope *s)
{
	i32 n_pixels = s->img_h * s->img_w;
	i32 n_frames = n_pixels / s->channels;
	src_resample_block(s->hscan_src_dst, n_frames, s->share_il, s->draw_frames, s->channels);

#if 1
	/* non-interleaved */
	int c_width = s->img_w / s->channels;
	for (i32 c = 0; c < s->channels; c++) { /* c = channel */
		for (i32 i = 0, n = 0; i < s->img_h; i++) { /* i = row, n = frame */
			for (i32 j = c * c_width; j < (c + 1) * c_width; j++, n++) { /* j = column */
				i32 ix = (n * s->channels) + c;
				float z = fabsf(s->hscan_src_dst[ix]);
				js_img_mod_plain(img, s, j, i, z);
			}
		}
	}
#endif

#if 0
  /* interleaved */
  for (i32 i = 0, j = 0; i < n_pixels; i++, j+=3) {
    f32 z = fabsf(s->hscan_src_dst[i]);
    u8 color[3];
    switch (s->img_colour_mode) {
    case IMG_GREY:
      xmemcpy(color, s->img_bg + j, 3);
      rgb_mul(color, z);
      break;
    case IMG_EGA_64:
      ega_rgb_lookup_f32(z, color);
      break;
    }
    xmemcpy(img + j, color, 3);
  }
#endif
}

void set_mode(struct scope *s, const char *mode)
{
	if (strncmp("signal", mode, 6) == 0) {
		s->mode = SIGNAL_MODE;
	} else if (strncmp("embed", mode, 5) == 0) {
		s->mode = EMBED_MODE;
	} else if (strncmp("hline", mode, 5) == 0) {
		s->mode = HLINE_MODE;
	} else if (strncmp("hscan", mode, 5) == 0) {
		s->mode = HSCAN_MODE;
	} else {
		fprintf(stderr, "rju-scope: illegal mode, %s\n", mode);
	}
}

void *
scope_osc_thread_procedure(void *ptr)
{
	struct scope *s = (struct scope *)ptr;
	while (1) {
		const int packet_extent = 32;
		uint8_t packet[packet_extent];
		int packet_sz = xrecv(s->fd, packet, 32, 0);
		osc_data_t o[1];
		if (OSC_PARSE_MSG("/colour-mode", ",s")) {
			s->img_colour_mode = img_colour_mode_parse(o[0].s);
		} else if (OSC_PARSE_MSG("/delay", ",f")) {
			s->delay_msec = o[0].f;
			s->delay_frames = floorf((s->delay_msec / 1000.0) * s->sample_rate);
		} else if (OSC_PARSE_MSG("/embed", ",i")) {
			s->embed_n = o[0].i;
		} else if (OSC_PARSE_MSG("/frames", ",i")) {
			s->draw_frames = o[0].i > s->data_frames ? s->data_frames : o[0].i;
		} else if (OSC_PARSE_MSG("/incr", ",f")) {
			s->embed_incr = o[0].f;
		} else if (OSC_PARSE_MSG("/input-gain", ",f")) {
			s->input_gain = o[0].f;
		} else if (OSC_PARSE_MSG("/mode", ",s")) {
			set_mode(s, o[0].s);
		} else if (OSC_PARSE_MSG("/print", ",i")) {
			if (o[0].i > 0)
				scope_print(s);
		} else if (OSC_PARSE_MSG("/style", ",s")) {
			s->signal_style = signal_style_parse(o[0].s);
		} else {
			fprintf(stderr, "rju-scope: dropped packet: %8s\n", packet);
		}
	}
	return NULL;
}

/* The data is channel separated into 'share_ul'.  The image data
   'image' is cleared and a user selected drawing procedure is
   invoked.  The draw procedure must accept any combination of window
   size and frame count, and any number of channels.  */

typedef void (*draw_fn_t)(u8 *img, struct scope *s);

draw_fn_t
draw_fn_select(i32 mode)
{
	if (mode == SIGNAL_MODE) {
		return signal_draw;
	} else if (mode == EMBED_MODE) {
		return embed_draw;
	} else if (mode == HLINE_MODE) {
		return hline_draw;
	} else if (mode == HSCAN_MODE) {
		return hscan_draw;
	} else {
		fprintf(stderr, "rju-scope: illegal mode, %d\n", mode);
		return signal_draw;
	}
}

void *
scope_draw_thread_procedure(void *ptr)
{
	struct scope *s = (struct scope *)ptr;
	Ximg_t *x = ximg_open(s->img_w, s->img_h, "rju-scope");
	u8 *img = img_alloc(s->img_w, s->img_h, 3);
	while (!observe_end_of_process()) {
		char b;
		xread(s->pipe[0], &b, 1); /* WAIT */
		signal_clip(s->share_il, s->data_frames * s->channels, -1.0, 1.0);
		signal_uninterleave(s->share_ul, s->share_il, s->data_frames, s->channels);
		img_memset(img, s->img_w, s->img_h, 3, 255);
		draw_fn_t draw_fn = draw_fn_select(s->mode);
		draw_fn(img, s);
		ximg_blit(x, img);
		if (s->image_directory) {
			char name[256];
			snprintf(name, 256, "%s/rju-scope.%06d.ppm", s->image_directory, s->image_cnt);
			img_write_ppm_file(img, s->img_w, s->img_h, name);
		}
		s->image_cnt++;
	}
	ximg_close(x);
	free(img);
	return NULL;
}

/* Accumulate the input signal at `d->data'.  When an image is due to
   be displayed copy the accumulator into `d->share_il' and poke the
   drawing routine. */

int scope_process(jack_nframes_t nframes, void *ptr)
{
	struct scope *d = (struct scope *)ptr;
	float *in[MAX_CHANNELS];
	for (i32 i = 0; i < d->channels; i++) {
		in[i] = (float *)jack_port_get_buffer(d->port[i], nframes);
	}
	for (i32 i = 0; i < nframes; i++) {
		for (i32 j = 0; j < d->channels; j++) {
			d->data[d->data_location++] = (float)in[j][i] * d->input_gain;
			if (d->data_location >= d->data_samples) {
				d->data_location = 0;
			}
		}
		d->delay_location++;
		if (d->delay_location >= d->delay_frames && (!d->zero_crossing || (i > 0 && in[0][i] > 0.0 && in[0][i - 1] <= 0.0) || d->delay_location >= d->delay_frames * 2)) {
			signal_copy_circular(d->share_il, d->data, d->data_samples, d->data_location);
			d->delay_location = 0;
			char b = 1;
			xwrite(d->pipe[1], &b, 1); /* QUEUE DRAW */
		}
	}
	return 0;
}

void scope_usage(void)
{
	printf("Usage: rju-scope [options]\n");
	printf(" -b INT  : Scope size in frames (default=512)\n");
	printf(" -c STR  : Colour mode, grey|ega64 (default=grey)\n");
	printf(" -d REAL : Delay time in ms between scope updates (default=100)\n");
	printf(" -e INT  : Embedding delay in frames (default=6)\n");
	printf(" -f STR  : Request images be stored at location (default=NULL)\n");
	printf(" -g REAL : Set input gain (default=1.0)\n");
	printf(" -h      : Print usage information\n");
	printf(" -h INT  : Scope height in pixels (default=512)\n");
	printf(" -i STR  : PNG file name for hline mask (default=NULL)\n");
	printf(" -k INT  : Port index offset (default=0)\n");
	printf(" -m STR  : Scope operating mode (default=signal)\n");
	printf(" -n INT  : Number of channels (default=1)\n");
	printf(" -p STR  : Jack port pattern to connect to (default=nil)\n");
	printf(" -s STR  : Drawing style for signal mode (default=dot)\n");
	printf(" -u INT  : UDP port number for OSC packets (default=57140)\n");
	printf(" -U      : Do not generate unique jack client name (ie. do not append PID)\n");
	printf(" -w INT  : Scope width in pixels (default=512)\n");
	printf(" -z      : Do not align to zero-crossing\n");
	exit(1);
}

#define opt_limit(nm, p, q) \
	if (q > p) { \
		fprintf(stderr, "illegal option: %s (%d > %d)", nm, q, p); \
		exit(1); \
	}

int main(int argc, char **argv)
{
	observe_signals();
	struct scope d;
	scope_init(&d);
	int port_n = 57140;
	int o;
	char *p = NULL;
	int k = 0;
	bool unique = true;
	while ((o = getopt(argc, argv, "b:c:d:e:f:g:h:i:k:m:n:p:s:u:Uw:z")) != -1) {
		switch (o) {
		case 'b':
			d.data_frames = strtol(optarg, NULL, 0);
			break;
		case 'c':
			d.img_colour_mode = img_colour_mode_parse(optarg);
			break;
		case 'd':
			d.delay_msec = strtod(optarg, NULL);
			break;
		case 'e':
			d.embed_n = strtol(optarg, NULL, 0);
			break;
		case 'f':
			d.image_directory = optarg;
			break;
		case 'g':
			d.input_gain = strtod(optarg, NULL);
			break;
		case 'h':
			d.img_h = (i32)strtol(optarg, NULL, 0);
			opt_limit("img_h", MAX_WINDOW_SIZE, d.img_h);
			break;
		case 'i':
			d.img_bg_fn = strdup(optarg);
			break;
		case 'k':
			k = (int)strtol(optarg, NULL, 0);
			break;
		case 'm':
			set_mode(&d, optarg);
			break;
		case 'n':
			d.channels = (i32)strtol(optarg, NULL, 0);
			opt_limit("channels", MAX_CHANNELS, d.channels);
			break;
		case 'p':
			p = xmalloc(128);
			strncpy(p, optarg, 128 - 1);
			break;
		case 's':
			d.signal_style = signal_style_parse(optarg);
			break;
		case 'u':
			port_n = strtol(optarg, NULL, 0);
			break;
		case 'U':
			unique = false;
			break;
		case 'w':
			d.img_w = (i32)strtol(optarg, NULL, 0);
			opt_limit("img_w", MAX_WINDOW_SIZE, d.img_w);
			break;
		case 'z':
			d.zero_crossing = false;
			break;
		default:
			fprintf(stderr, "rju-scope: illegal option, %c\n", (char)o);
			scope_usage();
			break;
		}
	}
	d.draw_frames = d.data_frames;
	d.data_samples = d.data_frames * d.channels;
	size_t data_bytes = (size_t)d.data_samples * sizeof(float);
	d.data = xmalloc(data_bytes);
	d.share_il = xmalloc(data_bytes);
	d.share_ul = xmalloc(data_bytes);
	hline_init(&d);
	hscan_init(&d);
	d.fd = socket_udp(0);
	bind_inet(d.fd, NULL, port_n);
	xpipe(d.pipe);
	pthread_create(&(d.osc_thread), NULL, scope_osc_thread_procedure, &d);
	pthread_create(&(d.draw_thread), NULL, scope_draw_thread_procedure, &d);
	char nm[64] = "rju-scope";
	jack_client_t *c = NULL;
	if (unique) {
		c = jack_client_unique_store(nm);
	} else {
		c = jack_client_open(nm, JackNullOption, NULL);
	}
	die_when(!c, "rju-scope: could not create jack client: %s", nm);
	jack_set_error_function(jack_client_minimal_error_handler);
	jack_on_shutdown(c, jack_client_minimal_shutdown_handler, 0);
	jack_set_process_callback(c, scope_process, &d);
	d.sample_rate = (float)jack_get_sample_rate(c);
	d.delay_frames = floorf((d.delay_msec / 1000.0) * d.sample_rate);
	jack_port_make_standard(c, d.port, d.channels, false, false);
	die_when(jack_client_activate(c), "rju-scope: jack_activate() failed\n");
	if (!p) {
		p = getenv("RJU_SCOPE_CONNECT_TO");
	}
	if (p) {
		char q[128];
		snprintf(q, 128, "%s:in_%%d", nm);
		jack_port_connect_pattern(c, d.channels, k, p, q);
	}
	pthread_join(d.draw_thread, NULL);
	jack_client_close(c);
	close(d.pipe[0]);
	close(d.pipe[1]);
	free(d.data);
	free(d.share_il);
	free(d.share_ul);
	return EXIT_SUCCESS;
}
