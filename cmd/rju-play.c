#include <stdbool.h> /* C99 */
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <pthread.h> /* POSIX */
#include <unistd.h>

#include <jack/jack.h> /* libjack */
#include <jack/thread.h>

#include <samplerate.h> /* libsamplerate */

#include "r-common/c/failure.h"
#include "r-common/c/file.h"
#include "r-common/c/int.h"
#include "r-common/c/jack-client.h"
#include "r-common/c/jack-port.h"
#include "r-common/c/jack-ringbuffer.h"
#include "r-common/c/jack-transport.h"
#include "r-common/c/memory.h"
#include "r-common/c/observe-signal.h"
#include "r-common/c/print.h"
#include "r-common/c/sf-sndfile.h"

#define NAME_MAX 64

struct player_opt
{
  int buffer_frames;
  int minimal_frames;
  i64 seek_request;
  double seek_request_sec;
  bool transport_aware;
  bool unique_name;
  double src_ratio;
  int rb_request_frames;
  int converter;
  char client_name[NAME_MAX];
  char *dst_pattern;
  float ampl;
  bool loop;
};

struct player
{
  int buffer_bytes;
  int buffer_samples;
  float *d_buffer;
  float *j_buffer;
  float *k_buffer;
  SNDFILE *sound_file;
  int channels;
  jack_port_t **output_port;
  float **out;
  jack_ringbuffer_t *rb;
  pthread_t disk_thread;
  int pipe[2];
  jack_client_t *client;
  SRC_STATE *src;
  struct player_opt o;
};

/* Read the sound file from disk and write to the ring buffer until
   the end of file, at which point either loop or return. */
void *disk_proc(void *PTR)
{
  struct player *d = (struct player *)PTR;
  while(!observe_end_of_process()) {

    /* Handle seek request. */
    if(d->o.seek_request >= 0) {
      sf_count_t err = sf_seek(d->sound_file,
			       (sf_count_t)d->o.seek_request, SEEK_SET);
      if(err == -1) {
	eprintf("rju-play: seek request failed, %ld\n",
		(long)d->o.seek_request);
      }
      d->o.seek_request = -1;
    }

    /* Wait for write space at the ring buffer. */
    int nbytes = d->o.minimal_frames * sizeof(float) * d->channels;
    nbytes = jack_ringbuffer_wait_for_write(d->rb, nbytes, d->pipe[0]);

    /* Do not overflow the local buffer. */
    if(nbytes > d->buffer_bytes) {
      eprintf("rju-play: impossible condition, write space.\n");
      nbytes = d->buffer_bytes;
    }

    /* Read sound file data, which *must* be frame aligned. */
    int nframes =(nbytes / sizeof(float))/ d->channels;
    int nsamples = nframes * d->channels;
    sf_count_t err = xsf_read_float(d->sound_file,
				    d->d_buffer,
				    (sf_count_t)nsamples);
    if(err == 0) {
      if(d->o.transport_aware) {
	memset(d->d_buffer, 0, nsamples * sizeof(float));
	err = nsamples;
      } else if(d->o.loop) {
        sf_seek(d->sound_file, 0, SEEK_SET);
      } else {
	return NULL;
      }
    }

    /* Apply amplitude gain. */
    for(sf_count_t i = 0;i < err; i++) {
      d->d_buffer[i] *= d->o.ampl;
    }

    /* Write data to ring buffer. */
    jack_ringbuffer_write(d->rb,
			  (char *)d->d_buffer,
			  (size_t)err * sizeof(float));
  }

  return NULL;
}

int sync_handler(jack_transport_state_t state,
		 jack_position_t *position,
		 void *PTR)
{
  struct player *d = PTR;
  d->o.seek_request = (i64)position->frame;
  return 1;
}

void signal_set(float **s, int n, int c, float z)
{
  int j;
  for(j = 0; j < c; j++) {
    int i;
    for(i = 0; i < n; i++) {
      s[j][i] = z;
    }
  }
}

/* Write data from the ring buffer to the JACK output ports.  If the
   disk thread is late, ie. the ring buffer is empty print a warning
   and zero the output ports.  */
int signal_proc(jack_nframes_t nframes, void *PTR)
{
  struct player *d = (struct player *)PTR;
  int nsamples = nframes * d->channels;
  int nbytes = nsamples * sizeof(float);

  /* Ensure the period size is workable. */
  die_when(nbytes >= d->buffer_bytes,"rju-play: period size exceeds limit\n");

  /* Get port data buffers. */
  int i,j;
  for(i = 0; i < d->channels; i++) {
    d->out[i] = (float *)jack_port_get_buffer(d->output_port[i], nframes);
  }

  /* Write silence if the transport is stopped.  If stopped the disk
     thread will sleep and signals will be ignored, so check here
     also. */
  if(d->o.transport_aware && !jack_transport_is_rolling(d->client)) {
    if(observe_end_of_process ()) {
      FAILURE; /* ? */
      return 1;
    } else {
      signal_set(d->out, nframes, d->channels, 0.0);
      return 0;
    }
  }

  /* Get data from sample rate converter, this returns the number of
     frames acquired. */
  long err = src_callback_read (d->src,
				d->o.src_ratio,
				(long)nframes,
				d->j_buffer);
  die_when(err==0,
           "rju-play: sample rate converter failed: %s\n",
           src_strerror(src_error(d->src)));

  /* Uninterleave available data to the output buffers. */
  for(i = 0; i < err; i++) {
    for(j = 0; j < d->channels; j++) {
      d->out[j][i] = d->j_buffer[(i*d->channels)+j];
    }
  }

  /* If any sample data is unavailable inform the user and zero the
     output buffers.  The print statement is not correct, a this
     should set a flag and have another thread take appropriate
     action. */
  if(err < nframes) {
    eprintf("rju-play: disk thread late (%ld < %d)\n", err, nframes);
    for(i = err; i < nframes; i++) {
      for(j = 0; j < d->channels; j++) {
	d->out[j][i] = 0.0;
      }
    }
  }

  /* Indicate to the disk thread that the ring buffer has been read
     from.  This is done by writing a single byte to a communication
     pipe.  Once the disk thread gets so far ahead that the ring
     buffer is full it reads this communication channel to wait for
     space to become available.  So long as PIPE_BUF is not a
     pathologically small value this write operation is atomic and
     will not block.  The number of bytes that can accumulate in the
     pipe is a factor of the relative sizes of the ring buffer and the
     process callback, but should in no case be very large. */
  char b = 1;
  xwrite(d->pipe[1], &b, 1);
  return 0;
}

void usage(void)
{
  eprintf("Usage: rju-play [ options ] sound-file...\n");
  eprintf("    -b N : Ring buffer size in frames (default=4096).\n");
  eprintf("    -c N : ID of conversion algorithm (default=2, SRC_SINC_FASTEST).\n");
  eprintf("    -d N : Destination port pattern (default=NULL).\n");
  eprintf("    -g N : amplitude gain (multiplier, default=1).\n");
  eprintf("    -i N : Initial disk seek in frames (default=0).\n");
  eprintf("    -I N : Initial disk seek in seconds (default=0).\n");
  eprintf("    -l   : Loop input file indefinitely.\n");
  eprintf("    -m N : Minimal disk read size in frames (default=32).\n");
  eprintf("    -q N : Frames to request from ring buffer (default=64).\n");
  eprintf("    -r N : Resampling ratio multiplier (default=1.0).\n");
  eprintf("    -t   : Jack transport awareness.\n");
  eprintf("    -u   : Do not generate unique jack client name (ie. do not append PID)\n");
  FAILURE;
}

/* Get data from ring buffer.  Return number of frames read.  This
   could check the read size first, but then would still need to check
   the actual result size, and therefore have two error cases.  Since
   there is no alternative but to drop sample data in any case it does
   not matter much. */
long read_input_from_rb(void *PTR, float **buf)
{
  struct player *d = PTR;
  int nsamples = d->channels * d->o.rb_request_frames;
  int nbytes = (size_t)nsamples * sizeof(float);

  int err = jack_ringbuffer_read(d->rb,
				 (char *)d->k_buffer,
				 nbytes);
  err /= d->channels * sizeof(float);
  *buf = d->k_buffer;

  /* SRC locks up if we return zero here, return a silent frame */
  if(err==0) {
    eprintf("rju-play: ringbuffer empty... zeroing data\n");
    memset(d->k_buffer, 0, (size_t)nsamples * sizeof(float));
    err = d->o.rb_request_frames;
  }

  return (long)err;
}

int jackplay(const char *file_name,
             struct player_opt o)
{
  struct player d;
  d.o = o;
  observe_signals ();

  /* Open sound file. */
  SF_INFO sfinfo;
  d.sound_file = xsf_open(file_name, SFM_READ, &sfinfo);
  d.channels = sfinfo.channels;

  /* Allocate channel based data. */
  die_when(d.channels < 1,"rju-play: channels < 1: %d\n",d.channels);
  d.out = xmalloc(d.channels * sizeof(float *));
  d.output_port = xmalloc(d.channels * sizeof(jack_port_t *));

  /* Allocate buffers. */
  d.buffer_samples = d.o.buffer_frames * d.channels;
  d.buffer_bytes = d.buffer_samples * sizeof(float);
  d.d_buffer = xmalloc(d.buffer_bytes);
  d.j_buffer = xmalloc(d.buffer_bytes);
  d.k_buffer = xmalloc(d.buffer_bytes);
  d.rb = jack_ringbuffer_create(d.buffer_bytes);

  /* Setup sample rate conversion. */
  int err;
  d.src = src_callback_new (read_input_from_rb,
			    d.o.converter,
			    d.channels,
			    &err,
			    &d);
  die_when(!d.src,"rju-play: SRC setup failed: %s\n",src_strerror(err));

  /* Create communication pipe. */
  xpipe(d.pipe);

  /* Become a client of the JACK server.  */
  if(d.o.unique_name) {
    d.client = jack_client_unique_store(d.o.client_name);
  } else {
    d.client = jack_client_open(d.o.client_name,JackNullOption,NULL);
  }
  die_when(!d.client,"rju-play: create client: %s", d.o.client_name);

  /* Start disk thread, the priority number is a random guess.... */
  jack_client_create_thread (d.client,
			     &(d.disk_thread),
			     50,
			     true,
			     disk_proc,
			     &d);

  /* Set error, process and shutdown handlers. */
  jack_set_error_function(jack_client_minimal_error_handler);
  jack_on_shutdown(d.client, jack_client_minimal_shutdown_handler, 0);
  if(d.o.transport_aware) {
    jack_set_sync_callback(d.client, sync_handler, &d);
  }
  jack_set_process_callback(d.client, signal_proc, &d);

  /* Inform the user of sample-rate mismatch and set SRC ratio. */
  double osr = (double) jack_get_sample_rate(d.client);
  double isr = (double) sfinfo.samplerate;
  if(osr != isr) {
    d.o.src_ratio *= (osr / isr);
    eprintf("rju-play: resampling, sample rate of file != server, %G != %G (%G)\n",
	    isr,
	    osr,
            d.o.src_ratio);
  }

  /* Translate initial seek request if given in seconds and cancel. */
  if (d.o.seek_request_sec > 0) {
    d.o.seek_request = (i64) (isr * d.o.seek_request_sec);
    d.o.seek_request_sec = -1;
  }

  /* Create output ports, connect if env variable set and activate
     client. */
  jack_port_make_standard(d.client, d.output_port, d.channels, true, false);
  jack_client_activate(d.client);
  if (o.dst_pattern == NULL) {
    o.dst_pattern = getenv("RJU_PLAY_CONNECT_TO");
  }
  if (o.dst_pattern) {
    char src_pattern[128];
    snprintf(src_pattern,128,"%s:out_%%d",d.o.client_name);
    jack_port_connect_pattern(d.client,d.channels,0,src_pattern,o.dst_pattern);
  }

  /* Wait for disk thread to end, which it does when it reaches the
     end of the file or is interrupted. */
  pthread_join(d.disk_thread, NULL);

  /* Close sound file, free ring buffer, close JACK connection, close
     pipe, free data buffers, indicate success. */
  jack_client_close(d.client);
  sf_close(d.sound_file);
  jack_ringbuffer_free(d.rb);
  close(d.pipe[0]);
  close(d.pipe[1]);
  free(d.d_buffer);
  free(d.j_buffer);
  free(d.k_buffer);
  free(d.out);
  free(d.output_port);
  src_delete(d.src);
  return 0;
}

int main(int argc, char *argv[])
{
  struct player_opt o;
  int c;

  o.buffer_frames = 4096;
  o.minimal_frames = 32;
  o.seek_request = -1;
  o.seek_request_sec = -1;
  o.transport_aware = false;
  o.unique_name = true;
  o.src_ratio = 1.0;
  o.rb_request_frames = 64;
  o.converter = SRC_SINC_FASTEST;
  o.ampl = 1.0;
  o.dst_pattern = NULL;
  o.loop = false;
  strncpy(o.client_name, "rju-play", NAME_MAX - 1);

  while((c = getopt(argc, argv, "b:c:d:g:hi:I:lm:n:q:r:tu")) != -1) {
    switch(c) {
    case 'b':
      o.buffer_frames = (int)strtol(optarg, NULL, 0);
      break;
    case 'c':
      o.converter = (int)strtol(optarg, NULL, 0);
      break;
    case 'd':
      o.dst_pattern = malloc(jack_port_name_size());
      strncpy(o.dst_pattern, optarg, jack_port_name_size() - 1);
      eprintf("jack destination port pattern: %s\n", o.dst_pattern);
      break;
    case 'g':
      o.ampl = strtof(optarg, NULL);
      break;
    case 'h':
      usage ();
      break;
    case 'i':
      o.seek_request = (i64)strtol(optarg, NULL, 0);
      break;
    case 'I':
      o.seek_request_sec = strtod(optarg,NULL);
      break;
    case 'l':
      o.loop = true;
      break;
    case 'm':
      o.minimal_frames = (int)strtol(optarg, NULL, 0);
      break;
    case 'n':
      strncpy(o.client_name, optarg, NAME_MAX - 1);
      eprintf("jack client name: %s\n", o.client_name);
      break;
    case 'q':
      o.rb_request_frames = (int)strtol(optarg, NULL, 0);
      break;
    case 'r':
      o.src_ratio = strtod(optarg, NULL);
      break;
    case 't':
      o.transport_aware = true;
      break;
    case 'u':
      o.unique_name = false;
      break;
    default:
      eprintf("rju-play: illegal option, %c\n", c);
      usage ();
      break;
    }
  }
  if(optind > argc - 1) {
    usage ();
  }
  int i;
  for(i = optind; i < argc; i++) {
    printf("rju-play: %s\n", argv[i]);
    jackplay(argv[i], o);
  }
  return EXIT_SUCCESS;
}
