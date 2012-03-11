#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <pthread.h>

#include "c-common/failure.h"
#include "c-common/file.h"
#include "c-common/jack-client.h"
#include "c-common/jack-port.h"
#include "c-common/jack-ringbuffer.h"
#include "c-common/memory.h"
#include "c-common/observe-signal.h"
#include "c-common/print.h"
#include "c-common/signal-interleave.h"
#include "c-common/sound-file.h"

struct recorder
{
  int buffer_bytes;
  int buffer_samples;
  int buffer_frames;
  int minimal_frames;
  float timer_seconds;
  int timer_frames;
  int timer_counter;
  float sample_rate;
  float *d_buffer;
  float *j_buffer;
  float *u_buffer;
  int file_format;
  SNDFILE **sound_file;
  int multiple_sound_files;
  int channels;
  jack_port_t **input_port;
  float **in;
  jack_ringbuffer_t *ring_buffer;
  pthread_t disk_thread;
  int pipe[2];
};

void write_to_disk(struct recorder *d, int nframes)
{
  if(d->multiple_sound_files) {
    float *p = d->u_buffer;
    signal_uninterleave(p, d->d_buffer, nframes, d->channels);
    int i;
    for(i = 0; i < d->channels; i++) {
      xsf_write_float(d->sound_file[i],
		      p,
		      (sf_count_t)nframes);
      p += nframes;
    }
  } else {
    int nsamples = nframes * d->channels;
    xsf_write_float(d->sound_file[0],
		    d->d_buffer,
		    (sf_count_t)nsamples);
  }
}

void *disk_thread_procedure(void *PTR)
{
  struct recorder *d = (struct recorder *) PTR;
  while(!observe_end_of_process()) {

    /* Wait for data at the ring buffer. */
    int nbytes = d->minimal_frames * sizeof(float) * d->channels;
    nbytes = jack_ringbuffer_wait_for_read(d->ring_buffer, nbytes,
					   d->pipe[0]);

    /* Drop excessive data to not overflow the local buffer. */
    if(nbytes > d->buffer_bytes) {
      eprintf("jack-record: impossible condition, read space.\n");
      nbytes = d->buffer_bytes;
    }

    /* Read data from the ring buffer. */
    jack_ringbuffer_read(d->ring_buffer,
			 (char *) d->d_buffer,
			 nbytes);

    /* Do write operation.  The sample count *must* be an integral
       number of frames. */
    int nframes = (nbytes / sizeof(float))/ d->channels;
    write_to_disk(d, nframes);

    /* Handle timer */
    d->timer_counter += nframes;
    if(d->timer_frames > 0 && d->timer_counter >= d->timer_frames) {
      return NULL;
    }
  }
  return NULL;
}

/* Write data from the JACK input ports to the ring buffer.  If the
   disk thread is late, ie. the ring buffer is full, print an error
   and halt the client.  */
int process(jack_nframes_t nframes, void *PTR)
{
  struct recorder *d = (struct recorder *) PTR;
  int nsamples = nframes * d->channels;
  int nbytes = nsamples * sizeof(float);

  /* Get port data buffers. */
  int i;
  for(i = 0; i < d->channels; i++) {
    d->in[i] = (float *) jack_port_get_buffer(d->input_port[i], nframes);
  }

  /* Check period size is workable. If the buffer is large, ie 4096
     frames, this should never be of practical concern. */
  if(nbytes >= d->buffer_bytes) {
    eprintf("jack-record: period size exceeds limit\n");
    FAILURE;
    return 1;
  }

  /* Check that there is adequate space in the ringbuffer. */
  int space = (int) jack_ringbuffer_write_space(d->ring_buffer);
  if(space < nbytes) {
    eprintf("jack-record: overflow error, %d > %d\n", nbytes, space);
    FAILURE;
    return 1;
  }

  /* Interleave input to buffer and copy into ringbuffer. */
  signal_interleave_to(d->j_buffer,
		       (const float **)d->in,
		       nframes,
		       d->channels);
  int err = jack_ringbuffer_write(d->ring_buffer,
				  (char *) d->j_buffer,
				  (size_t) nbytes);
  if(err != nbytes) {
    eprintf("jack-record: error writing to ringbuffer, %d != %d\n",
	    err, nbytes);
    FAILURE;
    return 1;
  }

  /* Poke the disk thread to indicate data is on the ring buffer. */
  char b = 1;
  xwrite(d->pipe[1], &b, 1);

  return 0;
}

void usage(void)
{
  eprintf("Usage: jack-record [ options ] sound-file\n");
  eprintf("    -b N : Ring buffer size in frames (default=4096).\n");
  eprintf("    -f N : File format (default=0x10006).\n");
  eprintf("    -m N : Minimal disk read size in frames (default=32).\n");
  eprintf("    -n N : Number of channels (default=2).\n");
  eprintf("    -p S : Jack port pattern to connect to (default=nil).\n");
  eprintf("    -s   : Write to multiple single channel sound files.\n");
  eprintf("    -t N : Set a timer to record for N seconds (default=-1).\n");
  FAILURE;
}

int main(int argc, char *argv[])
{
  observe_signals ();
  struct recorder d;
  d.buffer_frames = 4096;
  d.minimal_frames = 32;
  d.channels = 2;
  d.timer_seconds = -1.0;
  d.timer_counter = 0;
  d.file_format = SF_FORMAT_WAV | SF_FORMAT_FLOAT;
  d.multiple_sound_files = 0;
  int c;
  char *p = NULL;
  while((c = getopt(argc, argv, "b:f:hm:n:p:st:")) != -1) {
    switch(c) {
    case 'b':
      d.buffer_frames = (int) strtol(optarg, NULL, 0);
      break;
    case 'f':
      d.file_format = (int) strtol(optarg, NULL, 0);
      break;
    case 'h':
      usage ();
      break;
    case 'm':
      d.minimal_frames = (int) strtol(optarg, NULL, 0);
      break;
    case 'n':
      d.channels = (int) strtol(optarg, NULL, 0);
      break;
    case 'p':
      p = malloc(128);
      strncpy(p,optarg,128);
      break;
    case 's':
      d.multiple_sound_files = 1;
      break;
    case 't':
      d.timer_seconds = (float) strtod(optarg, NULL);
      break;
    default:
      eprintf("jack-record: illegal option, %c\n", c);
      usage ();
      break;
    }
  }
  if(optind != argc - 1) {
    usage ();
  }

  /* Allocate channel based data. */
  if(d.channels < 1) {
    eprintf("jack-record: illegal number of channels: %d\n", d.channels);
    FAILURE;
  }
  d.in = xmalloc(d.channels * sizeof(float *));
  d.sound_file = xmalloc(d.channels * sizeof(SNDFILE *));
  d.input_port = xmalloc(d.channels * sizeof(jack_port_t *));

  /* Connect to JACK. */
  char client_name[64] = "jack-record";
  jack_client_t *client = jack_client_unique_store(client_name);
  jack_set_error_function(jack_client_minimal_error_handler);
  jack_on_shutdown(client, jack_client_minimal_shutdown_handler, 0);
  jack_set_process_callback(client, process, &d);
  d.sample_rate = jack_get_sample_rate(client);

  /* Setup timer. */
  if(d.timer_seconds < 0.0) {
    d.timer_frames = -1;
  } else {
    d.timer_frames = d.timer_seconds * d.sample_rate;
  }

  /* Create sound file. */
  SF_INFO sfinfo;
  sfinfo.samplerate = (int) d.sample_rate;
  sfinfo.frames = 0;
  sfinfo.format = d.file_format;
  if(d.multiple_sound_files) {
    if(!strstr(argv[optind], "%d")) {
      eprintf("jack-record: illegal template, '%s'\n", argv[optind]);
      usage ();
    }
    sfinfo.channels = 1;
    int i;
    for(i = 0; i < d.channels; i++) {
      char name[512];
      snprintf(name, 512, argv[optind], i);
      d.sound_file[i] = xsf_open(name, SFM_WRITE, &sfinfo);
    }
  } else {
    sfinfo.channels = d.channels;
    d.sound_file[0] = xsf_open(argv[optind], SFM_WRITE, &sfinfo);
  }

  /* Allocate buffers. */
  d.buffer_samples = d.buffer_frames * d.channels;
  d.buffer_bytes = d.buffer_samples * sizeof(float);
  d.d_buffer = xmalloc(d.buffer_bytes);
  d.j_buffer = xmalloc(d.buffer_bytes);
  d.u_buffer = xmalloc(d.buffer_bytes);
  d.ring_buffer = jack_ringbuffer_create(d.buffer_bytes);

  /* Create communication pipe. */
  xpipe(d.pipe);

  /* Start disk thread. */
  pthread_create (&(d.disk_thread),
		  NULL,
		  disk_thread_procedure,
		  &d);

  /* Create ports, connect to if given, activate client. */
  jack_port_make_standard(client, d.input_port, d.channels, false);
  jack_client_activate(client);
  if (p) {
    char q[128];
    snprintf(q,128,"%s:in_%%d",client_name);
    jack_port_connect_pattern(client,d.channels,p,q);
  }

  /* Wait for disk thread to end, which it does when it reaches the
     end of the file or is interrupted. */
  pthread_join(d.disk_thread, NULL);

  /* Close sound file, free ring buffer, close JACK connection, close
     pipe, free data buffers, indicate success. */
  jack_client_close(client);
  if(d.multiple_sound_files) {
    int i;
    for(i = 0; i < d.channels; i++) {
      sf_close(d.sound_file[i]);
    }
  } else {
    sf_close(d.sound_file[0]);
  }
  jack_ringbuffer_free(d.ring_buffer);
  close(d.pipe[0]);
  close(d.pipe[1]);
  free(d.d_buffer);
  free(d.j_buffer);
  free(d.u_buffer);
  free(d.in);
  free(d.input_port);
  free(d.sound_file);
  return EXIT_SUCCESS;
}
