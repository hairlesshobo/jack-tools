#include <stdio.h> /* C99 */
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#include <pthread.h> /* Posix */
#include <unistd.h>

#include "r-common/c/file.h"
#include "r-common/c/jack-client.h"
#include "r-common/c/jack-port.h"
#include "r-common/c/memory.h"
#include "r-common/c/observe-signal.h"
#include "r-common/c/ringbuffer.h"
#include "r-common/c/ringbuffer-fd.h"
#include "r-common/c/signal-interleave.h"
#include "r-common/c/sf-sndfile.h"

struct recorder
{
  bool unique_name;
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
  ringbuffer_t *rb;
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
    nbytes = ringbuffer_wait_for_read(d->rb, nbytes,
					   d->pipe[0]);

    /* Drop excessive data to not overflow the local buffer. */
    if(nbytes > d->buffer_bytes) {
      fprintf(stderr, "rju-record: impossible condition, read space.\n");
      nbytes = d->buffer_bytes;
    }

    /* Read data from the ring buffer. */
    ringbuffer_read(d->rb,
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

/* Write data from the Jack input ports to the ring buffer.  If the
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
  die_when(nbytes >= d->buffer_bytes,"rju-record: period size exceeds limit\n");

  /* Check that there is adequate space in the ringbuffer. */
  int space = (int)ringbuffer_write_space(d->rb);
  die_when(space < nbytes,"rju-record: overflow error, %d > %d\n", nbytes, space);

  /* Interleave input to buffer and copy into ringbuffer. */
  signal_interleave_to(d->j_buffer,
		       (const float **)d->in,
		       nframes,
		       d->channels);
  int err = ringbuffer_write(d->rb,
				  (char *) d->j_buffer,
				  (size_t) nbytes);
  die_when(err != nbytes,"rju-record: ringbuffer write error, %d != %d\n",err,nbytes);

  /* Poke the disk thread to indicate data is on the ring buffer. */
  char b = 1;
  xwrite(d->pipe[1], &b, 1);

  return 0;
}

void usage(void)
{
  printf("Usage: rju-record [options] sound-file\n");
  printf("  -b N : Ring buffer size in frames (default=4096).\n");
  printf("  -f N : File format (default=0x10006).\n");
  printf("  -m N : Minimal disk transfer size in frames (default=32).\n");
  printf("  -n N : Number of channels (default=2).\n");
  printf("  -o N : Jack port source offset (default=0).\n");
  printf("  -p S : Jack port pattern to connect to (default=nil).\n");
  printf("  -s   : Write to multiple single channel sound files.\n");
  printf("  -t N : Set a timer to record for N seconds (default=-1).\n");
  printf("  -u   : Do not generate unique jack client name (ie. do not append PID)\n");
  exit(1);
}

int main(int argc, char *argv[])
{
  observe_signals ();
  struct recorder d;
  d.unique_name = true;
  d.buffer_frames = 4096;
  d.minimal_frames = 32;
  d.channels = 2;
  d.timer_seconds = -1.0;
  d.timer_counter = 0;
  d.file_format = SF_FORMAT_WAV | SF_FORMAT_FLOAT;
  d.multiple_sound_files = 0;
  int c;
  char *p_pattern = NULL;
  int o = 0;
  while((c = getopt(argc, argv, "b:f:hm:n:o:p:st:u")) != -1) {
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
    case 'o':
      o = (int) strtol(optarg, NULL, 0);
      break;
    case 'p':
      p_pattern = malloc(256);
      strncpy(p_pattern,optarg,255);
      break;
    case 's':
      d.multiple_sound_files = 1;
      break;
    case 't':
      d.timer_seconds = (float) strtod(optarg, NULL);
      break;
    case 'u':
      d.unique_name = false;
      break;
    default:
      fprintf(stderr, "rju-record: illegal option, %c\n", c);
      usage ();
      break;
    }
  }
  if(optind != argc - 1) {
    usage ();
  }

  /* Allocate channel based data. */
  die_when(d.channels < 1,"rju-record: channels < 1: %d\n", d.channels);
  d.in = xmalloc(d.channels * sizeof(float *));
  d.sound_file = xmalloc(d.channels * sizeof(SNDFILE *));
  d.input_port = xmalloc(d.channels * sizeof(jack_port_t *));

  /* Connect to Jack. */
  char client_name[64] = "rju-record";
  jack_client_t *client;
  if(d.unique_name) {
    client = jack_client_unique_store(client_name);
  } else {
    client = jack_client_open(client_name,JackNullOption,NULL);
  }
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
      fprintf(stderr, "rju-record: illegal template, '%s'\n", argv[optind]);
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
  d.rb = ringbuffer_create(d.buffer_bytes);

  /* Create communication pipe. */
  xpipe(d.pipe);

  /* Start disk thread. */
  pthread_create (&(d.disk_thread),
		  NULL,
		  disk_thread_procedure,
		  &d);

  /* Create ports, connect to if given, activate client. */
  jack_port_make_standard(client, d.input_port, d.channels, false, false);
  jack_client_activate(client);
  if(p_pattern) {
    char q[128];
    snprintf(q,128,"%s:in_%%d",client_name);
    jack_port_connect_pattern(client,d.channels,o,p_pattern,q);
  }

  /* Wait for disk thread to end, which it does when it reaches the
     end of the file or is interrupted. */
  pthread_join(d.disk_thread, NULL);

  /* Close sound file, free ring buffer, close Jack connection, close
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
  ringbuffer_free(d.rb);
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
