#include <math.h> /* C99 */
#include <stdbool.h>
#include <stdio.h> /* C99 */
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <pthread.h> /* Posix */
#include <unistd.h>

#include <curses.h>

#include "rju-record.h"
#include "status.c"
#include "jack/metadata.h"

#include "r-common/c/file.h"
#include "r-common/c/memory.h"
#include "r-common/c/observe-signal.h"
#include "r-common/c/ringbuffer-fd.h"
#include "r-common/c/ringbuffer.h"
#include "r-common/c/signal-interleave.h"

#include "r-common/c/jack-client.c"
#include "r-common/c/jack-port.c"
#include "r-common/c/sf-sndfile.c"

// TODO list:
// gracefully handle jack server shutdown
// gracefully handle jack hardware removal
// long options support
// option for output directory, create if doesn't exist
// option for meter peak hold time
// option to enable console output status
// option to output status as json lines?
// option to specify what to do on stream error (xrun, etc) - quit or continue (with audible glitch - possible silence?)
// support combination file types, single and multiple channel files in a single project
// support passing config with file name and input channel numbers

void write_to_disk(struct recorder *recorder_obj, int nframes)
{
	if (recorder_obj->multiple_sound_files) {
		float *p = recorder_obj->u_buffer;
		signal_uninterleave(p, recorder_obj->d_buffer, nframes, recorder_obj->channels);
		int i;
		for (i = 0; i < recorder_obj->channels; i++) {
			xsf_write_float(recorder_obj->sound_file[i],
				p,
				(sf_count_t)nframes);
			p += nframes;
		}
	} else {
		int nsamples = nframes * recorder_obj->channels;
		xsf_write_float(recorder_obj->sound_file[0],
			recorder_obj->d_buffer,
			(sf_count_t)nsamples);
	}
}

void *disk_thread_procedure(void *PTR)
{
	struct recorder *recorder_obj = (struct recorder *)PTR;
	uint64_t loop = 0;

	while (!observe_end_of_process()) {
		// printf("loop %llu\n", loop);
		loop++;

		/* Wait for data at the ring buffer. */
		int nbytes = recorder_obj->minimal_frames * sizeof(float) * recorder_obj->channels;
		nbytes = ringbuffer_wait_for_read(recorder_obj->rb, nbytes, recorder_obj->pipe[0]);

		if (recorder_obj->do_abort == 1) {
			fprintf(stderr, "rju-record: aborting file write\n");
			break;
		}

		/* Drop excessive data to not overflow the local buffer. */
		if (nbytes > recorder_obj->buffer_bytes) {
			fprintf(stderr, "rju-record: impossible condition, read space.\n");
			nbytes = recorder_obj->buffer_bytes;
		}

		/* Read data from the ring buffer. */
		ringbuffer_read(recorder_obj->rb,
			(char *)recorder_obj->d_buffer,
			nbytes);

		/* Do write operation.  The sample count *must* be an integral number of frames. */
		int nframes = (nbytes / sizeof(float)) / recorder_obj->channels;
		write_to_disk(recorder_obj, nframes);

		/* Handle timer */
		recorder_obj->timer_counter += nframes;
		if (recorder_obj->timer_frames > 0 && recorder_obj->timer_counter >= recorder_obj->timer_frames) {
			return NULL;
		}
	}

	recorder_obj->do_abort = 1;

	return NULL;
}

void jack_shutdown(void *PTR)
{
	struct recorder *recorder_obj = (struct recorder *)PTR;

	recorder_obj->do_abort = 1;

	fprintf(stderr, "jack shutdown\n");
}

void jack_error_handler(const char *desc)
{
	fprintf(stderr, "jack error: %s\n", desc);
}

/* Write data from the Jack input ports to the ring buffer.  If the
   disk thread is late, ie. the ring buffer is full, print an error
   and halt the client.  */
int process(jack_nframes_t nframes, void *PTR)
{
	// clock_t tic = clock();
	struct recorder *recorder_obj = (struct recorder *)PTR;

	// if we were instructed to abort, then we shouldn't process any more frames
	if (recorder_obj->do_abort == 1)
		return 0;

	int nsamples = nframes * recorder_obj->channels;
	int nbytes = nsamples * sizeof(float);

	/* Get port data buffers. */
	int i;
	for (i = 0; i < recorder_obj->channels; i++) {
		recorder_obj->in[i] = (float *)jack_port_get_buffer(recorder_obj->input_port[i], nframes);

		// calculate level meteres
		recorder_obj->sig_lvl[0][i] = 0.0;

		for (int j = 0; j < nframes; j++) {
			float x = fabsf(recorder_obj->in[i][j]);

			if (x > recorder_obj->sig_max[i])
				recorder_obj->sig_max[i] = x;

			if (x > recorder_obj->sig_lvl[0][i])
				recorder_obj->sig_lvl[0][i] = x;
		}

		recorder_obj->sig_lvl[1][i] = (recorder_obj->sig_lvl[0][i] + recorder_obj->sig_lvl[1][i]) / 2.0;
	}

	/* Check period size is workable. If the buffer is large, ie 4096
	   frames, this should never be of practical concern. */
	abort_when(nbytes >= recorder_obj->buffer_bytes, "rju-record: period size exceeds limit\n");

	/* Check that there is adequate space in the ringbuffer. */
	int space = (int)ringbuffer_write_space(recorder_obj->rb);
	// TODO: how does this affect the recording - should we make this a command that continues on error vs quits
	if (space < nbytes) {
		recorder_obj->xrun_count++;
		fprintf(stderr, "rju-record: overflow error (xrun?), %d > %d\n", nbytes, space);
	}
	// abort_when(space < nbytes, "rju-record: overflow error (xrun?), %d > %d\n", nbytes, space);

	/* Interleave input to buffer and copy into ringbuffer. */
	signal_interleave_to(recorder_obj->j_buffer,
		(const float **)recorder_obj->in,
		nframes,
		recorder_obj->channels);
	
	int err = ringbuffer_write(recorder_obj->rb,
		(char *)recorder_obj->j_buffer,
		(size_t)nbytes);

	// abort_when(err != nbytes, "rju-record: ringbuffer write error, %d != %d\n", err, nbytes);
	if (err != nbytes) {
		fprintf(stderr, "rju-record: ringbuffer write error, %d != %d\n", err, nbytes);
	}

	/* Poke the disk thread to indicate data is on the ring buffer. */
	char b = 1;
	xwrite(recorder_obj->pipe[1], &b, 1);

	recorder_obj->last_frame = jack_last_frame_time(recorder_obj->client);
	// clock_t toc = clock();

	// recorder_obj->performance = (double)(toc - tic) / CLOCKS_PER_SEC;

	return 0;
}

void usage(void)
{
	printf("Usage: rju-record [options] sound-file\n");
	printf("  -b N : Ring buffer size in frames (default=4096).\n");
	printf("  -f N : File format (wav, rf64).\n");
	printf("  -r N : file bitrate (16, 24, 32).\n");
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
	observe_signals();

	struct recorder recorder_obj;
	recorder_obj.unique_name = true;
	recorder_obj.buffer_frames = 4096;
	recorder_obj.minimal_frames = 32;
	recorder_obj.channels = 2;
	recorder_obj.timer_seconds = -1.0;
	recorder_obj.timer_counter = 0;
	recorder_obj.multiple_sound_files = 0;
	recorder_obj.do_abort = 0;
	recorder_obj.bit_rate = 16;
	recorder_obj.xrun_count = 0;
	recorder_obj.performance = 0;

	char *port_name_pattern = NULL;
	char *format_name = NULL;
	int port_offset = 0;
	int c;
	
	recorder_obj.file_format = SF_FORMAT_WAV | SF_FORMAT_FLOAT;


	while ((c = getopt(argc, argv, "b:f:r:hm:n:o:p:st:u")) != -1) {
		switch (c) {
			case 'b':
				recorder_obj.buffer_frames = (int)strtol(optarg, NULL, 0);
				break;
			case 'f':
				format_name = malloc(256);
				strncpy(format_name, optarg, 255);
				break;
			case 'r':
				recorder_obj.bit_rate = (int)strtol(optarg, NULL, 0);
				break;
			case 'h':
				usage();
				break;
			case 'm':
				recorder_obj.minimal_frames = (int)strtol(optarg, NULL, 0);
				break;
			case 'n':
				recorder_obj.channels = (int)strtol(optarg, NULL, 0);
				break;
			case 'o':
				port_offset = (int)strtol(optarg, NULL, 0);
				break;
			case 'p':
				port_name_pattern = malloc(256);
				strncpy(port_name_pattern, optarg, 255);
				break;
			case 's':
				recorder_obj.multiple_sound_files = 1;
				break;
			case 't':
				recorder_obj.timer_seconds = (float)strtod(optarg, NULL);
				break;
			case 'u':
				recorder_obj.unique_name = false;
				break;
			default:
				fprintf(stderr, "rju-record: illegal option, %c\n", c);
				usage();
				break;
		}
	}

	if (optind != argc - 1)
		usage();

	int file_format = 0;
	int file_bitrate = 0;

	switch (recorder_obj.bit_rate) {
		case (16):
			file_bitrate = SF_FORMAT_PCM_16;
			break;
		case (24):
			file_bitrate = SF_FORMAT_PCM_24;
			break;
		case (32):
			file_bitrate = SF_FORMAT_PCM_32;
			break;
		default:
			fprintf(stderr, "Unknown bitrate provided: %d\n", file_bitrate);
			return EXIT_FAILURE;
	}

	if (format_name) {
		if (strcmp(format_name, "wav") == 0) {
			fprintf(stderr, "Selected format: %d bit wav\n", recorder_obj.bit_rate);
			file_format = SF_FORMAT_WAV;
		} else if (strcmp(format_name, "rf64") == 0) {
			fprintf(stderr, "Selected format: %d bit rf64\n", recorder_obj.bit_rate);
			file_format = SF_FORMAT_RF64;
		} else {
			fprintf(stderr, "ERROR: unknown file format: %s\n", format_name);
			return EXIT_FAILURE;
		}
	} else {
		fprintf(stderr, "ERROR: file format is required\n");
		return EXIT_FAILURE;
	}

	recorder_obj.file_format = file_format | file_bitrate;

	/* Allocate channel based data. */
	die_when(recorder_obj.channels < 1, "rju-record: channels < 1: %d\n", recorder_obj.channels);
	recorder_obj.in = xmalloc(recorder_obj.channels * sizeof(float *));
	recorder_obj.sound_file = xmalloc(recorder_obj.channels * sizeof(SNDFILE *));
	recorder_obj.input_port = xmalloc(recorder_obj.channels * sizeof(jack_port_t *));

	/* Connect to Jack. */
	char client_name[64] = "rju-record";

	if (recorder_obj.unique_name) 
		recorder_obj.client = jack_client_unique_store(client_name);
	else
		recorder_obj.client = jack_client_open(client_name, JackNullOption, NULL);

	jack_set_error_function(jack_error_handler);
	jack_on_shutdown(recorder_obj.client, jack_shutdown, &recorder_obj);
	jack_set_process_callback(recorder_obj.client, process, &recorder_obj);

	recorder_obj.start_frame = jack_frame_time(recorder_obj.client);
	recorder_obj.sample_rate = jack_get_sample_rate(recorder_obj.client);

	/* Setup timer. */
	if (recorder_obj.timer_seconds < 0.0)
		recorder_obj.timer_frames = -1;
	else
		recorder_obj.timer_frames = recorder_obj.timer_seconds * recorder_obj.sample_rate;

	/* Create sound file. */
	SF_INFO sfinfo;
	sfinfo.samplerate = (int)recorder_obj.sample_rate;
	sfinfo.frames = 0;
	sfinfo.format = recorder_obj.file_format;

	if (recorder_obj.multiple_sound_files) {
		if (!strstr(argv[optind], "%d") && !strstr(argv[optind], "%02d")) {
			fprintf(stderr, "rju-record: illegal template, '%s'\n", argv[optind]);
			usage();
		}

		sfinfo.channels = 1;

		for (int i = 0; i < recorder_obj.channels; i++) {
			char name[512];
			snprintf(name, 512, argv[optind], i+1);
			recorder_obj.sound_file[i] = xsf_open(name, SFM_WRITE, &sfinfo);
		}
	} else {
		sfinfo.channels = recorder_obj.channels;
		recorder_obj.sound_file[0] = xsf_open(argv[optind], SFM_WRITE, &sfinfo);
	}

	/* Allocate buffers. */
	recorder_obj.buffer_samples = recorder_obj.buffer_frames * recorder_obj.channels;
	recorder_obj.buffer_bytes = recorder_obj.buffer_samples * sizeof(float);
	recorder_obj.d_buffer = xmalloc(recorder_obj.buffer_bytes);
	recorder_obj.j_buffer = xmalloc(recorder_obj.buffer_bytes);
	recorder_obj.u_buffer = xmalloc(recorder_obj.buffer_bytes);
	recorder_obj.rb = ringbuffer_create(recorder_obj.buffer_bytes);

	/* Create communication pipe. */
	xpipe(recorder_obj.pipe);

	/* Start disk thread. */
	pthread_create(&(recorder_obj.disk_thread),
		NULL,
		disk_thread_procedure,
		&recorder_obj);

	/* Create ports, connect to if given, activate client. */
	jack_port_make_standard(recorder_obj.client, recorder_obj.input_port, recorder_obj.channels, false, false);
	jack_client_activate(recorder_obj.client);

	if (port_name_pattern) {
		char q[128];
		snprintf(q, 128, "%s:in_%%d", client_name);
		jack_port_connect_pattern(recorder_obj.client, recorder_obj.channels, port_offset, port_name_pattern, q);
	}

	/* Start status update thread. */
	pthread_create(&(recorder_obj.status_thread),
		NULL,
		status_update_procedure,
		&recorder_obj);

	/* Wait for disk thread to end, which it does when it reaches the
	   end of the file or is interrupted. */
	printf("waiting for disk thread\n");
	pthread_join(recorder_obj.disk_thread, NULL);
	printf("disk thread joined\n");
	pthread_join(recorder_obj.status_thread, NULL);

	/* Close sound file, free ring buffer, close Jack connection, close
	   pipe, free data buffers, indicate success. */
	jack_client_close(recorder_obj.client);
	if (recorder_obj.multiple_sound_files)
		for (int i = 0; i < recorder_obj.channels; i++)
			sf_close(recorder_obj.sound_file[i]);
	else
		sf_close(recorder_obj.sound_file[0]);

	ringbuffer_free(recorder_obj.rb);
	close(recorder_obj.pipe[0]);
	close(recorder_obj.pipe[1]);
	free(recorder_obj.d_buffer);
	free(recorder_obj.j_buffer);
	free(recorder_obj.u_buffer);
	free(recorder_obj.in);
	free(recorder_obj.input_port);
	free(recorder_obj.sound_file);

	printf("\n");

	if (recorder_obj.do_abort == 1)
		return EXIT_FAILURE;
	else
		return EXIT_SUCCESS;
}
