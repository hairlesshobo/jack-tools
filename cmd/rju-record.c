#include <math.h> /* C99 */
#include <stdbool.h>
#include <stdio.h> /* C99 */
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <pthread.h> /* Posix */
#include <unistd.h>
#include <getopt.h>

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
// track long-term max separate from peak
// bottom number should show the max level since reset, and red background on clip
// gracefully handle jack server shutdown
// gracefully handle jack hardware removal
// option for output directory, create if doesn't exist
// option for meter peak hold time
// option to enable console output status
// option to output status as json lines?
// possibly add silence on xrun error?
// support combination file types, single and multiple channel files in a single project
// support passing config with file name and input channel numbers


/// @brief Method used to wait for data on the ring buffer prior to writing to disk
/// @param r ring buffer
/// @param nbytes byte count to wait for
/// @param fd IPC communication pipe
/// @param recorder_obj Structure containing config and recording data
/// @return number of bytes read
int ringbuffer_wait_read(const ringbuffer_t *rb, int needed_byte_count, int fd, struct recorder *recorder_obj)
{
	int space = (int)ringbuffer_read_space(rb);

	// so this just loops until there is enough in the ring buffer to read the requested number of bytes, 
	// but doesn't actually read from the buffer
	while (space < needed_byte_count && recorder_obj->do_abort == false) {
		// i think this is simply used as a way to abort out of the wait, because it doesn't actually
		// do anything with the single byte it read. pretty sure this is just a "is the pipe alive"
		// check and nothing more. definitely replace this with something more sophisticated, or at
		// least better for error handling so we don't end up with corrupt output files
		char b;
		if (read(fd, &b, 1) == -1) {
			fprintf(stderr, "%s: error reading communication pipe\n", __func__);
			exit(1);
		}

		space = (int)ringbuffer_read_space(rb);
	}

	if (recorder_obj->do_abort == true)
		return 0;

	return space;
}


/// @brief Function that is called to actually write the audio frames to disk
/// @param recorder_obj Structure containing config and recording data
/// @param nframes Number of frames being written to disk
void write_to_disk(struct recorder *recorder_obj, int nframes)
{
	if (recorder_obj->multiple_sound_files) {
		float *uninterleave_buffer_ptr = recorder_obj->uninterleave_buffer;
		signal_uninterleave(uninterleave_buffer_ptr, recorder_obj->disk_write_buffer, nframes, recorder_obj->channels);
		int i;
		for (i = 0; i < recorder_obj->channels; i++) {
			xsf_write_float(recorder_obj->sound_file[i],
				uninterleave_buffer_ptr,
				(sf_count_t)nframes);
			uninterleave_buffer_ptr += nframes;
		}
	} else {
		int nsamples = nframes * recorder_obj->channels;
		xsf_write_float(recorder_obj->sound_file[0],
			recorder_obj->disk_write_buffer,
			(sf_count_t)nsamples);
	}
}

/// @brief Procedure used for disk writing thread
/// @param PTR Pointer to the structure containing config and recording data
/// @return none
void *disk_thread_procedure(void *PTR)
{
	struct recorder *recorder_obj = (struct recorder *)PTR;
	// uint64_t loop = 0;

	while (!observe_end_of_process()) {
		// printf("loop %llu\n", loop);
		// loop++;

		/* Wait for data at the ring buffer. */
		int needed_byte_count = recorder_obj->minimal_frames * sizeof(float) * recorder_obj->channels;
		int available_byte_count = ringbuffer_wait_read(recorder_obj->rb, needed_byte_count, recorder_obj->pipe[0], recorder_obj);

		// the number of bytes available in the ring buffer exceeds what we want, so 
		// we have to drop the excess
		// TODO: why is this necessary? is it even?
		/* Drop excessive data to not overflow the local buffer. */
		if (available_byte_count > recorder_obj->buffer_bytes) {
			fprintf(stderr, "rju-record: impossible condition, read space.\n");
			available_byte_count = recorder_obj->buffer_bytes;
		}

		if (recorder_obj->do_abort == true) {
			fprintf(stderr, "rju-record: aborting file write\n");
			break;
		}

		/* Read data from the ring buffer. */
		ringbuffer_read(recorder_obj->rb, (char *)recorder_obj->disk_write_buffer, available_byte_count);

		/* Do write operation.  The sample count *must* be an integral number of frames. */
		int frame_count_to_write = (available_byte_count / sizeof(float)) / recorder_obj->channels;
		write_to_disk(recorder_obj, frame_count_to_write);

		/* Handle timer */
		recorder_obj->timer_counter += frame_count_to_write;
		if (recorder_obj->timer_frames > 0 && recorder_obj->timer_counter >= recorder_obj->timer_frames) {
			return NULL;
		}
	}

	recorder_obj->do_abort = 1;

	return NULL;
}


/// @brief Callback that is executed whenever jack shuts down cleanly
/// @param PTR Pointer to the structure containing config and recording data
void jack_shutdown(void *PTR)
{
	struct recorder *recorder_obj = (struct recorder *)PTR;

	recorder_obj->do_abort = 1;
	recorder_obj->rb->size_mask = 0;
	recorder_obj->rb->write_ptr = 0;
	recorder_obj->rb->read_ptr = 0;

	fprintf(stderr, "jack shutdown\n");
}


/// @brief Callback that is excecuted whenever a jack error occurs
/// @param desc String description of the error
void jack_error_handler(const char *desc)
{
	fprintf(stderr, "jack error: %s\n", desc);
}

/* Write data from the Jack input ports to the ring buffer.  If the
   disk thread is late, ie. the ring buffer is full, print an error
   and halt the client.  */

/// @brief Callback that is called any time jack has more data for us to read
/// @param nframes Number of frames to be read from jack
/// @param PTR Pointer to the structure containing config and recording data
/// @return 0 on success, non-zero on error
int process(jack_nframes_t nframes, void *PTR)
{
	// clock_t tic = clock();
	struct recorder *recorder_obj = (struct recorder *)PTR;

	// TODO: this should be done more elegantly - write out the available frames and THEN abort
	// if we were instructed to abort, then we shouldn't process any more frames
	if (recorder_obj->do_abort == 1)
		return 0;

	int received_sample_count = nframes * recorder_obj->channels;
	int received_byte_count = received_sample_count * sizeof(float);

	/* Get port data buffers. */
	// TODO: This will have to be adjusted when we add diverse file channel support
	for (int i = 0; i < recorder_obj->channels; i++) {
		recorder_obj->in[i] = (float *)jack_port_get_buffer(recorder_obj->input_port[i], nframes);

		// calculate level meters
		recorder_obj->sig_lvl[0][i] = 0.0;

		for (int j = 0; j < nframes; j++) {
			float sig_level = fabsf(recorder_obj->in[i][j]);

			if (sig_level > recorder_obj->sig_peak[i])
				recorder_obj->sig_peak[i] = sig_level;

			if (sig_level > recorder_obj->sig_lvl[0][i])
				recorder_obj->sig_lvl[0][i] = sig_level;
		}

		recorder_obj->sig_lvl[1][i] = (recorder_obj->sig_lvl[0][i] + recorder_obj->sig_lvl[1][i]) / 2.0;
	}

	/* Check period size is workable. If the buffer is large, ie 4096
	   frames, this should never be of practical concern. */
	abort_or_alert_when(received_byte_count >= recorder_obj->buffer_bytes, "rju-record: period size exceeds limit\n");

	/* Check that there is adequate space in the ringbuffer. */
	int space = (int)ringbuffer_write_space(recorder_obj->rb);

	float buffer_available = (1.0 - ((float)space / (float)recorder_obj->buffer_bytes)) * 100.0;
	recorder_obj->buffer_performance[recorder_obj->buffer_performance_index] = buffer_available;
	recorder_obj->buffer_performance_index += 1;

	if (recorder_obj->buffer_performance_index > BUFFER_PERF_SAMPLES)
	{
		recorder_obj->buffer_performance_index = 0;

		if (recorder_obj->buffer_performance_filled == false)
			recorder_obj->buffer_performance_filled = true;
	}

	// this means that the available space for writing is less than the number of 
	// bytes we received from jack this is called a buffer overflow because data is 
	// coming in faster than we can write it to disk. quick fix: increase the 
	// buffer size. A high buffer size can add latency, but in our use case of simply 
	// writing channels to disk, it doesn't matter and the improved stability
	// is the highest priority
	abort_or_alert_when(space < received_byte_count, "rju-record: overflow error (xrun?), %d > %d\n", received_byte_count, space);

	/* Interleave input to buffer */
	signal_interleave_to(recorder_obj->interleaved_buffer,
		(const float **)recorder_obj->in,
		nframes,
		recorder_obj->channels);
	
	// copy input data to ring buffer
	int err = ringbuffer_write(recorder_obj->rb, (char *)recorder_obj->interleaved_buffer, (size_t)received_byte_count);

	abort_or_alert_when(err != received_byte_count, "rju-record: ringbuffer write error, %d != %d\n", err, received_byte_count);

	/* Poke the disk thread to indicate data is on the ring buffer. */
	char b = 1;
	xwrite(recorder_obj->pipe[1], &b, 1);

	recorder_obj->last_frame = jack_last_frame_time(recorder_obj->client);
	// clock_t toc = clock();

	// recorder_obj->performance = (double)(toc - tic) / CLOCKS_PER_SEC;

	recorder_obj->last_received_data_time = time(NULL);

	return 0;
}

void usage(void)
{
	printf("Usage: rju-record [options] sound-file\n");
	printf("\n");
	printf("  --port_pattern         -p  <string>             : Jack port pattern to connect to (default: system:capture_%%d)\n");
	printf("  --channels             -c  [number]             : Number of channels (default: 2)\n");
	printf("  --bitrate              -b  [16, 24, 32]         : file bitrate (default: 16)\n");
	printf("  --format               -f  [wav, bwf/rf64]      : File format (default: wav)\n");
	printf("  --disk_transfer_size   -d  [number]             : Minimal disk transfer size in frames (default: 32)\n");
	printf("  --time_limit           -l  [number]             : Set a limit timer to record for N seconds (default: unlimited)\n");
	printf("  --port_offset          -o  [number]             : Jack port source offset (default: 0)\n");
	printf("  --buffer_size          -x  [number]             : Ring buffer size in frames (default: 4096)\n");
	printf("  --abort_on_error       -a                       : Abort recording if an error occurs (default: no)\n");
	printf("  --multi_file           -m                       : Write to multiple single channel sound files (defualt: no)\n");
	printf("  --no_unique            -u                       : Do not generate unique jack client name (ie. do not append PID)\n");
	printf("  --help                 -h                       : Show this usage page\n");
	exit(1);
}

int parse_opts(int argc, char *argv[], struct recorder *recorder_obj) {
	recorder_obj->unique_name = true;
	recorder_obj->buffer_frames = 4096;
	recorder_obj->minimal_frames = 32;
	recorder_obj->channels = 2;
	recorder_obj->timer_seconds = -1.0;
	recorder_obj->timer_counter = 0;
	recorder_obj->multiple_sound_files = 0;
	recorder_obj->do_abort = 0;
	recorder_obj->abort_on_error = 0;
	recorder_obj->bit_rate = 16;
	recorder_obj->xrun_count = 0;
	// recorder_obj->performance = 0;
	recorder_obj->file_format = SF_FORMAT_WAV | SF_FORMAT_FLOAT;
	recorder_obj->last_received_data_time = 0;
	recorder_obj->buffer_performance_index = 0;
	recorder_obj->buffer_performance_filled = false;

	strncpy(recorder_obj->port_name_pattern, "system:capture_%d", PORT_NAME_PATTERN_WIDTH-1);

    struct option long_options[] = {
        {"port_pattern", required_argument, NULL, 'p'},
        {"channels", required_argument, NULL, 'c'},
        {"bitrate", required_argument, NULL, 'b'},
        {"format", required_argument, NULL, 'f'},
        {"disk_transfer_size", required_argument, NULL, 'd'},
        {"time_limit", required_argument, NULL, 'l'},
        {"multi_file", no_argument, NULL, 'm'},
        {"abort_on_error", no_argument, NULL, 'a'},
        {"port_offset", required_argument, NULL, 'p'},
        {"no_unique", no_argument, NULL, 'u'},
        {"buffer_size", required_argument, NULL, 'x'},
        {NULL, 0, NULL, 0}
    };

	// char *port_name_pattern = NULL;
	char *format_name = "wav";
	// int port_offset = 0;
	int c;
	

	while ((c = getopt_long(argc, argv, "p:c:b:f:d:l:mo:ux:ha",
					long_options, NULL)) != -1) {
		switch (c) {
			case 'x':
				recorder_obj->buffer_frames = (int)strtol(optarg, NULL, 0);
				break;
			case 'f':
				format_name = malloc(256);
				strncpy(format_name, optarg, 255);
				break;
			case 'b':
				recorder_obj->bit_rate = (int)strtol(optarg, NULL, 0);
				break;
			case 'h':
				usage();
				break;
			case 'd':
				recorder_obj->minimal_frames = (int)strtol(optarg, NULL, 0);
				break;
			case 'c':
				recorder_obj->channels = (int)strtol(optarg, NULL, 0);
				break;
			case 'o':
				recorder_obj->port_offset = (int)strtol(optarg, NULL, 0);
				break;
			case 'p':
				// recorder_obj->port_name_pattern = malloc(256);
				strncpy(recorder_obj->port_name_pattern, optarg, PORT_NAME_PATTERN_WIDTH-1);
				break;
			case 'm':
				recorder_obj->multiple_sound_files = 1;
				break;
			case 'l':
				recorder_obj->timer_seconds = (float)strtod(optarg, NULL);
				break;
			case 'u':
				recorder_obj->unique_name = false;
				break;
			case 'a':
				recorder_obj->abort_on_error = true;
				break;
			default:
				fprintf(stderr, "\n");
				usage();
				break;
		}
	}

	if (optind != argc - 1)
		usage();

	int file_format = 0;
	int file_bitrate = 0;

	switch (recorder_obj->bit_rate) {
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
			fprintf(stderr, "Unknown bitrate provided: %d\n", recorder_obj->bit_rate);

			usage();
			return EXIT_FAILURE;
	}

	if (strcmp(format_name, "wav") == 0) {
		strncpy(recorder_obj->format_name, "WAV", 4);
		file_format = SF_FORMAT_WAV;
	} else if (strcmp(format_name, "bwf") == 0 || strcmp(format_name, "rf64") == 0) {
		strncpy(recorder_obj->format_name, "BWF", 4);
		file_format = SF_FORMAT_RF64;
	} else {
		fprintf(stderr, "ERROR: unknown file format: %s\n", format_name);
		return EXIT_FAILURE;
	}

	recorder_obj->file_format = file_format | file_bitrate;

	die_when(recorder_obj->channels < 1, "ERROR: channels count provided is less than 1: %d\n", recorder_obj->channels);

	clear_peaks(recorder_obj);
	clear_max(recorder_obj);

	return 0;
}

int main(int argc, char *argv[])
{
	observe_signals();

	struct recorder recorder_obj;

	int opt_results = parse_opts(argc, argv, &recorder_obj);

	die_when(opt_results != 0, "Invalid configuration provided");

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

	/* Allocate channel based data. */
	recorder_obj.in = xmalloc(recorder_obj.channels * sizeof(float *));
	recorder_obj.sound_file = xmalloc(recorder_obj.channels * sizeof(SNDFILE *));
	recorder_obj.input_port = xmalloc(recorder_obj.channels * sizeof(jack_port_t *));

	/* Allocate buffers. */
	recorder_obj.buffer_samples = recorder_obj.buffer_frames * recorder_obj.channels;
	recorder_obj.buffer_bytes = recorder_obj.buffer_samples * sizeof(float);
	recorder_obj.disk_write_buffer = xmalloc(recorder_obj.buffer_bytes);
	recorder_obj.interleaved_buffer = xmalloc(recorder_obj.buffer_bytes);
	recorder_obj.uninterleave_buffer = xmalloc(recorder_obj.buffer_bytes);
	recorder_obj.rb = ringbuffer_create(recorder_obj.buffer_bytes);
	recorder_obj.buffer_bytes = recorder_obj.rb->size;

	/* Setup timer. */
	if (recorder_obj.timer_seconds <= 0.0)
		recorder_obj.timer_frames = -1;
	else
		recorder_obj.timer_frames = recorder_obj.timer_seconds * recorder_obj.sample_rate;

	/* Create sound file. */
	SF_INFO sfinfo;
	sfinfo.samplerate = (int)recorder_obj.sample_rate;
	sfinfo.frames = 0;
	sfinfo.format = recorder_obj.file_format;
	sfinfo.channels = 1;

	if (recorder_obj.multiple_sound_files) {
		// TODO: improve or remove this altogether
		if (!strstr(argv[optind], "%d") && !strstr(argv[optind], "%02d")) {
			fprintf(stderr, "rju-record: illegal template, '%s'\n", argv[optind]);
			usage();
		}

		for (int i = 0; i < recorder_obj.channels; i++) {
			char name[512];
			snprintf(name, 512, argv[optind], i+1);
			recorder_obj.sound_file[i] = xsf_open(name, SFM_WRITE, &sfinfo);
		}
	} else {
		sfinfo.channels = recorder_obj.channels;
		recorder_obj.sound_file[0] = xsf_open(argv[optind], SFM_WRITE, &sfinfo);
	}

	/* Create communication pipe. */
	xpipe(recorder_obj.pipe);

	/* Start disk thread. */
	pthread_create(&(recorder_obj.disk_thread), NULL, disk_thread_procedure, &recorder_obj);

	/* Create ports, connect to if given, activate client. */
	char connect_pattern[128];
	snprintf(connect_pattern, 128, "%s:in_%%d", client_name);

	jack_port_make_standard(recorder_obj.client, recorder_obj.input_port, recorder_obj.channels, false, false);
	jack_client_activate(recorder_obj.client);
	jack_port_connect_pattern(recorder_obj.client, recorder_obj.channels, recorder_obj.port_offset, recorder_obj.port_name_pattern, connect_pattern);

	/* Start status update thread. */
	pthread_create(&(recorder_obj.status_thread), NULL, status_update_procedure, &recorder_obj);

	/* Wait for disk thread to end, which it does when it reaches the
	   end of the file or is interrupted. */
	fprintf(stderr, "waiting for disk thread\n");
	pthread_join(recorder_obj.disk_thread, NULL);
	fprintf(stderr, "disk thread joined\n");
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
	free(recorder_obj.disk_write_buffer);
	free(recorder_obj.interleaved_buffer);
	free(recorder_obj.uninterleave_buffer);
	free(recorder_obj.in);
	free(recorder_obj.input_port);
	free(recorder_obj.sound_file);

	printf("\n");

	if (recorder_obj.do_abort == 1)
		return EXIT_FAILURE;
	else
		return EXIT_SUCCESS;
}
