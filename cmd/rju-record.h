#ifndef RJU_RECORD_H
#define RJU_RECORD_H

#include <sndfile.h>
#include <stdbool.h>

#include <jack/jack.h>

#include "r-common/c/ringbuffer.h"

#define BUFFER_PERF_SAMPLES 4
#define PORT_NAME_PATTERN_WIDTH 64
#define METER_STEP_COUNT 21
// TODO: Add option validation that the provided channel count is > 0 and <= MAX_NC
#define MAX_NC 48
// TODO: move to cli option
#define PEAK_HOLD_MS 750
// TODO: move to cli option
#define USE_CURSES 1

// number of seconds to abort recording if no data received from jack
// TODO: move to CLI option
#define TIMEOUT_NO_DATA 2

const float meter_steps[METER_STEP_COUNT] = {
	0, -1, -2, -3, -4, -6, -8, -10, 
	-12, -15, -18, -21, -24, -27, -30, 
	-36, -42, -48, -54, -60
};

#define abort_or_alert_when(x, ...) \
	if (x) { \
		printf("\n"); \
		fprintf(stderr, __VA_ARGS__); \
		if (recorder_obj->abort_on_error == true) { \
			recorder_obj->do_abort = 1; \
		} \
	}

struct recorder {
	// user options
	int channels;
	int multiple_sound_files;
	int bit_rate;
	char format_name[4];
    char port_name_pattern[PORT_NAME_PATTERN_WIDTH];
	int port_offset;
	int abort_on_error;
	int minimal_frames;
	int buffer_frames;
	float timer_seconds;

	// for time-limited recording
	int timer_frames;
	int timer_counter;

	// ring buffer used for briefly storing data from jack while waiting 
	// to write to disk
	ringbuffer_t *rb;


	// buffer to hold data that is pending disk write, soon to be sent to 
	// `write_to_disk` function
	float *disk_write_buffer;


	// buffer for holding interleaved data that is to be written to the ringbuffer
	float *interleaved_buffer;

	// buffer used to hold uninterleaved data ready to be written to 
	// disk when writing multiple files
	float *uninterleave_buffer;


	// for tracking recording duration
	jack_nframes_t start_frame;
	jack_nframes_t last_frame;

	// level and peak metering
	float sig_lvl[2][MAX_NC]; /* signal level (ie. level) */
	float sig_max[MAX_NC]; /* signal maximum (since reset) */
	float sig_peak[MAX_NC]; /* signal peaks (peak hold) */

	// threading and IPC
	pthread_t disk_thread;
	pthread_t status_thread;
	int pipe[2];
	int do_abort;

	int file_format;
	SNDFILE **sound_file;
	jack_port_t **input_port;
	float **in;
	jack_client_t *client;
	bool unique_name;
	float sample_rate;
	int buffer_bytes;
	int buffer_samples;
	uint32_t xrun_count;
	float buffer_performance[BUFFER_PERF_SAMPLES];
	int buffer_performance_index;
	int buffer_performance_filled;

	time_t last_received_data_time;
};

#endif
