#ifndef _RJU_RECORD_H
#define _RJU_RECORD_H

#include <sndfile.h>
#include <stdbool.h>

#include <jack/jack.h>

#include "lib/ringbuffer.h"

#define BUFFER_PERF_SAMPLES 4
#define PORT_NAME_PATTERN_WIDTH 64
#define MAX_NC 48

// number of seconds to abort recording if no data received from jack
// TODO: move to CLI option
#define TIMEOUT_NO_DATA 2

#define OUTPUT_NONE 0
#define OUTPUT_CURSES 1
#define OUTPUT_JSON 2
#define OUTPUT_TEXT 3

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
	uint8_t output_type;
	uint16_t peak_hold_ms;

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
	short sig_lvl[2][MAX_NC]; /* signal level (ie. level) */
	short sig_max[MAX_NC]; /* signal maximum (since reset) */
	short sig_peak[MAX_NC]; /* signal peaks (peak hold) */

	// threading and IPC
	pthread_t disk_thread;
	pthread_t status_thread;
	int disk_pipe[2];
	int messaging_pipe[2];
	int do_abort;

	int file_format;
	SNDFILE **sound_file;
	jack_port_t **input_port;
	float **in;
	bool unique_name;
	float sample_rate;
	int buffer_bytes;
	int buffer_samples;
	uint32_t error_count;
	float buffer_performance[BUFFER_PERF_SAMPLES];
	int buffer_performance_index;
	int buffer_performance_filled;

	time_t last_received_data_time;

	FILE **log_file;
	jack_client_t *client;
};

#endif
