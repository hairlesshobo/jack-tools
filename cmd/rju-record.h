#ifndef RJU_RECORD_H
#define RJU_RECORD_H

#include <sndfile.h>
#include <stdbool.h>

#include <jack/jack.h>

#include "r-common/c/ringbuffer.h"

#define MAX_NC 64
#define METER_STEP_COUNT 21
// TODO: move to cli option
#define PEAK_HOLD_MS 750

const float meter_steps[METER_STEP_COUNT] = {0, -1, -2, -3, -4, -6, -8, -10, -12, -15, -18, -21, -24, -27, -30, -36, -42, -48, -54, -60};

#define abort_when(x, ...) \
	if (x) { \
		printf("\n"); \
		fprintf(stderr, __VA_ARGS__); \
		recorder_obj->do_abort = 1; \
	}

struct recorder {
	bool unique_name;
	int buffer_bytes;
	int buffer_samples;
	int buffer_frames;
	int minimal_frames;
	int bit_rate;
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
    char port_name_pattern[256];
	int port_offset;
	float **in;
	ringbuffer_t *rb;
	pthread_t disk_thread;
	pthread_t status_thread;
	int pipe[2];
	int do_abort;
	jack_client_t *client;
	jack_nframes_t start_frame;
	jack_nframes_t last_frame;
	uint32_t xrun_count;
	double performance;
	float sig_max[MAX_NC]; /* signal maxima (ie. hold) */
	float sig_lvl[2][MAX_NC]; /* signal level (ie. level) */
};

#endif
