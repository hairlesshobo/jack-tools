#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "rju-record.h"
#include "status.h"
#include "status_utils.h"
#include "status_curses.h"
#include "r-common/c/file.h"
#include "r-common/c/observe-signal.h"

int printlg(int fdes, char* fmt, ...)
{
	va_list args;
    va_start(args, fmt);

	char* string = malloc(256);
	vsnprintf(string, 256, fmt, args);
	int char_count = xwrite(fdes, string, 255);
	free(string);

    va_end(args);

	return char_count;
}

void *status_update_procedure(void *PTR)
{
	struct recorder *recorder_obj = (struct recorder *)PTR;
	struct CursesSupport* cursesSupport;

	char status_line[256];

	if (recorder_obj->output_type == OUTPUT_CURSES) {
		cursesSupport = (struct CursesSupport*)malloc(sizeof(struct CursesSupport));
		init_curses(cursesSupport);
	}

	uint64_t last_reset_time = get_time_millis();
	// uint64_t last_status_time = get_time_millis();

	while (!observe_end_of_process()) {
		if (recorder_obj->do_abort == 1)
			break;

		if (recorder_obj->last_received_data_time > 0 && time(NULL) - recorder_obj->last_received_data_time > TIMEOUT_NO_DATA) {
			fprintf(stderr, "rju-record: No data received from jack after %d seconds (hardware removed?), aborting recording.\n", TIMEOUT_NO_DATA);
			recorder_obj->do_abort = 1;
			break;
		}

		if (get_time_millis() - last_reset_time > PEAK_HOLD_MS) {
			last_reset_time = get_time_millis();
			clear_peaks(recorder_obj);
		}

		// calculate file sizes
		uint64_t individual_file_size = ((uint64_t)recorder_obj->timer_counter * (uint64_t)recorder_obj->bit_rate) / 8ULL;
		uint64_t total_output_size = recorder_obj->channels * individual_file_size;

		// calculate recording duration
		jack_nframes_t jack_frames = jack_frame_time(recorder_obj->client);
		float elapsed_time = ((float)(jack_frames - recorder_obj->start_frame)) / (float)recorder_obj->sample_rate;

		// calculate buffer state
		float buffer_state = calculate_buffer_state(recorder_obj);

		// handle writing log messages
		if (read(recorder_obj->messaging_pipe[0], &status_line, 255) >= 0) {
			if (recorder_obj->output_type == OUTPUT_CURSES)
    			write_curses_log_line(cursesSupport, status_line);

			memset(&status_line, 0, 256);
		}

		if (recorder_obj->output_type == OUTPUT_CURSES) {
			update_curses_status(
				cursesSupport, 
				recorder_obj,
				individual_file_size,
				total_output_size,
				elapsed_time,
				buffer_state);
		}
	}

	if (recorder_obj->output_type == OUTPUT_CURSES)
		cleanup_curses(cursesSupport);

	return NULL;
}
