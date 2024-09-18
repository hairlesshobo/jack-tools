#include <math.h> /* C99 */
#include <stdarg.h>
#include <unistd.h>

#include "rju-record.h"
#include "status.h"
#include "status_utils.h"

const char *format_size(uint64_t bytes)
{
	char *suffix[] = {"B", "KiB", "MiB", "GiB", "TiB"};
	char length = sizeof(suffix) / sizeof(suffix[0]);

	int i = 0;
	double dblBytes = bytes;

	if (bytes > 1024) {
		for (i = 0; (bytes / 1024) > 0 && i<length-1; i++, bytes /= 1024)
			dblBytes = bytes / 1024.0;
	}

	static char output[200];
	sprintf(output, "%.02lf %s", dblBytes, suffix[i]);
	return output;
}

const char *format_duration(float duration)
{
	int hours = 0;
	int minutes = 0;
	int seconds = 0;
	int mseconds = 0;

	if (duration > 3600) {
		hours = duration / 3600;
		duration -= hours * 3600;
	}

	if (duration > 60) {
		minutes = duration / 60;
		duration -= minutes * 60;
	}

	seconds = (int)duration;
	duration -= seconds;
	mseconds = duration * 1000;

	static char result[16];
	snprintf(result, 13, "%02d:%02d:%02d.%03d", hours, minutes, seconds, mseconds);

	return result;
}

short amp_to_db(float x)
{
	return (short)(log10(x) * 20.0);
}

float calculate_buffer_state(struct recorder* recorder_obj)
{
    float sum = 0.0;
    int diviser = 0;

    if (recorder_obj->buffer_performance_filled == false) {
        for (int i = 0; i < recorder_obj->buffer_performance_index; i++) 
            sum += recorder_obj->buffer_performance[i];

        diviser = recorder_obj->buffer_performance_index;
    } else {
        for (int i = 0; i < BUFFER_PERF_SAMPLES; i++)
            sum += recorder_obj->buffer_performance[i];

        diviser = BUFFER_PERF_SAMPLES;
    }

    return sum / (float)diviser;
}

int abort_or_alert_when(struct recorder *recorder_obj, int condition, char* fmt, ...)
{
	va_list args;
    va_start(args, fmt);

	if (condition)
	{
		printlg(recorder_obj->messaging_pipe[1], recorder_obj->log_file, fmt, args);

		if (recorder_obj->abort_on_error == true) {
			recorder_obj->do_abort = 1;
		}

		return 1;
	}

	va_end(args);

	return 0;
}

void clear_peaks(struct recorder *recorder_obj)
{
	for (int i = 0; i < MAX_NC; i++) {
		recorder_obj->sig_peak[i] = -99;
	}
}

void clear_max(struct recorder *recorder_obj)
{
	for (int i = 0; i < MAX_NC; i++) {
		recorder_obj->sig_max[i] = -99;
	}
}

// thank you to the answer here: https://stackoverflow.com/a/11765441
uint64_t get_time_millis(void)
{
	struct timespec tms;

    /* The C11 way */
    /* if (! timespec_get(&tms, TIME_UTC)) { */

    /* POSIX.1-2008 way */
    if (clock_gettime(CLOCK_REALTIME, &tms)) {
        return -1;
    }
    /* seconds, multiplied with 1 million */
    int64_t micros = tms.tv_sec * 1000000;

    /* Add full microseconds */
    micros += tms.tv_nsec/1000;

    /* round up if necessary */
    if (tms.tv_nsec % 1000 >= 500) {
        ++micros;
    }

	return micros / 1000;
}
