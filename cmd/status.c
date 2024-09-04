#include <math.h> /* C99 */
#include <stdbool.h>
#include <stdio.h> /* C99 */
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <locale.h>

#include <pthread.h> /* Posix */
#include <unistd.h>

#include <curses.h>

#include "rju-record.h"
#include "r-common/c/observe-signal.h"

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

const char *format_duration(float duration) {
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

float amp_to_db(float x)
{
	return (log10(x) * 20.0);
}

void clear_peaks(struct recorder *recorder_obj)
{
	for (int i = 0; i < recorder_obj->channels; i++) {
		recorder_obj->sig_max[i] = 0.0;
	}
}

void color_by_sig_level(float level)
{
	if (level > -1) {
		attron(COLOR_PAIR(4));
	}
	// else if (level >= -1) {
	// 	attron(COLOR_PAIR(4));
	// }
    else if (level >= -6) {
		attron(COLOR_PAIR(3));
	}
	else if (level >= -18) {
		attron(COLOR_PAIR(2));
	}
	else {
		attron(COLOR_PAIR(6));
	}
}

void *status_update_procedure(void *PTR)
{
	// setup curses
    setlocale(LC_ALL, "");
	initscr();
	keypad(stdscr, TRUE);
	nonl();
	halfdelay(1);
	noecho();

	start_color();
	init_pair(1, COLOR_WHITE, COLOR_BLACK);
    init_pair(2, COLOR_GREEN, COLOR_BLACK);
	init_pair(3, COLOR_YELLOW, COLOR_BLACK);
	init_pair(4, COLOR_RED, COLOR_BLACK);
	init_pair(5, COLOR_MAGENTA, COLOR_BLACK);
    init_pair(6, COLOR_CYAN, COLOR_BLACK);

	uint64_t last_reset_time = get_time_millis();

	struct recorder *recorder_obj = (struct recorder *)PTR;
	while (!observe_end_of_process()) {
		if (recorder_obj->do_abort == 1)
			break;

		char c = getch();
		switch (c) {
			case 'x':
				clear_peaks(recorder_obj);
				break;
		}

		if (get_time_millis() - last_reset_time > PEAK_HOLD_MS) {
			last_reset_time = get_time_millis();
			clear_peaks(recorder_obj);
		}

		erase();

		uint64_t file_size = (recorder_obj->timer_counter * recorder_obj->bit_rate) / 8;
		int file_count = 1;
		jack_nframes_t jack_frames = jack_frame_time(recorder_obj->client);
		float elapsed_time = ((float)(jack_frames - recorder_obj->start_frame)) / (float)recorder_obj->sample_rate;

		if (recorder_obj->multiple_sound_files == 1)
			file_count = recorder_obj->channels;

		attron(COLOR_PAIR(1));
		printw("Recording | Channels: %d, Files: %d, Elapsed: %s, size: %s, xruns: %d\n", 
			recorder_obj->channels, 
			file_count, 
			format_duration(elapsed_time), 
			format_size(file_size),
			recorder_obj->xrun_count
			);
			// (recorder_obj->performance / (1.0 / (float)recorder_obj->sample_rate)) * 100.0);

		printw("\n");
		char *set_max = malloc(sizeof(char) * recorder_obj->channels);
		memset(set_max, 0, sizeof(char) * recorder_obj->channels);

		for (int l = -2; l < METER_STEP_COUNT; l++) {
			if (l == -2) {
				printw("     |");

				for (int channel = 0; channel < recorder_obj->channels; channel++) {
					int leftover = (channel+1) % 10;

					// we only care to show the decade digit at the top of each decade
					if (leftover == 0) {
						int decade = (int)((channel+1) / 10);
						printw("  %d", decade);
					}
					else
						printw("   ");
				}

				printw("\n");
			}

			else if (l == -1) {
				attron(COLOR_PAIR(1));
				printw("     |");

				for (int channel = 0; channel < recorder_obj->channels; channel++) {
					int leftover = (channel+1) % 10;

					printw("  %1d", leftover);
				}

				printw("\n");
			}

			else if (l == METER_STEP_COUNT-1) {
				attron(COLOR_PAIR(1));
				printw("     |");
				for (int channel = 0; channel < recorder_obj->channels; channel++) {
					float level = amp_to_db(recorder_obj->sig_lvl[0][channel]);

					if (level <= -99)
						level = -99;

					printw("%3.0f", level*-1);
				}
				printw("\n");
			}

			else {
				float meter_step = meter_steps[l];
				attron(COLOR_PAIR(1));
				printw(" %3.0f |", meter_steps[l]);

				for (int channel = 0; channel < recorder_obj->channels; channel++) {
					float level = amp_to_db(recorder_obj->sig_lvl[0][channel]);
					float max_level = amp_to_db(recorder_obj->sig_max[channel]);


					if (max_level > meter_step && set_max[channel] == 0) {
						color_by_sig_level(max_level);
						printw("  ");
                        addch(ACS_CKBOARD);
						set_max[channel] = 1;
					}
					else if (level > meter_step) {
						color_by_sig_level(meter_step);
                        printw("  ");
                        addch(ACS_CKBOARD);

						// if (meter_step >= -1) {
						// 	printw("  X");
						// }
						// else if (meter_step >= -6) {
						// 	printw("  #");
						// }
						// else if (meter_step >= -18) {
						// 	printw("  ^");
						// }
						// else {
						// 	printw("  *");
						// }
					}

					else
						printw("   ");
				}

				printw("\n");
			}
		}


		refresh();
	}

	endwin();

	return NULL;
}
