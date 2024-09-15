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

#define COLOR_CLIP_FG 4
#define COLOR_CLIP_BG 10
#define COLOR_HOT_FG 3
#define COLOR_HOT_BG 11
#define COLOR_OK_FG 2
#define COLOR_OK_BG 12
#define COLOR_WEAK_FG 6
#define COLOR_WEAK_BG 13

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

short amp_to_db(float x)
{
	return (short)(log10(x) * 20.0);
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

void foreground_color_by_sig_level(WINDOW* win, short level)
{
	if (level > -1)
		wattron(win, COLOR_PAIR(COLOR_CLIP_FG));
    else if (level >= -6) 
		wattron(win, COLOR_PAIR(COLOR_HOT_FG));
	else if (level >= -18) 
		wattron(win, COLOR_PAIR(COLOR_OK_FG));
	else 
		wattron(win, COLOR_PAIR(COLOR_WEAK_FG));
}

void background_color_by_sig_level(WINDOW* win, short level)
{
	if (level > -1) 
		wattron(win, COLOR_PAIR(COLOR_CLIP_BG));
    else if (level >= -6) 
		wattron(win, COLOR_PAIR(COLOR_HOT_BG));
	else if (level >= -18) 
		wattron(win, COLOR_PAIR(COLOR_OK_BG));
	else 
		wattron(win, COLOR_PAIR(COLOR_WEAK_BG));
}

void *status_update_procedure(void *PTR)
{
	struct recorder *recorder_obj = (struct recorder *)PTR;

	WINDOW* window_status;
	WINDOW* window_logs;

	if (USE_CURSES == true) {
		// setup curses
		setlocale(LC_ALL, "");
		initscr();
		keypad(stdscr, TRUE);
		nonl();
		halfdelay(1);
		noecho();
		curs_set(0);
		window_status = newwin(34, 0, 0, 0);
		window_logs = newwin(0, 0, 34, 0);

		refresh();

		box(window_status, 0, 0);
		box(window_logs, 0, 0);
		mvwprintw(window_logs, 1, 1, "subwindow");


		refresh();
		wrefresh(window_status);
		wrefresh(window_logs);

		start_color();
		init_pair(1, COLOR_WHITE, COLOR_BLACK);
		init_pair(2, COLOR_GREEN, COLOR_BLACK);
		init_pair(3, COLOR_YELLOW, COLOR_BLACK);
		init_pair(4, COLOR_RED, COLOR_BLACK);
		init_pair(5, COLOR_MAGENTA, COLOR_BLACK);
		init_pair(6, COLOR_CYAN, COLOR_BLACK);

		init_pair(10, COLOR_WHITE, COLOR_RED);
		init_pair(11, COLOR_WHITE, COLOR_YELLOW);
		init_pair(12, COLOR_WHITE, COLOR_GREEN);
		init_pair(13, COLOR_WHITE, COLOR_CYAN);
	}

	uint64_t last_reset_time = get_time_millis();

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

		if (USE_CURSES == true) {
			char c = getch();
			switch (c) {
				case 'x':
					clear_max(recorder_obj);
					break;
			}

			wmove(window_status, 0, 0);

			werase(window_status);
			wattron(window_status, COLOR_PAIR(1));
			box(window_status, 0, 0);
		}

		uint64_t file_size = (recorder_obj->timer_counter * recorder_obj->bit_rate) / 8;
		jack_nframes_t jack_frames = jack_frame_time(recorder_obj->client);
		float elapsed_time = ((float)(jack_frames - recorder_obj->start_frame)) / (float)recorder_obj->sample_rate;

		if (USE_CURSES == true) {
			int sc = 2;
			int sl = 1; /* track the line number */

			wattron(window_status, COLOR_PAIR(1));
			mvwprintw(window_status, sl++, sc, "Status      : Recording");
			mvwprintw(window_status, sl++, sc, "Elapsed     : %s", format_duration(elapsed_time));
			mvwprintw(window_status, sl++, sc, "Format      : %dbit/%2.0fk %s, Channels: %d",
				recorder_obj->bit_rate,
				(float)recorder_obj->sample_rate/(float)1000,
				recorder_obj->format_name,
				recorder_obj->channels
			);

			// TODO: logic needs to change to prepare for varied file layouts
			mvwprintw(window_status, sl++, sc, "File size   : %s", format_size(file_size));
			if (recorder_obj->multiple_sound_files == 1)
				wprintw(window_status, " x %d, total size: %s", recorder_obj->channels, format_size(recorder_obj->channels * file_size));
			
			
			// calculate the buffer performance
			// int buffer_read_space = (int)ringbuffer_read_space(recorder_obj->rb);
			// float buffer_used = ((float)buffer_read_space / (float)recorder_obj->buffer_bytes) * 100.0;
			

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

			mvwprintw(window_status, sl++, sc, "Buffer state: %1.0f%%", (sum / (float)diviser));

			mvwprintw(window_status, sl++, sc, "Error count : %d", recorder_obj->xrun_count);
				// (recorder_obj->performance / (1.0 / (float)recorder_obj->sample_rate)) * 100.0);

			// draw the channel input level meter on the screen
			sl++;
			char *set_max = malloc(sizeof(char) * recorder_obj->channels);
			memset(set_max, 0, sizeof(char) * recorder_obj->channels);

			for (int l = -3; l < METER_STEP_COUNT+1; l++) {
				// first of two header lines containing channel number
				if (l == -3) {
					wmove(window_status, sl++, sc);
					wprintw(window_status, "     ");
					waddch(window_status, ACS_VLINE);

					for (int channel = 0; channel < recorder_obj->channels; channel++) {
						int leftover = (channel+1) % 10;

						// we only care to show the decade digit at the top of each decade
						if (leftover == 0) {
							int decade = (int)((channel+1) / 10);
							wprintw(window_status, "   %d", decade);
						}
						else
							wprintw(window_status, "    ");
					}
				}

				// second of two header lines containing channel count
				else if (l == -2) {
					wmove(window_status, sl++, sc);
					wattron(window_status, COLOR_PAIR(1));
					wprintw(window_status, "     ");
					waddch(window_status, ACS_VLINE);

					for (int channel = 0; channel < recorder_obj->channels; channel++) {
						int leftover = (channel+1) % 10;

						wprintw(window_status, "   %1d", leftover);
					}
				}

				// footer showing the input signal level
				else if (l == METER_STEP_COUNT) {
					wmove(window_status, sl++, sc);
					wattron(window_status, COLOR_PAIR(1));
					wprintw(window_status, "     ");
					waddch(window_status, ACS_VLINE);

					for (int channel = 0; channel < recorder_obj->channels; channel++) {
						short level = recorder_obj->sig_max[channel];

						if (level <= -99)
							level = -99;

						foreground_color_by_sig_level(window_status, level);
						wprintw(window_status, " %3d", level);
					}
				}

				else if (l == -1 || l == METER_STEP_COUNT-1) {
					wmove(window_status, sl++, sc);
					wattron(window_status, COLOR_PAIR(1));
					waddch(window_status, ACS_HLINE);
					waddch(window_status, ACS_HLINE);
					waddch(window_status, ACS_HLINE);
					waddch(window_status, ACS_HLINE);
					waddch(window_status, ACS_HLINE);
					waddch(window_status, ACS_PLUS);
					

					for (int channel = 0; channel < recorder_obj->channels+1; channel++)
						for (int i = 0; i < 5; i++)
							waddch(window_status, ACS_HLINE);
				}

				// draw the input level lines
				else {
					wmove(window_status, sl++, sc);
					float meter_step = meter_steps[l];
					wattron(window_status, COLOR_PAIR(1));
					wprintw(window_status, " %3.0f ", meter_steps[l]);
					waddch(window_status, ACS_VLINE);

					for (int channel = 0; channel < recorder_obj->channels; channel++) {
						short level = recorder_obj->sig_lvl[0][channel];
						short peak_level = recorder_obj->sig_peak[channel];

						if (peak_level > meter_step && set_max[channel] == false) {
							foreground_color_by_sig_level(window_status, peak_level);
							wprintw(window_status, "   ");
							waddch(window_status, ACS_CKBOARD | A_BOLD);
							set_max[channel] = true;
						}
						else if (level > meter_step) {
							foreground_color_by_sig_level(window_status, meter_step);
							wprintw(window_status, "   ");
							waddch(window_status, ACS_CKBOARD | A_BOLD);
						}

						else
							wprintw(window_status, "    ");
					}
				}
			}


			refresh();
			wrefresh(window_status);
			wrefresh(window_logs);
		}
	}

	if (USE_CURSES == true)
		endwin();

	return NULL;
}
