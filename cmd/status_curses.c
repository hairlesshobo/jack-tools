#include <stdlib.h>
#include <string.h>
#include <locale.h>

#include <unistd.h>

#include <curses.h>

#include "rju-record.h"
#include "status_utils.h"
#include "status_curses.h"

const float meter_steps[METER_STEP_COUNT] = {
	0, -1, -2, -3, -4, -6, -8, -10, 
	-12, -15, -18, -21, -24, -27, -30, 
	-36, -42, -48, -54, -60
};


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

void init_curses(struct CursesSupport* cursesSupport)
{
	// setup curses
	setlocale(LC_ALL, "");
	initscr();
	keypad(stdscr, TRUE);
	nonl();
	// halfdelay(1);
	nodelay(stdscr, true);
	noecho();
	curs_set(0);
	cursesSupport->window_status = newwin(34, 0, 0, 0);
	cursesSupport->window_logs = newwin(0, 0, 34, 0);
	scrollok(cursesSupport->window_logs, TRUE);

	refresh();

	box(cursesSupport->window_status, 0, 0);
	// box(cursesSupport->window_logs, 0, 0);

	refresh();
	wrefresh(cursesSupport->window_status);
	wrefresh(cursesSupport->window_logs);

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

void cleanup_curses(struct CursesSupport* cursesSupport)
{
    endwin();
    free(cursesSupport);
}

void write_curses_log_line(struct CursesSupport* cursesSupport, char* status_line)
{
    time_t timer = time(NULL);
    struct tm* tm_info = localtime(&timer);

    char buffer[26];
    strftime(buffer, 26, "%Y-%m-%d %H:%M:%S", tm_info);

    wprintw(cursesSupport->window_logs, "[%s]:   %s", buffer, status_line);
}

void update_curses_status(
	struct CursesSupport* cursesSupport, 
	struct recorder* recorder_obj,
	uint64_t individual_file_size,
	uint64_t total_output_size,
	float elapsed_time,
	float buffer_state)
{
    char c = getch();
    switch (c) {
        case 'x':
            clear_max(recorder_obj);
            break;
    }
        
	wmove(cursesSupport->window_status, 0, 0);
	werase(cursesSupport->window_status);
	wattron(cursesSupport->window_status, COLOR_PAIR(1));
	box(cursesSupport->window_status, 0, 0);

	int sc = 2;
	int sl = 1; /* track the line number */

	wattron(cursesSupport->window_status, COLOR_PAIR(1));
	mvwprintw(cursesSupport->window_status, sl++, sc, "Status      : Recording");
	mvwprintw(cursesSupport->window_status, sl++, sc, "Elapsed     : %s", format_duration(elapsed_time));
	mvwprintw(cursesSupport->window_status, sl++, sc, "Format      : %dbit/%2.0fk %s, Channels: %d",
		recorder_obj->bit_rate,
		(float)recorder_obj->sample_rate/(float)1000,
		recorder_obj->format_name,
		recorder_obj->channels
	);

	// TODO: logic needs to change to prepare for varied file layouts
	mvwprintw(cursesSupport->window_status, sl++, sc, "File size   : %s", format_size(individual_file_size));
	if (recorder_obj->multiple_sound_files == 1)
		wprintw(cursesSupport->window_status, " x %d, total size: %s", recorder_obj->channels, format_size(total_output_size));
	
	mvwprintw(cursesSupport->window_status, sl++, sc, "Buffer state: %1.0f%%", buffer_state);
	mvwprintw(cursesSupport->window_status, sl++, sc, "Error count : %d", recorder_obj->error_count);
		// (recorder_obj->performance / (1.0 / (float)recorder_obj->sample_rate)) * 100.0);

	// draw the channel input level meter on the screen
	sl++;
	char *set_max = malloc(sizeof(char) * recorder_obj->channels);
	memset(set_max, 0, sizeof(char) * recorder_obj->channels);

	for (int l = -3; l < METER_STEP_COUNT+1; l++) {
		// first of two header lines containing channel number
		if (l == -3) {
			wmove(cursesSupport->window_status, sl++, sc);
			wprintw(cursesSupport->window_status, "     ");
			waddch(cursesSupport->window_status, ACS_VLINE);

			for (int channel = 0; channel < recorder_obj->channels; channel++) {
				int leftover = (channel+1) % 10;

				// we only care to show the decade digit at the top of each decade
				if (leftover == 0) {
					int decade = (int)((channel+1) / 10);
					wprintw(cursesSupport->window_status, "   %d", decade);
				}
				else
					wprintw(cursesSupport->window_status, "    ");
			}
		}

		// second of two header lines containing channel count
		else if (l == -2) {
			wmove(cursesSupport->window_status, sl++, sc);
			wattron(cursesSupport->window_status, COLOR_PAIR(1));
			wprintw(cursesSupport->window_status, "     ");
			waddch(cursesSupport->window_status, ACS_VLINE);

			for (int channel = 0; channel < recorder_obj->channels; channel++) {
				int leftover = (channel+1) % 10;

				wprintw(cursesSupport->window_status, "   %1d", leftover);
			}
		}

		// footer showing the input signal level
		else if (l == METER_STEP_COUNT) {
			wmove(cursesSupport->window_status, sl++, sc);
			wattron(cursesSupport->window_status, COLOR_PAIR(1));
			wprintw(cursesSupport->window_status, "     ");
			waddch(cursesSupport->window_status, ACS_VLINE);

			for (int channel = 0; channel < recorder_obj->channels; channel++) {
				short level = recorder_obj->sig_max[channel];

				if (level <= -99)
					level = -99;

				foreground_color_by_sig_level(cursesSupport->window_status, level);
				wprintw(cursesSupport->window_status, " %3d", level);
			}
		}

		else if (l == -1 || l == METER_STEP_COUNT-1) {
			wmove(cursesSupport->window_status, sl++, sc);
			wattron(cursesSupport->window_status, COLOR_PAIR(1));
			waddch(cursesSupport->window_status, ACS_HLINE);
			waddch(cursesSupport->window_status, ACS_HLINE);
			waddch(cursesSupport->window_status, ACS_HLINE);
			waddch(cursesSupport->window_status, ACS_HLINE);
			waddch(cursesSupport->window_status, ACS_HLINE);
			waddch(cursesSupport->window_status, ACS_PLUS);
			

			for (int channel = 0; channel < recorder_obj->channels+1; channel++)
				for (int i = 0; i < 5; i++)
					waddch(cursesSupport->window_status, ACS_HLINE);
		}

		// draw the input level lines
		else {
			wmove(cursesSupport->window_status, sl++, sc);
			float meter_step = meter_steps[l];
			wattron(cursesSupport->window_status, COLOR_PAIR(1));
			wprintw(cursesSupport->window_status, " %3.0f ", meter_steps[l]);
			waddch(cursesSupport->window_status, ACS_VLINE);

			for (int channel = 0; channel < recorder_obj->channels; channel++) {
				short level = recorder_obj->sig_lvl[0][channel];
				short peak_level = recorder_obj->sig_peak[channel];

				if (peak_level > meter_step && set_max[channel] == false) {
					foreground_color_by_sig_level(cursesSupport->window_status, peak_level);
					wprintw(cursesSupport->window_status, "   ");
					waddch(cursesSupport->window_status, ACS_CKBOARD | A_BOLD);
					set_max[channel] = true;
				}
				else if (level > meter_step) {
					foreground_color_by_sig_level(cursesSupport->window_status, meter_step);
					wprintw(cursesSupport->window_status, "   ");
					waddch(cursesSupport->window_status, ACS_CKBOARD | A_BOLD);
				}

				else
					wprintw(cursesSupport->window_status, "    ");
			}
		}
	}


	// refresh();
	wrefresh(cursesSupport->window_status);
	wrefresh(cursesSupport->window_logs);
}
