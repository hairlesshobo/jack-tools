#ifndef STATUS_CURSES_H
#define STATUS_CURSES_H

#include <curses.h>

#define COLOR_CLIP_FG 4
#define COLOR_CLIP_BG 10
#define COLOR_HOT_FG 3
#define COLOR_HOT_BG 11
#define COLOR_OK_FG 2
#define COLOR_OK_BG 12
#define COLOR_WEAK_FG 6
#define COLOR_WEAK_BG 13

struct CursesSupport {
	WINDOW* window_status;
	WINDOW* window_logs;
};

void foreground_color_by_sig_level(WINDOW* win, short level);
void background_color_by_sig_level(WINDOW* win, short level);
void write_curses_log_line(struct CursesSupport* cursesSupport, char* status_line);
void init_curses(struct CursesSupport* cursesSupport);
void cleanup_curses(struct CursesSupport* cursesSupport);
void update_curses_status(
	struct CursesSupport* cursesSupport, 
	struct recorder* recorder_obj,
	uint64_t individual_file_size,
	uint64_t total_output_size,
	float elapsed_time,
	float buffer_state);

#endif
