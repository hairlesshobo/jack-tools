#ifndef STATUS_JSON_H
#define STATUS_JSON_H

#include <curses.h>

void write_json_log_line(char* status_line);
void write_json_status(
	struct recorder* recorder_obj,
	uint64_t individual_file_size,
	uint64_t total_output_size,
	float elapsed_time,
	float buffer_state);
void write_json_config_line(struct recorder* recorder_obj);

#endif
