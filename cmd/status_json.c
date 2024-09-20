#include <stdlib.h>
#include <string.h>

#include <unistd.h>

#include "recorder.h"
#include "status_utils.h"

char* json_config_line =
    "{"
    "\"type\": \"config\", "
    "\"channels\": %d, "
    "\"format\": \"%s\", "
    "\"bitrate\": %d, " 
    "\"sample_rate\": %1.0f"
    "}\n";


char* json_status_line = 
    "{"
    "\"type\": \"status\", "
    "\"status\": \"recording\", "
    "\"elapsed_seconds\": %1.0f, "
    "\"error_count\": %d, "
    "\"buffer_state\": \"%1.0f%%\", "
    "\"total_bytes\": %ull"
    "}\n";

char* json_log_line =
    "{"
    "\"type\": \"log\", "
    "\"timestamp\": %ull, "
    "\"message\": \"%s\""
    "}\n";

void write_json_log_line(char* status_line)
{
    // trim trailing newline, if any
    size_t ln = strlen(status_line) - 1;
    if (*status_line && status_line[ln] == '\n') 
        status_line[ln] = '\0';

    fprintf(stdout, json_log_line,
        (unsigned long)time(NULL),
        status_line
    );
}

void write_json_status(
	struct recorder* recorder_obj,
	uint64_t individual_file_size,
	uint64_t total_output_size,
	float elapsed_time,
	float buffer_state)
{
    fprintf(stdout, json_status_line, 
        elapsed_time, 
        recorder_obj->error_count,
        buffer_state,
        total_output_size
    );
}

void write_json_config_line(struct recorder* recorder_obj)
{
    fprintf(stdout, json_config_line, 
        recorder_obj->channels,
        recorder_obj->format_name,
        recorder_obj->bit_rate,
        recorder_obj->sample_rate
    );
}
