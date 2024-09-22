#include "logging.h"

struct logging* setup_logging(const char* file_name, const char* mode)
{
    struct logging* logging = malloc(sizeof(struct logging));
    logging->write_to_stderr = true;

    xpipe(logging->pipe_handle);
	fcntl(logging->pipe_handle[0], F_SETFL, O_NONBLOCK);

	logging->file_handle = malloc(sizeof(FILE *));
	*logging->file_handle = fopen(file_name, mode);

    return logging;
}

const char* get_log_level(uint8_t level)
{
    switch (level)
    {
        case L_ERROR:
            return "error";
        case L_WARN:
            return "warning";
        case L_INFO:
            return "info";
        case L_DEBUG:
            return "debug";
        default:
            return "unknown";
    }
}

int writelog(struct logging* logging, uint8_t log_level, char* fmt, ...)
{
	va_list args;
    va_start(args, fmt);

	char* message = malloc(256);
	vsnprintf(message, 256, fmt, args);

    // trim trailing newline, if any
    size_t ln = strlen(message) - 1;
    if (*message && message[ln] == '\n') 
        message[ln] = ' ';

    time_t timer = time(NULL);
    // TODO: why is this throwing an EXC_BAD_ACCESS error at times?
    struct tm* tm_info = localtime(&timer);

    char dtm[26];
    strftime(dtm, 26, "%Y-%m-%d %H:%M:%S", tm_info);

    char* string = malloc(256);
    snprintf(string, 255, "[%s] [%s]   %s", dtm, get_log_level(log_level), message);

	fprintf(*logging->file_handle, "%s\n", string);

    int char_count = -1;

    if (logging->pipe_handle[1] >= 0)
	    char_count = xwrite(logging->pipe_handle[1], string, 255);

    if (logging->write_to_stderr == true)
        fprintf(stderr, "%s\n", string);

	free(message);
    free(string);

    va_end(args);

	return char_count;
}
