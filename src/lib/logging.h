#ifndef _LOGGING_H
#define _LOGGING_H

#include <stdarg.h>
#include <fcntl.h> 
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "file.h"

#define L_NONE 0
#define L_ERROR 1
#define L_WARN 2
#define L_INFO 3
#define L_DEBUG 4

struct logging {
    int pipe_handle[2];
    FILE** file_handle;
    bool write_to_stderr;
};


struct logging* setup_logging(const char* file_name, const char* mode);
int writelog(struct logging* logging, uint8_t log_level, char* fmt, ...);

#endif
