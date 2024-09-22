#ifndef _ARGS_H
#define _ARGS_H

#include "lib/logging.h"

void usage(void);
bool parse_opts(struct logging* logging, int argc, char *argv[], struct recorder *recorder_obj);

#endif
