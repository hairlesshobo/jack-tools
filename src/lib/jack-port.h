#ifndef _COMMON_JACK_PORT_H
#define _COMMON_JACK_PORT_H

#include <jack/jack.h>
#include <stdbool.h>

#include "logging.h"
#include "../recorder.h"

void jack_register_input_ports(struct logging* logging, struct recorder *recorder_obj);
int jack_port_connect_named(struct recorder *recorder_obj, const char *src, const char *dst);
void jack_port_connect_pattern(struct recorder *recorder_obj, char *dst);

#endif
