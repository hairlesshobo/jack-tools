#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <errno.h>
#include <sys/types.h>
#include <unistd.h>

#include "failure.h"
#include "jack-port.h"
#include "print.h"
#include "../recorder.h"
#include "../status.h"

void jack_register_input_ports(struct recorder *recorder_obj)
{
	int i;
	for (i = 0; i < recorder_obj->channels; i++) {
		char name[64];
		snprintf(name, 64, "in_%d", i + 1);
		recorder_obj->input_port[i] = jack_port_register(recorder_obj->client, name, JACK_DEFAULT_AUDIO_TYPE, JackPortIsInput, 0);

		if (!recorder_obj->input_port[i]) {
			printlg(recorder_obj->messaging_pipe[1], recorder_obj->log_file, "ERROR: jack_port_register() failed\n");
			FAILURE;
		}
	}
}

int jack_port_connect_named(struct recorder *recorder_obj, const char *src, const char *dst)
{
	int err = jack_connect(recorder_obj->client, src, dst);

	if (err) {
		printlg(recorder_obj->messaging_pipe[1], recorder_obj->log_file, "ERROR: jack_connect() failed: '%s' -> '%s'\n", src, dst);

		switch (err) {
			case EEXIST:
				printlg(recorder_obj->messaging_pipe[1], recorder_obj->log_file, "ERROR: jack_connect() failed: connection exists\n");
				break;
			default:
				printlg(recorder_obj->messaging_pipe[1], recorder_obj->log_file, "ERROR: jack_connect() failed: unknown reason\n");
				/*       FAILURE; */
				break;
		}
	}
	return err;
}

/* TRUE iff the input port `l' is connected to the output port `r'. */

void jack_port_connect_pattern(struct recorder *recorder_obj, char *dst)
{
	int i;
	for (i = 0; i < recorder_obj->channels; i++) {
		char src_name[64], dst_name[64];
		snprintf(src_name, 64, recorder_obj->port_name_pattern, i + recorder_obj->port_offset + 1);
		snprintf(dst_name, 64, dst, i + 1);

		jack_port_connect_named(recorder_obj, src_name, dst_name);
	}
}

