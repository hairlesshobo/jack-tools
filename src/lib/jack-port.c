#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <errno.h>
#include <sys/types.h>
#include <unistd.h>

#include "failure.h"
#include "jack-port.h"
#include "print.h"

void jack_port_make_standard(jack_client_t *client, jack_port_t **ports, int n, bool output, bool midi)
{
	int i;
	for (i = 0; i < n; i++) {
		char name[64];
		int direction = output ? JackPortIsOutput : JackPortIsInput;
		snprintf(name, 64, output ? (midi ? "midi_out_%d" : "out_%d") : (midi ? "midi_in_%d" : "in_%d"), i + 1);
		ports[i] = jack_port_register(client, name, midi ? JACK_DEFAULT_MIDI_TYPE : JACK_DEFAULT_AUDIO_TYPE, direction, 0);
		if (!ports[i]) {
			eprintf("jack_port_register() failed\n");
			FAILURE;
		}
	}
}

int jack_port_connect_named(jack_client_t *client, const char *src, const char *dst)
{
	int err = jack_connect(client, src, dst);
	if (err) {
		eprintf("jack_connect() failed: '%s' -> '%s'\n", src, dst);
		switch (err) {
		case EEXIST:
			eprintf("jack_connect() failed: connection exists\n");
			break;
		default:
			eprintf("jack_connect() failed: unknown reason\n");
			/*       FAILURE; */
			break;
		}
	}
	return err;
}

int jack_port_disconnect_named(jack_client_t *client, const char *src, const char *dst)
{
	int err = jack_disconnect(client, src, dst);
	if (err) {
		eprintf("jack_disconnect() failed: '%s' -> '%s'\n", src, dst);
		FAILURE;
	}
	return err;
}

/* TRUE iff the input port `l' is connected to the output port `r'. */

int jack_port_is_connected_p(jack_client_t *j, const char *l, const char *r)
{
	const char **c;
	c = jack_port_get_all_connections(j, jack_port_by_name(j, l));
	if (c) {
		int k;
		for (k = 0; c[k]; k++) {
			if (strcmp(c[k], r) == 0) {
				free(c);
				return 1;
			}
		}
		free(c);
	}
	return 0;
}

/* Delete all connections at the port `l'. */
void jack_port_clear_all_connections(jack_client_t *j, const char *l)
{
	const char **c;
	c = jack_port_get_all_connections(j, jack_port_by_name(j, l));
	if (c) {
		int k;
		for (k = 0; c[k]; k++) {
			jack_port_disconnect_named(j, l, c[k]);
		}
		free(c);
	}
}

void jack_port_connect_pattern(jack_client_t *client, int n, int k, char *src, char *dst)
{
	int i;
	for (i = 0; i < n; i++) {
		char src_name[64], dst_name[64];
		snprintf(src_name, 64, src, i + k + 1);
		snprintf(dst_name, 64, dst, i + 1);
		jack_port_connect_named(client, src_name, dst_name);
	}
}

void jack_port_connect_to_env(jack_client_t *c, int n, int k, char *env)
{
	char *dst_pattern = getenv(env);
	if (dst_pattern) {
		char src_pattern[128];
		char *c_name = jack_get_client_name(c);
		snprintf(src_pattern, 128, "%s:out_%%d", c_name);
		jack_port_connect_pattern(c, n, k, src_pattern, dst_pattern);
	}
}

void jack_port_connect_from_env(jack_client_t *c, int n, int k, char *env)
{
	char *src_pattern = getenv(env);
	if (src_pattern) {
		char dst_pattern[128];
		char *c_name = jack_get_client_name(c);
		snprintf(dst_pattern, 128, "%s:in_%%d", c_name);
		jack_port_connect_pattern(c, n, k, src_pattern, dst_pattern);
	}
}
