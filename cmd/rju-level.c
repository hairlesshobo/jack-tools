#include <math.h> /* C99 */
#include <signal.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <dlfcn.h> /* POSIX */
#include <unistd.h>

#include <curses.h>

#include <jack/jack.h>
#include <jack/thread.h>

#include "r-common/c/failure.h"
#include "r-common/c/print.h"
#include "r-common/c/time-pause.h"

#include "r-common/c/jack-port.c"

void usage(void)
{
	eprintf("Usage: rju-level [ options ]\n");
	eprintf("  -k INT -- port index offset\n");
	eprintf("  -n INT -- number of channels\n");
	eprintf("  -p STR -- port connection pattern\n");
	exit(EXIT_SUCCESS);
}

#define MAX_STR 256
#define MAX_NC 256

struct world {
	jack_client_t *c; /* client */
	char cn[MAX_STR]; /* client name */
	char pp[MAX_STR]; /* port pattern */
	int pk; /* port offset */
	jack_port_t **ip; /* input ports */
	int32_t nc; /* number of channels */
	float sr; /* sample rate */
	float **in; /* input data */
	bool ef; /* exit flag */
	bool vb; /* verbose flag */
	float s_max[MAX_NC]; /* signal maxima (ie. hold) */
	float s_lvl[2][MAX_NC]; /* signal level (ie. level) */
};

void finish(int sig)
{
	fprintf(stderr, "rju-level: finish\n");
	endwin();
	exit(0);
}

void mvaddlvl(int x, int y, double n)
{
	char tmp[512];
	snprintf(tmp, 512, "%5.1f", (n < -90 ? -90 : n));
	mvaddstr(x, y, tmp);
}

int dsp_run(jack_nframes_t nf, void *ptr)
{
	struct world *w = (struct world *)ptr;
	for (int i = 0; i < w->nc; i++) {
		float *s = (float *)jack_port_get_buffer(w->ip[i], nf);
		w->s_lvl[0][i] = 0.0;
		for (int j = 0; j < nf; j++) {
			float x = fabsf(s[j]);
			if (x > w->s_max[i]) {
				w->s_max[i] = x;
			}
			if (x > w->s_lvl[0][i]) {
				w->s_lvl[0][i] = x;
			}
		}
		w->s_lvl[1][i] = (w->s_lvl[0][i] + w->s_lvl[1][i]) / 2.0;
	}
	return 0;
}

float amp_to_db(float x)
{
	return (log10(x) * 20.0);
}

int main(int argc, char **argv)
{
	struct world w;
	w.vb = false;
	w.ef = false;
	w.nc = 2;
	w.pk = 0;
	for (int i = 0; i < MAX_NC; i++) {
		w.s_max[i] = 0.0;
		w.s_lvl[0][i] = 0.0;
		w.s_lvl[1][i] = 0.0;
	}
	strncpy(w.pp, "system:capture_%d", 255);
	int c;
	while ((c = getopt(argc, argv, "hk:n:p:")) != -1) {
		switch (c) {
		case 'h':
			usage();
			break;
		case 'k':
			w.pk = (int)strtol(optarg, NULL, 0);
			break;
		case 'n':
			w.nc = (int32_t)strtol(optarg, NULL, 0);
			break;
		case 'p':
			strncpy(w.pp, optarg, 255);
			break;
		}
	}
	w.ip = malloc(w.nc * sizeof(jack_port_t *));
	w.in = malloc(w.nc * sizeof(float *));
	w.c = jack_client_open("rju-level", JackNullOption, NULL);
	die_when(!w.c, "could not create jack client\n");
	jack_set_process_callback(w.c, dsp_run, &w);
	w.sr = (float)jack_get_sample_rate(w.c);
	jack_port_make_standard(w.c, w.ip, w.nc, false, false);
	if (jack_activate(w.c)) {
		die("rju-dl: jack_activate() failed\n");
	}
	jack_port_connect_pattern(w.c, w.nc, w.pk, w.pp, "rju-level:in_%d");

	if (w.vb) {
		fprintf(stderr, "rju-level: init curses\n");
	}
	signal(SIGINT, finish);
	initscr();
	keypad(stdscr, TRUE);
	nonl();
	halfdelay(1);
	noecho();

	while (!w.ef) {
		c = getch();
		switch (c) {
		case 'z':
			for (int i = 0; i < w.nc; i++) {
				w.s_max[i] = 0.0;
			}
			erase();
			refresh();
			break;
		case 'q':
			finish(SIGINT);
			break;
		case ERR:
			break;
		}

		mvaddstr(0, 0, "rju-level");
		for (int i = 0; i < w.nc; i++) {
			mvaddlvl(2, i * 10, amp_to_db(w.s_max[i]));
			mvaddlvl(3, i * 10, amp_to_db(w.s_lvl[1][i]));
		}
	}
	fprintf(stderr, "START CLOSE\n");
	jack_client_close(w.c);
	fprintf(stderr, "END CLOSE\n");
	return EXIT_SUCCESS;
}
