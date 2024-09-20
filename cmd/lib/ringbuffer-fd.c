#include <stdio.h>
#include <stdlib.h>

#include <unistd.h>

#include "ringbuffer.h"

int ringbuffer_wait_for_read(const ringbuffer_t *r, int nbytes, int fd)
{
	int space = (int)ringbuffer_read_space(r);
	while (space < nbytes) {
		char b;
		if (read(fd, &b, 1) == -1) {
			fprintf(stderr, "%s: error reading communication pipe\n", __func__);
			exit(1);
		}
		space = (int)ringbuffer_read_space(r);
	}
	return space;
}

int ringbuffer_wait_for_write(ringbuffer_t *r, int nbytes, int fd)
{
	int space = (int)ringbuffer_write_space(r);
	while (space < nbytes) {
		char b;
		if (read(fd, &b, 1) == -1) {
			fprintf(stderr, "%s: error reading communication pipe\n", __func__);
			exit(1);
		}
		space = (int)ringbuffer_write_space(r);
	}
	return space;
}
