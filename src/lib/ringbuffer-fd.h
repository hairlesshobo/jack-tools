#ifndef _RINGBUFFER_FD_H
#define _RINGBUFFER_FD_H

#include "ringbuffer.h"

int ringbuffer_wait_for_read(const ringbuffer_t *r, int nbytes, int fd);
int ringbuffer_wait_for_write(ringbuffer_t *r, int nbytes, int fd);

#endif
