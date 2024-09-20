#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ringbuffer.h"

/// @brief Prepare a new ringbuffer
/// @param sz size of the new ring buffer to create
/// @return Pointer to the new ringbuffer
ringbuffer_t *ringbuffer_create(size_t sz)
{
	// Allocate the space for the struct
	ringbuffer_t *rb = malloc(sizeof(ringbuffer_t));

	// failed to allocate memory?
	if (rb == NULL) {
		return NULL;
	}

	// why is the size determined in this manner??
	int power_of_two = 1;
	while ((1 << power_of_two) < sz) {
		power_of_two++;
	}
	rb->size = 1 << power_of_two;

	// what is the purpose of the size mask?
	rb->size_mask = rb->size;
	rb->size_mask -= 1;
	rb->write_ptr = 0;
	rb->read_ptr = 0;

	// allocate the actual buffer
	rb->buf = malloc(rb->size);

	// if we failed to allocate the memory, we abort
	if (rb->buf == NULL) {
		free(rb);
		return NULL;
	}

	return rb;
}

void ringbuffer_free(ringbuffer_t *rb)
{
	free(rb->buf);
	free(rb);
}

/// @brief Get the number of bytes available for reading from the ring buffer
/// @param rb pointer to the ring buffer to check
/// @return number of bytes available for reading
size_t ringbuffer_read_space(const ringbuffer_t *rb)
{
	size_t write_index = rb->write_ptr;
	size_t read_index = rb->read_ptr;

	if (write_index > read_index) {
		return write_index - read_index;
	} else {
		return (write_index - read_index + rb->size) & rb->size_mask;
	}
}


/// @brief Get the number of bytes remaining in the ring buffer for writing to
/// @param rb pointer to the ring buffer
/// @return number of bytes of space remaining in ring buffer
size_t ringbuffer_write_space(const ringbuffer_t *rb)
{
	size_t write_index = rb->write_ptr;
	size_t read_index = rb->read_ptr;
	if (write_index > read_index) {
		return ((read_index - write_index + rb->size) & rb->size_mask) - 1;
	} else if (write_index < read_index) {
		return (read_index - write_index) - 1;
	} else {
		return rb->size - 1;
	}
}

size_t ringbuffer_read(ringbuffer_t *rb, char *dest, size_t cnt)
{
	// check how many bytes are available to be read from the buffer
	size_t free_cnt = ringbuffer_read_space(rb);

	// if there are no bytes to read, there is nothing to do
	if (free_cnt == 0) {
		return 0;
	}

	// number of bytes to read.. if we are requesting more than is avilable, only return the number of
	// bytes actually available
	size_t to_read = cnt > free_cnt ? free_cnt : cnt;

	// get the offset of the end of the bytes we need to read	
	size_t end_index = rb->read_ptr + to_read;

	size_t n1, n2;

	// if the end index is higher than the size of the ring buffer, we'll have to loop
	// around and read form the beginning once we hit the end
	if (end_index > rb->size) {
		n1 = rb->size - rb->read_ptr;
		n2 = end_index & rb->size_mask;
	} else {
		n1 = to_read;
		n2 = 0;
	}

	// copy the requested bytes from the buffer to the provided destination
	memcpy(dest, &(rb->buf[rb->read_ptr]), n1);

	// adjust the read index according to the number of bytes we read
	rb->read_ptr = (rb->read_ptr + n1) & rb->size_mask;

	// TODO: this is where it locks up once jack hardware is removed
	// this second section only has to happen if we are looping around to the beginning
	// of the ring buffer
	if (n2) {
		// copy the remaining requested bytes from the beginning of the buffer to the
		// provided destination
		memcpy(dest + n1, &(rb->buf[rb->read_ptr]), n2);

		// adjust the read index for the ring buffer
		rb->read_ptr = (rb->read_ptr + n2) & rb->size_mask;
	}
	return to_read;
}

size_t ringbuffer_peek(ringbuffer_t *rb, char *dest, size_t cnt)
{
	size_t tmp_read_ptr = rb->read_ptr;
	size_t free_cnt = ringbuffer_read_space(rb);
	if (free_cnt == 0) {
		return 0;
	}
	size_t to_read = cnt > free_cnt ? free_cnt : cnt;
	size_t cnt2 = tmp_read_ptr + to_read;
	size_t n1, n2;
	if (cnt2 > rb->size) {
		n1 = rb->size - tmp_read_ptr;
		n2 = cnt2 & rb->size_mask;
	} else {
		n1 = to_read;
		n2 = 0;
	}
	memcpy(dest, &(rb->buf[tmp_read_ptr]), n1);
	tmp_read_ptr = (tmp_read_ptr + n1) & rb->size_mask;
	if (n2) {
		memcpy(dest + n1, &(rb->buf[tmp_read_ptr]), n2);
	}
	return to_read;
}

size_t ringbuffer_write(ringbuffer_t *rb, const char *src, size_t cnt)
{
	size_t free_cnt = ringbuffer_write_space(rb);
	if (free_cnt == 0) {
		return 0;
	}
	size_t to_write = cnt > free_cnt ? free_cnt : cnt;
	size_t cnt2 = rb->write_ptr + to_write;
	size_t n1, n2;
	if (cnt2 > rb->size) {
		n1 = rb->size - rb->write_ptr;
		n2 = cnt2 & rb->size_mask;
	} else {
		n1 = to_write;
		n2 = 0;
	}
	memcpy(&(rb->buf[rb->write_ptr]), src, n1);
	rb->write_ptr = (rb->write_ptr + n1) & rb->size_mask;
	if (n2) {
		memcpy(&(rb->buf[rb->write_ptr]), src + n1, n2);
		rb->write_ptr = (rb->write_ptr + n2) & rb->size_mask;
	}
	return to_write;
}

void ringbuffer_read_advance(ringbuffer_t *rb, size_t cnt)
{
	size_t tmp = (rb->read_ptr + cnt) & rb->size_mask;
	rb->read_ptr = tmp;
}

void ringbuffer_write_advance(ringbuffer_t *rb, size_t cnt)
{
	size_t tmp = (rb->write_ptr + cnt) & rb->size_mask;
	rb->write_ptr = tmp;
}

void ringbuffer_get_read_vector(const ringbuffer_t *rb, ringbuffer_data_t *vec)
{
	size_t w = rb->write_ptr;
	size_t r = rb->read_ptr;
	size_t free_cnt;
	if (w > r) {
		free_cnt = w - r;
	} else {
		free_cnt = (w - r + rb->size) & rb->size_mask;
	}
	size_t cnt2 = r + free_cnt;
	if (cnt2 > rb->size) {
		vec[0].buf = &(rb->buf[r]);
		vec[0].len = rb->size - r;
		vec[1].buf = rb->buf;
		vec[1].len = cnt2 & rb->size_mask;

	} else {
		vec[0].buf = &(rb->buf[r]);
		vec[0].len = free_cnt;
		vec[1].len = 0;
	}
}

void ringbuffer_get_write_vector(const ringbuffer_t *rb, ringbuffer_data_t *vec)
{
	size_t w = rb->write_ptr;
	size_t r = rb->read_ptr;
	size_t free_cnt;
	if (w > r) {
		free_cnt = ((r - w + rb->size) & rb->size_mask) - 1;
	} else if (w < r) {
		free_cnt = (r - w) - 1;
	} else {
		free_cnt = rb->size - 1;
	}
	size_t cnt2 = w + free_cnt;
	if (cnt2 > rb->size) {
		vec[0].buf = &(rb->buf[w]);
		vec[0].len = rb->size - w;
		vec[1].buf = rb->buf;
		vec[1].len = cnt2 & rb->size_mask;
	} else {
		vec[0].buf = &(rb->buf[w]);
		vec[0].len = free_cnt;
		vec[1].len = 0;
	}
}

void ringbuffer_print_debug(const ringbuffer_t *r, const char *s)
{
	fprintf(
		stderr,
		"%s: read_ptr=%d,write_ptr=%d,size=%d,size_mask=%d\n",
		s,
		(int)r->read_ptr,
		(int)r->write_ptr,
		(int)r->size,
		(int)r->size_mask);
}

void ringbuffer_read_exactly(ringbuffer_t *rb, char *dest, size_t cnt)
{
	size_t err = ringbuffer_read(rb, dest, cnt);
	if (err != cnt) {
		fprintf(stderr, "%s: error reading ring buffer (%lu != %lu)\n", __func__, err, cnt);
		exit(1);
	}
}

void ringbuffer_write_exactly(ringbuffer_t *rb, const char *src, size_t cnt)
{
	size_t err = ringbuffer_write(rb, src, cnt);
	if (err != cnt) {
		fprintf(stderr, "%s: error writing ring buffer (%lu != %lu)\n", __func__, err, cnt);
		exit(1);
	}
}
