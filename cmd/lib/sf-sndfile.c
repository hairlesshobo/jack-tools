#include <stdlib.h>
#include <string.h>

#include "failure.h"
#include "memory.h"
#include "sf-sndfile.h"

SNDFILE *xsf_open(const char *path, int mode, SF_INFO *sfinfo)
{
	SNDFILE *s = sf_open(path, mode, sfinfo);
	if (!s) {
		fprintf(stderr, "sf_open() failed\n");
		sf_perror(s);
		FAILURE;
	}
	return s;
}

void xsf_handle_error(SNDFILE *sndfile)
{
	int errnum = sf_error(sndfile);
	const char *errstr = sf_error_number(errnum);
	fprintf(stderr, "libsndfile failed: %s\n", errstr);
	FAILURE;
}

sf_count_t xsf_read_float(SNDFILE *sndfile, float *ptr, sf_count_t items)
{
	sf_count_t err = sf_read_float(sndfile, ptr, items);
	if (err < 0) {
		xsf_handle_error(sndfile);
	}
	return err;
}

sf_count_t xsf_write_float(SNDFILE *sndfile, float *ptr, sf_count_t items)
{
	sf_count_t err = sf_write_float(sndfile, ptr, items);
	if (err < 0) {
		xsf_handle_error(sndfile);
	}
	return err;
}

/* Read the sound file at `name' to a newly allocated array and store
   the size (in samples) at `n'. */

float *xsf_read_signal_file(const char *name, int nc, int *n)
{
	SF_INFO sfi;
	SNDFILE *sfp = xsf_open(name, SFM_READ, &sfi);
	if (sfi.channels != nc) {
		fprintf(stderr, "illegal channel count: %d\n", sfi.channels);
		FAILURE;
	}
	*n = sfi.frames * sfi.channels;
	float *data = (float *)xmalloc((size_t)*n * sizeof(float));
	int err = sf_read_float(sfp, data, *n);
	if (err == -1) {
		fprintf(stderr, "sf_read_float() failed\n");
		sf_perror(sfp);
		FAILURE;
	}
	return data;
}

/* Write a NeXT F32 sound file */

bool xsf_write_au_f32(const char *name, int nc, int nf, int sr, const float *data)
{
	SF_INFO sfi;
	xmemset(&sfi, 0, sizeof(sfi));
	sfi.channels = nc;
	sfi.samplerate = sr;
	sfi.frames = nf;
	sfi.format = (SF_FORMAT_AU | SF_FORMAT_FLOAT);
	if (!sf_format_check(&sfi)) {
		fprintf(stderr, "sf_format_check() failed\n");
		return false;
	}
	SNDFILE *sfp = xsf_open(name, SFM_WRITE, &sfi);
	int err = sf_write_float(sfp, data, nf * nc);
	if (err == -1) {
		fprintf(stderr, "sf_write_float() failed\n");
		sf_perror(sfp);
		return false;
	}
	sf_close(sfp);
	return true;
}
