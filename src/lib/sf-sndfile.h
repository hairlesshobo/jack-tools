#ifndef _COMMON_SF_SNDFILE_H
#define _COMMON_SF_SNDFILE_H

#include <sndfile.h>
#include <stdbool.h>

SNDFILE *xsf_open(const char *path, int mode, SF_INFO *sfinfo);
void xsf_handle_error(SNDFILE *sndfile);
sf_count_t xsf_read_float(SNDFILE *sndfile, float *ptr, sf_count_t items);
sf_count_t xsf_write_float(SNDFILE *sndfile, float *ptr, sf_count_t items);
float *xsf_read_signal_file(const char *name, int nc, int *n);
bool xsf_write_au_f32(const char *name, int nc, int nf, int sr, const float *data);

#endif
