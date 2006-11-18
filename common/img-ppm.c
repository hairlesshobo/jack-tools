/*  img-ppm.c - (c) rohan drape, 2005-2006 */

#include <stdio.h>
#include <string.h>
#include <math.h>

#include "failure.h"
#include "memory.h"
#include "int.h"
#include "img-ppm.h"

i32 img_write_ppm(u8 *data, i32 w, i32 h, FILE *fp)
{
  fprintf(fp, "P6 %d %d 255\n", w, h);
  fwrite(data, w * h, 3, fp);
  return 0;
}

i32 img_write_ppm_file(u8 *data, i32 w, i32 h, const char *name)
{
  FILE *fp = fopen(name, "w");
  if(!fp){
    return -1;
  }
  img_write_ppm(data, w, h, fp);
  fclose(fp);
  return 0; 
}

u8 *img_read_ppm(i32 *w, i32 *h, FILE *fp)
{
  i32 s_err = fscanf(fp, "P6 %d %d %*d%*c", w, h);
  if(s_err != 4){
    fprintf(stderr, "img_read_ppm(): non P6 ppm file (err=%d)\n", s_err);
    FAILURE;
  }
  size_t n = *w * *h;
  u8 *data = xmalloc(n);
  size_t r_err = fread(data, 3, n, fp);
  if(r_err != n){
    fprintf(stderr, "img_read_ppm(): fread() failed");
    FAILURE;
  }
  return data;
}

u8 *img_read_ppm_file(i32 *w, i32 *h, const char *name)
{
  FILE *fp = fopen(name, "r");
  if(!fp){
    return NULL;
  }
  u8 *data = img_read_ppm(w, h, fp);
  fclose(fp);
  return data;
}
