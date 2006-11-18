#ifndef _COMMON_XIMG_H
#define _COMMON_XIMG_H

#include <X11/Intrinsic.h>
#include <X11/extensions/Xdbe.h>
#include <X11/Xatom.h>
#include <X11/keysym.h>

typedef struct {
  Display *d ;
  Window w ;
  Visual *v ;
  XImage *image ;
  GC draw ;
  XdbeBackBuffer b ;
  XdbeSwapInfo swap ;
  uint32_t width ;
  uint32_t height ;
} Ximg_t ;

Ximg_t *ximg_open(int width, int height, char *name);
void ximg_blit(Ximg_t *x, uint8_t *data);
void ximg_mouse(Ximg_t *x, int *dx, int *dy, int *b);
void ximg_close(Ximg_t *x);

#endif
