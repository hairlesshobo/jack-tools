/*  ximg.c - (c) rohan drape, 2005-2006 */

#include <stdio.h>
#include <stdlib.h>

#include "failure.h"
#include "int.h"
#include "memory.h"
#include "ximg.h"

static int 
ximg_handle_error(Display *d, XErrorEvent *e)
{
  char message[128];
  XGetErrorText(d, e->error_code, message, 128);
  fprintf(stderr, "X error: %s\n", message);
  FAILURE;
  return 1;
}

static int 
ximg_handle_io_error(Display *d)
{
  fprintf(stderr, "X IO error\n");
  FAILURE;
  return 1;
}

Ximg_t *
ximg_open(int width, int height, char *name)
{
  Ximg_t *x = xmalloc(sizeof(Ximg_t));
  x->width = width;
  x->height = height;
  x->d = XOpenDisplay(NULL);
  XSetErrorHandler(ximg_handle_error);
  XSetIOErrorHandler(ximg_handle_io_error);
  Window r = DefaultRootWindow(x->d );
  x->v = DefaultVisual(x->d, DefaultScreen(x->d));
  XGCValues draw;
  draw.background = WhitePixel(x->d, DefaultScreen( x->d));
  draw.foreground = BlackPixel(x->d, DefaultScreen( x->d));
  x->draw = XCreateGC(x->d, r, GCForeground | GCBackground, &draw );
  x->w = XCreateSimpleWindow(x->d, r,
			       0, 0, x->width, x->height,
			       0, BlackPixel(x->d, DefaultScreen( x->d)),
			       WhitePixel(x->d, DefaultScreen( x->d)));
  XSetStandardProperties(x->d, x->w, name, name, None, NULL, 0, NULL);
  XSizeHints h;
  h.width = h.min_width = h.max_width = h.base_width = width;
  h.height = h.min_height = h.max_height = h.base_height = height;
  h.win_gravity = CenterGravity;
  h.flags = PSize | PMinSize | PMaxSize | PBaseSize | PWinGravity;
  XSetWMNormalHints(x->d, x->w, &h);
  x->b = XdbeAllocateBackBufferName(x->d, x->w, XdbeUndefined);
  x->swap.swap_window = x->w;
  x->swap.swap_action = XdbeUndefined;
  if(x->v->class != TrueColor){
    fprintf(stderr, "ximg_open(): Non true color visual");
    FAILURE;
  }
  x->image = XCreateImage(x->d, x->v, 
			    DisplayPlanes(x->d, DefaultScreen( x->d)), 
			    ZPixmap, 0, NULL, 
			    x->width, x->height, BitmapPad(x->d), 0);  
  if(! x->image){
    fprintf(stderr, "XCreateImage() failed");  
    FAILURE;
  }
  x->image->data = xmalloc(x->height * x->image->bytes_per_line);
  XPutImage(x->d, x->w, x->draw, x->image, 0, 0, 0, 0, x->width, x->height);
  XFlush(x->d);
  XMapWindow(x->d, x->w);
  return x;
}

/* Draw the pixel-map at data.  `data' is a set of RGB triples, each
   one byte, in row major order.  The pixmap is the same size as the
   window.  Modified from work by John Bradley in xv(1).  */

static inline int 
highbit(u32 ul)
{
  i32 i;
  u32 hb = 0x80 << 24;   /* hb = 0x80000000UL */
  for(i = 31; ((ul & hb) == 0) && i >= 0;  i--, ul <<= 1);
  return i;
}

void 
ximg_blit(Ximg_t *x, u8 *data)
{
  u32 rmask = x->v->red_mask;
  u32 gmask = x->v->green_mask;
  u32 bmask = x->v->blue_mask;
  int rshift = 7 - highbit(rmask);
  int gshift = 7 - highbit(gmask);
  int bshift = 7 - highbit(bmask);
  int maplen = x->v->map_entries;
  if (maplen>256) {
    maplen=256;
  }
  int bperline = x->image->bytes_per_line;
  int bperpix  = x->image->bits_per_pixel;
  int border   = x->image->byte_order;
  if (bperpix != 8 && bperpix != 16 && bperpix != 24 && bperpix != 32) {
    fprintf(stderr, "ximg_blit() : domain error : bperpix");
    abort ();
  }
  u8 *lip = (u8*)x->image->data;  
  u8 *pp = data;
  int i;
  for (i=0; i<x->height; i++, lip+=bperline) {
    int j;
    u8 *ip = lip;
    for (j=0; j<x->width; j++) {
      u32 r = *pp++;  
      u32 g = *pp++;  
      u32 b = *pp++;
      r = (rshift<0) ? r << (-rshift) : r >> rshift;
      g = (gshift<0) ? g << (-gshift) : g >> gshift;
      b = (bshift<0) ? b << (-bshift) : b >> bshift;
      r = r & rmask;
      g = g & gmask;
      b = b & bmask;
      u32 xcol = r | g | b;
      if (bperpix == 32) {
	if (border == MSBFirst) {
	  *ip++ = (xcol>>24) & 0xff;
	  *ip++ = (xcol>>16) & 0xff;
	  *ip++ = (xcol>>8)  & 0xff;
	  *ip++ =  xcol      & 0xff;
	}
	else {
	  *ip++ =  xcol      & 0xff;
	  *ip++ = (xcol>>8)  & 0xff;
	  *ip++ = (xcol>>16) & 0xff;
	  *ip++ = (xcol>>24) & 0xff;
	}
      }
      else if (bperpix == 24) {
	if (border == MSBFirst) {
	  *ip++ = (xcol>>16) & 0xff;
	  *ip++ = (xcol>>8)  & 0xff;
	  *ip++ =  xcol      & 0xff;
	}
	else {
	  *ip++ =  xcol      & 0xff;
	  *ip++ = (xcol>>8)  & 0xff;
	  *ip++ = (xcol>>16) & 0xff;
	}
      }
      else if (bperpix == 16) {
	if (border == MSBFirst) {
	  *ip++ = (xcol>>8)  & 0xff;
	  *ip++ =  xcol      & 0xff;
	}
	else {
	  *ip++ =  xcol      & 0xff;
	  *ip++ = (xcol>>8)  & 0xff;
	}
      }
      else if (bperpix == 8) {
	*ip++ =  xcol      & 0xff;
      }
    }
  }
  XdbeBeginIdiom( x->d);
  XPutImage(x->d, x->b, x->draw, x->image, 0, 0, 0, 0, x->width, x->height);
  XdbeSwapBuffers(x->d, &(x->swap), 1);
  XSync(x->d, 0);
  XdbeEndIdiom(x->d);
}

void
ximg_mouse(Ximg_t *x, int *dx, int *dy, int *b) 
{
  int rootx, rooty;
  Window root, child;
  unsigned int mask;
  XQueryPointer(x->d, x->w, &root, &child, &rootx, &rooty, dx, dy, &mask);
  *b = (int)mask;
}

void 
ximg_close(Ximg_t *x)
{
  XdbeDeallocateBackBufferName(x->d, x->b);
  XFreeGC(x->d, x->draw);
  XDestroyImage(x->image);
  XFlush(x->d);
  XSync(x->d, 0);
  XDestroyWindow(x->d, x->w);
  XCloseDisplay(x->d);
  free(x);
}
