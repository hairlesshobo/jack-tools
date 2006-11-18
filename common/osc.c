/*  osc.c - (c) rohan drape, 2005-2006 */

#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdbool.h>
#include <math.h>

#include "byte-order.h"
#include "failure.h"
#include "osc.h"

void osc_print_packet(const u8 *packet, i32 packet_sz)
{
  i32 i;
  for(i = 0; i < packet_sz; i++) {
    fprintf(stderr, "%3d (%1.1s) ", packet[i], packet + i);
    if(( i + 1)% 4  == 0) {
      fprintf(stderr, "\n");
    }
  }
}

i32 osc_str_bound(i32 n)
{
  return n +(4 -(n % 4));
}

bool osc_is_numerical_type(u8 c)
{
  return c == 'i' || c == 'h' || c == 't' || c == 'f' || c == 'd';
}

bool osc_is_string_type(u8 c)
{
  return c == 's' || c == 'S';
}

bool osc_can_coerce(u8 u, u8 d)
{
  return(( u == d) ||
	 (osc_is_numerical_type(u) && osc_is_numerical_type(d)) ||
	 (osc_is_string_type(u) && osc_is_string_type(d)));
}

void osc_coerce(u8 u, u8 d, osc_data_t *o)
{
  if(u == d) {
    return;
  }
  if(u == 's' && d == 'S') {
    o->s = o->S;
  } else if(u == 'S' && d == 's') {
    o->S = o->s;
  } else if(u == 'i' && d == 'h') {
    o->i = (i32) o->h;
  } else if(u == 'i' && d == 't') {
    o->i = (i32) o->t;
  } else if(u == 'i' && d == 'f') {
    o->i = (i32) floorf(o->f);
  } else if(u == 'i' && d == 'd') {
    o->i = (i32) floor(o->d);
  } else if(u == 'h' && d == 'i') {
    o->h = (i64) o->i;
  } else if(u == 'h' && d == 't') {
    o->h = (i64) o->t;
  } else if(u == 'h' && d == 'f') {
    o->h = (i64) floorf(o->f);
  } else if(u == 'h' && d == 'd') {
    o->h = (i64) floor(o->d);
  } else if(u == 't' && d == 'i') {
    o->t = (u64) o->i;
  } else if(u == 't' && d == 'h') {
    o->t = (u64) o->t;
  } else if(u == 't' && d == 'f') {
    o->t = (u64) floorf(o->f);
  } else if(u == 't' && d == 'd') {
    o->t = (u64) floor(o->d);
  } else if(u == 'f' && d == 'i') {
    o->f = (f32) o->i;
  } else if(u == 'f' && d == 'h') {
    o->f = (f32) o->h;
  } else if(u == 'f' && d == 't') {
    o->f = (f32) o->t;
  } else if(u == 'f' && d == 'd') {
    o->f = (f32) o->d;
  } else if(u == 'd' && d == 'i') {
    o->d = (f64) o->i;
  } else if(u == 'd' && d == 'h') {
    o->d = (f64) o->h;
  } else if(u == 'd' && d == 't') {
    o->d = (f64) o->t;
  } else if(u == 'd' && d == 'f') {
    o->d = (f64) o->d;
  } else {
    fprintf(stderr, "osc_coerce: illegal coercion '%c' <- '%c'\n", u, d);
    FAILURE;
  }
}

void osc_coerce_arguments(const char *u_dsc, const char *p_dsc, osc_data_t *data)
{
  u_dsc++;
  p_dsc++;
  while(*u_dsc != '\0') {
    osc_coerce(*u_dsc++, *p_dsc++, data++);
  }
}

i32 u8strlen(const u8* b)
{
  return strlen((const char*)b);
}

i32 u8strncmp(const u8 *a, const u8 *b, i32 n)
{
  return strncmp((const char*)a, (const char*)b, n);
}

i32 osc_collect_arguments(const char *dsc, const u8 *p, osc_data_t *data)
{
  dsc++;
  i32 i = 0;
  while(*dsc != '\0') {
    u8 c = *dsc;
    if(c == 'i') {
      data[i].i = ntoh_i32_from_buf(p);
      p += 4;
    } else if(c == 'h') {
      data[i].h = ntoh_i64_from_buf(p);
      p += 8;
    } else if(c == 't') {
      data[i].t = ntoh_u64_from_buf(p);
      p += 8;
    } else if(c == 'f') {
      data[i].f = ntoh_f32_from_buf(p);
      p += 4; 
    } else if(c == 'd') {
      data[i].d = ntoh_f64_from_buf(p);
      p += 8; 
    } else if(c == 's') {
      data[i].s = (char*)p;
      p += osc_str_bound(u8strlen(p));
    } else if(c == 'S') {
      data[i].S = (char*)p;
      p += osc_str_bound(u8strlen(p));
    } else if(c == 'c') {
      data[i].c = ntoh_u32_from_buf(p);
      p += 4; 
    } else if(c == 'm') {
      data[i].m = ntoh_u32_from_buf(p);
      p += 4; 
    } else if(c == 'r') {
      data[i].r = ntoh_u32_from_buf(p);
      p += 4; 
    } else if(c == 'b') {
      data[i].b.data = p + 4;
      data[i].b.size = ntoh_i32_from_buf(p);
      p += data[i].b.size + 4;
    } else {
      return -1;
    }
    dsc++;
    i++;
  }
  return 0;
}

i32 osc_pack_arguments(const char *dsc, const osc_data_t *data, u8 *p)
{
  dsc++;
  i32 i = 0;
  while(*dsc != '\0') {
    u8 c = *dsc;
    if(c == 'i') {
      ntoh_i32_to_buf(p, data[i].i);
      p += 4;
    } else if(c == 'h') {
      ntoh_i64_to_buf(p, data[i].h);
      p += 8;
    } else if(c == 't') {
      ntoh_u64_to_buf(p, data[i].t);
      p += 8;
    } else if(c == 'f') {
      ntoh_f32_to_buf(p, data[i].f);
      p += 4; 
    } else if(c == 'd') {
      ntoh_f64_to_buf(p, data[i].d);
      p += 8; 
    } else if(c == 's') {
      i32 n = strlen(data[i].s);
      memcpy(p, data[i].s, n+1);
      p += osc_str_bound(n);
    } else if(c == 'S') {
      i32 n = strlen(data[i].s);
      memcpy(p, data[i].S, n+1);
      p += osc_str_bound(n);
    } else if(c == 'c') {
      ntoh_u32_to_buf(p, data[i].c);
      p += 4; 
    } else if(c == 'm') {
      ntoh_u32_to_buf(p, data[i].m);
      p += 4; 
    } else if(c == 'r') {
      ntoh_u32_to_buf(p, data[i].r);
      p += 4; 
    } else if(c == 'b') {
      ntoh_i32_to_buf(p, data[i].b.size);
      memcpy(p + 4, data[i].b.data, data[i].b.size);
      p += osc_str_bound(data[i].b.size)+ 4;
    } else {
      return -1;
    }
    dsc++;
    i++;
  }
  return 0;
}

i32 osc_dsc_read_arg_len(const char *dsc, const u8 *p)
{
  if(*dsc++ != ',') {
    fprintf(stderr, "osc_dsc_read_arg_len: illegal descriptor '%s'\n", dsc);
    return -1;
  }
  i32 n = 0;
  while(*dsc != '\0') {
    u8 c = *dsc;
    if(c == 'i' || c == 'f' || c == 'c' || c == 'm' || c == 'r') {
      n += 4; 
    } else if(c == 'd' || c == 'h' || c == 't') {
      n += 8;
    } else if(c == 's' || c == 'S') {
      n += osc_str_bound(u8strlen(p + n));
    } else if(c == 'b') {
      n += osc_str_bound(ntoh_i32_from_buf(p + n)) + 4;
    } else {
      return -1;
    }
    dsc++;
  }
  return n;
}

i32
osc_dsc_calculate_arg_len(const char *dsc, const osc_data_t *data)
{
  if(*dsc++ != ',') {
    fprintf(stderr, "osc_dsc_calculate_arg_len: illegal descriptor '%s'\n", dsc);
    return -1;
  }
  i32 n = 0;
  while(*dsc != '\0') {
    u8 c = *dsc;
    if(c == 'i' || c == 'f' || c == 'c' || c == 'm' || c == 'r') {
      n += 4; 
    } else if(c == 'd' || c == 'h' || c == 't') {
      n += 8;
    } else if(c == 's') {
      n += osc_str_bound(strlen(data->s));
    } else if(c == 'S') {
      n += osc_str_bound(strlen(data->S));
    } else if(c == 'b') {
      n += osc_str_bound(data->b.size)+ 4;
    } else {
      return -1;
    }
    dsc++;
    data++;
  }
  return n;
}

i32 osc_match_address(const char *addr, const u8 *packet)
{
  size_t addr_n = strlen(addr);
  if(strncmp(addr, (char*)packet, addr_n)!= 0) {
    return 0;
  } else {
    return osc_str_bound(addr_n);
  }
}

i32 osc_match_dsc(const char *u_dsc, const char *p_dsc)
{
  size_t u_dsc_n = strlen(u_dsc);
  if(*u_dsc++ != ',' || *p_dsc++ != ',') {
    return 0;
  }
  while(*u_dsc != '\0') {
    if(! osc_can_coerce(*u_dsc++, *p_dsc++)) {
      return 0;
    }
  }
  return osc_str_bound(u_dsc_n);
}

i32 osc_parse_message(const char *addr, const char *dsc, const u8 *packet, i32 packet_sz, osc_data_t *data)
{
  i32 addr_len = osc_match_address(addr, packet);
  if(! addr_len) {
    return 0;
  }
  const char *p_dsc = (char*)packet + addr_len;
  i32 dsc_len = osc_match_dsc(dsc, p_dsc);
  if(! dsc_len) {
    return 0;
  }
  const u8 *p_arg = (u8*)p_dsc + dsc_len;
  i32 arg_len = osc_dsc_read_arg_len(p_dsc, p_arg);
  if(packet_sz < addr_len + dsc_len + arg_len) {
    return 0;
  }
  i32 err = osc_collect_arguments(p_dsc, p_arg, data);
  if(err) {
    return 0;
  }
  osc_coerce_arguments(dsc, p_dsc, data);
  return addr_len + dsc_len + arg_len;
}

i32 osc_construct_message(const char *addr, const char *dsc, const osc_data_t *data, u8 *packet, i32 packet_sz)
{
  i32 addr_n = strlen(addr);
  i32 addr_len = osc_str_bound(addr_n);
  i32 dsc_n = strlen(dsc);
  i32 dsc_len = osc_str_bound(dsc_n);
  i32 arg_len = osc_dsc_calculate_arg_len(dsc, data);
  memset(packet, 0, addr_len + dsc_len + arg_len);
  memcpy(packet, addr, addr_n);
  memcpy(packet + addr_len, dsc, dsc_n);
  if(packet_sz < addr_len + dsc_len + arg_len) {
    return 0;
  }
  i32 err = osc_pack_arguments(dsc, data, packet + addr_len + dsc_len);
  if(err) {
    return 0;
  }
  return addr_len + dsc_len + arg_len;
}

i32 osc_build_message(u8 *packet, i32 packet_sz,
		      const char *addr, const char *dsc, ...)
{
  osc_data_t data[256];
  
  va_list ap;
  va_start(ap, dsc);

  i32 index;
  index = 0;

  while(dsc && dsc[index+1]) {
    switch(dsc[index+1]) {
    case 'i':
      data[index].i = va_arg(ap, i32);
      break;
    case 'h':
      data[index].h = va_arg(ap, i64);
      break;
    case 't':
      data[index].t = va_arg(ap, u64);
      break;
    case 'f':
      data[index].f = va_arg(ap, f64);
      break;
    case 'd':
      data[index].d = va_arg(ap, f64);
      break;
    case 's':
      data[index].s = va_arg(ap, char*);
      break;
    case 'S':
      data[index].S = va_arg(ap, char*);
      break;
    case 'c':
      data[index].c = va_arg(ap, u32);
      break;
    case 'm':
      data[index].m = va_arg(ap, u32);
      break;
    case 'r':
      data[index].r = va_arg(ap, u32);
      break;
    case 'b':
      data[index].b = va_arg(ap, osc_blob_t);
      break;
    default:
      return 0;
    }
    index++;
  }
  va_end(ap);
  
  return osc_construct_message(addr, dsc, data, packet, packet_sz);
}
