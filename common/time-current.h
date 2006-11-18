#ifndef _COMMON_TIME_CURRENT_H
#define _COMMON_TIME_CURRENT_H

#include <sys/time.h>
#include "float.h"

f64 current_time_as_utc_real(void);
struct timeval current_time_as_utc_timeval(void);

#endif
