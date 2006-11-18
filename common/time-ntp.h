#ifndef _COMMON_TIME_NTP_H
#define _COMMON_TIME_NTP_H

u64 utc_timeval_to_ntp(struct timeval tv);
u64 utc_real_to_ntp(f64 utc);
u32 utc_usec_to_ntp_approx(u32 usec);
u64 utc_timeval_to_ntp_approx(struct timeval tv);

#endif
