#ifndef STATUS_UTILS_H
#define STATUS_UTILS_H

const char *format_size(uint64_t bytes);
const char *format_duration(float duration);
short amp_to_db(float x);
int abort_or_alert_when(struct recorder *recorder_obj, int condition, char* fmt, ...);
void clear_peaks(struct recorder *recorder_obj);
void clear_max(struct recorder *recorder_obj);
uint64_t get_time_millis(void);

#endif
