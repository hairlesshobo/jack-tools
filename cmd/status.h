#ifndef STATUS_H
#define STATUS_H

int printlg(int fdes, FILE** log_file, char* fmt, ...);
void *status_update_procedure(void *PTR);

#endif
