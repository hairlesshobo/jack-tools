#ifdef HAVE_SYS_INOTIFY_H

#include <sys/inotify.h>

#define INOTIFY_BUFLEN (5*(100 + sizeof(struct inotify_event)))

static void
watch_inotify(struct plumber *p)
{
  int fd = inotify_init();
  if(fd < 0) {
    perror("inotify_init");
    exit(1);
  }
  char buf[INOTIFY_BUFLEN];
  while(1) {
    int i;
    for(i=0; i<p->g; i++) {
      if(inotify_add_watch(fd, p->i[i], IN_MODIFY) < 0) {
        eprintf("Cannot watch '%s': %s\n", p->i[i], strerror(errno));
      }
    }
    eprintf("inotify read\n");
    int len = read(fd, buf, INOTIFY_BUFLEN);
    if(len < 0 && EINTR) {
      continue;
    }
    if(len < 0) {
      perror("reading from inotify fd");
    }
    eprintf("inotify notification received\n");
    SEND_WAKEUP
  }
}

#else

static void
watch_inotify(struct plumber *p)
{
}

#endif

