#include <math.h> /* C99 */
#include <signal.h>
#include <stdbool.h>
#include <stdlib.h>
#include <time.h>

#include <unistd.h> /* POSIX */

#include <curses.h>

#include <jack/jack.h>
#include <jack/transport.h>

#include "c-common/jack-client.h"

struct transport {
  bool verbose;
  bool rolling;
  double time;
  double incr;
  double skip;
  jack_client_t *jk;
};

void finish(int sig)
{
  fprintf(stderr, "rju-transport: finish\n");
  endwin();
  exit(0);
}

void getcmd(const char *prompt, char *result, int n)
{
  move(2,0);
  addstr(prompt);
  echo();
  getnstr(result, n);
  noecho();
  halfdelay(1);
  move(2,0);
  clrtoeol();
}

double getnum(const char *prompt)
{
  char tmp[512];
  getcmd(prompt, tmp, 512);
  return strtod(tmp, NULL);
}

void mvaddtime(int x, int y, double t)
{
  char tmp[512];
  time_t a = floor(t);
  struct tm *b = gmtime(&a);
  strftime(tmp, 512, "%H:%M:%S", b);
  mvaddstr(x, y, tmp);
}

void locate(struct transport *t)
{
  jack_nframes_t frame = t->time * jack_get_sample_rate(t->jk);
  jack_transport_locate(t->jk, frame);
}

void offset(struct transport *t, double o)
{
  t->time += o;
  if(t->time < 0.0) {
    t->time = 0.0;
  }
  locate(t);
}

void usage(void)
{
  printf("Usage: rju-transport [options]\n");
  printf("   -h: print usage\n");
  exit(0);
}

int main(int argc, char **argv)
{
  struct transport t;
  int c;

  t.verbose = false;
  t.rolling = false;
  t.time = 0.0;
  t.incr = 5.0;
  t.skip = 60.0;

  while((c = getopt(argc, argv, "hv")) != -1) {
    switch(c) {
    case 'h': usage(); break;
    case 'v': t.verbose = true; break;
    }
  }

  t.jk = jack_client_open("rju-transport", JackNullOption, NULL);
  if(t.jk) {
    jack_set_error_function(jack_client_minimal_error_handler);
    jack_on_shutdown(t.jk, jack_client_minimal_shutdown_handler, 0);
    jack_activate(t.jk);
  } else {
    fprintf(stderr, "rju-transport: could not connect to jack\n");
    exit(1);
  }

  if(t.verbose) fprintf(stderr, "rju-transport: init curses\n");
  signal(SIGINT, finish);
  initscr();
  keypad(stdscr, TRUE);
  nonl();
  halfdelay(1);
  noecho();

  while (true) {
    int c;
    jack_transport_state_t s;
    jack_position_t p;

    s = jack_transport_query(t.jk , &p);
    t.rolling = s & JackTransportRolling;
    t.time = (double)p.frame / (double)p.frame_rate;

    c = getch();
    switch(c) {
    case 'r':
      erase();
      refresh();
      break;
    case ' ':
    case 's':
      if(t.rolling) {
	jack_transport_stop(t.jk);
      } else {
	jack_transport_start(t.jk);
      }
      break;
    case 'f':
    case '>':
    case KEY_RIGHT:
      offset(&t, t.incr);
      break;
    case 'F':
    case '.':
    case KEY_UP:
      offset(&t, t.skip);
      break;
    case 'b':
    case '<':
    case KEY_LEFT:
      offset(&t, -t.incr);
      break;
    case 'B':
    case ',':
    case KEY_DOWN:
      offset(&t, -t.skip);
      break;
    case 'z':
    case KEY_SLEFT:
      t.time = 0.0;
      locate(&t);
      break;
    case 'i':
      t.incr = getnum("Set increment: ");
      break;
      /* A little bit clever, start typing a number to enter locate mode... */
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      ungetch(c);
    case 'l':
      t.time = getnum("Locate to: ");
      locate(&t);
      break;
    case 'q':
      finish(SIGINT);
      break;
    case ERR:
      break;
    }

    mvaddstr(0, 0, "rju-transport");
    mvaddstr(2, 0, "[s]tart,[s]top,[f]orward,[b]ack,[l]ocate,[i]ncrement,[z]ero,[q]uit");
    mvaddch(1, 0, t.rolling ? ACS_RARROW : ACS_BLOCK);
    mvaddtime(1, 6, t.time);
  }

  jack_client_close(t.jk);
  endwin();
  return 0;
}
