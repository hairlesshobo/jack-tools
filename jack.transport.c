/*****  jack.transport.c - (c) rohan drape, 2006-2008 *****/

#include <stdlib.h>
#include <stdbool.h>
#include <math.h>
#include <time.h>
#include <curses.h>
#include <signal.h>
#include <jack/jack.h>
#include <jack/transport.h>

struct transport {
  bool rolling;
  double time;
  double incr;
  double skip;
  jack_client_t *jk;
};

void finish(int sig)
{
  endwin();
  exit(0);
}

void shutdown(void *PTR)
{
  exit(1);
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

int main(int argc, char **argv)
{
  struct transport t;
  t.rolling = false;
  t.time = 0.0;
  t.incr = 5.0;
  t.skip = 60.0;
  t.jk = jack_client_open("jack.transport", JackNullOption, NULL);
  if(t.jk) {
    jack_on_shutdown(t.jk, shutdown, 0);
    jack_activate(t.jk);
  } else {
    fprintf(stderr, "jack.transport: could not connect to jack.\n");
    exit(1);
  }
  
  signal(SIGINT, finish);
  initscr();
  keypad(stdscr, TRUE);
  nonl();
  halfdelay(1);
  noecho();
  
  while (1) {
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
      if(t.rolling) {
	jack_transport_stop(t.jk);
      } else {
	jack_transport_start(t.jk);
      }
      break;
    case '>':
    case KEY_RIGHT:
      offset(&t, t.incr);
      break;
    case '.':
    case KEY_UP:
      offset(&t, t.skip);
      break;
    case '<':
    case KEY_LEFT:
      offset(&t, -t.incr);
      break;
    case KEY_DOWN:
    case ',':
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
      
    mvaddstr(0, 0, "jack.transport - (c) rohan drape, 2006-2008");
    mvaddch(1, 0, t.rolling ? ACS_RARROW : ACS_BLOCK);
    mvaddtime(1, 4, t.time);
  }
  
  jack_client_close(t.jk);
  endwin();
  return 0;
}
