#include <ctype.h> /* ISO C99 */
#include <errno.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <regex.h> /* POSIX */
#include <pthread.h>
#include <unistd.h>

#include "c-common/file.h"
#include "c-common/int.h"
#include "c-common/jack-client.h"
#include "c-common/jack-port.h"
#include "c-common/print.h"
#include "c-common/xregcomp.h"
#include "c-common/time-timespec.h"

#define MAX_SETS      8
#define MAX_RULES     512
#define MAX_STR       512
#define MAX_SUBEXP    4
#define DEFAULT_DELAY 1000     /* μs 1,000μs = 0.001s */
#define SYS_RULESET   "/etc/rju-plumbing"

enum action {
  ignore,
  disconnect,
  connect,
};

struct rule
{
  enum action command;
  char left[MAX_STR];
  regex_t left_c;
  char right[MAX_STR];
};

struct plumber
{
  struct rule r[MAX_RULES];     /* Rule set */
  int n;                        /* Number of rules at r */
  char i[MAX_SETS][MAX_STR];    /* Rule set filenames */
  int g;                        /* Number of rule set files at i */
  jack_client_t *j;             /* jack client structure */
  pthread_t t;                  /* plumbing thread */
  pthread_mutex_t lock;
  pthread_cond_t cond;
  int w;                        /* Do not send wakeup unless TRUE */
  time_t m;                     /* Time that the rule set was last modified. */
  u64 u;                        /* Number of usecs to defer for connections. */
  int d;                        /* Run as daeomon. */
  int o;                        /* Include ordinary rule files. */
  int q;                        /* Quiet operation. */
};

/* Parse a plumbing command name. */
static enum action
parse_command(const char *s)
{
  if(strcmp(s, "connect") == 0) {
    return connect;
  } else if(strcmp(s, "disconnect") == 0) {
    return disconnect;
  } else {
    eprintf("%s: unknown rule: %s\n", __func__, s);
    return ignore;
  }
}

/* Add a rule to the rule set, pre-compile LHS. */
static void
add_rule_to_set(struct plumber *p, enum action command, const char *left, const char *right)
{
  vprintf(p->q == 0, "Add rule: '%d', '%s' - '%s'\n", command, left, right);
  p->r[p->n].command = command;
  snprintf(p->r[p->n].left, MAX_STR, "^%.*s$", MAX_STR - 3, left);
  xregcomp(&(p->r[p->n].left_c), p->r[p->n].left, REG_EXTENDED);
  snprintf(p->r[p->n].right, MAX_STR, "^%.*s$", MAX_STR - 3, right);
  p->n += 1;
}

/* Clear all rules, free pre-compiled LHS. */
static void
clear_rule_set(struct plumber *p)
{
  int i;
  for(i = 0; i < p->n; i++) {
    regfree(&(p->r[i].left_c));
  }
  p->n = 0;
}

/* Parse the rule at s.  This is a pretend parser... */
static void
acquire_rule_string(struct plumber *p, const char *s)
{
  if(s[0] == ';' || s[0] == '\0') {
    return;
  }
  if(p->n >= MAX_RULES) {
    vprintf(p->q == 0, "Rule lost, too many rules: '%s'\n", s);
    return;
  }
  char s_command[MAX_STR], s_left[MAX_STR], s_right[MAX_STR];
  int err = sscanf(s, "(%s \"%[^\"]\" \"%[^\"]\")", s_command, s_left, s_right);
  if(err != 3) {
    vprintf(p->q == 0, "Rule lost, scan failed: '%s'\n", s);
    return;
  }
  vprintf(p->q == 0, "Rule accepted: '%s', '%s' - '%s'\n", s_command, s_left, s_right);
  add_rule_to_set(p, parse_command(s_command), s_left, s_right);
}

/* Read rules from file */
static void
acquire_rule_file(struct plumber *p, const char *f)
{
  FILE *fp = fopen(f, "r");
  if(!fp) {
    vprintf(p->q == 0, "Rule file inaccessible: '%s'\n", f);
    return;
  }
  char s[MAX_STR];
  while(fgets(s, MAX_STR, fp)) {
    s[strlen(s)-1] = '\0';
    acquire_rule_string(p, s);
  }
  fclose(fp);
  return;
}

/* Append `f' to the list of known rule files. */
static void
append_rule_file(struct plumber *p, char *f)
{
  if(p->g >= MAX_SETS) {
    vprintf(p->q == 0, "Rule file discarded, too many rule files: '%s', %d\n", f, p->g);
    return;
  }
  strncpy(p->i[p->g], f, MAX_STR);
  vprintf(p->q == 0, "Append rule file: '%s'\n", p->i[p->g]);
  p->g += 1;
  p->m = 0;
}

/* Load the `n' rule files at `f'. */
static void
append_rule_file_list(struct plumber *p, char **f, int n)
{
  int i;
  for(i = 0; i < n; i++) {
    append_rule_file(p, f[i]);
  }
}

/* Load the system wide and user specific rule files. */
static void
append_ordinary_rule_files(struct plumber *p)
{
  append_rule_file(p, SYS_RULESET);
  char usr[MAX_STR];
  snprintf(usr, MAX_STR, "%s/.rju-plumbing", getenv("HOME"));
  append_rule_file(p, usr);
}

/* Returns TRUE iff the rule file `f' is modified after `m'. */
static bool
rule_file_is_modified_p(struct plumber *p, const char *f, time_t m)
{
  if(!file_exists_p(f)) {
    vprintf(p->q == 0, "Rule file does not exist: '%s'\n",  f);
    return false;
  }
  time_t mtime = stat_mtime(f);
  if(mtime <= m) {
    dprintf("Rule file not modified: '%s'", f);
    return false;
  }
  return true;
}

/* Returns TRUE if any of the rule files at `p' are modified. */
static bool
any_rule_file_modified_p(struct plumber *p)
{
  int i;
  for(i = 0; i < p->g; i++) {
    if(rule_file_is_modified_p(p, p->i[i], p->m)) {
      return true;
    }
  }
  return false;
}

/* Consult all rule files.  If any is modified clear the rule set and re-read all files. */
static void
acquire_rule_set(struct plumber *p)
{
  if(!any_rule_file_modified_p(p)) {
    return;
  }
  clear_rule_set(p);
  int i;
  for(i = 0; i < p->g; i++) {
    acquire_rule_file(p, p->i[i]);
  }
  p->m = time(NULL);
}

/* If the LHS matches return TRUE and write subexp. */
static bool
left_applies_p(regex_t *l, const char *p_l, regmatch_t *subexp)
{
  return regexec(l, p_l, MAX_SUBEXP, subexp, 0) == 0;
}

/* Make the right hand side (rhs) regular expression by replacing the
   escape sequence '\1' at `right' with the submatch at `left'
   indicated by `a' and `b'.  */
static void
make_rhs(const char *left, int a, int b, const char *right, char *rhs)
{
  char *replace_p = strchr(right, '\\');
  if(!replace_p) {
    strcpy (rhs, right);
    return;
  }
  int copy_n = replace_p - right;
  int after_n = strlen(right) - copy_n - 2;
  int insert_n = (b - a);
  memcpy(rhs, right, copy_n);
  memcpy(rhs + copy_n, left + a, insert_n);
  memcpy(rhs + copy_n + insert_n, right + copy_n + 2, after_n);
  rhs[copy_n + insert_n + after_n] = '\0';
}

/* If the RHS matches return TRUE. */
static bool
right_applies_p(const char *p_l, const char *r, const char *p_r, regmatch_t *subexp)
{
  char rhs[MAX_STR];
  if(subexp[1].rm_so >= 0) {
    make_rhs(p_l, subexp[1].rm_so, subexp[1].rm_eo, r, rhs);
  } else {
    strcpy(rhs, r);
  }
  regex_t rr;
  xregcomp(&rr, rhs, REG_NOSUB | REG_EXTENDED);
  int err = regexec(&rr, p_r, 0, NULL, 0);
  regfree(&rr);
  return  err == 0;
}

/* The traversal macros bind the values left and right over body.
   TRAVERSE_CONNECTIONS binds for every existing connection that matches the rule.
   TRAVERSE_LISTS binds for all possible connections that match the rule.  */
#define TRAVERSE_CONNECTIONS(p,r,p_left,body)                           \
  int i;                                                                \
  regmatch_t subexp[MAX_SUBEXP];                                        \
  for(i = 0; p_left[i]; i++) {                                          \
    if(left_applies_p(&(r->left_c), p_left[i], subexp)) {               \
      jack_port_t *port = jack_port_by_name(p->j, p_left[i]);           \
      const char **c = jack_port_get_all_connections(p->j, port);       \
      if(c) {                                                           \
        int j;                                                          \
        for(j = 0; c[j]; j++) {                                         \
          if(right_applies_p(p_left[i], r->right, c[j], subexp)) {      \
            const char *left = p_left[i];                               \
            const char *right = c[j];                                   \
            body;                                                       \
          }                                                             \
        }                                                               \
        free(c);                                                        \
      }                                                                 \
    }                                                                   \
  }

#define TRAVERSE_LISTS(r,p_left,p_right,body)                           \
  int i;                                                                \
  regmatch_t subexp[MAX_SUBEXP];                                        \
  for(i = 0; p_left[i]; i++) {                                          \
    if(left_applies_p(&(r->left_c), p_left[i], subexp)) {               \
      int j;                                                            \
      for(j = 0; p_right[j]; j++) {                                     \
        if(right_applies_p(p_left[i], r->right, p_right[j], subexp)) {  \
          const char *left = p_left[i];                                 \
          const char *right = p_right[j];                               \
          body;                                                         \
        }                                                               \
      }                                                                 \
    }                                                                   \
  }

static void
apply_disconnect_rule(struct plumber *p, struct rule *r, const char **p_left, const char **p_right)
{
  TRAVERSE_CONNECTIONS(p, r, p_left,
                       vprintf(p->q == 0, "Disconnect: '%s' -> '%s'\n", left, right);
                       jack_port_disconnect_named(p->j, left, right));
}

static void
apply_connect_rule(struct plumber *p, struct rule *r, const char **p_left, const char **p_right)
{
  TRAVERSE_LISTS(r, p_left, p_right,
                 if(!jack_port_is_connected_p(p->j, left, right)) {
                   vprintf(p->q == 0, "Connect: '%s' -> '%s'\n", left, right);
                   jack_port_connect_named(p->j, left, right);
                 });
}

#define TRAVERSE_RULE_SET(p,class)                              \
  for(i = 0; i < p->n; i++) {                                   \
    if(p->r[i].command == class) {                              \
      apply_##class##_rule(p, &(p->r[i]), p_left, p_right);     \
    }                                                           \
  }

/* Run the set of plumbing rules. */
static void
apply_rule_set(struct plumber *p)
{
  acquire_rule_set(p);
  const char **p_left, **p_right;
  p_left = jack_get_ports(p->j, NULL, NULL, JackPortIsOutput);
  p_right = jack_get_ports(p->j, NULL, NULL, JackPortIsInput);
  if(p_left && p_right) {
    int i;
    TRAVERSE_RULE_SET(p, disconnect);
    TRAVERSE_RULE_SET(p, connect);
  }
  if(p_left) {
    free(p_left);
  }
  if(p_right) {
    free(p_right);
  }
}

/* Port notifications tend to arrive in sets, when one is signaled the
   plumber enters a nanosleep loop only continuing when the notifications
   stop arriving.  The `w' field is -1 when not in a set, >1 when
   requests are arriving and zero when a set is ended.  */
static void
wait_on_connection_set(struct plumber *p)
{
  struct timespec t;
  t = usec_to_timespec(p->u);
  eprintf("%s: wait init: w=%d\n", __func__, p->w);
  pthread_cond_wait(&p->cond, &p->lock);
  eprintf("%s: wait seq: w=%d\n", __func__, p->w);
  while(p->w > 0) {
    p->w = 0;
    nanosleep(&t, NULL);
    eprintf("%s: sleep: u=%lu, w=%d\n", __func__, p->u, p->w);
  }
  eprintf ("%s: wait end\n", __func__);
}

static void *
plumbing_daemon(void *PTR)
{
  struct plumber *p = (struct plumber*) PTR;
  pthread_mutex_lock (&p->lock);
  while(1) {
    wait_on_connection_set(p);
    apply_rule_set(p);
    p->w = -1;
  }
  pthread_mutex_unlock(&p->lock);
  return NULL;
}

static void
send_wakeup(struct plumber *p)
{
  if(p->w < 0) {
    pthread_mutex_lock(&p->lock);
    pthread_cond_signal(&p->cond);
    p->w = 1;
    pthread_mutex_unlock(&p->lock);
  }
  p->w += 1;
}

static void
on_client_registration(const char *nm, int st, void *PTR)
{
  eprintf("%s: '%s', %d\n", __func__, nm, st);
}

static void
on_port_registration(jack_port_id_t id, int st, void *PTR)
{
  eprintf("%s: %u, %d\n", __func__, id, st);
  send_wakeup(PTR);
}

static int
on_reorder(void *PTR)
{
  eprintf("%s\n", __func__);
  return 0;
}

static void
init_plumber_defaults(struct plumber *p)
{
  p->g = 0;
  p->n = 0;
  p->w = -1;
  p->m = 0;
  p->u = DEFAULT_DELAY;
  p->d = 1;
  p->o = 1;
  p->q = 0;
  pthread_mutex_init(&p->lock, NULL);
  pthread_cond_init(&p->cond, NULL);
}

static void
init_plumber_connection(struct plumber *p)
{
  p->j = jack_client_unique("rju-plumbing");
}

static void
finalize_plumber(struct plumber *p)
{
  jack_client_close(p->j);
  pthread_cond_destroy(&p->cond);
  pthread_mutex_destroy(&p->lock);
}

static void
as_daemon(struct plumber *p)
{
  pthread_create(&(p->t), NULL, plumbing_daemon, p);
  jack_set_client_registration_callback(p->j, on_client_registration, p);
  jack_set_port_registration_callback(p->j, on_port_registration, p);
  jack_set_graph_order_callback(p->j, on_reorder, p);
  jack_client_activate(p->j);
  apply_rule_set(p);
  pthread_join(p->t, NULL);
}

static void
plumber_usage(void)
{
  eprintf("Usage: rju-plumbing [options] [rule-files]\n");
  eprintf("    -d   : Do not start as daemon\n");
  eprintf("    -o   : Do not acquire ordinary rule files\n");
  eprintf("    -q   : Quiet operation\n");
  eprintf("    -u N : Micro-seconds to defer at connection (default=%d)\n", DEFAULT_DELAY);
  exit(1);
}

static void
parse_arguments(struct plumber *p, char **argv, int argc)
{
  int c;
  while(( c = getopt(argc, argv, "dhoqu:")) != -1) {
    switch(c) {
    case 'd': p->d = 0; break;
    case 'h': plumber_usage (); break;
    case 'o': p->o = 0; break;
    case 'q': p->q = 1; break;
    case 'u': p->u = u64_max(1,strtoul(optarg, NULL, 0)); break;
    default: plumber_usage (); break;
    }
  }
  append_rule_file_list(p, argv + optind, argc - optind);
}

static void
start_plumber(struct plumber *p, char **argv, int argc)
{
  init_plumber_defaults(p);
  parse_arguments(p, argv, argc);
  init_plumber_connection(p);
  if(p->o) {
    append_ordinary_rule_files(p);
  }
  if(p->d) {
    as_daemon(p);
  } else {
    apply_rule_set(p);
  }
  finalize_plumber(p);
}

int
main(int argc, char *argv[])
{
  struct plumber p;
  start_plumber(&p, argv, argc);
  return 0;
}
