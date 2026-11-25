/* main.c -- command line, initialization and the entry point.
 *
 *   neogenera [-i IMAGE] [--no-image] [--load FILE]... [--eval FORM]...
 *             [--save-image FILE] [--gc-stress] [--gc-verbose] [--no-toplevel]
 *
 * The interpreter runs in a thread with a very large stack so deeply
 * recursive Lisp code does not overflow the C stack.
 */
#include "neogenera.h"
#include <pthread.h>
#include <signal.h>
#include <execinfo.h>
#include <unistd.h>

int g_argc;
char **g_argv;

static void crash_handler(int sig) {
  void *frames[48];
  int n = backtrace(frames, 48);
  const char msg[] = "\nFATAL: fatal signal in NeoGenera kernel; backtrace:\n";
  if (write(2, msg, sizeof msg - 1) < 0) _exit(70);
  backtrace_symbols_fd(frames, n, 2);
  _exit(128 + sig);
}

typedef struct { int argc; char **argv; int rc; size_t stack_bytes; } MainArgs;

static void init_all(void) {
  init_memory();
  init_eval();
  init_symbols();
  init_streams();
  init_vm();
  init_prims_core();
  init_prims_data(); init_prims_num(); init_prims_term(); init_prims_sys();
  init_prims_io(); init_prims_bio();
}

static void *real_main(void *p) {
  MainArgs *a = p;
  { char here; ng_stack_limit = &here - a->stack_bytes + a->stack_bytes / 8; }   /* keep an eighth of the stack as margin */
  static char altstack2[1 << 16];
  stack_t ss2 = { .ss_sp = altstack2, .ss_size = sizeof altstack2, .ss_flags = 0 };
#if !defined(__SANITIZE_ADDRESS__)
  sigaltstack(&ss2, NULL);
#endif
  int argc = a->argc; char **argv = a->argv;
  const char *image = NULL;
  const char *volatile top_name = "NG-TOPLEVEL";
  const char *volatile save = NULL;
  int use_image = 1;
  volatile int toplevel = 1;
  const char *loads[64]; int nloads = 0;
  const char *evals[64]; int nevals = 0;
  char order[128]; int norder = 0;           /* 'l' or 'e' in command-line order */

  for (int i = 1; i < argc; i++) {
    if (!strcmp(argv[i], "-i") && i + 1 < argc) image = argv[++i];
    else if (!strcmp(argv[i], "--no-image")) use_image = 0;
    else if (!strcmp(argv[i], "--load") && i + 1 < argc && nloads < 64) { loads[nloads++] = argv[++i]; order[norder++] = 'l'; }
    else if (!strcmp(argv[i], "--eval") && i + 1 < argc && nevals < 64) { evals[nevals++] = argv[++i]; order[norder++] = 'e'; }
    else if (!strcmp(argv[i], "--save-image") && i + 1 < argc) save = argv[++i];
    else if (!strcmp(argv[i], "--gc-stress")) gc_stress = 1;
    else if (!strcmp(argv[i], "--gc-verbose")) gc_verbose = 1;
    else if (!strcmp(argv[i], "--no-toplevel")) toplevel = 0;
    else if (!strcmp(argv[i], "--ui")) top_name = "NG-UI";
    else if (!strcmp(argv[i], "--toplevel") && i + 1 < argc) top_name = argv[++i];
    else if (!strcmp(argv[i], "--help")) {
      puts("usage: neogenera [-i IMAGE] [--no-image] [--load FILE] [--eval FORM] [--save-image FILE]\n"
           "                 [--gc-stress] [--gc-verbose] [--no-toplevel]");
      a->rc = 0; return NULL;
    } else { fprintf(stderr, "unknown argument: %s\n", argv[i]); a->rc = 2; return NULL; }
  }
  g_argc = argc; g_argv = argv;

  init_all();
  if (use_image) {
    const char *img = image ? image : getenv("NG_IMAGE");
    if (img) {
      int rc = load_image(img);
      if (rc != 0) { fprintf(stderr, "cannot load image %s (code %d)\n", img, rc); a->rc = 2; return NULL; }
    }
  }
  gc_pending = gc_stress;

  int li = 0, ei = 0;
  for (int i = 0; i < norder; i++) {
    if (order[i] == 'l') {
      if (load_file(loads[li++], 0) != 0) { a->rc = 1; return NULL; }
    } else {
      const char *src = evals[ei++];
      Frame f;
      int ssp = spsp, svsp = vsp, snr = nroots;
      frame_push(&f, F_TOP, NIL);
      if (setjmp(f.jb) == 0) {
        Obj form = read_from_cstring(src);
        Obj r = eval(form, NIL, NIL);
        frame_pop(&f);
        print_obj(stream_stdout, r, 1); stream_putc(stream_stdout, '\n');
      } else {
        frame_top = f.prev;
        print_error_message(f.value);
        unbind_to(ssp); vsp = svsp; nroots = snr;
        a->rc = 1; return NULL;
      }
    }
  }
  if (save) {
    int rc = save_image(save);
    if (rc != 0) { fprintf(stderr, "cannot save image %s\n", save); a->rc = 1; return NULL; }
    fprintf(stderr, "; saved image %s\n", save);
  }
  a->rc = toplevel ? run_toplevel(top_name) : 0;
  fflush(stdout);
  return NULL;
}

int main(int argc, char **argv) {
  MainArgs a = { argc, argv, 0, 0 };
#if !defined(__SANITIZE_ADDRESS__)
  static char altstack[1 << 16];
  stack_t ss = { .ss_sp = altstack, .ss_size = sizeof altstack, .ss_flags = 0 };
  sigaltstack(&ss, NULL);
  struct sigaction sa;
  memset(&sa, 0, sizeof sa);
  sa.sa_handler = crash_handler;
  sa.sa_flags = SA_ONSTACK;
  sigaction(SIGSEGV, &sa, NULL);
  sigaction(SIGBUS, &sa, NULL);
  sigaction(SIGABRT, &sa, NULL);
#endif
  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_attr_setstacksize(&attr, (size_t)1 << 30);
  pthread_t th;
  a.stack_bytes = (size_t)1 << 30;
  if (pthread_create(&th, &attr, real_main, &a) != 0) { a.stack_bytes = (size_t)6 << 20 ; real_main(&a); return a.rc; }
  pthread_join(th, NULL);
  return a.rc;
}
