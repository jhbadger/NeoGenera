/* prims_sys.c -- file-system, subprocess and profiling primitives for the developer tools. */
#include "neogenera.h"
#include <dirent.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <signal.h>
#include <errno.h>
#include <fcntl.h>

static Obj need_str(Obj o) { if (!STRINGP(o)) type_error("STRING", o); return o; }

PRIM(p_directory) {
  /* (%directory path) -> list of (name type size mtime), type :FILE :DIRECTORY :OTHER; NIL if unreadable */
  UNUSED_ARGS;
  const char *path = STR_DATA(need_str(argv[0]));
  DIR *d = opendir(path);
  if (!d) return NIL;
  Obj head = NIL, tail = NIL;
  struct dirent *e;
  while ((e = readdir(d))) {
    if (!strcmp(e->d_name, ".") || !strcmp(e->d_name, "..")) continue;
    char full[4096]; snprintf(full, sizeof full, "%s/%s", path, e->d_name);
    struct stat st;
    const char *type = "OTHER"; long size = 0, mtime = 0;
    if (stat(full, &st) == 0) { type = S_ISDIR(st.st_mode) ? "DIRECTORY" : S_ISREG(st.st_mode) ? "FILE" : "OTHER"; size = (long)st.st_size; mtime = (long)st.st_mtime; }
    Obj ent = cons(make_cstring(e->d_name), cons(intern_kw(type), cons(FIX(size), cons(FIX(mtime), NIL))));
    Obj cell = cons(ent, NIL);
    if (head == NIL) head = cell; else CDR(tail) = cell;
    tail = cell;
  }
  closedir(d);
  return head;
}

PRIM(p_file_attrs) {
  UNUSED_ARGS;
  struct stat st;
  if (stat(STR_DATA(need_str(argv[0])), &st) != 0) return NIL;
  return list3(intern_kw(S_ISDIR(st.st_mode) ? "DIRECTORY" : "FILE"), FIX((SObj)st.st_size), FIX((SObj)st.st_mtime));
}
PRIM(p_rename_file) { UNUSED_ARGS; return BOOL(rename(STR_DATA(need_str(argv[0])), STR_DATA(need_str(argv[1]))) == 0); }
PRIM(p_make_directory) { UNUSED_ARGS; return BOOL(mkdir(STR_DATA(need_str(argv[0])), 0777) == 0); }
PRIM(p_getcwd) { UNUSED_ARGS; char b[4096]; return getcwd(b, sizeof b) ? make_cstring(b) : NIL; }

PRIM(p_run_program) {
  /* (%run-program program args [stdin-string]) -> (values output exit-status).  No shell: execvp. */
  const char *prog = STR_DATA(need_str(argv[0]));
  int n = 0; for (Obj l = argv[1]; CONSP(l); l = CDR(l)) n++;
  char **av = malloc((size_t)(n + 2) * sizeof(char *));
  av[0] = (char *)prog; int i = 1;
  for (Obj l = argv[1]; CONSP(l); l = CDR(l)) av[i++] = (char *)STR_DATA(need_str(CAR(l)));
  av[i] = NULL;
  const char *input = (argc > 2 && STRINGP(argv[2])) ? STR_DATA(argv[2]) : NULL;
  size_t ilen = input ? STR_LEN(argv[2]) : 0;
  int out[2], in[2];
  if (pipe(out) != 0 || pipe(in) != 0) { free(av); lisp_error("RUN-PROGRAM: pipe failed"); }
  fflush(stdout); fflush(stderr);
  pid_t pid = fork();
  if (pid < 0) { free(av); lisp_error("RUN-PROGRAM: fork failed"); }
  if (pid == 0) {
    dup2(in[0], 0); dup2(out[1], 1); dup2(out[1], 2);
    close(in[0]); close(in[1]); close(out[0]); close(out[1]);
    execvp(prog, av);
    _exit(127);
  }
  free(av);
  close(in[0]); close(out[1]);
  if (ilen) { size_t off = 0; while (off < ilen) { ssize_t w = write(in[1], input + off, ilen - off); if (w <= 0) break; off += (size_t)w; } }
  close(in[1]);
  Buf b; buf_init(&b);
  char tmp[4096]; ssize_t r;
  while ((r = read(out[0], tmp, sizeof tmp)) > 0) buf_put(&b, tmp, (size_t)r);
  close(out[0]);
  int status = 0; waitpid(pid, &status, 0);
  Obj s = make_string(b.buf ? b.buf : "", b.len);
  buf_free(&b);
  mv_vals[0] = s; mv_vals[1] = FIX(WIFEXITED(status) ? WEXITSTATUS(status) : 128 + (WIFSIGNALED(status) ? WTERMSIG(status) : 0));
  mv_count = 2;
  return s;
}

/* ---- statistical profiler ---------------------------------------------------
 * SIGPROF only sets a flag; the VM records a sample at its next safe point (a call
 * or a backward jump), where allocating is allowed.  A sample is the list of function
 * names of the innermost frames.  Only compiled (VM) frames are visible. */
volatile sig_atomic_t prof_pending;
static Obj prof_samples;
static int prof_running;

static void on_prof(int s) { (void)s; prof_pending = 1; }

void prof_record(void) {
  prof_pending = 0;
  if (!prof_running) return;
  Obj head = NIL, tail = NIL;
  int depth = 0;
  for (int i = vfp - 1; i >= 0 && depth < 16; i--, depth++) {
    Obj name = SLOT(SLOT(vframes[i].closure, 0), C_NAME);
    Obj cell = cons(name, NIL);
    if (head == NIL) head = cell; else CDR(tail) = cell;
    tail = cell;
  }
  prof_samples = cons(head, prof_samples);
}

PRIM(p_profile_start) {
  UNUSED_ARGS;
  long us = FIXP(argv[0]) ? (long)UNFIX(argv[0]) : 5000;
  struct sigaction sa; memset(&sa, 0, sizeof sa); sa.sa_handler = on_prof; sa.sa_flags = SA_RESTART;
  sigaction(SIGPROF, &sa, NULL);
  struct itimerval it = { { 0, us }, { 0, us } };
  prof_samples = NIL; prof_pending = 0; prof_running = 1;
  setitimer(ITIMER_PROF, &it, NULL);
  return TT;
}
PRIM(p_profile_stop) {
  UNUSED_ARGS;
  struct itimerval it = { { 0, 0 }, { 0, 0 } };
  setitimer(ITIMER_PROF, &it, NULL);
  prof_running = 0; prof_pending = 0;
  Obj s = prof_samples; prof_samples = NIL;
  return s;
}

void init_prims_sys(void) {
  add_root(&prof_samples); prof_samples = NIL;
  defprim("%DIRECTORY", p_directory, 1, 1); defprim("%FILE-ATTRS", p_file_attrs, 1, 1);
  defprim("%RENAME-FILE", p_rename_file, 2, 2); defprim("%MAKE-DIRECTORY", p_make_directory, 1, 1);
  defprim("%GETCWD", p_getcwd, 0, 0); defprim_mv("%RUN-PROGRAM", p_run_program, 2, 3);
  defprim("%PROFILE-START", p_profile_start, 1, 1); defprim("%PROFILE-STOP", p_profile_stop, 0, 0);
}
