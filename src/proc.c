/* proc.c -- Lisp processes: threads that take turns holding one interpreter lock.
 *
 * Every Lisp process runs on its own POSIX thread with its own C stack, value stack, VM frames,
 * special-binding stack, root stack and catch frames, but only the process holding the interpreter
 * lock (CURRENT) runs Lisp.  The interpreter's globals (vstack, vsp, specpdl, frame_top, ...) always
 * describe the current process: switching saves them into the outgoing PROC and loads the incoming one.
 *
 * Special bindings are shallow (the symbol's value cell holds the innermost binding), so a switch
 * also swaps them: the outgoing process's bindings are undone, keeping its values in its specpdl,
 * and the incoming process's are redone.  Global values are shared by all processes.
 *
 * A process gives up the lock only
 *   - at a VM safe point (CALL, TAILCALL, backward JMP) when NG_ATTENTION is set: by the ticker
 *     thread every quantum (preemption), by an interrupt or arrest request, or after a switch-in that
 *     has one pending;
 *   - around a blocking system call (sleep, terminal input, a subprocess), bracketed by
 *     PROC_BLOCKING_BEGIN / PROC_BLOCKING_END, with no unprotected Obj live in C;
 *   - when it finishes.
 * These are exactly the places a collection may already happen, so another process's GC never sees
 * a half-built object, and the GC forwards the saved roots of every process that is not running.
 *
 * The scheduler is round robin over the processes waiting for the lock that are not arrested.
 * PROCESS-WAIT (Lisp) polls its predicate; when nothing else is runnable it sleeps a few
 * milliseconds without the lock (%PROCESS-WAIT-YIELD), so idle waiting costs little.
 */
#include "neogenera.h"
#include <pthread.h>
#include <time.h>
#include <errno.h>
#include <poll.h>
#include <unistd.h>

#define MAX_PROCS 256
#define QUANTUM_MS 20
#define WAIT_SLEEP_MS 5

/* stack sizes of processes other than the initial one */
#define P_VSTACK  (1 << 20)
#define P_VFRAMES (1 << 17)
#define P_SPEC    (1 << 16)
#define P_ROOTS   (1 << 17)
#define P_CSTACK  ((size_t)256 << 20)

typedef struct Proc {
  int used, dead, started, id;
  int ready;                      /* waiting for the interpreter lock */
  int arrested, waiting, interrupt, no_preempt;
  int initial;
  pthread_cond_t cv; int cv_init;
  /* saved interpreter state (valid while the process is not current) */
  Obj *vstack; int vsp, vstack_size;
  VFrame *vframes; int vfp, vframe_size;
  SpecBind *specpdl; int spsp, spec_size;
  Obj **roots; int nroots, roots_size;
  Frame *frame_top; int eval_depth, gc_inhibit;
  int vsp_limit, vfp_limit, root_limit; char *stack_limit;
  int mv_count; Obj mv[MAX_MV];
  Obj obj, fn;                    /* the Lisp process object and the function the process runs */
  /* accounting */
  long long run_us, t_in; long switches;
} Proc;

static Proc procs[MAX_PROCS];
static pthread_mutex_t mx = PTHREAD_MUTEX_INITIALIZER;
static Proc *current;              /* holder of the interpreter lock, or NULL */
static int nlive, next_id = 1, rr;  /* live processes, id counter, round-robin cursor */
static __thread Proc *me;
volatile int ng_attention;
static int ticker_started;
static int no_preempt;               /* WITHOUT-INTERRUPTS depth of the running process */

static long long now_us(void) { struct timespec ts; clock_gettime(CLOCK_MONOTONIC, &ts); return (long long)ts.tv_sec * 1000000 + ts.tv_nsec / 1000; }

/* ---- saving and restoring the interpreter globals ------------------------------------------------ */

static void save_state(Proc *p) {
  for (int i = spsp - 1; i >= 0; i--) {             /* undo the bindings, keeping their values */
    Obj s = specpdl[i].sym, v = SYM_VALUE(s);
    SYM_VALUE(s) = specpdl[i].old; WB(s, SYM_VALUE(s)); specpdl[i].old = v;
  }
  p->vstack = vstack; p->vsp = vsp; p->vstack_size = vstack_size;
  p->vframes = vframes; p->vfp = vfp; p->vframe_size = vframe_size;
  p->specpdl = specpdl; p->spsp = spsp; p->spec_size = spec_size;
  p->roots = root_stack; p->nroots = nroots; p->roots_size = root_stack_size;
  p->frame_top = frame_top; p->eval_depth = eval_depth; p->gc_inhibit = gc_inhibit;
  p->vsp_limit = ng_vsp_limit; p->vfp_limit = ng_vfp_limit; p->root_limit = ng_root_limit; p->stack_limit = ng_stack_limit;
  p->mv_count = mv_count; p->no_preempt = no_preempt;
  for (int i = 0; i < mv_count && i < MAX_MV; i++) p->mv[i] = mv_vals[i];
  long long t = now_us(); p->run_us += t - p->t_in;
}

static void restore_state(Proc *p) {
  vstack = p->vstack; vsp = p->vsp; vstack_size = p->vstack_size;
  vframes = p->vframes; vfp = p->vfp; vframe_size = p->vframe_size;
  specpdl = p->specpdl; spsp = p->spsp; spec_size = p->spec_size;
  root_stack = p->roots; nroots = p->nroots; root_stack_size = p->roots_size;
  frame_top = p->frame_top; eval_depth = p->eval_depth; gc_inhibit = p->gc_inhibit;
  ng_vsp_limit = p->vsp_limit; ng_vfp_limit = p->vfp_limit; ng_root_limit = p->root_limit; ng_stack_limit = p->stack_limit;
  mv_count = p->mv_count; no_preempt = p->no_preempt;
  for (int i = 0; i < mv_count && i < MAX_MV; i++) mv_vals[i] = p->mv[i];
  for (int i = 0; i < spsp; i++) {                   /* redo the bindings */
    Obj s = specpdl[i].sym, v = SYM_VALUE(s);
    SYM_VALUE(s) = specpdl[i].old; WB(s, SYM_VALUE(s)); specpdl[i].old = v;
  }
  p->t_in = now_us(); p->switches++;
  if (p->interrupt || p->arrested) ng_attention = 1;
}

/* ---- the scheduler (all under MX) ----------------------------------------------------------------- */

static Proc *pick_ready(Proc *skip) {
  for (int k = 1; k <= MAX_PROCS; k++) {
    Proc *q = &procs[(rr + k) % MAX_PROCS];
    if (q->used && !q->dead && q->ready && !q->arrested && q != skip) { rr = (int)(q - procs); return q; }
  }
  return NULL;
}

static void hand_off(Proc *next) {
  current = next;
  if (next) pthread_cond_signal(&next->cv);
}

static void wait_turn(Proc *p) {        /* with MX held: wait until P holds the lock */
  if (current == NULL && !p->arrested) current = p;
  p->ready = 1;
  while (current != p) {
    if (current == NULL && !p->arrested) { current = p; break; }
    pthread_cond_wait(&p->cv, &mx);
  }
  p->ready = 0;
}

static void acquire(Proc *p) {
  pthread_mutex_lock(&mx);
  wait_turn(p);
  pthread_mutex_unlock(&mx);
  restore_state(p);
}

static void release(Proc *p) {
  save_state(p);
  pthread_mutex_lock(&mx);
  hand_off(pick_ready(p));
  pthread_mutex_unlock(&mx);
}

/* Give the lock to another runnable process, if there is one (or if P is arrested); returns 1 if P was switched out. */
static int yield(Proc *p) {
  pthread_mutex_lock(&mx);
  Proc *next = pick_ready(p);
  if (!next && !p->arrested) { pthread_mutex_unlock(&mx); return 0; }
  save_state(p);
  hand_off(next);
  wait_turn(p);
  pthread_mutex_unlock(&mx);
  restore_state(p);
  return 1;
}

int proc_blocking_begin(void) {
  Proc *p = me;
  if (!p || nlive <= 1 || gc_inhibit) return 0;
  release(p);
  return 1;
}

void proc_blocking_end(int token) { if (token) acquire(me); }

static Proc *find_proc(Obj obj) {
  for (int i = 0; i < MAX_PROCS; i++) if (procs[i].used && !procs[i].dead && procs[i].obj == obj && obj != NIL) return &procs[i];
  return NULL;
}

/* ---- safe points ------------------------------------------------------------------------------------ */

/* call the Lisp function NAME with no arguments at a safe point, keeping the current multiple values */
static void run_hook(const char *name) {
  Obj fn = SYM_FUNC(intern_cl(name));
  if (fn == NIL || CONSP(fn)) return;
  Obj saved = NIL;                                    /* the multiple values may be live at a CALL */
  if (mv_count != 1) for (int i = mv_count - 1; i >= 0; i--) saved = cons(mv_vals[i], saved);
  int cnt = mv_count;
  PROTECT(saved);
  PUSH(fn); apply_stack(0);
  UNPROTECT(1);
  if (cnt != 1) values_list(saved); else mv_count = 1;
}

void proc_attention(void) {
  ng_attention = 0;
  Proc *p = me;
  if (no_preempt) return;
  if (finalizers_pending) run_hook("%RUN-FINALIZERS");    /* (it clears the flag, and does nothing when nested) */
  if (!p) return;
  if (p->interrupt) {
    p->interrupt = 0;
    run_hook("%PROCESS-RUN-INTERRUPTS");
  }
  if (nlive > 1 || p->arrested) yield(p);
}

static void *ticker(void *arg) {
  (void)arg;
  for (;;) {
    struct timespec ts = { 0, QUANTUM_MS * 1000000L };
    nanosleep(&ts, NULL);
    if (nlive > 1) ng_attention = 1;
  }
  return NULL;
}

/* ---- GC roots of the processes that are not running ------------------------------------------------- */

void gc_forward_roots(Obj (*fwd)(Obj)) {
  for (int k = 0; k < MAX_PROCS; k++) {
    Proc *p = &procs[k];
    if (!p->used || p->dead) continue;
    p->obj = fwd(p->obj); p->fn = fwd(p->fn);
    if (p == me || !p->started) continue;           /* the running process's state is in the globals */
    for (int i = 0; i < p->vsp; i++) p->vstack[i] = fwd(p->vstack[i]);
    for (int i = 0; i < p->spsp; i++) { p->specpdl[i].sym = fwd(p->specpdl[i].sym); p->specpdl[i].old = fwd(p->specpdl[i].old); }
    for (int i = 0; i < p->nroots; i++) *p->roots[i] = fwd(*p->roots[i]);
    for (int i = 0; i < p->vfp; i++) p->vframes[i].closure = fwd(p->vframes[i].closure);
    for (int i = 0; i < p->mv_count && i < MAX_MV; i++) p->mv[i] = fwd(p->mv[i]);
    for (Frame *f = p->frame_top; f; f = f->prev) {
      f->tag = fwd(f->tag); f->value = fwd(f->value); f->cleanup = fwd(f->cleanup); f->mvlist = fwd(f->mvlist);
    }
  }
}

/* ---- process threads ------------------------------------------------------------------------------- */

static void free_stacks(Proc *p) {
  free(p->vstack); free(p->vframes); free(p->specpdl); free(p->roots);
  p->vstack = NULL; p->vframes = NULL; p->specpdl = NULL; p->roots = NULL;
}

static void *proc_main(void *arg) {
  Proc *p = arg;
  me = p;
  { char here; p->stack_limit = &here - P_CSTACK + P_CSTACK / 8; }
  acquire(p);
  Frame f;
  frame_push(&f, F_TOP, NIL);
  if (setjmp(f.jb) == 0) {
    Obj fn = p->fn;
    PUSH(fn); apply_stack(0);
    frame_pop(&f);
  } else {
    frame_top = f.prev;                              /* an error nobody handled: the process just ends */
  }
  /* finish: still holding the lock */
  pthread_mutex_lock(&mx);
  p->dead = 1; nlive--;
  free_stacks(p);
  p->obj = NIL; p->fn = NIL;
  p->used = 0;
  hand_off(pick_ready(p));
  pthread_mutex_unlock(&mx);
  return NULL;
}

static Proc *new_proc(void) {
  for (int i = 0; i < MAX_PROCS; i++) {
    Proc *p = &procs[i];
    if (!p->used) {
      int ci = p->cv_init; pthread_cond_t cv = p->cv;
      memset(p, 0, sizeof *p);
      p->cv_init = ci; p->cv = cv;
      if (!p->cv_init) { pthread_cond_init(&p->cv, NULL); p->cv_init = 1; }
      p->used = 1; p->id = next_id++; p->obj = NIL; p->fn = NIL;
      return p;
    }
  }
  return NULL;
}

void init_proc(void) {
  Proc *p = new_proc();                              /* the initial process: the thread running main */
  p->initial = 1; p->started = 1;
  me = p; current = p; nlive = 1;
  p->t_in = now_us();
}

/* ---- primitives -------------------------------------------------------------------------------------- */

PRIM(p_process_create) {
  /* (%process-create function process-object) -> T; the new process starts when it is scheduled */
  UNUSED_ARGS;
  Proc *p = new_proc();
  if (!p) lisp_error("Too many processes (at most %d)", MAX_PROCS);
  p->vstack = calloc(P_VSTACK, sizeof(Obj)); p->vstack_size = P_VSTACK;
  p->vframes = calloc(P_VFRAMES, sizeof(VFrame)); p->vframe_size = P_VFRAMES;
  p->specpdl = calloc(P_SPEC, sizeof(SpecBind)); p->spec_size = P_SPEC;
  p->roots = calloc(P_ROOTS, sizeof(Obj *)); p->roots_size = P_ROOTS;
  if (!p->vstack || !p->vframes || !p->specpdl || !p->roots) { free_stacks(p); p->used = 0; lisp_error("Cannot allocate the stacks of a new process"); }
  p->vsp_limit = P_VSTACK - P_VSTACK / 8; p->vfp_limit = P_VFRAMES - P_VFRAMES / 8; p->root_limit = P_ROOTS - P_ROOTS / 8;
  p->mv_count = 1;
  p->fn = argv[0]; p->obj = argv[1];
  p->ready = 1; p->started = 1;
  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_attr_setstacksize(&attr, P_CSTACK);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
  pthread_t th;
  pthread_mutex_lock(&mx);
  nlive++;
  int rc = pthread_create(&th, &attr, proc_main, p);
  if (rc != 0) { nlive--; free_stacks(p); p->used = 0; p->obj = NIL; p->fn = NIL; }
  pthread_mutex_unlock(&mx);
  pthread_attr_destroy(&attr);
  if (rc != 0) lisp_error("Cannot create a process thread (error %d)", rc);
  if (!ticker_started) {
    pthread_t tt; pthread_attr_t ta; pthread_attr_init(&ta);
    pthread_attr_setdetachstate(&ta, PTHREAD_CREATE_DETACHED);
    if (pthread_create(&tt, &ta, ticker, NULL) == 0) ticker_started = 1;
    pthread_attr_destroy(&ta);
  }
  return TT;
}

PRIM(p_process_current) { UNUSED_ARGS; return me ? me->obj : NIL; }

PRIM(p_process_set_current) {
  /* (%process-set-current object): name the Lisp object of the running process (the initial process) */
  UNUSED_ARGS; if (me) me->obj = argv[0]; return argv[0];
}

PRIM(p_process_list) {
  UNUSED_ARGS;
  Obj l = NIL;
  PROTECT(l);
  for (int i = MAX_PROCS - 1; i >= 0; i--)
    if (procs[i].used && !procs[i].dead && procs[i].obj != NIL) l = cons(procs[i].obj, l);
  UNPROTECT(1);
  return l;
}

PRIM(p_process_alive) { UNUSED_ARGS; return BOOL(find_proc(argv[0]) != NULL); }

PRIM(p_process_info) {
  /* (%process-info process) -> (id run-microseconds switches state) or NIL; state :run :ready :blocked :arrested :wait */
  UNUSED_ARGS;
  Proc *p = find_proc(argv[0]);
  if (!p) return NIL;
  long long run = p->run_us + (p == me ? now_us() - p->t_in : 0);
  const char *st = p == me ? "RUN" : p->arrested ? "ARRESTED" : p->waiting ? "WAIT" : p->ready ? "READY" : "BLOCKED";
  Obj kw = intern_kw(st);
  Obj l = cons(kw, NIL);
  l = cons(FIX(p->switches), l);
  l = cons(int_from_i64(run), l);
  return cons(FIX(p->id), l);
}

PRIM(p_process_yield) { UNUSED_ARGS; return BOOL(me && yield(me)); }

PRIM(p_process_wait_yield) {
  /* Called by PROCESS-WAIT each time its predicate is false.  Lets another process run; when nothing
     else can run, sleeps without the lock so the predicate is polled only every few milliseconds. */
  UNUSED_ARGS;
  Proc *p = me;
  if (!p) return NIL;
  p->waiting = 1;
  int other_busy = 0;
  pthread_mutex_lock(&mx);
  for (int i = 0; i < MAX_PROCS; i++) {
    Proc *q = &procs[i];
    if (q != p && q->used && !q->dead && q->ready && !q->arrested && !q->waiting) { other_busy = 1; break; }
  }
  pthread_mutex_unlock(&mx);
  if (other_busy) yield(p);
  else {
    int tok = proc_blocking_begin();
    struct timespec ts = { 0, WAIT_SLEEP_MS * 1000000L };
    nanosleep(&ts, NULL);
    proc_blocking_end(tok);
  }
  return NIL;
}

PRIM(p_process_set_waiting) { UNUSED_ARGS; if (me) me->waiting = argv[0] != NIL; return argv[0]; }

PRIM(p_process_interrupt) {
  /* (%process-interrupt process): make the process run %PROCESS-RUN-INTERRUPTS at its next safe point */
  UNUSED_ARGS;
  Proc *p = find_proc(argv[0]);
  if (!p) return NIL;
  p->interrupt = 1;
  if (p == me) ng_attention = 1;
  return TT;
}

PRIM(p_process_arrest) {
  /* (%process-arrest process flag): an arrested process is not scheduled */
  UNUSED_ARGS;
  Proc *p = find_proc(argv[0]);
  if (!p) return NIL;
  if (p->initial && argv[1] != NIL) lisp_error("The initial process cannot be arrested");
  pthread_mutex_lock(&mx);
  p->arrested = argv[1] != NIL;
  pthread_mutex_unlock(&mx);
  if (p == me && p->arrested) ng_attention = 1;
  return TT;
}

PRIM(p_preempt_inhibit) {
  /* (%preempt-inhibit delta) -> new depth.  While the depth is positive the running process is not
     preempted and runs no interrupts (it still gives up the lock in blocking calls). */
  UNUSED_ARGS;
  no_preempt += (int)UNFIX(argv[0]);
  if (no_preempt < 0) no_preempt = 0;
  if (no_preempt == 0 && (finalizers_pending || (me && (me->interrupt || me->arrested || nlive > 1)))) ng_attention = 1;
  return FIX(no_preempt);
}

PRIM(p_process_count) { UNUSED_ARGS; return FIX(nlive); }

static int ws_char(int c) { return c == ' ' || c == '\t' || c == '\n' || c == '\r'; }

PRIM(p_stdin_wait) {
  /* (%stdin-wait stream): before reading STREAM, when it is the terminal's stdin and other processes exist,
     skip the whitespace already typed and wait without the lock until a character that starts a form
     arrives.  (stdin is unbuffered on a terminal, so poll() on the descriptor tells the truth.)  The
     reader can then still block holding the lock in the middle of a form that is being typed. */
  UNUSED_ARGS;
  Obj s = argv[0];
  if (nlive <= 1 || !isatty(0) || !IS_TYPE(s, T_STREAM) || UNFIX(SLOT(s, ST_KIND)) != SK_STDIN) return NIL;
  PROTECT(s);
  for (;;) {
    long pk = (long)UNFIX(SLOT(s, ST_PEEK));
    if (pk >= 0) { if (ws_char((int)pk)) { stream_getc(s); continue; } break; }
    struct pollfd pf = { 0, POLLIN, 0 };
    if (poll(&pf, 1, 0) > 0) {
      int c = stream_getc(s);
      if (c < 0) break;
      if (!ws_char(c)) { stream_ungetc(s, c); break; }
      continue;
    }
    if (nlive <= 1) break;
    int tok = proc_blocking_begin();
    poll(&pf, 1, 100);
    proc_blocking_end(tok);
  }
  UNPROTECT(1);
  return TT;
}

void init_prims_proc(void) {
  defprim("%PROCESS-CREATE", p_process_create, 2, 2);
  defprim("%PROCESS-CURRENT", p_process_current, 0, 0);
  defprim("%PROCESS-SET-CURRENT", p_process_set_current, 1, 1);
  defprim("%PROCESS-LIST", p_process_list, 0, 0);
  defprim("%PROCESS-ALIVE-P", p_process_alive, 1, 1);
  defprim("%PROCESS-INFO", p_process_info, 1, 1);
  defprim("%PROCESS-YIELD", p_process_yield, 0, 0);
  defprim("%PROCESS-WAIT-YIELD", p_process_wait_yield, 0, 0);
  defprim("%PROCESS-SET-WAITING", p_process_set_waiting, 1, 1);
  defprim("%PROCESS-INTERRUPT", p_process_interrupt, 1, 1);
  defprim("%PROCESS-ARREST", p_process_arrest, 2, 2);
  defprim("%PROCESS-COUNT", p_process_count, 0, 0);
  defprim("%PREEMPT-INHIBIT", p_preempt_inhibit, 1, 1);
  defprim("%STDIN-WAIT", p_stdin_wait, 1, 1);
}
