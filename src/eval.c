/* eval.c -- dynamic state, non-local exits, errors, the stage-0 evaluator
 * and generic function application.
 *
 * The evaluator is a straightforward S-expression interpreter.  It exists to
 * bootstrap the system: the Lisp-written compiler first runs on it (stage 0),
 * and afterwards compiled code runs on the bytecode VM (vm.c).  Both kinds of
 * function are callable from each other through apply_stack().
 */
#include "neogenera.h"

static Obj initial_vstack[VSTACK_SIZE];
Obj *vstack = initial_vstack;
int vsp, vstack_size = VSTACK_SIZE;
static SpecBind initial_specpdl[SPEC_SIZE];
SpecBind *specpdl = initial_specpdl;
int spsp, spec_size = SPEC_SIZE;
static VFrame initial_vframes[VFRAME_SIZE];
VFrame *vframes = initial_vframes;
int vfp, vframe_size = VFRAME_SIZE;
Obj mv_vals[MAX_MV];
int mv_count = 1;
Frame *frame_top;
int eval_depth;
char *ng_stack_limit = NULL;
/* Each stack keeps a reserve above its soft limit for the error handling of an overflow (an eighth of a small
   stack, at most the fixed amounts the initial process uses). */
static int reserve(int size, int most) { int r = size / 8; return r < most ? r : most; }
int ng_vfp_limit = VFRAME_SIZE - 8192, ng_vsp_limit = VSTACK_SIZE - 32768;
void ng_vm_overflow(void) { ng_vfp_limit = vframe_size - 2; ng_vsp_limit = vstack_size - reserve(vstack_size, 4096) / 2; lisp_error("Control stack exhausted (VM)"); }
int ng_root_limit = MAX_ROOTS - 8192;
void ng_root_overflow(void) { eval_depth = 0; ng_root_limit = root_stack_size - 16; lisp_error("Control stack exhausted (root stack)"); }
void reset_stack_limits(void) {
  ng_root_limit = root_stack_size - reserve(root_stack_size, 8192);
  ng_vfp_limit = vframe_size - reserve(vframe_size, 8192); ng_vsp_limit = vstack_size - reserve(vstack_size, 32768);
}
void ng_stack_overflow(void) { eval_depth = 0; lisp_error("Control stack exhausted (C stack)"); }

#define MAX_EVAL_DEPTH 60000

/* ---- dynamic binding -------------------------------------------------- */
void dynbind(Obj sym, Obj val) {
  if (spsp >= spec_size) lisp_error("special binding stack exhausted");
  specpdl[spsp].sym = sym;
  specpdl[spsp].old = SYM_VALUE(sym);
  spsp++;
  SYM_VALUE(sym) = val; WB(sym, val);
}

void unbind_to(int h) {
  while (spsp > h) { spsp--; SYM_VALUE(specpdl[spsp].sym) = specpdl[spsp].old; WB(specpdl[spsp].sym, specpdl[spsp].old); }
}

/* ---- frames and non-local exit ---------------------------------------- */
void frame_push(Frame *f, int kind, Obj tag) {
  f->prev = frame_top; f->kind = kind; f->tag = tag;
  f->value = NIL; f->cleanup = NIL; f->mvlist = UNBOUND;
  f->spsp = spsp; f->vsp = vsp; f->vfp = vfp; f->nroots = nroots; f->eval_depth = eval_depth; f->gci = gc_inhibit;
  frame_top = f;
}

void frame_pop(Frame *f) { frame_top = f->prev; }

Frame *find_frame(int kind, Obj tag) {
  for (Frame *f = frame_top; f; f = f->prev)
    if (f->kind == kind && (kind == F_TOP || f->tag == tag)) return f;
  return NULL;
}

static void restore_state(Frame *f) {
  reset_stack_limits();                      /* closes the error-handling reserves again */
  unbind_to(f->spsp); vsp = f->vsp; vfp = f->vfp; nroots = f->nroots; eval_depth = f->eval_depth; gc_inhibit = f->gci;
}

void throw_to(Frame *target, Obj value) {
  target->value = value;
  while (frame_top != target) {
    Frame *f = frame_top;
    if (!f) { fprintf(stderr, "internal error: throw target not on frame chain\n"); abort(); }
    frame_top = f->prev;
    if (f->kind == F_UNWIND) {
      restore_state(f);
      Obj saved_mv = target->mvlist;     /* target is still scanned by the GC */
      (void)saved_mv;
      Obj cleanup = f->cleanup;
      PUSH(cleanup);
      apply_stack(0);
    }
  }
  restore_state(target);
  longjmp(target->jb, 1);
}

static Obj mv_to_list(void) {
  Obj l = NIL;
  for (int i = mv_count - 1; i >= 0; i--) l = cons(mv_vals[i], l);
  return l;
}

void throw_values(Frame *target, Obj value) {
  target->mvlist = (mv_count != 1) ? mv_to_list() : UNBOUND;
  throw_to(target, value);
}

Obj values_list(Obj list) {
  int n = 0;
  Obj first = NIL;
  for (Obj l = list; CONSP(l); l = CDR(l)) {
    if (n >= MAX_MV) lisp_error("too many values");
    mv_vals[n++] = CAR(l);
  }
  if (n > 0) first = mv_vals[0];
  mv_count = n;
  return first;
}

Obj frame_result(Frame *f) {
  Obj v = f->value;
  if (f->mvlist != UNBOUND) { Obj l = f->mvlist; f->mvlist = UNBOUND; return values_list(l); }
  mv_count = 1;
  return v;
}

/* ---- errors ----------------------------------------------------------- */
static void signal_error_msg(const char *msg) {
  Obj fn = SYM_FUNC(Q_signal_error);
  if (fn != NIL && SYM_VALUE(Q_in_signal) == NIL) {
    int ssp = spsp;
    dynbind(Q_in_signal, TT);
    Obj m = make_cstring(msg);
    PUSH(fn); PUSH(m);
    apply_stack(1);
    unbind_to(ssp);
  }
  Frame *top = find_frame(F_TOP, NIL);
  if (!top) { fprintf(stderr, "Error: %s\n", msg); exit(1); }
  throw_to(top, make_cstring(msg));
}

void lisp_error(const char *fmt, ...) {
  char buf[4096];
  va_list ap; va_start(ap, fmt);
  vsnprintf(buf, sizeof buf, fmt, ap);
  va_end(ap);
  signal_error_msg(buf);
  abort();
}

void type_error(const char *what, Obj got) {
  Buf b; buf_init(&b);
  print_to_buf(&b, got, 1, 0);
  char msg[2048];
  snprintf(msg, sizeof msg, "The value %.1000s is not of type %s", b.buf ? b.buf : "?", what);
  buf_free(&b);
  lisp_error("%s", msg);
}

/* ---- helpers ---------------------------------------------------------- */
static int is_special_sym(Obj s) { return (UNFIX(SYM_FLAGS(s)) & SF_SPECIAL) != 0; }

static Obj lookup_var_cell(Obj sym, Obj vars) {
  for (; CONSP(vars); vars = CDR(vars)) if (CAR(CAR(vars)) == sym) return CAR(vars);
  return NIL;
}

static Obj lookup_fun_cell(Obj name, Obj funs) {
  for (; CONSP(funs); funs = CDR(funs)) if (CAR(CAR(funs)) == name) return CAR(funs);
  return NIL;
}

static Obj lookup_tagged(Obj marker, Obj name, Obj funs) {
  for (; CONSP(funs); funs = CDR(funs)) {
    Obj k = CAR(CAR(funs));
    if (CONSP(k) && CAR(k) == marker && CDR(k) == name) return CAR(funs);
  }
  return NIL;
}

static int is_ll_keyword(Obj s) {
  return s == Q_optional || s == Q_rest || s == Q_body || s == Q_key || s == Q_aux ||
         s == Q_allow || s == Q_whole || s == Q_environment;
}

static Obj make_evalfn(Obj name, Obj ll, Obj body, Obj vars, Obj funs) {
  Obj f = make_obj(T_EVALFN, EF_SLOTS);
  SLOT(f, EF_NAME) = name; SLOT(f, EF_LAMBDA) = ll; SLOT(f, EF_BODY) = body;
  SLOT(f, EF_VARS) = vars; SLOT(f, EF_FUNS) = funs;
  long n = 0; Obj l = ll;
  while (CONSP(l) && SYMBOLP(CAR(l)) && !is_ll_keyword(CAR(l))) { n++; l = CDR(l); }
  SLOT(f, EF_SIMPLE) = (l == NIL) ? FIX(n) : FIX(-1);
  return f;
}

static int declared_special(Obj sym, Obj body) {
  for (; CONSP(body); body = CDR(body)) {
    Obj f = CAR(body);
    if (!(CONSP(f) && CAR(f) == Q_declare)) { if (STRINGP(f)) continue; break; }
    for (Obj d = CDR(f); CONSP(d); d = CDR(d)) {
      Obj spec = CAR(d);
      if (CONSP(spec) && CAR(spec) == Q_special)
        for (Obj s = CDR(spec); CONSP(s); s = CDR(s)) if (CAR(s) == sym) return 1;
    }
  }
  return 0;
}

static Obj eval_body(Obj body, Obj vars, Obj funs) {
  int saved = nroots;
  PROTECT(body); PROTECT(vars); PROTECT(funs);
  Obj r = NIL;
  mv_count = 1;
  while (CONSP(body)) {
    r = eval(CAR(body), vars, funs);
    body = CDR(body);
  }
  nroots = saved;
  return r;
}

/* bind one variable into the lexical alist (or dynamically if special) */
static Obj bind_var(Obj sym, Obj val, Obj vars, Obj body) {
  if (!SYMBOLP(sym) || sym == NIL || sym == TT) lisp_error("Bad variable name in binding");
  if (is_special_sym(sym) || (body != NIL && declared_special(sym, body))) { dynbind(sym, val); return vars; }
  return cons(cons(sym, val), vars);
}

/* General lambda-list binder.  `destructure` allows nested patterns, &whole
 * and dotted tails (macro lambda lists).  Returns the extended alist. */
static Obj bind_ll(Obj ll, Obj args, Obj vars, Obj funs, int destructure, Obj whole, Obj body) {
  int saved = nroots;
  Obj p = ll, item = NIL, val = NIL, kargs = NIL, spec = NIL;
  Obj var = NIL, def = NIL, svar = NIL, kw = NIL;
  PROTECT(ll); PROTECT(args); PROTECT(vars); PROTECT(funs); PROTECT(whole); PROTECT(body);
  PROTECT(p); PROTECT(item); PROTECT(val); PROTECT(kargs); PROTECT(spec);
  PROTECT(var); PROTECT(def); PROTECT(svar); PROTECT(kw);
  enum { REQ, OPT, REST, KEY, AUX } mode = REQ;
  int allow_other = 0, saw_key = 0, saw_rest = 0;
  Obj known[48]; int nknown = 0;

  while (CONSP(p)) {
    item = CAR(p); p = CDR(p);
    if (SYMBOLP(item) && is_ll_keyword(item)) {
      if (item == Q_optional) { mode = OPT; continue; }
      if (item == Q_rest || item == Q_body) {
        if (!CONSP(p)) lisp_error("Malformed lambda list: nothing after &rest");
        vars = bind_var(CAR(p), args, vars, body); p = CDR(p);
        mode = REST; saw_rest = 1; continue;
      }
      if (item == Q_key) { mode = KEY; saw_key = 1; kargs = args; if (list_length(kargs) < 0 || (list_length(kargs) & 1)) lisp_error("Odd number of keyword arguments"); continue; }
      if (item == Q_allow) { allow_other = 1; continue; }
      if (item == Q_aux) { mode = AUX; continue; }
      if (item == Q_whole) { if (CONSP(p)) { vars = bind_var(CAR(p), whole == NIL ? args : whole, vars, body); p = CDR(p); } continue; }
      if (item == Q_environment) { if (CONSP(p)) { vars = bind_var(CAR(p), NIL, vars, body); p = CDR(p); } continue; }
    }
    switch (mode) {
    case REQ:
      if (!CONSP(args)) lisp_error("Too few arguments in call (lambda list %s)", STR_C(print_to_string(ll, 1, 0)));
      val = CAR(args); args = CDR(args);
      if (CONSP(item) && destructure) vars = bind_ll(item, val, vars, funs, 1, NIL, body);
      else vars = bind_var(item, val, vars, body);
      break;
    case OPT: {
      var = item; def = NIL; svar = NIL;
      if (CONSP(item)) { var = CAR(item); spec = CDR(item); if (CONSP(spec)) { def = CAR(spec); if (CONSP(CDR(spec))) svar = CAR(CDR(spec)); } }
      int supplied = CONSP(args);
      if (supplied) { val = CAR(args); args = CDR(args); }
      else val = (def == NIL) ? NIL : eval(def, vars, funs);
      if (CONSP(var) && destructure) vars = bind_ll(var, val, vars, funs, 1, NIL, body);
      else vars = bind_var(var, val, vars, body);
      if (svar != NIL) vars = bind_var(svar, BOOL(supplied), vars, body);
      break;
    }
    case REST:
      lisp_error("Malformed lambda list: unexpected item after &rest");
    case KEY: {
      var = item; def = NIL; svar = NIL; kw = NIL;
      if (CONSP(item)) {
        var = CAR(item); spec = CDR(item);
        if (CONSP(spec)) { def = CAR(spec); if (CONSP(CDR(spec))) svar = CAR(CDR(spec)); }
        if (CONSP(var)) { kw = CAR(var); var = CAR(CDR(var)); }
      }
      if (kw == NIL) kw = intern(STR_C(SYM_NAME(var)), PKG_KEYWORD);
      if (nknown < 48) { known[nknown] = kw; PROTECT(known[nknown]); nknown++; }
      int supplied = 0;
      for (Obj q = kargs; CONSP(q); q = CDR(CDR(q))) if (CAR(q) == kw) { val = CAR(CDR(q)); supplied = 1; break; }
      if (!supplied) val = (def == NIL) ? NIL : eval(def, vars, funs);
      vars = bind_var(var, val, vars, body);
      if (svar != NIL) vars = bind_var(svar, BOOL(supplied), vars, body);
      break;
    }
    case AUX: {
      var = item; def = NIL;
      if (CONSP(item)) { var = CAR(item); if (CONSP(CDR(item))) def = CAR(CDR(item)); }
      val = (def == NIL) ? NIL : eval(def, vars, funs);
      vars = bind_var(var, val, vars, body);
      break;
    }
    }
  }
  if (p != NIL && SYMBOLP(p) && destructure) { vars = bind_var(p, args, vars, body); args = NIL; saw_rest = 1; }
  if (saw_key && !allow_other) {
    /* unknown keyword check */
    int allow_flag = 0;
    for (Obj q = kargs; CONSP(q) && CONSP(CDR(q)); q = CDR(CDR(q)))
      if (CAR(q) == intern_kw("ALLOW-OTHER-KEYS") && CAR(CDR(q)) != NIL) allow_flag = 1;
    if (!allow_flag)
      for (Obj q = kargs; CONSP(q); q = CDR(CDR(q))) {
        int ok = 0;
        for (int i = 0; i < nknown; i++) if (known[i] == CAR(q)) { ok = 1; break; }
        if (CAR(q) == intern_kw("ALLOW-OTHER-KEYS")) ok = 1;
        if (!ok) lisp_error("Unknown &KEY argument: %s", STR_C(print_to_string(CAR(q), 1, 0)));
      }
  }
  if (!saw_rest && !saw_key && args != NIL)
    lisp_error("Too many arguments in call (lambda list %s)", STR_C(print_to_string(ll, 1, 0)));
  Obj result = vars;
  nroots = saved;
  return result;
}

/* ---- application ------------------------------------------------------ */
Obj resolve_function(Obj f) {
  if (SYMBOLP(f) && f != NIL) {
    Obj fn = SYM_FUNC(f);
    if (fn == NIL) lisp_error("The function %s is undefined", STR_C(SYM_NAME(f)));
    if (CONSP(fn)) lisp_error("%s is a macro, not a function", STR_C(SYM_NAME(f)));
    return fn;
  }
  return f;
}

static Obj apply_evalfn(int n) {
  Obj fn = vstack[vsp - n - 1];
  int saved = nroots, ssp = spsp, base = vsp - n - 1;
  Obj vars = SLOT(fn, EF_VARS), funs = SLOT(fn, EF_FUNS), body = SLOT(fn, EF_BODY);
  PROTECT(vars); PROTECT(funs); PROTECT(body);
  long simple = (long)UNFIX(SLOT(fn, EF_SIMPLE));
  if (simple == -2) {                         /* macro function: (form env) */
    if (n != 2) lisp_error("macro function called with wrong arguments");
    Obj form = vstack[vsp - 2];
    Obj args = CONSP(form) ? CDR(form) : NIL;
    vars = bind_ll(SLOT(fn, EF_LAMBDA), args, vars, funs, 1, form, body);
  } else if (simple >= 0) {
    if (n != simple) lisp_error("Wrong number of arguments to %s: expected %ld, got %d",
                                SLOT(fn, EF_NAME) == NIL ? "#<anonymous function>" : STR_C(print_to_string(SLOT(fn, EF_NAME), 1, 0)), simple, n);
    Obj ll = SLOT(fn, EF_LAMBDA);
    for (int i = 0; i < n; i++, ll = CDR(ll)) vars = bind_var(CAR(ll), vstack[base + 1 + i], vars, body);
  } else {
    Obj args = NIL;
    for (int i = 1; i <= n; i++) args = cons(vstack[vsp - i], args);
    vars = bind_ll(SLOT(fn, EF_LAMBDA), args, vars, funs, 0, NIL, body);
  }
  Obj r = eval_body(body, vars, funs);
  unbind_to(ssp);
  nroots = saved;
  vsp = base;
  return r;
}

Obj apply_stack(int n) {
  Obj fn = vstack[vsp - n - 1];
  if (SYMBOLP(fn)) { fn = resolve_function(fn); vstack[vsp - n - 1] = fn; }
  if (!OBJP(fn)) type_error("FUNCTION", fn);
  switch (OBJ_TYPE(fn)) {
  case T_PRIM: {
    PrimEntry *pe = &prim_table[UNFIX(SLOT(fn, PR_INDEX))];
    if (n < pe->min || (pe->max >= 0 && n > pe->max))
      lisp_error("Wrong number of arguments to %s: got %d", pe->name, n);
    Obj r = pe->fn(n, &vstack[vsp - n]);
    if (!pe->mv) mv_count = 1;
    vsp -= n + 1;
    return r;
  }
  case T_EVALFN: {
    STACK_CHECK(); if (++eval_depth > MAX_EVAL_DEPTH) { eval_depth = 0; lisp_error("Control stack exhausted"); }
    Obj r = apply_evalfn(n);
    eval_depth--;
    return r;
  }
  case T_CLOSURE: return vm_apply_closure(n);
  case T_STRUCT: {
    /* A structure is callable if Lisp says so (funcallable CLOS instances, Flavors instances): the function
       %FUNCALL-INSTANCE is called with the instance and the arguments.  It signals the usual error otherwise. */
    Obj hook = SYM_FUNC(Q_funcall_instance);
    if (hook == NIL || CONSP(hook)) type_error("FUNCTION", fn);
    int base = vsp - n - 1;
    PUSH(NIL);
    for (int i = vsp - 1; i > base; i--) vstack[i] = vstack[i - 1];
    vstack[base] = hook;
    return apply_stack(n + 1);
  }
  default: type_error("FUNCTION", fn);
  }
}

Obj call0(Obj fn) { PUSH(fn); return apply_stack(0); }
Obj call1(Obj fn, Obj a) { PUSH(fn); PUSH(a); return apply_stack(1); }
Obj call2(Obj fn, Obj a, Obj b) { PUSH(fn); PUSH(a); PUSH(b); return apply_stack(2); }
Obj call3(Obj fn, Obj a, Obj b, Obj c) { PUSH(fn); PUSH(a); PUSH(b); PUSH(c); return apply_stack(3); }

Obj apply_list(Obj fn, Obj args) {
  PUSH(fn);
  int n = 0;
  for (; CONSP(args); args = CDR(args)) { PUSH(CAR(args)); n++; }
  return apply_stack(n);
}

/* ---- special forms ---------------------------------------------------- */
#define RET(x) do { Obj _r = (x); nroots = saved; return _r; } while (0)

static Obj sf_let(Obj form, Obj vars, Obj funs, int star) {
  int saved = nroots, ssp = spsp, svsp = vsp;
  Obj bindings = CONSP(CDR(form)) ? CAR(CDR(form)) : NIL;
  Obj body = CONSP(CDR(form)) ? CDR(CDR(form)) : NIL;
  Obj newvars = vars, b = NIL, bd = NIL, v = NIL;
  PROTECT(form); PROTECT(vars); PROTECT(funs); PROTECT(bindings); PROTECT(body);
  PROTECT(newvars); PROTECT(b); PROTECT(bd); PROTECT(v);
  if (!star) {
    int n = 0;
    for (b = bindings; CONSP(b); b = CDR(b)) {
      bd = CAR(b);
      v = NIL;
      if (CONSP(bd) && CONSP(CDR(bd))) v = eval(CAR(CDR(bd)), vars, funs);
      PUSH(v); n++;
    }
    int i = 0;
    for (b = bindings; CONSP(b); b = CDR(b), i++) {
      bd = CAR(b);
      Obj sym = CONSP(bd) ? CAR(bd) : bd;
      newvars = bind_var(sym, vstack[svsp + i], newvars, body);
    }
    vsp = svsp;
  } else {
    for (b = bindings; CONSP(b); b = CDR(b)) {
      bd = CAR(b);
      v = NIL;
      if (CONSP(bd) && CONSP(CDR(bd))) v = eval(CAR(CDR(bd)), newvars, funs);
      Obj sym = CONSP(bd) ? CAR(bd) : bd;
      newvars = bind_var(sym, v, newvars, body);
    }
  }
  Obj r = eval_body(body, newvars, funs);
  unbind_to(ssp);
  RET(r);
}

static Obj sf_flet(Obj form, Obj vars, Obj funs, int labels) {
  int saved = nroots;
  Obj bindings = CAR(CDR(form)), body = CDR(CDR(form)), newfuns = funs, b = NIL, spec = NIL, cell = NIL, entries = NIL;
  PROTECT(form); PROTECT(vars); PROTECT(funs); PROTECT(bindings); PROTECT(body);
  PROTECT(newfuns); PROTECT(b); PROTECT(spec); PROTECT(cell); PROTECT(entries);
  if (labels) {
    for (b = bindings; CONSP(b); b = CDR(b)) {
      spec = CAR(b);
      cell = cons(fn_symbol(CAR(spec)), NIL);
      newfuns = cons(cell, newfuns);
    }
    entries = newfuns;
    for (b = bindings; CONSP(b); b = CDR(b)) {
      spec = CAR(b);
      /* entries are in reverse binding order */
      Obj fnname = fn_symbol(CAR(spec));
      Obj e = lookup_fun_cell(fnname, entries);
      Obj bname = SYMBOLP(CAR(spec)) ? CAR(spec) : CAR(CDR(CAR(spec)));
      Obj blk = cons(Q_block, cons(bname, CDR(CDR(spec))));
      CDR(e) = make_evalfn(fnname, CAR(CDR(spec)), list1(blk), vars, newfuns); WB(e, CDR(e));
    }
  } else {
    for (b = bindings; CONSP(b); b = CDR(b)) {
      spec = CAR(b);
      Obj fnname = fn_symbol(CAR(spec));
      Obj bname = SYMBOLP(CAR(spec)) ? CAR(spec) : CAR(CDR(CAR(spec)));
      Obj blk = cons(Q_block, cons(bname, CDR(CDR(spec))));
      cell = cons(fnname, make_evalfn(fnname, CAR(CDR(spec)), list1(blk), vars, funs));
      newfuns = cons(cell, newfuns);
    }
  }
  RET(eval_body(body, vars, newfuns));
}

static Obj sf_macrolet(Obj form, Obj vars, Obj funs) {
  int saved = nroots;
  Obj bindings = CAR(CDR(form)), body = CDR(CDR(form)), newfuns = funs, b = NIL, spec = NIL, fn = NIL;
  PROTECT(form); PROTECT(vars); PROTECT(funs); PROTECT(bindings); PROTECT(body);
  PROTECT(newfuns); PROTECT(b); PROTECT(spec); PROTECT(fn);
  for (b = bindings; CONSP(b); b = CDR(b)) {
    spec = CAR(b);
    fn = make_evalfn(CAR(spec), CAR(CDR(spec)), CDR(CDR(spec)), vars, funs);
    SLOT(fn, EF_SIMPLE) = FIX(-2);
    newfuns = cons(cons(CAR(spec), cons(Q_macro_marker, fn)), newfuns);
  }
  RET(eval_body(body, vars, newfuns));
}

static Obj skip_docstring(Obj body) {
  if (CONSP(body) && STRINGP(CAR(body)) && CONSP(CDR(body))) return CDR(body);
  return body;
}

static Obj sf_block(Obj form, Obj vars, Obj funs) {
  int saved = nroots;
  PROTECT(form); PROTECT(vars); PROTECT(funs);
  Obj token = cons(NIL, NIL);
  PROTECT(token);
  Obj name = CAR(CDR(form));
  Obj nf = cons(cons(cons(Q_block, name), token), funs);
  PROTECT(nf);
  Frame f;
  frame_push(&f, F_BLOCK, token);
  Obj r;
  if (setjmp(f.jb) == 0) {
    r = eval_body(CDR(CDR(form)), vars, nf);
    frame_pop(&f);
  } else {
    frame_top = f.prev;
    r = frame_result(&f);
  }
  RET(r);
}

static Obj sf_tagbody(Obj form, Obj vars, Obj funs) {
  int saved = nroots;
  Obj token = cons(NIL, NIL), forms = CDR(form), newfuns = funs, cur = NIL, l = NIL;
  PROTECT(form); PROTECT(vars); PROTECT(funs); PROTECT(token); PROTECT(forms);
  PROTECT(newfuns); PROTECT(cur); PROTECT(l);
  for (l = forms; CONSP(l); l = CDR(l))
    if (!CONSP(CAR(l))) newfuns = cons(cons(cons(Q_go, CAR(l)), cons(token, CDR(l))), newfuns);
  cur = forms;
  Frame f;
  frame_push(&f, F_TAGBODY, token);
  for (;;) {
    if (setjmp(f.jb) == 0) {
      while (CONSP(cur)) {
        Obj x = CAR(cur);
        cur = CDR(cur);
        if (CONSP(x)) eval(x, vars, newfuns);
      }
      frame_pop(&f);
      break;
    } else {
      cur = f.value;
    }
  }
  mv_count = 1;
  RET(NIL);
}

static Obj sf_unwind_protect(Obj form, Obj vars, Obj funs) {
  int saved = nroots;
  Obj cleanup_forms = CDR(CDR(form)), cleanup = NIL, r = NIL, mvl = UNBOUND;
  PROTECT(form); PROTECT(vars); PROTECT(funs); PROTECT(cleanup_forms);
  PROTECT(cleanup); PROTECT(r); PROTECT(mvl);
  cleanup = make_evalfn(NIL, NIL, cleanup_forms, vars, funs);
  Frame f;
  frame_push(&f, F_UNWIND, NIL);
  f.cleanup = cleanup;
  r = eval(CAR(CDR(form)), vars, funs);
  if (mv_count != 1) mvl = mv_to_list();
  frame_pop(&f);
  eval_body(cleanup_forms, vars, funs);
  if (mvl != UNBOUND) r = values_list(mvl); else mv_count = 1;
  RET(r);
}

static Obj sf_catch(Obj form, Obj vars, Obj funs) {
  int saved = nroots;
  PROTECT(form); PROTECT(vars); PROTECT(funs);
  Obj tag = eval(CAR(CDR(form)), vars, funs);
  PROTECT(tag);
  Frame f;
  frame_push(&f, F_CATCH, tag);
  Obj r;
  if (setjmp(f.jb) == 0) {
    r = eval_body(CDR(CDR(form)), vars, funs);
    frame_pop(&f);
  } else {
    frame_top = f.prev;
    r = frame_result(&f);
  }
  RET(r);
}

static Obj sf_mv_call(Obj form, Obj vars, Obj funs) {
  int saved = nroots, svsp = vsp;
  Obj a = CDR(CDR(form));
  PROTECT(form); PROTECT(vars); PROTECT(funs); PROTECT(a);
  Obj fn = eval(CAR(CDR(form)), vars, funs);
  if (SYMBOLP(fn)) fn = resolve_function(fn);
  PUSH(fn);
  int n = 0;
  while (CONSP(a)) {
    Obj v = eval(CAR(a), vars, funs);
    if (mv_count == 1) { PUSH(v); n++; }
    else { int c = mv_count; for (int i = 0; i < c; i++) { PUSH(mv_vals[i]); n++; } }
    a = CDR(a);
  }
  (void)svsp;
  RET(apply_stack(n));
}

static Obj sf_mv_prog1(Obj form, Obj vars, Obj funs) {
  int saved = nroots;
  Obj r = NIL, mvl = UNBOUND;
  PROTECT(form); PROTECT(vars); PROTECT(funs); PROTECT(r); PROTECT(mvl);
  r = eval(CAR(CDR(form)), vars, funs);
  if (mv_count != 1) mvl = mv_to_list();
  eval_body(CDR(CDR(form)), vars, funs);
  if (mvl != UNBOUND) r = values_list(mvl); else mv_count = 1;
  RET(r);
}

static Obj sf_progv(Obj form, Obj vars, Obj funs) {
  int saved = nroots, ssp = spsp;
  Obj syms = NIL, vals = NIL;
  PROTECT(form); PROTECT(vars); PROTECT(funs); PROTECT(syms); PROTECT(vals);
  syms = eval(CAR(CDR(form)), vars, funs);
  vals = eval(CAR(CDR(CDR(form))), vars, funs);
  for (; CONSP(syms); syms = CDR(syms)) {
    dynbind(CAR(syms), CONSP(vals) ? CAR(vals) : UNBOUND);
    if (CONSP(vals)) vals = CDR(vals);
  }
  Obj r = eval_body(CDR(CDR(CDR(form))), vars, funs);
  unbind_to(ssp);
  RET(r);
}

static void assign_var(Obj sym, Obj val, Obj vars) {
  Obj cell = lookup_var_cell(sym, vars);
  if (cell != NIL) { CDR(cell) = val; WB(cell, val); return; }
  if (UNFIX(SYM_FLAGS(sym)) & SF_CONSTANT) lisp_error("Cannot assign to the constant %s", STR_C(SYM_NAME(sym)));
  SYM_VALUE(sym) = val; WB(sym, val);
}

static Obj eval_special(int sf, Obj form, Obj vars, Obj funs) {
  int saved = nroots;
  Obj r = NIL;
  switch (sf) {
  case SFID_QUOTE: mv_count = 1; return CAR(CDR(form));
  case SFID_IF: {
    PROTECT(form); PROTECT(vars); PROTECT(funs);
    Obj t = eval(CAR(CDR(form)), vars, funs);
    Obj rest = CDR(CDR(form));
    if (t != NIL) r = eval(CAR(rest), vars, funs);
    else if (CONSP(CDR(rest))) r = eval(CAR(CDR(rest)), vars, funs);
    else { r = NIL; mv_count = 1; }
    RET(r);
  }
  case SFID_PROGN: case SFID_LOCALLY:
    return eval_body(CDR(form), vars, funs);
  case SFID_LET: return sf_let(form, vars, funs, 0);
  case SFID_LETSTAR: return sf_let(form, vars, funs, 1);
  case SFID_SETQ: {
    Obj l = CDR(form), sym = NIL, v = NIL;
    PROTECT(form); PROTECT(vars); PROTECT(funs); PROTECT(l); PROTECT(sym); PROTECT(v);
    mv_count = 1;
    while (CONSP(l)) {
      sym = CAR(l);
      if (!SYMBOLP(sym)) lisp_error("SETQ: not a symbol");
      if (!CONSP(CDR(l))) lisp_error("SETQ: odd number of arguments");
      v = eval(CAR(CDR(l)), vars, funs);
      assign_var(sym, v, vars);
      l = CDR(CDR(l));
    }
    mv_count = 1;
    RET(v);
  }
  case SFID_FUNCTION: {
    Obj arg = CAR(CDR(form));
    mv_count = 1;
    if (CONSP(arg) && CAR(arg) == Q_lambda)
      return make_evalfn(NIL, CAR(CDR(arg)), CDR(CDR(arg)), vars, funs);
    Obj sym = fn_symbol(arg);
    Obj cell = lookup_fun_cell(sym, funs);
    Obj f = (cell != NIL) ? CDR(cell) : SYM_FUNC(sym);
    if (f == NIL) lisp_error("The function %s is undefined", STR_C(SYM_NAME(sym)));
    if (CONSP(f)) lisp_error("%s is a macro, not a function", STR_C(SYM_NAME(sym)));
    return f;
  }
  case SFID_LAMBDA:
    mv_count = 1;
    return make_evalfn(NIL, CAR(CDR(form)), CDR(CDR(form)), vars, funs);
  case SFID_FLET: return sf_flet(form, vars, funs, 0);
  case SFID_LABELS: return sf_flet(form, vars, funs, 1);
  case SFID_MACROLET: return sf_macrolet(form, vars, funs);
  case SFID_BLOCK: return sf_block(form, vars, funs);
  case SFID_RETURN_FROM: {
    PROTECT(form); PROTECT(vars); PROTECT(funs);
    Obj name = CAR(CDR(form));
    PROTECT(name);
    Obj entry = lookup_tagged(Q_block, name, funs);
    if (entry == NIL) lisp_error("RETURN-FROM: no block named %s", STR_C(print_to_string(name, 1, 0)));
    Obj token = CDR(entry);
    PROTECT(token);
    Obj v = CONSP(CDR(CDR(form))) ? eval(CAR(CDR(CDR(form))), vars, funs) : (mv_count = 1, NIL);
    Frame *t = find_frame(F_BLOCK, token);
    if (!t) lisp_error("RETURN-FROM: the block %s is no longer active", STR_C(print_to_string(name, 1, 0)));
    throw_values(t, v);
  }
  case SFID_TAGBODY: return sf_tagbody(form, vars, funs);
  case SFID_GO: {
    Obj tag = CAR(CDR(form));
    Obj entry = lookup_tagged(Q_go, tag, funs);
    if (entry == NIL) lisp_error("GO: no tag named %s", STR_C(print_to_string(tag, 1, 0)));
    Obj token = CAR(CDR(entry)), rest = CDR(CDR(entry));
    Frame *t = find_frame(F_TAGBODY, token);
    if (!t) lisp_error("GO: the tagbody for %s is no longer active", STR_C(print_to_string(tag, 1, 0)));
    t->mvlist = UNBOUND;
    throw_to(t, rest);
  }
  case SFID_CATCH: return sf_catch(form, vars, funs);
  case SFID_UNWIND_PROTECT: return sf_unwind_protect(form, vars, funs);
  case SFID_MV_CALL: return sf_mv_call(form, vars, funs);
  case SFID_MV_PROG1: return sf_mv_prog1(form, vars, funs);
  case SFID_PROGV: return sf_progv(form, vars, funs);
  case SFID_THE: return eval(CAR(CDR(CDR(form))), vars, funs);
  case SFID_LTV: return eval(CAR(CDR(form)), vars, funs);
  case SFID_DECLARE: mv_count = 1; return NIL;
  case SFID_AND: {
    Obj l = CDR(form);
    PROTECT(form); PROTECT(vars); PROTECT(funs); PROTECT(l);
    r = TT; mv_count = 1;
    while (CONSP(l)) {
      if (CDR(l) == NIL) { r = eval(CAR(l), vars, funs); break; }
      r = eval(CAR(l), vars, funs);
      if (r == NIL) { mv_count = 1; break; }
      l = CDR(l);
    }
    RET(r);
  }
  case SFID_OR: {
    Obj l = CDR(form);
    PROTECT(form); PROTECT(vars); PROTECT(funs); PROTECT(l);
    r = NIL; mv_count = 1;
    while (CONSP(l)) {
      if (CDR(l) == NIL) { r = eval(CAR(l), vars, funs); break; }
      r = eval(CAR(l), vars, funs);
      if (r != NIL) { mv_count = 1; break; }
      l = CDR(l);
    }
    RET(r);
  }
  case SFID_WHEN: case SFID_UNLESS: {
    PROTECT(form); PROTECT(vars); PROTECT(funs);
    Obj t = eval(CAR(CDR(form)), vars, funs);
    if ((t != NIL) == (sf == SFID_WHEN)) r = eval_body(CDR(CDR(form)), vars, funs);
    else { r = NIL; mv_count = 1; }
    RET(r);
  }
  case SFID_COND: {
    Obj l = CDR(form), clause = NIL, t = NIL;
    PROTECT(form); PROTECT(vars); PROTECT(funs); PROTECT(l); PROTECT(clause); PROTECT(t);
    r = NIL; mv_count = 1;
    while (CONSP(l)) {
      clause = CAR(l);
      t = eval(CAR(clause), vars, funs);
      if (t != NIL) { r = (CDR(clause) == NIL) ? t : eval_body(CDR(clause), vars, funs); break; }
      l = CDR(l);
    }
    RET(r);
  }
  case SFID_EVAL_WHEN: {
    Obj sits = CAR(CDR(form));
    for (; CONSP(sits); sits = CDR(sits))
      if (CAR(sits) == Q_execute || (SYMBOLP(CAR(sits)) && !strcmp(STR_C(SYM_NAME(CAR(sits))), "EVAL")))
        return eval_body(CDR(CDR(form)), vars, funs);
    mv_count = 1;
    return NIL;
  }
  case SFID_DEFUN: {
    PROTECT(form); PROTECT(vars); PROTECT(funs);
    Obj name = CAR(CDR(form)), sym = fn_symbol(name);
    sym_lock_check(sym, "define a function on");
    Obj ll = CAR(CDR(CDR(form))), body = skip_docstring(CDR(CDR(CDR(form))));
    Obj blk = cons(Q_block, cons(SYMBOLP(name) ? name : sym, body));
    SYM_FUNC(sym) = make_evalfn(sym, ll, list1(blk), vars, funs); WB(sym, SYM_FUNC(sym));
    mv_count = 1;
    RET(sym);
  }
  case SFID_DEFMACRO: {
    PROTECT(form); PROTECT(vars); PROTECT(funs);
    Obj sym = CAR(CDR(form));
    sym_lock_check(sym, "define a macro on");
    Obj fn = make_evalfn(sym, CAR(CDR(CDR(form))), skip_docstring(CDR(CDR(CDR(form)))), vars, funs);
    SLOT(fn, EF_SIMPLE) = FIX(-2);
    SYM_FUNC(sym) = cons(Q_macro_marker, fn); WB(sym, SYM_FUNC(sym));
    mv_count = 1;
    RET(sym);
  }
  case SFID_DEFVAR: case SFID_DEFPARAMETER: case SFID_DEFCONSTANT: {
    PROTECT(form); PROTECT(vars); PROTECT(funs);
    Obj sym = CAR(CDR(form));
    Obj rest = CDR(CDR(form));
    PROTECT(sym); PROTECT(rest);
    if (sf == SFID_DEFCONSTANT ? !(UNFIX(SYM_FLAGS(sym)) & SF_CONSTANT) : !(UNFIX(SYM_FLAGS(sym)) & SF_SPECIAL))
      sym_lock_check(sym, sf == SFID_DEFCONSTANT ? "define as a constant" : "define as a special variable");
    long fl = UNFIX(SYM_FLAGS(sym)) | SF_SPECIAL | (sf == SFID_DEFCONSTANT ? SF_CONSTANT : 0);
    if (sf == SFID_DEFCONSTANT) {
      Obj v = eval(CAR(rest), vars, funs);
      SYM_VALUE(sym) = v; WB(sym, v);
    } else if (CONSP(rest) && (sf == SFID_DEFPARAMETER || SYM_VALUE(sym) == UNBOUND)) {
      Obj v = eval(CAR(rest), vars, funs);
      SYM_VALUE(sym) = v; WB(sym, v);
    }
    SYM_FLAGS(sym) = FIX(fl);
    mv_count = 1;
    RET(sym);
  }
  default:
    lisp_error("internal: unhandled special form %d", sf);
  }
}

/* ---- eval ------------------------------------------------------------- */
#ifdef NG_DEBUG
int obj_valid(Obj o);
static void dbg_check(const char *where, Obj o) {
  if (!obj_valid(o)) { fprintf(stderr, "NG_DEBUG: invalid object %lx at %s (nroots=%d vsp=%d)\n", (unsigned long)o, where, nroots, vsp); abort(); }
}
#else
#define dbg_check(w, o) ((void)0)
#endif
Obj eval(Obj form, Obj vars, Obj funs) {
  dbg_check("eval form", form); dbg_check("eval vars", vars); dbg_check("eval funs", funs);
  if (!CONSP(form)) {
    mv_count = 1;
    if (SYMBOLP(form)) {
      Obj c = lookup_var_cell(form, vars);
      if (c != NIL) return CDR(c);
      Obj v = SYM_VALUE(form);
      if (v == UNBOUND) lisp_error("The variable %s is unbound", STR_C(SYM_NAME(form)));
      return v;
    }
    return form;
  }
  int saved = nroots;
  PROTECT(form); PROTECT(vars); PROTECT(funs);
  STACK_CHECK(); if (++eval_depth > MAX_EVAL_DEPTH) { eval_depth = 0; lisp_error("Control stack exhausted (evaluator depth)"); }
  if (gc_pending && !gc_inhibit) gc();
  if (ng_attention && !gc_inhibit) proc_attention();      /* also a process-switch point */
  Obj head = CAR(form), f = NIL, r;
  if (SYMBOLP(head)) {
    int sf = SYM_SFID(head);
    if (sf) { r = eval_special(sf, form, vars, funs); eval_depth--; nroots = saved; return r; }
    Obj cell = lookup_fun_cell(head, funs);
    f = (cell != NIL) ? CDR(cell) : SYM_FUNC(head);
    if (f == NIL) lisp_error("The function %s is undefined", STR_C(SYM_NAME(head)));
    if (CONSP(f) && CAR(f) == Q_macro_marker) {
      Obj exp = call2(CDR(f), form, NIL);
      CAR(form) = Q_progn;                 /* displace: memoize expansion */
      CDR(form) = cons(exp, NIL); WB(form, CDR(form));
      r = eval(form, vars, funs);
      eval_depth--; nroots = saved; return r;
    }
  } else if (CONSP(head) && CAR(head) == Q_lambda) {
    f = make_evalfn(NIL, CAR(CDR(head)), CDR(CDR(head)), vars, funs);
  } else {
    lisp_error("Illegal function call: %s", STR_C(print_to_string(head, 1, 0)));
  }
  PUSH(f);
  Obj a = CDR(form);
  PROTECT(a);
  int n = 0;
  while (CONSP(a)) {
    Obj v = eval(CAR(a), vars, funs);
    PUSH(v); n++;
    a = CDR(a);
  }
  r = apply_stack(n);
  eval_depth--;
  nroots = saved;
  return r;
}

void init_eval(void) {
  vsp = 0; spsp = 0; vfp = 0; mv_count = 1; frame_top = NULL; eval_depth = 0;
  for (int i = 0; i < MAX_MV; i++) mv_vals[i] = NIL;
}
