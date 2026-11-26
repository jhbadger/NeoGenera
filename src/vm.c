/* vm.c -- the bytecode virtual machine.
 *
 * A stack machine over the shared value stack.  VM-to-VM calls and returns
 * do not recurse in C; only calls into primitives or stage-0 evaluator
 * closures do.  GC runs only at CALL/TAILCALL safe points, and all VM state
 * lives in rooted memory (vstack, vframes), so the interpreter loop reloads
 * its cached pointers (RELOAD) after anything that may collect.
 *
 * Instruction encoding: an int32 array (stored in a raw string object).
 * See docs/design/vm.md for the instruction set.
 */
#include "neogenera.h"

static void vm_enter(int n) {
  STACK_CHECK();
  Obj fn = vstack[vsp - n - 1];
  Obj code = SLOT(fn, 0);
  int nreq = (int)UNFIX(SLOT(code, C_NREQ)), nopt = (int)UNFIX(SLOT(code, C_NOPT));
  int rest = SLOT(code, C_REST) != NIL;
  int fixed = nreq + nopt;
  int base = vsp - n;
  if (n < nreq || (!rest && n > fixed))
    lisp_error("Wrong number of arguments to %s: got %d (takes %d to %s)",
               STR_C(print_to_string(SLOT(code, C_NAME), 1, 0)), n, nreq,
               rest ? "any number" : STR_C(print_to_string(FIX(fixed), 0, 0)));
  if (vfp >= ng_vfp_limit || vsp > ng_vsp_limit) ng_vm_overflow();
  if (n >= fixed) {
    if (rest) {
      Obj l = NIL;
      for (int i = vsp - 1; i >= base + fixed; i--) l = cons(vstack[i], l);
      vsp = base + fixed;
      PUSH(l);
    }
  } else {
    while (n < fixed) { PUSH(UNSUPPLIED); n++; }
    if (rest) PUSH(NIL);
  }
  vframes[vfp].closure = fn;
  vframes[vfp].pc = 0;
  vframes[vfp].base = base;
  vframes[vfp].spsp = spsp;
  vfp++;
}

static Obj vm_execute(int stop_vfp) {
  VFrame *fr;
  Obj closure, code, *consts;
  int32_t *ip;
  int base, pc;
  Obj r;

#define RELOAD() do { \
    fr = &vframes[vfp - 1]; closure = fr->closure; code = SLOT(closure, 0); \
    ip = (int32_t *)NSTR_DATA(SLOT(code, C_INSTRS)); consts = &SLOT(SLOT(code, C_CONSTS), 0); \
    base = fr->base; pc = fr->pc; } while (0)

  RELOAD();
  for (;;) {
    switch (ip[pc++]) {
    case OP_CONST: { int k = ip[pc++]; PUSH(consts[k]); break; }
    case OP_LREF: { int i = ip[pc++]; PUSH(vstack[base + i]); break; }
    case OP_LSET: { int i = ip[pc++]; vstack[base + i] = vstack[--vsp]; break; }
    case OP_FREF: { int i = ip[pc++]; PUSH(SLOT(closure, 1 + i)); break; }
    case OP_GREF: {
      Obj s = consts[ip[pc++]];
      Obj v = SYM_VALUE(s);
      if (v == UNBOUND) lisp_error("The variable %s is unbound", STR_C(SYM_NAME(s)));
      PUSH(v);
      break;
    }
    case OP_GSET: {
      Obj s = consts[ip[pc++]];
      if (UNFIX(SYM_FLAGS(s)) & SF_CONSTANT) lisp_error("Cannot assign to the constant %s", STR_C(SYM_NAME(s)));
      SYM_VALUE(s) = vstack[--vsp]; WB(s, SYM_VALUE(s));
      break;
    }
    case OP_GFUN: {
      Obj s = consts[ip[pc++]];
      Obj f = SYM_FUNC(s);
      if (f == NIL) lisp_error("The function %s is undefined", STR_C(SYM_NAME(s)));
      if (CONSP(f)) lisp_error("%s is a macro, not a function", STR_C(SYM_NAME(s)));
      PUSH(f);
      break;
    }
    case OP_POP: vsp--; break;
    case OP_DUP: { Obj v = vstack[vsp - 1]; PUSH(v); break; }
    case OP_JMP: {
      int t = ip[pc];
      if (t < pc && (prof_pending || ng_attention)) {
        fr->pc = pc;
        if (prof_pending) prof_record();
        if (ng_attention && !gc_inhibit) { proc_attention(); RELOAD(); }
      }
      pc = t;
      break;
    }
    case OP_JNIL: { int t = ip[pc++]; if (vstack[--vsp] == NIL) pc = t; break; }
    case OP_JNNIL: { int t = ip[pc++]; if (vstack[--vsp] != NIL) pc = t; break; }
    case OP_CALL: {
      int n = ip[pc++];
      fr->pc = pc;
      if (prof_pending) prof_record();
      if (gc_pending && !gc_inhibit) { gc(); RELOAD(); }
      if (ng_attention && !gc_inhibit) { proc_attention(); RELOAD(); }
      Obj fn = vstack[vsp - n - 1];
      if (IS_TYPE(fn, T_CLOSURE)) { vm_enter(n); RELOAD(); break; }
      r = apply_stack(n);
      RELOAD();
      PUSH(r);
      break;
    }
    case OP_TAILCALL: {
      int n = ip[pc++];
      fr->pc = pc;
      if (prof_pending) prof_record();
      if (gc_pending && !gc_inhibit) { gc(); RELOAD(); }
      if (ng_attention && !gc_inhibit) { proc_attention(); RELOAD(); }
      Obj fn = vstack[vsp - n - 1];
      if (IS_TYPE(fn, T_CLOSURE)) {
        int dst = base - 1;
        memmove(&vstack[dst], &vstack[vsp - n - 1], (size_t)(n + 1) * sizeof(Obj));
        vsp = dst + n + 1;
        vfp--;
        vm_enter(n);
        RELOAD();
        break;
      }
      r = apply_stack(n);
      RELOAD();
      goto do_return;                   /* preserves multiple values */
    }
    case OP_RET:
      r = vstack[--vsp];
      mv_count = 1;
    do_return:
      vsp = base - 1;
      vfp--;
      if (vfp == stop_vfp) return r;
      PUSH(r);
      RELOAD();
      break;
    case OP_RETV:                       /* return, keeping the multiple values left by the preceding call */
      r = vstack[--vsp];
      goto do_return;
    case OP_CLOSURE: {
      int k = ip[pc++], n = ip[pc++];
      Obj c = make_obj(T_CLOSURE, (size_t)(1 + n));
      SLOT(c, 0) = consts[k];
      for (int i = 0; i < n; i++) SLOT(c, 1 + i) = vstack[vsp - n + i];
      vsp -= n;
      PUSH(c);
      break;
    }
    case OP_BOX: vstack[vsp - 1] = cons(vstack[vsp - 1], NIL); break;
    case OP_UNBOX: vstack[vsp - 1] = CAR(vstack[vsp - 1]); break;
    case OP_BOXSET: { Obj v = vstack[--vsp]; Obj b = vstack[vsp - 1]; CAR(b) = v; WB(b, v); vstack[vsp - 1] = v; break; }
    case OP_BIND: { Obj s = consts[ip[pc++]]; dynbind(s, vstack[--vsp]); break; }
    case OP_UNBIND: { int n = ip[pc++]; unbind_to(spsp - n); break; }
    case OP_SLIDE: { int n = ip[pc++]; Obj top = vstack[vsp - 1]; vsp -= n; vstack[vsp - 1] = top; break; }
    case OP_CAR: { Obj v = vstack[vsp - 1]; vstack[vsp - 1] = CONSP(v) ? CAR(v) : car_of(v); break; }
    case OP_CDR: { Obj v = vstack[vsp - 1]; vstack[vsp - 1] = CONSP(v) ? CDR(v) : cdr_of(v); break; }
    case OP_CONS: { Obj d = vstack[--vsp]; vstack[vsp - 1] = cons(vstack[vsp - 1], d); break; }
    case OP_EQ: { Obj b = vstack[--vsp]; vstack[vsp - 1] = BOOL(vstack[vsp - 1] == b); break; }
    case OP_NOT: vstack[vsp - 1] = BOOL(vstack[vsp - 1] == NIL); break;
    case OP_ADD: {
      Obj b = vstack[--vsp], a = vstack[vsp - 1], s;
      if (FIXP(a) && FIXP(b) && !__builtin_add_overflow((SObj)a, (SObj)b, (SObj *)&s)) vstack[vsp - 1] = s;
      else vstack[vsp - 1] = num_add(a, b);
      break;
    }
    case OP_SUB: {
      Obj b = vstack[--vsp], a = vstack[vsp - 1], s;
      if (FIXP(a) && FIXP(b) && !__builtin_sub_overflow((SObj)a, (SObj)b, (SObj *)&s)) vstack[vsp - 1] = s;
      else vstack[vsp - 1] = num_sub(a, b);
      break;
    }
#define CMPOP(OPC, OP) \
    case OPC: { Obj b = vstack[--vsp], a = vstack[vsp - 1]; \
      if (FIXP(a) && FIXP(b)) { vstack[vsp - 1] = BOOL((SObj)a OP (SObj)b); } \
      else { vstack[vsp - 1] = BOOL(num_cmp(a, b) OP 0); } \
      break; }
    CMPOP(OP_LT, <)
    CMPOP(OP_GT, >)
    CMPOP(OP_LE, <=)
    CMPOP(OP_GE, >=)
    case OP_NUMEQ: { Obj b = vstack[--vsp], a = vstack[vsp - 1];
      vstack[vsp - 1] = (FIXP(a) && FIXP(b)) ? BOOL(a == b) : BOOL(num_equal(a, b));
      break; }
    default:
      lisp_error("VM: bad opcode %d", ip[pc - 1]);
    }
  }
}

/* ---- returning from and restarting frames (the debugger) ----
   Every nested VM run has an F_VMENTRY frame.  Frames within one run are flat (no C between them), so to return
   from frame K, or to run it again, the stack is unwound -- UNWIND-PROTECT cleanups run, special bindings are
   undone -- to the run that holds K, which then resumes: in K's caller with the given values, or at the start of
   K with its arguments as they are now. */
static SObj need_fix(Obj o) { if (!FIXP(o)) type_error("FIXNUM", o); return UNFIX(o); }
static Obj frame_resume_values;
static int frame_resume_kind, frame_resume_k;      /* 1: return, 2: restart */

static int frame_nparams(int k) {
  Obj code = SLOT(vframes[k].closure, 0);
  return (int)UNFIX(SLOT(code, C_NREQ)) + (int)UNFIX(SLOT(code, C_NOPT)) + (SLOT(code, C_REST) != NIL ? 1 : 0);
}

static const int op_nargs[OP_COUNT] = {
  [OP_CONST] = 1, [OP_LREF] = 1, [OP_LSET] = 1, [OP_FREF] = 1, [OP_GREF] = 1, [OP_GSET] = 1, [OP_GFUN] = 1,
  [OP_JMP] = 1, [OP_JNIL] = 1, [OP_JNNIL] = 1, [OP_CALL] = 1, [OP_TAILCALL] = 1, [OP_CLOSURE] = 2,
  [OP_BIND] = 1, [OP_UNBIND] = 1, [OP_SLIDE] = 1
};

/* parameters boxed in place (LREF i BOX LSET i) before PC hold boxes now: take their values out again */
static void unbox_params(int k) {
  Obj code = SLOT(vframes[k].closure, 0);
  Obj instrs = SLOT(code, C_INSTRS);
  const int32_t *ip = (const int32_t *)NSTR_DATA(instrs);
  int n = (int)(NSTR_LEN(instrs) / sizeof(int32_t)), np = frame_nparams(k), stop = vframes[k].pc;
  for (int pc = 0; pc < n && pc < stop; ) {
    int op = ip[pc];
    if (op < 0 || op >= OP_COUNT) return;
    if (op == OP_LREF && pc + 4 < n && ip[pc + 2] == OP_BOX && ip[pc + 3] == OP_LSET && ip[pc + 4] == ip[pc + 1]
        && ip[pc + 1] < np && pc + 4 < stop) {
      Obj *slot = &vstack[vframes[k].base + ip[pc + 1]];
      if (CONSP(*slot)) *slot = CAR(*slot);
      pc += 5;
      continue;
    }
    pc += 1 + op_nargs[op];
  }
}

static void frame_resume(int kind, int k, Obj values) __attribute__((noreturn));
static void frame_resume(int kind, int k, Obj values) {
  if (k < 0 || k >= vfp) lisp_error("There is no frame %d", k);
  Frame *e = NULL;
  for (Frame *f = frame_top; f; f = f->prev) if (f->kind == F_VMENTRY && f->vfp <= k) { e = f; break; }
  if (!e) lisp_error("Frame %d is not in a nested VM run, so it cannot be resumed", k);
  frame_resume_values = values; frame_resume_kind = kind; frame_resume_k = k;
  while (frame_top != e) {                         /* run the cleanups on the way */
    Frame *f = frame_top;
    frame_top = f->prev;
    if (f->kind == F_UNWIND) {
      unbind_to(f->spsp); vsp = f->vsp; vfp = f->vfp; nroots = f->nroots; eval_depth = f->eval_depth; gc_inhibit = f->gci;
      PUSH(f->cleanup);
      apply_stack(0);
    }
  }
  siglongjmp(e->sjb, 1);
}

PRIM(p_frame_return) {
  /* (%frame-return k values): make VM frame K return VALUES (a list) */
  UNUSED_ARGS;
  frame_resume(1, (int)need_fix(argv[0]), argv[1]);
}
PRIM(p_frame_restart) {
  /* (%frame-restart k): run VM frame K again from its start, with its arguments as they are now */
  UNUSED_ARGS;
  frame_resume(2, (int)need_fix(argv[0]), NIL);
}
PRIM(p_frame_resumable) {
  /* (%frame-resumable-p k): whether frame K can be returned from or restarted */
  UNUSED_ARGS;
  int k = (int)need_fix(argv[0]);
  if (k < 0 || k >= vfp) return NIL;
  for (Frame *f = frame_top; f; f = f->prev) if (f->kind == F_VMENTRY && f->vfp <= k) return TT;
  return NIL;
}

Obj vm_apply_closure(int n) {
  int stop = vfp;
  if (++eval_depth > 60000) { eval_depth = 0; lisp_error("Control stack exhausted (nested VM)"); }
  vm_enter(n);
  Frame f;
  frame_push(&f, F_VMENTRY, NIL);
  f.vfp = stop;
  if (sigsetjmp(f.sjb, 0) != 0) {                  /* a frame of this run is being returned from or restarted */
    reset_stack_limits();
    frame_top = &f; nroots = f.nroots; eval_depth = f.eval_depth; gc_inhibit = f.gci;
    int k = frame_resume_k;
    unbind_to(vframes[k].spsp);
    if (frame_resume_kind == 1) {
      Obj vals = frame_resume_values; frame_resume_values = NIL;
      vsp = vframes[k].base - 1; vfp = k;
      Obj r = values_list(vals);
      if (k == stop) { frame_pop(&f); eval_depth--; return r; }
      PUSH(r);
    } else {
      unbox_params(k);
      vfp = k + 1; vsp = vframes[k].base + frame_nparams(k); vframes[k].pc = 0;
    }
  }
  Obj r = vm_execute(stop);
  frame_pop(&f);
  eval_depth--;
  return r;
}

void init_vm(void) {
  add_root(&frame_resume_values); frame_resume_values = NIL;
  defprim("%FRAME-RETURN", p_frame_return, 2, 2);
  defprim("%FRAME-RESTART", p_frame_restart, 1, 1);
  defprim("%FRAME-RESUMABLE-P", p_frame_resumable, 1, 1);
}
