/* prims_core.c -- list, symbol, package and control primitives. */
#include "neogenera.h"
#include <time.h>
#include <errno.h>
#include <unistd.h>

PrimEntry prim_table[MAX_PRIMS];
int nprims;
extern int g_argc;
extern char **g_argv;

static Obj def_prim(const char *name, PrimFn fn, int min, int max, int mv) {
  if (nprims >= MAX_PRIMS) { fprintf(stderr, "too many primitives\n"); abort(); }
  int i = nprims++;
  prim_table[i] = (PrimEntry){ name, fn, min, max, mv, 0 };
  Obj sym = intern_cl(name);
  Obj p = make_obj(T_PRIM, PR_SLOTS);
  SLOT(p, PR_INDEX) = FIX(i); SLOT(p, PR_NAME) = sym;
  SLOT(p, PR_MIN) = FIX(min); SLOT(p, PR_MAX) = FIX(max);
  SYM_FUNC(sym) = p;
  return sym;
}
Obj defprim(const char *n, PrimFn f, int min, int max) { return def_prim(n, f, min, max, 0); }
Obj defprim_mv(const char *n, PrimFn f, int min, int max) { return def_prim(n, f, min, max, 1); }

static SObj need_fix(Obj o) { if (!FIXP(o)) type_error("FIXNUM", o); return UNFIX(o); }
static Obj need_sym(Obj o) { if (!SYMBOLP(o)) type_error("SYMBOL", o); return o; }
static Obj need_str(Obj o) { if (!STRINGP(o)) type_error("STRING", o); return o; }
static Obj str_designator(Obj o) {
  if (STRINGP(o)) return o;
  if (SYMBOLP(o)) return SYM_NAME(o);
  if (CHARP(o)) { char c = (char)CHAR_CODE(o); return make_string(&c, 1); }
  type_error("STRING-DESIGNATOR", o);
}

/* ---- conses and lists ------------------------------------------------- */
PRIM(p_cons) { UNUSED_ARGS; return cons(argv[0], argv[1]); }
PRIM(p_car) { UNUSED_ARGS; return car_of(argv[0]); }
PRIM(p_cdr) { UNUSED_ARGS; return cdr_of(argv[0]); }
PRIM(p_rplaca) { UNUSED_ARGS; if (!CONSP(argv[0])) type_error("CONS", argv[0]); CAR(argv[0]) = argv[1]; WB(argv[0], argv[1]); return argv[0]; }
PRIM(p_rplacd) { UNUSED_ARGS; if (!CONSP(argv[0])) type_error("CONS", argv[0]); CDR(argv[0]) = argv[1]; WB(argv[0], argv[1]); return argv[0]; }
PRIM(p_caar) { UNUSED_ARGS; return car_of(car_of(argv[0])); }
PRIM(p_cadr) { UNUSED_ARGS; return car_of(cdr_of(argv[0])); }
PRIM(p_cdar) { UNUSED_ARGS; return cdr_of(car_of(argv[0])); }
PRIM(p_cddr) { UNUSED_ARGS; return cdr_of(cdr_of(argv[0])); }
PRIM(p_caddr) { UNUSED_ARGS; return car_of(cdr_of(cdr_of(argv[0]))); }
PRIM(p_cdddr) { UNUSED_ARGS; return cdr_of(cdr_of(cdr_of(argv[0]))); }
PRIM(p_cadddr) { UNUSED_ARGS; return car_of(cdr_of(cdr_of(cdr_of(argv[0])))); }
PRIM(p_cddddr) { UNUSED_ARGS; return cdr_of(cdr_of(cdr_of(cdr_of(argv[0])))); }

PRIM(p_list) {
  Obj l = NIL;
  for (int i = argc - 1; i >= 0; i--) l = cons(argv[i], l);
  return l;
}
PRIM(p_list_star) {
  Obj l = argv[argc - 1];
  for (int i = argc - 2; i >= 0; i--) l = cons(argv[i], l);
  return l;
}
PRIM(p_append) {
  if (argc == 0) return NIL;
  Obj result = argv[argc - 1];
  for (int i = argc - 2; i >= 0; i--) {
    Obj l = argv[i];
    if (l == NIL) continue;
    if (!CONSP(l)) type_error("LIST", l);
    Obj head = NIL, tail = NIL;
    for (; CONSP(l); l = CDR(l)) {
      Obj c = cons(CAR(l), NIL);
      if (head == NIL) head = c; else CDR(tail) = c;
      tail = c;
    }
    if (l != NIL) type_error("LIST", argv[i]);
    CDR(tail) = result;
    result = head;
  }
  return result;
}
PRIM(p_nconc) {
  Obj result = NIL, tail = NIL;
  for (int i = 0; i < argc; i++) {
    Obj l = argv[i];
    if (l == NIL) continue;
    if (i == argc - 1 || !CONSP(l)) { if (result == NIL) result = l; else { CDR(tail) = l; WB(tail, l); } break; }
    if (result == NIL) result = l; else { CDR(tail) = l; WB(tail, l); }
    tail = l; while (CONSP(CDR(tail))) tail = CDR(tail);
  }
  return result;
}
PRIM(p_length) {
  UNUSED_ARGS;
  Obj x = argv[0];
  if (x == NIL) return FIX(0);
  if (CONSP(x)) { long n = list_length(x); if (n < 0) type_error("PROPER-LIST", x); return FIX(n); }
  if (STRINGP(x)) return FIX(STR_LEN(x));
  if (VECTORP(x)) return FIX(OBJ_NWORDS(x));
  if (IS_TYPE(x, T_ADVEC)) return SLOT(x, AV_FILL);
  type_error("SEQUENCE", x);
}
PRIM(p_reverse) {
  UNUSED_ARGS;
  Obj x = argv[0];
  if (LISTP(x)) { Obj r = NIL; for (; CONSP(x); x = CDR(x)) r = cons(CAR(x), r); return r; }
  if (STRINGP(x)) { size_t n = STR_LEN(x); Obj s = make_string(NULL, n); for (size_t i = 0; i < n; i++) STR_DATA(s)[i] = STR_DATA(x)[n - 1 - i]; return s; }
  if (VECTORP(x)) { size_t n = OBJ_NWORDS(x); Obj v = make_vector(n, NIL); for (size_t i = 0; i < n; i++) SLOT(v, i) = SLOT(x, n - 1 - i); return v; }
  type_error("SEQUENCE", x);
}
PRIM(p_nreverse) {
  UNUSED_ARGS;
  if (LISTP(argv[0])) return nreverse_list(argv[0]);
  return p_reverse(argc, argv);
}
PRIM(p_nth) {
  UNUSED_ARGS;
  SObj n = need_fix(argv[0]); Obj l = argv[1];
  while (n-- > 0 && CONSP(l)) l = CDR(l);
  return car_of(l);
}
PRIM(p_nthcdr) {
  UNUSED_ARGS;
  SObj n = need_fix(argv[0]); Obj l = argv[1];
  while (n-- > 0 && CONSP(l)) l = CDR(l);
  return l;
}
PRIM(p_last) {
  SObj n = argc > 1 ? need_fix(argv[1]) : 1;
  Obj l = argv[0];
  long len = list_length(l);
  if (len < 0) type_error("PROPER-LIST", l);
  for (long i = 0; i < len - n; i++) l = CDR(l);
  return l;
}
PRIM(p_copy_list) {
  UNUSED_ARGS;
  Obj l = argv[0], head = NIL, tail = NIL;
  for (; CONSP(l); l = CDR(l)) { Obj c = cons(CAR(l), NIL); if (head == NIL) head = c; else CDR(tail) = c; tail = c; }
  if (tail != NIL) CDR(tail) = l;
  return head;
}
PRIM(p_copy_tree) {
  UNUSED_ARGS;
  /* iterative on cdr, recursive on car */
  Obj l = argv[0];
  if (!CONSP(l)) return l;
  Obj head = NIL, tail = NIL;
  for (; CONSP(l); l = CDR(l)) {
    Obj a = CAR(l);
    if (CONSP(a)) { Obj sub = a; a = p_copy_tree(1, &sub); }
    Obj c = cons(a, NIL);
    if (head == NIL) head = c; else CDR(tail) = c;
    tail = c;
  }
  if (tail != NIL) CDR(tail) = l;
  return head;
}
PRIM(p_memq) { UNUSED_ARGS; for (Obj l = argv[1]; CONSP(l); l = CDR(l)) if (CAR(l) == argv[0]) return l; return NIL; }
PRIM(p_memql) { UNUSED_ARGS; for (Obj l = argv[1]; CONSP(l); l = CDR(l)) if (eql(CAR(l), argv[0])) return l; return NIL; }
PRIM(p_member_equal) { UNUSED_ARGS; for (Obj l = argv[1]; CONSP(l); l = CDR(l)) if (equal(CAR(l), argv[0])) return l; return NIL; }
PRIM(p_assq) { UNUSED_ARGS; for (Obj l = argv[1]; CONSP(l); l = CDR(l)) if (CONSP(CAR(l)) && CAR(CAR(l)) == argv[0]) return CAR(l); return NIL; }
PRIM(p_assql) { UNUSED_ARGS; for (Obj l = argv[1]; CONSP(l); l = CDR(l)) if (CONSP(CAR(l)) && eql(CAR(CAR(l)), argv[0])) return CAR(l); return NIL; }
PRIM(p_assoc_equal) { UNUSED_ARGS; for (Obj l = argv[1]; CONSP(l); l = CDR(l)) if (CONSP(CAR(l)) && equal(CAR(CAR(l)), argv[0])) return CAR(l); return NIL; }

/* ---- predicates and equality ------------------------------------------ */
PRIM(p_null) { UNUSED_ARGS; return BOOL(argv[0] == NIL); }
PRIM(p_atom) { UNUSED_ARGS; return BOOL(!CONSP(argv[0])); }
PRIM(p_consp) { UNUSED_ARGS; return BOOL(CONSP(argv[0])); }
PRIM(p_listp) { UNUSED_ARGS; return BOOL(LISTP(argv[0])); }
PRIM(p_symbolp) { UNUSED_ARGS; return BOOL(SYMBOLP(argv[0])); }
PRIM(p_stringp) { UNUSED_ARGS; return BOOL(STRINGP(argv[0])); }
PRIM(p_characterp) { UNUSED_ARGS; return BOOL(CHARP(argv[0])); }
PRIM(p_vectorp) { UNUSED_ARGS; Obj x = argv[0]; return BOOL(VECTORP(x) || STRINGP(x) || IS_TYPE(x, T_ADVEC)); }
PRIM(p_simple_vector_p) { UNUSED_ARGS; return BOOL(VECTORP(argv[0])); }
PRIM(p_functionp) { UNUSED_ARGS; Obj x = argv[0]; return BOOL(IS_TYPE(x, T_EVALFN) || IS_TYPE(x, T_CLOSURE) || IS_TYPE(x, T_PRIM)); }
PRIM(p_packagep) { UNUSED_ARGS; return BOOL(IS_TYPE(argv[0], T_PACKAGE)); }
PRIM(p_streamp) { UNUSED_ARGS; return BOOL(IS_TYPE(argv[0], T_STREAM)); }
PRIM(p_structp) { UNUSED_ARGS; return BOOL(IS_TYPE(argv[0], T_STRUCT)); }
PRIM(p_compiled_function_p) { UNUSED_ARGS; return BOOL(IS_TYPE(argv[0], T_CLOSURE)); }
PRIM(p_eq) { UNUSED_ARGS; return BOOL(argv[0] == argv[1]); }
PRIM(p_eql) { UNUSED_ARGS; return BOOL(eql(argv[0], argv[1])); }
PRIM(p_equal) { UNUSED_ARGS; return BOOL(equal(argv[0], argv[1])); }
PRIM(p_equalp) { UNUSED_ARGS; return BOOL(equalp(argv[0], argv[1])); }
PRIM(p_sxhash) { UNUSED_ARGS; return FIX((SObj)(sxhash(argv[0], 4) & 0xfffffffffffULL)); }
PRIM(p_unsupplied) { UNUSED_ARGS; return UNSUPPLIED; }
PRIM(p_getf_key) {
  /* (%getf-key plist key) -> value or the UNSUPPLIED marker */
  UNUSED_ARGS;
  for (Obj p = argv[0]; CONSP(p) && CONSP(CDR(p)); p = CDR(CDR(p))) if (CAR(p) == argv[1]) return CAR(CDR(p));
  return UNSUPPLIED;
}
PRIM(p_unsupplied_p) { UNUSED_ARGS; return BOOL(argv[0] == UNSUPPLIED); }

/* ---- symbols ----------------------------------------------------------- */
PRIM(p_symbol_name) { UNUSED_ARGS; return SYM_NAME(need_sym(argv[0])); }
PRIM(p_symbol_value) {
  UNUSED_ARGS; Obj s = need_sym(argv[0]); Obj v = SYM_VALUE(s);
  if (v == UNBOUND) lisp_error("The variable %s is unbound", STR_DATA(SYM_NAME(s)));
  return v;
}
PRIM(p_set) {
  UNUSED_ARGS; Obj s = need_sym(argv[0]);
  if (UNFIX(SYM_FLAGS(s)) & SF_CONSTANT) lisp_error("Cannot assign to the constant %s", STR_DATA(SYM_NAME(s)));
  SYM_VALUE(s) = argv[1]; WB(s, argv[1]);
  return argv[1];
}
PRIM(p_symbol_function) {
  UNUSED_ARGS; Obj s = need_sym(argv[0]); Obj f = SYM_FUNC(s);
  if (f == NIL) lisp_error("The function %s is undefined", STR_DATA(SYM_NAME(s)));
  return CONSP(f) ? CDR(f) : f;
}
PRIM(p_set_symbol_function) { UNUSED_ARGS; SYM_FUNC(need_sym(argv[0])) = argv[1]; WB(argv[0], argv[1]); return argv[1]; }
PRIM(p_macro_function) {
  UNUSED_ARGS;
  if (!SYMBOLP(argv[0])) return NIL;
  Obj f = SYM_FUNC(argv[0]);
  return (CONSP(f) && CAR(f) == Q_macro_marker) ? CDR(f) : NIL;
}
PRIM(p_set_macro_function) { UNUSED_ARGS; SYM_FUNC(need_sym(argv[0])) = cons(Q_macro_marker, argv[1]); WB(argv[0], SYM_FUNC(argv[0])); return argv[1]; }
PRIM(p_special_operator_p) {
  UNUSED_ARGS;
  if (!SYMBOLP(argv[0])) return NIL;
  switch (SYM_SFID(argv[0])) {
  case SFID_QUOTE: case SFID_FUNCTION: case SFID_IF: case SFID_PROGN: case SFID_LET: case SFID_LETSTAR:
  case SFID_SETQ: case SFID_FLET: case SFID_LABELS: case SFID_MACROLET: case SFID_BLOCK:
  case SFID_RETURN_FROM: case SFID_TAGBODY: case SFID_GO: case SFID_CATCH: case SFID_UNWIND_PROTECT:
  case SFID_MV_CALL: case SFID_MV_PROG1: case SFID_PROGV: case SFID_EVAL_WHEN: case SFID_THE:
  case SFID_LOCALLY: case SFID_DECLARE: case SFID_LTV:
    return TT;
  default: return NIL;
  }
}
PRIM(p_symbol_plist) { UNUSED_ARGS; return SYM_PLIST(need_sym(argv[0])); }
PRIM(p_set_symbol_plist) { UNUSED_ARGS; SYM_PLIST(need_sym(argv[0])) = argv[1]; WB(argv[0], argv[1]); return argv[1]; }
PRIM(p_symbol_package) { UNUSED_ARGS; return SYM_PKG(need_sym(argv[0])); }
PRIM(p_get) { return plist_get(SYM_PLIST(need_sym(argv[0])), argv[1], argc > 2 ? argv[2] : NIL); }
PRIM(p_put) { UNUSED_ARGS; Obj s = need_sym(argv[0]); SYM_PLIST(s) = plist_put(SYM_PLIST(s), argv[1], argv[2]); WB(s, SYM_PLIST(s)); return argv[2]; }
PRIM(p_boundp) { UNUSED_ARGS; return BOOL(SYM_VALUE(need_sym(argv[0])) != UNBOUND); }
PRIM(p_fboundp) { UNUSED_ARGS; return BOOL(SYM_FUNC(need_sym(argv[0])) != NIL); }
PRIM(p_makunbound) { UNUSED_ARGS; SYM_VALUE(need_sym(argv[0])) = UNBOUND; return argv[0]; }
PRIM(p_fmakunbound) { UNUSED_ARGS; SYM_FUNC(need_sym(argv[0])) = NIL; return argv[0]; }
PRIM(p_make_symbol) { UNUSED_ARGS; need_str(argv[0]); return make_symbol_obj(make_string(STR_DATA(argv[0]), STR_LEN(argv[0]))); }
PRIM(p_keywordp) { UNUSED_ARGS; return BOOL(SYMBOLP(argv[0]) && SYM_PKG(argv[0]) == PKG_KEYWORD); }
PRIM(p_proclaim_special) { UNUSED_ARGS; Obj s = need_sym(argv[0]); SYM_FLAGS(s) = FIX(UNFIX(SYM_FLAGS(s)) | SF_SPECIAL); return s; }
PRIM(p_special_p) { UNUSED_ARGS; return BOOL(SYMBOLP(argv[0]) && (UNFIX(SYM_FLAGS(argv[0])) & SF_SPECIAL)); }
PRIM(p_constant_p) { UNUSED_ARGS; return BOOL(SYMBOLP(argv[0]) && (UNFIX(SYM_FLAGS(argv[0])) & SF_CONSTANT)); }
PRIM(p_proclaim_constant) { UNUSED_ARGS; Obj s = need_sym(argv[0]); SYM_FLAGS(s) = FIX(UNFIX(SYM_FLAGS(s)) | SF_SPECIAL | SF_CONSTANT); return s; }

static Obj status_kw(int st) {
  switch (st) { case 1: return intern_kw("INTERNAL"); case 2: return intern_kw("EXTERNAL"); case 3: return intern_kw("INHERITED"); default: return NIL; }
}
static Obj pkg_arg(int argc, Obj *argv, int i) {
  if (argc > i && argv[i] != NIL) {
    Obj p = find_package(argv[i]);
    if (p == NIL) lisp_error("No package named %s", STR_DATA(print_to_string(argv[i], 1, 0)));
    return p;
  }
  return current_package();
}
PRIM(p_intern) {
  Obj pkg = pkg_arg(argc, argv, 1);
  int st; Obj s = intern_obj(need_str(argv[0]), pkg, &st);
  values_list(NIL);
  mv_vals[0] = s; mv_vals[1] = status_kw(st); mv_count = 2;
  return s;
}
PRIM(p_find_symbol) {
  Obj pkg = pkg_arg(argc, argv, 1);
  int st; Obj s = find_symbol_in(need_str(argv[0]), pkg, &st);
  mv_vals[0] = st ? s : NIL; mv_vals[1] = status_kw(st); mv_count = 2;
  return mv_vals[0];
}
PRIM(p_find_package) { UNUSED_ARGS; return find_package(argv[0]); }
PRIM(p_make_package) {
  /* (%make-package name nicknames uses) */
  UNUSED_ARGS;
  Obj name = str_designator(argv[0]);
  if (find_package(name) != NIL) lisp_error("Package %s already exists", STR_DATA(name));
  Obj uses = NIL;
  for (Obj l = argv[2]; CONSP(l); l = CDR(l)) { Obj p = find_package(CAR(l)); if (p == NIL) lisp_error("No such package to use"); uses = cons(p, uses); }
  Obj nicks = NIL;
  for (Obj l = argv[1]; CONSP(l); l = CDR(l)) nicks = cons(str_designator(CAR(l)), nicks);
  return make_package(name, nicks, nreverse_list(uses));
}
PRIM(p_package_name) { UNUSED_ARGS; Obj p = find_package(argv[0]); if (p == NIL) type_error("PACKAGE", argv[0]); return SLOT(p, PK_NAME); }
PRIM(p_list_all_packages) { UNUSED_ARGS; return all_packages; }
PRIM(p_use_package) {
  Obj pkg = pkg_arg(argc, argv, 1);
  Obj l = argv[0];
  if (!CONSP(l)) l = list1(l);
  for (; CONSP(l); l = CDR(l)) { Obj u = find_package(CAR(l)); if (u == NIL) lisp_error("No such package"); use_package(pkg, u); }
  return TT;
}
PRIM(p_export) {
  Obj pkg = pkg_arg(argc, argv, 1);
  Obj l = argv[0];
  if (!CONSP(l)) l = list1(l);
  for (; CONSP(l); l = CDR(l)) export_symbol(need_sym(CAR(l)), pkg);
  return TT;
}
PRIM(p_import) {
  Obj pkg = pkg_arg(argc, argv, 1);
  Obj l = argv[0];
  if (!CONSP(l)) l = list1(l);
  for (; CONSP(l); l = CDR(l)) {
    Obj s = need_sym(CAR(l)); int st; Obj name = SYM_NAME(s);
    Obj ex = find_symbol_in(name, pkg, &st);
    if (st == 1 || st == 2) { if (ex != s) lisp_error("Import conflict for %s", STR_DATA(name)); continue; }
    /* add into the package's present table */
    Obj tab = SLOT(pkg, PK_INTERNAL);
    uint64_t h = 1469598103934665603ULL;
    for (size_t i = 0; i < STR_LEN(name); i++) { h ^= (unsigned char)STR_DATA(name)[i]; h *= 1099511628211ULL; }
    size_t idx = h % OBJ_NWORDS(tab);
    SLOT(tab, idx) = cons(s, SLOT(tab, idx)); WB(tab, SLOT(tab, idx));
    if (SYM_PKG(s) == NIL) { SYM_PKG(s) = pkg; WB(s, pkg); }
  }
  return TT;
}
PRIM(p_package_symbols) {
  /* (%package-symbols pkg external-only-p) */
  UNUSED_ARGS;
  Obj pkg = find_package(argv[0]);
  if (pkg == NIL) type_error("PACKAGE", argv[0]);
  Obj tab = SLOT(pkg, argv[1] != NIL ? PK_EXTERNAL : PK_INTERNAL), r = NIL;
  for (size_t i = 0; i < OBJ_NWORDS(tab); i++) for (Obj l = SLOT(tab, i); CONSP(l); l = CDR(l)) r = cons(CAR(l), r);
  return r;
}
PRIM(p_package_use_list) { UNUSED_ARGS; Obj p = find_package(argv[0]); if (p == NIL) type_error("PACKAGE", argv[0]); return SLOT(p, PK_USES); }
PRIM(p_set_export_all) { UNUSED_ARGS; export_all_cl = argv[0] != NIL; return argv[0]; }

/* ---- control ----------------------------------------------------------- */
PRIM(p_funcall) {
  int saved = vsp;
  Obj r = apply_stack(argc - 1);
  vsp = saved;
  return r;
}
PRIM(p_apply) {
  int saved = vsp, n = 0;
  PUSH(argv[0]);
  for (int i = 1; i < argc - 1; i++) { PUSH(argv[i]); n++; }
  Obj l = argv[argc - 1];
  if (argc > 1) {
    for (; CONSP(l); l = CDR(l)) { PUSH(CAR(l)); n++; }
    if (l != NIL) type_error("LIST", argv[argc - 1]);
  }
  Obj r = apply_stack(n);
  vsp = saved;
  return r;
}
PRIM(p_values) {
  if (argc > MAX_MV) lisp_error("too many values");
  for (int i = 0; i < argc; i++) mv_vals[i] = argv[i];
  mv_count = argc;
  return argc ? argv[0] : NIL;
}
PRIM(p_values_list) { UNUSED_ARGS; return values_list(argv[0]); }
PRIM(p_mv_call) {
  int saved = vsp, n = 0;
  PUSH(argv[0]);
  for (int i = 1; i < argc; i++) {
    Obj v = call0(argv[i]);
    if (mv_count == 1) { PUSH(v); n++; }
    else { int c = mv_count; for (int j = 0; j < c; j++) { PUSH(mv_vals[j]); n++; } }
  }
  Obj r = apply_stack(n);
  vsp = saved;
  return r;
}
PRIM(p_catch) {
  UNUSED_ARGS;
  Frame f;
  frame_push(&f, F_CATCH, argv[0]);
  Obj r;
  if (setjmp(f.jb) == 0) { r = call0(argv[1]); frame_pop(&f); }
  else { frame_top = f.prev; r = frame_result(&f); }
  return r;
}
PRIM(p_throw) {
  UNUSED_ARGS;
  Frame *t = find_frame(F_CATCH, argv[0]);
  if (!t) lisp_error("Attempt to throw to a tag that does not exist: %s", STR_DATA(print_to_string(argv[0], 1, 0)));
  t->mvlist = UNBOUND;
  throw_to(t, argv[1]);
}
PRIM(p_block) {
  UNUSED_ARGS;
  Obj token = cons(NIL, NIL);
  Frame f;
  frame_push(&f, F_BLOCK, token);
  Obj r;
  if (setjmp(f.jb) == 0) { r = call1(argv[0], token); frame_pop(&f); }
  else { frame_top = f.prev; r = frame_result(&f); }
  return r;
}
PRIM(p_return_from) {
  UNUSED_ARGS;
  Frame *t = find_frame(F_BLOCK, argv[0]);
  if (!t) lisp_error("RETURN-FROM: the block is no longer active");
  t->mvlist = UNBOUND;
  throw_to(t, argv[1]);
}
PRIM(p_return_from_mv) {
  UNUSED_ARGS;
  Frame *t = find_frame(F_BLOCK, argv[0]);
  if (!t) lisp_error("RETURN-FROM: the block is no longer active");
  t->mvlist = argv[1];
  throw_to(t, CONSP(argv[1]) ? CAR(argv[1]) : NIL);
}
PRIM(p_unwind_protect) {
  UNUSED_ARGS;
  Obj r, mvl = UNBOUND;
  PROTECT(r); PROTECT(mvl);
  Frame f;
  frame_push(&f, F_UNWIND, NIL);
  f.cleanup = argv[1];
  r = call0(argv[0]);
  if (mv_count != 1) { mvl = NIL; for (int i = mv_count - 1; i >= 0; i--) mvl = cons(mv_vals[i], mvl); }
  frame_pop(&f);
  call0(argv[1]);
  if (mvl != UNBOUND) r = values_list(mvl); else mv_count = 1;
  UNPROTECT(2);
  return r;
}
PRIM(p_progv) {
  UNUSED_ARGS;
  int ssp = spsp;
  Obj syms = argv[0], vals = argv[1];
  for (; CONSP(syms); syms = CDR(syms)) { dynbind(CAR(syms), CONSP(vals) ? CAR(vals) : UNBOUND); if (CONSP(vals)) vals = CDR(vals); }
  Obj r = call0(argv[2]);
  unbind_to(ssp);
  return r;
}
PRIM(p_error_to_top) {
  UNUSED_ARGS;
  Frame *top = find_frame(F_TOP, NIL);
  if (!top) { fprintf(stderr, "Error: %s\n", STR_DATA(argv[0])); exit(1); }
  throw_to(top, argv[0]);
}
PRIM(p_eval) { UNUSED_ARGS; return eval(argv[0], NIL, NIL); }
PRIM(p_identity) { UNUSED_ARGS; mv_count = 1; return argv[0]; }
PRIM(p_load_time_dummy) { UNUSED_ARGS; return NIL; }

/* ---- code objects and structures --------------------------------------- */
PRIM(p_make_code) {
  /* (%make-code name nreq nopt restp instr-list consts-vector arglist) */
  UNUSED_ARGS;
  long n = list_length(argv[4]);
  if (n < 0) type_error("LIST", argv[4]);
  Obj bytes = make_string(NULL, (size_t)n * 4);
  int32_t *ip = (int32_t *)STR_DATA(bytes);
  long i = 0;
  for (Obj l = argv[4]; CONSP(l); l = CDR(l)) ip[i++] = (int32_t)need_fix(CAR(l));
  Obj c = make_obj(T_CODE, C_SLOTS);
  SLOT(c, C_NAME) = argv[0]; SLOT(c, C_NREQ) = argv[1]; SLOT(c, C_NOPT) = argv[2];
  SLOT(c, C_REST) = argv[3]; SLOT(c, C_INSTRS) = bytes; SLOT(c, C_CONSTS) = argv[5];
  SLOT(c, C_ARGLIST) = argv[6];
  return c;
}
PRIM(p_code_parts) {
  UNUSED_ARGS;
  Obj c = argv[0];
  if (IS_TYPE(c, T_CLOSURE)) c = SLOT(c, 0);
  if (!IS_TYPE(c, T_CODE)) type_error("CODE", c);
  Obj bytes = SLOT(c, C_INSTRS);
  size_t n = STR_LEN(bytes) / 4;
  Obj il = NIL;
  int32_t *ip = (int32_t *)STR_DATA(bytes);
  for (size_t i = n; i > 0; i--) il = cons(FIX(ip[i - 1]), il);
  return cons(SLOT(c, C_NAME), cons(SLOT(c, C_NREQ), cons(SLOT(c, C_NOPT), cons(SLOT(c, C_REST),
         cons(il, cons(SLOT(c, C_CONSTS), cons(SLOT(c, C_ARGLIST), NIL)))))));
}
PRIM(p_codep) { UNUSED_ARGS; return BOOL(IS_TYPE(argv[0], T_CODE)); }
PRIM(p_code_closure) {
  /* (%code-closure code free-list) */
  UNUSED_ARGS;
  if (!IS_TYPE(argv[0], T_CODE)) type_error("CODE", argv[0]);
  long n = list_length(argv[1]);
  Obj c = make_obj(T_CLOSURE, (size_t)(1 + n));
  SLOT(c, 0) = argv[0];
  long i = 1;
  for (Obj l = argv[1]; CONSP(l); l = CDR(l)) SLOT(c, i++) = CAR(l);
  return c;
}
PRIM(p_function_name) {
  UNUSED_ARGS;
  Obj f = argv[0];
  if (IS_TYPE(f, T_EVALFN)) return SLOT(f, EF_NAME);
  if (IS_TYPE(f, T_CLOSURE)) return SLOT(SLOT(f, 0), C_NAME);
  if (IS_TYPE(f, T_PRIM)) return SLOT(f, PR_NAME);
  return NIL;
}
PRIM(p_set_function_name) {
  UNUSED_ARGS;
  Obj f = argv[0];
  if (IS_TYPE(f, T_EVALFN)) { SLOT(f, EF_NAME) = argv[1]; WB(f, argv[1]); }
  else if (IS_TYPE(f, T_CLOSURE)) { SLOT(SLOT(f, 0), C_NAME) = argv[1]; WB(SLOT(f, 0), argv[1]); }
  return f;
}
PRIM(p_function_lambda_expression) {
  UNUSED_ARGS;
  Obj f = argv[0];
  if (IS_TYPE(f, T_EVALFN)) return cons(SLOT(f, EF_LAMBDA), SLOT(f, EF_BODY));
  return NIL;
}
PRIM(p_make_struct) {
  Obj s = make_obj(T_STRUCT, (size_t)argc);
  for (int i = 0; i < argc; i++) SLOT(s, i) = argv[i];
  return s;
}
PRIM(p_struct_ref) {
  UNUSED_ARGS;
  if (!IS_TYPE(argv[0], T_STRUCT)) type_error("STRUCTURE", argv[0]);
  SObj i = need_fix(argv[1]);
  if (i < 0 || (size_t)i >= OBJ_NWORDS(argv[0])) lisp_error("structure index out of range");
  return SLOT(argv[0], i);
}
PRIM(p_struct_set) {
  UNUSED_ARGS;
  if (!IS_TYPE(argv[0], T_STRUCT)) type_error("STRUCTURE", argv[0]);
  SObj i = need_fix(argv[1]);
  if (i < 0 || (size_t)i >= OBJ_NWORDS(argv[0])) lisp_error("structure index out of range");
  SLOT(argv[0], i) = argv[2]; WB(argv[0], argv[2]);
  return argv[2];
}
PRIM(p_struct_type) { UNUSED_ARGS; if (!IS_TYPE(argv[0], T_STRUCT)) return NIL; return SLOT(argv[0], 0); }

/* A structure instance's slot 0 is its descriptor (NAME . SLOT-NAMES).  The
 * :include parent of a struct type is kept on the type name's plist. */
static int struct_typep(Obj o, Obj name) {
  if (!IS_TYPE(o, T_STRUCT)) return 0;
  Obj d = SLOT(o, 0);
  Obj n = CONSP(d) ? CAR(d) : d;
  while (SYMBOLP(n) && n != NIL) {
    if (n == name) return 1;
    n = plist_get(SYM_PLIST(n), Q_struct_parent, NIL);
  }
  return 0;
}
PRIM(p_struct_typep) { UNUSED_ARGS; return BOOL(struct_typep(argv[0], argv[1])); }
PRIM(p_struct_ref_typed) {
  /* (%struct-ref-typed obj index type-name) */
  UNUSED_ARGS;
  if (!struct_typep(argv[0], argv[2])) type_error(STR_DATA(print_to_string(argv[2], 1, 0)), argv[0]);
  return SLOT(argv[0], UNFIX(argv[1]));
}
PRIM(p_copy_struct) {
  UNUSED_ARGS;
  Obj x = argv[0];
  if (!IS_TYPE(x, T_STRUCT)) type_error("STRUCTURE", x);
  size_t n = OBJ_NWORDS(x);
  Obj s = make_obj(T_STRUCT, n);
  for (size_t i = 0; i < n; i++) SLOT(s, i) = SLOT(x, i);
  return s;
}
PRIM(p_struct_set_typed) {
  /* (%struct-set-typed obj index type-name value) */
  UNUSED_ARGS;
  if (!struct_typep(argv[0], argv[2])) type_error(STR_DATA(print_to_string(argv[2], 1, 0)), argv[0]);
  SLOT(argv[0], UNFIX(argv[1])) = argv[3]; WB(argv[0], argv[3]);
  return argv[3];
}

PRIM(p_backtrace) {
  /* (%backtrace max) -> list of (function name args) for active VM frames, innermost first */
  int max = (argc > 0 && FIXP(argv[0])) ? (int)UNFIX(argv[0]) : 100;
  Obj head = NIL, tail = NIL;
  for (int i = vfp - 1; i >= 0 && max-- > 0; i--) {
    Obj fn = vframes[i].closure, code = SLOT(fn, 0);
    int n = (int)UNFIX(SLOT(code, C_NREQ)) + (int)UNFIX(SLOT(code, C_NOPT)) + (SLOT(code, C_REST) != NIL ? 1 : 0);
    int base = vframes[i].base;
    Obj args = NIL;
    for (int k = n - 1; k >= 0; k--) args = cons(vstack[base + k], args);
    Obj cell = cons(list3(fn, SLOT(code, C_NAME), args), NIL);
    if (head == NIL) head = cell; else CDR(tail) = cell;
    tail = cell;
  }
  return head;
}
PRIM(p_make_instance) {
  /* (%make-instance class nslots): struct whose slot 0 is CLASS and whose other slots are unbound */
  UNUSED_ARGS;
  SObj n = need_fix(argv[1]);
  if (n < 0 || n > 100000) lisp_error("%%MAKE-INSTANCE: bad slot count");
  Obj s = make_obj(T_STRUCT, (size_t)n + 1);
  SLOT(s, 0) = argv[0];
  for (SObj i = 1; i <= n; i++) SLOT(s, i) = UNSUPPLIED;
  return s;
}
PRIM(p_last_read_start) { UNUSED_ARGS; return FIX(last_read_start); }
PRIM(p_stream_position) { UNUSED_ARGS; if (!IS_TYPE(argv[0], T_STREAM)) type_error("STREAM", argv[0]); return FIX(stream_position(argv[0])); }
PRIM(p_utf8_encode) {
  UNUSED_ARGS;
  Obj tmp = make_string_out_stream();
  stream_putc(tmp, (int)need_fix(argv[0]));
  return stream_output_string(tmp);
}
PRIM(p_utf8_decode) {
  /* (%utf8-decode string index) -> (values code next-index) */
  UNUSED_ARGS;
  Obj str = need_str(argv[0]); SObj i = need_fix(argv[1]);
  const unsigned char *p = (const unsigned char *)STR_DATA(str); SObj n = (SObj)STR_LEN(str);
  if (i < 0 || i >= n) lisp_error("%%UTF8-DECODE: index out of range");
  int c = p[i], extra = c >= 0xF0 ? 3 : c >= 0xE0 ? 2 : c >= 0xC0 ? 1 : 0;
  int code = extra == 3 ? (c & 7) : extra == 2 ? (c & 15) : extra == 1 ? (c & 31) : c;
  SObj j = i + 1;
  for (; extra > 0 && j < n && (p[j] & 0xC0) == 0x80; extra--, j++) code = (code << 6) | (p[j] & 0x3F);
  mv_vals[0] = FIX(code); mv_vals[1] = FIX(j); mv_count = 2;
  return mv_vals[0];
}

/* ---- system ------------------------------------------------------------ */
PRIM(p_gc) { UNUSED_ARGS; gc_full(); return FIX((SObj)heap_bytes_used()); }
PRIM(p_gc_stress) { UNUSED_ARGS; gc_stress = argv[0] != NIL; gc_pending = gc_stress; return argv[0]; }
PRIM(p_gc_count) { UNUSED_ARGS; return FIX((SObj)gc_count); }
PRIM(p_heap_info) {
  /* (%heap-info) -> (object-bytes cons-bytes semispace-bytes collections) */
  UNUSED_ARGS; size_t o, c, sm; heap_info(&o, &c, &sm);
  return cons(FIX((SObj)o), cons(FIX((SObj)c), cons(FIX((SObj)sm), cons(FIX((SObj)gc_count), NIL))));
}
PRIM(p_heap_used) { UNUSED_ARGS; return FIX((SObj)heap_bytes_used()); }
PRIM(p_exit) {
  int code = (argc > 0 && FIXP(argv[0])) ? (int)UNFIX(argv[0]) : 0;
  fflush(stdout); fflush(stderr);
  exit(code);
}
PRIM(p_real_time) {
  UNUSED_ARGS;
  struct timespec ts; clock_gettime(CLOCK_MONOTONIC, &ts);
  return FIX((SObj)ts.tv_sec * 1000000 + ts.tv_nsec / 1000);
}
PRIM(p_universal_time) { UNUSED_ARGS; return FIX((SObj)time(NULL) + 2208988800LL); }
PRIM(p_local_offset) {
  /* (%local-offset universal-time) -> (values seconds-east-of-UTC daylight-saving-p) for the local time zone */
  UNUSED_ARGS;
  if (!is_integer(argv[0]) || !FIXP(argv[0])) type_error("FIXNUM", argv[0]);
  time_t t = (time_t)(UNFIX(argv[0]) - 2208988800LL);
  struct tm tm;
  if (!localtime_r(&t, &tm)) { mv_vals[0] = FIX(0); mv_vals[1] = NIL; mv_count = 2; return FIX(0); }
  long off = (long)tm.tm_gmtoff;
  mv_vals[0] = FIX(off); mv_vals[1] = BOOL(tm.tm_isdst > 0); mv_count = 2;
  return FIX(off);
}
PRIM(p_sleep) {
  UNUSED_ARGS;
  double d = num_to_double(argv[0]);
  if (d > 0) {
    int tok = proc_blocking_begin();                 /* other processes run while this one sleeps */
    struct timespec ts = { (time_t)d, (long)((d - (double)(time_t)d) * 1e9) };
    while (nanosleep(&ts, &ts) != 0 && errno == EINTR) {}
    proc_blocking_end(tok);
  }
  return NIL;
}
PRIM(p_getenv) { UNUSED_ARGS; char *e = getenv(STR_DATA(need_str(argv[0]))); return e ? make_cstring(e) : NIL; }
PRIM(p_command_line) {
  UNUSED_ARGS;
  Obj l = NIL;
  for (int i = g_argc - 1; i >= 0; i--) l = cons(make_cstring(g_argv[i]), l);
  return l;
}
PRIM(p_save_image) { UNUSED_ARGS; return FIX(save_image(STR_DATA(need_str(argv[0])))); }
PRIM(p_request_save) { UNUSED_ARGS; return FIX(request_save(STR_DATA(need_str(argv[0])))); }
PRIM(p_save_result) { UNUSED_ARGS; int rc = save_result(); return rc == -99 ? NIL : FIX(rc); }
PRIM(p_set_signal_hook_dummy) { UNUSED_ARGS; return NIL; }

void init_prims_core(void) {
  defprim("CONS", p_cons, 2, 2); defprim("CAR", p_car, 1, 1); defprim("CDR", p_cdr, 1, 1);
  defprim("RPLACA", p_rplaca, 2, 2); defprim("RPLACD", p_rplacd, 2, 2);
  defprim("CAAR", p_caar, 1, 1); defprim("CADR", p_cadr, 1, 1); defprim("CDAR", p_cdar, 1, 1);
  defprim("CDDR", p_cddr, 1, 1); defprim("CADDR", p_caddr, 1, 1); defprim("CDDDR", p_cdddr, 1, 1);
  defprim("CADDDR", p_cadddr, 1, 1); defprim("CDDDDR", p_cddddr, 1, 1);
  defprim("FIRST", p_car, 1, 1); defprim("REST", p_cdr, 1, 1); defprim("SECOND", p_cadr, 1, 1); defprim("THIRD", p_caddr, 1, 1);
  defprim("LIST", p_list, 0, -1); defprim("LIST*", p_list_star, 1, -1);
  defprim("APPEND", p_append, 0, -1); defprim("NCONC", p_nconc, 0, -1);
  defprim("LENGTH", p_length, 1, 1); defprim("REVERSE", p_reverse, 1, 1); defprim("NREVERSE", p_nreverse, 1, 1);
  defprim("NTH", p_nth, 2, 2); defprim("NTHCDR", p_nthcdr, 2, 2); defprim("LAST", p_last, 1, 2);
  defprim("COPY-LIST", p_copy_list, 1, 1); defprim("COPY-TREE", p_copy_tree, 1, 1);
  defprim("MEMQ", p_memq, 2, 2); defprim("MEMQL", p_memql, 2, 2); defprim("%MEMBER-EQUAL", p_member_equal, 2, 2);
  defprim("ASSQ", p_assq, 2, 2); defprim("ASSQL", p_assql, 2, 2); defprim("%ASSOC-EQUAL", p_assoc_equal, 2, 2);
  defprim("NULL", p_null, 1, 1); defprim("NOT", p_null, 1, 1); defprim("ATOM", p_atom, 1, 1);
  defprim("CONSP", p_consp, 1, 1); defprim("LISTP", p_listp, 1, 1); defprim("SYMBOLP", p_symbolp, 1, 1);
  defprim("STRINGP", p_stringp, 1, 1); defprim("CHARACTERP", p_characterp, 1, 1);
  defprim("VECTORP", p_vectorp, 1, 1); defprim("SIMPLE-VECTOR-P", p_simple_vector_p, 1, 1);
  defprim("FUNCTIONP", p_functionp, 1, 1); defprim("PACKAGEP", p_packagep, 1, 1);
  defprim("STREAMP", p_streamp, 1, 1); defprim("STRUCTP", p_structp, 1, 1);
  defprim("COMPILED-FUNCTION-P", p_compiled_function_p, 1, 1);
  defprim("EQ", p_eq, 2, 2); defprim("EQL", p_eql, 2, 2); defprim("EQUAL", p_equal, 2, 2); defprim("EQUALP", p_equalp, 2, 2);
  defprim("SXHASH", p_sxhash, 1, 1);
  defprim("%UNSUPPLIED", p_unsupplied, 0, 0); defprim("%GETF-KEY", p_getf_key, 2, 2); defprim("%UNSUPPLIED-P", p_unsupplied_p, 1, 1);
  defprim("SYMBOL-NAME", p_symbol_name, 1, 1); defprim("SYMBOL-VALUE", p_symbol_value, 1, 1);
  defprim("SET", p_set, 2, 2); defprim("SYMBOL-FUNCTION", p_symbol_function, 1, 1);
  defprim("%SET-SYMBOL-FUNCTION", p_set_symbol_function, 2, 2);
  defprim("MACRO-FUNCTION", p_macro_function, 1, 2); defprim("%SET-MACRO-FUNCTION", p_set_macro_function, 2, 2);
  defprim("SPECIAL-OPERATOR-P", p_special_operator_p, 1, 1);
  defprim("SYMBOL-PLIST", p_symbol_plist, 1, 1); defprim("%SET-SYMBOL-PLIST", p_set_symbol_plist, 2, 2);
  defprim("SYMBOL-PACKAGE", p_symbol_package, 1, 1);
  defprim("GET", p_get, 2, 3); defprim("%PUT", p_put, 3, 3);
  defprim("BOUNDP", p_boundp, 1, 1); defprim("FBOUNDP", p_fboundp, 1, 1);
  defprim("MAKUNBOUND", p_makunbound, 1, 1); defprim("FMAKUNBOUND", p_fmakunbound, 1, 1);
  defprim("MAKE-SYMBOL", p_make_symbol, 1, 1); defprim("KEYWORDP", p_keywordp, 1, 1);
  defprim("%PROCLAIM-SPECIAL", p_proclaim_special, 1, 1); defprim("%SPECIAL-P", p_special_p, 1, 1);
  defprim("%CONSTANT-P", p_constant_p, 1, 1); defprim("%PROCLAIM-CONSTANT", p_proclaim_constant, 1, 1);
  defprim_mv("INTERN", p_intern, 1, 2); defprim_mv("FIND-SYMBOL", p_find_symbol, 1, 2);
  defprim("FIND-PACKAGE", p_find_package, 1, 1); defprim("%MAKE-PACKAGE", p_make_package, 3, 3);
  defprim("PACKAGE-NAME", p_package_name, 1, 1); defprim("LIST-ALL-PACKAGES", p_list_all_packages, 0, 0);
  defprim("USE-PACKAGE", p_use_package, 1, 2); defprim("EXPORT", p_export, 1, 2); defprim("IMPORT", p_import, 1, 2);
  defprim("%PACKAGE-SYMBOLS", p_package_symbols, 2, 2); defprim("PACKAGE-USE-LIST", p_package_use_list, 1, 1);
  defprim("%SET-EXPORT-ALL-CL", p_set_export_all, 1, 1);
  defprim_mv("FUNCALL", p_funcall, 1, -1); defprim_mv("APPLY", p_apply, 1, -1);
  defprim_mv("VALUES", p_values, 0, -1); defprim_mv("VALUES-LIST", p_values_list, 1, 1);
  defprim_mv("%MULTIPLE-VALUE-CALL", p_mv_call, 1, -1);
  defprim_mv("%CATCH", p_catch, 2, 2); defprim("THROW", p_throw, 2, 2);
  defprim_mv("%BLOCK", p_block, 1, 1); defprim("%RETURN-FROM", p_return_from, 2, 2);
  defprim("%RETURN-FROM-MV", p_return_from_mv, 2, 2);
  defprim_mv("%UNWIND-PROTECT", p_unwind_protect, 2, 2); defprim_mv("%PROGV", p_progv, 3, 3);
  defprim("%ERROR-TO-TOP", p_error_to_top, 1, 1);
  defprim_mv("%EVAL", p_eval, 1, 1); defprim("IDENTITY", p_identity, 1, 1);
  defprim("%MAKE-CODE", p_make_code, 7, 7); defprim("%CODE-PARTS", p_code_parts, 1, 1);
  defprim("%CODEP", p_codep, 1, 1); defprim("%CODE-CLOSURE", p_code_closure, 2, 2);
  defprim("%FUNCTION-NAME", p_function_name, 1, 1); defprim("%SET-FUNCTION-NAME", p_set_function_name, 2, 2);
  defprim("%FUNCTION-LAMBDA-EXPRESSION", p_function_lambda_expression, 1, 1);
  defprim("%MAKE-STRUCT", p_make_struct, 1, -1); defprim("%STRUCT-REF", p_struct_ref, 2, 2);
  defprim("%STRUCT-SET", p_struct_set, 3, 3); defprim("%STRUCT-TYPE", p_struct_type, 1, 1);
  defprim("%STRUCT-TYPEP", p_struct_typep, 2, 2); defprim("%STRUCT-REF-TYPED", p_struct_ref_typed, 3, 3);
  defprim("%STRUCT-SET-TYPED", p_struct_set_typed, 4, 4); defprim("%COPY-STRUCT", p_copy_struct, 1, 1);
  defprim("%MAKE-INSTANCE", p_make_instance, 2, 2); defprim("%NATIVE-STREAM-P", p_streamp, 1, 1);
  defprim("%BACKTRACE", p_backtrace, 0, 1); defprim("%LAST-READ-START", p_last_read_start, 0, 0);
  defprim("%STREAM-POSITION", p_stream_position, 1, 1); defprim("%UTF8-ENCODE", p_utf8_encode, 1, 1);
  defprim_mv("%UTF8-DECODE", p_utf8_decode, 2, 2);
  defprim("GC", p_gc, 0, 0); defprim("%GC-STRESS", p_gc_stress, 1, 1); defprim("%GC-COUNT", p_gc_count, 0, 0); defprim("%HEAP-INFO", p_heap_info, 0, 0);
  defprim("%HEAP-USED", p_heap_used, 0, 0);
  defprim("EXIT", p_exit, 0, 1); defprim("QUIT", p_exit, 0, 1);
  defprim("GET-INTERNAL-REAL-TIME", p_real_time, 0, 0); defprim("GET-UNIVERSAL-TIME", p_universal_time, 0, 0); defprim_mv("%LOCAL-OFFSET", p_local_offset, 1, 1);
  defprim("SLEEP", p_sleep, 1, 1); defprim("GETENV", p_getenv, 1, 1); defprim("%COMMAND-LINE", p_command_line, 0, 0);
  defprim("%SAVE-IMAGE", p_save_image, 1, 1); defprim("%REQUEST-SAVE", p_request_save, 1, 1); defprim("%SAVE-RESULT", p_save_result, 0, 0);
  (void)p_load_time_dummy; (void)p_set_signal_hook_dummy;
}
