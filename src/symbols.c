/* symbols.c -- symbols, packages, list helpers, equality and hashing. */
#include "neogenera.h"
#include <ctype.h>

Obj PKG_CL, PKG_KEYWORD, PKG_USER, all_packages;
int export_all_cl = 1;

#define X(id, str) Obj Q_##id;
SYMBOL_LIST(X)
#undef X

#define TABLE_SIZE 1021

static uint64_t hash_bytes(const char *s, size_t n) {
  uint64_t h = 1469598103934665603ULL;
  for (size_t i = 0; i < n; i++) { h ^= (unsigned char)s[i]; h *= 1099511628211ULL; }
  return h;
}

/* ---- list helpers ----------------------------------------------------- */
Obj list1(Obj a) { return cons(a, NIL); }
Obj list2(Obj a, Obj b) { return cons(a, cons(b, NIL)); }
Obj list3(Obj a, Obj b, Obj c) { return cons(a, cons(b, cons(c, NIL))); }

Obj car_of(Obj o) {
  if (CONSP(o)) return CAR(o);
  if (o == NIL) return NIL;
  type_error("LIST", o);
}
Obj cdr_of(Obj o) {
  if (CONSP(o)) return CDR(o);
  if (o == NIL) return NIL;
  type_error("LIST", o);
}

long list_length(Obj l) {
  long n = 0; Obj slow = l;
  while (CONSP(l)) {
    l = CDR(l); n++;
    if (!CONSP(l)) break;
    l = CDR(l); n++;
    slow = CDR(slow);
    if (l == slow) return -1;
  }
  return l == NIL ? n : -1;
}

Obj nreverse_list(Obj l) {
  Obj prev = NIL;
  while (CONSP(l)) { Obj next = CDR(l); CDR(l) = prev; prev = l; l = next; }
  return prev;
}

Obj plist_get(Obj plist, Obj key, Obj dflt) {
  while (CONSP(plist) && CONSP(CDR(plist))) {
    if (CAR(plist) == key) return CAR(CDR(plist));
    plist = CDR(CDR(plist));
  }
  return dflt;
}

Obj plist_put(Obj plist, Obj key, Obj val) {
  for (Obj p = plist; CONSP(p) && CONSP(CDR(p)); p = CDR(CDR(p)))
    if (CAR(p) == key) { CAR(CDR(p)) = val; return plist; }
  return cons(key, cons(val, plist));
}

/* ---- equality & hashing ----------------------------------------------- */
int eql(Obj a, Obj b) {
  if (a == b) return 1;
  if (FLONUMP(a) && FLONUMP(b)) return memcmp(&SLOT(a, 0), &SLOT(b, 0), sizeof(double)) == 0;
  return 0;
}

int equal(Obj a, Obj b) {
  for (;;) {
    if (eql(a, b)) return 1;
    if (CONSP(a) && CONSP(b)) { if (!equal(CAR(a), CAR(b))) return 0; a = CDR(a); b = CDR(b); continue; }
    if (STRINGP(a) && STRINGP(b))
      return STR_LEN(a) == STR_LEN(b) && memcmp(STR_DATA(a), STR_DATA(b), STR_LEN(a)) == 0;
    return 0;
  }
}

static int char_eqi(int a, int b) { return tolower(a) == tolower(b); }

int equalp(Obj a, Obj b) {
  for (;;) {
    if (eql(a, b)) return 1;
    if (is_number(a) && is_number(b)) return num_cmp(a, b) == 0;
    if (CHARP(a) && CHARP(b)) return char_eqi(CHAR_CODE(a), CHAR_CODE(b));
    if (CONSP(a) && CONSP(b)) { if (!equalp(CAR(a), CAR(b))) return 0; a = CDR(a); b = CDR(b); continue; }
    if (STRINGP(a) && STRINGP(b)) {
      if (STR_LEN(a) != STR_LEN(b)) return 0;
      for (size_t i = 0; i < STR_LEN(a); i++) if (!char_eqi(STR_DATA(a)[i], STR_DATA(b)[i])) return 0;
      return 1;
    }
    if (VECTORP(a) && VECTORP(b)) {
      if (OBJ_NWORDS(a) != OBJ_NWORDS(b)) return 0;
      for (size_t i = 0; i < OBJ_NWORDS(a); i++) if (!equalp(SLOT(a, i), SLOT(b, i))) return 0;
      return 1;
    }
    return 0;
  }
}

uint64_t sxhash(Obj o, int depth) {
  if (FIXP(o)) return (uint64_t)UNFIX(o) * 0x9E3779B97F4A7C15ULL;
  if (CHARP(o)) return (uint64_t)CHAR_CODE(o) * 31 + 7;
  if (CONSP(o)) {
    if (depth <= 0) return 17;
    uint64_t h = 1; int n = 0;
    while (CONSP(o) && n < 8) { h = h * 31 + sxhash(CAR(o), depth - 1); o = CDR(o); n++; }
    if (!CONSP(o) && o != NIL) h = h * 31 + sxhash(o, depth - 1);
    return h;
  }
  if (!OBJP(o)) return (uint64_t)o;
  switch (OBJ_TYPE(o)) {
  case T_STRING: return hash_bytes(STR_DATA(o), STR_LEN(o));
  case T_SYMBOL: return hash_bytes(STR_DATA(SYM_NAME(o)), STR_LEN(SYM_NAME(o))) ^ 0x5bd1e995;
  case T_FLONUM: { uint64_t b; memcpy(&b, &SLOT(o, 0), 8); return b * 0x9E3779B97F4A7C15ULL; }
  case T_VECTOR: return 0x1234 + OBJ_NWORDS(o);
  default: return 99;
  }
}

/* ---- package tables --------------------------------------------------- */
static Obj table_lookup(Obj table, const char *name, size_t len) {
  size_t idx = hash_bytes(name, len) % TABLE_SIZE;
  for (Obj l = SLOT(table, idx); l != NIL; l = CDR(l)) {
    Obj s = CAR(l), n = SYM_NAME(s);
    if (STR_LEN(n) == len && memcmp(STR_DATA(n), name, len) == 0) return s;
  }
  return UNBOUND;
}

static void table_add(Obj table, Obj sym) {
  Obj n = SYM_NAME(sym);
  size_t idx = hash_bytes(STR_DATA(n), STR_LEN(n)) % TABLE_SIZE;
  SLOT(table, idx) = cons(sym, SLOT(table, idx));
}

/* table_remove intentionally omitted: UNINTERN is not supported yet */

Obj make_package(Obj name, Obj nicks, Obj uses) {
  Obj p = make_obj(T_PACKAGE, PK_SLOTS);
  SLOT(p, PK_NAME) = name;
  SLOT(p, PK_NICK) = nicks;
  SLOT(p, PK_INTERNAL) = make_vector(TABLE_SIZE, NIL);
  SLOT(p, PK_EXTERNAL) = make_vector(TABLE_SIZE, NIL);
  SLOT(p, PK_USES) = NIL;
  SLOT(p, PK_USEDBY) = NIL;
  all_packages = cons(p, all_packages);
  for (Obj u = uses; CONSP(u); u = CDR(u)) use_package(p, CAR(u));
  return p;
}

void use_package(Obj pkg, Obj used) {
  for (Obj l = SLOT(pkg, PK_USES); l != NIL; l = CDR(l)) if (CAR(l) == used) return;
  SLOT(pkg, PK_USES) = cons(used, SLOT(pkg, PK_USES));
  SLOT(used, PK_USEDBY) = cons(pkg, SLOT(used, PK_USEDBY));
}

static int pkg_name_is(Obj pkg, const char *name, size_t len) {
  Obj n = SLOT(pkg, PK_NAME);
  if (STR_LEN(n) == len && memcmp(STR_DATA(n), name, len) == 0) return 1;
  for (Obj l = SLOT(pkg, PK_NICK); CONSP(l); l = CDR(l)) {
    Obj m = CAR(l);
    if (STR_LEN(m) == len && memcmp(STR_DATA(m), name, len) == 0) return 1;
  }
  return 0;
}

Obj find_package_c(const char *name) {
  size_t len = strlen(name);
  for (Obj l = all_packages; CONSP(l); l = CDR(l)) if (pkg_name_is(CAR(l), name, len)) return CAR(l);
  return NIL;
}

Obj find_package(Obj d) {
  if (IS_TYPE(d, T_PACKAGE)) return d;
  Obj s = d;
  if (SYMBOLP(d)) s = SYM_NAME(d);
  if (!STRINGP(s)) return NIL;
  for (Obj l = all_packages; CONSP(l); l = CDR(l))
    if (pkg_name_is(CAR(l), STR_DATA(s), STR_LEN(s))) return CAR(l);
  return NIL;
}

Obj find_symbol_in(Obj namestr, Obj pkg, int *status) {
  const char *n = STR_DATA(namestr); size_t len = STR_LEN(namestr);
  Obj s = table_lookup(SLOT(pkg, PK_INTERNAL), n, len);
  if (s != UNBOUND) {
    *status = table_lookup(SLOT(pkg, PK_EXTERNAL), n, len) != UNBOUND ? 2 : 1;
    return s;
  }
  for (Obj u = SLOT(pkg, PK_USES); u != NIL; u = CDR(u)) {
    s = table_lookup(SLOT(CAR(u), PK_EXTERNAL), n, len);
    if (s != UNBOUND) { *status = 3; return s; }
  }
  *status = 0;
  return NIL;
}

void export_symbol(Obj sym, Obj pkg) {
  Obj n = SYM_NAME(sym);
  if (table_lookup(SLOT(pkg, PK_INTERNAL), STR_DATA(n), STR_LEN(n)) == UNBOUND)
    table_add(SLOT(pkg, PK_INTERNAL), sym);
  if (table_lookup(SLOT(pkg, PK_EXTERNAL), STR_DATA(n), STR_LEN(n)) == UNBOUND)
    table_add(SLOT(pkg, PK_EXTERNAL), sym);
}

Obj intern_obj(Obj namestr, Obj pkg, int *status) {
  Obj s = find_symbol_in(namestr, pkg, status);
  if (*status) return s;
  Obj name = make_string(STR_DATA(namestr), STR_LEN(namestr));
  s = make_symbol_obj(name);
  SYM_PKG(s) = pkg;
  table_add(SLOT(pkg, PK_INTERNAL), s);
  if (pkg == PKG_KEYWORD) {
    table_add(SLOT(pkg, PK_EXTERNAL), s);
    SYM_VALUE(s) = s;
    SYM_FLAGS(s) = FIX(SF_CONSTANT | SF_SPECIAL);
  } else if (pkg == PKG_CL && export_all_cl && STR_DATA(name)[0] != '%') {
    table_add(SLOT(pkg, PK_EXTERNAL), s);
  }
  return s;
}

Obj intern(const char *name, Obj pkg) {
  Obj str = make_cstring(name);
  int st;
  return intern_obj(str, pkg, &st);
}

static Obj intern_up(const char *name, Obj pkg) {
  char buf[256]; size_t n = strlen(name);
  if (n >= sizeof buf) lisp_error("symbol name too long");
  for (size_t i = 0; i <= n; i++) buf[i] = (char)toupper((unsigned char)name[i]);
  return intern(buf, pkg);
}
Obj intern_cl(const char *name) { return intern_up(name, PKG_CL); }
Obj intern_kw(const char *name) { return intern_up(name, PKG_KEYWORD); }

Obj current_package(void) {
  Obj p = SYM_VALUE(Q_package);
  return IS_TYPE(p, T_PACKAGE) ? p : PKG_USER;
}

Obj fn_symbol(Obj name) {
  if (SYMBOLP(name)) return name;
  if (CONSP(name) && CAR(name) == Q_setf && CONSP(CDR(name)) && SYMBOLP(CAR(CDR(name)))) {
    Obj base = CAR(CDR(name));
    char buf[512];
    snprintf(buf, sizeof buf, "(SETF %s)", STR_DATA(SYM_NAME(base)));
    Obj pkg = SYM_PKG(base) == NIL ? current_package() : SYM_PKG(base);
    return intern(buf, pkg);
  }
  type_error("FUNCTION-NAME", name);
}

static void set_special_form(Obj sym, int id) { SYM_FLAGS(sym) = FIX(UNFIX(SYM_FLAGS(sym)) | ((SObj)id << 8)); }

void init_symbols(void) {
  all_packages = NIL;
  add_root(&PKG_CL); add_root(&PKG_KEYWORD); add_root(&PKG_USER); add_root(&all_packages);
#define X(id, str) add_root(&Q_##id);
  SYMBOL_LIST(X)
#undef X
  PKG_CL = make_package(make_cstring("COMMON-LISP"), list1(make_cstring("CL")), NIL);
  PKG_KEYWORD = make_package(make_cstring("KEYWORD"), NIL, NIL);
  PKG_USER = make_package(make_cstring("COMMON-LISP-USER"), list1(make_cstring("CL-USER")), NIL);
  use_package(PKG_USER, PKG_CL);

  /* NIL lives outside the heap; give it a name and put it in CL */
  SYM_NAME(NIL) = make_cstring("NIL");
  SYM_PKG(NIL) = PKG_CL;
  table_add(SLOT(PKG_CL, PK_INTERNAL), NIL);
  table_add(SLOT(PKG_CL, PK_EXTERNAL), NIL);

  TT = intern_cl("T");
  SYM_VALUE(TT) = TT;
  SYM_FLAGS(TT) = FIX(SF_CONSTANT | SF_SPECIAL);

#define X(id, str) Q_##id = (str[0] == ':') ? intern_kw(str + 1) : intern_cl(str);
  SYMBOL_LIST(X)
#undef X

  struct { Obj *s; int id; } sf[] = {
    {&Q_quote, SFID_QUOTE}, {&Q_function, SFID_FUNCTION}, {&Q_lambda, SFID_LAMBDA},
    {&Q_if_, SFID_IF}, {&Q_progn, SFID_PROGN}, {&Q_let, SFID_LET}, {&Q_letstar, SFID_LETSTAR},
    {&Q_setq, SFID_SETQ}, {&Q_flet, SFID_FLET}, {&Q_labels, SFID_LABELS},
    {&Q_macrolet, SFID_MACROLET}, {&Q_block, SFID_BLOCK}, {&Q_return_from, SFID_RETURN_FROM},
    {&Q_tagbody, SFID_TAGBODY}, {&Q_go, SFID_GO}, {&Q_catch, SFID_CATCH},
    {&Q_unwind_protect, SFID_UNWIND_PROTECT}, {&Q_mv_call, SFID_MV_CALL},
    {&Q_mv_prog1, SFID_MV_PROG1}, {&Q_defun, SFID_DEFUN}, {&Q_defmacro, SFID_DEFMACRO},
    {&Q_defvar, SFID_DEFVAR}, {&Q_defparameter, SFID_DEFPARAMETER},
    {&Q_defconstant, SFID_DEFCONSTANT}, {&Q_declare, SFID_DECLARE},
    {&Q_eval_when, SFID_EVAL_WHEN}, {&Q_the, SFID_THE}, {&Q_locally, SFID_LOCALLY},
    {&Q_and_, SFID_AND}, {&Q_or_, SFID_OR}, {&Q_cond, SFID_COND}, {&Q_when, SFID_WHEN},
    {&Q_unless, SFID_UNLESS}, {&Q_progv, SFID_PROGV}, {&Q_ltv, SFID_LTV},
  };
  for (size_t i = 0; i < sizeof sf / sizeof sf[0]; i++) set_special_form(*sf[i].s, sf[i].id);

  Obj specials[] = { Q_package, Q_stdout, Q_stdin, Q_stderr, Q_features, Q_print_pkg, Q_read_eval, Q_in_signal };
  for (size_t i = 0; i < sizeof specials / sizeof specials[0]; i++)
    SYM_FLAGS(specials[i]) = FIX(UNFIX(SYM_FLAGS(specials[i])) | SF_SPECIAL);
  SYM_VALUE(Q_in_signal) = NIL;
  SYM_VALUE(Q_package) = PKG_USER;
  SYM_VALUE(Q_print_pkg) = NIL;
  SYM_VALUE(Q_read_eval) = TT;
  SYM_VALUE(Q_features) = list3(intern_kw("NEOGENERA"), intern_kw("COMMON-LISP"), intern_kw("BYTECODE"));
}
