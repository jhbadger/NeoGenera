/* symbols.c -- symbols, packages, list helpers, equality and hashing. */
#include "neogenera.h"
#include <ctype.h>

Obj PKG_CL, PKG_KEYWORD, PKG_USER, all_packages;
int export_all_cl = 1;

#define X(id, str) Obj Q_##id;
SYMBOL_LIST(X)
#undef X

#define TABLE_SIZE 1021


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
  while (CONSP(l)) { Obj next = CDR(l); CDR(l) = prev; WB(l, prev); prev = l; l = next; }
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
    if (CAR(p) == key) { CAR(CDR(p)) = val; WB(CDR(p), val); return plist; }
  return cons(key, cons(val, plist));
}

/* ---- equality & hashing ----------------------------------------------- */
int eql(Obj a, Obj b) {
  if (a == b) return 1;
  if (FLONUMP(a) && FLONUMP(b)) return memcmp(&SLOT(a, 0), &SLOT(b, 0), sizeof(double)) == 0;
  if (OBJP(a) && OBJP(b) && (OBJ_TYPE(a) == T_BIGNUM || OBJ_TYPE(a) == T_RATIO || OBJ_TYPE(a) == T_COMPLEX) && OBJ_TYPE(a) == OBJ_TYPE(b)) return num_eql(a, b);
  return 0;
}

/* Is O a PATHNAME or LOGICAL-PATHNAME structure?  EQUAL compares those by component. */
static int pathname_struct_p(Obj o) {
  if (!IS_TYPE(o, T_STRUCT)) return 0;
  Obj type = SLOT(o, 0);
  Obj name = CONSP(type) ? CAR(type) : type;
  if (!SYMBOLP(name) || !STRINGP(SYM_NAME(name))) return 0;
  return str_equal_c(SYM_NAME(name), "PATHNAME", 8) || str_equal_c(SYM_NAME(name), "LOGICAL-PATHNAME", 16);
}

int equal(Obj a, Obj b) {
  STACK_CHECK();
  for (;;) {
    if (eql(a, b)) return 1;
    if (CONSP(a) && CONSP(b)) { if (!equal(CAR(a), CAR(b))) return 0; a = CDR(a); b = CDR(b); continue; }
    if (STRINGP(a) && STRINGP(b)) return str_equal(a, b);
    if (pathname_struct_p(a) && pathname_struct_p(b) && SLOT(a, 0) == SLOT(b, 0) && OBJ_NWORDS(a) == OBJ_NWORDS(b)) {
      for (size_t i = 1; i < OBJ_NWORDS(a); i++) if (!equal(SLOT(a, i), SLOT(b, i))) return 0;   /* pathnames: by component */
      return 1;
    }
    return 0;
  }
}

static int char_eqi(int a, int b) { return a == b || cp_downcase(a) == cp_downcase(b); }

int equalp(Obj a, Obj b) {
  STACK_CHECK();
  for (;;) {
    if (eql(a, b)) return 1;
    if (is_number(a) && is_number(b)) return num_equal(a, b);
    if (CHARP(a) && CHARP(b)) return char_eqi(CHAR_CODE(a), CHAR_CODE(b));
    if (CONSP(a) && CONSP(b)) { if (!equalp(CAR(a), CAR(b))) return 0; a = CDR(a); b = CDR(b); continue; }
    if (STRINGP(a) && STRINGP(b)) {
      size_t n = str_len(a);
      if (n != str_len(b)) return 0;
      for (size_t i = 0; i < n; i++) if (!char_eqi(str_ref(a, i), str_ref(b, i))) return 0;
      return 1;
    }
    if (VECTORP(a) && VECTORP(b)) {
      if (OBJ_NWORDS(a) != OBJ_NWORDS(b)) return 0;
      for (size_t i = 0; i < OBJ_NWORDS(a); i++) if (!equalp(SLOT(a, i), SLOT(b, i))) return 0;
      return 1;
    }
    if (IS_TYPE(a, T_MDARRAY) && IS_TYPE(b, T_MDARRAY)) {       /* same dimensions, elements EQUALP */
      if (!equal(SLOT(a, AR_DIMS), SLOT(b, AR_DIMS))) return 0;
      a = SLOT(a, AR_DATA); b = SLOT(b, AR_DATA);
      continue;
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
  case T_STRING: case T_WSTRING: case T_STRBOX: return str_hash(o);
  case T_SYMBOL: return str_hash(SYM_NAME(o)) ^ 0x5bd1e995;
  case T_FLONUM: { uint64_t b; memcpy(&b, &SLOT(o, 0), 8); return b * 0x9E3779B97F4A7C15ULL; }
  case T_VECTOR: return 0x1234 + OBJ_NWORDS(o);
  case T_BIGNUM: case T_RATIO: case T_COMPLEX: return num_hash(o);
  default: return 99;
  }
}

/* ---- package tables --------------------------------------------------- */
/* The symbol named NAME (a string) in TABLE, or UNBOUND. */
static Obj table_lookup(Obj table, Obj name) {
  size_t idx = str_hash(name) % TABLE_SIZE;
  for (Obj l = SLOT(table, idx); l != NIL; l = CDR(l)) {
    Obj s = CAR(l);
    if (str_equal(SYM_NAME(s), name)) return s;
  }
  return UNBOUND;
}

static void table_add(Obj table, Obj sym) {
  Obj n = SYM_NAME(sym);
  size_t idx = str_hash(n) % TABLE_SIZE;
  SLOT(table, idx) = cons(sym, SLOT(table, idx)); WB(table, SLOT(table, idx));
}

/* Remove SYM (by identity) from TABLE; 1 if it was there. */
static int table_remove(Obj table, Obj sym) {
  size_t idx = str_hash(SYM_NAME(sym)) % TABLE_SIZE;
  Obj prev = NIL;
  for (Obj l = SLOT(table, idx); l != NIL; prev = l, l = CDR(l)) {
    if (CAR(l) != sym) continue;
    if (prev == NIL) { SLOT(table, idx) = CDR(l); WB(table, SLOT(table, idx)); }
    else { CDR(prev) = CDR(l); WB(prev, CDR(prev)); }
    return 1;
  }
  return 0;
}

/* A fresh list: L without the element X. */
static Obj list_without(Obj l, Obj x) {
  Obj r = NIL;
  for (; CONSP(l); l = CDR(l)) if (CAR(l) != x) r = cons(CAR(l), r);
  return nreverse_list(r);
}
static int list_has(Obj l, Obj x) {
  for (; CONSP(l); l = CDR(l)) if (CAR(l) == x) return 1;
  return 0;
}

/* ---- package locks ------------------------------------------------------ */
int pkg_locked(Obj pkg) { return SLOT(pkg, PK_LOCK) != NIL; }

static int locks_off(void) { return SYM_VALUE(Q_pkg_locks_off) != NIL && SYM_VALUE(Q_pkg_locks_off) != UNBOUND; }

void pkg_lock_check(Obj pkg, const char *op) {
  if (!pkg_locked(pkg) || locks_off()) return;
  lisp_error("Package lock violation: cannot %s the locked package %s", op, STR_C(SLOT(pkg, PK_NAME)));
}

void sym_lock_check(Obj sym, const char *op) {
  Obj pkg;
  if (!SYMBOLP(sym) || !IS_TYPE((pkg = SYM_PKG(sym)), T_PACKAGE) || !pkg_locked(pkg) || locks_off()) return;
  lisp_error("Package lock violation: cannot %s %s, a symbol of the locked package %s",
             op, STR_C(SYM_NAME(sym)), STR_C(SLOT(pkg, PK_NAME)));
}

/* Names of the form "(SETF FOO)" are the function names behind (setf foo): creating them is bookkeeping, not
   a definition, so interning one never trips a package lock (defining the function still does). */
static int setf_name_p(Obj name) {
  static const char pre[] = "(SETF ";
  if (str_len(name) < sizeof pre - 1) return 0;
  for (size_t i = 0; i < sizeof pre - 1; i++) if (str_ref(name, i) != (unsigned char)pre[i]) return 0;
  return 1;
}

/* A name conflict is a package error, signalled with the "Package error:" prefix (see %CONDITION-CLASS-FOR). */
static void name_conflict(Obj a, Obj b, Obj pkg, const char *what) {
  lisp_error("Package error: %s would cause a name conflict in package %s between %s and %s", what,
             STR_C(SLOT(pkg, PK_NAME)), STR_C(print_to_string(a, 1, 0)), STR_C(print_to_string(b, 1, 0)));
}

int package_shadows_p(Obj pkg, Obj sym) { return list_has(SLOT(pkg, PK_SHADOW), sym); }

Obj make_package(Obj name, Obj nicks, Obj uses) {
  Obj p = make_obj(T_PACKAGE, PK_SLOTS);
  SLOT(p, PK_NAME) = name;
  SLOT(p, PK_NICK) = nicks;
  SLOT(p, PK_INTERNAL) = make_vector(TABLE_SIZE, NIL);
  SLOT(p, PK_EXTERNAL) = make_vector(TABLE_SIZE, NIL);
  SLOT(p, PK_USES) = NIL;
  SLOT(p, PK_USEDBY) = NIL;
  SLOT(p, PK_SHADOW) = NIL;
  SLOT(p, PK_LOCK) = NIL;
  all_packages = cons(p, all_packages);
  for (Obj u = uses; CONSP(u); u = CDR(u)) use_package(p, CAR(u));
  return p;
}

void use_package(Obj pkg, Obj used) {
  for (Obj l = SLOT(pkg, PK_USES); l != NIL; l = CDR(l)) if (CAR(l) == used) return;
  if (pkg == used) return;
  pkg_lock_check(pkg, "use a package in");
  /* every external symbol of USED must not clash with a different symbol accessible in PKG */
  Obj ext = SLOT(used, PK_EXTERNAL);
  for (size_t i = 0; i < OBJ_NWORDS(ext); i++)
    for (Obj l = SLOT(ext, i); CONSP(l); l = CDR(l)) {
      Obj s = CAR(l); int st;
      Obj other = find_symbol_in(SYM_NAME(s), pkg, &st);
      if (st && other != s && !list_has(SLOT(pkg, PK_SHADOW), other)) name_conflict(s, other, pkg, "using a package");
    }
  SLOT(pkg, PK_USES) = cons(used, SLOT(pkg, PK_USES)); WB(pkg, SLOT(pkg, PK_USES));
  SLOT(used, PK_USEDBY) = cons(pkg, SLOT(used, PK_USEDBY)); WB(used, SLOT(used, PK_USEDBY));
}

void unuse_package(Obj pkg, Obj used) {
  if (!list_has(SLOT(pkg, PK_USES), used)) return;
  pkg_lock_check(pkg, "stop using a package in");
  SLOT(pkg, PK_USES) = list_without(SLOT(pkg, PK_USES), used); WB(pkg, SLOT(pkg, PK_USES));
  SLOT(used, PK_USEDBY) = list_without(SLOT(used, PK_USEDBY), pkg); WB(used, SLOT(used, PK_USEDBY));
}


static int pkg_name_is(Obj pkg, Obj name) {
  if (str_equal(SLOT(pkg, PK_NAME), name)) return 1;
  for (Obj l = SLOT(pkg, PK_NICK); CONSP(l); l = CDR(l)) if (str_equal(CAR(l), name)) return 1;
  return 0;
}
static int pkg_name_is_c(Obj pkg, const char *name, size_t len) {
  if (str_equal_c(SLOT(pkg, PK_NAME), name, len)) return 1;
  for (Obj l = SLOT(pkg, PK_NICK); CONSP(l); l = CDR(l)) if (str_equal_c(CAR(l), name, len)) return 1;
  return 0;
}

Obj find_package_c(const char *name) {
  size_t len = strlen(name);
  for (Obj l = all_packages; CONSP(l); l = CDR(l)) if (pkg_name_is_c(CAR(l), name, len)) return CAR(l);
  return NIL;
}

Obj find_package(Obj d) {
  if (IS_TYPE(d, T_PACKAGE)) return d;
  Obj s = d;
  if (SYMBOLP(d)) s = SYM_NAME(d);
  if (!STRINGP(s)) return NIL;
  for (Obj l = all_packages; CONSP(l); l = CDR(l))
    if (pkg_name_is(CAR(l), s)) return CAR(l);
  return NIL;
}

Obj find_symbol_in(Obj namestr, Obj pkg, int *status) {
  Obj s = table_lookup(SLOT(pkg, PK_INTERNAL), namestr);
  if (s != UNBOUND) {
    *status = table_lookup(SLOT(pkg, PK_EXTERNAL), namestr) != UNBOUND ? 2 : 1;
    return s;
  }
  for (Obj u = SLOT(pkg, PK_USES); u != NIL; u = CDR(u)) {
    s = table_lookup(SLOT(CAR(u), PK_EXTERNAL), namestr);
    if (s != UNBOUND) { *status = 3; return s; }
  }
  *status = 0;
  return NIL;
}

void export_symbol(Obj sym, Obj pkg) {
  Obj n = SYM_NAME(sym);
  if (table_lookup(SLOT(pkg, PK_EXTERNAL), n) == sym) return;          /* already external */
  pkg_lock_check(pkg, "export a symbol from");
  Obj here = table_lookup(SLOT(pkg, PK_INTERNAL), n);
  if (here != UNBOUND && here != sym) name_conflict(sym, here, pkg, "exporting");
  if (here == UNBOUND) {
    int st; Obj acc = find_symbol_in(n, pkg, &st);
    if (st == 3 && acc != sym) name_conflict(sym, acc, pkg, "exporting");
  }
  /* exporting must not clash with a different symbol accessible in a package that uses PKG */
  for (Obj u = SLOT(pkg, PK_USEDBY); CONSP(u); u = CDR(u)) {
    int st; Obj other = find_symbol_in(n, CAR(u), &st);
    if (st && other != sym && !list_has(SLOT(CAR(u), PK_SHADOW), other))
      name_conflict(sym, other, CAR(u), "exporting");
  }
  if (here == UNBOUND) table_add(SLOT(pkg, PK_INTERNAL), sym);
  table_add(SLOT(pkg, PK_EXTERNAL), sym);
}

/* UNEXPORT: the symbol stays present in PKG, but is no longer external.  1 if it was external. */
int unexport_symbol(Obj sym, Obj pkg) {
  if (table_lookup(SLOT(pkg, PK_EXTERNAL), SYM_NAME(sym)) != sym) return 0;
  pkg_lock_check(pkg, "unexport a symbol from");
  table_remove(SLOT(pkg, PK_EXTERNAL), sym);
  return 1;
}

/* UNINTERN: remove SYM from PKG if it is present there.  The symbol becomes uninterned if PKG was its home. */
int unintern_symbol(Obj sym, Obj pkg) {
  if (table_lookup(SLOT(pkg, PK_INTERNAL), SYM_NAME(sym)) != sym) return 0;
  pkg_lock_check(pkg, "unintern a symbol from");
  if (list_has(SLOT(pkg, PK_SHADOW), sym)) {
    /* removing a shadowing symbol may uncover two different inherited symbols of the same name */
    Obj found = UNBOUND;
    for (Obj u = SLOT(pkg, PK_USES); CONSP(u); u = CDR(u)) {
      Obj s = table_lookup(SLOT(CAR(u), PK_EXTERNAL), SYM_NAME(sym));
      if (s == UNBOUND) continue;
      if (found != UNBOUND && found != s) name_conflict(found, s, pkg, "uninterning a shadowing symbol");
      found = s;
    }
    SLOT(pkg, PK_SHADOW) = list_without(SLOT(pkg, PK_SHADOW), sym); WB(pkg, SLOT(pkg, PK_SHADOW));
  }
  table_remove(SLOT(pkg, PK_EXTERNAL), sym);
  table_remove(SLOT(pkg, PK_INTERNAL), sym);
  if (SYM_PKG(sym) == pkg) SYM_PKG(sym) = NIL;
  return 1;
}

static void add_shadow(Obj pkg, Obj sym) {
  if (list_has(SLOT(pkg, PK_SHADOW), sym)) return;
  SLOT(pkg, PK_SHADOW) = cons(sym, SLOT(pkg, PK_SHADOW)); WB(pkg, SLOT(pkg, PK_SHADOW));
}

/* SHADOW: make a symbol named NAMESTR present in PKG (creating one if none is present) and record it as shadowing. */
Obj shadow_name(Obj namestr, Obj pkg) {
  Obj s = table_lookup(SLOT(pkg, PK_INTERNAL), namestr);
  if (s == UNBOUND) {
    pkg_lock_check(pkg, "shadow a symbol in");
    s = make_symbol_obj(substring(namestr, 0, str_len(namestr)));
    SYM_PKG(s) = pkg; WB(s, pkg);
    table_add(SLOT(pkg, PK_INTERNAL), s);
  } else if (!list_has(SLOT(pkg, PK_SHADOW), s)) pkg_lock_check(pkg, "shadow a symbol in");
  add_shadow(pkg, s);
  return s;
}

/* IMPORT one symbol into PKG, signalling a name conflict with any different accessible symbol. */
void import_symbol(Obj sym, Obj pkg) {
  Obj name = SYM_NAME(sym); int st;
  Obj ex = find_symbol_in(name, pkg, &st);
  if (st == 1 || st == 2) {
    if (ex != sym) name_conflict(sym, ex, pkg, "importing");
    return;                                                         /* already present */
  }
  pkg_lock_check(pkg, "import a symbol into");
  if (st == 3 && ex != sym) name_conflict(sym, ex, pkg, "importing");
  table_add(SLOT(pkg, PK_INTERNAL), sym);
  if (SYM_PKG(sym) == NIL) { SYM_PKG(sym) = pkg; WB(sym, pkg); }
}

/* SHADOWING-IMPORT: like IMPORT, but a different symbol of that name already present in PKG is displaced. */
void shadowing_import_symbol(Obj sym, Obj pkg) {
  pkg_lock_check(pkg, "shadowing-import a symbol into");
  Obj here = table_lookup(SLOT(pkg, PK_INTERNAL), SYM_NAME(sym));
  if (here != UNBOUND && here != sym) {
    table_remove(SLOT(pkg, PK_EXTERNAL), here);
    table_remove(SLOT(pkg, PK_INTERNAL), here);
    SLOT(pkg, PK_SHADOW) = list_without(SLOT(pkg, PK_SHADOW), here); WB(pkg, SLOT(pkg, PK_SHADOW));
    if (SYM_PKG(here) == pkg) SYM_PKG(here) = NIL;
    here = UNBOUND;
  }
  if (here == UNBOUND) {
    table_add(SLOT(pkg, PK_INTERNAL), sym);
    if (SYM_PKG(sym) == NIL) { SYM_PKG(sym) = pkg; WB(sym, pkg); }
  }
  add_shadow(pkg, sym);
}

/* DELETE-PACKAGE: the caller has already checked that no other package uses PKG.  0 if already deleted. */
int delete_package(Obj pkg) {
  if (SLOT(pkg, PK_NAME) == NIL) return 0;
  if (pkg == PKG_CL || pkg == PKG_KEYWORD) lisp_error("Package error: cannot delete the standard package %s", STR_C(SLOT(pkg, PK_NAME)));
  pkg_lock_check(pkg, "delete");
  for (Obj l = SLOT(pkg, PK_USES); CONSP(l); l = CDR(l)) {
    SLOT(CAR(l), PK_USEDBY) = list_without(SLOT(CAR(l), PK_USEDBY), pkg); WB(CAR(l), SLOT(CAR(l), PK_USEDBY));
  }
  for (Obj l = SLOT(pkg, PK_USEDBY); CONSP(l); l = CDR(l)) {
    SLOT(CAR(l), PK_USES) = list_without(SLOT(CAR(l), PK_USES), pkg); WB(CAR(l), SLOT(CAR(l), PK_USES));
  }
  Obj tab = SLOT(pkg, PK_INTERNAL);
  for (size_t i = 0; i < OBJ_NWORDS(tab); i++)
    for (Obj l = SLOT(tab, i); CONSP(l); l = CDR(l)) if (SYM_PKG(CAR(l)) == pkg) SYM_PKG(CAR(l)) = NIL;
  all_packages = list_without(all_packages, pkg);
  SLOT(pkg, PK_NAME) = NIL; SLOT(pkg, PK_NICK) = NIL; SLOT(pkg, PK_USES) = NIL; SLOT(pkg, PK_USEDBY) = NIL;
  SLOT(pkg, PK_SHADOW) = NIL;
  SLOT(pkg, PK_INTERNAL) = make_vector(TABLE_SIZE, NIL); WB(pkg, SLOT(pkg, PK_INTERNAL));
  SLOT(pkg, PK_EXTERNAL) = make_vector(TABLE_SIZE, NIL); WB(pkg, SLOT(pkg, PK_EXTERNAL));
  return 1;
}

/* RENAME-PACKAGE: NEWNAME and each of NICKS (strings) must not name a different package. */
void rename_package(Obj pkg, Obj newname, Obj nicks) {
  pkg_lock_check(pkg, "rename");
  Obj clash = find_package(newname);
  if (clash != NIL && clash != pkg) lisp_error("Package error: a package named %s already exists", STR_C(newname));
  for (Obj l = nicks; CONSP(l); l = CDR(l)) {
    clash = find_package(CAR(l));
    if (clash != NIL && clash != pkg) lisp_error("Package error: a package named %s already exists", STR_C(CAR(l)));
  }
  SLOT(pkg, PK_NAME) = newname; WB(pkg, newname);
  SLOT(pkg, PK_NICK) = nicks; WB(pkg, nicks);
}


Obj intern_obj(Obj namestr, Obj pkg, int *status) {
  Obj s = find_symbol_in(namestr, pkg, status);
  if (*status) return s;
  if (pkg != PKG_KEYWORD && !setf_name_p(namestr)) {
    if (pkg_locked(pkg) && !locks_off())
      lisp_error("Package lock violation: cannot intern %s in the locked package %s", STR_C(namestr), STR_C(SLOT(pkg, PK_NAME)));
  }
  Obj name = substring(namestr, 0, str_len(namestr));          /* a private copy, narrow when it can be */
  s = make_symbol_obj(name);
  SYM_PKG(s) = pkg; WB(s, pkg);
  table_add(SLOT(pkg, PK_INTERNAL), s);
  if (pkg == PKG_KEYWORD) {
    table_add(SLOT(pkg, PK_EXTERNAL), s);
    SYM_VALUE(s) = s;
    SYM_FLAGS(s) = FIX(SF_CONSTANT | SF_SPECIAL);
  } else if (pkg == PKG_CL && export_all_cl && !(str_len(name) > 0 && str_ref(name, 0) == '%')) {
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
    snprintf(buf, sizeof buf, "(SETF %s)", STR_C(SYM_NAME(base)));
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

#define X(id, str) Q_##id = (str[0] == ':') ? intern_kw(&(str)[1]) : intern_cl(str);
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

  Obj specials[] = { Q_package, Q_stdout, Q_stdin, Q_stderr, Q_features, Q_print_pkg, Q_read_eval, Q_in_signal, Q_readtable,
                     Q_print_base, Q_print_radix, Q_print_pretty, Q_pkg_locks_off, Q_print_level, Q_print_length, Q_print_circle, Q_print_case };
  for (size_t i = 0; i < sizeof specials / sizeof specials[0]; i++)
    SYM_FLAGS(specials[i]) = FIX(UNFIX(SYM_FLAGS(specials[i])) | SF_SPECIAL);
  SYM_VALUE(Q_pkg_locks_off) = NIL;
  SYM_VALUE(Q_print_level) = NIL; SYM_VALUE(Q_print_length) = NIL; SYM_VALUE(Q_print_circle) = NIL;
  SYM_VALUE(Q_print_case) = intern_kw("UPCASE");
  SYM_VALUE(Q_print_base) = FIX(10);
  SYM_VALUE(Q_print_radix) = NIL;
  SYM_VALUE(Q_print_pretty) = NIL;
  SYM_VALUE(Q_in_signal) = NIL;
  SYM_VALUE(Q_package) = PKG_USER;
  SYM_VALUE(Q_print_pkg) = NIL;
  SYM_VALUE(Q_read_eval) = TT;
  SYM_VALUE(Q_readtable) = NIL;                 /* NIL: the built-in syntax only (until Lisp installs a readtable) */
  SYM_VALUE(Q_features) = list3(intern_kw("NEOGENERA"), intern_kw("COMMON-LISP"), intern_kw("BYTECODE"));
}
