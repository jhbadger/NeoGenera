/* prims_data.c -- numbers, characters, strings and vectors.
 *
 * Numeric tower for now: fixnum and flonum only.  Overflow signals an error
 * (bignums and ratios are scheduled for Phase 4/8).  `/` returns an integer
 * when the division is exact and a flonum otherwise.
 */
#include "neogenera.h"
#include <ctype.h>
#include <math.h>

static SObj need_fix(Obj o) { if (!FIXP(o)) type_error("FIXNUM", o); return UNFIX(o); }
static Obj need_str(Obj o) { if (!STRINGP(o)) type_error("STRING", o); return o; }
static Obj str_designator(Obj o) {
  if (STRINGP(o)) return o;
  if (SYMBOLP(o)) return SYM_NAME(o);
  if (CHARP(o)) { char c = (char)CHAR_CODE(o); return make_string(&c, 1); }
  type_error("STRING-DESIGNATOR", o);
}

/* ---- characters -------------------------------------------------------- */
static int need_char(Obj o) { if (!CHARP(o)) type_error("CHARACTER", o); return CHAR_CODE(o); }
PRIM(p_char_code) { UNUSED_ARGS; return FIX(need_char(argv[0])); }
PRIM(p_code_char) { UNUSED_ARGS; SObj c = need_fix(argv[0]); if (c < 0 || c > 0x10FFFF) return NIL; return MK_CHAR((int)c); }
PRIM(p_char_upcase) { UNUSED_ARGS; int c = need_char(argv[0]); return c < 128 ? MK_CHAR(toupper(c)) : argv[0]; }
PRIM(p_char_downcase) { UNUSED_ARGS; int c = need_char(argv[0]); return c < 128 ? MK_CHAR(tolower(c)) : argv[0]; }
PRIM(p_alpha_char_p) { UNUSED_ARGS; int c = need_char(argv[0]); return BOOL(c < 128 ? isalpha(c) : c > 191); }
PRIM(p_alphanumericp) { UNUSED_ARGS; int c = need_char(argv[0]); return BOOL(c < 128 ? isalnum(c) : c > 191); }
PRIM(p_upper_case_p) { UNUSED_ARGS; int c = need_char(argv[0]); return BOOL(c < 128 && isupper(c)); }
PRIM(p_lower_case_p) { UNUSED_ARGS; int c = need_char(argv[0]); return BOOL(c < 128 && islower(c)); }
PRIM(p_digit_char_p) {
  int c = need_char(argv[0]), radix = argc > 1 ? (int)need_fix(argv[1]) : 10;
  if (c >= 128) return NIL;
  int u = toupper(c), d = isdigit(u) ? u - '0' : (u >= 'A' && u <= 'Z') ? u - 'A' + 10 : 99;
  return d < radix ? FIX(d) : NIL;
}
PRIM(p_digit_char) { UNUSED_ARGS; SObj w = need_fix(argv[0]); if (w < 0 || w > 35) return NIL; return MK_CHAR(w < 10 ? '0' + (int)w : 'A' + (int)w - 10); }

/* ---- strings ----------------------------------------------------------- */
PRIM(p_string) { UNUSED_ARGS; return str_designator(argv[0]); }
static int cmp_strings(Obj a, Obj b, int ci) {
  size_t la = STR_LEN(a), lb = STR_LEN(b), n = la < lb ? la : lb;
  for (size_t i = 0; i < n; i++) {
    int x = (unsigned char)STR_DATA(a)[i], y = (unsigned char)STR_DATA(b)[i];
    if (ci) { x = tolower(x); y = tolower(y); }
    if (x != y) return x < y ? -1 : 1;
  }
  return la < lb ? -1 : la > lb;
}
PRIM(p_string_compare) { UNUSED_ARGS; return FIX(cmp_strings(str_designator(argv[0]), str_designator(argv[1]), argv[2] != NIL)); }
PRIM(p_string_eq) { UNUSED_ARGS; return BOOL(cmp_strings(str_designator(argv[0]), str_designator(argv[1]), 0) == 0); }
PRIM(p_string_upcase) { UNUSED_ARGS; Obj s = str_designator(argv[0]); Obj r = make_string(STR_DATA(s), STR_LEN(s)); for (size_t i = 0; i < STR_LEN(r); i++) STR_DATA(r)[i] = (char)toupper((unsigned char)STR_DATA(r)[i]); return r; }
PRIM(p_string_downcase) { UNUSED_ARGS; Obj s = str_designator(argv[0]); Obj r = make_string(STR_DATA(s), STR_LEN(s)); for (size_t i = 0; i < STR_LEN(r); i++) STR_DATA(r)[i] = (char)tolower((unsigned char)STR_DATA(r)[i]); return r; }
PRIM(p_string_concat) {
  size_t total = 0;
  for (int i = 0; i < argc; i++) total += STR_LEN(str_designator(argv[i]));
  Obj r = make_string(NULL, total);
  size_t pos = 0;
  for (int i = 0; i < argc; i++) { Obj s = str_designator(argv[i]); memcpy(STR_DATA(r) + pos, STR_DATA(s), STR_LEN(s)); pos += STR_LEN(s); }
  return r;
}
PRIM(p_make_string) {
  SObj n = need_fix(argv[0]);
  if (n < 0) lisp_error("MAKE-STRING: negative length");
  Obj s = make_string(NULL, (size_t)n);
  if (argc > 1 && CHARP(argv[1])) memset(STR_DATA(s), CHAR_CODE(argv[1]) & 0xff, (size_t)n);
  return s;
}
PRIM(p_string_search) {
  /* (%string-search needle haystack start) -> index or NIL */
  UNUSED_ARGS;
  Obj n = str_designator(argv[0]), h = str_designator(argv[1]);
  SObj start = need_fix(argv[2]);
  if (start < 0) start = 0;
  size_t ln = STR_LEN(n), lh = STR_LEN(h);
  if (ln == 0) return (size_t)start <= lh ? FIX(start) : NIL;
  for (size_t i = (size_t)start; i + ln <= lh; i++) if (memcmp(STR_DATA(h) + i, STR_DATA(n), ln) == 0) return FIX(i);
  return NIL;
}
PRIM(p_string_position_char) {
  UNUSED_ARGS;
  Obj s = need_str(argv[0]); int c = need_char(argv[1]); SObj start = need_fix(argv[2]);
  for (size_t i = (size_t)(start < 0 ? 0 : start); i < STR_LEN(s); i++) if ((unsigned char)STR_DATA(s)[i] == c) return FIX(i);
  return NIL;
}

/* ---- vectors and sequences -------------------------------------------- */
PRIM(p_list_to_vector) {
  UNUSED_ARGS;
  long n = list_length(argv[0]);
  if (n < 0) type_error("PROPER-LIST", argv[0]);
  Obj v = make_vector((size_t)n, NIL);
  long i = 0;
  for (Obj l = argv[0]; CONSP(l); l = CDR(l)) SLOT(v, i++) = CAR(l);
  return v;
}
PRIM(p_vector) { Obj v = make_vector((size_t)argc, NIL); for (int i = 0; i < argc; i++) SLOT(v, i) = argv[i]; return v; }
PRIM(p_make_vector) { UNUSED_ARGS; SObj n = need_fix(argv[0]); if (n < 0 || n > (1L << 32)) lisp_error("MAKE-ARRAY: bad size"); return make_vector((size_t)n, argv[1]); }
PRIM(p_vector_to_list) {
  UNUSED_ARGS;
  Obj x = argv[0], l = NIL;
  if (STRINGP(x)) { for (size_t i = STR_LEN(x); i > 0; i--) l = cons(MK_CHAR((unsigned char)STR_DATA(x)[i - 1]), l); return l; }
  if (VECTORP(x)) { for (size_t i = OBJ_NWORDS(x); i > 0; i--) l = cons(SLOT(x, i - 1), l); return l; }
  if (IS_TYPE(x, T_ADVEC)) {
    Obj d = SLOT(x, AV_DATA); long f = (long)UNFIX(SLOT(x, AV_FILL));
    for (long i = f; i > 0; i--) l = cons(STRINGP(d) ? MK_CHAR((unsigned char)STR_DATA(d)[i - 1]) : SLOT(d, i - 1), l);
    return l;
  }
  if (LISTP(x)) return x;
  type_error("SEQUENCE", x);
}
PRIM(p_aref) {
  UNUSED_ARGS;
  Obj x = argv[0]; SObj i = need_fix(argv[1]);
  if (VECTORP(x)) { if (i < 0 || (size_t)i >= OBJ_NWORDS(x)) lisp_error("Index %ld out of bounds for vector of length %zu", (long)i, OBJ_NWORDS(x)); return SLOT(x, i); }
  if (STRINGP(x)) { if (i < 0 || (size_t)i >= STR_LEN(x)) lisp_error("Index %ld out of bounds for string of length %zu", (long)i, STR_LEN(x)); return MK_CHAR((unsigned char)STR_DATA(x)[i]); }
  if (IS_TYPE(x, T_ADVEC)) {
    Obj d = SLOT(x, AV_DATA); long f = (long)UNFIX(SLOT(x, AV_FILL));
    if (i < 0 || i >= f) lisp_error("Index %ld out of bounds (fill pointer %ld)", (long)i, f);
    return STRINGP(d) ? MK_CHAR((unsigned char)STR_DATA(d)[i]) : SLOT(d, i);
  }
  type_error("ARRAY", x);
}
PRIM(p_aset) {
  UNUSED_ARGS;
  Obj x = argv[0]; SObj i = need_fix(argv[1]); Obj v = argv[2];
  if (VECTORP(x)) { if (i < 0 || (size_t)i >= OBJ_NWORDS(x)) lisp_error("Index %ld out of bounds", (long)i); SLOT(x, i) = v; return v; }
  if (STRINGP(x)) { if (i < 0 || (size_t)i >= STR_LEN(x)) lisp_error("Index %ld out of bounds", (long)i); STR_DATA(x)[i] = (char)need_char(v); return v; }
  if (IS_TYPE(x, T_ADVEC)) {
    Obj d = SLOT(x, AV_DATA); long f = (long)UNFIX(SLOT(x, AV_FILL));
    if (i < 0 || i >= f) lisp_error("Index %ld out of bounds", (long)i);
    if (STRINGP(d)) STR_DATA(d)[i] = (char)need_char(v); else SLOT(d, i) = v;
    return v;
  }
  type_error("ARRAY", x);
}
PRIM(p_make_adjustable) {
  /* (%make-adjustable capacity stringp) -> adjustable vector with fill pointer 0 */
  UNUSED_ARGS;
  SObj cap = need_fix(argv[0]); if (cap < 4) cap = 4;
  Obj v = make_obj(T_ADVEC, AV_SLOTS);
  SLOT(v, AV_DATA) = argv[1] != NIL ? make_string(NULL, (size_t)cap) : make_vector((size_t)cap, NIL);
  SLOT(v, AV_FILL) = FIX(0);
  return v;
}
PRIM(p_vector_push_extend) {
  UNUSED_ARGS;
  Obj v = argv[1];
  if (!IS_TYPE(v, T_ADVEC)) type_error("ADJUSTABLE-VECTOR", v);
  Obj d = SLOT(v, AV_DATA); long f = (long)UNFIX(SLOT(v, AV_FILL));
  size_t cap = STRINGP(d) ? STR_LEN(d) : OBJ_NWORDS(d);
  if ((size_t)f >= cap) {
    size_t ncap = cap * 2;
    Obj nd = STRINGP(d) ? make_string(NULL, ncap) : make_vector(ncap, NIL);
    if (STRINGP(d)) memcpy(STR_DATA(nd), STR_DATA(d), (size_t)f);
    else for (long i = 0; i < f; i++) SLOT(nd, i) = SLOT(d, i);
    SLOT(v, AV_DATA) = nd; d = nd;
  }
  if (STRINGP(d)) STR_DATA(d)[f] = (char)need_char(argv[0]); else SLOT(d, f) = argv[0];
  SLOT(v, AV_FILL) = FIX(f + 1);
  return FIX(f);
}
PRIM(p_fill_pointer) { UNUSED_ARGS; if (!IS_TYPE(argv[0], T_ADVEC)) return NIL; return SLOT(argv[0], AV_FILL); }
PRIM(p_set_fill_pointer) { UNUSED_ARGS; if (!IS_TYPE(argv[0], T_ADVEC)) type_error("ADJUSTABLE-VECTOR", argv[0]); SLOT(argv[0], AV_FILL) = FIX(need_fix(argv[1])); return argv[1]; }
PRIM(p_advecp) { UNUSED_ARGS; return BOOL(IS_TYPE(argv[0], T_ADVEC)); }
PRIM(p_subseq) {
  Obj x = argv[0]; SObj s = need_fix(argv[1]);
  if (LISTP(x)) {
    long len = list_length(x); if (len < 0) type_error("PROPER-LIST", x);
    SObj e = (argc > 2 && argv[2] != NIL) ? need_fix(argv[2]) : len;
    if (s < 0 || e > len || s > e) lisp_error("SUBSEQ: bad bounds %ld %ld", (long)s, (long)e);
    for (SObj i = 0; i < s; i++) x = CDR(x);
    Obj head = NIL, tail = NIL;
    for (SObj i = s; i < e; i++, x = CDR(x)) { Obj c = cons(CAR(x), NIL); if (head == NIL) head = c; else CDR(tail) = c; tail = c; }
    return head;
  }
  long len;
  if (STRINGP(x)) len = (long)STR_LEN(x); else if (VECTORP(x)) len = (long)OBJ_NWORDS(x);
  else if (IS_TYPE(x, T_ADVEC)) len = (long)UNFIX(SLOT(x, AV_FILL)); else type_error("SEQUENCE", x);
  SObj e = (argc > 2 && argv[2] != NIL) ? need_fix(argv[2]) : len;
  if (s < 0 || e > len || s > e) lisp_error("SUBSEQ: bad bounds %ld %ld for length %ld", (long)s, (long)e, len);
  if (STRINGP(x)) return make_string(STR_DATA(x) + s, (size_t)(e - s));
  Obj src = IS_TYPE(x, T_ADVEC) ? SLOT(x, AV_DATA) : x;
  if (STRINGP(src)) return make_string(STR_DATA(src) + s, (size_t)(e - s));
  Obj v = make_vector((size_t)(e - s), NIL);
  for (SObj i = s; i < e; i++) SLOT(v, i - s) = SLOT(src, i);
  return v;
}

void init_prims_data(void) {
  defprim("CHAR-CODE", p_char_code, 1, 1); defprim("CHAR-INT", p_char_code, 1, 1); defprim("CODE-CHAR", p_code_char, 1, 1);
  defprim("CHAR-UPCASE", p_char_upcase, 1, 1); defprim("CHAR-DOWNCASE", p_char_downcase, 1, 1);
  defprim("ALPHA-CHAR-P", p_alpha_char_p, 1, 1); defprim("ALPHANUMERICP", p_alphanumericp, 1, 1);
  defprim("UPPER-CASE-P", p_upper_case_p, 1, 1); defprim("LOWER-CASE-P", p_lower_case_p, 1, 1);
  defprim("DIGIT-CHAR-P", p_digit_char_p, 1, 2); defprim("DIGIT-CHAR", p_digit_char, 1, 2);
  defprim("STRING", p_string, 1, 1); defprim("%STRING-COMPARE", p_string_compare, 3, 3);
  defprim("STRING=", p_string_eq, 2, 2);
  defprim("STRING-UPCASE", p_string_upcase, 1, 1); defprim("STRING-DOWNCASE", p_string_downcase, 1, 1);
  defprim("%STRING-CONCAT", p_string_concat, 0, -1); defprim("MAKE-STRING", p_make_string, 1, 2);
  defprim("%STRING-SEARCH", p_string_search, 3, 3); defprim("%STRING-POSITION-CHAR", p_string_position_char, 3, 3);
  defprim("%LIST-TO-VECTOR", p_list_to_vector, 1, 1); defprim("VECTOR", p_vector, 0, -1);
  defprim("%MAKE-VECTOR", p_make_vector, 2, 2); defprim("%VECTOR-TO-LIST", p_vector_to_list, 1, 1);
  defprim("AREF", p_aref, 2, 2); defprim("SVREF", p_aref, 2, 2); defprim("CHAR", p_aref, 2, 2); defprim("SCHAR", p_aref, 2, 2);
  defprim("%ASET", p_aset, 3, 3);
  defprim("%MAKE-ADJUSTABLE", p_make_adjustable, 2, 2); defprim("VECTOR-PUSH-EXTEND", p_vector_push_extend, 2, 3);
  defprim("FILL-POINTER", p_fill_pointer, 1, 1); defprim("%SET-FILL-POINTER", p_set_fill_pointer, 2, 2);
  defprim("%ADVEC-P", p_advecp, 1, 1);
  defprim("SUBSEQ", p_subseq, 2, 3);
}
