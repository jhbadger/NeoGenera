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

/* ---- numbers ---------------------------------------------------------- */
int is_number(Obj o) { return FIXP(o) || FLONUMP(o); }
double num_to_double(Obj o) {
  if (FIXP(o)) return (double)UNFIX(o);
  if (FLONUMP(o)) return flonum_value(o);
  type_error("NUMBER", o);
}
static Obj mk_int(SObj v) {
  if (!FIX_FITS(v)) lisp_error("Integer overflow: bignums are not implemented yet");
  return FIX(v);
}
Obj num_add(Obj a, Obj b) {
  if (FIXP(a) && FIXP(b)) { SObj r; if (__builtin_add_overflow(UNFIX(a), UNFIX(b), &r)) lisp_error("Integer overflow in +"); return mk_int(r); }
  return make_flonum(num_to_double(a) + num_to_double(b));
}
Obj num_sub(Obj a, Obj b) {
  if (FIXP(a) && FIXP(b)) { SObj r; if (__builtin_sub_overflow(UNFIX(a), UNFIX(b), &r)) lisp_error("Integer overflow in -"); return mk_int(r); }
  return make_flonum(num_to_double(a) - num_to_double(b));
}
Obj num_mul(Obj a, Obj b) {
  if (FIXP(a) && FIXP(b)) { SObj r; if (__builtin_mul_overflow(UNFIX(a), UNFIX(b), &r)) lisp_error("Integer overflow in *"); return mk_int(r); }
  return make_flonum(num_to_double(a) * num_to_double(b));
}
static Obj num_div(Obj a, Obj b) {
  if (FIXP(a) && FIXP(b)) {
    SObj x = UNFIX(a), y = UNFIX(b);
    if (y == 0) lisp_error("Division by zero");
    if (x % y == 0) return mk_int(x / y);
    return make_flonum((double)x / (double)y);
  }
  double d = num_to_double(b);
  if (d == 0.0) lisp_error("Division by zero");
  return make_flonum(num_to_double(a) / d);
}
int num_cmp(Obj a, Obj b) {
  if (FIXP(a) && FIXP(b)) return UNFIX(a) < UNFIX(b) ? -1 : UNFIX(a) > UNFIX(b);
  double x = num_to_double(a), y = num_to_double(b);
  return x < y ? -1 : x > y;
}

PRIM(p_add) { Obj r = FIX(0); for (int i = 0; i < argc; i++) { if (!is_number(argv[i])) type_error("NUMBER", argv[i]); r = num_add(r, argv[i]); } return r; }
PRIM(p_mul) { Obj r = FIX(1); for (int i = 0; i < argc; i++) { if (!is_number(argv[i])) type_error("NUMBER", argv[i]); r = num_mul(r, argv[i]); } return r; }
PRIM(p_sub) {
  if (!is_number(argv[0])) type_error("NUMBER", argv[0]);
  if (argc == 1) return num_sub(FIX(0), argv[0]);
  Obj r = argv[0];
  for (int i = 1; i < argc; i++) r = num_sub(r, argv[i]);
  return r;
}
PRIM(p_div) {
  if (argc == 1) return num_div(FIX(1), argv[0]);
  Obj r = argv[0];
  for (int i = 1; i < argc; i++) r = num_div(r, argv[i]);
  return r;
}
#define CMP_PRIM(name, OP) PRIM(name) { \
  for (int i = 0; i < argc; i++) if (!is_number(argv[i])) type_error("NUMBER", argv[i]); \
  for (int i = 0; i + 1 < argc; i++) if (!(num_cmp(argv[i], argv[i + 1]) OP 0)) return NIL; \
  return TT; }
CMP_PRIM(p_numeq, ==)
CMP_PRIM(p_lt, <)
CMP_PRIM(p_gt, >)
CMP_PRIM(p_le, <=)
CMP_PRIM(p_ge, >=)
PRIM(p_numne) { UNUSED_ARGS; return BOOL(num_cmp(argv[0], argv[1]) != 0); }
PRIM(p_inc) { UNUSED_ARGS; return num_add(argv[0], FIX(1)); }
PRIM(p_dec) { UNUSED_ARGS; return num_sub(argv[0], FIX(1)); }
PRIM(p_abs) { UNUSED_ARGS; if (FIXP(argv[0])) return mk_int(llabs(UNFIX(argv[0]))); return make_flonum(fabs(num_to_double(argv[0]))); }
PRIM(p_min) { Obj r = argv[0]; for (int i = 1; i < argc; i++) if (num_cmp(argv[i], r) < 0) r = argv[i]; if (!is_number(r)) type_error("NUMBER", r); return r; }
PRIM(p_max) { Obj r = argv[0]; for (int i = 1; i < argc; i++) if (num_cmp(argv[i], r) > 0) r = argv[i]; if (!is_number(r)) type_error("NUMBER", r); return r; }
PRIM(p_zerop) { UNUSED_ARGS; return BOOL(num_cmp(argv[0], FIX(0)) == 0); }
PRIM(p_plusp) { UNUSED_ARGS; return BOOL(num_cmp(argv[0], FIX(0)) > 0); }
PRIM(p_minusp) { UNUSED_ARGS; return BOOL(num_cmp(argv[0], FIX(0)) < 0); }
PRIM(p_evenp) { UNUSED_ARGS; return BOOL((need_fix(argv[0]) & 1) == 0); }
PRIM(p_oddp) { UNUSED_ARGS; return BOOL((need_fix(argv[0]) & 1) != 0); }
PRIM(p_numberp) { UNUSED_ARGS; return BOOL(is_number(argv[0])); }
PRIM(p_integerp) { UNUSED_ARGS; return BOOL(FIXP(argv[0])); }
PRIM(p_floatp) { UNUSED_ARGS; return BOOL(FLONUMP(argv[0])); }

static SObj floor_div(SObj a, SObj b) { SObj q = a / b; if ((a % b != 0) && ((a < 0) != (b < 0))) q--; return q; }

static Obj divide_op(int argc, Obj *argv, int mode) {  /* 0 floor 1 ceiling 2 truncate 3 round */
  Obj x = argv[0], y = argc > 1 ? argv[1] : FIX(1);
  if (!is_number(x)) type_error("NUMBER", x);
  if (!is_number(y)) type_error("NUMBER", y);
  Obj q, r;
  if (FIXP(x) && FIXP(y)) {
    SObj a = UNFIX(x), b = UNFIX(y);
    if (b == 0) lisp_error("Division by zero");
    SObj qq;
    switch (mode) {
    case 0: qq = floor_div(a, b); break;
    case 1: qq = -floor_div(-a, b); break;
    case 2: qq = a / b; break;
    default: {
      SObj f = floor_div(a, b), rem = a - f * b;
      SObj twice = 2 * (rem < 0 ? -rem : rem), ab = b < 0 ? -b : b;
      if (twice > ab || (twice == ab && (f & 1))) f++;
      qq = f;
    }
    }
    q = mk_int(qq); r = mk_int(a - qq * b);
  } else {
    double a = num_to_double(x), b = num_to_double(y);
    if (b == 0.0) lisp_error("Division by zero");
    double d = a / b, qd;
    switch (mode) { case 0: qd = floor(d); break; case 1: qd = ceil(d); break; case 2: qd = trunc(d); break; default: qd = nearbyint(d); }
    if (fabs(qd) > (double)FIX_MAX) lisp_error("Integer overflow in rounding");
    q = FIX((SObj)qd); r = make_flonum(a - qd * b);
  }
  mv_vals[0] = q; mv_vals[1] = r; mv_count = 2;
  return q;
}
PRIM(p_floor) { return divide_op(argc, argv, 0); }
PRIM(p_ceiling) { return divide_op(argc, argv, 1); }
PRIM(p_truncate) { return divide_op(argc, argv, 2); }
PRIM(p_round) { return divide_op(argc, argv, 3); }
PRIM(p_mod) { divide_op(argc, argv, 0); mv_count = 1; return mv_vals[1]; }
PRIM(p_rem) { divide_op(argc, argv, 2); mv_count = 1; return mv_vals[1]; }
PRIM(p_gcd) {
  SObj g = 0;
  for (int i = 0; i < argc; i++) { SObj a = llabs(need_fix(argv[i])), b = g; while (b) { SObj t = a % b; a = b; b = t; } g = a; }
  return FIX(g);
}
PRIM(p_expt) {
  UNUSED_ARGS;
  Obj b = argv[0], e = argv[1];
  if (FIXP(b) && FIXP(e) && UNFIX(e) >= 0) {
    Obj r = FIX(1); SObj n = UNFIX(e); Obj base = b;
    while (n) { if (n & 1) r = num_mul(r, base); n >>= 1; if (n) base = num_mul(base, base); }
    return r;
  }
  return make_flonum(pow(num_to_double(b), num_to_double(e)));
}
PRIM(p_sqrt) { UNUSED_ARGS; double d = num_to_double(argv[0]); if (d < 0) lisp_error("SQRT of a negative number (complex numbers are not implemented)"); return make_flonum(sqrt(d)); }
PRIM(p_isqrt) { UNUSED_ARGS; SObj n = need_fix(argv[0]); if (n < 0) lisp_error("ISQRT of negative"); SObj r = (SObj)sqrt((double)n); while (r * r > n) r--; while ((r + 1) * (r + 1) <= n) r++; return FIX(r); }
PRIM(p_exp) { UNUSED_ARGS; return make_flonum(exp(num_to_double(argv[0]))); }
PRIM(p_log) {
  double x = num_to_double(argv[0]);
  if (x <= 0) lisp_error("LOG of a non-positive number");
  if (argc > 1) return make_flonum(log(x) / log(num_to_double(argv[1])));
  return make_flonum(log(x));
}
PRIM(p_sin) { UNUSED_ARGS; return make_flonum(sin(num_to_double(argv[0]))); }
PRIM(p_cos) { UNUSED_ARGS; return make_flonum(cos(num_to_double(argv[0]))); }
PRIM(p_tan) { UNUSED_ARGS; return make_flonum(tan(num_to_double(argv[0]))); }
PRIM(p_asin) { UNUSED_ARGS; return make_flonum(asin(num_to_double(argv[0]))); }
PRIM(p_acos) { UNUSED_ARGS; return make_flonum(acos(num_to_double(argv[0]))); }
PRIM(p_atan) { if (argc > 1) return make_flonum(atan2(num_to_double(argv[0]), num_to_double(argv[1]))); return make_flonum(atan(num_to_double(argv[0]))); }
PRIM(p_float) { UNUSED_ARGS; return make_flonum(num_to_double(argv[0])); }
PRIM(p_ash) {
  UNUSED_ARGS;
  SObj n = need_fix(argv[0]), s = need_fix(argv[1]);
  if (s >= 0) {
    if (s > 62 || (n != 0 && (n > (FIX_MAX >> s) || n < (FIX_MIN >> s)))) { if (n == 0) return FIX(0); lisp_error("Integer overflow in ASH"); }
    return mk_int(n << s);
  }
  return FIX(s < -63 ? (n < 0 ? -1 : 0) : n >> -s);
}
PRIM(p_logand) { SObj r = -1; for (int i = 0; i < argc; i++) r &= need_fix(argv[i]); return FIX(r); }
PRIM(p_logior) { SObj r = 0; for (int i = 0; i < argc; i++) r |= need_fix(argv[i]); return FIX(r); }
PRIM(p_logxor) { SObj r = 0; for (int i = 0; i < argc; i++) r ^= need_fix(argv[i]); return FIX(r); }
PRIM(p_lognot) { UNUSED_ARGS; return FIX(~need_fix(argv[0])); }
static uint64_t rng_state = 0x2545F4914F6CDD1DULL;
static uint64_t rng_next(void) { rng_state ^= rng_state << 13; rng_state ^= rng_state >> 7; rng_state ^= rng_state << 17; return rng_state; }
PRIM(p_random) {
  UNUSED_ARGS;
  if (FLONUMP(argv[0])) return make_flonum(flonum_value(argv[0]) * ((double)(rng_next() >> 11) / 9007199254740992.0));
  SObj n = need_fix(argv[0]);
  if (n <= 0) lisp_error("RANDOM: argument must be positive");
  return FIX((SObj)(rng_next() % (uint64_t)n));
}
PRIM(p_seed_random) { UNUSED_ARGS; rng_state = (uint64_t)need_fix(argv[0]) * 2654435761u + 88172645463325252ULL; if (!rng_state) rng_state = 1; return NIL; }

PRIM(p_number_to_string) {
  Obj n = argv[0];
  int radix = argc > 1 ? (int)need_fix(argv[1]) : 10;
  if (FLONUMP(n)) { Buf b; buf_init(&b); format_double(&b, flonum_value(n)); Obj s = make_string(b.buf, b.len); buf_free(&b); return s; }
  SObj v = need_fix(n);
  char tmp[80]; int i = 79; tmp[i] = 0;
  int neg = v < 0; uint64_t u = neg ? (uint64_t)(-v) : (uint64_t)v;
  if (u == 0) tmp[--i] = '0';
  while (u) { int d = (int)(u % (uint64_t)radix); tmp[--i] = (char)(d < 10 ? '0' + d : 'A' + d - 10); u /= (uint64_t)radix; }
  if (neg) tmp[--i] = '-';
  return make_cstring(tmp + i);
}
PRIM(p_parse_integer) {
  /* (%parse-integer string start end radix) -> integer or NIL */
  UNUSED_ARGS;
  need_str(argv[0]);
  SObj s = need_fix(argv[1]), e = argv[2] == NIL ? (SObj)STR_LEN(argv[0]) : need_fix(argv[2]);
  int radix = (int)need_fix(argv[3]);
  const char *p = STR_DATA(argv[0]);
  while (s < e && isspace((unsigned char)p[s])) s++;
  while (e > s && isspace((unsigned char)p[e - 1])) e--;
  int neg = 0;
  if (s < e && (p[s] == '+' || p[s] == '-')) { neg = p[s] == '-'; s++; }
  if (s >= e) return NIL;
  SObj v = 0;
  for (; s < e; s++) {
    int c = toupper((unsigned char)p[s]), d = isdigit(c) ? c - '0' : (c >= 'A' && c <= 'Z') ? c - 'A' + 10 : 99;
    if (d >= radix) return NIL;
    if (__builtin_mul_overflow(v, (SObj)radix, &v) || __builtin_add_overflow(v, (SObj)d, &v)) lisp_error("Integer overflow in PARSE-INTEGER");
  }
  return mk_int(neg ? -v : v);
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
  defprim("+", p_add, 0, -1); defprim("-", p_sub, 1, -1); defprim("*", p_mul, 0, -1); defprim("/", p_div, 1, -1);
  defprim("=", p_numeq, 1, -1); defprim("<", p_lt, 1, -1); defprim(">", p_gt, 1, -1);
  defprim("<=", p_le, 1, -1); defprim(">=", p_ge, 1, -1); defprim("/=", p_numne, 2, 2);
  defprim("1+", p_inc, 1, 1); defprim("1-", p_dec, 1, 1); defprim("ABS", p_abs, 1, 1);
  defprim("MIN", p_min, 1, -1); defprim("MAX", p_max, 1, -1);
  defprim("ZEROP", p_zerop, 1, 1); defprim("PLUSP", p_plusp, 1, 1); defprim("MINUSP", p_minusp, 1, 1);
  defprim("EVENP", p_evenp, 1, 1); defprim("ODDP", p_oddp, 1, 1);
  defprim("NUMBERP", p_numberp, 1, 1); defprim("INTEGERP", p_integerp, 1, 1); defprim("FLOATP", p_floatp, 1, 1);
  defprim("FIXNUMP", p_integerp, 1, 1); defprim("RATIONALP", p_integerp, 1, 1); defprim("REALP", p_numberp, 1, 1);
  defprim_mv("FLOOR", p_floor, 1, 2); defprim_mv("CEILING", p_ceiling, 1, 2);
  defprim_mv("TRUNCATE", p_truncate, 1, 2); defprim_mv("ROUND", p_round, 1, 2);
  defprim_mv("MOD", p_mod, 2, 2); defprim_mv("REM", p_rem, 2, 2);
  defprim("GCD", p_gcd, 0, -1); defprim("EXPT", p_expt, 2, 2); defprim("SQRT", p_sqrt, 1, 1); defprim("ISQRT", p_isqrt, 1, 1);
  defprim("EXP", p_exp, 1, 1); defprim("LOG", p_log, 1, 2);
  defprim("SIN", p_sin, 1, 1); defprim("COS", p_cos, 1, 1); defprim("TAN", p_tan, 1, 1);
  defprim("ASIN", p_asin, 1, 1); defprim("ACOS", p_acos, 1, 1); defprim("ATAN", p_atan, 1, 2);
  defprim("FLOAT", p_float, 1, 2);
  defprim("ASH", p_ash, 2, 2); defprim("LOGAND", p_logand, 0, -1); defprim("LOGIOR", p_logior, 0, -1);
  defprim("LOGXOR", p_logxor, 0, -1); defprim("LOGNOT", p_lognot, 1, 1);
  defprim("RANDOM", p_random, 1, 2); defprim("%SEED-RANDOM", p_seed_random, 1, 1);
  defprim("%NUMBER-TO-STRING", p_number_to_string, 1, 2); defprim("%PARSE-INTEGER", p_parse_integer, 4, 4);
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
