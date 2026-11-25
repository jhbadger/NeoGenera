/* prims_num.c -- numeric primitives over the full tower (fixnum, bignum, ratio, flonum). */
#include "neogenera.h"
#include <math.h>

static SObj need_fix(Obj o) { if (!FIXP(o)) type_error("FIXNUM", o); return UNFIX(o); }
static Obj need_int(Obj o) { if (!is_integer(o)) type_error("INTEGER", o); return o; }
static Obj need_num(Obj o) { if (!is_number(o)) type_error("NUMBER", o); return o; }
static int rationalp(Obj o) { return is_integer(o) || IS_TYPE(o, T_RATIO); }

PRIM(p_add) { Obj r = FIX(0); for (int i = 0; i < argc; i++) r = num_add(r, need_num(argv[i])); return r; }
PRIM(p_mul) { Obj r = FIX(1); for (int i = 0; i < argc; i++) r = num_mul(r, need_num(argv[i])); return r; }
PRIM(p_sub) {
  need_num(argv[0]);
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
  for (int i = 0; i < argc; i++) need_num(argv[i]); \
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
PRIM(p_abs) { UNUSED_ARGS; need_num(argv[0]); return num_sign(argv[0]) < 0 ? num_sub(FIX(0), argv[0]) : argv[0]; }
PRIM(p_min) { Obj r = need_num(argv[0]); for (int i = 1; i < argc; i++) if (num_cmp(need_num(argv[i]), r) < 0) r = argv[i]; return r; }
PRIM(p_max) { Obj r = need_num(argv[0]); for (int i = 1; i < argc; i++) if (num_cmp(need_num(argv[i]), r) > 0) r = argv[i]; return r; }
PRIM(p_zerop) { UNUSED_ARGS; need_num(argv[0]); return BOOL(num_sign(argv[0]) == 0); }
PRIM(p_plusp) { UNUSED_ARGS; need_num(argv[0]); return BOOL(num_sign(argv[0]) > 0); }
PRIM(p_minusp) { UNUSED_ARGS; need_num(argv[0]); return BOOL(num_sign(argv[0]) < 0); }
PRIM(p_evenp) { UNUSED_ARGS; return BOOL(int_even(need_int(argv[0]))); }
PRIM(p_oddp) { UNUSED_ARGS; return BOOL(!int_even(need_int(argv[0]))); }
PRIM(p_numberp) { UNUSED_ARGS; return BOOL(is_number(argv[0])); }
PRIM(p_integerp) { UNUSED_ARGS; return BOOL(is_integer(argv[0])); }
PRIM(p_fixnump) { UNUSED_ARGS; return BOOL(FIXP(argv[0])); }
PRIM(p_bignump) { UNUSED_ARGS; return BOOL(IS_TYPE(argv[0], T_BIGNUM)); }
PRIM(p_ratiop) { UNUSED_ARGS; return BOOL(IS_TYPE(argv[0], T_RATIO)); }
PRIM(p_rationalp) { UNUSED_ARGS; return BOOL(rationalp(argv[0])); }
PRIM(p_floatp) { UNUSED_ARGS; return BOOL(FLONUMP(argv[0])); }

/* floor / ceiling / truncate / round -> (values quotient remainder) */
static Obj divide_op(int argc, Obj *argv, int mode) {
  Obj x = need_num(argv[0]), y = argc > 1 ? need_num(argv[1]) : FIX(1);
  Obj q, r;
  if (is_integer(x) && is_integer(y)) int_divmod(x, y, mode, &q, &r);
  else if (rationalp(x) && rationalp(y)) {
    if (num_sign(y) == 0) lisp_error("Division by zero");
    Obj t = num_div(x, y), rem;
    int_divmod(ratio_numerator(t), ratio_denominator(t), mode, &q, &rem);
    r = num_sub(x, num_mul(q, y));
  } else {
    double a = num_to_double(x), b = num_to_double(y);
    if (b == 0.0) lisp_error("Division by zero");
    double d = a / b, qd;
    switch (mode) { case 0: qd = floor(d); break; case 1: qd = ceil(d); break; case 2: qd = trunc(d); break; default: qd = nearbyint(d); }
    q = int_from_double(qd); r = make_flonum(a - qd * b);
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
PRIM(p_gcd) { Obj g = FIX(0); for (int i = 0; i < argc; i++) g = int_gcd(g, need_int(argv[i])); return g; }
PRIM(p_expt) {
  UNUSED_ARGS;
  Obj b = need_num(argv[0]), e = need_num(argv[1]);
  if (is_integer(e) && rationalp(b)) {
    if (!FIXP(e)) { if (b == FIX(1) || b == FIX(0)) return b; lisp_error("EXPT: exponent too large"); }
    int64_t n = UNFIX(e), an = n < 0 ? -n : n;
    Obj num = int_pow(ratio_numerator(b), an), den = int_pow(ratio_denominator(b), an);
    return n >= 0 ? make_ratio(num, den) : make_ratio(den, num);
  }
  if (is_integer(e) && FLONUMP(b)) return make_flonum(pow(flonum_value(b), (double)UNFIX(e)));
  double bd = num_to_double(b);
  if (bd < 0 && !is_integer(e)) lisp_error("EXPT of a negative base to a non-integer power (complex numbers are not implemented)");
  return make_flonum(pow(bd, num_to_double(e)));
}
PRIM(p_sqrt) { UNUSED_ARGS; double d = num_to_double(argv[0]); if (d < 0) lisp_error("SQRT of a negative number (complex numbers are not implemented)"); return make_flonum(sqrt(d)); }
PRIM(p_isqrt) { UNUSED_ARGS; return int_isqrt(need_int(argv[0])); }
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
PRIM(p_rational) { UNUSED_ARGS; Obj x = need_num(argv[0]); return FLONUMP(x) ? float_to_rational(flonum_value(x)) : x; }
PRIM(p_numerator) { UNUSED_ARGS; if (!rationalp(argv[0])) type_error("RATIONAL", argv[0]); return ratio_numerator(argv[0]); }
PRIM(p_denominator) { UNUSED_ARGS; if (!rationalp(argv[0])) type_error("RATIONAL", argv[0]); return ratio_denominator(argv[0]); }
PRIM(p_ash) { UNUSED_ARGS; Obj n = need_int(argv[0]); if (!FIXP(argv[1])) { if (int_sign(n) == 0) return n; if (int_sign(argv[1]) < 0) return int_sign(n) < 0 ? FIX(-1) : FIX(0); lisp_error("ASH: shift count too large"); } return int_ash(n, UNFIX(argv[1])); }
PRIM(p_logand) { Obj r = FIX(-1); for (int i = 0; i < argc; i++) r = int_logop(0, r, need_int(argv[i])); return r; }
PRIM(p_logior) { Obj r = FIX(0); for (int i = 0; i < argc; i++) r = int_logop(1, r, need_int(argv[i])); return r; }
PRIM(p_logxor) { Obj r = FIX(0); for (int i = 0; i < argc; i++) r = int_logop(2, r, need_int(argv[i])); return r; }
PRIM(p_lognot) { UNUSED_ARGS; return int_sub(int_neg(need_int(argv[0])), FIX(1)); }
PRIM(p_integer_length) { UNUSED_ARGS; return FIX(int_length(need_int(argv[0]))); }

static uint64_t rng_state = 0x2545F4914F6CDD1DULL;
static uint64_t rng_next(void) { rng_state ^= rng_state << 13; rng_state ^= rng_state >> 7; rng_state ^= rng_state << 17; return rng_state; }
PRIM(p_random) {
  UNUSED_ARGS;
  if (FLONUMP(argv[0])) return make_flonum(flonum_value(argv[0]) * ((double)(rng_next() >> 11) / 9007199254740992.0));
  Obj n = need_int(argv[0]);
  if (int_sign(n) <= 0) lisp_error("RANDOM: argument must be positive");
  if (FIXP(n)) return FIX((SObj)(rng_next() % (uint64_t)UNFIX(n)));
  Obj acc = FIX(0);                        /* bignum limit: build enough random bits, reduce */
  for (int64_t bits = 0; bits < int_length(n) + 64; bits += 32) acc = int_add(int_ash(acc, 32), FIX((SObj)(rng_next() & 0xFFFFFFFFULL)));
  Obj q, r; int_divmod(acc, n, 0, &q, &r);
  return r;
}
PRIM(p_seed_random) { UNUSED_ARGS; rng_state = (uint64_t)need_fix(argv[0]) * 2654435761u + 88172645463325252ULL; if (!rng_state) rng_state = 1; return NIL; }

PRIM(p_number_to_string) {
  Obj n = need_num(argv[0]);
  int radix = argc > 1 ? (int)need_fix(argv[1]) : 10;
  Buf b; buf_init(&b);
  if (FLONUMP(n)) format_double(&b, flonum_value(n)); else number_print(&b, n, radix);
  Obj s = make_string(b.buf ? b.buf : "", b.len);
  buf_free(&b);
  return s;
}
PRIM(p_parse_integer) {
  /* (%parse-integer string start end radix) -> integer or NIL */
  UNUSED_ARGS;
  if (!STRINGP(argv[0])) type_error("STRING", argv[0]);
  SObj s = need_fix(argv[1]), e = argv[2] == NIL ? (SObj)STR_LEN(argv[0]) : need_fix(argv[2]);
  int radix = (int)need_fix(argv[3]);
  const char *p = STR_DATA(argv[0]);
  while (s < e && (p[s] == ' ' || p[s] == '\t' || p[s] == '\n')) s++;
  while (e > s && (p[e - 1] == ' ' || p[e - 1] == '\t' || p[e - 1] == '\n')) e--;
  Obj r = int_parse(p + s, (size_t)(e - s), radix);
  return r == UNBOUND ? NIL : r;
}

void init_prims_num(void) {
  defprim("+", p_add, 0, -1); defprim("-", p_sub, 1, -1); defprim("*", p_mul, 0, -1); defprim("/", p_div, 1, -1);
  defprim("=", p_numeq, 1, -1); defprim("<", p_lt, 1, -1); defprim(">", p_gt, 1, -1);
  defprim("<=", p_le, 1, -1); defprim(">=", p_ge, 1, -1); defprim("/=", p_numne, 2, 2);
  defprim("1+", p_inc, 1, 1); defprim("1-", p_dec, 1, 1); defprim("ABS", p_abs, 1, 1);
  defprim("MIN", p_min, 1, -1); defprim("MAX", p_max, 1, -1);
  defprim("ZEROP", p_zerop, 1, 1); defprim("PLUSP", p_plusp, 1, 1); defprim("MINUSP", p_minusp, 1, 1);
  defprim("EVENP", p_evenp, 1, 1); defprim("ODDP", p_oddp, 1, 1);
  defprim("NUMBERP", p_numberp, 1, 1); defprim("INTEGERP", p_integerp, 1, 1); defprim("FLOATP", p_floatp, 1, 1);
  defprim("FIXNUMP", p_fixnump, 1, 1); defprim("BIGNUMP", p_bignump, 1, 1); defprim("RATIOP", p_ratiop, 1, 1);
  defprim("RATIONALP", p_rationalp, 1, 1); defprim("REALP", p_numberp, 1, 1);
  defprim_mv("FLOOR", p_floor, 1, 2); defprim_mv("CEILING", p_ceiling, 1, 2);
  defprim_mv("TRUNCATE", p_truncate, 1, 2); defprim_mv("ROUND", p_round, 1, 2);
  defprim_mv("MOD", p_mod, 2, 2); defprim_mv("REM", p_rem, 2, 2);
  defprim("GCD", p_gcd, 0, -1); defprim("EXPT", p_expt, 2, 2); defprim("SQRT", p_sqrt, 1, 1); defprim("ISQRT", p_isqrt, 1, 1);
  defprim("EXP", p_exp, 1, 1); defprim("LOG", p_log, 1, 2);
  defprim("SIN", p_sin, 1, 1); defprim("COS", p_cos, 1, 1); defprim("TAN", p_tan, 1, 1);
  defprim("ASIN", p_asin, 1, 1); defprim("ACOS", p_acos, 1, 1); defprim("ATAN", p_atan, 1, 2);
  defprim("FLOAT", p_float, 1, 2); defprim("RATIONAL", p_rational, 1, 1);
  defprim("NUMERATOR", p_numerator, 1, 1); defprim("DENOMINATOR", p_denominator, 1, 1);
  defprim("ASH", p_ash, 2, 2); defprim("LOGAND", p_logand, 0, -1); defprim("LOGIOR", p_logior, 0, -1);
  defprim("LOGXOR", p_logxor, 0, -1); defprim("LOGNOT", p_lognot, 1, 1); defprim("INTEGER-LENGTH", p_integer_length, 1, 1);
  defprim("RANDOM", p_random, 1, 2); defprim("%SEED-RANDOM", p_seed_random, 1, 1);
  defprim("%NUMBER-TO-STRING", p_number_to_string, 1, 2); defprim("%PARSE-INTEGER", p_parse_integer, 4, 4);
}
