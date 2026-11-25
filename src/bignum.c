/* bignum.c -- arbitrary-precision integers, ratios and the generic numeric tower.
 *
 * A bignum is a raw heap object: payload[0] = (nlimbs << 1) | negative, then
 * 32-bit limbs (little endian), two per word.  Bignums are always normalized:
 * a value that fits a fixnum is a fixnum, and there are no leading zero limbs.
 * Allocation never collects (see docs/design/gc.md), so primitives may hold
 * pointers into operand objects while they allocate results.
 */
#include "neogenera.h"
#include <math.h>
#include <ctype.h>

typedef uint32_t limb;
typedef struct { int neg; size_t n; const limb *d; limb tmp[2]; } BV;

static void *xmalloc(size_t n) {
  void *p = malloc(n ? n : 1);
  if (!p) { fprintf(stderr, "out of memory\n"); exit(2); }
  return p;
}

#define BIGP(o) IS_TYPE(o, T_BIGNUM)
int is_integer(Obj o) { return FIXP(o) || BIGP(o); }

static void bv_of(Obj o, BV *v) {
  if (FIXP(o)) {
    SObj x = UNFIX(o);
    uint64_t m = x < 0 ? (uint64_t)0 - (uint64_t)x : (uint64_t)x;
    v->neg = x < 0;
    v->tmp[0] = (limb)m; v->tmp[1] = (limb)(m >> 32);
    v->n = v->tmp[1] ? 2 : (v->tmp[0] ? 1 : 0);
    v->d = v->tmp;
  } else {
    uint64_t h = (uint64_t)SLOT(o, 0);
    v->neg = (int)(h & 1); v->n = (size_t)(h >> 1); v->d = (const limb *)&SLOT(o, 1);
  }
}

static Obj big_make(int neg, const limb *d, size_t n) {
  while (n && d[n - 1] == 0) n--;
  if (n == 0) return FIX(0);
  if (n <= 2) {
    uint64_t m = d[0] | (n == 2 ? (uint64_t)d[1] << 32 : 0);
    if (!neg && m <= (uint64_t)FIX_MAX) return FIX((SObj)m);
    if (neg && m <= (uint64_t)FIX_MAX + 1) return FIX(-(SObj)m);
  }
  Obj o = make_raw(T_BIGNUM, 1 + (n + 1) / 2);
  SLOT(o, 0) = ((Obj)n << 1) | (neg ? 1 : 0);
  memcpy(&SLOT(o, 1), d, n * sizeof(limb));
  return o;
}

Obj int_from_i64(int64_t v) {
  if (FIX_FITS(v)) return FIX(v);
  uint64_t m = v < 0 ? (uint64_t)0 - (uint64_t)v : (uint64_t)v;
  limb d[2] = { (limb)m, (limb)(m >> 32) };
  return big_make(v < 0, d, 2);
}

/* ---- magnitude arithmetic --------------------------------------------- */
static int mag_cmp(const limb *a, size_t na, const limb *b, size_t nb) {
  if (na != nb) return na < nb ? -1 : 1;
  for (size_t i = na; i-- > 0;) if (a[i] != b[i]) return a[i] < b[i] ? -1 : 1;
  return 0;
}
static size_t mag_add(const limb *a, size_t na, const limb *b, size_t nb, limb *out) {
  if (na < nb) { const limb *t = a; a = b; b = t; size_t tn = na; na = nb; nb = tn; }
  uint64_t c = 0; size_t i;
  for (i = 0; i < na; i++) { c += (uint64_t)a[i] + (i < nb ? b[i] : 0); out[i] = (limb)c; c >>= 32; }
  if (c) out[i++] = (limb)c;
  return i;
}
static size_t mag_sub(const limb *a, size_t na, const limb *b, size_t nb, limb *out) { /* a >= b */
  int64_t br = 0;
  for (size_t i = 0; i < na; i++) {
    int64_t t = (int64_t)a[i] - (i < nb ? (int64_t)b[i] : 0) - br;
    if (t < 0) { t += (int64_t)1 << 32; br = 1; } else br = 0;
    out[i] = (limb)t;
  }
  while (na && out[na - 1] == 0) na--;
  return na;
}
/* Multiplication: schoolbook below KARA_CUT limbs, Karatsuba above, chunking for unbalanced operands.
   KMUL writes exactly na+nb limbs to OUT (leading zeros included). */
#define KARA_CUT 32
static void school_mul(const limb *a, size_t na, const limb *b, size_t nb, limb *out) {
  memset(out, 0, (na + nb) * sizeof(limb));
  for (size_t i = 0; i < na; i++) {
    uint64_t c = 0;
    for (size_t j = 0; j < nb; j++) {
      uint64_t t = (uint64_t)a[i] * b[j] + out[i + j] + c;
      out[i + j] = (limb)t; c = t >> 32;
    }
    out[i + nb] = (limb)c;
  }
}
static void add_in(limb *x, size_t nx, const limb *y, size_t ny) {   /* x += y, carry stays inside x */
  uint64_t c = 0; size_t i = 0;
  for (; i < ny && i < nx; i++) { uint64_t t = (uint64_t)x[i] + y[i] + c; x[i] = (limb)t; c = t >> 32; }
  for (; c && i < nx; i++) { uint64_t t = (uint64_t)x[i] + c; x[i] = (limb)t; c = t >> 32; }
}
static void sub_in(limb *x, size_t nx, const limb *y, size_t ny) {   /* x -= y, x >= y */
  int64_t b = 0; size_t i = 0;
  for (; i < nx; i++) {
    int64_t t = (int64_t)x[i] - (i < ny ? (int64_t)y[i] : 0) - b;
    if (t < 0) { t += ((int64_t)1 << 32); b = 1; } else b = 0;
    x[i] = (limb)t;
    if (i >= ny && !b) break;
  }
}
static void kmul(const limb *a, size_t na, const limb *b, size_t nb, limb *out) {
  if (na < nb) { const limb *t = a; a = b; b = t; size_t tn = na; na = nb; nb = tn; }
  if (nb < KARA_CUT) { school_mul(a, na, b, nb, out); return; }
  size_t h = (na + 1) / 2;
  if (nb <= h) {                                   /* unbalanced: multiply NB-sized chunks of A */
    memset(out, 0, (na + nb) * sizeof(limb));
    limb *tmp = xmalloc(2 * nb * sizeof(limb));
    for (size_t off = 0; off < na; off += nb) {
      size_t len = na - off < nb ? na - off : nb;
      kmul(a + off, len, b, nb, tmp);
      add_in(out + off, na + nb - off, tmp, len + nb);
    }
    free(tmp); return;
  }
  size_t a1n = na - h, b1n = nb - h;
  limb *buf = xmalloc((2 * (h + 1) + 2 * h + 2) * sizeof(limb));
  limb *sa = buf, *sb = buf + h + 1, *z1 = buf + 2 * (h + 1);
  memset(sa, 0, (h + 1) * sizeof(limb)); memcpy(sa, a, h * sizeof(limb)); add_in(sa, h + 1, a + h, a1n);
  memset(sb, 0, (h + 1) * sizeof(limb)); memcpy(sb, b, h * sizeof(limb)); add_in(sb, h + 1, b + h, b1n);
  kmul(sa, h + 1, sb, h + 1, z1);                  /* (a0+a1)(b0+b1): 2h+2 limbs */
  kmul(a, h, b, h, out);                           /* z0 -> out[0..2h) */
  kmul(a + h, a1n, b + h, b1n, out + 2 * h);       /* z2 -> out[2h..na+nb) */
  sub_in(z1, 2 * h + 2, out, 2 * h);
  sub_in(z1, 2 * h + 2, out + 2 * h, a1n + b1n);
  add_in(out + h, na + nb - h, z1, 2 * h + 2 > na + nb - h ? na + nb - h : 2 * h + 2);
  free(buf);
}
static size_t mag_mul(const limb *a, size_t na, const limb *b, size_t nb, limb *out) {
  kmul(a, na, b, nb, out);
  size_t n = na + nb;
  while (n && out[n - 1] == 0) n--;
  return n;
}

/* Knuth algorithm D (after Hacker's Delight).  q has m-n+1 limbs, r has n. */
static void mag_divmnu(limb *q, limb *r, const limb *u, size_t m, const limb *v, size_t n) {
  const uint64_t b = 1ULL << 32;
  if (n == 1) {
    uint64_t k = 0;
    for (size_t j = m; j-- > 0;) { uint64_t cur = k * b + u[j]; q[j] = (limb)(cur / v[0]); k = cur - (uint64_t)q[j] * v[0]; }
    r[0] = (limb)k;
    return;
  }
  int s = __builtin_clz(v[n - 1]);
  limb *vn = xmalloc(n * 4), *un = xmalloc((m + 1) * 4);
  for (size_t i = n - 1; i > 0; i--) vn[i] = (limb)(((uint64_t)v[i] << s) | ((uint64_t)v[i - 1] >> (32 - s)));
  vn[0] = (limb)((uint64_t)v[0] << s);
  un[m] = (limb)((uint64_t)u[m - 1] >> (32 - s));
  for (size_t i = m - 1; i > 0; i--) un[i] = (limb)(((uint64_t)u[i] << s) | ((uint64_t)u[i - 1] >> (32 - s)));
  un[0] = (limb)((uint64_t)u[0] << s);
  for (size_t jj = m - n + 1; jj-- > 0;) {
    size_t j = jj;
    uint64_t num = (uint64_t)un[j + n] * b + un[j + n - 1];
    uint64_t qhat = num / vn[n - 1], rhat = num - qhat * vn[n - 1];
    while (qhat >= b || qhat * vn[n - 2] > b * rhat + un[j + n - 2]) {
      qhat--; rhat += vn[n - 1];
      if (rhat >= b) break;
    }
    int64_t k = 0, t;
    for (size_t i = 0; i < n; i++) {
      uint64_t p = qhat * vn[i];
      t = (int64_t)un[i + j] - k - (int64_t)(p & 0xFFFFFFFFULL);
      un[i + j] = (limb)t;
      k = (int64_t)(p >> 32) - (t >> 32);
    }
    t = (int64_t)un[j + n] - k;
    un[j + n] = (limb)t;
    q[j] = (limb)qhat;
    if (t < 0) {
      q[j]--;
      uint64_t c = 0;
      for (size_t i = 0; i < n; i++) { c += (uint64_t)un[i + j] + vn[i]; un[i + j] = (limb)c; c >>= 32; }
      un[j + n] += (limb)c;
    }
  }
  for (size_t i = 0; i < n; i++) r[i] = (limb)(((uint64_t)un[i] >> s) | ((uint64_t)un[i + 1] << (32 - s)));
  free(vn); free(un);
}

/* ---- integer operations on Obj ----------------------------------------- */
int int_sign(Obj a) {
  if (FIXP(a)) { SObj x = UNFIX(a); return (x > 0) - (x < 0); }
  return (SLOT(a, 0) & 1) ? -1 : 1;
}
Obj int_neg(Obj a) {
  if (FIXP(a)) return int_from_i64(-UNFIX(a));
  BV x; bv_of(a, &x);
  return big_make(!x.neg, x.d, x.n);
}
Obj int_abs(Obj a) { return int_sign(a) < 0 ? int_neg(a) : a; }
int int_even(Obj a) { if (FIXP(a)) return (UNFIX(a) & 1) == 0; return (((const limb *)&SLOT(a, 1))[0] & 1) == 0; }

int int_cmp(Obj a, Obj b) {
  if (FIXP(a) && FIXP(b)) return UNFIX(a) < UNFIX(b) ? -1 : UNFIX(a) > UNFIX(b);
  BV x, y; bv_of(a, &x); bv_of(b, &y);
  if (x.neg != y.neg) return x.neg ? -1 : 1;
  int c = mag_cmp(x.d, x.n, y.d, y.n);
  return x.neg ? -c : c;
}

Obj int_add(Obj a, Obj b) {
  if (FIXP(a) && FIXP(b)) return int_from_i64(UNFIX(a) + UNFIX(b));
  BV x, y; bv_of(a, &x); bv_of(b, &y);
  size_t cap = (x.n > y.n ? x.n : y.n) + 1;
  limb *o = xmalloc(cap * 4); Obj r; size_t n;
  if (x.neg == y.neg) { n = mag_add(x.d, x.n, y.d, y.n, o); r = big_make(x.neg, o, n); }
  else {
    int c = mag_cmp(x.d, x.n, y.d, y.n);
    if (c == 0) r = FIX(0);
    else if (c > 0) { n = mag_sub(x.d, x.n, y.d, y.n, o); r = big_make(x.neg, o, n); }
    else { n = mag_sub(y.d, y.n, x.d, x.n, o); r = big_make(y.neg, o, n); }
  }
  free(o);
  return r;
}
Obj int_sub(Obj a, Obj b) {
  if (FIXP(a) && FIXP(b)) return int_from_i64(UNFIX(a) - UNFIX(b));
  return int_add(a, int_neg(b));
}
Obj int_mul(Obj a, Obj b) {
  if (FIXP(a) && FIXP(b)) {
    __int128 p = (__int128)UNFIX(a) * UNFIX(b);
    if (p >= INT64_MIN && p <= INT64_MAX) return int_from_i64((int64_t)p);
  }
  BV x, y; bv_of(a, &x); bv_of(b, &y);
  if (x.n == 0 || y.n == 0) return FIX(0);
  limb *o = xmalloc((x.n + y.n) * 4);
  size_t n = mag_mul(x.d, x.n, y.d, y.n, o);
  Obj r = big_make(x.neg != y.neg, o, n);
  free(o);
  return r;
}

/* mode: 0 floor, 1 ceiling, 2 truncate, 3 round (ties to even) */
void int_divmod(Obj a, Obj b, int mode, Obj *qo, Obj *ro) {
  if (int_sign(b) == 0) lisp_error("Division by zero");
  Obj q, r;
  int aneg = int_sign(a) < 0, bneg = int_sign(b) < 0;
  if (FIXP(a) && FIXP(b)) {
    int64_t x = UNFIX(a), y = UNFIX(b), qq = x / y, rr = x % y;
    if (mode == 0 && rr != 0 && ((rr < 0) != (y < 0))) { qq--; rr += y; }
    else if (mode == 1 && rr != 0 && ((rr < 0) == (y < 0))) { qq++; rr -= y; }
    else if (mode == 3) {
      int64_t ar = rr < 0 ? -rr : rr, ay = y < 0 ? -y : y;
      if (2 * ar > ay || (2 * ar == ay && (qq & 1))) { if (aneg == bneg) { qq++; rr -= y; } else { qq--; rr += y; } }
    }
    *qo = int_from_i64(qq); *ro = int_from_i64(rr);
    return;
  }
  BV x, y; bv_of(a, &x); bv_of(b, &y);
  if (mag_cmp(x.d, x.n, y.d, y.n) < 0) { q = FIX(0); r = a; }
  else {
    size_t nq = x.n - y.n + 1;
    limb *qd = xmalloc(nq * 4), *rd = xmalloc(y.n * 4);
    mag_divmnu(qd, rd, x.d, x.n, y.d, y.n);
    q = big_make(x.neg != y.neg, qd, nq);
    r = big_make(x.neg, rd, y.n);
    free(qd); free(rd);
  }
  int rz = int_sign(r) == 0;
  if (mode == 0 && !rz && aneg != bneg) { q = int_sub(q, FIX(1)); r = int_add(r, b); }
  else if (mode == 1 && !rz && aneg == bneg) { q = int_add(q, FIX(1)); r = int_sub(r, b); }
  else if (mode == 3 && !rz) {
    int c = int_cmp(int_abs(int_add(r, r)), int_abs(b));
    if (c > 0 || (c == 0 && !int_even(q))) {
      if (aneg == bneg) { q = int_add(q, FIX(1)); r = int_sub(r, b); } else { q = int_sub(q, FIX(1)); r = int_add(r, b); }
    }
  }
  *qo = q; *ro = r;
}

Obj int_gcd(Obj a, Obj b) {
  a = int_abs(a); b = int_abs(b);
  while (int_sign(b) != 0) { Obj q, r; int_divmod(a, b, 2, &q, &r); a = b; b = r; }
  return a;
}

Obj int_pow(Obj base, int64_t e) {
  Obj r = FIX(1);
  while (e > 0) { if (e & 1) r = int_mul(r, base); e >>= 1; if (e) base = int_mul(base, base); }
  return r;
}

double int_to_double(Obj a) {
  if (FIXP(a)) return (double)UNFIX(a);
  BV x; bv_of(a, &x);
  double d = 0;
  for (size_t i = x.n; i-- > 0;) d = d * 4294967296.0 + x.d[i];
  return x.neg ? -d : d;
}

Obj int_ash(Obj a, int64_t s) {
  if (int_sign(a) == 0) return FIX(0);
  if (s < 0) {
    if (FIXP(a)) return FIX(UNFIX(a) >> (-s > 63 ? 63 : -s));
    int neg = int_sign(a) < 0;
    Obj m = neg ? int_sub(int_neg(a), FIX(1)) : a;     /* nonnegative */
    BV x; bv_of(m, &x);
    size_t ls = (size_t)(-s) / 32, bs = (size_t)(-s) % 32;
    Obj r;
    if (ls >= x.n) r = FIX(0);
    else {
      size_t n = x.n - ls; limb *o = xmalloc(n * 4);
      for (size_t i = 0; i < n; i++) {
        uint64_t lo = x.d[i + ls], hi = (i + ls + 1 < x.n) ? x.d[i + ls + 1] : 0;
        o[i] = (limb)(((hi << 32) | lo) >> bs);
      }
      r = big_make(0, o, n); free(o);
    }
    return neg ? int_neg(int_add(r, FIX(1))) : r;
  }
  BV x; bv_of(a, &x);
  size_t ls = (size_t)s / 32, bs = (size_t)s % 32, n = x.n + ls + 1;
  if (n > (1u << 26)) lisp_error("ASH: result too large");
  limb *o = xmalloc(n * 4); memset(o, 0, n * 4);
  for (size_t i = 0; i < x.n; i++) {
    uint64_t v = (uint64_t)x.d[i] << bs;
    o[i + ls] |= (limb)v; o[i + ls + 1] |= (limb)(v >> 32);
  }
  Obj r = big_make(x.neg, o, n); free(o);
  return r;
}

Obj int_from_double(double d) {         /* truncates */
  if (isnan(d) || isinf(d)) lisp_error("Cannot convert %s to an integer", isnan(d) ? "NaN" : "infinity");
  d = trunc(d);
  if (fabs(d) < 4.0e18) return int_from_i64((int64_t)d);
  int e; double f = frexp(fabs(d), &e);
  uint64_t m = (uint64_t)ldexp(f, 53);
  Obj r = int_ash(int_from_i64((int64_t)m), e - 53);
  return d < 0 ? int_neg(r) : r;
}

int64_t int_length(Obj a) {              /* integer-length */
  if (int_sign(a) < 0) a = int_sub(int_neg(a), FIX(1));
  if (FIXP(a)) { uint64_t m = (uint64_t)UNFIX(a); return m ? 64 - __builtin_clzll(m) : 0; }
  BV x; bv_of(a, &x);
  return (int64_t)(x.n - 1) * 32 + (32 - __builtin_clz(x.d[x.n - 1]));
}

Obj int_isqrt(Obj n) {
  if (int_sign(n) < 0) lisp_error("ISQRT of a negative number");
  if (FIXP(n)) { SObj v = UNFIX(n), r = (SObj)sqrt((double)v); while (r * r > v) r--; while ((r + 1) * (r + 1) <= v) r++; return FIX(r); }
  Obj x = int_ash(FIX(1), (int_length(n) + 1) / 2);
  for (;;) {
    Obj q, r; int_divmod(n, x, 2, &q, &r);
    Obj y = int_ash(int_add(x, q), -1);
    if (int_cmp(y, x) >= 0) return x;
    x = y;
  }
}

/* two's-complement bitwise operations; op: 0 and, 1 or, 2 xor */
static void to_twos(const BV *v, limb *out, size_t L) {
  for (size_t i = 0; i < L; i++) out[i] = i < v->n ? v->d[i] : 0;
  if (v->neg) { uint64_t c = 1; for (size_t i = 0; i < L; i++) { c += (limb)~out[i]; out[i] = (limb)c; c >>= 32; } }
}
Obj int_logop(int op, Obj a, Obj b) {
  BV x, y; bv_of(a, &x); bv_of(b, &y);
  size_t L = (x.n > y.n ? x.n : y.n) + 1;
  limb *p = xmalloc(L * 4), *q = xmalloc(L * 4);
  to_twos(&x, p, L); to_twos(&y, q, L);
  for (size_t i = 0; i < L; i++) p[i] = op == 0 ? (p[i] & q[i]) : op == 1 ? (p[i] | q[i]) : (p[i] ^ q[i]);
  int neg = (p[L - 1] >> 31) & 1;
  if (neg) { uint64_t c = 1; for (size_t i = 0; i < L; i++) { c += (limb)~p[i]; p[i] = (limb)c; c >>= 32; } }
  Obj r = big_make(neg, p, L);
  free(p); free(q);
  return r;
}

uint64_t int_hash(Obj a) {
  if (FIXP(a)) return (uint64_t)UNFIX(a) * 0x9E3779B97F4A7C15ULL;
  BV x; bv_of(a, &x);
  uint64_t h = x.neg ? 77 : 33;
  for (size_t i = 0; i < x.n; i++) h = h * 1099511628211ULL + x.d[i];
  return h;
}
int int_eql(Obj a, Obj b) { return int_cmp(a, b) == 0; }

/* ---- printing and parsing --------------------------------------------- */
void int_print(Buf *b, Obj a, int radix) {
  if (radix < 2 || radix > 36) radix = 10;
  if (FIXP(a) && radix == 10) { char t[32]; snprintf(t, sizeof t, "%ld", (long)UNFIX(a)); buf_puts(b, t); return; }
  BV x; bv_of(a, &x);
  if (x.n == 0) { buf_putc(b, '0'); return; }
  uint32_t chunk = (uint32_t)radix; int digits = 1;
  while ((uint64_t)chunk * (uint64_t)radix <= 0xFFFFFFFFULL) { chunk *= (uint32_t)radix; digits++; }
  limb *w = xmalloc(x.n * 4); memcpy(w, x.d, x.n * 4);
  size_t n = x.n;
  char *out = xmalloc(x.n * 33 + 8); size_t len = 0;
  while (n) {
    uint64_t rem = 0;
    for (size_t i = n; i-- > 0;) { uint64_t cur = (rem << 32) | w[i]; w[i] = (limb)(cur / chunk); rem = cur % chunk; }
    while (n && w[n - 1] == 0) n--;
    for (int k = 0; k < digits && (n || rem); k++) { int dg = (int)(rem % (uint64_t)radix); rem /= (uint64_t)radix; out[len++] = (char)(dg < 10 ? '0' + dg : 'A' + dg - 10); }
  }
  if (x.neg) buf_putc(b, '-');
  while (len) buf_putc(b, out[--len]);
  free(w); free(out);
}

Obj int_parse(const char *s, size_t len, int radix) {      /* UNBOUND if invalid */
  size_t i = 0; int neg = 0;
  if (i < len && (s[i] == '+' || s[i] == '-')) { neg = s[i] == '-'; i++; }
  if (i >= len) return UNBOUND;
  size_t cap = (len * 6) / 32 + 3, n = 0;
  limb *d = xmalloc(cap * 4); memset(d, 0, cap * 4);
  for (; i < len; i++) {
    int c = toupper((unsigned char)s[i]), dg = isdigit(c) ? c - '0' : (c >= 'A' && c <= 'Z') ? c - 'A' + 10 : 99;
    if (dg >= radix) { free(d); return UNBOUND; }
    uint64_t carry = (uint64_t)dg;
    for (size_t k = 0; k < n; k++) { uint64_t t = (uint64_t)d[k] * (uint64_t)radix + carry; d[k] = (limb)t; carry = t >> 32; }
    if (carry) d[n++] = (limb)carry;
  }
  Obj r = big_make(neg, d, n);
  free(d);
  return r;
}

/* ---- ratios ------------------------------------------------------------ */
#define RATIOP(o) IS_TYPE(o, T_RATIO)
Obj make_ratio(Obj n, Obj d) {
  if (int_sign(d) == 0) lisp_error("Division by zero");
  if (int_sign(d) < 0) { n = int_neg(n); d = int_neg(d); }
  Obj g = int_gcd(n, d);
  if (!(FIXP(g) && UNFIX(g) == 1)) { Obj q, r; int_divmod(n, g, 2, &q, &r); n = q; int_divmod(d, g, 2, &q, &r); d = q; }
  if (FIXP(d) && UNFIX(d) == 1) return n;
  Obj o = make_obj(T_RATIO, 2);
  SLOT(o, 0) = n; SLOT(o, 1) = d;
  return o;
}
static Obj rnum(Obj o) { return RATIOP(o) ? SLOT(o, 0) : o; }
static Obj rden(Obj o) { return RATIOP(o) ? SLOT(o, 1) : FIX(1); }
Obj ratio_numerator(Obj o) { return rnum(o); }
Obj ratio_denominator(Obj o) { return rden(o); }

/* ---- generic numbers --------------------------------------------------- */
enum { K_NONE = -1, K_INT, K_RATIO, K_FLO };
static int kind(Obj o) {
  if (FIXP(o) || BIGP(o)) return K_INT;
  if (RATIOP(o)) return K_RATIO;
  if (FLONUMP(o)) return K_FLO;
  return K_NONE;
}
int is_number(Obj o) { return kind(o) != K_NONE; }
static int need(Obj o) { int k = kind(o); if (k == K_NONE) type_error("NUMBER", o); return k; }

double num_to_double(Obj o) {
  switch (need(o)) {
  case K_INT: return int_to_double(o);
  case K_FLO: return flonum_value(o);
  default: {
    double n = int_to_double(rnum(o)), d = int_to_double(rden(o));
    if (isinf(n) || isinf(d)) {            /* scale down huge ratios */
      Obj q, r; int_divmod(rnum(o), rden(o), 2, &q, &r);
      return int_to_double(q);
    }
    return n / d;
  }
  }
}

Obj num_add(Obj a, Obj b) {
  int ka = need(a), kb = need(b);
  if (ka == K_FLO || kb == K_FLO) return make_flonum(num_to_double(a) + num_to_double(b));
  if (ka == K_INT && kb == K_INT) return int_add(a, b);
  return make_ratio(int_add(int_mul(rnum(a), rden(b)), int_mul(rnum(b), rden(a))), int_mul(rden(a), rden(b)));
}
Obj num_sub(Obj a, Obj b) {
  int ka = need(a), kb = need(b);
  if (ka == K_FLO || kb == K_FLO) return make_flonum(num_to_double(a) - num_to_double(b));
  if (ka == K_INT && kb == K_INT) return int_sub(a, b);
  return make_ratio(int_sub(int_mul(rnum(a), rden(b)), int_mul(rnum(b), rden(a))), int_mul(rden(a), rden(b)));
}
Obj num_mul(Obj a, Obj b) {
  int ka = need(a), kb = need(b);
  if (ka == K_FLO || kb == K_FLO) return make_flonum(num_to_double(a) * num_to_double(b));
  if (ka == K_INT && kb == K_INT) return int_mul(a, b);
  return make_ratio(int_mul(rnum(a), rnum(b)), int_mul(rden(a), rden(b)));
}
Obj num_div(Obj a, Obj b) {
  int ka = need(a), kb = need(b);
  if (ka == K_FLO || kb == K_FLO) {
    double d = num_to_double(b);
    if (d == 0.0) lisp_error("Division by zero");
    return make_flonum(num_to_double(a) / d);
  }
  if (int_sign(rnum(b)) == 0) lisp_error("Division by zero");
  return make_ratio(int_mul(rnum(a), rden(b)), int_mul(rden(a), rnum(b)));
}
int num_cmp(Obj a, Obj b) {
  int ka = need(a), kb = need(b);
  if (ka == K_FLO || kb == K_FLO) { double x = num_to_double(a), y = num_to_double(b); return x < y ? -1 : x > y; }
  if (ka == K_INT && kb == K_INT) return int_cmp(a, b);
  return int_cmp(int_mul(rnum(a), rden(b)), int_mul(rnum(b), rden(a)));
}
int num_sign(Obj a) {
  switch (need(a)) { case K_INT: return int_sign(a); case K_RATIO: return int_sign(rnum(a)); default: { double d = flonum_value(a); return (d > 0) - (d < 0); } }
}
int num_eql(Obj a, Obj b) {
  if (BIGP(a) && BIGP(b)) return int_cmp(a, b) == 0;
  if (RATIOP(a) && RATIOP(b)) return int_cmp(rnum(a), rnum(b)) == 0 && int_cmp(rden(a), rden(b)) == 0;
  return 0;
}
uint64_t num_hash(Obj a) {
  if (BIGP(a)) return int_hash(a);
  return int_hash(rnum(a)) * 31 + int_hash(rden(a));
}

void number_print(Buf *b, Obj o, int radix) {
  if (RATIOP(o)) { int_print(b, rnum(o), radix); buf_putc(b, '/'); int_print(b, rden(o), radix); }
  else int_print(b, o, radix);
}

/* exact rational for a float */
Obj float_to_rational(double d) {
  if (isnan(d) || isinf(d)) lisp_error("Cannot convert a non-finite float to a rational");
  if (d == 0.0) return FIX(0);
  int e; double f = frexp(d, &e);
  Obj m = int_from_i64((int64_t)ldexp(f, 53));
  e -= 53;
  if (e >= 0) return int_ash(m, e);
  return make_ratio(m, int_ash(FIX(1), -e));
}
