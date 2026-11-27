/* printer.c -- the S-expression printer (C implementation). */
#include "neogenera.h"
#include <ctype.h>
#include <math.h>

void buf_init(Buf *b) { b->buf = NULL; b->len = b->cap = 0; }
void buf_free(Buf *b) { free(b->buf); b->buf = NULL; b->len = b->cap = 0; }
void buf_put(Buf *b, const char *s, size_t n) {
  if (b->len + n + 1 > b->cap) {
    size_t cap = b->cap ? b->cap * 2 : 64;
    while (cap < b->len + n + 1) cap *= 2;
    b->buf = realloc(b->buf, cap);
    if (!b->buf) { fprintf(stderr, "out of memory\n"); exit(2); }
    b->cap = cap;
  }
  memcpy(b->buf + b->len, s, n);
  b->len += n;
  b->buf[b->len] = 0;
}
void buf_puts(Buf *b, const char *s) { buf_put(b, s, strlen(s)); }
void buf_putc(Buf *b, int c) { char ch = (char)c; buf_put(b, &ch, 1); }

void format_double(Buf *b, double d) {
  if (isnan(d)) { buf_puts(b, "#<NaN>"); return; }
  if (isinf(d)) { buf_puts(b, d > 0 ? "#<+Infinity>" : "#<-Infinity>"); return; }
  char tmp[64];
  double ad = fabs(d);
  if (ad != 0.0 && (ad >= 1e7 || ad < 1e-3)) {
    /* exponent notation: shortest round-tripping digits, written d.ddde[-]x */
    for (int prec = 14; prec <= 16; prec++) {
      snprintf(tmp, sizeof tmp, "%.*e", prec, d);
      if (strtod(tmp, NULL) == d) break;
    }
    char *e = strchr(tmp, 'e');
    int ex = atoi(e + 1);
    char mant[64]; size_t ml = (size_t)(e - tmp);
    memcpy(mant, tmp, ml); mant[ml] = 0;
    /* strip trailing zeros of the mantissa but keep one digit after the point */
    char *dot = strchr(mant, '.');
    if (dot) { size_t n = strlen(mant); while (n > 0 && mant[n - 1] == '0') n--; if (mant[n - 1] == '.') n++; mant[n] = 0; if (mant[n - 1] == '.') strcat(mant, "0"); }
    else strcat(mant, ".0");
    char eb[16]; snprintf(eb, sizeof eb, "e%d", ex);
    buf_puts(b, mant); buf_puts(b, eb);
    return;
  }
  for (int prec = 15; prec <= 17; prec++) {
    snprintf(tmp, sizeof tmp, "%.*g", prec, d);
    if (strtod(tmp, NULL) == d) break;
  }
  buf_puts(b, tmp);
  if (!strchr(tmp, '.')) buf_puts(b, ".0");
}

static int looks_numeric(const char *s, size_t n) {
  if (n == 0) return 0;
  Obj dummy; (void)dummy;
  size_t i = 0;
  if (s[i] == '+' || s[i] == '-') i++;
  if (i >= n) return 0;
  int digit = 0;
  for (; i < n; i++) {
    if (isdigit((unsigned char)s[i])) digit = 1;
    else if (!(s[i] == '.' || s[i] == 'E' || s[i] == 'D' || s[i] == '+' || s[i] == '-')) return 0;
  }
  return digit;
}

static int needs_bars(const char *s, size_t n) {
  if (n == 0) return 1;
  if (n == 1 && s[0] == '.') return 1;
  if (looks_numeric(s, n)) return 1;
  for (size_t i = 0; i < n; i++) {
    unsigned char c = (unsigned char)s[i];
    if (islower(c) || c <= ' ' || strchr("()'\";,`|\\:#", c)) { if (c == '#' && i > 0) continue; return 1; }
  }
  return 0;
}

/* Printer state read from the printer variables when the outermost print starts (saved and restored around nested
   prints, because a PRINT-OBJECT method may rebind them). */
static int pr_case = 0;                         /* *PRINT-CASE*: 0 :UPCASE, 1 :DOWNCASE, 2 :CAPITALIZE */
static long pr_level = -1, pr_length = -1;      /* *PRINT-LEVEL*, *PRINT-LENGTH*; -1 for no limit */

/* NAME with *PRINT-CASE* applied.  Only all-uppercase names change: a name with lowercase letters is already
   distinctive (it is printed with bars under escaping). */
static void put_cased(Buf *b, const char *s, size_t n) {
  if (pr_case == 0) { buf_put(b, s, n); return; }
  for (size_t i = 0; i < n; i++) if (s[i] >= 'a' && s[i] <= 'z') { buf_put(b, s, n); return; }
  int in_word = 0;
  for (size_t i = 0; i < n; i++) {
    unsigned char c = (unsigned char)s[i];
    int alnum = (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9');
    if (c >= 'A' && c <= 'Z' && (pr_case == 1 || in_word)) c = (unsigned char)(c + 32);
    in_word = alnum;
    buf_putc(b, c);
  }
}

static void print_symbol_name(Buf *b, Obj name, int escape) {
  size_t n; const char *s = str_c(name, &n);
  if (escape && needs_bars(s, n)) {
    buf_putc(b, '|');
    for (size_t i = 0; i < n; i++) { if (s[i] == '|' || s[i] == '\\') buf_putc(b, '\\'); buf_putc(b, s[i]); }
    buf_putc(b, '|');
  } else put_cased(b, s, n);
}

static void print_symbol(Buf *b, Obj sym, int escape, int readable) {
  Obj name = SYM_NAME(sym), pkg = SYM_PKG(sym);
  if (!escape) { size_t n; const char *s = str_c(name, &n); put_cased(b, s, n); return; }
  if (pkg == NIL && sym != NIL) { buf_puts(b, "#:"); print_symbol_name(b, name, 1); return; }
  if (pkg == PKG_KEYWORD) { buf_putc(b, ':'); print_symbol_name(b, name, 1); return; }
  int st = 0;
  if (!readable) {
    Obj found = find_symbol_in(name, current_package(), &st);
    if (st && found == sym) { print_symbol_name(b, name, 1); return; }
  }
  if (readable && pkg == PKG_USER) { /* fall through to explicit prefix */ }
  st = 0; (void)find_symbol_in(name, pkg, &st);
  Obj pn = SLOT(pkg, PK_NAME);
  /* use the shortest nickname for CL */
  if (pkg == PKG_CL) put_cased(b, "CL", 2); else print_symbol_name(b, pn, 1);
  buf_puts(b, st == 2 ? ":" : "::");
  print_symbol_name(b, name, 1);
}

static void print_string(Buf *b, Obj s, int escape) {
  if (!escape) { buf_put_str(b, s); return; }
  buf_putc(b, '"');
  size_t n = str_len(s);
  for (size_t i = 0; i < n; i++) {
    int c = str_ref(s, i);
    if (c == '"' || c == '\\') buf_putc(b, '\\');
    buf_put_cp(b, c);
  }
  buf_putc(b, '"');
}

static void print_char(Buf *b, int c, int escape) {
  if (!escape) { buf_put_cp(b, c); return; }
  buf_puts(b, "#\\");
  switch (c) {
  case ' ': buf_puts(b, "Space"); return;
  case '\n': buf_puts(b, "Newline"); return;
  case '\t': buf_puts(b, "Tab"); return;
  case '\r': buf_puts(b, "Return"); return;
  case '\f': buf_puts(b, "Page"); return;
  case 127: buf_puts(b, "Rubout"); return;
  case 8: buf_puts(b, "Backspace"); return;
  case 0: buf_puts(b, "Nul"); return;
  case 27: buf_puts(b, "Escape"); return;
  default: break;
  }
  if (c < 32) { char t[16]; snprintf(t, sizeof t, "U%X", c); buf_puts(b, t); return; }
  buf_put_cp(b, c);
}

static void print_rec(Buf *b, Obj o, int escape, int readable, int depth);

/* *PRINT-BASE* / *PRINT-RADIX* as read when the outermost print started (saved and restored around nested prints,
   because a PRINT-OBJECT method may rebind them). */
static int pr_base = 10, pr_radix = 0;
static int current_print_base(void) {
  Obj v = SYM_VALUE(Q_print_base);
  return (FIXP(v) && UNFIX(v) >= 2 && UNFIX(v) <= 36) ? (int)UNFIX(v) : 10;
}

/* an integer or ratio in the current base, with the radix marker when *PRINT-RADIX* is true:
   "#x" "#o" "#b" "#Nr" before the digits, or a trailing "." for a decimal integer ("#10r" for a decimal ratio) */
static void print_rational(Buf *b, Obj o) {
  int ratio = OBJP(o) && OBJ_TYPE(o) == T_RATIO;
  if (pr_radix) {
    if (pr_base == 10) { if (ratio) buf_puts(b, "#10r"); }
    else if (pr_base == 2) buf_puts(b, "#b");
    else if (pr_base == 8) buf_puts(b, "#o");
    else if (pr_base == 16) buf_puts(b, "#x");
    else { char t[16]; snprintf(t, sizeof t, "#%dr", pr_base); buf_puts(b, t); }
  }
  number_print(b, o, pr_base);
  if (pr_radix && pr_base == 10 && !ratio) buf_putc(b, '.');
}
/* ---- *PRINT-CIRCLE* ------------------------------------------------------------------------------------------
   A first pass over the object records every cons, vector and structure in an open-addressing table keyed by address
   (nothing moves while a print runs).  An object met twice is shared; its label is 0 (seen once), -1 (shared, no label
   yet) or N > 0 (printed as #N=; later occurrences print #N#). */
typedef struct { Obj *key; long *lab; size_t cap, cnt; } CTab;
static CTab *pr_circ = NULL;
static long pr_next_label = 0;

static int composite_p(Obj o) {
  if (CONSP(o)) return 1;
  if (!OBJP(o)) return 0;
  switch (OBJ_TYPE(o)) {
  case T_VECTOR: case T_MDARRAY: case T_STRUCT: return 1;
  case T_ADVEC: return !av_char_p(o);
  default: return 0;
  }
}
static size_t ct_slot(Obj o, size_t cap) { return (size_t)(((uint64_t)o >> 3) * 0x9E3779B97F4A7C15ULL) & (cap - 1); }
static CTab *ct_new(void) {
  CTab *t = malloc(sizeof *t);
  t->cap = 256; t->cnt = 0; t->key = calloc(t->cap, sizeof(Obj)); t->lab = calloc(t->cap, sizeof(long));
  return t;
}
static void ct_free(CTab *t) { if (t) { free(t->key); free(t->lab); free(t); } }
static long *ct_find(CTab *t, Obj o) {
  for (size_t i = ct_slot(o, t->cap); t->key[i]; i = (i + 1) & (t->cap - 1)) if (t->key[i] == o) return &t->lab[i];
  return NULL;
}
static void ct_grow(CTab *t) {
  Obj *ok = t->key; long *ol = t->lab; size_t ocap = t->cap;
  t->cap *= 2; t->key = calloc(t->cap, sizeof(Obj)); t->lab = calloc(t->cap, sizeof(long));
  for (size_t j = 0; j < ocap; j++) if (ok[j]) {
    size_t i = ct_slot(ok[j], t->cap);
    while (t->key[i]) i = (i + 1) & (t->cap - 1);
    t->key[i] = ok[j]; t->lab[i] = ol[j];
  }
  free(ok); free(ol);
}
/* 1 if O was already visited (and is now marked shared) */
static int ct_visit(CTab *t, Obj o) {
  long *p = ct_find(t, o);
  if (p) { if (*p == 0) *p = -1; return 1; }
  if ((t->cnt + 1) * 2 > t->cap) ct_grow(t);
  size_t i = ct_slot(o, t->cap);
  while (t->key[i]) i = (i + 1) & (t->cap - 1);
  t->key[i] = o; t->lab[i] = 0; t->cnt++;
  return 0;
}
static void circ_scan(CTab *t, Obj o) {
  STACK_CHECK();
  for (;;) {
    if (CONSP(o)) { if (ct_visit(t, o)) return; circ_scan(t, CAR(o)); o = CDR(o); continue; }
    if (!composite_p(o)) return;
    if (ct_visit(t, o)) return;
    switch (OBJ_TYPE(o)) {
    case T_VECTOR: for (size_t i = 0; i < OBJ_NWORDS(o); i++) circ_scan(t, SLOT(o, i)); break;
    case T_ADVEC: { long fill = av_length(o); for (long i = 0; i < fill; i++) circ_scan(t, av_ref(o, i)); break; }
    case T_STRUCT: for (size_t i = 1; i < OBJ_NWORDS(o); i++) circ_scan(t, SLOT(o, i)); break;
    case T_MDARRAY: { Obj d = SLOT(o, AR_DATA); if (SLOT(o, AR_OFFSET) == NIL && !STRINGP(d)) circ_scan(t, d); break; }
    default: break;
    }
    return;
  }
}
/* A cdr that is shared (or labelled) must be printed in dotted form so that it can carry its label. */
static int circ_shared(Obj o) { long *p = pr_circ ? ct_find(pr_circ, o) : NULL; return p && *p != 0; }

/* the elements of a multi-dimensional array from row-major index START, as nested lists by DIMS */
static long print_md(Buf *b, Obj data, Obj dims, long start, int escape, int readable, int depth) {
  if (dims == NIL) {
    Obj x = arr_rm_ref(data, start);
    print_rec(b, x, escape, readable, depth + 1);
    return start + 1;
  }
  long n = (long)UNFIX(CAR(dims));
  buf_putc(b, '(');
  for (long i = 0; i < n; i++) { if (i) buf_putc(b, ' '); start = print_md(b, data, CDR(dims), start, escape, readable, depth + 1); }
  buf_putc(b, ')');
  return start;
}
void print_object_rec(Buf *b, Obj o) { print_rec(b, o, 1, 0, 0); }    /* the parts of a complex number */

static void print_rec(Buf *b, Obj o, int escape, int readable, int depth) {
  STACK_CHECK();
  if (depth > 2000) { buf_puts(b, "..."); return; }
  if (FIXP(o)) { print_rational(b, o); return; }
  if (CHARP(o)) { print_char(b, CHAR_CODE(o), escape); return; }
  if (composite_p(o)) {
    int too_deep = pr_level >= 0 && depth >= pr_level;
    if (pr_circ) {
      long *lab = ct_find(pr_circ, o);
      if (lab && *lab > 0) { char t[32]; snprintf(t, sizeof t, "#%ld#", *lab); buf_puts(b, t); return; }
      if (lab && *lab == -1 && !too_deep) { char t[32]; *lab = ++pr_next_label; snprintf(t, sizeof t, "#%ld=", *lab); buf_puts(b, t); }
    }
    if (too_deep) { buf_putc(b, '#'); return; }
  }
  if (CONSP(o)) {
    Obj h = CAR(o);
    if (CONSP(CDR(o)) && CDR(CDR(o)) == NIL) {
      const char *pre = NULL;
      if (h == Q_quote) pre = "'";
      else if (h == Q_function) pre = "#'";
      if (pre && !circ_shared(CDR(o))) { buf_puts(b, pre); print_rec(b, CAR(CDR(o)), escape, readable, depth + 1); return; }
    }
    buf_putc(b, '(');
    long n = 0, cnt = 0;
    for (;;) {
      if (pr_length >= 0 && cnt >= pr_length) { buf_puts(b, "..."); break; }
      print_rec(b, CAR(o), escape, readable, depth + 1);
      cnt++;
      o = CDR(o);
      if (o == NIL) break;
      if (!CONSP(o)) { buf_puts(b, " . "); print_rec(b, o, escape, readable, depth + 1); break; }
      if (circ_shared(o)) { buf_puts(b, " . "); print_rec(b, o, escape, readable, depth); break; }
      buf_putc(b, ' ');
      if (++n > 100000) { buf_puts(b, "..."); break; }
    }
    buf_putc(b, ')');
    return;
  }
  if (!OBJP(o)) {
    if (o == UNBOUND) buf_puts(b, "#<UNBOUND>");
    else if (o == EOF_OBJ) buf_puts(b, "#<EOF>");
    else if (o == UNSUPPLIED) buf_puts(b, "#<UNSUPPLIED>");
    else buf_puts(b, "#<IMMEDIATE>");
    return;
  }
  switch (OBJ_TYPE(o)) {
  case T_SYMBOL: print_symbol(b, o, escape, readable); return;
  case T_STRING: case T_WSTRING: case T_STRBOX: print_string(b, o, escape); return;
  case T_FLONUM: format_double(b, flonum_value(o)); return;
  case T_BIGNUM: case T_RATIO: print_rational(b, o); return;
  case T_COMPLEX: number_print(b, o, pr_base); return;
  case T_VECTOR: {
    buf_puts(b, "#(");
    for (size_t i = 0; i < OBJ_NWORDS(o); i++) {
      if (i) buf_putc(b, ' ');
      if (pr_length >= 0 && (long)i >= pr_length) { buf_puts(b, "..."); break; }
      print_rec(b, SLOT(o, i), escape, readable, depth + 1);
    }
    buf_putc(b, ')');
    return;
  }
  case T_MDARRAY: {
    Obj dims = SLOT(o, AR_DIMS); long rank = list_length(dims);
    char hdr[32]; snprintf(hdr, sizeof hdr, "#%ldA", rank); buf_puts(b, hdr);
    print_md(b, o, dims, 0, escape, readable, depth);
    return;
  }
  case T_ADVEC: {
    long fill = av_length(o);
    if (av_char_p(o)) { print_string(b, av_to_string(o), escape); return; }
    int bits = SLOT(o, AV_ETYPE) == FIX(AE_BIT);
    buf_puts(b, bits ? "#*" : "#(");
    for (long i = 0; i < fill; i++) {
      if (bits) { buf_putc(b, UNFIX(av_ref(o, i)) ? '1' : '0'); continue; }
      if (i) buf_putc(b, ' ');
      if (pr_length >= 0 && i >= pr_length) { buf_puts(b, "..."); break; }
      print_rec(b, av_ref(o, i), escape, readable, depth + 1);
    }
    if (!bits) buf_putc(b, ')');
    return;
  }
  case T_EVALFN: buf_puts(b, "#<FUNCTION "); print_rec(b, SLOT(o, EF_NAME), 1, 0, depth + 1); buf_putc(b, '>'); return;
  case T_CLOSURE: buf_puts(b, "#<COMPILED-FUNCTION "); print_rec(b, SLOT(SLOT(o, 0), C_NAME), 1, 0, depth + 1); buf_putc(b, '>'); return;
  case T_CODE: buf_puts(b, "#<CODE "); print_rec(b, SLOT(o, C_NAME), 1, 0, depth + 1); buf_putc(b, '>'); return;
  case T_PRIM: buf_puts(b, "#<PRIMITIVE "); print_rec(b, SLOT(o, PR_NAME), 1, 0, depth + 1); buf_putc(b, '>'); return;
  case T_STREAM: buf_puts(b, "#<STREAM>"); return;
  case T_PACKAGE:
    if (SLOT(o, PK_NAME) == NIL) { buf_puts(b, "#<PACKAGE (deleted)>"); return; }
    buf_puts(b, "#<PACKAGE "); print_string(b, SLOT(o, PK_NAME), 0); buf_putc(b, '>'); return;
  case T_STRUCT: {
    Obj hook = SYM_FUNC(Q_print_hook);
    if (hook != NIL && !CONSP(hook)) {
      gc_inhibit++;
      Obj r = call2(hook, o, BOOL(escape));
      gc_inhibit--;
      if (STRINGP(r)) { buf_put_str(b, r); return; }
    }
    Obj type = SLOT(o, 0);
    if (IS_TYPE(type, T_STRUCT)) { buf_puts(b, "#<INSTANCE>"); return; }
    buf_puts(b, "#S(");
    Obj name = CONSP(type) ? CAR(type) : type;
    print_rec(b, name, escape, readable, depth + 1);
    Obj slots = CONSP(type) ? CDR(type) : NIL;
    for (size_t i = 1; i < OBJ_NWORDS(o); i++) {
      buf_putc(b, ' ');
      if (CONSP(slots)) { buf_putc(b, ':'); print_rec(b, CAR(slots), 0, 0, depth + 1); slots = CDR(slots); buf_putc(b, ' '); }
      print_rec(b, SLOT(o, i), escape, readable, depth + 1);
    }
    buf_putc(b, ')');
    return;
  }
  case T_WEAK: {
    int mode = (int)UNFIX(SLOT(o, WK_MODE));
    if (mode == WKM_BROKEN || mode == WKM_FIRED) { buf_puts(b, "#<WEAK-POINTER broken>"); return; }
    buf_puts(b, mode == WKM_FINAL ? "#<FINALIZER " : mode == WKM_POINTER ? "#<WEAK-POINTER " : "#<WEAK-ENTRY ");
    print_rec(b, SLOT(o, WK_KEY), escape, readable, depth + 1);
    buf_putc(b, '>');
    return;
  }
  default: buf_puts(b, "#<OBJECT>"); return;
  }
}

void print_to_buf(Buf *b, Obj o, int escape, int readable) {
  int sb = pr_base, sr = pr_radix, sc = pr_case;
  long slv = pr_level, sln = pr_length, snext = pr_next_label;
  CTab *scirc = pr_circ;
  pr_base = current_print_base(); pr_radix = SYM_VALUE(Q_print_radix) != NIL;
  Obj v = SYM_VALUE(Q_print_level);  pr_level  = (FIXP(v) && UNFIX(v) >= 0) ? (long)UNFIX(v) : -1;
  v = SYM_VALUE(Q_print_length);     pr_length = (FIXP(v) && UNFIX(v) >= 0) ? (long)UNFIX(v) : -1;
  v = SYM_VALUE(Q_print_case);
  pr_case = 0;
  if (SYMBOLP(v) && v != NIL) {
    const char *nm = STR_C(SYM_NAME(v));
    if (!strcmp(nm, "DOWNCASE")) pr_case = 1; else if (!strcmp(nm, "CAPITALIZE")) pr_case = 2;
  }
  pr_circ = NULL; pr_next_label = 0;
  if (SYM_VALUE(Q_print_circle) != NIL && composite_p(o)) { pr_circ = ct_new(); circ_scan(pr_circ, o); }
  print_rec(b, o, escape, readable, 0);
  ct_free(pr_circ);
  pr_base = sb; pr_radix = sr; pr_case = sc; pr_level = slv; pr_length = sln; pr_circ = scirc; pr_next_label = snext;
}

/* *PRINT-PRETTY* true: a list is laid out by the Lisp function %PP-HOOK (object escape stream -> string, or NIL to
   print it flat); STREAM is NIL when printing to a string.  *PRINT-LEVEL* and *PRINT-CIRCLE* are honoured only by the
   flat printer, so when either is set the list is printed flat. */
static Obj pretty_string(Obj o, int escape, Obj stream) {
  if (!CONSP(o) || SYM_VALUE(Q_print_pretty) == NIL) return NIL;
  if (SYM_VALUE(Q_print_circle) != NIL || SYM_VALUE(Q_print_level) != NIL) return NIL;
  Obj hook = SYM_FUNC(Q_pp_hook);
  if (hook == NIL || CONSP(hook)) return NIL;
  gc_inhibit++;
  Obj r = call3(hook, o, BOOL(escape), stream);
  gc_inhibit--;
  return STRINGP(r) ? r : NIL;
}

Obj print_to_string(Obj o, int escape, int readable) {
  if (!readable) { Obj p = pretty_string(o, escape, NIL); if (p != NIL) return p; }
  Buf b; buf_init(&b);
  print_to_buf(&b, o, escape, readable);
  Obj s = make_string_utf8(b.buf ? b.buf : "", b.len);
  buf_free(&b);
  return s;
}

void print_obj(Obj stream, Obj o, int escape) {
  Obj p = pretty_string(o, escape, stream);
  if (p != NIL) { Buf pb; buf_init(&pb); buf_put_str(&pb, p); stream_write(stream, pb.buf ? pb.buf : "", pb.len); buf_free(&pb); return; }
  Buf b; buf_init(&b);
  print_to_buf(&b, o, escape, 0);
  stream_write(stream, b.buf ? b.buf : "", b.len);
  buf_free(&b);
}
