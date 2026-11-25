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

static void print_symbol_name(Buf *b, Obj name, int escape) {
  const char *s = STR_DATA(name); size_t n = STR_LEN(name);
  if (escape && needs_bars(s, n)) {
    buf_putc(b, '|');
    for (size_t i = 0; i < n; i++) { if (s[i] == '|' || s[i] == '\\') buf_putc(b, '\\'); buf_putc(b, s[i]); }
    buf_putc(b, '|');
  } else buf_put(b, s, n);
}

static void print_symbol(Buf *b, Obj sym, int escape, int readable) {
  Obj name = SYM_NAME(sym), pkg = SYM_PKG(sym);
  if (!escape) { buf_put(b, STR_DATA(name), STR_LEN(name)); return; }
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
  if (pkg == PKG_CL) buf_puts(b, "CL"); else print_symbol_name(b, pn, 1);
  buf_puts(b, st == 2 ? ":" : "::");
  print_symbol_name(b, name, 1);
}

static void print_string(Buf *b, Obj s, int escape) {
  if (!escape) { buf_put(b, STR_DATA(s), STR_LEN(s)); return; }
  buf_putc(b, '"');
  for (size_t i = 0; i < STR_LEN(s); i++) {
    char c = STR_DATA(s)[i];
    if (c == '"' || c == '\\') buf_putc(b, '\\');
    buf_putc(b, c);
  }
  buf_putc(b, '"');
}

static void print_char(Buf *b, int c, int escape) {
  if (!escape) {
    Obj tmp = make_string_out_stream();
    stream_putc(tmp, c);
    buf_put(b, STR_DATA(SLOT(tmp, ST_BUF)), (size_t)UNFIX(SLOT(tmp, ST_LEN)));
    return;
  }
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
  Obj tmp = make_string_out_stream();
  stream_putc(tmp, c);
  buf_put(b, STR_DATA(SLOT(tmp, ST_BUF)), (size_t)UNFIX(SLOT(tmp, ST_LEN)));
}

static void print_rec(Buf *b, Obj o, int escape, int readable, int depth) {
  if (depth > 2000) { buf_puts(b, "..."); return; }
  if (FIXP(o)) { char t[32]; snprintf(t, sizeof t, "%ld", (long)UNFIX(o)); buf_puts(b, t); return; }
  if (CHARP(o)) { print_char(b, CHAR_CODE(o), escape); return; }
  if (CONSP(o)) {
    Obj h = CAR(o);
    if (CONSP(CDR(o)) && CDR(CDR(o)) == NIL) {
      const char *pre = NULL;
      if (h == Q_quote) pre = "'";
      else if (h == Q_function) pre = "#'";
      if (pre) { buf_puts(b, pre); print_rec(b, CAR(CDR(o)), escape, readable, depth + 1); return; }
    }
    buf_putc(b, '(');
    long n = 0;
    for (;;) {
      print_rec(b, CAR(o), escape, readable, depth + 1);
      o = CDR(o);
      if (o == NIL) break;
      if (!CONSP(o)) { buf_puts(b, " . "); print_rec(b, o, escape, readable, depth + 1); break; }
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
  case T_STRING: print_string(b, o, escape); return;
  case T_FLONUM: format_double(b, flonum_value(o)); return;
  case T_VECTOR: {
    buf_puts(b, "#(");
    for (size_t i = 0; i < OBJ_NWORDS(o); i++) { if (i) buf_putc(b, ' '); print_rec(b, SLOT(o, i), escape, readable, depth + 1); }
    buf_putc(b, ')');
    return;
  }
  case T_ADVEC: {
    Obj data = SLOT(o, AV_DATA); long fill = (long)UNFIX(SLOT(o, AV_FILL));
    if (STRINGP(data)) {
      Obj s = make_string(STR_DATA(data), (size_t)fill);
      print_string(b, s, escape);
    } else {
      buf_puts(b, "#(");
      for (long i = 0; i < fill; i++) { if (i) buf_putc(b, ' '); print_rec(b, SLOT(data, i), escape, readable, depth + 1); }
      buf_putc(b, ')');
    }
    return;
  }
  case T_EVALFN: buf_puts(b, "#<FUNCTION "); print_rec(b, SLOT(o, EF_NAME), 1, 0, depth + 1); buf_putc(b, '>'); return;
  case T_CLOSURE: buf_puts(b, "#<COMPILED-FUNCTION "); print_rec(b, SLOT(SLOT(o, 0), C_NAME), 1, 0, depth + 1); buf_putc(b, '>'); return;
  case T_CODE: buf_puts(b, "#<CODE "); print_rec(b, SLOT(o, C_NAME), 1, 0, depth + 1); buf_putc(b, '>'); return;
  case T_PRIM: buf_puts(b, "#<PRIMITIVE "); print_rec(b, SLOT(o, PR_NAME), 1, 0, depth + 1); buf_putc(b, '>'); return;
  case T_STREAM: buf_puts(b, "#<STREAM>"); return;
  case T_PACKAGE: buf_puts(b, "#<PACKAGE "); print_string(b, SLOT(o, PK_NAME), 0); buf_putc(b, '>'); return;
  case T_STRUCT: {
    Obj type = SLOT(o, 0);
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
  default: buf_puts(b, "#<OBJECT>"); return;
  }
}

void print_to_buf(Buf *b, Obj o, int escape, int readable) { print_rec(b, o, escape, readable, 0); }

Obj print_to_string(Obj o, int escape, int readable) {
  Buf b; buf_init(&b);
  print_to_buf(&b, o, escape, readable);
  Obj s = make_string(b.buf ? b.buf : "", b.len);
  buf_free(&b);
  return s;
}

void print_obj(Obj stream, Obj o, int escape) {
  Buf b; buf_init(&b);
  print_to_buf(&b, o, escape, 0);
  stream_write(stream, b.buf ? b.buf : "", b.len);
  buf_free(&b);
}
