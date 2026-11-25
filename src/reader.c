/* reader.c -- the S-expression reader (C implementation; see ROADMAP Phase 2).
 *
 * Supports: lists, dotted pairs, strings, characters, fixnums, flonums,
 * package-qualified symbols, quote/backquote/comma, #' #( #\ #: #| |# #+ #-
 * #x #b #o #. and ; comments.  Backquote is expanded here into LIST / CONS /
 * APPEND / QUOTE calls.
 */
#include "neogenera.h"
#include <ctype.h>
#include <errno.h>
#include <math.h>

static int qq_depth;

static Obj rd(Obj s);

static void reader_error(const char *msg) __attribute__((noreturn));
static void reader_error(const char *msg) { lisp_error("READ error: %s", msg); }

static int is_ws(int c) { return c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f' || c == 11; }
static int is_term(int c) {
  return c == '(' || c == ')' || c == '\'' || c == '"' || c == ';' || c == '`' || c == ',';
}
static int is_delim(int c) { return c < 0 || is_ws(c) || is_term(c); }

/* skip whitespace and comments; returns next char (unread) or -1 */
static int skip_ws(Obj s) {
  for (;;) {
    int c = stream_getc(s);
    if (c < 0) return -1;
    if (is_ws(c)) continue;
    if (c == ';') { while ((c = stream_getc(s)) >= 0 && c != '\n') ; if (c < 0) return -1; continue; }
    stream_ungetc(s, c);
    return c;
  }
}

/* ---- numbers ---------------------------------------------------------- */
static int parse_number(const char *tok, Obj *out) {
  const char *p = tok;
  if (*p == '+' || *p == '-') p++;
  const char *digits = p;
  while (isdigit((unsigned char)*p)) p++;
  size_t nd = (size_t)(p - digits);
  if (*p == 0 && nd > 0) { *out = int_parse(tok, strlen(tok), 10); return 1; }
  if (*p == '/' && nd > 0) {                              /* ratio */
    const char *den = p + 1, *q = den;
    while (isdigit((unsigned char)*q)) q++;
    if (*q == 0 && q > den) {
      Obj n = int_parse(tok, (size_t)(p - tok), 10), d = int_parse(den, strlen(den), 10);
      *out = make_ratio(n, d);
      return 1;
    }
    return 0;
  }
  if (*p == '.' && p[1] == 0 && nd > 0) { *out = int_parse(tok, strlen(tok) - 1, 10); return 1; }   /* "12." */
  /* float syntax */
  size_t nfrac = 0;
  int have_dot = 0;
  if (*p == '.') { have_dot = 1; p++; const char *f = p; while (isdigit((unsigned char)*p)) p++; nfrac = (size_t)(p - f); }
  int have_exp = 0;
  if (*p && strchr("EDFSL", *p) && (nd > 0 || nfrac > 0)) {
    const char *q = p + 1;
    if (*q == '+' || *q == '-') q++;
    if (isdigit((unsigned char)*q)) { while (isdigit((unsigned char)*q)) q++; if (*q == 0) { have_exp = 1; p = q; } }
  }
  if (*p != 0) return 0;
  if (!(have_dot ? (nfrac > 0 || (nd > 0 && have_exp)) : (nd > 0 && have_exp))) return 0;
  if (have_dot && nfrac == 0 && !have_exp) return 0;
  char buf[128]; size_t n = strlen(tok);
  if (n >= sizeof buf) return 0;
  memcpy(buf, tok, n + 1);
  for (size_t i = 0; i < n; i++) if (strchr("DFSL", buf[i]) && i > 0) buf[i] = 'e';
  *out = make_flonum(strtod(buf, NULL));
  return 1;
}

/* ---- tokens ------------------------------------------------------------ */
static Obj make_symbol_from_token(const char *buf, size_t len, int *colons, int ncolons, int escaped) {
  (void)escaped;
  if (ncolons == 0) {
    int st; Obj str = make_string(buf, len);
    return intern_obj(str, current_package(), &st);
  }
  int first = colons[0];
  int dbl = (ncolons >= 2 && colons[1] == first + 1);
  if (ncolons > 2 || (ncolons == 2 && !dbl)) reader_error("too many colons in token");
  const char *name = buf + first + (dbl ? 2 : 1);
  size_t nlen = len - (size_t)first - (dbl ? 2 : 1);
  Obj nstr = make_string(name, nlen);
  Obj pkg;
  if (first == 0) pkg = PKG_KEYWORD;
  else {
    char pn[256];
    if ((size_t)first >= sizeof pn) reader_error("package name too long");
    memcpy(pn, buf, (size_t)first); pn[first] = 0;
    pkg = find_package_c(pn);
    if (pkg == NIL) lisp_error("READ error: no package named %s", pn);
  }
  int st;
  if (dbl || pkg == PKG_KEYWORD) return intern_obj(nstr, pkg, &st);
  Obj sym = find_symbol_in(nstr, pkg, &st);
  if (st != 2 && !(st == 1 && pkg == PKG_CL)) {
    if (st == 0 || st == 1) lisp_error("READ error: %s is not an external symbol of package %s", STR_DATA(nstr), STR_DATA(SLOT(pkg, PK_NAME)));
  }
  return sym;
}

/* read a token starting with 'first' (already consumed). */
static Obj read_atom(Obj s, int first, int *was_dot) {
  Buf b; buf_init(&b);
  int colons[4]; int ncolons = 0, escaped = 0;
  int c = first;
  if (was_dot) *was_dot = 0;
  for (;;) {
    if (c == '\\') {
      int n = stream_getc(s); if (n < 0) reader_error("EOF after backslash");
      buf_putc(&b, n); escaped = 1;
    } else if (c == '|') {
      escaped = 1;
      for (;;) {
        int n = stream_getc(s);
        if (n < 0) reader_error("EOF inside |...|");
        if (n == '|') break;
        if (n == '\\') { n = stream_getc(s); if (n < 0) reader_error("EOF"); }
        buf_putc(&b, n);
      }
    } else {
      if (c == ':' ) { if (ncolons < 4) colons[ncolons] = (int)b.len; ncolons++; buf_putc(&b, ':'); }
      else buf_putc(&b, toupper(c));
    }
    c = stream_getc(s);
    if (is_delim(c)) { stream_ungetc(s, c); break; }
  }
  if (ncolons > 4) reader_error("too many colons");
  buf_putc(&b, 0); b.len--;
  Obj result;
  if (!escaped && b.len == 1 && b.buf[0] == '.') { if (was_dot) *was_dot = 1; buf_free(&b); return NIL; }
  if (!escaped && ncolons == 0 && parse_number(b.buf, &result)) { buf_free(&b); return result; }
  result = make_symbol_from_token(b.buf, b.len, colons, ncolons > 4 ? 4 : ncolons, escaped);
  buf_free(&b);
  return result;
}

/* ---- backquote -------------------------------------------------------- */
static int is_form(Obj x, Obj head) { return CONSP(x) && CAR(x) == head && CONSP(CDR(x)) && CDR(CDR(x)) == NIL; }

static Obj bq(Obj x, int depth);

/* Code inside a depth-1 comma may itself contain backquotes that were left
 * unexpanded by the reader; expand them as fresh depth-1 templates. */
static Obj expand_inner(Obj e) {
  if (!CONSP(e)) return e;
  if (is_form(e, Q_quasiquote)) return bq(expand_inner(CAR(CDR(e))), 1);
  if (CAR(e) == Q_quote) return e;
  Obj head = NIL, tail = NIL, l = e;
  for (; CONSP(l); l = CDR(l)) {
    Obj c = cons(expand_inner(CAR(l)), NIL);
    if (head == NIL) head = c; else CDR(tail) = c;
    tail = c;
  }
  if (l != NIL) CDR(tail) = l;
  return head;
}

static Obj bq_list(Obj x, int depth) {
  /* x is a list (possibly dotted); build an expression producing it */
  Obj segs = NIL;                       /* reversed list of segment exprs */
  Obj cur = NIL;                        /* reversed run of single elements */
  Obj l = x;
  while (CONSP(l)) {
    if (is_form(l, Q_unquote) || is_form(l, Q_quasiquote)) break;   /* dotted unquote tail */
    Obj e = CAR(l);
    if (is_form(e, Q_unquote_splicing) && depth == 1) {
      if (cur != NIL) { segs = cons(cons(Q_list, nreverse_list(cur)), segs); cur = NIL; }
      segs = cons(expand_inner(CAR(CDR(e))), segs);
    } else {
      Obj ex;
      if (is_form(e, Q_unquote_splicing)) ex = list3(Q_list, list2(Q_quote, Q_unquote_splicing), bq(CAR(CDR(e)), depth - 1));
      else ex = bq(e, depth);
      cur = cons(ex, cur);
    }
    l = CDR(l);
  }
  if (cur != NIL) segs = cons(cons(Q_list, nreverse_list(cur)), segs);
  if (l != NIL) segs = cons(bq(l, depth), segs);
  segs = nreverse_list(segs);
  if (segs == NIL) return NIL;
  if (CDR(segs) == NIL) {
    Obj only = CAR(segs);
    /* a lone spliced expression must still be copied? CL allows sharing */
    return only;
  }
  return cons(Q_append, segs);
}

static int constantp_simple(Obj e) { return !CONSP(e) && !(SYMBOLP(e) && e != NIL && e != TT && SYM_PKG(e) != PKG_KEYWORD); }

static Obj bq(Obj x, int depth) {
  if (!CONSP(x)) {
    if (SYMBOLP(x) && x != NIL && x != TT && SYM_PKG(x) != PKG_KEYWORD) return list2(Q_quote, x);
    if (VECTORP(x)) {
      Obj l = NIL;
      for (size_t i = OBJ_NWORDS(x); i > 0; i--) l = cons(SLOT(x, i - 1), l);
      return list2(Q_list_to_vector, bq(l, depth));
    }
    return x;
  }
  if (is_form(x, Q_unquote)) {
    if (depth == 1) return expand_inner(CAR(CDR(x)));
    return list3(Q_list, list2(Q_quote, Q_unquote), bq(CAR(CDR(x)), depth - 1));
  }
  if (is_form(x, Q_quasiquote)) return list3(Q_list, list2(Q_quote, Q_quasiquote), bq(CAR(CDR(x)), depth + 1));
  if (is_form(x, Q_unquote_splicing)) {
    if (depth == 1) reader_error(",@ not in a list");
    return list3(Q_list, list2(Q_quote, Q_unquote_splicing), bq(CAR(CDR(x)), depth - 1));
  }
  Obj r = bq_list(x, depth);
  /* fold fully constant lists into a quote */
  if (CONSP(r) && CAR(r) == Q_list) {
    int all = 1;
    for (Obj l = CDR(r); l != NIL; l = CDR(l)) if (!(CONSP(CAR(l)) && CAR(CAR(l)) == Q_quote) && !constantp_simple(CAR(l))) { all = 0; break; }
    if (all) return list2(Q_quote, x);
  }
  return r;
}

/* ---- main dispatch ---------------------------------------------------- */
static int feature_test(Obj f) {
  if (SYMBOLP(f)) {
    for (Obj l = SYM_VALUE(Q_features); CONSP(l); l = CDR(l)) if (CAR(l) == f) return 1;
    return 0;
  }
  if (CONSP(f)) {
    Obj h = CAR(f);
    if (SYMBOLP(h)) {
      const char *n = STR_DATA(SYM_NAME(h));
      if (!strcmp(n, "NOT")) return !feature_test(CAR(CDR(f)));
      if (!strcmp(n, "AND")) { for (Obj l = CDR(f); CONSP(l); l = CDR(l)) if (!feature_test(CAR(l))) return 0; return 1; }
      if (!strcmp(n, "OR")) { for (Obj l = CDR(f); CONSP(l); l = CDR(l)) if (feature_test(CAR(l))) return 1; return 0; }
    }
  }
  reader_error("bad feature expression");
}

static Obj read_list(Obj s, int close) {
  Obj head = NIL, tail = NIL;
  for (;;) {
    int c = skip_ws(s);
    if (c < 0) reader_error("EOF inside list");
    if (c == close) { stream_getc(s); return head; }
    if (c == ')') reader_error("mismatched close parenthesis");
    if (c == '.') {
      stream_getc(s);
      int n = stream_peekc(s);
      if (is_delim(n)) {
        if (head == NIL) reader_error("nothing before dot");
        Obj last = rd(s);
        if (skip_ws(s) != close) reader_error("expected ) after dotted tail");
        stream_getc(s);
        CDR(tail) = last;
        return head;
      }
      Obj a = read_atom(s, '.', NULL);
      Obj cell = cons(a, NIL);
      if (head == NIL) head = cell; else CDR(tail) = cell;
      tail = cell;
      continue;
    }
    Obj x = rd(s);
    Obj cell = cons(x, NIL);
    if (head == NIL) head = cell; else CDR(tail) = cell;
    tail = cell;
  }
}

static int utf8_continue(Obj s, int c) {
  int extra = c >= 0xF0 ? 3 : c >= 0xE0 ? 2 : c >= 0xC0 ? 1 : 0;
  int code = extra == 3 ? (c & 7) : extra == 2 ? (c & 15) : extra == 1 ? (c & 31) : c;
  while (extra-- > 0) { int n = stream_getc(s); if (n < 0) break; code = (code << 6) | (n & 0x3F); }
  return code;
}

static Obj read_char_literal(Obj s) {
  int c = stream_getc(s);
  if (c < 0) reader_error("EOF after #\\");
  Buf b; buf_init(&b);
  buf_putc(&b, c);
  for (;;) {
    int n = stream_getc(s);
    if (is_delim(n)) { stream_ungetc(s, n); break; }
    buf_putc(&b, n);
  }
  buf_putc(&b, 0); b.len--;
  Obj r;
  if (b.len == 1) r = MK_CHAR((unsigned char)b.buf[0]);
  else {
    unsigned char c0 = (unsigned char)b.buf[0];
    if (c0 >= 0xC0) {                       /* UTF-8 multibyte char */
      Obj ms = make_string(b.buf + 1, b.len - 1);
      Obj ss = make_string_in_stream(ms);
      r = MK_CHAR(utf8_continue(ss, c0));
    } else {
      static const struct { const char *n; int c; } names[] = {
        {"SPACE", ' '}, {"NEWLINE", '\n'}, {"TAB", '\t'}, {"RETURN", '\r'}, {"LINEFEED", '\n'},
        {"PAGE", '\f'}, {"RUBOUT", 127}, {"BACKSPACE", 8}, {"NUL", 0}, {"NULL", 0}, {"ESCAPE", 27}, {"BELL", 7}
      };
      char up[64]; size_t n = b.len < 63 ? b.len : 63;
      for (size_t i = 0; i < n; i++) up[i] = (char)toupper((unsigned char)b.buf[i]);
      up[n] = 0;
      r = NIL;
      for (size_t i = 0; i < sizeof names / sizeof names[0]; i++) if (!strcmp(up, names[i].n)) { r = MK_CHAR(names[i].c); break; }
      if (r == NIL) {
        if ((up[0] == 'U' || up[0] == 'X') && n > 1) r = MK_CHAR((int)strtol(up + 1, NULL, 16));
        else lisp_error("READ error: unknown character name #\\%s", b.buf);
      }
    }
  }
  buf_free(&b);
  return r;
}

static Obj read_hash(Obj s) {
  int c = stream_getc(s);
  if (c < 0) reader_error("EOF after #");
  while (isdigit(c)) c = stream_getc(s);        /* ignore numeric infix args */
  switch (c) {
  case '\'': return list2(Q_function, rd(s));
  case '(': {
    Obj l = read_list(s, ')');
    long n = list_length(l);
    Obj v = make_vector((size_t)n, NIL);
    for (long i = 0; i < n; i++, l = CDR(l)) SLOT(v, i) = CAR(l);
    return v;
  }
  case '\\': return read_char_literal(s);
  case ':': {
    int first = stream_getc(s);
    if (is_delim(first)) reader_error("bad #: syntax");
    Buf b; buf_init(&b);
    buf_putc(&b, toupper(first));
    for (;;) { int n = stream_getc(s); if (is_delim(n)) { stream_ungetc(s, n); break; } buf_putc(&b, toupper(n)); }
    Obj sym = make_symbol_obj(make_string(b.buf, b.len));
    buf_free(&b);
    return sym;
  }
  case '|': {
    int depth = 1, prev = 0;
    while (depth > 0) {
      int n = stream_getc(s);
      if (n < 0) reader_error("EOF in #| comment");
      if (prev == '|' && n == '#') { depth--; prev = 0; continue; }
      if (prev == '#' && n == '|') { depth++; prev = 0; continue; }
      prev = n;
    }
    return rd(s);
  }
  case '+': case '-': {
    Obj saved = SYM_VALUE(Q_package);
    SYM_VALUE(Q_package) = PKG_KEYWORD;
    Obj feat = rd(s);
    SYM_VALUE(Q_package) = saved;
    int ok = feature_test(feat);
    if (c == '-') ok = !ok;
    Obj form = rd(s);
    return ok ? form : rd(s);
  }
  case 'x': case 'X': case 'b': case 'B': case 'o': case 'O': {
    int base = (c == 'x' || c == 'X') ? 16 : (c == 'b' || c == 'B') ? 2 : 8;
    Buf b; buf_init(&b);
    for (;;) { int n = stream_getc(s); if (is_delim(n)) { stream_ungetc(s, n); break; } buf_putc(&b, n); }
    buf_putc(&b, 0);
    Obj v = int_parse(b.buf, b.len - 1, base);
    if (v == UNBOUND) reader_error("bad radix integer");
    buf_free(&b);
    return v;
  }
  case '.': {
    if (SYM_VALUE(Q_read_eval) == NIL) reader_error("#. disabled");
    Obj f = rd(s);
    return eval(f, NIL, NIL);
  }
  case '<': reader_error("unreadable object #<");
  default: lisp_error("READ error: unknown # dispatch character %c", c);
  }
}

static Obj rd(Obj s) {
  int c = skip_ws(s);
  if (c < 0) { reader_error("unexpected EOF"); }
  c = stream_getc(s);
  switch (c) {
  case '(': return read_list(s, ')');
  case ')': reader_error("unmatched close parenthesis");
  case '\'': return list2(Q_quote, rd(s));
  case '`': {
    qq_depth++;
    Obj x = rd(s);
    qq_depth--;
    if (qq_depth > 0) return list2(Q_quasiquote, x);
    return bq(x, 1);
  }
  case ',': {
    if (qq_depth == 0) reader_error("comma not inside backquote");
    int n = stream_peekc(s);
    if (n == '@') { stream_getc(s); return list2(Q_unquote_splicing, rd(s)); }
    return list2(Q_unquote, rd(s));
  }
  case '"': {
    Buf b; buf_init(&b);
    for (;;) {
      int n = stream_getc(s);
      if (n < 0) reader_error("EOF inside string");
      if (n == '"') break;
      if (n == '\\') { n = stream_getc(s); if (n < 0) reader_error("EOF inside string"); }
      buf_putc(&b, n);
    }
    Obj str = make_string(b.buf, b.len);
    buf_free(&b);
    return str;
  }
  case '#': return read_hash(s);
  default: return read_atom(s, c, NULL);
  }
}

long last_read_start;

Obj read_form(Obj stream, int *eof) {
  qq_depth = 0;
  int c = skip_ws(stream);
  if (c < 0) { *eof = 1; return NIL; }
  last_read_start = stream_position(stream);
  *eof = 0;
  return rd(stream);
}

Obj read_from_cstring(const char *str) {
  Obj ss = make_string_in_stream(make_cstring(str));
  int eof;
  return read_form(ss, &eof);
}
