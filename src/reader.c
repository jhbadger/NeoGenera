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

static int builtin_ws(int c) { return c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f' || c == 11; }
/* ---- the current readtable ------------------------------------------------------------------------------
 * *READTABLE* is NIL (built-in syntax only) or a READTABLE structure (lisp/core/57-reader.lisp) whose slots are
 * 1: case (fixnum: 0 upcase, 1 downcase, 2 preserve, 3 invert), 2: macro functions, 3: terminating flags and
 * 4: dispatch tables and 5: syntax overrides, each a vector indexed by character code below 128.  A syntax entry is NIL
 * (the character has the standard syntax: whitespace for space, tab, newline, return, page and VT; a macro character
 * for ( ) ' ` , " ; and #; a constituent otherwise), T (whitespace) or 0 (a plain constituent, or a macro character
 * whose function is in the macro table: neither whitespace nor standard macro syntax).  A dispatch table is a vector of 128
 * functions indexed by the upper-case sub-character. */
#define RT_SLOTS 6
static Obj cur_rt(void) {
  Obj rt = SYM_VALUE(Q_readtable);
  if (!IS_TYPE(rt, T_STRUCT) || OBJ_NWORDS(rt) < RT_SLOTS) return NIL;
  return rt;
}
static Obj rt_vec(Obj rt, int slot, int c) {
  if (rt == NIL || c < 0 || c >= 128) return NIL;
  Obj v = SLOT(rt, slot);
  return (IS_TYPE(v, T_VECTOR) && OBJ_NWORDS(v) >= 128) ? SLOT(v, c) : NIL;
}
static int rt_case(void) { Obj rt = cur_rt(); return rt == NIL || !FIXP(SLOT(rt, 1)) ? 0 : (int)UNFIX(SLOT(rt, 1)); }

/* the syntax override of C in the current readtable: NIL (standard), TT (whitespace) or FIX(0) (no standard syntax) */
static Obj rt_syntax(int c) { Obj rt = cur_rt(); return rt == NIL ? NIL : rt_vec(rt, 5, c); }
static int is_ws(int c) {
  Obj syn = rt_syntax(c);
  return syn == TT ? 1 : syn == NIL ? builtin_ws(c) : 0;
}
/* whether C still has the built-in macro syntax (it has not been made whitespace or a constituent) */
static int builtin_syntax_p(int c) { return rt_syntax(c) == NIL; }
int reader_whitespace_p(int c) { return is_ws(c); }       /* for PEEK-CHAR T: whitespace[2] in the current readtable */
static int is_term(int c) {
  Obj rt = cur_rt();
  if (rt != NIL && rt_vec(rt, 2, c) != NIL) return rt_vec(rt, 3, c) != NIL;
  if (c == '(' || c == ')' || c == '\'' || c == '"' || c == ';' || c == '`' || c == ',') return builtin_syntax_p(c);
  return 0;
}

/* Call a reader macro function: GC is inhibited, as the reader holds raw objects in C locals. */
static Obj call_reader_fn(Obj fn, Obj s, Obj a, Obj b, int nargs, int *nvalues) {
  gc_inhibit++;
  mv_count = 1;
  Obj r = nargs == 2 ? call2(fn, s, a) : call3(fn, s, a, b);
  gc_inhibit--;
  *nvalues = mv_count;
  return r;
}
static int is_delim(int c) { return c < 0 || is_ws(c) || is_term(c); }

/* skip whitespace and comments; returns next char (unread) or -1 */
static int skip_ws(Obj s) {
  for (;;) {
    int c = stream_getc(s);
    if (c < 0) return -1;
    if (is_ws(c)) continue;
    if (c == ';' && builtin_syntax_p(';') && rt_vec(cur_rt(), 2, ';') == NIL) { while ((c = stream_getc(s)) >= 0 && c != '\n') ; if (c < 0) return -1; continue; }
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
    int st; Obj str = make_string_utf8(buf, len);
    return intern_obj(str, current_package(), &st);
  }
  int first = colons[0];
  int dbl = (ncolons >= 2 && colons[1] == first + 1);
  if (ncolons > 2 || (ncolons == 2 && !dbl)) reader_error("too many colons in token");
  const char *name = buf + first + (dbl ? 2 : 1);
  size_t nlen = len - (size_t)first - (dbl ? 2 : 1);
  Obj nstr = make_string_utf8(name, nlen);
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
    if (st == 0 || st == 1) lisp_error("READ error: %s is not an external symbol of package %s", STR_C(nstr), STR_C(SLOT(pkg, PK_NAME)));
  }
  return sym;
}

/* the UTF-8 length of code point C */
static size_t cp_len(int c) { return c < 0x80 ? 1 : c < 0x800 ? 2 : c < 0x10000 ? 3 : 4; }

/* map the character at byte I of BUF (N bytes long, CP its code point) to code point TO, in place when the
   UTF-8 lengths agree (a mapping that would change the length leaves the character as it is) */
static void put_cp_at(char *buf, size_t i, size_t span, int cp, int to) {
  if (to == cp || span != cp_len(cp) || cp_len(to) != span) return;
  Buf b; char small[8]; b.buf = small; b.len = 0; b.cap = sizeof small;
  buf_put_cp(&b, to);
  memcpy(buf + i, b.buf, b.len);
}

/* Apply the readtable case to the unescaped characters of a token: MASK marks the escaped bytes. */
static void apply_case(char *buf, const char *escmask, size_t len, int mode) {
  if (mode == 2) return;
  int up = 0, low = 0;
  if (mode == 3) {                                  /* invert: only if all the unescaped letters have one case */
    for (size_t i = 0; i < len; ) {
      size_t at = i; int cp = utf8_decode((const unsigned char *)buf, len, &i);
      if (escmask[at]) continue;
      if (cp_downcase(cp) != cp) up = 1; else if (cp_upcase(cp) != cp) low = 1;
    }
    if (up && low) return;
  }
  for (size_t i = 0; i < len; ) {
    size_t at = i; int cp = utf8_decode((const unsigned char *)buf, len, &i);
    if (escmask[at]) continue;
    int down = mode == 1 || (mode == 3 && up);
    put_cp_at(buf, at, i - at, cp, down ? cp_downcase(cp) : cp_upcase(cp));
  }
}

/* add character C to a token (as UTF-8) and its escape flag for each byte */
static void tok_put(Buf *b, Buf *esc, int c, int escaped) {
  size_t before = b->len;
  buf_put_cp(b, c);
  for (size_t k = before; k < b->len; k++) buf_putc(esc, escaped);
}

/* read a token starting with 'first' (already consumed). */
static Obj read_atom(Obj s, int first, int *was_dot) {
  Buf b; buf_init(&b);
  Buf esc; buf_init(&esc);
  int colons[4]; int ncolons = 0, escaped = 0;
  int c = first;
  if (was_dot) *was_dot = 0;
  for (;;) {
    if (c == '\\') {
      int n = stream_getc(s); if (n < 0) reader_error("EOF after backslash");
      tok_put(&b, &esc, n, 1); escaped = 1;
    } else if (c == '|') {
      escaped = 1;
      for (;;) {
        int n = stream_getc(s);
        if (n < 0) reader_error("EOF inside |...|");
        if (n == '|') break;
        if (n == '\\') { n = stream_getc(s); if (n < 0) reader_error("EOF"); }
        tok_put(&b, &esc, n, 1);
      }
    } else {
      if (c == ':' ) { if (ncolons < 4) colons[ncolons] = (int)b.len; ncolons++; buf_putc(&b, ':'); buf_putc(&esc, 1); }
      else tok_put(&b, &esc, c, 0);
    }
    c = stream_getc(s);
    if (is_delim(c)) { stream_ungetc(s, c); break; }
  }
  if (ncolons > 4) reader_error("too many colons");
  apply_case(b.buf, esc.buf, b.len, rt_case());
  buf_free(&esc);
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
      const char *n = STR_C(SYM_NAME(h));
      if (!strcmp(n, "NOT")) return !feature_test(CAR(CDR(f)));
      if (!strcmp(n, "AND")) { for (Obj l = CDR(f); CONSP(l); l = CDR(l)) if (!feature_test(CAR(l))) return 0; return 1; }
      if (!strcmp(n, "OR")) { for (Obj l = CDR(f); CONSP(l); l = CDR(l)) if (feature_test(CAR(l))) return 1; return 0; }
    }
  }
  reader_error("bad feature expression");
}

static Obj rd1(Obj s);

static Obj read_list(Obj s, int close) {
  Obj head = NIL, tail = NIL;
  for (;;) {
    int c = skip_ws(s);
    if (c < 0) reader_error("EOF inside list");
    if (c == close) { stream_getc(s); return head; }
    if (c == ')' && builtin_syntax_p(')') && rt_vec(cur_rt(), 2, ')') == NIL) reader_error("mismatched close parenthesis");
    if (c == '.' && rt_vec(cur_rt(), 2, '.') == NIL) {
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
    Obj x = rd1(s);
    if (x == NOVALUE) continue;                     /* a comment, a skipped #+ form, a macro with no values */
    Obj cell = cons(x, NIL);
    if (head == NIL) head = cell; else CDR(tail) = cell;
    tail = cell;
  }
}

static Obj read_char_literal(Obj s) {
  int c = stream_getc(s);
  if (c < 0) reader_error("EOF after #\\");
  Buf b; buf_init(&b);
  buf_put_cp(&b, c);
  int count = 1;
  for (;;) {
    int n = stream_getc(s);
    if (is_delim(n)) { stream_ungetc(s, n); break; }
    buf_put_cp(&b, n); count++;
  }
  buf_putc(&b, 0); b.len--;
  Obj r;
  if (count == 1) r = MK_CHAR(c);
  else {
    {
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

static Obj read_hash_builtin(Obj s, int c);

static Obj read_hash(Obj s) {
  int c = stream_getc(s);
  if (c < 0) reader_error("EOF after #");
  long arg = -1;
  while (isdigit(c)) { arg = (arg < 0 ? 0 : arg) * 10 + (c - '0'); if (arg > 100000000) arg = 100000000; c = stream_getc(s); }
  Obj table = rt_vec(cur_rt(), 4, '#');
  if (IS_TYPE(table, T_VECTOR) && OBJ_NWORDS(table) >= 128 && c >= 0 && c < 128) {
    Obj fn = SLOT(table, toupper(c));
    if (fn != NIL) {
      int nv;
      Obj r = call_reader_fn(fn, s, MK_CHAR(c), arg < 0 ? NIL : FIX(arg), 3, &nv);
      return nv == 0 ? NOVALUE : r;
    }
  }
  return read_hash_builtin(s, c);
}

static Obj read_hash_builtin(Obj s, int c) {
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
    buf_put_cp(&b, first < 128 ? toupper(first) : first);
    for (;;) { int n = stream_getc(s); if (is_delim(n)) { stream_ungetc(s, n); break; } buf_put_cp(&b, n < 128 ? toupper(n) : n); }
    Obj sym = make_symbol_obj(make_string_utf8(b.buf, b.len));
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
    return NOVALUE;
  }
  case '+': case '-': {
    Obj saved = SYM_VALUE(Q_package);
    SYM_VALUE(Q_package) = PKG_KEYWORD;
    Obj feat = rd(s);
    SYM_VALUE(Q_package) = saved; WB(Q_package, saved);
    int ok = feature_test(feat);
    if (c == '-') ok = !ok;
    Obj form = rd(s);
    return ok ? form : NOVALUE;
  }
  case 'x': case 'X': case 'b': case 'B': case 'o': case 'O': {
    int base = (c == 'x' || c == 'X') ? 16 : (c == 'b' || c == 'B') ? 2 : 8;
    Buf b; buf_init(&b);
    for (;;) { int n = stream_getc(s); if (is_delim(n)) { stream_ungetc(s, n); break; } buf_put_cp(&b, n); }
    buf_putc(&b, 0);
    Obj v = int_parse(b.buf, b.len - 1, base);
    if (v == UNBOUND) reader_error("bad radix integer");
    buf_free(&b);
    return v;
  }
  case '.': {
    if (SYM_VALUE(Q_read_eval) == NIL) reader_error("#. disabled");
    Obj f = rd(s);
    gc_inhibit++;                                   /* the reader's callers hold raw objects */
    Obj v = eval(f, NIL, NIL);
    gc_inhibit--;
    return v;
  }
  case 'c': case 'C': {
    Obj parts = rd(s);
    if (!CONSP(parts) || !CONSP(CDR(parts)) || CDR(CDR(parts)) != NIL) reader_error("#C needs a list of two real numbers");
    return make_complex(CAR(parts), CAR(CDR(parts)));
  }
  case '<': reader_error("unreadable object #<");
  default: lisp_error("READ error: unknown # dispatch character %c", c);
  }
}

static Obj rd_builtin(Obj s, int c);

/* Read one object, or return NOVALUE for something that produced none (a comment, a skipped feature). */
static Obj rd1(Obj s) {
  STACK_CHECK();
  int c = skip_ws(s);
  if (c < 0) { reader_error("unexpected EOF"); }
  c = stream_getc(s);
  Obj fn = rt_vec(cur_rt(), 2, c);
  if (fn != NIL) {
    int nv;
    Obj r = call_reader_fn(fn, s, MK_CHAR(c), NIL, 2, &nv);
    return nv == 0 ? NOVALUE : r;
  }
  if (!builtin_syntax_p(c)) return read_atom(s, c, NULL);        /* made a constituent */
  return rd_builtin(s, c);
}

static Obj rd(Obj s) {
  for (;;) { Obj x = rd1(s); if (x != NOVALUE) return x; }
}

static Obj rd_builtin(Obj s, int c) {
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
      buf_put_cp(&b, n);
    }
    Obj str = make_string_utf8(b.buf, b.len);
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
  for (;;) {
    int c = skip_ws(stream);
    if (c < 0) { *eof = 1; return NIL; }
    last_read_start = stream_position(stream);
    *eof = 0;
    Obj x = rd1(stream);
    if (x != NOVALUE) return x;
  }
}

Obj read_form_recursive(Obj stream, int *eof) {
  int c = skip_ws(stream);
  if (c < 0) { *eof = 1; return NIL; }
  *eof = 0;
  return rd(stream);
}

/* ---- primitives for readtables (lisp/core/57-reader.lisp) --------------------------------------------------- */

PRIM(p_read_standard_macro) {
  /* (%read-standard-macro stream char): what the built-in syntax does for CHAR, which has been read */
  UNUSED_ARGS;
  if (!CHARP(argv[1])) type_error("CHARACTER", argv[1]);
  int c = CHAR_CODE(argv[1]);
  Obj x = c == '#' ? read_hash(argv[0]) : rd_builtin(argv[0], c);
  if (x == NOVALUE) { mv_count = 0; return NIL; }
  return x;
}

PRIM(p_read_standard_dispatch) {
  /* (%read-standard-dispatch stream sub-char arg): the built-in # syntax for SUB-CHAR */
  UNUSED_ARGS;
  if (!CHARP(argv[1])) type_error("CHARACTER", argv[1]);
  Obj x = read_hash_builtin(argv[0], CHAR_CODE(argv[1]));
  if (x == NOVALUE) { mv_count = 0; return NIL; }
  return x;
}

PRIM(p_read_delimited_list) {
  /* (%read-delimited-list char stream) */
  UNUSED_ARGS;
  if (!CHARP(argv[0])) type_error("CHARACTER", argv[0]);
  return read_list(argv[1], CHAR_CODE(argv[0]));
}

void init_reader_prims(void) {
  defprim_mv("%READ-STANDARD-MACRO", p_read_standard_macro, 2, 2);
  defprim_mv("%READ-STANDARD-DISPATCH", p_read_standard_dispatch, 3, 3);
  defprim("%READ-DELIMITED-LIST", p_read_delimited_list, 2, 2);
}

Obj read_from_cstring(const char *str) {
  Obj ss = make_string_in_stream(make_cstring(str));
  int eof;
  return read_form(ss, &eof);
}
