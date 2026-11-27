/* fasl.c -- the binary fasl format (docs/design/compiler.md, "Fasl format").
 *
 * A fasl file is a sequence of top-level code objects.  Layout (integers little-endian):
 *
 *     bytes 0-7    7F 'N' 'F' 'A' 'S' 'L' 0D 0A          magic (the CR LF catches text-mode mangling)
 *     byte  8      format version (FASL_VERSION)
 *     bytes 9-11   zero
 *     bytes 12-15  entry count
 *     entries      u32 length, then that many bytes of payload; one payload per top-level form
 *     last 4       CRC-32 of every byte before it
 *
 * A payload is one encoded object (always a code object).  Objects are a tag byte followed by data; integers inside
 * payloads are LEB128 varints (signed ones zigzag-coded).  Each payload has its own symbol table: a symbol is written
 * in full the first time it occurs and as a table index afterwards, so a payload decodes without any other state.
 * That matters because entries must be decoded one at a time, as the file loads: an early form may create the
 * package that a later form's symbols live in.
 *
 * Output is a function of the code objects alone (no timestamps, no addresses), which the self-hosting fixed-point
 * check relies on.  The decoder treats its input as hostile as far as memory safety goes: lengths are bounded by the
 * bytes that remain, nesting by the C stack check, and instruction streams are verified by vm_validate_code.
 * (It does not make a malicious fasl harmless -- it is code.)
 */
#include "neogenera.h"
#include <stdio.h>
#include <string.h>

#define FASL_VERSION 2
#define FASL_HEADER 16

enum {
  FT_NIL, FT_T, FT_FIXNUM, FT_BIGNUM, FT_RATIO, FT_COMPLEX, FT_FLONUM, FT_NSTRING, FT_WSTRING, FT_CHAR, FT_LIST,
  FT_VECTOR, FT_SYMBOL, FT_UNINTERNED, FT_SYMREF, FT_CODE, FT_UNSUPPLIED, FT_STRUCT, FT_MDARRAY, FT_COUNT
};

/* ---- CRC-32 ----------------------------------------------------------------------------------------------- */
static uint32_t crc_table[256];
static int crc_ready;
static uint32_t crc32_of(const unsigned char *p, size_t n) {
  if (!crc_ready) {
    for (uint32_t i = 0; i < 256; i++) {
      uint32_t c = i;
      for (int k = 0; k < 8; k++) c = (c & 1) ? 0xEDB88320u ^ (c >> 1) : c >> 1;
      crc_table[i] = c;
    }
    crc_ready = 1;
  }
  uint32_t c = 0xFFFFFFFFu;
  for (size_t i = 0; i < n; i++) c = crc_table[(c ^ p[i]) & 0xff] ^ (c >> 8);
  return c ^ 0xFFFFFFFFu;
}

/* ---- encoder ---------------------------------------------------------------------------------------------- */
typedef struct { Buf b; Obj syms; int nsyms; } Enc;      /* SYMS: list of the symbols seen, newest first */

static void put_byte(Enc *e, int c) { char ch = (char)c; buf_put(&e->b, &ch, 1); }
static void put_uvar(Enc *e, uint64_t v) {
  while (v >= 0x80) { put_byte(e, (int)(v & 0x7f) | 0x80); v >>= 7; }
  put_byte(e, (int)v);
}
static void put_svar(Enc *e, int64_t v) { put_uvar(e, ((uint64_t)v << 1) ^ (uint64_t)(v >> 63)); }
static void put_bytes(Enc *e, const char *s, size_t n) { put_uvar(e, n); buf_put(&e->b, s, n); }

static int sym_index(Enc *e, Obj s) {
  int i = e->nsyms - 1;
  for (Obj l = e->syms; CONSP(l); l = CDR(l), i--) if (CAR(l) == s) return i;
  return -1;
}
static void sym_note(Enc *e, Obj s) { e->syms = cons(s, e->syms); e->nsyms++; }

static void enc_obj(Enc *e, Obj o);

static void enc_string_utf8(Enc *e, Obj str) {
  size_t n; const char *s = str_c(str, &n);
  put_bytes(e, s, n);
}

static void enc_code(Enc *e, Obj c) {
  put_byte(e, FT_CODE);
  enc_obj(e, SLOT(c, C_NAME));
  enc_obj(e, SLOT(c, C_NREQ)); enc_obj(e, SLOT(c, C_NOPT));
  put_byte(e, SLOT(c, C_REST) != NIL);
  Obj bytes = SLOT(c, C_INSTRS);
  size_t n = NSTR_LEN(bytes) / 4;
  const int32_t *ip = (const int32_t *)NSTR_DATA(bytes);
  put_uvar(e, n);
  for (size_t i = 0; i < n; i++) put_svar(e, ip[i]);
  Obj consts = SLOT(c, C_CONSTS);
  size_t nc = OBJ_NWORDS(consts);
  put_uvar(e, nc);
  for (size_t i = 0; i < nc; i++) enc_obj(e, SLOT(consts, i));
  enc_obj(e, SLOT(c, C_ARGLIST));
}

static void enc_obj(Enc *e, Obj o) {
  STACK_CHECK();
  if (o == NIL) { put_byte(e, FT_NIL); return; }
  if (o == TT) { put_byte(e, FT_T); return; }
  if (FIXP(o)) { put_byte(e, FT_FIXNUM); put_svar(e, UNFIX(o)); return; }
  if (CHARP(o)) { put_byte(e, FT_CHAR); put_uvar(e, (uint64_t)CHAR_CODE(o)); return; }
  if (o == UNSUPPLIED) { put_byte(e, FT_UNSUPPLIED); return; }
  if (CONSP(o)) {
    size_t n = 0; Obj l = o;
    while (CONSP(l)) { n++; l = CDR(l); if (n > ((size_t)1 << 26)) lisp_error("Cannot dump a circular list to a fasl file"); }
    put_byte(e, FT_LIST); put_uvar(e, n);
    for (l = o; CONSP(l); l = CDR(l)) enc_obj(e, CAR(l));
    enc_obj(e, l);                                   /* the tail: NIL for a proper list */
    return;
  }
  if (OBJP(o)) {
    switch (OBJ_TYPE(o)) {
    case T_SYMBOL: {
      int i = sym_index(e, o);
      if (i >= 0) { put_byte(e, FT_SYMREF); put_uvar(e, (uint64_t)i); return; }
      Obj pkg = SYM_PKG(o);
      if (IS_TYPE(pkg, T_PACKAGE)) { put_byte(e, FT_SYMBOL); enc_string_utf8(e, SLOT(pkg, PK_NAME)); }
      else put_byte(e, FT_UNINTERNED);
      enc_string_utf8(e, SYM_NAME(o));
      sym_note(e, o);
      return;
    }
    case T_STRING: case T_WSTRING: case T_STRBOX: {
      size_t n = str_len(o); int wide = 0;
      for (size_t i = 0; i < n; i++) if (str_ref(o, i) > 255) { wide = 1; break; }
      if (!wide) {
        put_byte(e, FT_NSTRING); put_uvar(e, n);
        for (size_t i = 0; i < n; i++) put_byte(e, str_ref(o, i));
      } else {
        put_byte(e, FT_WSTRING); put_uvar(e, n);
        for (size_t i = 0; i < n; i++) put_uvar(e, (uint64_t)str_ref(o, i));
      }
      return;
    }
    case T_FLONUM: {
      double d = flonum_value(o); uint64_t u; memcpy(&u, &d, 8);
      put_byte(e, FT_FLONUM);
      for (int i = 0; i < 8; i++) put_byte(e, (int)((u >> (8 * i)) & 0xff));
      return;
    }
    case T_BIGNUM: {
      Buf t; buf_init(&t);
      int_print(&t, o, 10);
      put_byte(e, FT_BIGNUM); put_bytes(e, t.buf, t.len);
      buf_free(&t);
      return;
    }
    case T_RATIO: put_byte(e, FT_RATIO); enc_obj(e, ratio_numerator(o)); enc_obj(e, ratio_denominator(o)); return;
    case T_COMPLEX: put_byte(e, FT_COMPLEX); enc_obj(e, SLOT(o, 0)); enc_obj(e, SLOT(o, 1)); return;
    case T_VECTOR: {
      size_t n = OBJ_NWORDS(o);
      put_byte(e, FT_VECTOR); put_uvar(e, n);
      for (size_t i = 0; i < n; i++) enc_obj(e, SLOT(o, i));
      return;
    }
    case T_STRUCT: {
      size_t n = OBJ_NWORDS(o);
      put_byte(e, FT_STRUCT); put_uvar(e, n);
      for (size_t i = 0; i < n; i++) enc_obj(e, SLOT(o, i));
      return;
    }
    case T_MDARRAY: put_byte(e, FT_MDARRAY); enc_obj(e, SLOT(o, AR_DATA)); enc_obj(e, SLOT(o, AR_DIMS)); return;
    case T_CODE: enc_code(e, o); return;
    default: break;
    }
  }
  Obj r = print_to_string(o, 1, 0);
  lisp_error("Cannot dump %s to a fasl file", STR_C(r));
}

PRIM(p_fasl_encode) {
  /* (%fasl-encode code) -> the payload bytes, as a narrow string */
  UNUSED_ARGS;
  if (!IS_TYPE(argv[0], T_CODE)) type_error("CODE", argv[0]);
  Enc e; buf_init(&e.b); e.syms = NIL; e.nsyms = 0;
  enc_obj(&e, argv[0]);                              /* e.syms holds conses: allocation never collects */
  Obj s = make_string(e.b.buf, e.b.len);
  buf_free(&e.b);
  return s;
}

static void put32(Buf *b, uint32_t v) { char c[4] = { (char)v, (char)(v >> 8), (char)(v >> 16), (char)(v >> 24) }; buf_put(b, c, 4); }

PRIM(p_fasl_write_file) {
  /* (%fasl-write-file path payloads) -> number of bytes written */
  UNUSED_ARGS;
  if (!STRINGP(argv[0])) type_error("STRING", argv[0]);
  Buf b; buf_init(&b);
  static const char magic[8] = { 0x7f, 'N', 'F', 'A', 'S', 'L', 0x0d, 0x0a };
  buf_put(&b, magic, 8);
  char ver[4] = { FASL_VERSION, 0, 0, 0 };
  buf_put(&b, ver, 4);
  long n = list_length(argv[1]);
  if (n < 0) { buf_free(&b); type_error("LIST", argv[1]); }
  put32(&b, (uint32_t)n);
  for (Obj l = argv[1]; CONSP(l); l = CDR(l)) {
    Obj p = CAR(l);
    if (!IS_TYPE(p, T_STRING)) { buf_free(&b); type_error("fasl payload", p); }
    put32(&b, (uint32_t)NSTR_LEN(p));
    buf_put(&b, NSTR_DATA(p), NSTR_LEN(p));
  }
  put32(&b, crc32_of((const unsigned char *)b.buf, b.len));
  FILE *f = fopen(STR_C(argv[0]), "wb");
  if (!f) { buf_free(&b); lisp_error("Cannot open %s for writing", STR_C(argv[0])); }
  size_t w = fwrite(b.buf, 1, b.len, f);
  int bad = fclose(f) != 0 || w != b.len;
  size_t total = b.len;
  buf_free(&b);
  if (bad) lisp_error("Error writing %s", STR_C(argv[0]));
  return FIX((SObj)total);
}

PRIM(p_fasl_write_raw) {
  /* (%fasl-write-raw path bytes) -- write the bytes of a narrow string to a file verbatim (tests build damaged files) */
  UNUSED_ARGS;
  if (!STRINGP(argv[0])) type_error("STRING", argv[0]);
  if (!IS_TYPE(argv[1], T_STRING)) type_error("narrow string", argv[1]);
  FILE *f = fopen(STR_C(argv[0]), "wb");
  if (!f) lisp_error("Cannot open %s for writing", STR_C(argv[0]));
  size_t n = NSTR_LEN(argv[1]);
  size_t w = fwrite(NSTR_DATA(argv[1]), 1, n, f);
  int bad = fclose(f) != 0 || w != n;
  if (bad) lisp_error("Error writing %s", STR_C(argv[0]));
  return FIX((SObj)n);
}

PRIM(p_fasl_read_file) {
  /* (%fasl-read-file path) -> the file's bytes as a narrow string, or NIL when it cannot be opened */
  UNUSED_ARGS;
  if (!STRINGP(argv[0])) type_error("STRING", argv[0]);
  FILE *f = fopen(STR_C(argv[0]), "rb");
  if (!f) return NIL;
  Buf b; buf_init(&b);
  char chunk[65536]; size_t n;
  while ((n = fread(chunk, 1, sizeof chunk, f)) > 0) buf_put(&b, chunk, n);
  fclose(f);
  Obj s = make_string(b.buf, b.len);
  buf_free(&b);
  return s;
}

/* ---- decoder ---------------------------------------------------------------------------------------------- */
typedef struct { const unsigned char *p; size_t n, pos; Obj syms; int nsyms; } Dec;   /* SYMS: vector, grown by doubling */

static void bad(const char *why) __attribute__((noreturn));
static void bad(const char *why) { lisp_error("Bad fasl file: %s", why); }

static int get_byte(Dec *d) { if (d->pos >= d->n) bad("truncated"); return d->p[d->pos++]; }
static uint64_t get_uvar(Dec *d) {
  uint64_t v = 0; int shift = 0;
  for (;;) {
    int c = get_byte(d);
    if (shift == 63 && (c & 0x7e)) bad("integer too large");
    v |= (uint64_t)(c & 0x7f) << shift;
    if (!(c & 0x80)) return v;
    shift += 7;
    if (shift > 63) bad("integer too large");
  }
}
static int64_t get_svar(Dec *d) { uint64_t u = get_uvar(d); return (int64_t)(u >> 1) ^ -(int64_t)(u & 1); }
static size_t get_len(Dec *d, size_t unit) {       /* a count of things that each take at least UNIT bytes */
  uint64_t n = get_uvar(d);
  if (n > (d->n - d->pos) / (unit ? unit : 1)) bad("length exceeds the data");
  return (size_t)n;
}
static const unsigned char *get_raw(Dec *d, size_t n) {
  if (n > d->n - d->pos) bad("truncated");
  const unsigned char *p = d->p + d->pos; d->pos += n; return p;
}

static Obj dec_obj(Dec *d);

static void dec_note(Dec *d, Obj s) {
  size_t cap = d->syms == NIL ? 0 : OBJ_NWORDS(d->syms);
  if ((size_t)d->nsyms >= cap) {
    size_t ncap = cap ? cap * 2 : 16;
    Obj nv = make_vector(ncap, NIL);
    for (size_t i = 0; i < cap; i++) SLOT(nv, i) = SLOT(d->syms, i);
    d->syms = nv;
  }
  SLOT(d->syms, d->nsyms++) = s;
}

static Obj dec_name(Dec *d) {
  size_t n = get_len(d, 1);
  const unsigned char *s = get_raw(d, n);
  return make_string_utf8((const char *)s, n);
}

static int dec_fixnum_field(Dec *d, const char *what) {
  Obj v = dec_obj(d);
  if (!FIXP(v) || UNFIX(v) < 0 || UNFIX(v) > 1000000) bad(what);
  return (int)UNFIX(v);
}

static Obj dec_code(Dec *d) {
  Obj name = dec_obj(d);
  Obj nreq = FIX(dec_fixnum_field(d, "bad required-argument count"));
  Obj nopt = FIX(dec_fixnum_field(d, "bad optional-argument count"));
  Obj rest = get_byte(d) ? TT : NIL;
  size_t ni = get_len(d, 1);
  Obj bytes = make_string(NULL, ni * 4);
  int32_t *ip = (int32_t *)NSTR_DATA(bytes);
  for (size_t i = 0; i < ni; i++) {
    int64_t w = get_svar(d);
    if (w < INT32_MIN || w > INT32_MAX) bad("instruction word out of range");
    ip[i] = (int32_t)w;
  }
  size_t nc = get_len(d, 1);
  Obj consts = make_vector(nc, NIL);
  for (size_t i = 0; i < nc; i++) SLOT(consts, i) = dec_obj(d);
  Obj arglist = dec_obj(d);
  const char *why = vm_validate_code(ip, ni, nc);
  if (why) bad(why);
  for (size_t i = 0; i < ni; ) {                      /* CLOSURE must name a code object */
    int op = ip[i];
    if (op == OP_CLOSURE && !IS_TYPE(SLOT(consts, ip[i + 1]), T_CODE)) bad("CLOSURE constant is not code");
    if (op == OP_CHKRANGE) {
      Obj r = SLOT(consts, ip[i + 1]);
      if (!CONSP(r) || !FIXP(CAR(r)) || !FIXP(CDR(r))) bad("CHKRANGE constant is not a pair of fixnums");
    }
    if ((op == OP_GREF || op == OP_GSET || op == OP_GFUN || op == OP_BIND) && !SYMBOLP(SLOT(consts, ip[i + 1])))
      bad("global operand is not a symbol");
    i += 1 + (size_t)vm_op_nargs(op);
  }
  Obj c = make_obj(T_CODE, C_SLOTS);
  SLOT(c, C_NAME) = name; SLOT(c, C_NREQ) = nreq; SLOT(c, C_NOPT) = nopt; SLOT(c, C_REST) = rest;
  SLOT(c, C_INSTRS) = bytes; SLOT(c, C_CONSTS) = consts; SLOT(c, C_ARGLIST) = arglist;
  return c;
}

static Obj dec_obj(Dec *d) {
  STACK_CHECK();
  int t = get_byte(d);
  switch (t) {
  case FT_NIL: return NIL;
  case FT_T: return TT;
  case FT_FIXNUM: {
    int64_t v = get_svar(d);
    if (!FIX_FITS(v)) bad("fixnum out of range");
    return FIX(v);
  }
  case FT_CHAR: { uint64_t c = get_uvar(d); if (c > 0x10FFFF) bad("bad character"); return MK_CHAR((int)c); }
  case FT_UNSUPPLIED: return UNSUPPLIED;
  case FT_FLONUM: {
    const unsigned char *p = get_raw(d, 8); uint64_t u = 0; double x;
    for (int i = 0; i < 8; i++) u |= (uint64_t)p[i] << (8 * i);
    memcpy(&x, &u, 8);
    return make_flonum(x);
  }
  case FT_BIGNUM: {
    size_t n = get_len(d, 1);
    const unsigned char *p = get_raw(d, n);
    Obj v = int_parse((const char *)p, n, 10);
    if (v == UNBOUND) bad("bad integer");
    return v;
  }
  case FT_RATIO: {
    Obj n = dec_obj(d), den = dec_obj(d);
    if (!is_integer(n) || !is_integer(den) || int_sign(den) <= 0) bad("bad ratio");
    return make_ratio(n, den);
  }
  case FT_COMPLEX: {
    Obj re = dec_obj(d), im = dec_obj(d);
    if (!(FIXP(re) || FLONUMP(re) || IS_TYPE(re, T_BIGNUM) || IS_TYPE(re, T_RATIO)) ||
        !(FIXP(im) || FLONUMP(im) || IS_TYPE(im, T_BIGNUM) || IS_TYPE(im, T_RATIO))) bad("bad complex number");
    return make_complex(re, im);
  }
  case FT_NSTRING: {
    size_t n = get_len(d, 1);
    return make_string((const char *)get_raw(d, n), n);
  }
  case FT_WSTRING: {
    size_t n = get_len(d, 1);
    Obj w = make_wstring(n);
    for (size_t i = 0; i < n; i++) {
      uint64_t c = get_uvar(d);
      if (c > 0x10FFFF) bad("bad character");
      WSTR_DATA(w)[i] = (uint32_t)c;
    }
    return w;
  }
  case FT_LIST: {
    size_t n = get_len(d, 1);
    if (n == 0) bad("empty list run");
    /* build the run in order without recursion on the spine */
    Obj head = NIL, tail = NIL;
    for (size_t i = 0; i < n; i++) {
      Obj c = cons(dec_obj(d), NIL);
      if (head == NIL) head = c; else { CDR(tail) = c; WB(tail, c); }
      tail = c;
    }
    Obj end = dec_obj(d);
    CDR(tail) = end; WB(tail, end);
    return head;
  }
  case FT_VECTOR: {
    size_t n = get_len(d, 1);
    Obj v = make_vector(n, NIL);
    for (size_t i = 0; i < n; i++) { Obj x = dec_obj(d); SLOT(v, i) = x; WB(v, x); }
    return v;
  }
  case FT_STRUCT: {
    size_t n = get_len(d, 1);
    if (n == 0) bad("empty structure");
    Obj s = make_obj(T_STRUCT, n);
    for (size_t i = 0; i < n; i++) { Obj x = dec_obj(d); SLOT(s, i) = x; WB(s, x); }
    return s;
  }
  case FT_MDARRAY: {
    Obj data = dec_obj(d), dims = dec_obj(d);
    if (!VECTORP(data) && !STRINGP(data)) bad("bad array data");
    Obj a = make_obj(T_MDARRAY, AR_SLOTS);
    SLOT(a, AR_DATA) = data; SLOT(a, AR_DIMS) = dims; WB(a, data); WB(a, dims);
    return a;
  }
  case FT_SYMBOL: {
    Obj pname = dec_name(d), name = dec_name(d);
    Obj pkg = find_package(pname);
    if (pkg == NIL) lisp_error("Bad fasl file: the package %s does not exist", STR_C(pname));
    int st;
    Obj s = intern_obj(name, pkg, &st);
    dec_note(d, s);
    return s;
  }
  case FT_UNINTERNED: {
    Obj s = make_symbol_obj(dec_name(d));
    dec_note(d, s);
    return s;
  }
  case FT_SYMREF: {
    uint64_t i = get_uvar(d);
    if (i >= (uint64_t)d->nsyms) bad("bad symbol reference");
    return SLOT(d->syms, i);
  }
  case FT_CODE: return dec_code(d);
  default: bad("unknown object tag");
  }
}

static uint32_t rd32(const unsigned char *p) { return (uint32_t)p[0] | (uint32_t)p[1] << 8 | (uint32_t)p[2] << 16 | (uint32_t)p[3] << 24; }

PRIM(p_fasl_entries) {
  /* (%fasl-entries bytes) -> the number of entries; signals an error unless BYTES is a well-formed fasl file */
  UNUSED_ARGS;
  Obj s = argv[0];
  if (!IS_TYPE(s, T_STRING)) type_error("fasl bytes", s);
  const unsigned char *p = (const unsigned char *)NSTR_DATA(s);
  size_t n = NSTR_LEN(s);
  static const unsigned char magic[8] = { 0x7f, 'N', 'F', 'A', 'S', 'L', 0x0d, 0x0a };
  if (n < FASL_HEADER + 4 || memcmp(p, magic, 8) != 0) bad("not a NeoGenera binary fasl file");
  if (p[8] != FASL_VERSION) lisp_error("Bad fasl file: format version %d, this system reads version %d (recompile it)", p[8], FASL_VERSION);
  if (crc32_of(p, n - 4) != rd32(p + n - 4)) bad("checksum mismatch (the file is damaged or truncated)");
  uint32_t count = rd32(p + 12);
  size_t pos = FASL_HEADER;
  for (uint32_t i = 0; i < count; i++) {
    if (n - 4 - pos < 4) bad("truncated entry table");
    uint32_t len = rd32(p + pos); pos += 4;
    if (len > n - 4 - pos) bad("entry runs past the end of the file");
    pos += len;
  }
  if (pos != n - 4) bad("trailing bytes after the last entry");
  return FIX((SObj)count);
}

PRIM(p_fasl_decode) {
  /* (%fasl-decode bytes pos) -> (code . next-pos); POS is FASL-HEADER (16) for the first entry.  BYTES must have
     passed %fasl-entries. */
  UNUSED_ARGS;
  Obj s = argv[0];
  if (!IS_TYPE(s, T_STRING)) type_error("fasl bytes", s);
  if (!FIXP(argv[1])) type_error("FIXNUM", argv[1]);
  const unsigned char *p = (const unsigned char *)NSTR_DATA(s);
  size_t n = NSTR_LEN(s), pos = (size_t)UNFIX(argv[1]);
  if (UNFIX(argv[1]) < FASL_HEADER || pos + 4 > n - 4) bad("bad entry position");
  uint32_t len = rd32(p + pos); pos += 4;
  if (len > n - 4 - pos) bad("entry runs past the end of the file");
  Dec d = { p + pos, len, 0, NIL, 0 };
  Obj code = dec_obj(&d);
  if (!IS_TYPE(code, T_CODE)) bad("entry is not a code object");
  if (d.pos != d.n) bad("trailing bytes inside an entry");
  return cons(code, FIX((SObj)(pos + len)));
}

void init_fasl(void) {
  defprim("%FASL-ENCODE", p_fasl_encode, 1, 1);
  defprim("%FASL-WRITE-FILE", p_fasl_write_file, 2, 2);
  defprim("%FASL-READ-FILE", p_fasl_read_file, 1, 1);
  defprim("%FASL-WRITE-RAW", p_fasl_write_raw, 2, 2);
  defprim("%FASL-ENTRIES", p_fasl_entries, 1, 1);
  defprim("%FASL-DECODE", p_fasl_decode, 2, 2);
}
