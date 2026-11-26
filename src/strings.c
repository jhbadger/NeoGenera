/* strings.c -- strings of characters.
 *
 * A string is a sequence of characters (code points up to U+10FFFF), held in one of three objects:
 *   T_STRING  a narrow string: one byte per character, for code points below 256 (Latin-1);
 *   T_WSTRING a wide string: 32 bits per character;
 *   T_STRBOX  a narrow string that had a wide character stored into it: it was turned in place into a box
 *             holding a wide copy, so every reference to it sees the change (slot 0 is the wide string).
 * Strings are made narrow when they can be.  Text crosses the kernel's edges (files, the terminal, the operating
 * system, C string literals) as UTF-8: MAKE_STRING_UTF8 decodes it and STR_C encodes it.
 */
#include "neogenera.h"

Obj str_resolve(Obj o) { return IS_TYPE(o, T_STRBOX) ? SLOT(o, 0) : o; }

size_t str_len(Obj o) {
  o = str_resolve(o);
  return IS_TYPE(o, T_WSTRING) ? WSTR_LEN(o) : NSTR_LEN(o);
}

int str_ref(Obj o, size_t i) {
  o = str_resolve(o);
  return IS_TYPE(o, T_WSTRING) ? (int)WSTR_DATA(o)[i] : (unsigned char)NSTR_DATA(o)[i];
}

int str_wide_p(Obj o) { return IS_TYPE(str_resolve(o), T_WSTRING); }

Obj make_wstring(size_t n) {
  size_t words = 1 + (n * 4 + 7) / 8;
  Obj o = make_raw(T_WSTRING, words);
  SLOT(o, 0) = (Obj)n;
  return o;
}

/* a wide copy of the narrow string N */
static Obj widen(Obj n) {
  size_t len = NSTR_LEN(n);
  Obj w = make_wstring(len);
  for (size_t i = 0; i < len; i++) WSTR_DATA(w)[i] = (unsigned char)NSTR_DATA(n)[i];
  return w;
}

void str_set(Obj o, size_t i, int c) {
  if (IS_TYPE(o, T_STRBOX)) { WSTR_DATA(SLOT(o, 0))[i] = (uint32_t)c; return; }
  if (IS_TYPE(o, T_WSTRING)) { WSTR_DATA(o)[i] = (uint32_t)c; return; }
  if (c < 256) { NSTR_DATA(o)[i] = (char)c; return; }
  /* a wide character in a narrow string: box it around a wide copy */
  PROTECT(o);
  Obj w = widen(o);
  nroots--;
  size_t nw = OBJ_NWORDS(o);
  PTR(o)[0] = HDR(nw, T_STRBOX) | (PTR(o)[0] & (Obj)8);           /* (keep the remembered bit) */
  SLOT(o, 0) = w; WB(o, w);
  for (size_t k = 1; k < nw; k++) SLOT(o, k) = FIX(0);
  WSTR_DATA(w)[i] = (uint32_t)c;
}

/* ---- making strings ----------------------------------------------------- */

Obj make_string_codes(const uint32_t *codes, size_t n) {
  int wide = 0;
  for (size_t i = 0; i < n; i++) if (codes[i] > 255) { wide = 1; break; }
  if (!wide) {
    Obj s = make_string(NULL, n);
    for (size_t i = 0; i < n; i++) NSTR_DATA(s)[i] = (char)codes[i];
    return s;
  }
  Obj w = make_wstring(n);
  memcpy(WSTR_DATA(w), codes, n * 4);
  return w;
}

/* Decode the UTF-8 sequence at S[*I] (of N bytes); a malformed byte stands for itself. */
int utf8_decode(const unsigned char *s, size_t n, size_t *i) {
  unsigned c = s[*i];
  int len = c < 0x80 ? 1 : (c >> 5) == 6 ? 2 : (c >> 4) == 14 ? 3 : (c >> 3) == 30 ? 4 : 0;
  if (len == 0 || *i + (size_t)len > n) { (*i)++; return (int)c; }
  unsigned cp = len == 1 ? c : len == 2 ? (c & 0x1F) : len == 3 ? (c & 0x0F) : (c & 0x07);
  for (int k = 1; k < len; k++) {
    unsigned d = s[*i + (size_t)k];
    if ((d & 0xC0) != 0x80) { (*i)++; return (int)c; }
    cp = (cp << 6) | (d & 0x3F);
  }
  if (cp > 0x10FFFF || (len == 2 && cp < 0x80) || (len == 3 && cp < 0x800) || (len == 4 && cp < 0x10000)) { (*i)++; return (int)c; }
  *i += (size_t)len;
  return (int)cp;
}

Obj make_string_utf8(const char *s, size_t n) {
  const unsigned char *u = (const unsigned char *)s;
  size_t i = 0, count = 0; int wide = 0, ascii = 1;
  while (i < n) { if (u[i] >= 0x80) ascii = 0; int c = utf8_decode(u, n, &i); if (c > 255) wide = 1; count++; }
  if (ascii) return make_string(s, n);
  Obj r = wide ? make_wstring(count) : make_string(NULL, count);
  i = 0;
  for (size_t k = 0; k < count; k++) {
    int c = utf8_decode(u, n, &i);
    if (wide) WSTR_DATA(r)[k] = (uint32_t)c; else NSTR_DATA(r)[k] = (char)c;
  }
  return r;
}

Obj make_cstring(const char *s) { return make_string_utf8(s, strlen(s)); }

Obj substring(Obj o, size_t start, size_t end) {
  o = str_resolve(o);
  if (!IS_TYPE(o, T_WSTRING)) return make_string(NSTR_DATA(o) + start, end - start);
  int wide = 0;
  for (size_t i = start; i < end; i++) if (WSTR_DATA(o)[i] > 255) { wide = 1; break; }
  if (!wide) { Obj s = make_string(NULL, end - start); for (size_t i = start; i < end; i++) NSTR_DATA(s)[i - start] = (char)WSTR_DATA(o)[i]; return s; }
  PROTECT(o);
  Obj w = make_wstring(end - start);
  nroots--;
  memcpy(WSTR_DATA(w), WSTR_DATA(o) + start, (end - start) * 4);
  return w;
}

/* A narrow copy of string O (or O itself), or NIL if it has a character above 255. */
Obj str_narrow(Obj o) {
  o = str_resolve(o);
  if (!IS_TYPE(o, T_WSTRING)) return o;
  size_t n = WSTR_LEN(o);
  for (size_t i = 0; i < n; i++) if (WSTR_DATA(o)[i] > 255) return NIL;
  return substring(o, 0, n);
}

Obj need_narrow(Obj o) {
  if (!STRINGP(o)) type_error("STRING", o);
  Obj n = str_narrow(o);
  if (n == NIL) lisp_error("This operation needs a string of characters below 256");
  return n;
}

/* ---- UTF-8 out ------------------------------------------------------------ */

void buf_put_cp(Buf *b, int c) {
  char t[4]; size_t n;
  unsigned u = (unsigned)c;
  if (u < 0x80) { t[0] = (char)u; n = 1; }
  else if (u < 0x800) { t[0] = (char)(0xC0 | (u >> 6)); t[1] = (char)(0x80 | (u & 0x3F)); n = 2; }
  else if (u < 0x10000) { t[0] = (char)(0xE0 | (u >> 12)); t[1] = (char)(0x80 | ((u >> 6) & 0x3F)); t[2] = (char)(0x80 | (u & 0x3F)); n = 3; }
  else { t[0] = (char)(0xF0 | (u >> 18)); t[1] = (char)(0x80 | ((u >> 12) & 0x3F)); t[2] = (char)(0x80 | ((u >> 6) & 0x3F)); t[3] = (char)(0x80 | (u & 0x3F)); n = 4; }
  buf_put(b, t, n);
}

void buf_put_str(Buf *b, Obj s) {
  s = str_resolve(s);
  if (IS_TYPE(s, T_STRING)) {
    const unsigned char *p = (const unsigned char *)NSTR_DATA(s); size_t n = NSTR_LEN(s), i = 0;
    while (i < n) {                                   /* runs of ASCII in one copy */
      size_t j = i; while (j < n && p[j] < 0x80) j++;
      if (j > i) buf_put(b, (const char *)p + i, j - i);
      if (j < n) buf_put_cp(b, p[j++]);
      i = j;
    }
  } else {
    for (size_t i = 0; i < WSTR_LEN(s); i++) buf_put_cp(b, (int)WSTR_DATA(s)[i]);
  }
}

/* The UTF-8 text of string O, NUL-terminated.  A narrow ASCII string gives its own bytes; otherwise the text is
   in one of 16 rotating buffers, valid until 16 more calls.  *LEN (if given) gets its length in bytes. */
const char *str_c(Obj o, size_t *len) {
  o = str_resolve(o);
  if (IS_TYPE(o, T_STRING)) {
    const unsigned char *p = (const unsigned char *)NSTR_DATA(o); size_t n = NSTR_LEN(o), i = 0;
    while (i < n && p[i] < 0x80) i++;
    if (i == n) { if (len) *len = n; return (const char *)p; }
  }
  static Buf ring[16]; static int next;
  Buf *b = &ring[next]; next = (next + 1) % 16;
  b->len = 0;
  buf_put_str(b, o);
  buf_putc(b, 0); b->len--;                          /* (also allocates the buffer for an empty string) */
  if (len) *len = b->len;
  return b->buf;
}

/* ---- comparing and hashing --------------------------------------------------- */

int str_equal(Obj a, Obj b) {
  a = str_resolve(a); b = str_resolve(b);
  size_t n = str_len(a);
  if (n != str_len(b)) return 0;
  if (IS_TYPE(a, T_STRING) && IS_TYPE(b, T_STRING)) return memcmp(NSTR_DATA(a), NSTR_DATA(b), n) == 0;
  for (size_t i = 0; i < n; i++) if (str_ref(a, i) != str_ref(b, i)) return 0;
  return 1;
}

int str_equal_c(Obj a, const char *s, size_t n) {
  a = str_resolve(a);
  if (IS_TYPE(a, T_STRING)) {                        /* an ASCII C string against a narrow string: compare bytes */
    int ascii = 1; for (size_t i = 0; i < n; i++) if ((unsigned char)s[i] >= 0x80) { ascii = 0; break; }
    if (ascii) return NSTR_LEN(a) == n && memcmp(NSTR_DATA(a), s, n) == 0;
  }
  const unsigned char *u = (const unsigned char *)s; size_t i = 0, k = 0, len = str_len(a);
  while (i < n) { if (k >= len) return 0; if (utf8_decode(u, n, &i) != str_ref(a, k++)) return 0; }
  return k == len;
}

uint64_t str_hash(Obj o) {
  o = str_resolve(o);
  uint64_t h = 1469598103934665603ULL;
  size_t n = str_len(o);
  if (IS_TYPE(o, T_STRING)) {
    const unsigned char *p = (const unsigned char *)NSTR_DATA(o);
    for (size_t i = 0; i < n; i++) { h ^= p[i]; h *= 1099511628211ULL; }
  } else {
    for (size_t i = 0; i < n; i++) {
      uint32_t c = WSTR_DATA(o)[i];
      if (c < 256) { h ^= c; h *= 1099511628211ULL; }
      else for (int k = 0; k < 4; k++) { h ^= (c >> (8 * k)) & 0xFF; h *= 1099511628211ULL; h ^= 0xA5; }
    }
  }
  return h;
}

uint64_t str_hash_c(const char *s, size_t n) {
  /* the hash STR_HASH gives the string MAKE_STRING_UTF8 would make of S */
  const unsigned char *u = (const unsigned char *)s; size_t i = 0;
  uint64_t h = 1469598103934665603ULL;
  while (i < n) {
    int c = u[i] < 0x80 ? u[i++] : utf8_decode(u, n, &i);
    if (c < 256) { h ^= (uint64_t)c; h *= 1099511628211ULL; }
    else for (int k = 0; k < 4; k++) { h ^= ((unsigned)c >> (8 * k)) & 0xFF; h *= 1099511628211ULL; h ^= 0xA5; }
  }
  return h;
}

/* ---- case ---------------------------------------------------------------------- */
/* The same mapping as lisp/core/63-unicode.lisp: ASCII, Latin-1, Latin Extended-A and Additional, Greek, Cyrillic,
   Armenian and fullwidth Latin. */
int cp_upcase(int c) {
  if (c >= 'a' && c <= 'z') return c - 32;
  if (c < 128) return c;
  if (c >= 0xE0 && c <= 0xFE) return c == 0xF7 ? c : c - 32;
  if (c == 0xFF) return 0x178;
  if (c == 0xB5) return 0x39C;
  if (c < 0x100) return c;
  if (c <= 0x17F) {
    if (c == 0x131) return 'I'; if (c == 0x17F) return 'S';
    if ((c <= 0x137) || (c >= 0x14A && c <= 0x177)) return (c & 1) ? c - 1 : c;
    if ((c >= 0x139 && c <= 0x148) || (c >= 0x179 && c <= 0x17E)) return (c & 1) ? c : c - 1;
    return c;
  }
  if (c >= 0x3B1 && c <= 0x3C9) return c == 0x3C2 ? 0x3A3 : c - 32;
  if (c >= 0x3CA && c <= 0x3CB) return c - 32;
  if (c == 0x3AC) return 0x386; if (c >= 0x3AD && c <= 0x3AF) return c - 37;
  if (c == 0x3CC) return 0x38C; if (c >= 0x3CD && c <= 0x3CE) return c - 63;
  if (c >= 0x430 && c <= 0x44F) return c - 32;
  if (c >= 0x450 && c <= 0x45F) return c - 80;
  if ((c >= 0x460 && c <= 0x481) || (c >= 0x48A && c <= 0x4BF) || (c >= 0x4D0 && c <= 0x52F)) return (c & 1) ? c - 1 : c;
  if (c >= 0x4C1 && c <= 0x4CE) return (c & 1) ? c : c - 1;
  if (c == 0x4CF) return 0x4C0;
  if (c >= 0x561 && c <= 0x586) return c - 48;
  if (c >= 0x1E00 && c <= 0x1EFF) return ((c & 1) && !(c >= 0x1E96 && c <= 0x1E9F)) ? c - 1 : c;
  if (c >= 0xFF41 && c <= 0xFF5A) return c - 32;
  return c;
}

int cp_downcase(int c) {
  if (c >= 'A' && c <= 'Z') return c + 32;
  if (c < 128) return c;
  if (c >= 0xC0 && c <= 0xDE) return c == 0xD7 ? c : c + 32;
  if (c < 0x100) return c;
  if (c <= 0x17F) {
    if (c == 0x130) return 'i'; if (c == 0x178) return 0xFF;
    if ((c <= 0x137) || (c >= 0x14A && c <= 0x177)) return (c & 1) ? c : c + 1;
    if ((c >= 0x139 && c <= 0x148) || (c >= 0x179 && c <= 0x17E)) return (c & 1) ? c + 1 : c;
    return c;
  }
  if (c >= 0x391 && c <= 0x3A9) return c == 0x3A2 ? c : c + 32;
  if (c >= 0x3AA && c <= 0x3AB) return c + 32;
  if (c == 0x386) return 0x3AC; if (c >= 0x388 && c <= 0x38A) return c + 37;
  if (c == 0x38C) return 0x3CC; if (c >= 0x38E && c <= 0x38F) return c + 63;
  if (c >= 0x410 && c <= 0x42F) return c + 32;
  if (c >= 0x400 && c <= 0x40F) return c + 80;
  if ((c >= 0x460 && c <= 0x481) || (c >= 0x48A && c <= 0x4BF) || (c >= 0x4D0 && c <= 0x52F)) return (c & 1) ? c : c + 1;
  if (c >= 0x4C1 && c <= 0x4CE) return (c & 1) ? c + 1 : c;
  if (c == 0x4C0) return 0x4CF;
  if (c >= 0x531 && c <= 0x556) return c + 48;
  if (c >= 0x1E00 && c <= 0x1EFF) return (!(c & 1) && !(c >= 0x1E96 && c <= 0x1E9F)) ? c + 1 : c;
  if (c >= 0xFF21 && c <= 0xFF3A) return c + 32;
  return c;
}
