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
  if (CHARP(o)) { uint32_t c = (uint32_t)CHAR_CODE(o); return make_string_codes(&c, 1); }
  type_error("STRING-DESIGNATOR", o);
}

/* ---- characters -------------------------------------------------------- */
static int need_char(Obj o) { if (!CHARP(o)) type_error("CHARACTER", o); return CHAR_CODE(o); }
PRIM(p_char_code) { UNUSED_ARGS; return FIX(need_char(argv[0])); }
PRIM(p_code_char) { UNUSED_ARGS; SObj c = need_fix(argv[0]); if (c < 0 || c > 0x10FFFF) return NIL; return MK_CHAR((int)c); }
PRIM(p_char_upcase) { UNUSED_ARGS; return MK_CHAR(cp_upcase(need_char(argv[0]))); }
PRIM(p_char_downcase) { UNUSED_ARGS; return MK_CHAR(cp_downcase(need_char(argv[0]))); }
/* letters: ASCII ones, and above 191 anything with a case or outside the punctuation and symbol blocks */
static int cp_alpha(int c) {
  if (c < 128) return isalpha(c);
  if (c == 0xD7 || c == 0xF7 || c < 0xC0) return 0;
  if (cp_upcase(c) != c || cp_downcase(c) != c) return 1;
  return !((c >= 0x2000 && c <= 0x2BFF) || (c >= 0x3000 && c <= 0x303F) || (c >= 0xFE10 && c <= 0xFE6F) || (c >= 0xFF00 && c <= 0xFF20) || (c >= 0x1F000 && c <= 0x1FAFF));
}
PRIM(p_alpha_char_p) { UNUSED_ARGS; return BOOL(cp_alpha(need_char(argv[0]))); }
PRIM(p_alphanumericp) { UNUSED_ARGS; int c = need_char(argv[0]); return BOOL((c < 128 && isdigit(c)) || cp_alpha(c)); }
PRIM(p_upper_case_p) { UNUSED_ARGS; int c = need_char(argv[0]); return BOOL(cp_downcase(c) != c); }
PRIM(p_lower_case_p) { UNUSED_ARGS; int c = need_char(argv[0]); return BOOL(cp_upcase(c) != c); }
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
  size_t la = str_len(a), lb = str_len(b), n = la < lb ? la : lb;
  for (size_t i = 0; i < n; i++) {
    int x = str_ref(a, i), y = str_ref(b, i);
    if (ci) { x = cp_downcase(x); y = cp_downcase(y); }
    if (x != y) return x < y ? -1 : 1;
  }
  return la < lb ? -1 : la > lb;
}
PRIM(p_string_compare) { UNUSED_ARGS; return FIX(cmp_strings(str_designator(argv[0]), str_designator(argv[1]), argv[2] != NIL)); }
PRIM(p_string_eq) { UNUSED_ARGS; return BOOL(cmp_strings(str_designator(argv[0]), str_designator(argv[1]), 0) == 0); }
static Obj map_case(Obj s, int up) {
  size_t n = str_len(s);
  Obj r = substring(s, 0, n);
  for (size_t i = 0; i < n; i++) { int c = str_ref(r, i), d = up ? cp_upcase(c) : cp_downcase(c); if (d != c) str_set(r, i, d); }
  return r;
}
PRIM(p_string_upcase) { UNUSED_ARGS; return map_case(str_designator(argv[0]), 1); }
PRIM(p_string_downcase) { UNUSED_ARGS; return map_case(str_designator(argv[0]), 0); }
PRIM(p_string_concat) {
  size_t total = 0; int wide = 0;
  for (int i = 0; i < argc; i++) { Obj s = str_designator(argv[i]); total += str_len(s); if (str_wide_p(s)) wide = 1; }
  if (!wide) {
    Obj r = make_string(NULL, total);
    size_t pos = 0;
    for (int i = 0; i < argc; i++) { Obj s = str_resolve(str_designator(argv[i])); memcpy(NSTR_DATA(r) + pos, NSTR_DATA(s), NSTR_LEN(s)); pos += NSTR_LEN(s); }
    return r;
  }
  Obj r = make_wstring(total);
  size_t pos = 0;
  for (int i = 0; i < argc; i++) { Obj s = str_designator(argv[i]); size_t n = str_len(s); for (size_t k = 0; k < n; k++) WSTR_DATA(r)[pos++] = (uint32_t)str_ref(s, k); }
  return str_narrow(r) != NIL ? str_narrow(r) : r;
}
PRIM(p_make_string) {
  SObj n = need_fix(argv[0]);
  if (n < 0) lisp_error("MAKE-STRING: negative length");
  int c = (argc > 1 && CHARP(argv[1])) ? CHAR_CODE(argv[1]) : 0;
  if (c > 255) { Obj w = make_wstring((size_t)n); for (SObj i = 0; i < n; i++) WSTR_DATA(w)[i] = (uint32_t)c; return w; }
  Obj s = make_string(NULL, (size_t)n);
  if (c) memset(NSTR_DATA(s), c, (size_t)n);
  return s;
}
PRIM(p_string_search) {
  /* (%string-search needle haystack start) -> index or NIL */
  UNUSED_ARGS;
  Obj n = str_designator(argv[0]), h = str_designator(argv[1]);
  SObj start = need_fix(argv[2]);
  if (start < 0) start = 0;
  size_t ln = str_len(n), lh = str_len(h);
  if (ln == 0) return (size_t)start <= lh ? FIX(start) : NIL;
  n = str_resolve(n); h = str_resolve(h);
  if (IS_TYPE(n, T_STRING) && IS_TYPE(h, T_STRING)) {
    for (size_t i = (size_t)start; i + ln <= lh; i++) if (memcmp(NSTR_DATA(h) + i, NSTR_DATA(n), ln) == 0) return FIX(i);
    return NIL;
  }
  int first = str_ref(n, 0);
  for (size_t i = (size_t)start; i + ln <= lh; i++) {
    if (str_ref(h, i) != first) continue;
    size_t k = 1; while (k < ln && str_ref(h, i + k) == str_ref(n, k)) k++;
    if (k == ln) return FIX(i);
  }
  return NIL;
}
PRIM(p_string_position_char) {
  UNUSED_ARGS;
  Obj s = need_str(argv[0]); int c = need_char(argv[1]); SObj start = need_fix(argv[2]);
  size_t n = str_len(s);
  for (size_t i = (size_t)(start < 0 ? 0 : start); i < n; i++) if (str_ref(s, i) == c) return FIX(i);
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
  if (STRINGP(x)) { for (size_t i = str_len(x); i > 0; i--) l = cons(MK_CHAR(str_ref(x, i - 1)), l); return l; }
  if (VECTORP(x)) { for (size_t i = OBJ_NWORDS(x); i > 0; i--) l = cons(SLOT(x, i - 1), l); return l; }
  if (IS_TYPE(x, T_ADVEC)) {
    Obj d = SLOT(x, AV_DATA); long f = (long)UNFIX(SLOT(x, AV_FILL));
    for (long i = f; i > 0; i--) l = cons(STRINGP(d) ? MK_CHAR(str_ref(d, (size_t)i - 1)) : SLOT(d, i - 1), l);
    return l;
  }
  if (LISTP(x)) return x;
  type_error("SEQUENCE", x);
}
/* the row-major index of the subscripts argv[1 .. 1+n) in the multi-dimensional array X */
static SObj md_index(Obj x, Obj *subs, int n) {
  Obj dims = SLOT(x, AR_DIMS);
  if (list_length(dims) != n) lisp_error("Wrong number of subscripts: %d for an array of rank %ld", n, list_length(dims));
  SObj idx = 0;
  for (int k = 0; k < n; k++, dims = CDR(dims)) {
    SObj d = UNFIX(CAR(dims)), s = need_fix(subs[k]);
    if (s < 0 || s >= d) lisp_error("Subscript %ld out of bounds for dimension %d (size %ld)", (long)s, k, (long)d);
    idx = idx * d + s;
  }
  return idx;
}
static Obj md_get(Obj data, SObj i) { return STRINGP(data) ? MK_CHAR(str_ref(data, (size_t)i)) : SLOT(data, i); }
static void md_set(Obj data, SObj i, Obj v) { if (STRINGP(data)) str_set(data, (size_t)i, need_char(v)); else { SLOT(data, i) = v; WB(data, v); } }

PRIM(p_aref) {
  Obj x = argv[0];
  if (IS_TYPE(x, T_MDARRAY)) return md_get(SLOT(x, AR_DATA), md_index(x, argv + 1, argc - 1));
  if (argc != 2) lisp_error("Wrong number of subscripts: %d for a vector", argc - 1);
  SObj i = need_fix(argv[1]);
  if (VECTORP(x)) { if (i < 0 || (size_t)i >= OBJ_NWORDS(x)) lisp_error("Index %ld out of bounds for vector of length %zu", (long)i, OBJ_NWORDS(x)); return SLOT(x, i); }
  if (STRINGP(x)) { size_t n = str_len(x); if (i < 0 || (size_t)i >= n) lisp_error("Index %ld out of bounds for string of length %zu", (long)i, n); return MK_CHAR(str_ref(x, (size_t)i)); }
  if (IS_TYPE(x, T_ADVEC)) {
    Obj d = SLOT(x, AV_DATA); long f = (long)UNFIX(SLOT(x, AV_FILL));
    if (i < 0 || i >= f) lisp_error("Index %ld out of bounds (fill pointer %ld)", (long)i, f);
    return STRINGP(d) ? MK_CHAR(str_ref(d, (size_t)i)) : SLOT(d, i);
  }
  type_error("ARRAY", x);
}
PRIM(p_aset) {
  /* (%aset array subscript... value) */
  Obj x = argv[0], v = argv[argc - 1];
  if (IS_TYPE(x, T_MDARRAY)) { md_set(SLOT(x, AR_DATA), md_index(x, argv + 1, argc - 2), v); return v; }
  if (argc != 3) lisp_error("Wrong number of subscripts: %d for a vector", argc - 2);
  SObj i = need_fix(argv[1]);
  if (VECTORP(x)) { if (i < 0 || (size_t)i >= OBJ_NWORDS(x)) lisp_error("Index %ld out of bounds", (long)i); SLOT(x, i) = v; WB(x, v); return v; }
  if (STRINGP(x)) { if (i < 0 || (size_t)i >= str_len(x)) lisp_error("Index %ld out of bounds", (long)i); str_set(x, (size_t)i, need_char(v)); return v; }
  if (IS_TYPE(x, T_ADVEC)) {
    Obj d = SLOT(x, AV_DATA); long f = (long)UNFIX(SLOT(x, AV_FILL));
    if (i < 0 || i >= f) lisp_error("Index %ld out of bounds", (long)i);
    if (STRINGP(d)) str_set(d, (size_t)i, need_char(v)); else { SLOT(d, i) = v; WB(d, v); }
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
  size_t cap = STRINGP(d) ? str_len(d) : OBJ_NWORDS(d);
  if ((size_t)f >= cap) {
    size_t ncap = cap * 2;
    Obj nd = STRINGP(d) ? (str_wide_p(d) ? make_wstring(ncap) : make_string(NULL, ncap)) : make_vector(ncap, NIL);
    d = SLOT(v, AV_DATA);
    if (STRINGP(d)) for (long i = 0; i < f; i++) str_set(nd, (size_t)i, str_ref(d, (size_t)i));
    else for (long i = 0; i < f; i++) SLOT(nd, i) = SLOT(d, i);
    SLOT(v, AV_DATA) = nd; WB(v, nd); d = nd;
  }
  if (STRINGP(d)) str_set(d, (size_t)f, need_char(argv[0])); else { SLOT(d, f) = argv[0]; WB(d, argv[0]); }
  SLOT(v, AV_FILL) = FIX(f + 1);
  return FIX(f);
}
PRIM(p_fill_pointer) { UNUSED_ARGS; if (!IS_TYPE(argv[0], T_ADVEC)) return NIL; return SLOT(argv[0], AV_FILL); }
PRIM(p_set_fill_pointer) { UNUSED_ARGS; if (!IS_TYPE(argv[0], T_ADVEC)) type_error("ADJUSTABLE-VECTOR", argv[0]); SLOT(argv[0], AV_FILL) = FIX(need_fix(argv[1])); return argv[1]; }
PRIM(p_advecp) { UNUSED_ARGS; return BOOL(IS_TYPE(argv[0], T_ADVEC)); }
PRIM(p_make_mdarray) {
  /* (%make-md-array data dims) */
  UNUSED_ARGS;
  Obj a = make_obj(T_MDARRAY, AR_SLOTS);
  SLOT(a, AR_DATA) = argv[0]; SLOT(a, AR_DIMS) = argv[1];
  return a;
}
PRIM(p_mdarrayp) { UNUSED_ARGS; return BOOL(IS_TYPE(argv[0], T_MDARRAY)); }
PRIM(p_md_data) { UNUSED_ARGS; if (!IS_TYPE(argv[0], T_MDARRAY)) type_error("ARRAY", argv[0]); return SLOT(argv[0], AR_DATA); }
PRIM(p_md_dims) { UNUSED_ARGS; if (!IS_TYPE(argv[0], T_MDARRAY)) type_error("ARRAY", argv[0]); return SLOT(argv[0], AR_DIMS); }
/* ---- weak objects (see T_WEAK in neogenera.h) ---- */
static Obj need_weak(Obj o) { if (!IS_TYPE(o, T_WEAK)) type_error("WEAK-POINTER", o); return o; }
PRIM(p_make_weak) {
  /* (%make-weak mode key value) */
  UNUSED_ARGS;
  long mode = need_fix(argv[0]);
  if (mode < WKM_POINTER || mode > WKM_FINAL) lisp_error("bad weak object mode %ld", mode);
  Obj w = make_obj(T_WEAK, WK_SLOTS);
  SLOT(w, WK_MODE) = argv[0]; SLOT(w, WK_KEY) = argv[1]; SLOT(w, WK_VALUE) = argv[2];
  return w;
}
PRIM(p_weakp) { UNUSED_ARGS; return BOOL(IS_TYPE(argv[0], T_WEAK)); }
PRIM(p_weak_mode) { UNUSED_ARGS; return SLOT(need_weak(argv[0]), WK_MODE); }
PRIM(p_weak_key) { UNUSED_ARGS; return SLOT(need_weak(argv[0]), WK_KEY); }
PRIM(p_weak_value) { UNUSED_ARGS; return SLOT(need_weak(argv[0]), WK_VALUE); }
PRIM(p_weak_set_value) {
  UNUSED_ARGS;
  Obj w = need_weak(argv[0]);
  if (SLOT(w, WK_MODE) == FIX(WKM_BROKEN)) return argv[1];
  SLOT(w, WK_VALUE) = argv[1]; WB(w, argv[1]);
  return argv[1];
}
PRIM(p_weak_break) {
  /* (%weak-break w): break W now (a cancelled finalizer, a removed table entry) */
  UNUSED_ARGS;
  Obj w = need_weak(argv[0]);
  SLOT(w, WK_MODE) = FIX(WKM_BROKEN); SLOT(w, WK_KEY) = NIL; SLOT(w, WK_VALUE) = NIL;
  return NIL;
}
PRIM(p_finalizers_pending) {
  /* (%finalizers-pending &optional clear) -> whether a finalizer fired since the flag was last cleared */
  int was = finalizers_pending;
  if (argc > 0 && argv[0] != NIL) finalizers_pending = 0;
  return BOOL(was);
}

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
  if (STRINGP(x)) len = (long)str_len(x); else if (VECTORP(x)) len = (long)OBJ_NWORDS(x);
  else if (IS_TYPE(x, T_ADVEC)) len = (long)UNFIX(SLOT(x, AV_FILL)); else type_error("SEQUENCE", x);
  SObj e = (argc > 2 && argv[2] != NIL) ? need_fix(argv[2]) : len;
  if (s < 0 || e > len || s > e) lisp_error("SUBSEQ: bad bounds %ld %ld for length %ld", (long)s, (long)e, len);
  if (STRINGP(x)) return substring(x, (size_t)s, (size_t)e);
  Obj src = IS_TYPE(x, T_ADVEC) ? SLOT(x, AV_DATA) : x;
  if (STRINGP(src)) return substring(src, (size_t)s, (size_t)e);
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
  defprim("AREF", p_aref, 1, -1); defprim("SVREF", p_aref, 2, 2); defprim("CHAR", p_aref, 2, 2); defprim("SCHAR", p_aref, 2, 2);
  defprim("%ASET", p_aset, 2, -1);
  defprim("%MAKE-WEAK", p_make_weak, 3, 3); defprim("%WEAKP", p_weakp, 1, 1);
  defprim("%WEAK-MODE", p_weak_mode, 1, 1); defprim("%WEAK-KEY", p_weak_key, 1, 1);
  defprim("%WEAK-VALUE", p_weak_value, 1, 1); defprim("%WEAK-SET-VALUE", p_weak_set_value, 2, 2);
  defprim("%WEAK-BREAK", p_weak_break, 1, 1); defprim("%FINALIZERS-PENDING", p_finalizers_pending, 0, 1);
  defprim("%MAKE-MD-ARRAY", p_make_mdarray, 2, 2); defprim("%MD-ARRAY-P", p_mdarrayp, 1, 1);
  defprim("%MD-DATA", p_md_data, 1, 1); defprim("%MD-DIMS", p_md_dims, 1, 1);
  defprim("%MAKE-ADJUSTABLE", p_make_adjustable, 2, 2); defprim("VECTOR-PUSH-EXTEND", p_vector_push_extend, 2, 3);
  defprim("FILL-POINTER", p_fill_pointer, 1, 1); defprim("%SET-FILL-POINTER", p_set_fill_pointer, 2, 2);
  defprim("%ADVEC-P", p_advecp, 1, 1);
  defprim("SUBSEQ", p_subseq, 2, 3);
}
