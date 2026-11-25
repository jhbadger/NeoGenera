/* prims_bio.c -- bioinformatics kernels: affine-gap dynamic programming and suffix arrays.
 *
 * %DP-ALIGN fills a Gotoh three-state matrix for global, local or semi-global alignment and
 * returns the score, the end cell and a byte matrix of traceback pointers (the traceback itself is
 * done in Lisp).  %SUFFIX-ARRAY builds a suffix array by prefix doubling.
 */
#include "neogenera.h"
#include <limits.h>

static SObj need_fix(Obj o) { if (!FIXP(o)) type_error("FIXNUM", o); return UNFIX(o); }
static Obj need_str(Obj o) { if (!STRINGP(o)) type_error("STRING", o); return o; }

#define NEG (-(1 << 28))

/* Traceback byte: bits 0-1 origin of M, 2-3 origin of X (gap consuming A), 4-5 origin of Y (gap consuming B);
   origins: 0 = M, 1 = X, 2 = Y, 3 = start of the alignment. */
PRIM(p_dp_align) {
  UNUSED_ARGS;
  Obj a = need_str(argv[0]), b = need_str(argv[1]), tab = argv[2];
  int open = (int)need_fix(argv[3]), ext = (int)need_fix(argv[4]), mode = (int)need_fix(argv[5]);
  long band = (long)need_fix(argv[6]);
  size_t n = STR_LEN(a), m = STR_LEN(b);
  if (!VECTORP(tab) || OBJ_NWORDS(tab) < 65536) lisp_error("DP-ALIGN: bad score table");
  if ((n + 1) * (m + 1) > ((size_t)1 << 31)) lisp_error("DP-ALIGN: sequences too long for a full matrix (use a band)");
  int *sc = malloc(65536 * sizeof(int));
  for (int i = 0; i < 65536; i++) sc[i] = FIXP(SLOT(tab, i)) ? (int)UNFIX(SLOT(tab, i)) : -4;
  size_t w = m + 1;
  unsigned char *tr = calloc((n + 1) * w, 1);
  int *buf = malloc(6 * w * sizeof(int));
  int *Mp = buf, *Xp = buf + w, *Yp = buf + 2 * w, *Mc = buf + 3 * w, *Xc = buf + 4 * w, *Yc = buf + 5 * w;
  const unsigned char *A = (const unsigned char *)STR_DATA(a), *B = (const unsigned char *)STR_DATA(b);
  long dlo = 0, dhi = 0;
  if (band >= 0) { dlo = (m > n ? 0 : (long)m - (long)n) - band; dhi = (m > n ? (long)m - (long)n : 0) + band; }
  /* row 0 */
  for (size_t j = 0; j <= m; j++) {
    Mp[j] = NEG; Xp[j] = NEG; Yp[j] = NEG;
    if (mode == 0) {
      if (j == 0) Mp[0] = 0;
      else { Yp[j] = -(open + (int)(j - 1) * ext); tr[j] = (unsigned char)(((j == 1 ? 3 : 2) << 4)); }
    } else { Mp[j] = 0; tr[j] = 3; }
  }
  int best = INT_MIN; size_t bi = 0, bj = 0; int bstate = 0;
  if (mode == 1) { best = 0; }
  if (mode == 0 && n == 0) { /* handled below */ }
  for (size_t i = 1; i <= n; i++) {
    for (size_t j = 0; j <= m; j++) { Mc[j] = NEG; Xc[j] = NEG; Yc[j] = NEG; }
    /* column 0 */
    if (mode == 0) { Xc[0] = -(open + (int)(i - 1) * ext); tr[i * w] = (unsigned char)((i == 1 ? 3 : 1) << 2); }
    else { Mc[0] = 0; tr[i * w] = 3; }
    for (size_t j = 1; j <= m; j++) {
      if (band >= 0) { long d = (long)j - (long)i; if (d < dlo || d > dhi) continue; }
      int s = sc[(A[i - 1] << 8) | B[j - 1]];
      /* M */
      int bp = Mp[j - 1], bo = 0;
      if (Xp[j - 1] > bp) { bp = Xp[j - 1]; bo = 1; }
      if (Yp[j - 1] > bp) { bp = Yp[j - 1]; bo = 2; }
      int mval, mfrom;
      if (mode == 1) { if (bp > 0) { mval = bp + s; mfrom = bo; } else { mval = s; mfrom = 3; } if (mval <= 0) { mval = 0; mfrom = 3; } }
      else { mval = bp <= NEG / 2 ? NEG : bp + s; mfrom = bo; }
      /* X: gap in B (consumes A[i-1]) */
      int xv = Mp[j] - open, xo = 0;
      if (Xp[j] - ext > xv) { xv = Xp[j] - ext; xo = 1; }
      if (Yp[j] - open > xv) { xv = Yp[j] - open; xo = 2; }
      if (xv < NEG) xv = NEG;
      /* Y: gap in A (consumes B[j-1]) */
      int yv = Mc[j - 1] - open, yo = 0;
      if (Yc[j - 1] - ext > yv) { yv = Yc[j - 1] - ext; yo = 1 + 1; }
      if (Xc[j - 1] - open > yv) { yv = Xc[j - 1] - open; yo = 1; }
      if (yv < NEG) yv = NEG;
      Mc[j] = mval; Xc[j] = xv; Yc[j] = yv;
      tr[i * w + j] = (unsigned char)(mfrom | (xo << 2) | (yo << 4));
      if (mode == 1 && mval > best) { best = mval; bi = i; bj = j; bstate = 0; }
    }
    if (mode == 2) {      /* free end gaps: the last column competes */
      if (m > 0 && Mc[m] > best) { best = Mc[m]; bi = i; bj = m; bstate = 0; }
    }
    int *t;
    t = Mp; Mp = Mc; Mc = t; t = Xp; Xp = Xc; Xc = t; t = Yp; Yp = Yc; Yc = t;
  }
  /* after the loop Mp/Xp/Yp hold row n */
  if (mode == 0) {
    best = Mp[m]; bstate = 0; bi = n; bj = m;
    if (Xp[m] > best) { best = Xp[m]; bstate = 1; }
    if (Yp[m] > best) { best = Yp[m]; bstate = 2; }
  } else if (mode == 2) {
    for (size_t j = 0; j <= m; j++) if (Mp[j] > best) { best = Mp[j]; bi = n; bj = j; bstate = 0; }
    if (n == 0 || best == INT_MIN) { best = 0; bi = 0; bj = 0; bstate = 0; }
  } else if (mode == 1 && best == 0) { bi = 0; bj = 0; }
  Obj trs = make_string((const char *)tr, (n + 1) * w);
  Obj res = cons(FIX(best), cons(FIX((SObj)bi), cons(FIX((SObj)bj), cons(FIX(bstate), cons(trs, NIL)))));
  free(sc); free(tr); free(buf);
  return res;
}

/* ---- suffix array (prefix doubling) ---------------------------------------- */
static int *g_rank, *g_tmp; static size_t g_n, g_k;
static int cmp_suffix(const void *x, const void *y) {
  int i = *(const int *)x, j = *(const int *)y;
  if (g_rank[i] != g_rank[j]) return g_rank[i] < g_rank[j] ? -1 : 1;
  int ri = (size_t)i + g_k < g_n ? g_rank[i + g_k] : -1, rj = (size_t)j + g_k < g_n ? g_rank[j + g_k] : -1;
  return ri < rj ? -1 : ri > rj;
}
PRIM(p_suffix_array) {
  UNUSED_ARGS;
  Obj s = need_str(argv[0]);
  size_t n = STR_LEN(s);
  if (n > ((size_t)1 << 30)) lisp_error("SUFFIX-ARRAY: string too long");
  int *sa = malloc((n ? n : 1) * sizeof(int)), *rank = malloc((n ? n : 1) * sizeof(int)), *tmp = malloc((n ? n : 1) * sizeof(int));
  for (size_t i = 0; i < n; i++) { sa[i] = (int)i; rank[i] = (unsigned char)STR_DATA(s)[i]; }
  g_rank = rank; g_tmp = tmp; g_n = n;
  for (g_k = 1;; g_k <<= 1) {
    qsort(sa, n, sizeof(int), cmp_suffix);
    if (n == 0) break;
    tmp[sa[0]] = 0;
    for (size_t i = 1; i < n; i++) tmp[sa[i]] = tmp[sa[i - 1]] + (cmp_suffix(&sa[i - 1], &sa[i]) < 0);
    memcpy(rank, tmp, n * sizeof(int));
    if (rank[sa[n - 1]] == (int)n - 1) break;
    if (g_k > n) break;
  }
  Obj v = make_vector(n, NIL);
  for (size_t i = 0; i < n; i++) SLOT(v, i) = FIX(sa[i]);
  free(sa); free(rank); free(tmp);
  return v;
}

void init_prims_bio(void) {
  defprim("%DP-ALIGN", p_dp_align, 7, 7);
  defprim("%SUFFIX-ARRAY", p_suffix_array, 1, 1);
}
