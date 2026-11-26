/* prims_gfx.c -- raster drawing on RGB images.
 *
 * An image is a Lisp string of WIDTH * HEIGHT * 3 bytes (red, green, blue, row by row from the top left).
 * Colours are integers whose low 24 bits are 0xRRGGBB (the RGB values of the window system, which add
 * #x1000000, work unchanged).  Lines and discs are anti-aliased by coverage: each pixel near the shape is
 * blended with the colour in proportion to how much of it the shape covers.
 */
#include "neogenera.h"
#include <math.h>

typedef struct { unsigned char *px; long w, h; } Img;

static Img need_img(Obj s, Obj w, Obj h) {
  if (!STRINGP(s)) type_error("STRING", s);
  if (!FIXP(w) || !FIXP(h) || UNFIX(w) <= 0 || UNFIX(h) <= 0) lisp_error("image size must be positive fixnums");
  if ((long)STR_LEN(s) < UNFIX(w) * UNFIX(h) * 3) lisp_error("image pixel string too short for %ldx%ld", (long)UNFIX(w), (long)UNFIX(h));
  Img im = { (unsigned char *)STR_DATA(s), (long)UNFIX(w), (long)UNFIX(h) };
  return im;
}

static long need_color(Obj c) { if (!FIXP(c)) type_error("COLOR (an integer)", c); return (long)UNFIX(c) & 0xFFFFFF; }
static double dnum(Obj o) { if (!is_number(o)) type_error("REAL", o); return num_to_double(o); }

static void blend(Img *im, long x, long y, long color, double a) {
  if (x < 0 || y < 0 || x >= im->w || y >= im->h || a <= 0) return;
  if (a > 1) a = 1;
  unsigned char *p = im->px + (y * im->w + x) * 3;
  int r = (int)(color >> 16) & 255, g = (int)(color >> 8) & 255, b = (int)color & 255;
  p[0] = (unsigned char)(p[0] + (r - p[0]) * a + 0.5);
  p[1] = (unsigned char)(p[1] + (g - p[1]) * a + 0.5);
  p[2] = (unsigned char)(p[2] + (b - p[2]) * a + 0.5);
}

PRIM(p_img_fill_rect) {
  /* (%img-fill-rect pixels w h x y rect-w rect-h color) */
  UNUSED_ARGS;
  Img im = need_img(argv[0], argv[1], argv[2]);
  long x0 = (long)floor(dnum(argv[3])), y0 = (long)floor(dnum(argv[4]));
  long x1 = x0 + (long)ceil(dnum(argv[5])), y1 = y0 + (long)ceil(dnum(argv[6]));
  long color = need_color(argv[7]);
  if (x0 < 0) x0 = 0; if (y0 < 0) y0 = 0; if (x1 > im.w) x1 = im.w; if (y1 > im.h) y1 = im.h;
  unsigned char r = (unsigned char)(color >> 16), g = (unsigned char)(color >> 8), b = (unsigned char)color;
  for (long y = y0; y < y1; y++)
    for (long x = x0; x < x1; x++) { unsigned char *p = im.px + (y * im.w + x) * 3; p[0] = r; p[1] = g; p[2] = b; }
  return NIL;
}

PRIM(p_img_line) {
  /* (%img-line pixels w h x0 y0 x1 y1 color thickness): an anti-aliased segment with round ends */
  UNUSED_ARGS;
  Img im = need_img(argv[0], argv[1], argv[2]);
  double ax = dnum(argv[3]), ay = dnum(argv[4]), bx = dnum(argv[5]), by = dnum(argv[6]);
  long color = need_color(argv[7]);
  double rad = dnum(argv[8]) / 2.0; if (rad < 0.5) rad = 0.5;
  double minx = fmin(ax, bx) - rad - 1, maxx = fmax(ax, bx) + rad + 1, miny = fmin(ay, by) - rad - 1, maxy = fmax(ay, by) + rad + 1;
  if (minx < 0) minx = 0; if (miny < 0) miny = 0; if (maxx > im.w - 1) maxx = im.w - 1; if (maxy > im.h - 1) maxy = im.h - 1;
  double dx = bx - ax, dy = by - ay, len2 = dx * dx + dy * dy;
  for (long y = (long)miny; y <= (long)maxy; y++)
    for (long x = (long)minx; x <= (long)maxx; x++) {
      double px = x + 0.5, py = y + 0.5, t = len2 > 0 ? ((px - ax) * dx + (py - ay) * dy) / len2 : 0;
      if (t < 0) t = 0; else if (t > 1) t = 1;
      double qx = ax + t * dx - px, qy = ay + t * dy - py, d = sqrt(qx * qx + qy * qy);
      blend(&im, x, y, color, rad + 0.5 - d);
    }
  return NIL;
}

PRIM(p_img_disc) {
  /* (%img-disc pixels w h cx cy radius color): an anti-aliased filled circle */
  UNUSED_ARGS;
  Img im = need_img(argv[0], argv[1], argv[2]);
  double cx = dnum(argv[3]), cy = dnum(argv[4]), r = dnum(argv[5]);
  long color = need_color(argv[6]);
  for (long y = (long)floor(cy - r - 1); y <= (long)ceil(cy + r + 1); y++)
    for (long x = (long)floor(cx - r - 1); x <= (long)ceil(cx + r + 1); x++) {
      double ddx = x + 0.5 - cx, ddy = y + 0.5 - cy;
      blend(&im, x, y, color, r + 0.5 - sqrt(ddx * ddx + ddy * ddy));
    }
  return NIL;
}

PRIM(p_img_pixel) {
  /* (%img-pixel pixels w h x y) -> the colour at X, Y as #xRRGGBB */
  UNUSED_ARGS;
  Img im = need_img(argv[0], argv[1], argv[2]);
  if (!FIXP(argv[3]) || !FIXP(argv[4])) lisp_error("pixel coordinates must be integers");
  long x = (long)UNFIX(argv[3]), y = (long)UNFIX(argv[4]);
  if (x < 0 || y < 0 || x >= im.w || y >= im.h) lisp_error("pixel %ld,%ld is outside the %ldx%ld image", x, y, im.w, im.h);
  unsigned char *p = im.px + (y * im.w + x) * 3;
  return FIX(((long)p[0] << 16) | ((long)p[1] << 8) | p[2]);
}

PRIM(p_img_cell_colors) {
  /* (%img-cell-colors pixels w h cols rows) -> vector of 2*cols*rows colours: for each cell, row by row, the
     average colour of its top half and of its bottom half (for drawing with half-block characters) */
  UNUSED_ARGS;
  Img im = need_img(argv[0], argv[1], argv[2]);
  if (!FIXP(argv[3]) || !FIXP(argv[4]) || UNFIX(argv[3]) <= 0 || UNFIX(argv[4]) <= 0) lisp_error("cell grid must be positive");
  long cols = (long)UNFIX(argv[3]), rows = (long)UNFIX(argv[4]);
  Obj v = make_vector((size_t)(2 * cols * rows), FIX(0));
  for (long r = 0; r < rows; r++)
    for (long c = 0; c < cols; c++)
      for (int half = 0; half < 2; half++) {
        long x0 = c * im.w / cols, x1 = (c + 1) * im.w / cols;
        long y0 = (2 * r + half) * im.h / (2 * rows), y1 = (2 * r + half + 1) * im.h / (2 * rows);
        if (x1 <= x0) x1 = x0 + 1; if (y1 <= y0) y1 = y0 + 1;
        if (x1 > im.w) x1 = im.w; if (y1 > im.h) y1 = im.h;
        double sr = 0, sg = 0, sb = 0; long n = 0;
        for (long y = y0; y < y1; y++)
          for (long x = x0; x < x1; x++) { unsigned char *p = im.px + (y * im.w + x) * 3; sr += p[0]; sg += p[1]; sb += p[2]; n++; }
        if (n == 0) n = 1;
        long col = ((long)(sr / n + 0.5) << 16) | ((long)(sg / n + 0.5) << 8) | (long)(sb / n + 0.5);
        SLOT(v, (size_t)(2 * (r * cols + c) + half)) = FIX(col);
      }
  return v;
}

/* ---- palette images: one byte per pixel, an index into a palette of 256 colours ----------------------------
 * Drawing with indices and then mapping them through a palette is how colour-map animation works: the shapes
 * are drawn once, and changing the palette changes their colours without drawing anything again. */

static unsigned char *need_indices(Obj s, Obj w, Obj h, long *pw, long *ph) {
  if (!STRINGP(s)) type_error("STRING", s);
  if (!FIXP(w) || !FIXP(h) || UNFIX(w) <= 0 || UNFIX(h) <= 0) lisp_error("image size must be positive fixnums");
  if ((long)STR_LEN(s) < UNFIX(w) * UNFIX(h)) lisp_error("index string too short for %ldx%ld", (long)UNFIX(w), (long)UNFIX(h));
  *pw = (long)UNFIX(w); *ph = (long)UNFIX(h);
  return (unsigned char *)STR_DATA(s);
}

PRIM(p_idx_fill) {
  /* (%idx-fill indices w h index) */
  UNUSED_ARGS;
  long w, h; unsigned char *ix = need_indices(argv[0], argv[1], argv[2], &w, &h);
  if (!FIXP(argv[3])) type_error("FIXNUM", argv[3]);
  memset(ix, (int)(UNFIX(argv[3]) & 255), (size_t)(w * h));
  return NIL;
}

PRIM(p_idx_disc) {
  /* (%idx-disc indices w h cx cy radius base n style): write palette indices BASE .. BASE+N-1 into a disc.
     STYLE 0 shades it as a sphere lit from the upper left (darkest BASE, highlight BASE+N-1); STYLE 1 numbers it
     in N rings from the centre out, so cycling those palette entries makes the rings ripple. */
  UNUSED_ARGS;
  long w, h; unsigned char *ix = need_indices(argv[0], argv[1], argv[2], &w, &h);
  double cx = dnum(argv[3]), cy = dnum(argv[4]), r = dnum(argv[5]);
  if (!FIXP(argv[6]) || !FIXP(argv[7]) || !FIXP(argv[8])) lisp_error("base, count and style must be fixnums");
  long base = (long)UNFIX(argv[6]), n = (long)UNFIX(argv[7]), style = (long)UNFIX(argv[8]);
  if (n < 1 || base < 0 || base + n > 256) lisp_error("palette indices %ld..%ld are outside 0..255", base, base + n - 1);
  if (r <= 0) return NIL;
  const double lx = -0.45, ly = -0.55, lz = 0.70;             /* unit vector towards the light */
  long y0 = (long)floor(cy - r), y1 = (long)ceil(cy + r), x0 = (long)floor(cx - r), x1 = (long)ceil(cx + r);
  if (y0 < 0) y0 = 0; if (x0 < 0) x0 = 0; if (y1 > h - 1) y1 = h - 1; if (x1 > w - 1) x1 = w - 1;
  for (long y = y0; y <= y1; y++)
    for (long x = x0; x <= x1; x++) {
      double nx = (x + 0.5 - cx) / r, ny = (y + 0.5 - cy) / r, d2 = nx * nx + ny * ny;
      if (d2 > 1) continue;
      double t;
      if (style == 1) t = sqrt(d2);
      else {
        double nz = sqrt(1 - d2), diff = nx * lx + ny * ly + nz * lz;
        if (diff < 0) diff = 0;
        double spec = pow(diff, 24);
        t = 0.08 + 0.72 * diff + 0.20 * spec;
      }
      long k = (long)(t * n); if (k < 0) k = 0; if (k > n - 1) k = n - 1;
      ix[y * w + x] = (unsigned char)(base + k);
    }
  return NIL;
}

PRIM(p_idx_map) {
  /* (%idx-map indices pixels w h palette): set each pixel of the RGB image to its index's colour in PALETTE, a
     vector of 256 colours */
  UNUSED_ARGS;
  long w, h; unsigned char *ix = need_indices(argv[0], argv[2], argv[3], &w, &h);
  Img im = need_img(argv[1], argv[2], argv[3]);
  Obj pal = argv[4];
  if (!VECTORP(pal) || OBJ_NWORDS(pal) < 256) type_error("VECTOR of 256 colours", pal);
  unsigned char lut[256 * 3];
  for (int i = 0; i < 256; i++) {
    long c = FIXP(SLOT(pal, i)) ? (long)UNFIX(SLOT(pal, i)) & 0xFFFFFF : 0;
    lut[i * 3] = (unsigned char)(c >> 16); lut[i * 3 + 1] = (unsigned char)(c >> 8); lut[i * 3 + 2] = (unsigned char)c;
  }
  for (long i = 0, n = w * h; i < n; i++) memcpy(im.px + i * 3, lut + ix[i] * 3, 3);
  return NIL;
}

#include <unistd.h>
PRIM(p_stream_terminal) {
  /* (%stream-terminal-p stream): is STREAM the process's standard output, and is that a terminal? */
  UNUSED_ARGS;
  Obj s = argv[0];
  return BOOL(IS_TYPE(s, T_STREAM) && UNFIX(SLOT(s, ST_KIND)) == SK_STDOUT && isatty(1));
}

void init_prims_gfx(void) {
  defprim("%STREAM-TERMINAL-P", p_stream_terminal, 1, 1);
  defprim("%IMG-FILL-RECT", p_img_fill_rect, 8, 8);
  defprim("%IMG-LINE", p_img_line, 9, 9);
  defprim("%IMG-DISC", p_img_disc, 7, 7);
  defprim("%IMG-PIXEL", p_img_pixel, 5, 5);
  defprim("%IMG-CELL-COLORS", p_img_cell_colors, 5, 5);
  defprim("%IDX-FILL", p_idx_fill, 4, 4);
  defprim("%IDX-DISC", p_idx_disc, 9, 9);
  defprim("%IDX-MAP", p_idx_map, 5, 5);
}
