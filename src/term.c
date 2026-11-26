/* term.c -- terminal backend: raw mode, UTF-8 output, key/mouse/resize input events.
 *
 * Events are returned to Lisp as lists:
 *   (:KEY code mods)                      code = code point, or a keyword (:UP :F5 ...)
 *   (:MOUSE kind col row button mods)     kind :PRESS :RELEASE :DRAG :MOVE :WHEEL-UP :WHEEL-DOWN; 0-based
 *   (:RESIZE rows cols)   (:EOF)
 * mods is a bit mask: 1 shift, 2 meta/alt, 4 control.  Mouse reporting uses the
 * SGR (1006) encoding with any-motion tracking (1003).  The input parser works on
 * an internal byte buffer, so it can be tested without a terminal (%TERM-PARSE).
 */
#include "neogenera.h"
#include <termios.h>
#include <unistd.h>
#include <poll.h>
#include <signal.h>
#include <errno.h>
#include <sys/ioctl.h>
#include <time.h>

static struct termios saved_tio;
static int term_active, atexit_set;
static volatile sig_atomic_t winch_flag;
static unsigned char ibuf[8192];
static size_t ilen;

static void on_winch(int s) { (void)s; winch_flag = 1; }

static void write_all(const char *p, size_t n) {
  while (n) {
    ssize_t w = write(1, p, n);
    if (w < 0) { if (errno == EINTR) continue; return; }
    p += w; n -= (size_t)w;
  }
}

void term_cleanup(void) {
  if (!term_active) return;
  const char *s = "\033[?1006l\033[?1003l\033[?1000l\033[0m\033[?25h\033[?1049l";
  write_all(s, strlen(s));
  tcsetattr(0, TCSAFLUSH, &saved_tio);
  term_active = 0;
}

PRIM(p_term_isatty) { UNUSED_ARGS; return BOOL(isatty(0) && isatty(1)); }

PRIM(p_term_init) {
  UNUSED_ARGS;
  if (!isatty(0) || !isatty(1)) return NIL;
  if (term_active) return TT;
  tcgetattr(0, &saved_tio);
  struct termios raw = saved_tio;
  cfmakeraw(&raw);
  raw.c_oflag |= OPOST;
  raw.c_cc[VMIN] = 1; raw.c_cc[VTIME] = 0;
  tcsetattr(0, TCSAFLUSH, &raw);
  struct sigaction sa; memset(&sa, 0, sizeof sa);
  sa.sa_handler = on_winch; sigaction(SIGWINCH, &sa, NULL);
  if (!atexit_set) { atexit(term_cleanup); atexit_set = 1; }
  const char *s = "\033[?1049h\033[?1000h\033[?1003h\033[?1006h\033[2J";
  write_all(s, strlen(s));
  term_active = 1;
  ilen = 0;
  return TT;
}

PRIM(p_term_restore) { UNUSED_ARGS; term_cleanup(); return NIL; }

static void get_size(int *rows, int *cols) {
  struct winsize ws;
  *rows = 24; *cols = 80;
  if (ioctl(1, TIOCGWINSZ, &ws) == 0 && ws.ws_row > 0 && ws.ws_col > 0) { *rows = ws.ws_row; *cols = ws.ws_col; return; }
  const char *l = getenv("LINES"), *c = getenv("COLUMNS");
  if (l && atoi(l) > 0) *rows = atoi(l);
  if (c && atoi(c) > 0) *cols = atoi(c);
}

PRIM(p_term_size) {
  UNUSED_ARGS;
  int r, c; get_size(&r, &c);
  mv_vals[0] = FIX(r); mv_vals[1] = FIX(c); mv_count = 2;
  return mv_vals[0];
}

static size_t put_utf8(unsigned char *o, long c) {
  if (c < 0x80) { o[0] = (unsigned char)c; return 1; }
  if (c < 0x800) { o[0] = (unsigned char)(0xC0 | (c >> 6)); o[1] = (unsigned char)(0x80 | (c & 0x3F)); return 2; }
  if (c < 0x10000) { o[0] = (unsigned char)(0xE0 | (c >> 12)); o[1] = (unsigned char)(0x80 | ((c >> 6) & 0x3F)); o[2] = (unsigned char)(0x80 | (c & 0x3F)); return 3; }
  o[0] = (unsigned char)(0xF0 | (c >> 18)); o[1] = (unsigned char)(0x80 | ((c >> 12) & 0x3F)); o[2] = (unsigned char)(0x80 | ((c >> 6) & 0x3F)); o[3] = (unsigned char)(0x80 | (c & 0x3F)); return 4;
}

PRIM(p_term_write_codes) {
  /* (%term-write-codes vector) -- a vector (or adjustable vector with fill pointer) of code points */
  UNUSED_ARGS;
  Obj v = argv[0]; size_t n; Obj data;
  if (IS_TYPE(v, T_VECTOR)) { data = v; n = OBJ_NWORDS(v); }
  else if (IS_TYPE(v, T_ADVEC) && VECTORP(SLOT(v, AV_DATA))) { data = SLOT(v, AV_DATA); n = (size_t)UNFIX(SLOT(v, AV_FILL)); }
  else type_error("VECTOR", v);
  unsigned char *buf = malloc(n * 4 + 1);
  size_t len = 0;
  for (size_t i = 0; i < n; i++) { Obj c = SLOT(data, i); if (FIXP(c) && UNFIX(c) >= 0 && UNFIX(c) < 0x110000) len += put_utf8(buf + len, UNFIX(c)); }
  write_all((char *)buf, len);
  free(buf);
  return FIX((SObj)len);
}

/* ---- input parsing ----------------------------------------------------- */
static Obj kw(const char *n) { return intern_kw(n); }
static Obj mk_key(long code, int mods) { return list3(kw("KEY"), FIX(code), FIX(mods)); }
static Obj mk_keysym(const char *name, int mods) { return list3(kw("KEY"), kw(name), FIX(mods)); }

static const char *tilde_key(int n) {
  switch (n) {
  case 1: case 7: return "HOME"; case 2: return "INSERT"; case 3: return "DELETE"; case 4: case 8: return "END";
  case 5: return "PGUP"; case 6: return "PGDN"; case 11: return "F1"; case 12: return "F2"; case 13: return "F3"; case 14: return "F4";
  case 15: return "F5"; case 17: return "F6"; case 18: return "F7"; case 19: return "F8"; case 20: return "F9"; case 21: return "F10";
  case 23: return "F11"; case 24: return "F12"; default: return NULL;
  }
}
static void consume(size_t n) { memmove(ibuf, ibuf + n, ilen - n); ilen -= n; }

/* returns 1 event produced (*out; NIL if the bytes were ignored), 0 need more, 2 lone ESC pending */
static int parse_one(Obj *out) {
  *out = NIL;
  if (ilen == 0) return 0;
  unsigned char b = ibuf[0];
  if (b != 0x1b) {
    int need = b >= 0xF0 ? 4 : b >= 0xE0 ? 3 : b >= 0xC0 ? 2 : 1;
    if ((size_t)need > ilen) return 0;
    long code = need == 4 ? (b & 7) : need == 3 ? (b & 15) : need == 2 ? (b & 31) : b;
    for (int i = 1; i < need; i++) code = (code << 6) | (ibuf[i] & 0x3F);
    consume((size_t)need);
    *out = mk_key(code, 0);
    return 1;
  }
  if (ilen == 1) return 2;
  unsigned char c = ibuf[1];
  /* replies the terminal sends on its own (kitty graphics APC "ESC _ G ... ESC \", DCS "ESC P > | ... ESC \"):
     swallow them so they never turn into keys */
  if ((c == '_' && ilen >= 3 && ibuf[2] == 'G') || (c == 'P' && ilen >= 3 && (ibuf[2] == '>' || ibuf[2] == '1' || ibuf[2] == '0'))) {
    for (size_t i = 2; i + 1 < ilen; i++)
      if (ibuf[i] == 0x1b && ibuf[i + 1] == '\\') { consume(i + 2); return 1; }
    return ilen >= sizeof ibuf / 2 ? (consume(ilen), 1) : 0;
  }
  if (c == '[') {
    size_t i = 2;
    while (i < ilen && !(ibuf[i] >= 0x40 && ibuf[i] <= 0x7e)) i++;
    if (i >= ilen) return 0;
    unsigned char fin = ibuf[i];
    char params[64]; size_t pl = i - 2 < 63 ? i - 2 : 63;
    memcpy(params, ibuf + 2, pl); params[pl] = 0;
    consume(i + 1);
    if (params[0] == '<' && (fin == 'M' || fin == 'm')) {
      int b2, x, y;
      if (sscanf(params + 1, "%d;%d;%d", &b2, &x, &y) == 3) {
        int btn = b2 & 3, mods = ((b2 & 4) ? 1 : 0) | ((b2 & 8) ? 2 : 0) | ((b2 & 16) ? 4 : 0);
        const char *kind; int button = btn;
        if (b2 & 64) { kind = (b2 & 1) ? "WHEEL-DOWN" : "WHEEL-UP"; button = 0; }
        else if (b2 & 32) { kind = btn == 3 ? "MOVE" : "DRAG"; }
        else kind = fin == 'M' ? "PRESS" : "RELEASE";
        *out = cons(kw("MOUSE"), cons(kw(kind), cons(FIX(x - 1), cons(FIX(y - 1), cons(FIX(button), cons(FIX(mods), NIL))))));
      }
      return 1;
    }
    int p1 = 0, p2 = 0, np = 0;
    { const char *p = params; while (*p && np < 2) { int v = 0, have = 0; while (*p >= '0' && *p <= '9') { v = v * 10 + (*p - '0'); p++; have = 1; } if (np == 0) p1 = have ? v : 0; else p2 = have ? v : 0; np++; if (*p == ';') p++; else break; } }
    int mods = p2 > 1 ? p2 - 1 : 0;
    const char *name = NULL;
    switch (fin) {
    case 'A': name = "UP"; break; case 'B': name = "DOWN"; break; case 'C': name = "RIGHT"; break; case 'D': name = "LEFT"; break;
    case 'H': name = "HOME"; break; case 'F': name = "END"; break;
    case 'P': name = "F1"; break; case 'Q': name = "F2"; break; case 'R': name = "F3"; break; case 'S': name = "F4"; break;
    case 'Z': *out = mk_key(9, 1); return 1;
    case '~': name = tilde_key(p1); break;
    default: break;
    }
    if (name) *out = mk_keysym(name, mods);
    return 1;
  }
  if (c == 'O') {
    if (ilen < 3) return 0;
    unsigned char d = ibuf[2]; const char *name = NULL;
    switch (d) { case 'A': name = "UP"; break; case 'B': name = "DOWN"; break; case 'C': name = "RIGHT"; break; case 'D': name = "LEFT"; break;
                 case 'H': name = "HOME"; break; case 'F': name = "END"; break; case 'P': name = "F1"; break; case 'Q': name = "F2"; break;
                 case 'R': name = "F3"; break; case 'S': name = "F4"; break; default: break; }
    consume(3);
    if (name) *out = mk_keysym(name, 0);
    return 1;
  }
  /* ESC <char>: Meta-char (ESC ESC is Meta-Escape) */
  int need = c >= 0xF0 ? 4 : c >= 0xE0 ? 3 : c >= 0xC0 ? 2 : 1;
  if ((size_t)(1 + need) > ilen) return 0;
  long code = need == 4 ? (c & 7) : need == 3 ? (c & 15) : need == 2 ? (c & 31) : c;
  for (int i = 1; i < need; i++) code = (code << 6) | (ibuf[1 + i] & 0x3F);
  consume((size_t)(1 + need));
  *out = mk_key(code, 2);
  return 1;
}

PRIM(p_term_parse) {
  /* (%term-parse bytes-string) -> list of events; for tests, never touches the terminal */
  UNUSED_ARGS;
  if (!STRINGP(argv[0])) type_error("STRING", argv[0]);
  ilen = 0;
  size_t n = STR_LEN(argv[0]); if (n > sizeof ibuf) n = sizeof ibuf;
  memcpy(ibuf, STR_DATA(argv[0]), n); ilen = n;
  Obj head = NIL, tail = NIL, ev;
  for (;;) {
    int r = parse_one(&ev);
    if (r == 0) break;
    if (r == 2) { consume(1); ev = mk_key(27, 0); }
    if (ev == NIL) continue;
    Obj cell = cons(ev, NIL);
    if (head == NIL) head = cell; else CDR(tail) = cell;
    tail = cell;
  }
  ilen = 0;
  return head;
}

static long now_ms(void) { struct timespec ts; clock_gettime(CLOCK_MONOTONIC, &ts); return (long)ts.tv_sec * 1000 + ts.tv_nsec / 1000000; }

PRIM(p_term_read_event) {
  /* (%term-read-event timeout-ms) -> event or NIL on timeout */
  UNUSED_ARGS;
  long timeout = FIXP(argv[0]) ? (long)UNFIX(argv[0]) : -1;
  long deadline = timeout < 0 ? -1 : now_ms() + timeout;
  for (;;) {
    if (winch_flag) { winch_flag = 0; int r, c; get_size(&r, &c); return list3(kw("RESIZE"), FIX(r), FIX(c)); }
    Obj ev;
    int r = parse_one(&ev);
    if (r == 1) { if (ev == NIL) continue; return ev; }
    long wait;
    if (r == 2) wait = 30;
    else if (deadline < 0) wait = 100;
    else { wait = deadline - now_ms(); if (wait < 0) wait = 0; if (wait > 100) wait = 100; }
    struct pollfd pf = { 0, POLLIN, 0 };
    int tok = proc_blocking_begin();                 /* other processes run while the terminal is idle */
    int pr = poll(&pf, 1, (int)wait);
    proc_blocking_end(tok);
    if (pr > 0) {
      if (ilen >= sizeof ibuf) ilen = 0;
      ssize_t n = read(0, ibuf + ilen, sizeof ibuf - ilen);
      if (n == 0) return list1(kw("EOF"));
      if (n > 0) ilen += (size_t)n;
      continue;
    }
    if (r == 2) { consume(1); return mk_key(27, 0); }
    if (deadline >= 0 && now_ms() >= deadline && !winch_flag) return NIL;
  }
}

/* ---- graphics: probing the terminal, sending images (kitty graphics protocol) -------------------------- */

static long now_ms2(void) { struct timespec ts; clock_gettime(CLOCK_MONOTONIC, &ts); return (long)ts.tv_sec * 1000 + ts.tv_nsec / 1000000; }

static const char *mem_find(const unsigned char *h, size_t hn, const char *needle) {
  size_t nn = strlen(needle);
  for (size_t i = 0; i + nn <= hn; i++) if (memcmp(h + i, needle, nn) == 0) return (const char *)h + i;
  return NULL;
}

PRIM(p_term_graphics_probe) {
  /* (%term-graphics-probe timeout-ms) -> (graphics-ok terminal-name cell-width cell-height)
     Asks the terminal (in raw mode) whether it accepts kitty graphics, what it is (XTVERSION) and its cell size
     in pixels, then waits for the primary device attributes reply, which every terminal sends, as the end
     marker.  Anything else that arrives meanwhile (keys typed) is kept for the input parser. */
  UNUSED_ARGS;
  if (!term_active) return NIL;
  long timeout = FIXP(argv[0]) ? (long)UNFIX(argv[0]) : 500;
  const char *q = "\033_Gi=31,s=1,v=1,a=q,t=d,f=24;AAAA\033\\\033[>q\033[16t\033[c";
  write_all(q, strlen(q));
  static unsigned char rb[4096]; size_t rn = 0;
  long deadline = now_ms2() + timeout;
  int done = 0;
  while (!done && rn < sizeof rb) {
    long wait = deadline - now_ms2(); if (wait <= 0) break;
    struct pollfd pf = { 0, POLLIN, 0 };
    if (poll(&pf, 1, (int)wait) <= 0) continue;
    ssize_t n = read(0, rb + rn, sizeof rb - rn);
    if (n <= 0) break;
    rn += (size_t)n;
    for (size_t i = 0; i + 2 < rn; i++)            /* DA1 reply: ESC [ ? ... c */
      if (rb[i] == 0x1b && rb[i + 1] == '[' && rb[i + 2] == '?') {
        size_t j = i + 3; while (j < rn && ((rb[j] >= '0' && rb[j] <= '9') || rb[j] == ';')) j++;
        if (j < rn && rb[j] == 'c') { done = 1; break; }
      }
  }
  int ok = mem_find(rb, rn, "\033_Gi=31;OK") != NULL;
  char name[128] = "";
  const char *v = mem_find(rb, rn, "\033P>|");
  if (v) {
    v += 4; size_t k = 0;
    while ((const unsigned char *)v < rb + rn && *v != 0x1b && k < sizeof name - 1) name[k++] = *v++;
    name[k] = 0;
  }
  int cw = 0, ch = 0;
  struct winsize ws;
  if (ioctl(1, TIOCGWINSZ, &ws) == 0 && ws.ws_col > 0 && ws.ws_row > 0 && ws.ws_xpixel > 0 && ws.ws_ypixel > 0) {
    cw = ws.ws_xpixel / ws.ws_col; ch = ws.ws_ypixel / ws.ws_row;
  } else {
    const char *t = mem_find(rb, rn, "\033[6;");
    if (t) { int h = 0, w = 0; if (sscanf(t + 4, "%d;%dt", &h, &w) == 2) { ch = h; cw = w; } }
  }
  /* keep whatever is not a reply (keys typed during the probe) for the input parser */
  for (size_t i = 0; i < rn; ) {
    if (rb[i] == 0x1b && i + 1 < rn && (rb[i + 1] == '_' || rb[i + 1] == 'P')) {     /* APC / DCS reply */
      size_t j = i + 2; while (j + 1 < rn && !(rb[j] == 0x1b && rb[j + 1] == '\\')) j++;
      i = j + 2; continue;
    }
    if (rb[i] == 0x1b && i + 2 < rn && rb[i + 1] == '[' && (rb[i + 2] == '?' || rb[i + 2] == '6')) {   /* DA1, cell size */
      size_t j = i + 2; while (j < rn && !(rb[j] >= 0x40 && rb[j] <= 0x7e && rb[j] != '?')) j++;
      if (j < rn && (rb[j] == 'c' || rb[j] == 't')) { i = j + 1; continue; }
    }
    if (ilen < sizeof ibuf) ibuf[ilen++] = rb[i];
    i++;
  }
  Obj nm = make_cstring(name);
  return cons(BOOL(ok), cons(nm, cons(FIX(cw), cons(FIX(ch), NIL))));
}

static const char b64[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

static void send_image(long id, const unsigned char *px, long w, long h, long cols, long rows, int placeholder) {
  size_t n = (size_t)(w * h * 3);
  size_t enc_len = (n + 2) / 3 * 4;
  char *enc = malloc(enc_len + 1);
  size_t k = 0;
  for (size_t i = 0; i < n; i += 3) {
    unsigned v = (unsigned)px[i] << 16 | (i + 1 < n ? (unsigned)px[i + 1] << 8 : 0) | (i + 2 < n ? px[i + 2] : 0);
    enc[k++] = b64[(v >> 18) & 63]; enc[k++] = b64[(v >> 12) & 63];
    enc[k++] = i + 1 < n ? b64[(v >> 6) & 63] : '='; enc[k++] = i + 2 < n ? b64[v & 63] : '=';
  }
  char head[256];
  const size_t chunk = 4096;
  for (size_t off = 0; off < k || off == 0; off += chunk) {
    size_t len = k - off < chunk ? k - off : chunk;
    int more = off + len < k;
    int hl;
    if (off == 0) {
      if (placeholder)
        hl = snprintf(head, sizeof head, "\033_Ga=T,U=1,q=2,f=24,i=%ld,s=%ld,v=%ld,c=%ld,r=%ld,m=%d;", id, w, h, cols, rows, more);
      else
        hl = snprintf(head, sizeof head, "\033_Ga=T,q=2,f=24,s=%ld,v=%ld,c=%ld,r=%ld,m=%d;", w, h, cols, rows, more);
    } else hl = snprintf(head, sizeof head, "\033_Gm=%d;", more);
    write_all(head, (size_t)hl);
    write_all(enc + off, len);
    write_all("\033\\", 2);
    if (!more) break;
  }
  free(enc);
}

static void need_image(Obj s, Obj w, Obj h) {
  if (!STRINGP(s)) type_error("STRING", s);
  if (!FIXP(w) || !FIXP(h) || UNFIX(w) <= 0 || UNFIX(h) <= 0) lisp_error("image size must be positive fixnums");
  if ((long)STR_LEN(s) < UNFIX(w) * UNFIX(h) * 3) lisp_error("image pixel string too short");
}

PRIM(p_term_image_send) {
  /* (%term-image-send id pixels width height cols rows): transmit an RGB image and give it a virtual placement
     of COLS x ROWS cells, for Unicode placeholder cells to show */
  UNUSED_ARGS;
  need_image(argv[1], argv[2], argv[3]);
  if (term_active) send_image((long)UNFIX(argv[0]), (const unsigned char *)STR_DATA(argv[1]), (long)UNFIX(argv[2]), (long)UNFIX(argv[3]),
                              (long)UNFIX(argv[4]), (long)UNFIX(argv[5]), 1);
  return NIL;
}

PRIM(p_term_image_inline) {
  /* (%term-image-inline pixels width height cols rows): show an image at the cursor (line mode, no window system) */
  UNUSED_ARGS;
  need_image(argv[0], argv[1], argv[2]);
  fflush(stdout);
  send_image(0, (const unsigned char *)STR_DATA(argv[0]), (long)UNFIX(argv[1]), (long)UNFIX(argv[2]), (long)UNFIX(argv[3]), (long)UNFIX(argv[4]), 0);
  return NIL;
}

PRIM(p_term_image_delete) {
  /* (%term-image-delete id) frees an image in the terminal; id 0 frees all of them */
  UNUSED_ARGS;
  char buf[64]; long id = FIXP(argv[0]) ? (long)UNFIX(argv[0]) : 0;
  int n = id ? snprintf(buf, sizeof buf, "\033_Ga=d,d=I,i=%ld,q=2\033\\", id) : snprintf(buf, sizeof buf, "\033_Ga=d,d=A,q=2\033\\");
  if (term_active || !id) write_all(buf, (size_t)n);
  return NIL;
}

void init_prims_term(void) {
  defprim("%TERM-ISATTY", p_term_isatty, 0, 0); defprim("%TERM-INIT", p_term_init, 0, 0);
  defprim("%TERM-RESTORE", p_term_restore, 0, 0); defprim_mv("%TERM-SIZE", p_term_size, 0, 0);
  defprim("%TERM-WRITE-CODES", p_term_write_codes, 1, 1); defprim("%TERM-PARSE", p_term_parse, 1, 1);
  defprim("%TERM-READ-EVENT", p_term_read_event, 1, 1);
  defprim("%TERM-GRAPHICS-PROBE", p_term_graphics_probe, 1, 1);
  defprim("%TERM-IMAGE-SEND", p_term_image_send, 6, 6);
  defprim("%TERM-IMAGE-INLINE", p_term_image_inline, 5, 5);
  defprim("%TERM-IMAGE-DELETE", p_term_image_delete, 1, 1);
}
