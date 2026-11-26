/* stream.c -- minimal stream layer: stdio, files and string streams. */
#include "neogenera.h"

Obj stream_stdin, stream_stdout, stream_stderr;

#define MAX_FILES 64
static FILE *files[MAX_FILES];

Obj make_stream(int kind, int file) {
  Obj s = make_obj(T_STREAM, ST_SLOTS);
  SLOT(s, ST_KIND) = FIX(kind);
  SLOT(s, ST_FILE) = FIX(file);
  SLOT(s, ST_LEN) = FIX(0);
  SLOT(s, ST_POS) = FIX(0);
  SLOT(s, ST_COL) = FIX(0);
  SLOT(s, ST_PEEK) = FIX(-1);
  return s;
}

void init_streams(void) {
  files[0] = stdin; files[1] = stdout; files[2] = stderr;
  add_root(&stream_stdin); add_root(&stream_stdout); add_root(&stream_stderr);
  stream_stdin = make_stream(SK_STDIN, 0);
  stream_stdout = make_stream(SK_STDOUT, 1);
  stream_stderr = make_stream(SK_STDERR, 2);
  SYM_VALUE(Q_stdin) = stream_stdin;
  SYM_VALUE(Q_stdout) = stream_stdout;
  SYM_VALUE(Q_stderr) = stream_stderr;
}

static int kind_of(Obj s) { return (int)UNFIX(SLOT(s, ST_KIND)); }

Obj make_string_out_stream(void) {
  Obj s = make_stream(SK_STR_OUT, -1);
  SLOT(s, ST_BUF) = make_string(NULL, 64);     /* its length is the capacity; it widens for a wide character */
  return s;
}

Obj make_string_in_stream(Obj str) {
  Obj s = make_stream(SK_STR_IN, -1);
  SLOT(s, ST_BUF) = str;
  SLOT(s, ST_LEN) = FIX(str_len(str));
  return s;
}

Obj stream_output_string(Obj s) {
  return substring(SLOT(s, ST_BUF), 0, (size_t)UNFIX(SLOT(s, ST_LEN)));
}

Obj open_file_stream(const char *path, int output, int append) {
  int i;
  for (i = 3; i < MAX_FILES; i++) if (!files[i]) break;
  if (i == MAX_FILES) lisp_error("too many open files");
  FILE *f = fopen(path, output ? (append ? "ab" : "wb") : "rb");
  if (!f) return NIL;
  files[i] = f;
  return make_stream(output ? SK_FILE_OUT : SK_FILE_IN, i);
}

Obj designate_out(Obj s) {
  if (s == NIL || s == TT) {
    Obj v = SYM_VALUE(Q_stdout);
    return IS_TYPE(v, T_STREAM) ? v : stream_stdout;
  }
  if (!IS_TYPE(s, T_STREAM)) type_error("OUTPUT-STREAM", s);
  return s;
}

Obj designate_in(Obj s) {
  if (s == NIL || s == TT) {
    Obj v = SYM_VALUE(Q_stdin);
    return IS_TYPE(v, T_STREAM) ? v : stream_stdin;
  }
  if (!IS_TYPE(s, T_STREAM)) type_error("INPUT-STREAM", s);
  return s;
}

/* the column after UTF-8 text BUF: characters are counted, not bytes */
static void update_col(Obj s, const char *buf, size_t n) {
  if (!n) return;
  long col = (long)UNFIX(SLOT(s, ST_COL));
  for (size_t i = 0; i < n; i++) {
    unsigned char c = (unsigned char)buf[i];
    if (c == '\n') col = 0; else if ((c & 0xC0) != 0x80) col++;
  }
  SLOT(s, ST_COL) = FIX(col);
}

/* append character C to a string output stream */
static void sout_put(Obj s, int c) {
  size_t len = (size_t)UNFIX(SLOT(s, ST_LEN));
  Obj b = SLOT(s, ST_BUF);
  int wide = str_wide_p(b);
  if (len + 1 > str_len(b) || (c > 255 && !wide)) {
    size_t cap = str_len(b);
    if (len + 1 > cap) cap *= 2;
    Obj nb = (wide || c > 255) ? make_wstring(cap) : make_string(NULL, cap);
    b = SLOT(s, ST_BUF);
    if (IS_TYPE(nb, T_WSTRING)) for (size_t i = 0; i < len; i++) WSTR_DATA(nb)[i] = (uint32_t)str_ref(b, i);
    else memcpy(NSTR_DATA(nb), NSTR_DATA(b), len);
    SLOT(s, ST_BUF) = nb; WB(s, nb); b = nb;
  }
  if (IS_TYPE(b, T_WSTRING)) WSTR_DATA(b)[len] = (uint32_t)c; else NSTR_DATA(b)[len] = (char)c;
  SLOT(s, ST_LEN) = FIX(len + 1);
  long col = (long)UNFIX(SLOT(s, ST_COL));
  SLOT(s, ST_COL) = FIX(c == '\n' ? 0 : col + 1);
}

/* write UTF-8 text */
void stream_write(Obj s, const char *buf, size_t n) {
  switch (kind_of(s)) {
  case SK_STDOUT: case SK_STDERR: case SK_FILE_OUT:
    fwrite(buf, 1, n, files[UNFIX(SLOT(s, ST_FILE))]);
    update_col(s, buf, n);
    break;
  case SK_STR_OUT: {
    const unsigned char *u = (const unsigned char *)buf; size_t i = 0;
    while (i < n) sout_put(s, u[i] < 0x80 ? u[i++] : utf8_decode(u, n, &i));
    break;
  }
  default: lisp_error("stream is not an output stream");
  }
}

void stream_putc(Obj s, int c) {
  if (kind_of(s) == SK_STR_OUT) { sout_put(s, c); return; }
  Buf b; char small[8]; b.buf = small; b.len = 0; b.cap = sizeof small;
  if (c < 128) { char ch = (char)c; stream_write(s, &ch, 1); return; }
  buf_put_cp(&b, c);
  stream_write(s, b.buf, b.len);
}

/* write the characters of string STR (from START to END) */
void stream_write_str(Obj s, Obj str, size_t start, size_t end) {
  if (kind_of(s) == SK_STR_OUT) { for (size_t i = start; i < end; i++) sout_put(s, str_ref(str, i)); return; }
  Buf b; buf_init(&b);
  for (size_t i = start; i < end; i++) buf_put_cp(&b, str_ref(str, i));
  stream_write(s, b.buf ? b.buf : "", b.len);
  buf_free(&b);
}

int stream_getc(Obj s) {
  switch (kind_of(s)) {
  case SK_STDIN: case SK_FILE_IN: {
    long pk = (long)UNFIX(SLOT(s, ST_PEEK));
    if (pk >= 0) { SLOT(s, ST_PEEK) = FIX(-1); SLOT(s, ST_POS) = FIX(UNFIX(SLOT(s, ST_POS)) + 1); return (int)pk; }
    if (kind_of(s) == SK_STDIN) fflush(stdout);
    FILE *f = files[UNFIX(SLOT(s, ST_FILE))];
    int c = fgetc(f);
    if (c < 0) return c;
    SLOT(s, ST_POS) = FIX(UNFIX(SLOT(s, ST_POS)) + 1);
    if (c < 0x80) return c;
    /* a UTF-8 sequence: decode it; a malformed byte stands for itself */
    int len = (c >> 5) == 6 ? 2 : (c >> 4) == 14 ? 3 : (c >> 3) == 30 ? 4 : 1;
    unsigned char seq[4]; seq[0] = (unsigned char)c; int k = 1;
    for (; k < len; k++) {
      int d = fgetc(f);
      if (d < 0) break;
      if ((d & 0xC0) != 0x80) { ungetc(d, f); break; }
      seq[k] = (unsigned char)d;
    }
    if (k < len) return c;              /* (the bytes read after it are lost: they were not a character) */
    size_t i = 0;
    return utf8_decode(seq, (size_t)len, &i);
  }
  case SK_STR_IN: {
    long pos = (long)UNFIX(SLOT(s, ST_POS)), len = (long)UNFIX(SLOT(s, ST_LEN));
    if (pos >= len) return -1;
    SLOT(s, ST_POS) = FIX(pos + 1);
    return str_ref(SLOT(s, ST_BUF), (size_t)pos);
  }
  default: lisp_error("stream is not an input stream");
  }
}

void stream_ungetc(Obj s, int c) {
  if (c < 0) return;
  if (kind_of(s) == SK_STR_IN) SLOT(s, ST_POS) = FIX(UNFIX(SLOT(s, ST_POS)) - 1);
  else { SLOT(s, ST_PEEK) = FIX(c); SLOT(s, ST_POS) = FIX(UNFIX(SLOT(s, ST_POS)) - 1); }
}

long stream_position(Obj s) { return (long)UNFIX(SLOT(s, ST_POS)); }

int stream_peekc(Obj s) { int c = stream_getc(s); stream_ungetc(s, c); return c; }

void stream_flush(Obj s) {
  switch (kind_of(s)) {
  case SK_STDOUT: case SK_STDERR: case SK_FILE_OUT: fflush(files[UNFIX(SLOT(s, ST_FILE))]); break;
  default: break;
  }
}

int stream_close(Obj s) {
  int k = kind_of(s);
  if (k == SK_FILE_IN || k == SK_FILE_OUT) {
    long i = (long)UNFIX(SLOT(s, ST_FILE));
    if (i >= 3 && files[i]) { fclose(files[i]); files[i] = NULL; }
    SLOT(s, ST_KIND) = FIX(SK_CLOSED);
    return 1;
  }
  return 0;
}
