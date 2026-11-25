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
  SLOT(s, ST_BUF) = make_string(NULL, 64);     /* STR_LEN == capacity */
  return s;
}

Obj make_string_in_stream(Obj str) {
  Obj s = make_stream(SK_STR_IN, -1);
  SLOT(s, ST_BUF) = str;
  SLOT(s, ST_LEN) = FIX(STR_LEN(str));
  return s;
}

Obj stream_output_string(Obj s) {
  return make_string(STR_DATA(SLOT(s, ST_BUF)), (size_t)UNFIX(SLOT(s, ST_LEN)));
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

static void update_col(Obj s, const char *buf, size_t n) {
  if (!n) return;
  long col = (long)UNFIX(SLOT(s, ST_COL));
  for (size_t i = 0; i < n; i++) { if (buf[i] == '\n') col = 0; else col++; }
  SLOT(s, ST_COL) = FIX(col);
}

void stream_write(Obj s, const char *buf, size_t n) {
  switch (kind_of(s)) {
  case SK_STDOUT: case SK_STDERR: case SK_FILE_OUT:
    fwrite(buf, 1, n, files[UNFIX(SLOT(s, ST_FILE))]);
    break;
  case SK_STR_OUT: {
    size_t len = (size_t)UNFIX(SLOT(s, ST_LEN));
    Obj b = SLOT(s, ST_BUF);
    if (len + n + 1 > STR_LEN(b)) {
      size_t cap = STR_LEN(b) * 2;
      while (cap < len + n + 1) cap *= 2;
      Obj nb = make_string(NULL, cap);
      memcpy(STR_DATA(nb), STR_DATA(SLOT(s, ST_BUF)), len);
      SLOT(s, ST_BUF) = nb; b = nb;
    }
    memcpy(STR_DATA(b) + len, buf, n);
    SLOT(s, ST_LEN) = FIX(len + n);
    break;
  }
  default: lisp_error("stream is not an output stream");
  }
  update_col(s, buf, n);
}

void stream_putc(Obj s, int c) {
  if (c < 128) { char ch = (char)c; stream_write(s, &ch, 1); return; }
  char b[4]; int n;
  if (c < 0x800) { b[0] = (char)(0xC0 | (c >> 6)); b[1] = (char)(0x80 | (c & 0x3F)); n = 2; }
  else if (c < 0x10000) { b[0] = (char)(0xE0 | (c >> 12)); b[1] = (char)(0x80 | ((c >> 6) & 0x3F)); b[2] = (char)(0x80 | (c & 0x3F)); n = 3; }
  else { b[0] = (char)(0xF0 | (c >> 18)); b[1] = (char)(0x80 | ((c >> 12) & 0x3F)); b[2] = (char)(0x80 | ((c >> 6) & 0x3F)); b[3] = (char)(0x80 | (c & 0x3F)); n = 4; }
  stream_write(s, b, (size_t)n);
}

int stream_getc(Obj s) {
  switch (kind_of(s)) {
  case SK_STDIN: case SK_FILE_IN: {
    long pk = (long)UNFIX(SLOT(s, ST_PEEK));
    if (pk >= 0) { SLOT(s, ST_PEEK) = FIX(-1); return (int)pk; }
    if (kind_of(s) == SK_STDIN) fflush(stdout);
    int c = fgetc(files[UNFIX(SLOT(s, ST_FILE))]);
    return c;
  }
  case SK_STR_IN: {
    long pos = (long)UNFIX(SLOT(s, ST_POS)), len = (long)UNFIX(SLOT(s, ST_LEN));
    if (pos >= len) return -1;
    SLOT(s, ST_POS) = FIX(pos + 1);
    return (unsigned char)STR_DATA(SLOT(s, ST_BUF))[pos];
  }
  default: lisp_error("stream is not an input stream");
  }
}

void stream_ungetc(Obj s, int c) {
  if (c < 0) return;
  if (kind_of(s) == SK_STR_IN) SLOT(s, ST_POS) = FIX(UNFIX(SLOT(s, ST_POS)) - 1);
  else SLOT(s, ST_PEEK) = FIX(c);
}

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
