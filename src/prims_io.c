/* prims_io.c -- streams, printing, reading and file primitives. */
#include "neogenera.h"
#include <limits.h>
#include <unistd.h>
#include <sys/stat.h>

static SObj need_fix(Obj o) { if (!FIXP(o)) type_error("FIXNUM", o); return UNFIX(o); }
static Obj need_str(Obj o) { if (!STRINGP(o)) type_error("STRING", o); return o; }

static Obj out_arg(int argc, Obj *argv, int i) { return designate_out(argc > i ? argv[i] : NIL); }
static Obj in_arg(int argc, Obj *argv, int i) { return designate_in(argc > i ? argv[i] : NIL); }

PRIM(p_write_string) {
  Obj s = argv[0];
  if (!STRINGP(s)) {
    Obj str = IS_TYPE(s, T_ADVEC) ? av_to_string(s) : NIL;
    if (str != NIL) s = str; else type_error("STRING", s);
  }
  Obj st = out_arg(argc, argv, 1);
  size_t start = 0, end = str_len(s);
  if (argc > 2 && argv[2] != NIL) start = (size_t)need_fix(argv[2]);
  if (argc > 3 && argv[3] != NIL) end = (size_t)need_fix(argv[3]);
  if (start > end || end > str_len(s)) lisp_error("WRITE-STRING: bad bounds");
  stream_write_str(st, s, start, end);
  return argv[0];
}
PRIM(p_write_char) {
  if (!CHARP(argv[0])) type_error("CHARACTER", argv[0]);
  stream_putc(out_arg(argc, argv, 1), CHAR_CODE(argv[0]));
  return argv[0];
}
PRIM(p_terpri) { stream_putc(out_arg(argc, argv, 0), '\n'); return NIL; }
PRIM(p_fresh_line) {
  Obj st = out_arg(argc, argv, 0);
  if (UNFIX(SLOT(st, ST_COL)) != 0) { stream_putc(st, '\n'); return TT; }
  return NIL;
}
PRIM(p_prin1) { print_obj(out_arg(argc, argv, 1), argv[0], 1); return argv[0]; }
PRIM(p_princ) { print_obj(out_arg(argc, argv, 1), argv[0], 0); return argv[0]; }
PRIM(p_print) {
  Obj st = out_arg(argc, argv, 1);
  stream_putc(st, '\n'); print_obj(st, argv[0], 1); stream_putc(st, ' ');
  return argv[0];
}
PRIM(p_write_line) {
  Obj st = out_arg(argc, argv, 1);
  Obj s = need_str(argv[0]);
  stream_write_str(st, s, 0, str_len(s)); stream_putc(st, '\n');
  return argv[0];
}
PRIM(p_finish_output) { stream_flush(out_arg(argc, argv, 0)); return NIL; }
PRIM(p_prin1_to_string) { UNUSED_ARGS; return print_to_string(argv[0], 1, 0); }
PRIM(p_princ_to_string) { UNUSED_ARGS; return print_to_string(argv[0], 0, 0); }
PRIM(p_readable_to_string) { UNUSED_ARGS; return print_to_string(argv[0], 1, 1); }
PRIM(p_stream_column) { UNUSED_ARGS; return SLOT(designate_out(argv[0]), ST_COL); }

PRIM(p_read) {
  Obj st = in_arg(argc, argv, 0);
  int eof;
  Obj r = (argc > 3 && argv[3] != NIL) ? read_form_recursive(st, &eof) : read_form(st, &eof);
  if (eof) {
    if (argc > 1 && argv[1] == NIL) return argc > 2 ? argv[2] : NIL;
    lisp_error("End of file while reading");
  }
  return r;
}
PRIM(p_read_from_string) {
  /* (%read-from-string string start) -> (values object end-position) ; EOF object when empty */
  UNUSED_ARGS;
  Obj str = need_str(argv[0]);
  Obj ss = make_string_in_stream(str);
  SLOT(ss, ST_POS) = FIX(need_fix(argv[1]));
  int eof;
  Obj r = read_form(ss, &eof);
  mv_vals[0] = eof ? EOF_OBJ : r; mv_vals[1] = SLOT(ss, ST_POS); mv_count = 2;
  return mv_vals[0];
}
PRIM(p_read_char) {
  Obj st = in_arg(argc, argv, 0);
  int c = stream_getc(st);
  if (c < 0) { if (argc > 1 && argv[1] == NIL) return argc > 2 ? argv[2] : NIL; lisp_error("End of file"); }
  return MK_CHAR(c);
}
PRIM(p_peek_char) {
  /* (peek-char peek-type stream eof-error-p eof-value) */
  Obj st = in_arg(argc, argv, 1);
  int c;
  if (argc > 0 && argv[0] == TT) { while ((c = stream_getc(st)) >= 0 && reader_whitespace_p(c)) ; stream_ungetc(st, c); }
  else c = stream_peekc(st);
  c = stream_peekc(st);
  if (c < 0) { if (argc > 2 && argv[2] == NIL) return argc > 3 ? argv[3] : NIL; lisp_error("End of file"); }
  return MK_CHAR(c);
}
PRIM(p_unread_char) { stream_ungetc(in_arg(argc, argv, 1), CHAR_CODE(argv[0])); return NIL; }
PRIM(p_read_line) {
  Obj st = in_arg(argc, argv, 0);
  Buf b; buf_init(&b);
  int c, got = 0, nl = 0;
  while ((c = stream_getc(st)) >= 0) { got = 1; if (c == '\n') { nl = 1; break; } buf_put_cp(&b, c); }
  if (!got) {
    buf_free(&b);
    mv_vals[0] = (argc > 2) ? argv[2] : NIL; mv_vals[1] = TT; mv_count = 2;
    if (argc > 1 && argv[1] == NIL) return mv_vals[0];
    lisp_error("End of file");
  }
  Obj s = make_string_utf8(b.buf ? b.buf : "", b.len);
  buf_free(&b);
  mv_vals[0] = s; mv_vals[1] = BOOL(!nl); mv_count = 2;
  return s;
}
PRIM(p_make_string_output_stream) { UNUSED_ARGS; return make_string_out_stream(); }
PRIM(p_get_output_stream_string) {
  UNUSED_ARGS;
  Obj s = argv[0];
  if (!IS_TYPE(s, T_STREAM)) type_error("STRING-OUTPUT-STREAM", s);
  Obj r = stream_output_string(s);
  SLOT(s, ST_LEN) = FIX(0);
  return r;
}
PRIM(p_make_string_input_stream) {
  need_str(argv[0]);
  Obj s = make_string_in_stream(argv[0]);
  if (argc > 1 && FIXP(argv[1])) SLOT(s, ST_POS) = argv[1];
  return s;
}
PRIM(p_open) {
  /* (%open path direction if-exists) direction :input|:output|:append */
  UNUSED_ARGS;
  Obj path = need_str(argv[0]);
  const char *d = STR_C(SYM_NAME(argv[1]));
  int output = strcmp(d, "INPUT") != 0, append = strcmp(d, "APPEND") == 0;
  return open_file_stream(STR_C(path), output, append);
}
PRIM(p_close) { UNUSED_ARGS; if (IS_TYPE(argv[0], T_STREAM)) stream_close(argv[0]); return TT; }
PRIM(p_probe_file) { UNUSED_ARGS; struct stat st; return stat(STR_C(need_str(argv[0])), &st) == 0 ? argv[0] : NIL; }
PRIM(p_truename) {
  /* (%truename path) -> the absolute path with symbolic links resolved, or NIL */
  UNUSED_ARGS;
  char buf[PATH_MAX];
  if (!realpath(STR_C(need_str(argv[0])), buf)) return NIL;
  struct stat st;
  if (stat(buf, &st) == 0 && S_ISDIR(st.st_mode) && strlen(buf) + 1 < sizeof buf && buf[strlen(buf) - 1] != '/') strcat(buf, "/");
  return make_cstring(buf);
}
PRIM(p_delete_file) { UNUSED_ARGS; return BOOL(unlink(STR_C(need_str(argv[0]))) == 0); }
PRIM(p_file_write_date) { UNUSED_ARGS; struct stat st; if (stat(STR_C(need_str(argv[0])), &st) != 0) return NIL; return FIX((SObj)st.st_mtime + 2208988800LL); }
PRIM(p_load_c) {
  /* (%load-file path) -- evaluator-level load used during bootstrap */
  UNUSED_ARGS;
  return BOOL(load_file(STR_C(need_str(argv[0])), 0) == 0);
}

#define ALIAS(n, f, a, b) defprim("%C-" n, f, a, b)
void init_prims_io(void) {
  ALIAS("WRITE-STRING", p_write_string, 1, 4); ALIAS("WRITE-CHAR", p_write_char, 1, 2);
  ALIAS("TERPRI", p_terpri, 0, 1); ALIAS("FRESH-LINE", p_fresh_line, 0, 1);
  ALIAS("PRIN1", p_prin1, 1, 2); ALIAS("PRINC", p_princ, 1, 2); ALIAS("PRINT", p_print, 1, 2);
  ALIAS("WRITE-LINE", p_write_line, 1, 2); ALIAS("FINISH-OUTPUT", p_finish_output, 0, 1);
  ALIAS("READ", p_read, 0, 4); ALIAS("READ-CHAR", p_read_char, 0, 4); ALIAS("PEEK-CHAR", p_peek_char, 0, 5);
  ALIAS("UNREAD-CHAR", p_unread_char, 1, 2); defprim_mv("%C-READ-LINE", p_read_line, 0, 4);
  ALIAS("CLOSE", p_close, 1, 2); ALIAS("STREAM-COLUMN", p_stream_column, 1, 1);
  defprim("WRITE-STRING", p_write_string, 1, 4); defprim("WRITE-CHAR", p_write_char, 1, 2);
  defprim("TERPRI", p_terpri, 0, 1); defprim("FRESH-LINE", p_fresh_line, 0, 1);
  defprim("PRIN1", p_prin1, 1, 2); defprim("PRINC", p_princ, 1, 2); defprim("PRINT", p_print, 1, 2);
  defprim("WRITE-LINE", p_write_line, 1, 2);
  defprim("FINISH-OUTPUT", p_finish_output, 0, 1); defprim("FORCE-OUTPUT", p_finish_output, 0, 1);
  defprim("PRIN1-TO-STRING", p_prin1_to_string, 1, 1); defprim("PRINC-TO-STRING", p_princ_to_string, 1, 1);
  defprim("%READABLE-TO-STRING", p_readable_to_string, 1, 1);
  defprim("%STREAM-COLUMN", p_stream_column, 1, 1);
  defprim("READ", p_read, 0, 4); defprim_mv("%READ-FROM-STRING", p_read_from_string, 2, 2);
  defprim("READ-CHAR", p_read_char, 0, 4); defprim("PEEK-CHAR", p_peek_char, 0, 5);
  defprim("UNREAD-CHAR", p_unread_char, 1, 2); defprim_mv("READ-LINE", p_read_line, 0, 4);
  defprim("MAKE-STRING-OUTPUT-STREAM", p_make_string_output_stream, 0, 0);
  defprim("GET-OUTPUT-STREAM-STRING", p_get_output_stream_string, 1, 1);
  defprim("MAKE-STRING-INPUT-STREAM", p_make_string_input_stream, 1, 3);
  defprim("%OPEN", p_open, 3, 3); defprim("CLOSE", p_close, 1, 2);
  defprim("PROBE-FILE", p_probe_file, 1, 1); defprim("DELETE-FILE", p_delete_file, 1, 1);
  defprim("FILE-WRITE-DATE", p_file_write_date, 1, 1);
  /* the same, under names that stay primitive when lisp/core/64-pathnames.lisp redefines the public ones */
  ALIAS("PROBE-FILE", p_probe_file, 1, 1); ALIAS("DELETE-FILE", p_delete_file, 1, 1);
  ALIAS("FILE-WRITE-DATE", p_file_write_date, 1, 1);
  defprim("%TRUENAME", p_truename, 1, 1);
  defprim("%LOAD-FILE", p_load_c, 1, 1);
}

/* ---- evaluator-level file loading and top level ----------------------- */
int quit_requested, quit_code;

void print_error_message(Obj msg) {
  fflush(stdout);
  fprintf(stderr, "Error: %s\n", STRINGP(msg) ? STR_C(msg) : "(unknown)");
  fflush(stderr);
}

int load_file(const char *path, int verbose) {
  Obj st = open_file_stream(path, 0, 0);
  if (st == NIL) { fprintf(stderr, "Cannot open %s\n", path); return -1; }
  int ssp = spsp, svsp = vsp, snroots = nroots;
  PROTECT(st);
  Obj saved_pkg = SYM_VALUE(Q_package);
  Obj pkg_root = saved_pkg; PROTECT(pkg_root);
  int status = 0;
  Frame f;
  frame_push(&f, F_TOP, NIL);
  if (setjmp(f.jb) == 0) {
    dynbind(Q_package, SYM_VALUE(Q_package));
    for (;;) {
      int eof;
      Obj form = read_form(st, &eof);
      if (eof) break;
      eval(form, NIL, NIL);
    }
    frame_pop(&f);
  } else {
    frame_top = f.prev;
    fflush(stdout);
    fprintf(stderr, "Error while loading %s: %s\n", path, STRINGP(f.value) ? STR_C(f.value) : "?");
    status = -1;
  }
  unbind_to(ssp); vsp = svsp; nroots = snroots;
  SYM_VALUE(Q_package) = pkg_root; WB(Q_package, pkg_root);
  if (verbose) fprintf(stderr, "; loaded %s\n", path);
  stream_close(st);
  return status;
}

/* A tiny REPL used only when no Lisp-level Listener is installed. */
static int c_repl(void) {
  Obj in = stream_stdin;
  int ssp = spsp, svsp = vsp, snroots = nroots;
  for (;;) {
    Frame f;
    frame_push(&f, F_TOP, NIL);
    if (setjmp(f.jb) == 0) {
      fputs("ng> ", stdout); fflush(stdout);
      int eof;
      Obj form = read_form(in, &eof);
      if (eof) { frame_pop(&f); fputc('\n', stdout); return 0; }
      Obj r = eval(form, NIL, NIL);
      int n = mv_count; Obj vals[16]; if (n > 16) n = 16;
      for (int i = 0; i < n; i++) vals[i] = (i == 0 && mv_count == 1) ? r : mv_vals[i];
      frame_pop(&f);
      for (int i = 0; i < n; i++) { print_obj(stream_stdout, vals[i], 1); stream_putc(stream_stdout, '\n'); }
    } else {
      frame_top = f.prev;
      print_error_message(f.value);
      unbind_to(ssp); vsp = svsp; nroots = snroots;
      SYM_VALUE(Q_in_signal) = NIL;
    }
    if (quit_requested) return quit_code;
  }
}

int run_toplevel(const char *name) {
  Obj fn = SYM_FUNC(intern(name ? name : "NG-TOPLEVEL", PKG_CL));
  if (fn == NIL || CONSP(fn)) return c_repl();
  int ssp = spsp, svsp = vsp, snroots = nroots;
  for (;;) {
    Frame f;
    frame_push(&f, F_TOP, NIL);
    if (setjmp(f.jb) == 0) { call0(fn); frame_pop(&f); return 0; }
    frame_top = f.prev;
    print_error_message(f.value);
    unbind_to(ssp); vsp = svsp; nroots = snroots;
    SYM_VALUE(Q_in_signal) = NIL;
  }
}
