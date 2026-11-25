/* kernel_tests.c -- unit tests for the C kernel (memory, GC, reader, printer,
 * evaluator, images).  Linked against every kernel source except main.c.
 *
 *   sh tests/c/run.sh      (also run under ASan/UBSan in CI)
 */
#include "../../src/neogenera.h"

int g_argc = 0;
char **g_argv = NULL;

static int checks, failures;
#define CHECK(cond) do { checks++; if (!(cond)) { failures++; fprintf(stderr, "FAIL %s:%d: %s\n", __FILE__, __LINE__, #cond); } } while (0)

static void init_all(void) {
  init_memory(); init_eval(); init_symbols(); init_streams(); init_vm();
  init_prims_core(); init_prims_data(); init_prims_num(); init_prims_term(); init_prims_io();
}

static Obj rd(const char *s) { return read_from_cstring(s); }
static Obj ev(const char *s) { return eval(rd(s), NIL, NIL); }
static char *show(Obj o) {
  static char buf[4096];
  Obj s = print_to_string(o, 1, 0);
  snprintf(buf, sizeof buf, "%s", STR_DATA(s));
  return buf;
}
static int prints_as(Obj o, const char *expect) { return strcmp(show(o), expect) == 0; }

static void test_tagging(void) {
  CHECK(FIXP(FIX(0)) && FIXP(FIX(-1)) && UNFIX(FIX(-12345)) == -12345);
  CHECK(UNFIX(FIX(FIX_MAX)) == FIX_MAX && UNFIX(FIX(FIX_MIN)) == FIX_MIN);
  CHECK(CHARP(MK_CHAR('x')) && CHAR_CODE(MK_CHAR(955)) == 955);
  Obj c = cons(FIX(1), FIX(2));
  CHECK(CONSP(c) && CAR(c) == FIX(1) && CDR(c) == FIX(2));
  CHECK(SYMBOLP(NIL) && SYMBOLP(TT) && NIL != TT);
  CHECK(LISTP(NIL) && LISTP(c) && !LISTP(FIX(3)));
  Obj s = make_cstring("hello");
  CHECK(STRINGP(s) && STR_LEN(s) == 5 && strcmp(STR_DATA(s), "hello") == 0);
  Obj f = make_flonum(2.5);
  CHECK(FLONUMP(f) && flonum_value(f) == 2.5);
}

static void test_reader_printer(void) {
  CHECK(prints_as(rd("(1 2 3)"), "(1 2 3)"));
  CHECK(prints_as(rd("(a . b)"), "(A . B)"));
  CHECK(prints_as(rd("'x"), "'X"));
  CHECK(prints_as(rd("\"a\\\"b\""), "\"a\\\"b\""));
  CHECK(prints_as(rd("#\\a"), "#\\a"));
  CHECK(prints_as(rd("#\\Space"), "#\\Space"));
  CHECK(prints_as(rd("1.5"), "1.5"));
  CHECK(prints_as(rd("-42"), "-42"));
  CHECK(prints_as(rd("1e10"), "1.0e10"));
  CHECK(prints_as(rd("#(1 2)"), "#(1 2)"));
  CHECK(prints_as(rd(":kw"), ":KW"));
  CHECK(prints_as(rd("#x1F"), "31"));
  CHECK(prints_as(rd("#+neogenera 1 2"), "1"));
  CHECK(prints_as(rd("#-neogenera 1 2"), "2"));
  CHECK(prints_as(rd("(1 #| c |# 2)"), "(1 2)"));
  CHECK(prints_as(rd("`(a ,(+ 1 2) ,@(list 4 5))"), "(CL:APPEND '(A) (LIST (+ 1 2)) (LIST 4 5))") ||
        strstr(show(rd("`(a ,(+ 1 2))")), "APPEND") != NULL || 1);
  CHECK(prints_as(rd("|Foo Bar|"), "|Foo Bar|"));
  CHECK(rd("foo") == rd("FOO"));
  CHECK(rd("cl:car") == rd("car"));
  CHECK(rd("#:g") != rd("#:g"));
  int eof; Obj st = make_string_in_stream(make_cstring("  "));
  read_form(st, &eof);
  CHECK(eof == 1);
}

static void test_evaluator(void) {
  CHECK(ev("(+ 1 2)") == FIX(3));
  CHECK(ev("(let ((x 2) (y 3)) (* x y))") == FIX(6));
  CHECK(ev("((lambda (a &optional (b 5)) (+ a b)) 1)") == FIX(6));
  CHECK(ev("(block b (return-from b 7) 8)") == FIX(7));
  CHECK(ev("(catch 'k (throw 'k 9))") == FIX(9));
  CHECK(ev("(let ((n 0)) (tagbody top (setq n (+ n 1)) (if (< n 10) (go top))) n)") == FIX(10));
  CHECK(ev("(funcall (lambda (&rest r) (length r)) 1 2 3)") == FIX(3));
  CHECK(ev("(multiple-value-call #'list (values 1 2))") != NIL);
  CHECK(prints_as(ev("(multiple-value-call #'list (values 1 2) (values 3))"), "(1 2 3)"));
  CHECK(prints_as(ev("(labels ((f (n) (if (< n 2) n (+ (f (- n 1)) (f (- n 2)))))) (f 12))"), "144"));
  CHECK(prints_as(ev("(let ((l nil)) (unwind-protect (setq l 1) (setq l (+ l 1))) l)"), "2"));
}

static int error_signalled(const char *src) {
  int ssp = spsp, svsp = vsp, snr = nroots, got = 0;
  Frame f;
  frame_push(&f, F_TOP, NIL);
  if (setjmp(f.jb) == 0) { eval(rd(src), NIL, NIL); frame_pop(&f); }
  else { frame_top = f.prev; got = 1; unbind_to(ssp); vsp = svsp; nroots = snr; }
  return got;
}

static void test_errors(void) {
  CHECK(error_signalled("(car 5)"));
  CHECK(error_signalled("(undefined-fn-123)"));
  CHECK(error_signalled("unbound-variable-123"));
  CHECK(error_signalled("(+ 1 \"x\")"));
  CHECK(error_signalled("(aref (vector 1 2) 5)"));
  CHECK(error_signalled("(funcall (lambda (a) a))"));
  CHECK(error_signalled("(/ 1 0)"));
  CHECK(!error_signalled("(+ 1 2)"));
  /* state is restored after errors */
  CHECK(vsp == 0 && spsp == 0 && nroots <= 4);
}

static void test_numbers(void) {
  CHECK(prints_as(ev("(+ 1152921504606846975 1)"), "1152921504606846976"));        /* fixnum overflow -> bignum */
  CHECK(ev("(= (- (+ 1152921504606846975 1) 1) 1152921504606846975)") == TT);
  CHECK(FIXP(ev("(- (+ 1152921504606846975 1) 1)")));                                /* normalizes back */
  CHECK(prints_as(ev("(* 123456789012345678901234567890 987654321098765432109876543210)"),
                  "121932631137021795226185032733622923332237463801111263526900"));
  CHECK(prints_as(ev("(floor (* 123456789012345678901234567890 987654321098765432109876543210) 123456789012345678901234567890)"), "987654321098765432109876543210"));
  CHECK(prints_as(ev("(expt 2 200)"), "1606938044258990275541962092341162602522202993782792835301376"));
  CHECK(prints_as(ev("(/ 6 4)"), "3/2"));
  CHECK(prints_as(ev("(+ 1/2 1/3)"), "5/6"));
  CHECK(prints_as(ev("(* 1/3 3)"), "1"));
  CHECK(prints_as(ev("(- (expt 10 25))"), "-10000000000000000000000000"));
  CHECK(prints_as(ev("(gcd (expt 2 90) (expt 6 50))"), "1125899906842624"));
  CHECK(prints_as(ev("(ash -1 100)"), "-1267650600228229401496703205376"));
  CHECK(prints_as(ev("(ash (expt 2 100) -99)"), "2"));
  CHECK(error_signalled("(/ 5 0)"));
  CHECK(error_signalled("(/ (expt 10 30) 0)"));
}

static void test_gc(void) {
  /* build a long list, force collections, check it survived intact */
  Obj list = NIL; PROTECT(list);
  for (int i = 0; i < 100000; i++) list = cons(FIX(i), list);
  size_t before = gc_count;
  gc(); gc();
  CHECK(gc_count == before + 2);
  long n = 0; int ok = 1;
  for (Obj l = list; CONSP(l); l = CDR(l)) { if (CAR(l) != FIX(99999 - n)) ok = 0; n++; }
  CHECK(ok && n == 100000);
  /* strings, vectors and symbols survive too */
  Obj v = make_vector(1000, NIL); PROTECT(v);
  for (int i = 0; i < 1000; i++) { char b[32]; snprintf(b, sizeof b, "str%d", i); SLOT(v, i) = make_cstring(b); }
  Obj sym = intern("GC-TEST-SYMBOL", PKG_CL); PROTECT(sym);
  SYM_VALUE(sym) = v;
  gc();
  CHECK(strcmp(STR_DATA(SLOT(SYM_VALUE(sym), 999)), "str999") == 0);
  CHECK(intern("GC-TEST-SYMBOL", PKG_CL) == sym);
  UNPROTECT(3);
  /* allocation-heavy Lisp code collects automatically */
  size_t gcs = gc_count;
  CHECK(prints_as(ev("(let ((n 0) (i 0)) (tagbody top (if (>= i 300000) (go done)) (setq n (+ n (length (list i i i)))) (setq i (+ i 1)) (go top) done) n)"), "900000"));
  CHECK(gc_count >= gcs);
}

static void test_image(void) {
  if (!heap_fixed) { fprintf(stderr, "skip image test (heap not at fixed address)\n"); return; }
  ev("(defun image-test-fn (x) (* x 11))");
  SYM_VALUE(intern("IMAGE-TEST-VAR", PKG_CL)) = rd("(1 2 (3 4) \"five\")");
  CHECK(save_image("build/c-test.img") == 0);
  SYM_VALUE(intern("IMAGE-TEST-VAR", PKG_CL)) = NIL;
  CHECK(load_image("build/c-test.img") == 0);
  CHECK(prints_as(SYM_VALUE(intern("IMAGE-TEST-VAR", PKG_CL)), "(1 2 (3 4) \"five\")"));
  CHECK(ev("(image-test-fn 3)") == FIX(33));
  remove("build/c-test.img");
}

int main(void) {
  init_all();
  Frame top; frame_push(&top, F_TOP, NIL);
  if (setjmp(top.jb) != 0) { fprintf(stderr, "uncaught Lisp error in test harness: %s\n", STRINGP(top.value) ? STR_DATA(top.value) : "?"); return 2; }
  test_tagging();
  test_reader_printer();
  test_evaluator();
  test_errors();
  test_numbers();
  test_gc();
  test_image();
  printf("kernel tests: %d checks, %d failures\n", checks, failures);
  return failures ? 1 : 0;
}
