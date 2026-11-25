/* neogenera.h -- shared declarations for the NeoGenera C kernel.
 *
 * See docs/design/object-model.md for the representation rules implemented
 * here.  Every Lisp value is a machine word (Obj) whose low 3 bits are a tag.
 */
#ifndef NEOGENERA_H
#define NEOGENERA_H

#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>
#include <stdarg.h>

typedef uintptr_t Obj;
typedef intptr_t SObj;

/* ---- tags ------------------------------------------------------------- */
#define TAG_MASK 7
#define TAG_FIX  0   /* fixnum: value << 3                                  */
#define TAG_CONS 1   /* pointer to a 2-word cons cell (in the cons space)   */
#define TAG_OBJ  2   /* pointer to a header-prefixed object                 */
#define TAG_IMM  3   /* immediate: character or special marker              */
#define TAG_HDR  7   /* appears only in header words, never in values       */

#define FIX(n)     ((Obj)((SObj)(n) * 8))
#define UNFIX(o)   ((SObj)(o) >> 3)
#define FIXP(o)    (((o) & TAG_MASK) == TAG_FIX)
#define FIX_MAX    (((SObj)1 << 60) - 1)
#define FIX_MIN    (-((SObj)1 << 60))
#define FIX_FITS(n) ((n) >= FIX_MIN && (n) <= FIX_MAX)

#define IMM_CHAR 0
#define IMM_SPEC 1
#define MK_IMM(k, p)  (((Obj)(p) << 8) | ((Obj)(k) << 3) | TAG_IMM)
#define MK_CHAR(c)    MK_IMM(IMM_CHAR, (c))
#define CHARP(o)      (((o) & 0xff) == TAG_IMM)
#define CHAR_CODE(o)  ((int)((o) >> 8))
#define UNBOUND       MK_IMM(IMM_SPEC, 0)
#define EOF_OBJ       MK_IMM(IMM_SPEC, 1)
#define UNSUPPLIED    MK_IMM(IMM_SPEC, 2)
#define FWD_MARK      MK_IMM(IMM_SPEC, 3)

/* ---- heap objects ----------------------------------------------------- */
enum {
  T_SYMBOL = 1, T_STRING, T_VECTOR, T_FLONUM, T_EVALFN, T_CLOSURE, T_CODE,
  T_PRIM, T_STREAM, T_PACKAGE, T_STRUCT, T_ADVEC
};

#define HDR(n, t)      (((Obj)(n) << 16) | ((Obj)(t) << 8) | TAG_HDR)
#define PTR(o)         ((Obj *)((o) & ~(Obj)TAG_MASK))
#define OBJP(o)        (((o) & TAG_MASK) == TAG_OBJ)
#define OBJ_TYPE(o)    ((int)((PTR(o)[0] >> 8) & 0xff))
#define OBJ_NWORDS(o)  ((size_t)(PTR(o)[0] >> 16))
#define IS_TYPE(o, t)  (OBJP(o) && OBJ_TYPE(o) == (t))
#define MK_OBJ(p)      ((Obj)(p) | TAG_OBJ)
#define SLOT(o, i)     (PTR(o)[1 + (i)])

#define CONSP(o)  (((o) & TAG_MASK) == TAG_CONS)
#define CAR(o)    (((Obj *)((o) - TAG_CONS))[0])
#define CDR(o)    (((Obj *)((o) - TAG_CONS))[1])

extern Obj NIL;   /* the symbol NIL (statically allocated, never moves) */
extern Obj TT;    /* the symbol T */

#define SYMBOLP(o)  IS_TYPE(o, T_SYMBOL)
#define STRINGP(o)  IS_TYPE(o, T_STRING)
#define VECTORP(o)  IS_TYPE(o, T_VECTOR)
#define FLONUMP(o)  IS_TYPE(o, T_FLONUM)
#define LISTP(o)    ((o) == NIL || CONSP(o))
#define BOOL(x)     ((x) ? TT : NIL)

/* symbols */
enum { S_NAME, S_VALUE, S_FUNC, S_PLIST, S_PKG, S_FLAGS, SYM_SLOTS };
#define SYM_NAME(s)   SLOT(s, S_NAME)
#define SYM_VALUE(s)  SLOT(s, S_VALUE)
#define SYM_FUNC(s)   SLOT(s, S_FUNC)
#define SYM_PLIST(s)  SLOT(s, S_PLIST)
#define SYM_PKG(s)    SLOT(s, S_PKG)
#define SYM_FLAGS(s)  SLOT(s, S_FLAGS)
#define SF_SPECIAL  1
#define SF_CONSTANT 2
#define SYM_SFID(s)   ((int)(UNFIX(SYM_FLAGS(s)) >> 8))

/* strings: payload[0] = byte length (raw), bytes follow, NUL terminated */
#define STR_LEN(o)   ((size_t)SLOT(o, 0))
#define STR_DATA(o)  ((char *)&SLOT(o, 1))

/* evaluator closures */
enum { EF_NAME, EF_LAMBDA, EF_BODY, EF_VARS, EF_FUNS, EF_SIMPLE, EF_SLOTS };
/* bytecode code objects */
enum { C_NAME, C_NREQ, C_NOPT, C_REST, C_INSTRS, C_CONSTS, C_ARGLIST, C_SLOTS };
/* primitives */
enum { PR_INDEX, PR_NAME, PR_MIN, PR_MAX, PR_SLOTS };
/* streams */
enum { ST_KIND, ST_FILE, ST_BUF, ST_LEN, ST_POS, ST_COL, ST_PEEK, ST_SLOTS };
enum { SK_STDIN, SK_STDOUT, SK_STDERR, SK_FILE_IN, SK_FILE_OUT, SK_STR_IN, SK_STR_OUT, SK_CLOSED };
/* packages */
enum { PK_NAME, PK_NICK, PK_INTERNAL, PK_EXTERNAL, PK_USES, PK_USEDBY, PK_SLOTS };
/* adjustable vectors */
enum { AV_DATA, AV_FILL, AV_SLOTS };

/* ---- memory ----------------------------------------------------------- */
void  init_memory(void);
Obj   cons(Obj a, Obj d);
Obj   make_obj(int type, size_t nslots);        /* slots initialised to NIL */
Obj   make_raw(int type, size_t nwords);        /* zeroed raw payload       */
Obj   make_string(const char *s, size_t len);
Obj   make_cstring(const char *s);
Obj   make_flonum(double d);
Obj   make_vector(size_t n, Obj init);
Obj   make_symbol_obj(Obj name);                /* uninterned symbol        */
double flonum_value(Obj o);
void  add_root(Obj *p);
void  gc(void);
void  gc_check(void);                           /* safe point               */
extern int    gc_pending, gc_stress, gc_verbose;
extern size_t gc_count;
extern int    heap_fixed;
size_t heap_bytes_used(void);
int   save_image(const char *path);
int   load_image(const char *path);

/* root stack for C locals */
#define MAX_ROOTS 65536
extern Obj *root_stack[MAX_ROOTS];
extern int  nroots;
#define PROTECT(v)   (root_stack[nroots++] = &(v))
#define UNPROTECT(n) (nroots -= (n))

/* ---- symbols & packages ----------------------------------------------- */
void  init_symbols(void);
Obj   intern(const char *name, Obj pkg);        /* exact-case name          */
Obj   intern_cl(const char *name);              /* upcases, interns in CL   */
Obj   intern_kw(const char *name);              /* upcases, in KEYWORD      */
Obj   intern_obj(Obj namestr, Obj pkg, int *status);
Obj   find_symbol_in(Obj namestr, Obj pkg, int *status);
Obj   find_package(Obj designator);
Obj   find_package_c(const char *name);
Obj   make_package(Obj name, Obj nicks, Obj uses);
void  export_symbol(Obj sym, Obj pkg);
void  use_package(Obj pkg, Obj used);
Obj   current_package(void);
extern Obj PKG_CL, PKG_KEYWORD, PKG_USER, all_packages;
extern int export_all_cl;                       /* boot flag                */
Obj   fn_symbol(Obj name);                      /* (setf x) -> symbol       */

#define SYMBOL_LIST(X) \
  X(quote,"QUOTE") X(function,"FUNCTION") X(lambda,"LAMBDA") X(if_,"IF") \
  X(progn,"PROGN") X(let,"LET") X(letstar,"LET*") X(setq,"SETQ") \
  X(flet,"FLET") X(labels,"LABELS") X(macrolet,"MACROLET") X(block,"BLOCK") \
  X(return_from,"RETURN-FROM") X(tagbody,"TAGBODY") X(go,"GO") \
  X(catch,"CATCH") X(unwind_protect,"UNWIND-PROTECT") \
  X(mv_call,"MULTIPLE-VALUE-CALL") X(mv_prog1,"MULTIPLE-VALUE-PROG1") \
  X(defun,"DEFUN") X(defmacro,"DEFMACRO") X(defvar,"DEFVAR") \
  X(defparameter,"DEFPARAMETER") X(defconstant,"DEFCONSTANT") \
  X(declare,"DECLARE") X(eval_when,"EVAL-WHEN") X(the,"THE") \
  X(locally,"LOCALLY") X(and_,"AND") X(or_,"OR") X(cond,"COND") \
  X(when,"WHEN") X(unless,"UNLESS") X(progv,"PROGV") X(ltv,"LOAD-TIME-VALUE") \
  X(special,"SPECIAL") X(optional,"&OPTIONAL") X(rest,"&REST") X(body,"&BODY") \
  X(key,"&KEY") X(aux,"&AUX") X(allow,"&ALLOW-OTHER-KEYS") X(whole,"&WHOLE") \
  X(environment,"&ENVIRONMENT") X(list,"LIST") X(cons,"CONS") X(append,"APPEND") \
  X(list_to_vector,"%LIST-TO-VECTOR") X(quasiquote,"QUASIQUOTE") \
  X(unquote,"UNQUOTE") X(unquote_splicing,"UNQUOTE-SPLICING") \
  X(macro_marker,"%MACRO") X(setf,"SETF") X(execute,":EXECUTE") \
  X(package,"*PACKAGE*") X(stdout,"*STANDARD-OUTPUT*") X(stdin,"*STANDARD-INPUT*") \
  X(stderr,"*ERROR-OUTPUT*") X(features,"*FEATURES*") X(signal_error,"%SIGNAL-ERROR") \
  X(sf_eval,"EVAL") X(print_pkg,"*PRINT-PACKAGES*") X(read_eval,"*READ-EVAL*") \
  X(in_signal,"*IN-ERROR-SIGNAL*") X(struct_parent,"%STRUCT-PARENT")
#define X(id, str) extern Obj Q_##id;
SYMBOL_LIST(X)
#undef X

enum {
  SFID_NONE = 0, SFID_QUOTE, SFID_FUNCTION, SFID_LAMBDA, SFID_IF, SFID_PROGN,
  SFID_LET, SFID_LETSTAR, SFID_SETQ, SFID_FLET, SFID_LABELS, SFID_MACROLET,
  SFID_BLOCK, SFID_RETURN_FROM, SFID_TAGBODY, SFID_GO, SFID_CATCH,
  SFID_UNWIND_PROTECT, SFID_MV_CALL, SFID_MV_PROG1, SFID_DEFUN, SFID_DEFMACRO,
  SFID_DEFVAR, SFID_DEFPARAMETER, SFID_DEFCONSTANT, SFID_DECLARE,
  SFID_EVAL_WHEN, SFID_THE, SFID_LOCALLY, SFID_AND, SFID_OR, SFID_COND,
  SFID_WHEN, SFID_UNLESS, SFID_PROGV, SFID_LTV
};

/* ---- list helpers ----------------------------------------------------- */
Obj   list1(Obj a);
Obj   list2(Obj a, Obj b);
Obj   list3(Obj a, Obj b, Obj c);
Obj   car_of(Obj o);                           /* checked */
Obj   cdr_of(Obj o);
long  list_length(Obj l);                      /* -1 if improper/circular */
Obj   nreverse_list(Obj l);
Obj   plist_get(Obj plist, Obj key, Obj dflt);
Obj   plist_put(Obj plist, Obj key, Obj val);
int   eql(Obj a, Obj b);
int   equal(Obj a, Obj b);
int   equalp(Obj a, Obj b);
uint64_t sxhash(Obj o, int depth);

/* ---- errors, dynamic state, non-local exit ---------------------------- */
void  lisp_error(const char *fmt, ...) __attribute__((noreturn, format(printf, 1, 2)));
void  type_error(const char *what, Obj got) __attribute__((noreturn));

#define VSTACK_SIZE (8 * 1024 * 1024)
#define SPEC_SIZE   (1024 * 1024)
#define VFRAME_SIZE (1024 * 1024)
extern Obj vstack[VSTACK_SIZE];
extern int vsp;
typedef struct { Obj sym; Obj old; } SpecBind;
extern SpecBind specpdl[SPEC_SIZE];
extern int spsp;
typedef struct { Obj closure; int pc; int base; } VFrame;
extern VFrame vframes[VFRAME_SIZE];
extern int vfp;

#define MAX_MV 256
extern Obj mv_vals[MAX_MV];
extern int mv_count;

#define PUSH(x) do { if (vsp >= VSTACK_SIZE - 8) lisp_error("value stack exhausted"); vstack[vsp++] = (x); } while (0)

void  dynbind(Obj sym, Obj val);
void  unbind_to(int height);

enum { F_TOP, F_CATCH, F_BLOCK, F_TAGBODY, F_UNWIND };
typedef struct Frame {
  struct Frame *prev;
  int kind;
  Obj tag, value, cleanup, mvlist;
  int spsp, vsp, vfp, nroots, eval_depth;
  jmp_buf jb;
} Frame;
extern Frame *frame_top;
extern int eval_depth;
void  frame_push(Frame *f, int kind, Obj tag);
void  frame_pop(Frame *f);
void  throw_to(Frame *target, Obj value) __attribute__((noreturn));
void  throw_values(Frame *target, Obj value) __attribute__((noreturn)); /* carries MV state */
Obj   frame_result(Frame *f);                  /* value (and MV) after a catch */
Frame *find_frame(int kind, Obj tag);

/* ---- evaluator, application, VM --------------------------------------- */
Obj   eval(Obj form, Obj vars, Obj funs);
Obj   apply_stack(int n);                      /* stack: fn a1..an -> result */
Obj   call0(Obj fn);
Obj   call1(Obj fn, Obj a);
Obj   call2(Obj fn, Obj a, Obj b);
Obj   call3(Obj fn, Obj a, Obj b, Obj c);
Obj   apply_list(Obj fn, Obj args);
Obj   resolve_function(Obj f);
Obj   vm_apply_closure(int n);                 /* closure on stack, run it */
void  init_eval(void);
void  init_vm(void);
Obj   num_add(Obj a, Obj b);
Obj   num_sub(Obj a, Obj b);
Obj   num_mul(Obj a, Obj b);
int   num_cmp(Obj a, Obj b);                   /* -1 0 1                    */
int   is_number(Obj o);
double num_to_double(Obj o);
enum {
  OP_CONST, OP_LREF, OP_LSET, OP_FREF, OP_GREF, OP_GSET, OP_GFUN, OP_POP,
  OP_DUP, OP_JMP, OP_JNIL, OP_JNNIL, OP_CALL, OP_TAILCALL, OP_RET, OP_CLOSURE,
  OP_BOX, OP_UNBOX, OP_BOXSET, OP_BIND, OP_UNBIND, OP_SLIDE, OP_CAR, OP_CDR,
  OP_CONS, OP_EQ, OP_NOT, OP_ADD, OP_SUB, OP_LT, OP_GT, OP_LE, OP_GE, OP_NUMEQ,
  OP_COUNT
};

/* ---- primitives ------------------------------------------------------- */
typedef Obj (*PrimFn)(int argc, Obj *argv);
typedef struct { const char *name; PrimFn fn; int min, max, mv; Obj obj; } PrimEntry;
#define MAX_PRIMS 1024
extern PrimEntry prim_table[MAX_PRIMS];
extern int nprims;
Obj   defprim(const char *name, PrimFn fn, int min, int max);
Obj   defprim_mv(const char *name, PrimFn fn, int min, int max);
void  init_prims_core(void);
void  init_prims_data(void);
void  init_prims_io(void);
Obj   values_list(Obj list);
#define PRIM(name) static Obj name(int argc, Obj *argv)
#define UNUSED_ARGS (void)argc; (void)argv

/* ---- streams, reader, printer ----------------------------------------- */
extern Obj stream_stdin, stream_stdout, stream_stderr;
Obj   make_stream(int kind, int file);
Obj   make_string_in_stream(Obj str);
Obj   make_string_out_stream(void);
Obj   stream_output_string(Obj s);
Obj   designate_out(Obj s);
Obj   designate_in(Obj s);
void  stream_write(Obj s, const char *buf, size_t n);
void  stream_putc(Obj s, int c);
int   stream_getc(Obj s);
int   stream_peekc(Obj s);
void  stream_ungetc(Obj s, int c);
void  stream_flush(Obj s);
int   stream_close(Obj s);
Obj   open_file_stream(const char *path, int output, int append);
void  init_streams(void);

Obj   read_form(Obj stream, int *eof);          /* sets *eof=1 at EOF       */
Obj   read_from_cstring(const char *s);

typedef struct { char *buf; size_t len, cap; } Buf;
void  buf_init(Buf *b);
void  buf_free(Buf *b);
void  buf_put(Buf *b, const char *s, size_t n);
void  buf_puts(Buf *b, const char *s);
void  buf_putc(Buf *b, int c);
void  print_to_buf(Buf *b, Obj o, int escape, int readable);
Obj   print_to_string(Obj o, int escape, int readable);
void  print_obj(Obj stream, Obj o, int escape);
void  format_double(Buf *b, double d);

/* ---- repl ------------------------------------------------------------- */
int   run_toplevel(const char *file);
void  print_error_message(Obj msg);
int   load_file(const char *path, int verbose);
extern int  quit_requested;
extern int  quit_code;

#endif
