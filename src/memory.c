/* memory.c -- allocation, copying garbage collector, heap images.
 *
 * Design (docs/design/gc.md):
 *  - Object space and cons space are separate Cheney semispaces, so the
 *    to-space can be scanned linearly (conses carry no header).
 *  - Collection happens only at *safe points* (gc_check / GC poll in the
 *    evaluator and VM), never inside an allocation.  C primitives therefore
 *    never see objects move unless they call back into Lisp.
 *  - Roots: registered globals, the value stack, the special-binding stack,
 *    the C-local root stack (PROTECT), VM frames, catch frames, MV registers.
 *  - Both semispaces live at a fixed virtual address so a heap image is just
 *    a raw dump of space A plus the root table.
 */
#include "neogenera.h"
#include <sys/mman.h>
#include <unistd.h>
#include <stdlib.h>

/* Under AddressSanitizer the dead semispace is poisoned after each
 * collection, so any stale pointer dereference is reported immediately. */
#if defined(__SANITIZE_ADDRESS__)
#include <sanitizer/asan_interface.h>
#define POISON(p, n)   ASAN_POISON_MEMORY_REGION((p), (n))
#define UNPOISON(p, n) ASAN_UNPOISON_MEMORY_REGION((p), (n))
#else
#define POISON(p, n)   ((void)0)
#define UNPOISON(p, n) ((void)0)
#endif

Obj NIL, TT;
/* NIL lives in a page just past the semispaces so that its address is the same
 * in every process (a heap image stores NIL pointers). */
static Obj *nil_storage;

/* Candidate fixed addresses for the heap, tried in order.  The first suits 47/48-bit address spaces (macOS,
 * x86-64 Linux); the second lies inside a 39-bit space (0x8000000000), which many Android/Termux aarch64
 * kernels use, below where the kernel puts PIE executables and the top-down mmap area there.  Images
 * record the address they were built at, so an image must be loaded on a system that picks the same one. */
static const uintptr_t heap_hints[] = { 0x300000000000ULL, 0x1000000000ULL };
#define NHINTS (sizeof heap_hints / sizeof heap_hints[0])
#ifndef MAP_FIXED_NOREPLACE
#define MAP_FIXED_NOREPLACE 0
#endif

static char  *region;
static size_t semi;                    /* bytes per semispace */
static char  *o_space[2], *c_space[2];
static int    cur;                     /* which semispace is "from" */
static Obj   *o_hp, *c_hp;             /* allocation pointers of the old generation */
static char  *o_base, *c_base, *o_limit, *c_limit;
/* Allocation stops at the soft limits (a reserve below the real end of the space) until the next collection; the
   reserve lets the error-handling code of an out-of-memory condition run.  o_limit/c_limit stay the true ends. */
#define HEAP_RESERVE ((size_t)4 << 20)
static char  *o_alloc, *c_alloc;
static int    exhaust_count;
static Obj   *o_trigger, *c_trigger;
static Obj   *o_to_hp, *c_to_hp;
int heap_fixed;
int gc_pending, gc_stress, gc_verbose, gc_verify;
int gc_inhibit;
size_t gc_count, gc_minor_count, gc_major_count;

/* ---- the nursery: new objects are allocated here; a minor collection promotes the survivors into the old
   generation (the semispaces above), a major collection copies everything into the other semispace. */
static size_t nursery;                 /* bytes in each nursery space */
static char  *n_o_base, *n_o_limit, *n_c_base, *n_c_limit;
static Obj   *n_o_hp, *n_c_hp;
static int    major_pending;
char *ng_old_lo, *ng_old_hi, *ng_nur_lo, *ng_nur_hi;

/* Remembered sets: old objects (and conses) that may point into the nursery.  An object is marked with a header
   bit, a cons with a bit in the cons bitmap of its semispace. */
#define REM_BIT ((Obj)8)
static Obj   *rs_o, *rs_c;
static size_t rs_o_n, rs_o_cap, rs_c_n, rs_c_cap;
static unsigned char *c_bits[2];

static Obj *global_roots[4096];
static int  nglobal_roots;

static Obj *initial_root_stack[MAX_ROOTS];
Obj **root_stack = initial_root_stack;
int  nroots, root_stack_size = MAX_ROOTS;

void add_root(Obj *p) {
  if (nglobal_roots >= 4096) { fprintf(stderr, "too many global roots\n"); abort(); }
  global_roots[nglobal_roots++] = p;
}

static void set_triggers(size_t live_o, size_t live_c) {
  size_t t = 48u << 20;
  size_t to = live_o * 2 > t ? live_o * 2 : t;
  size_t tc = live_c * 2 > t ? live_c * 2 : t;
  if (to > semi * 3 / 4) to = semi * 3 / 4;
  if (tc > semi * 3 / 4) tc = semi * 3 / 4;
  o_trigger = (Obj *)(o_base + to);
  c_trigger = (Obj *)(c_base + tc);
}

static void select_space(int which) {
  cur = which;
  o_base = o_space[cur]; o_limit = o_base + semi;
  c_base = c_space[cur]; c_limit = c_base + semi;
  o_alloc = o_limit - HEAP_RESERVE; c_alloc = c_limit - HEAP_RESERVE; exhaust_count = 0;
}

static void rs_push(Obj **arr, size_t *n, size_t *cap, Obj v) {
  if (*n >= *cap) {
    size_t nc = *cap ? *cap * 2 : 4096;
    Obj *na = realloc(*arr, nc * sizeof(Obj));
    if (!na) { fprintf(stderr, "FATAL: out of memory for the remembered set\n"); exit(3); }
    *arr = na; *cap = nc;
  }
  (*arr)[(*n)++] = v;
}

static inline int in_old_o(Obj *p) { return (char *)p >= o_base && (char *)p < o_limit; }
static inline int in_old_c(Obj *p) { return (char *)p >= c_base && (char *)p < c_limit; }
static inline size_t cons_bit(Obj *p) { return (size_t)((char *)p - c_base) / (2 * sizeof(Obj)); }

void wb_record(Obj c) {
  if ((c & TAG_MASK) == TAG_OBJ) {
    Obj *p = PTR(c);
    if (!in_old_o(p) || (p[0] & REM_BIT)) return;
    p[0] |= REM_BIT;
    rs_push(&rs_o, &rs_o_n, &rs_o_cap, c);
  } else if ((c & TAG_MASK) == TAG_CONS) {
    Obj *p = (Obj *)(c - TAG_CONS);
    if (!in_old_c(p)) return;
    size_t b = cons_bit(p);
    unsigned char *bits = c_bits[cur];
    if (bits[b >> 3] & (1u << (b & 7))) return;
    bits[b >> 3] |= (unsigned char)(1u << (b & 7));
    rs_push(&rs_c, &rs_c_n, &rs_c_cap, c);
  }
}

void init_memory(void) {
  size_t try_semi[] = { 1ULL << 29, 1ULL << 27, 1ULL << 25 };
  /* Prefer any fixed address (largest size first) over a floating one: images need a fixed heap. */
  for (int i = 0; i < 3 && !region; i++) {
    semi = try_semi[i];
    nursery = semi / 32 < ((size_t)8 << 20) ? semi / 32 : ((size_t)8 << 20);
    { const char *e = getenv("NG_NURSERY_KB"); long kb = e ? atol(e) : 0;   /* a small nursery for testing the barriers */
      if (kb >= 16 && (size_t)kb * 1024 < nursery) nursery = (size_t)kb * 1024; }
    size_t len = semi * 4 + 65536 + 2 * nursery;
    for (size_t k = 0; k < NHINTS && !region; k++) {
      void *want = (void *)heap_hints[k];
      void *p = mmap(want, len, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANON | MAP_NORESERVE | MAP_FIXED_NOREPLACE, -1, 0);
      if (p == MAP_FAILED) continue;
      if (p != want) { munmap(p, len); continue; }   /* hint not honoured (e.g. beyond a 39-bit VA space) */
      region = p;
      heap_fixed = 1;
    }
  }
  for (int i = 0; i < 3 && !region; i++) {
    semi = try_semi[i];
    nursery = semi / 32 < ((size_t)8 << 20) ? semi / 32 : ((size_t)8 << 20);
    { const char *e = getenv("NG_NURSERY_KB"); long kb = e ? atol(e) : 0;
      if (kb >= 16 && (size_t)kb * 1024 < nursery) nursery = (size_t)kb * 1024; }
    void *p = mmap(NULL, semi * 4 + 65536 + 2 * nursery, PROT_READ | PROT_WRITE,
                   MAP_PRIVATE | MAP_ANON | MAP_NORESERVE, -1, 0);
    if (p != MAP_FAILED) { region = p; heap_fixed = 0; }
  }
  if (!region) { fprintf(stderr, "cannot reserve heap\n"); exit(2); }
  o_space[0] = region;            o_space[1] = region + semi;
  c_space[0] = region + 2 * semi; c_space[1] = region + 3 * semi;
  n_o_base = region + 4 * semi + 65536; n_o_limit = n_o_base + nursery;
  n_c_base = n_o_limit;                 n_c_limit = n_c_base + nursery;
  n_o_hp = (Obj *)n_o_base; n_c_hp = (Obj *)n_c_base;
  ng_old_lo = region; ng_old_hi = region + 4 * semi;
  ng_nur_lo = n_o_base; ng_nur_hi = n_c_limit;
  for (int k = 0; k < 2; k++) {
    size_t nb = semi / (2 * sizeof(Obj)) / 8 + 1;
    void *b = mmap(NULL, nb, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON | MAP_NORESERVE, -1, 0);
    if (b == MAP_FAILED) { fprintf(stderr, "cannot reserve the cons bitmap\n"); exit(2); }
    c_bits[k] = b;
  }
  select_space(0);
  o_hp = (Obj *)o_base; c_hp = (Obj *)c_base;
  set_triggers(0, 0);
  nil_storage = (Obj *)(region + 4 * semi);

  nil_storage[0] = HDR(SYM_SLOTS, T_SYMBOL);
  NIL = MK_OBJ(&nil_storage[0]);
  for (int i = 1; i <= SYM_SLOTS; i++) nil_storage[i] = NIL;
  nil_storage[1 + S_FLAGS] = FIX(SF_CONSTANT | SF_SPECIAL);
  nil_storage[1 + S_NAME] = NIL;   /* real name installed by init_symbols */
  add_root(&NIL);                  /* harmless: NIL is static and never moves */
  add_root(&TT);
  TT = NIL;
}

static void heap_exhausted(void) {
  fprintf(stderr, "\nFATAL: heap exhausted (%zu collections)\n", gc_count);
  exit(3);
}

static void out_of_memory(size_t words) {
  if (++exhaust_count > 3) heap_exhausted();        /* the error handling itself keeps failing */
  lisp_error("Heap exhausted: cannot allocate %zu words", words);
}

/* Allocate N words directly in the old generation (for large objects, or when the nursery is full).  The object is
   remembered, since whatever its creator stores into it is not seen by a write barrier. */
static Obj *alloc_old_o(size_t n) {
  Obj *p = o_hp;
  size_t avail = (size_t)(o_alloc - (char *)p) / sizeof(Obj);
  if (n > avail) {
    if (o_alloc != o_limit) { o_alloc = o_limit; gc_pending = 1; major_pending = 1; avail = (size_t)(o_limit - (char *)p) / sizeof(Obj); }
    if (n > avail) out_of_memory(n);                 /* (n is compared before any pointer arithmetic: no overflow) */
  }
  o_hp = p + n;
  if (o_hp > o_trigger) { gc_pending = 1; major_pending = 1; }
  rs_push(&rs_o, &rs_o_n, &rs_o_cap, MK_OBJ(p));
  return p;
}

static inline Obj *alloc_o(size_t n) {
  Obj *p = n_o_hp;
  if (n <= (nursery / sizeof(Obj)) / 16 && (size_t)(n_o_limit - (char *)p) / sizeof(Obj) >= n) {
    n_o_hp = p + n;
    UNPOISON(p, n * sizeof(Obj));
    return p;
  }
  if (n <= (nursery / sizeof(Obj)) / 16) gc_pending = 1;      /* the nursery is full */
  return alloc_old_o(n);
}

Obj cons(Obj a, Obj d) {
  Obj *p = n_c_hp;
  if ((char *)(p + 2) <= n_c_limit) {
    n_c_hp = p + 2;
    UNPOISON(p, 2 * sizeof(Obj));
  } else {
    gc_pending = 1;
    p = c_hp;
    if ((char *)(p + 2) > c_alloc) {
      if (c_alloc != c_limit) { c_alloc = c_limit; major_pending = 1; }
      if ((char *)(p + 2) > c_limit) out_of_memory(2);
    }
    c_hp = p + 2;
    if (c_hp > c_trigger) major_pending = 1;
    size_t b = cons_bit(p);
    c_bits[cur][b >> 3] |= (unsigned char)(1u << (b & 7));
    rs_push(&rs_c, &rs_c_n, &rs_c_cap, (Obj)p | TAG_CONS);
  }
  p[0] = a; p[1] = d;
  return (Obj)p | TAG_CONS;
}

Obj make_obj(int type, size_t nslots) {
  Obj *p = alloc_o(1 + nslots);
  p[0] = HDR(nslots, type);
  for (size_t i = 0; i < nslots; i++) p[1 + i] = NIL;
  return MK_OBJ(p);
}

Obj make_raw(int type, size_t nwords) {
  Obj *p = alloc_o(1 + nwords);
  p[0] = HDR(nwords, type);
  memset(p + 1, 0, nwords * sizeof(Obj));
  return MK_OBJ(p);
}

Obj make_string(const char *s, size_t len) {
  size_t words = 1 + (len + 1 + 7) / 8;
  Obj *p = alloc_o(1 + words);
  p[0] = HDR(words, T_STRING);
  p[1] = (Obj)len;
  p[words] = 0;                       /* zero the last word (NUL padding) */
  if (s) memcpy((char *)&p[2], s, len); else memset((char *)&p[2], 0, len);
  ((char *)&p[2])[len] = 0;
  return MK_OBJ(p);
}

Obj make_cstring(const char *s) { return make_string(s, strlen(s)); }

Obj make_flonum(double d) {
  Obj *p = alloc_o(2);
  p[0] = HDR(1, T_FLONUM);
  memcpy(&p[1], &d, sizeof d);
  return MK_OBJ(p);
}

double flonum_value(Obj o) { double d; memcpy(&d, &SLOT(o, 0), sizeof d); return d; }

Obj make_vector(size_t n, Obj init) {
  Obj *p = alloc_o(1 + n);
  p[0] = HDR(n, T_VECTOR);
  for (size_t i = 0; i < n; i++) p[1 + i] = init;
  return MK_OBJ(p);
}

Obj make_symbol_obj(Obj name) {
  Obj s = make_obj(T_SYMBOL, SYM_SLOTS);
  SYM_NAME(s) = name;
  SYM_VALUE(s) = UNBOUND;
  SYM_FLAGS(s) = FIX(0);
  return s;
}

void heap_info(size_t *obj_used, size_t *cons_used, size_t *semispace) {
  *obj_used = (size_t)((char *)o_hp - o_base) + (size_t)((char *)n_o_hp - n_o_base);
  *cons_used = (size_t)((char *)c_hp - c_base) + (size_t)((char *)n_c_hp - n_c_base);
  *semispace = semi;
}

size_t heap_bytes_used(void) {
  return (size_t)((char *)o_hp - o_base) + (size_t)((char *)c_hp - c_base)
       + (size_t)((char *)n_o_hp - n_o_base) + (size_t)((char *)n_c_hp - n_c_base);
}

/* ---- the collector ---------------------------------------------------- */

static inline int in_obj_from(Obj *p) { return (char *)p >= o_base && (char *)p < o_limit; }
static inline int in_cons_from(Obj *p) { return (char *)p >= c_base && (char *)p < c_limit; }
static inline int in_nur_o(Obj *p) { return (char *)p >= n_o_base && (char *)p < n_o_limit; }
static inline int in_nur_c(Obj *p) { return (char *)p >= n_c_base && (char *)p < n_c_limit; }
static inline int raw_type(int t) { return t == T_STRING || t == T_FLONUM || t == T_BIGNUM; }

static Obj copy_cons(Obj *p) {
  if (p[0] == FWD_MARK) return p[1];
  Obj *n = c_to_hp; c_to_hp += 2;
  n[0] = p[0]; n[1] = p[1];
  p[0] = FWD_MARK; p[1] = (Obj)n | TAG_CONS;
  return (Obj)n | TAG_CONS;
}
static Obj copy_obj(Obj *p) {
  if ((p[0] & TAG_MASK) != TAG_HDR) return p[0];   /* already forwarded */
  size_t n = 1 + (size_t)(p[0] >> 16);
  Obj *q = o_to_hp; o_to_hp += n;
  memcpy(q, p, n * sizeof(Obj));
  q[0] &= ~REM_BIT;
  p[0] = (Obj)q | TAG_OBJ;
  return (Obj)q | TAG_OBJ;
}

/* major collection: copy the live objects of the old generation and the nursery into the other semispace */
static Obj forward_major(Obj o) {
  switch (o & TAG_MASK) {
  case TAG_CONS: { Obj *p = (Obj *)(o - TAG_CONS); return (in_cons_from(p) || in_nur_c(p)) ? copy_cons(p) : o; }
  case TAG_OBJ: { Obj *p = PTR(o); return (in_obj_from(p) || in_nur_o(p)) ? copy_obj(p) : o; }
  default: return o;
  }
}
/* minor collection: promote the live nursery objects to the end of the old generation */
static Obj forward_minor(Obj o) {
  switch (o & TAG_MASK) {
  case TAG_CONS: { Obj *p = (Obj *)(o - TAG_CONS); return in_nur_c(p) ? copy_cons(p) : o; }
  case TAG_OBJ: { Obj *p = PTR(o); return in_nur_o(p) ? copy_obj(p) : o; }
  default: return o;
  }
}

static void scavenge(Obj *oscan, Obj *cscan, Obj (*fwd)(Obj)) {
  for (;;) {
    int progress = 0;
    while (cscan < c_to_hp) {
      cscan[0] = fwd(cscan[0]);
      cscan[1] = fwd(cscan[1]);
      cscan += 2; progress = 1;
    }
    while (oscan < o_to_hp) {
      Obj h = oscan[0];
      size_t n = (size_t)(h >> 16);
      if (!raw_type((int)((h >> 8) & 0xff)))
        for (size_t i = 0; i < n; i++) oscan[1 + i] = fwd(oscan[1 + i]);
      oscan += 1 + n; progress = 1;
    }
    if (!progress) break;
  }
}

static void forward_all_roots(Obj (*fwd)(Obj)) {
  for (int i = 0; i < nglobal_roots; i++) *global_roots[i] = fwd(*global_roots[i]);
  for (int i = 1; i <= SYM_SLOTS; i++) nil_storage[i] = fwd(nil_storage[i]);
  for (int i = 0; i < vsp; i++) vstack[i] = fwd(vstack[i]);
  for (int i = 0; i < spsp; i++) { specpdl[i].sym = fwd(specpdl[i].sym); specpdl[i].old = fwd(specpdl[i].old); }
  for (int i = 0; i < nroots; i++) *root_stack[i] = fwd(*root_stack[i]);
  for (int i = 0; i < vfp; i++) vframes[i].closure = fwd(vframes[i].closure);
  for (int i = 0; i < mv_count && i < MAX_MV; i++) mv_vals[i] = fwd(mv_vals[i]);
  for (Frame *f = frame_top; f; f = f->prev) {
    f->tag = fwd(f->tag); f->value = fwd(f->value); f->cleanup = fwd(f->cleanup);
    f->mvlist = fwd(f->mvlist);
  }
  gc_forward_roots(fwd);
}

static void reset_nursery(void) {
  POISON(n_o_base, nursery); POISON(n_c_base, nursery);
  n_o_hp = (Obj *)n_o_base; n_c_hp = (Obj *)n_c_base;
}

static void clear_remembered(void) {
  for (size_t i = 0; i < rs_o_n; i++) { Obj *p = PTR(rs_o[i]); if (in_old_o(p) && (p[0] & TAG_MASK) == TAG_HDR) p[0] &= ~REM_BIT; }
  for (size_t i = 0; i < rs_c_n; i++) {
    Obj *p = (Obj *)(rs_c[i] - TAG_CONS);
    if (in_old_c(p)) { size_t b = cons_bit(p); c_bits[cur][b >> 3] &= (unsigned char)~(1u << (b & 7)); }
  }
  rs_o_n = rs_c_n = 0;
}

/* --gc-verify: before a minor collection, every pointer from the old generation into the nursery must come from a
   remembered object or cons; otherwise a write barrier is missing. */
static int describe_container(Obj *p, char *buf, size_t n) {
  int t = (int)((p[0] >> 8) & 0xff);
  if (t == T_SYMBOL && STRINGP(p[1 + S_NAME])) return snprintf(buf, n, "symbol %s", STR_DATA(p[1 + S_NAME]));
  return snprintf(buf, n, "object of type %d with %zu slots", t, (size_t)(p[0] >> 16));
}
static void verify_old_generation(void) {
  for (size_t i = 0; i < rs_o_n; i++) { Obj *p = PTR(rs_o[i]); if (in_old_o(p) && (p[0] & TAG_MASK) == TAG_HDR) p[0] |= REM_BIT; }
  for (Obj *p = (Obj *)o_base; p < o_hp; ) {
    Obj h = p[0]; size_t n = (size_t)(h >> 16);
    if (!raw_type((int)((h >> 8) & 0xff)) && !(h & REM_BIT))
      for (size_t i = 0; i < n; i++) {
        Obj v = p[1 + i];
        if (ng_heap_ptr(v) && (char *)(v & ~(Obj)7) >= ng_nur_lo && (char *)(v & ~(Obj)7) < ng_nur_hi) {
          char d[160]; describe_container(p, d, sizeof d);
          fprintf(stderr, "\nGC VERIFY: missing write barrier: slot %zu of %s points into the nursery\n", i, d);
          abort();
        }
      }
    p += 1 + n;
  }
  for (Obj *p = (Obj *)c_base; p < c_hp; p += 2) {
    size_t b = cons_bit(p);
    if (c_bits[cur][b >> 3] & (1u << (b & 7))) continue;
    for (int k = 0; k < 2; k++) {
      Obj v = p[k];
      if (ng_heap_ptr(v) && (char *)(v & ~(Obj)7) >= ng_nur_lo && (char *)(v & ~(Obj)7) < ng_nur_hi) {
        fprintf(stderr, "\nGC VERIFY: missing write barrier: the %s of a cons points into the nursery\n", k ? "cdr" : "car");
        abort();
      }
    }
  }
}

#include <time.h>
static double now_ms(void) { struct timespec ts; clock_gettime(CLOCK_MONOTONIC, &ts); return ts.tv_sec * 1e3 + ts.tv_nsec / 1e6; }

static void gc_major(void) {
  double t0 = gc_verbose ? now_ms() : 0;
  gc_count++; gc_major_count++;
  size_t before = heap_bytes_used();
  int to = 1 - cur;
  Obj *o_to_base = (Obj *)o_space[to], *c_to_base = (Obj *)c_space[to];
  UNPOISON(o_space[to], semi); UNPOISON(c_space[to], semi);
  UNPOISON(n_o_base, nursery); UNPOISON(n_c_base, nursery);
  o_to_hp = o_to_base; c_to_hp = c_to_base;
  forward_all_roots(forward_major);
  scavenge(o_to_base, c_to_base, forward_major);
  clear_remembered();                                   /* (the bits of the from-space bitmap) */

  size_t live_o = (size_t)((char *)o_to_hp - (char *)o_to_base);
  size_t live_c = (size_t)((char *)c_to_hp - (char *)c_to_base);
  POISON(o_space[cur], (size_t)((char *)o_hp - o_base));
  POISON(c_space[cur], (size_t)((char *)c_hp - c_base));
  select_space(to);
  o_hp = o_to_hp; c_hp = c_to_hp;
  reset_nursery();
  set_triggers(live_o, live_c);
  major_pending = 0;
  gc_pending = gc_stress ? 1 : 0;
  if (gc_verbose)
    fprintf(stderr, "[gc %zu (major): %zuK -> %zuK, %.1f ms]\n", gc_count, before >> 10, (live_o + live_c) >> 10, now_ms() - t0);
}

static void gc_minor(void) {
  size_t n_o_used = (size_t)((char *)n_o_hp - n_o_base), n_c_used = (size_t)((char *)n_c_hp - n_c_base);
  /* the survivors must fit in the old generation; if they might not, collect everything */
  if (major_pending || (size_t)(o_alloc - (char *)o_hp) < n_o_used + HEAP_RESERVE || (size_t)(c_alloc - (char *)c_hp) < n_c_used + HEAP_RESERVE) {
    gc_major();
    return;
  }
  if (gc_verify) verify_old_generation();
  double t0 = gc_verbose ? now_ms() : 0;
  gc_count++; gc_minor_count++;
  size_t before = n_o_used + n_c_used;
  Obj *oscan = o_hp, *cscan = c_hp;
  o_to_hp = o_hp; c_to_hp = c_hp;
  forward_all_roots(forward_minor);
  /* the remembered objects' slots are roots too */
  for (size_t i = 0; i < rs_o_n; i++) {
    Obj *p = PTR(rs_o[i]);
    if (!in_old_o(p) || (p[0] & TAG_MASK) != TAG_HDR) continue;
    size_t n = (size_t)(p[0] >> 16);
    if (!raw_type((int)((p[0] >> 8) & 0xff))) for (size_t k = 0; k < n; k++) p[1 + k] = forward_minor(p[1 + k]);
  }
  for (size_t i = 0; i < rs_c_n; i++) {
    Obj *p = (Obj *)(rs_c[i] - TAG_CONS);
    if (in_old_c(p)) { p[0] = forward_minor(p[0]); p[1] = forward_minor(p[1]); }
  }
  scavenge(oscan, cscan, forward_minor);
  clear_remembered();
  o_hp = o_to_hp; c_hp = c_to_hp;
  reset_nursery();
  if (o_hp > o_trigger || c_hp > c_trigger) major_pending = 1;
  gc_pending = (gc_stress || major_pending) ? 1 : 0;
  if (gc_stress && gc_minor_count % 64 == 0) major_pending = 1;     /* stress both kinds */
  if (gc_verbose)
    fprintf(stderr, "[gc %zu (minor): %zuK promoted of %zuK, %.2f ms]\n", gc_count,
            (size_t)(((char *)o_hp - (char *)oscan) + ((char *)c_hp - (char *)cscan)) >> 10, before >> 10, now_ms() - t0);
}

/* SAVE-WORLD: the image is written at the next safe point (where the VM reloads its cached pointers), not inside the
   primitive that asks for it. */
static char save_request[4096];
static int  save_requested, save_rc = -99;
int request_save(const char *path) {
  if (strlen(path) >= sizeof save_request) return -1;
  strcpy(save_request, path); save_requested = 1; save_rc = -99; gc_pending = 1;
  return 0;
}
int save_result(void) { return save_rc; }

void gc(void) {                      /* at a safe point: minor, or major when one is due */
  if (save_requested) { save_requested = 0; save_rc = save_image(save_request); return; }
  gc_minor();
}
void gc_full(void) { gc_major(); }

void gc_check(void) { if (gc_pending && !gc_inhibit) gc_minor(); }

/* Debug aid (build with -DNG_DEBUG): is `o` a plausible live value? */
int obj_valid(Obj o) {
  switch (o & TAG_MASK) {
  case TAG_FIX: case TAG_IMM: return 1;
  case TAG_CONS: { Obj *p = (Obj *)(o - TAG_CONS);
    return (in_cons_from(p) && (char *)(p + 2) <= (char *)c_hp) || (in_nur_c(p) && (char *)(p + 2) <= (char *)n_c_hp); }
  case TAG_OBJ: { Obj *p = PTR(o); if (p == &nil_storage[0]) return 1;
    return ((in_obj_from(p) && (char *)p < (char *)o_hp) || (in_nur_o(p) && (char *)p < (char *)n_o_hp)) && (p[0] & TAG_MASK) == TAG_HDR; }
  default: return 0;
  }
}

/* ---- heap images ------------------------------------------------------ */

typedef struct {
  char magic[8];
  uint64_t obj_base, cons_base, obj_used, cons_used, nroots, semi;
  uint64_t checksum;                       /* FNV-1a over everything after the header */
} ImgHdr;

static uint64_t fnv1a(uint64_t h, const void *data, size_t n) {
  const unsigned char *p = data;
  for (size_t i = 0; i < n; i++) { h ^= p[i]; h *= 1099511628211ULL; }
  return h;
}
#define FNV_INIT 14695981039346656037ULL

/* The image is written to PATH.tmp.<pid>, flushed to disk and renamed over PATH, so a crash (or a full disk)
   in the middle of a save never damages an existing image. */
int save_image(const char *path) {
  if (!heap_fixed) { fprintf(stderr, "image: heap not at fixed address\n"); return -1; }
  int saved_stress = gc_stress; gc_stress = 0;
  gc_major(); if (cur != 0) gc_major();
  gc_stress = saved_stress;
  char tmp[4096];
  if (snprintf(tmp, sizeof tmp, "%s.tmp.%ld", path, (long)getpid()) >= (int)sizeof tmp) return -1;
  FILE *f = fopen(tmp, "wb");
  if (!f) return -1;
  ImgHdr h; memset(&h, 0, sizeof h); memcpy(h.magic, "NGIMG005", 8);
  h.obj_base = (uint64_t)(uintptr_t)o_base; h.cons_base = (uint64_t)(uintptr_t)c_base;
  h.obj_used = (uint64_t)((char *)o_hp - o_base); h.cons_used = (uint64_t)((char *)c_hp - c_base);
  h.nroots = (uint64_t)nglobal_roots; h.semi = semi;
  uint64_t sum = FNV_INIT;
  for (int i = 0; i < nglobal_roots; i++) sum = fnv1a(sum, global_roots[i], sizeof(Obj));
  sum = fnv1a(sum, &nil_storage[1], sizeof(Obj) * SYM_SLOTS);
  sum = fnv1a(sum, o_base, h.obj_used);
  sum = fnv1a(sum, c_base, h.cons_used);
  h.checksum = sum;
  fwrite(&h, sizeof h, 1, f);
  for (int i = 0; i < nglobal_roots; i++) fwrite(global_roots[i], sizeof(Obj), 1, f);
  fwrite(&nil_storage[1], sizeof(Obj), SYM_SLOTS, f);
  fwrite(o_base, 1, h.obj_used, f);
  fwrite(c_base, 1, h.cons_used, f);
  int ok = fflush(f) == 0 && ferror(f) == 0 && fsync(fileno(f)) == 0;
  if (fclose(f) != 0) ok = 0;
  if (ok && rename(tmp, path) != 0) ok = 0;
  if (!ok) unlink(tmp);
  return ok ? 0 : -1;
}

/* Returns 0, or: -1 cannot open, -2 truncated or not an image, -3 built for a different heap layout,
   -4 corrupt (checksum mismatch) or an impossible header. */
int load_image(const char *path) {
  FILE *f = fopen(path, "rb");
  if (!f) return -1;
  ImgHdr h;
  if (fread(&h, sizeof h, 1, f) != 1 || memcmp(h.magic, "NGIMG005", 8) != 0) { fclose(f); return -2; }
  if (!heap_fixed || h.obj_base != (uint64_t)(uintptr_t)o_space[0] ||
      h.cons_base != (uint64_t)(uintptr_t)c_space[0] || h.semi != semi ||
      h.nroots != (uint64_t)nglobal_roots) { fclose(f); return -3; }
  if (h.obj_used > semi || h.cons_used > semi || (h.obj_used & 7) || (h.cons_used & 7)) { fclose(f); return -4; }
  /* first pass: verify the checksum before touching the live heap */
  uint64_t sum = FNV_INIT, remaining = h.nroots * sizeof(Obj) + sizeof(Obj) * SYM_SLOTS + h.obj_used + h.cons_used;
  static char chunk[1 << 16];
  while (remaining > 0) {
    size_t want = remaining < sizeof chunk ? (size_t)remaining : sizeof chunk;
    if (fread(chunk, 1, want, f) != want) { fclose(f); return -2; }
    sum = fnv1a(sum, chunk, want); remaining -= want;
  }
  if (sum != h.checksum) { fclose(f); return -4; }
  if (fseek(f, (long)sizeof h, SEEK_SET) != 0) { fclose(f); return -2; }
  select_space(0);
  for (int i = 0; i < nglobal_roots; i++)
    if (fread(global_roots[i], sizeof(Obj), 1, f) != 1) { fclose(f); return -2; }
  if (fread(&nil_storage[1], sizeof(Obj), SYM_SLOTS, f) != SYM_SLOTS) { fclose(f); return -2; }
  if (fread(o_base, 1, h.obj_used, f) != h.obj_used ||
      fread(c_base, 1, h.cons_used, f) != h.cons_used) { fclose(f); return -2; }
  fclose(f);
  o_hp = (Obj *)(o_base + h.obj_used); c_hp = (Obj *)(c_base + h.cons_used);
  n_o_hp = (Obj *)n_o_base; n_c_hp = (Obj *)n_c_base; rs_o_n = rs_c_n = 0; major_pending = 0;
  set_triggers(h.obj_used, h.cons_used);
  gc_pending = gc_stress ? 1 : 0;
  return 0;
}
