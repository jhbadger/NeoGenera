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
static Obj   *o_hp, *c_hp;
static char  *o_base, *c_base, *o_limit, *c_limit;
/* Allocation stops at the soft limits (a reserve below the real end of the space) until the next collection; the
   reserve lets the error-handling code of an out-of-memory condition run.  o_limit/c_limit stay the true ends. */
#define HEAP_RESERVE ((size_t)4 << 20)
static char  *o_alloc, *c_alloc;
static int    exhaust_count;
static Obj   *o_trigger, *c_trigger;
static Obj   *o_to_hp, *c_to_hp;
int heap_fixed;
int gc_pending, gc_stress, gc_verbose;
int gc_inhibit;
size_t gc_count;

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

void init_memory(void) {
  size_t try_semi[] = { 1ULL << 29, 1ULL << 27, 1ULL << 25 };
  /* Prefer any fixed address (largest size first) over a floating one: images need a fixed heap. */
  for (int i = 0; i < 3 && !region; i++) {
    semi = try_semi[i];
    size_t len = semi * 4 + 65536;
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
    void *p = mmap(NULL, semi * 4 + 65536, PROT_READ | PROT_WRITE,
                   MAP_PRIVATE | MAP_ANON | MAP_NORESERVE, -1, 0);
    if (p != MAP_FAILED) { region = p; heap_fixed = 0; }
  }
  if (!region) { fprintf(stderr, "cannot reserve heap\n"); exit(2); }
  o_space[0] = region;            o_space[1] = region + semi;
  c_space[0] = region + 2 * semi; c_space[1] = region + 3 * semi;
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

static inline Obj *alloc_o(size_t n) {
  Obj *p = o_hp;
  size_t avail = (size_t)(o_alloc - (char *)p) / sizeof(Obj);
  if (n > avail) {
    if (o_alloc != o_limit) { o_alloc = o_limit; gc_pending = 1; avail = (size_t)(o_limit - (char *)p) / sizeof(Obj); }
    if (n > avail) out_of_memory(n);                 /* (n is compared before any pointer arithmetic: no overflow) */
  }
  o_hp = p + n;
  if (o_hp > o_trigger) gc_pending = 1;
  return p;
}

Obj cons(Obj a, Obj d) {
  Obj *p = c_hp;
  if ((char *)(p + 2) > c_alloc) {
    if (c_alloc != c_limit) { c_alloc = c_limit; gc_pending = 1; }
    if ((char *)(p + 2) > c_limit) out_of_memory(2);
  }
  c_hp = p + 2;
  if (c_hp > c_trigger) gc_pending = 1;
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
  *obj_used = (size_t)((char *)o_hp - o_base); *cons_used = (size_t)((char *)c_hp - c_base); *semispace = semi;
}

size_t heap_bytes_used(void) {
  return (size_t)((char *)o_hp - o_base) + (size_t)((char *)c_hp - c_base);
}

/* ---- the collector ---------------------------------------------------- */

static inline int in_obj_from(Obj *p) { return (char *)p >= o_base && (char *)p < o_limit; }
static inline int in_cons_from(Obj *p) { return (char *)p >= c_base && (char *)p < c_limit; }

static Obj forward(Obj o) {
  switch (o & TAG_MASK) {
  case TAG_CONS: {
    Obj *p = (Obj *)(o - TAG_CONS);
    if (!in_cons_from(p)) return o;
    if (p[0] == FWD_MARK) return p[1];
    Obj *n = c_to_hp; c_to_hp += 2;
    n[0] = p[0]; n[1] = p[1];
    p[0] = FWD_MARK; p[1] = (Obj)n | TAG_CONS;
    return (Obj)n | TAG_CONS;
  }
  case TAG_OBJ: {
    Obj *p = PTR(o);
    if (!in_obj_from(p)) return o;
    if ((p[0] & TAG_MASK) != TAG_HDR) return p[0];   /* already forwarded */
    size_t n = 1 + (size_t)(p[0] >> 16);
    Obj *q = o_to_hp; o_to_hp += n;
    memcpy(q, p, n * sizeof(Obj));
    p[0] = (Obj)q | TAG_OBJ;
    return (Obj)q | TAG_OBJ;
  }
  default:
    return o;
  }
}

static void scavenge(Obj *oscan, Obj *cscan) {
  for (;;) {
    int progress = 0;
    while (cscan < c_to_hp) {
      cscan[0] = forward(cscan[0]);
      cscan[1] = forward(cscan[1]);
      cscan += 2; progress = 1;
    }
    while (oscan < o_to_hp) {
      Obj h = oscan[0];
      size_t n = (size_t)(h >> 16);
      int t = (int)((h >> 8) & 0xff);
      if (t != T_STRING && t != T_FLONUM && t != T_BIGNUM)
        for (size_t i = 0; i < n; i++) oscan[1 + i] = forward(oscan[1 + i]);
      oscan += 1 + n; progress = 1;
    }
    if (!progress) break;
  }
}

void gc(void) {
  gc_count++;
  size_t before = heap_bytes_used();
  int to = 1 - cur;
  Obj *o_to_base = (Obj *)o_space[to], *c_to_base = (Obj *)c_space[to];
  UNPOISON(o_space[to], semi); UNPOISON(c_space[to], semi);
  o_to_hp = o_to_base; c_to_hp = c_to_base;

  for (int i = 0; i < nglobal_roots; i++) *global_roots[i] = forward(*global_roots[i]);
  for (int i = 1; i <= SYM_SLOTS; i++) nil_storage[i] = forward(nil_storage[i]);
  for (int i = 0; i < vsp; i++) vstack[i] = forward(vstack[i]);
  for (int i = 0; i < spsp; i++) { specpdl[i].sym = forward(specpdl[i].sym); specpdl[i].old = forward(specpdl[i].old); }
  for (int i = 0; i < nroots; i++) *root_stack[i] = forward(*root_stack[i]);
  for (int i = 0; i < vfp; i++) vframes[i].closure = forward(vframes[i].closure);
  for (int i = 0; i < mv_count && i < MAX_MV; i++) mv_vals[i] = forward(mv_vals[i]);
  for (Frame *f = frame_top; f; f = f->prev) {
    f->tag = forward(f->tag); f->value = forward(f->value); f->cleanup = forward(f->cleanup);
    f->mvlist = forward(f->mvlist);
  }
  gc_forward_roots(forward);
  scavenge(o_to_base, c_to_base);

  size_t live_o = (size_t)((char *)o_to_hp - (char *)o_to_base);
  size_t live_c = (size_t)((char *)c_to_hp - (char *)c_to_base);
  POISON(o_space[cur], (size_t)((char *)o_hp - o_base));
  POISON(c_space[cur], (size_t)((char *)c_hp - c_base));
  select_space(to);
  o_hp = o_to_hp; c_hp = c_to_hp;
  set_triggers(live_o, live_c);
  gc_pending = gc_stress ? 1 : 0;
  if (gc_verbose)
    fprintf(stderr, "[gc %zu: %zuK -> %zuK]\n", gc_count, before >> 10, (live_o + live_c) >> 10);
}

void gc_check(void) { if (gc_pending && !gc_inhibit) gc(); }

/* Debug aid (build with -DNG_DEBUG): is `o` a plausible live value? */
int obj_valid(Obj o) {
  switch (o & TAG_MASK) {
  case TAG_FIX: case TAG_IMM: return 1;
  case TAG_CONS: { Obj *p = (Obj *)(o - TAG_CONS); return in_cons_from(p) && (char *)(p + 2) <= (char *)c_hp; }
  case TAG_OBJ: { Obj *p = PTR(o); if (p == &nil_storage[0]) return 1; return in_obj_from(p) && (char *)p < (char *)o_hp && (p[0] & TAG_MASK) == TAG_HDR; }
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
  gc(); if (cur != 0) gc();
  gc_stress = saved_stress;
  char tmp[4096];
  if (snprintf(tmp, sizeof tmp, "%s.tmp.%ld", path, (long)getpid()) >= (int)sizeof tmp) return -1;
  FILE *f = fopen(tmp, "wb");
  if (!f) return -1;
  ImgHdr h; memset(&h, 0, sizeof h); memcpy(h.magic, "NGIMG004", 8);
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
  if (fread(&h, sizeof h, 1, f) != 1 || memcmp(h.magic, "NGIMG004", 8) != 0) { fclose(f); return -2; }
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
  set_triggers(h.obj_used, h.cons_used);
  gc_pending = gc_stress ? 1 : 0;
  return 0;
}
