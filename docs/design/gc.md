# Garbage collector

Status: implemented in `src/memory.c`.

## Algorithm

A **generational** copying collector with two generations.

* **The nursery** (8 MB for objects and 8 MB for conses, after the NIL page) is where `alloc_o` and `cons` allocate.
* **The old generation** is a Cheney semispace pair for header-prefixed objects and another for conses. Conses
  carry no header, so mixing them into one space would make linear scanning of to-space ambiguous.

A **minor collection** runs at a safe point once the nursery is full. It copies the live nursery objects to the end
of the old generation ("promotes" them; there is no aging). The roots are the usual ones plus the **remembered
set**, then the promoted region is scanned Cheney-style, and the nursery is reset. A **major collection** copies the
live objects of the old generation and of the nursery into the other old semispace. It runs when the old generation
has grown past `max(48 MB, 2 × live data)` since the last one, when the old generation could not take the nursery's
survivors, and on `(gc)` and `save-image`.

**The write barrier.** A minor collection does not scan the old generation, so any pointer from it into the
nursery must be known. After every store of a Lisp value into an object that may already be old, C code calls
`WB(container, value)`. If the value points into the nursery and the container lies in the old generation, it
records the container: an object gets a header bit (`REM_BIT`, bit 3, outside the tag, type and size fields) and
goes on `rs_o`; a cons gets a bit in the cons bitmap of its semispace and goes on `rs_c`. Objects allocated
directly in the old generation (larger than 1/16 of the nursery, or when the nursery is full before the next safe
point) are remembered when allocated, because their creator fills them without barriers. Stores into objects
allocated since the last safe point need no barrier: they are in the nursery (or remembered already). The primitives
that mutate (`rplaca`, `%aset`, `%struct-set`, `set`, `(setf symbol-function)`, ...), special binding and
unbinding, the VM's global and box stores, `nconc`/`nreverse`, the evaluator's `setq`/`defun`/macro memoization
and the package tables all have barriers; Lisp code mutates only through these.

**Verifying the barrier.** With `--gc-verify` (or `NG_GC_VERIFY=1`), every minor collection first scans the whole
old generation and aborts, naming the container and slot, if a pointer into the nursery comes from an object or cons
that is not remembered. `NG_NURSERY_KB=64` makes the nursery tiny, so that collections, and checks, happen
thousands of times in a test run.

All the spaces are mapped at a fixed virtual address (the first of `heap_hints` in `memory.c` the kernel honours; a
lower one exists for 39-bit Android address spaces), so a heap image is a raw dump of the old generation after a
major collection (see `bootstrap.md`). Space is reserved with `MAP_NORESERVE`; only touched pages cost memory.

## Safe points

**Allocation never collects.** Collection happens only at *safe points*:

* the entry of `eval()` for a non-atomic form (the stage-0 evaluator), and
* `CALL` / `TAILCALL` in the VM.

Consequently a C primitive that only allocates can hold raw `Obj` values in locals without
protection. Only code that can call back into Lisp (`eval`, `apply_stack`, `funcall`,
`mapcar`-style helpers) needs care.

## Calling Lisp from the printer

The C printer calls the Lisp function `%PRINT-HOOK` for structure instances (CLOS instances, conditions,
`defstruct` objects with a `print-object` method). Because the printer holds raw pointers on the C
stack, collection is **inhibited** (`gc_inhibit`, saved and restored by catch frames) for the duration
of the call; an unusually allocation-heavy `print-object` method therefore grows the heap until it returns.

## Roots

* registered global variables (`add_root`), including the interned `Q_*` symbols,
* the NIL symbol's slots,
* the value stack `vstack[0 .. vsp)` and special-binding stack,
* VM frames (`vframes[i].closure`),
* multiple-value registers `mv_vals[0 .. mv_count)`,
* the `Frame` chain (catch/block/tagbody/unwind-protect records on the C stack),
* **the C-local root stack**: `PROTECT(var)` pushes the *address* of a C local, and the
  collector updates the variable in place,
* the saved stacks, bindings, root stacks, frames and multiple values of every Lisp process that
  is not running (`gc_forward_roots` in `proc.c`; see `processes.md`).

Rules for evaluator-level C code:

1. Any `Obj` local that is live across a call that may collect must be `PROTECT`ed, and the
   function must restore `nroots` on every exit (the `RET` macro does this).
2. Arguments of primitives are read through `argv` (which points into `vstack`), never
   copied into long-lived locals across callbacks.
3. Non-local exits restore `nroots`, `vsp`, `vfp` and the special-binding height from the
   target `Frame`.

## Verification tools

* `--gc-stress` makes every safe point collect. The whole Lisp suite passes under it
  (`make test-stress`, several minutes).
* An AddressSanitizer build **poisons the dead semispace** after each collection
  (`__SANITIZE_ADDRESS__` in `memory.c`), so any stale pointer is reported at the faulting
  access. Run `make asan` and use `build/neogenera-asan`.
* `-DNG_DEBUG` validates every `eval` argument against the live heap.
* The kernel installs a crash handler that prints a backtrace; `addr2line -e build/neogenera`
  resolves the offsets.
