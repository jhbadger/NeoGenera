# Garbage collector

Status: implemented in `src/memory.c`.

## Algorithm

A Cheney copying collector with **two separate semispace pairs**: one for header-prefixed
objects and one for conses. Conses carry no header, so mixing them into one space would
make linear scanning of to-space ambiguous. Collection scans both to-spaces alternately
until neither has unscanned data.

Both semispaces are mapped at a fixed virtual address (the first of `heap_hints` in `memory.c` the kernel honours; a lower one exists for 39-bit Android address spaces) so a heap image is a
raw dump (see `bootstrap.md`). Space is reserved with `MAP_NORESERVE`; only touched pages
cost memory. The collector triggers when allocation since the last collection exceeds
`max(48 MB, 2 × live data)`.

The collector is not generational yet; that is planned for roadmap Phase 4.

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
  collector updates the variable in place.

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
