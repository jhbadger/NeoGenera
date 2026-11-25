# Bootstrap, self-hosting and heap images

## Stages

```
C kernel ──► stage0.img  the C evaluator loads lisp/core and lisp/compiler (compiler is interpreted)
stage0 ──► s1/*.nfasl    the *interpreted* compiler compiles every source file
stage0 + s1 ─► stage1.img the same system, now entirely compiled bytecode (EVAL uses the compiler)
stage1 ──► s2/*.nfasl    the *compiled* compiler recompiles every source file
cmp s1 s2              must be identical (the fixed point)
stage1 + s2 ─► neogenera.img  finalized: CL exports frozen, *package* = CL-USER
```

`make world` runs all of it; `make selfhost` stops after the comparison. The order of source
files is in `lisp/sources.lisp` and is shared by every stage.

The fixed point is a strong test of the compiler *and* the VM: any difference between the
interpreted and compiled executions of the compiler changes its output. It has caught
three real bugs during development: lost tail calls and multiple values in `defun`
bodies, a bootstrap cycle when redefining `dolist`, and in-place macro memoization
mutating source forms.

Determinism requirements for the comparison: code objects never store `gensym` names
(uninterned function names are dropped), constants are deduplicated with `eql` in first-use
order, and `copy-tree` shields source forms from the evaluator.

## Package conventions during boot

All system code lives in `COMMON-LISP`. While `export_all_cl` is set (during boot), every
symbol interned there is exported, except names beginning with `%`, which are internal. The final
image turns the flag off. `COMMON-LISP-USER` uses `COMMON-LISP`.

## Heap images

`save-image` runs two collections (so live data is in semispace A), then writes:

```
header   magic "NGIMG002", object/cons space base addresses, used sizes, root count, semispace size
roots    the value of every registered global root, in registration order
nil      the six slots of the NIL symbol
objects  raw bytes of the object space
conses   raw bytes of the cons space
```

Loading maps the same address range, checks that the addresses, semispace size and root count
match, and reads the data back. Consequences:

* An image only loads into a kernel built from the same sources (the registration order of roots and
  the primitive table must match) and on a platform where the fixed mapping succeeds.
* The heap must contain no process-specific addresses (see `object-model.md`).
* Open file streams do not survive an image.

Start a session with `bin/neogenera` (uses `build/neogenera.img`), or pass `-i IMAGE`.
