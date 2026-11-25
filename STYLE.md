# Style guide

## C (`src/`)

* C11 (`-std=gnu11`), 2-space indent, no tabs, lines under ~110 columns.
* Names: `snake_case` functions, `UPPER_CASE` macros and enum constants, `Obj` for Lisp values.
* Primitives are declared with `PRIM(name)` and registered with `defprim`. Accessors for object
  layouts are macros over `SLOT(o, i)` with enum slot indices in `neogenera.h`.
* Any function that may run Lisp code or trigger a collection documents which locals it protects.
* Errors go through `lisp_error` / `type_error`; never `exit` from library code.
* Warnings are errors in spirit: build is clean with `-Wall -Wextra`.

## Lisp (`lisp/`)

* Files start with `(in-package :cl)` (the system lives in `COMMON-LISP`; see `docs/design/bootstrap.md`).
* Internal helpers are named `%like-this` and are not exported. Public names follow ANSI Common Lisp.
* 2-space indentation, standard Lisp layout, docstrings on exported functions.
* Code in `lisp/core` and `lisp/compiler` must be loadable by the stage-0 evaluator and compilable by
  the compiler itself: stay within the special forms listed in `docs/design/compiler.md`.
* Keep functions small; prefer `labels` and tail recursion to deep non-tail recursion in the compiler.

## Commits

Imperative mood, one logical change per commit, mention the roadmap phase when relevant.
