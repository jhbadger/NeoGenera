# Compatibility with ANSI Common Lisp

NeoGenera implements a large, practical subset of Common Lisp, written to be read and changed rather than to be
a drop-in replacement for SBCL or CCL. This document lists the deliberate deviations. `make conformance` runs
`tests/conformance/cases.lisp` (about 600 cases written against the specification) and writes
[`CONFORMANCE.md`](CONFORMANCE.md); the ANSI `ansi-test` suite is not used (it could not be fetched offline), so no
ansi-test pass rate is claimed.

## What is implemented

Numbers (fixnums, bignums with Karatsuba multiplication, exact ratios, double floats with IEEE infinities and NaN), conses and lists,
sequences, strings and characters, vectors, hash tables, structures, symbols and packages, `loop`, `format`, `defstruct`, conditions and
restarts, CLOS (multiple dispatch, `call-next-method`, `:before`/`:after`/`:around`, EQL specializers, C3 class precedence), Gray streams,
a printer with `pprint`, a compiler to bytecode that compiles itself, an interactive debugger, and `setf` for the common places
(including `getf`).

## Deliberate deviations

**Numbers**
* Complex numbers are in the tower (rational or float parts); `abs` of a complex number is always a float, and the transcendental
  functions use the C library's branch cuts.
* Floats are a single type (double); there are no single-float, short-float or long-float distinctions, and `float` takes no prototype.
* Division by zero with `/` signals `division-by-zero` for floats too; infinities and NaN arise from overflow and `+infinity+`, `+nan+`.
* `*print-base*` and `*print-radix*` are not honoured by the printer; use `~B ~O ~X ~R`.

**Strings and characters**
* Strings hold characters: `length`, `char`, `schar`, `subseq` and `position` count code points. Files, the terminal,
  the OS and `run-program` exchange UTF-8, decoded on input and encoded on output; a malformed byte reads as the
  character with that code. There are no external formats other than UTF-8. Case conversion and the character
  predicates cover Latin, Greek, Cyrillic and the other scripts of `lisp/core/63-unicode.lisp`; a mapping that changes
  a string's length (`ß` → `SS`) is not made. `base-char` is the same type as `character`.
* `string<`, `string>`, `string<=`, `string>=` and `string/=` return a boolean rather than the mismatch index, and do not take `:start`/`:end` keywords.
* `format` implements the standard directives, including `~E ~F ~G ~$` with all their parameters, `~<...~>` justification and
  `~<...~:>` logical blocks, `~_ ~I ~W ~/name/` and `~T`. `~R` spells numbers in English up to the quintillions.

**Arrays**
* Arrays of any rank exist (`make-array` with a list of dimensions, `aref` with several subscripts, `#2A(...)`); there are no displaced
  arrays and no specialised element types (the `:element-type` is accepted and ignored except for `character`). `adjust-array` returns a new
  array unless the vector is adjustable.

**Evaluation and the compiler**
* There is one lexical environment model and no `compile-file` external format: the compiler writes text fasls; `compile` and `compiled-eval` compile to bytecode.
* No `declare` optimisation qualities are acted on; type declarations are ignored (the compiler does not specialise).
* Redefinition of a function replaces it everywhere at once, including calls inside already-compiled code, because calls go through the symbol.
* `defmacro` expansion is memoised in place by the evaluator, so macros with side effects at expansion time run once per source form.

**CLOS**
* `change-class`, metaclasses, `:allocation :class` slots, non-standard method combinations and `update-instance-for-redefined-class` are not implemented;
  redefining a class does not update existing instances. `standard-class` is not a type specifier for class objects.
* The meta-object protocol is a small subset (`class-of`, `find-class`, `class-precedence-list`, slot access).

**Conditions and the debugger**
* The interactive debugger with restarts runs in both the line-mode and the window Listener. `break` enters it with a CONTINUE restart, and it honours `*debugger-hook*`.
* Missing: `with-condition-restarts`, `invoke-restart-interactively`, return-from-frame and retry in the debugger.

**Packages, reading and printing**
* Packages are simple: `defpackage`/`in-package` exist but package locks, `shadowing-import` subtleties and nicknames are minimal. Most library symbols live in `COMMON-LISP` (they are not exported from it in the strict sense).
* The reader is written in C and consults `*readtable*`: macro characters, dispatch characters and the readtable case are supported for
  characters below 128; `set-syntax-from-char` copies macro syntax only (a character cannot be made whitespace). `#C`, `#P` and `#A` read.
* `pprint` is a line-filling printer for code; `pprint-logical-block`, `pprint-newline`, `pprint-indent`, `pprint-tab` and `pprint-fill`/`-linear`/`-tabular`
  lay out logical blocks. `*print-pretty*` does not make `print` use them.

**System interface**
* Pathnames are Unix paths with Common Lisp components; logical pathnames translate through `logical-pathname-translations`. `directory`
  handles `*` and `**`. `with-open-file` supports `:direction`, `:if-exists`, `:if-does-not-exist` for text files only.
* The garbage collector is a generational copier (a nursery and a two-space old generation). `make-hash-table` takes
  `:weakness` (`:key`, `:value`, `:key-and-value`, `:key-or-value`, as in SBCL); `:key` entries are ephemerons, so
  a value that refers to its own key does not keep the entry. `make-weak-pointer`, `weak-pointer-value`,
  `finalize` (with `:dont-save`) and `cancel-finalization` follow SBCL. An object in the old generation is found
  dead only by a major collection, so a weak entry or finalizer may wait for one; `(gc)` forces it. Finalizers run at
  the next safe point after the collection, not during it. `eq` hash tables hash structure and CLOS instances to a
  single bucket (there are no address-based hashes in a moving collector), so a large `eq` table keyed by instances
  is slow, weak or not.

## Extensions

The Genera-flavoured environment (window system, presentations, editor, hypertext documentation, Inspector, cross-reference, profiler), the libraries for mathematics,
statistics and bioinformatics, and the safe subprocess layer are extensions and have their own manuals (`:doc`).
