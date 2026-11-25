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
* No complex numbers in the numeric tower. A library type (`complex`, `c+`, `c*`, ...) provides them, and `sqrt` and `log` return it for negative reals.
  The operators `+ - * /` accept only reals.
* Floats are a single type (double); there are no single-float, short-float or long-float distinctions, and `float` takes no prototype.
* Division by zero with `/` signals `division-by-zero` for floats too; infinities and NaN arise from overflow and `+infinity+`, `+nan+`.
* `*print-base*` and `*print-radix*` are not honoured by the printer; use `~B ~O ~X ~R`.

**Strings and characters**
* Strings are byte vectors holding UTF-8; `length` counts bytes, `char` indexes bytes, and `char-code` of a non-ASCII byte is the byte value.
  Non-ASCII text round-trips but is not character-addressable. Case conversion and `alpha-char-p` are ASCII-only.
* `string<`, `string>`, `string<=`, `string>=` and `string/=` return a boolean rather than the mismatch index, and do not take `:start`/`:end` keywords.
* `format` lacks `~E ~G` field-width options beyond digits, `~<..~:>` justification, `~/fn/`, `~T` tabulation and `~(`...`~)` with nesting corner cases;
  `~R` supports cardinal, ordinal, Roman and radix forms only.

**Arrays**
* Only one-dimensional arrays (vectors and strings) exist. `make-array` with a list of dimensions makes a vector of the first dimension; the
  matrix type of the numerical library is the substitute. No displaced arrays, no `array-dimensions`, no specialised element types (the `:element-type` is accepted and ignored except for strings).
* `arrayp` is true of vectors and strings only.

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
* The interactive debugger with restarts runs in the line-mode Listener. The window Listener shows errors with a backtrace after the evaluation has unwound.
* Missing: `with-condition-restarts`, `invoke-restart-interactively`, return-from-frame and retry in the debugger.

**Packages, reading and printing**
* Packages are simple: `defpackage`/`in-package` exist but package locks, `shadowing-import` subtleties and nicknames are minimal. Most library symbols live in `COMMON-LISP` (they are not exported from it in the strict sense).
* The reader and printer are written in C; readtables are not extensible. `#S` structure syntax, `#+`/`#-` feature expressions beyond simple features and `#.` (controlled by `*read-eval*`) are supported; `#C`, `#P`, `#A` and user reader macros are not.
* `pprint` is a line-filling printer without `pprint-logical-block`.

**System interface**
* `directory` is not provided; use `run-program` or the Dired commands. `with-open-file` supports `:direction`, `:if-exists`, `:if-does-not-exist` for text files only.
* There are no threads, no weak hash tables, no finalization, and the garbage collector is a non-generational two-space copier.

## Extensions

The Genera-flavoured environment (window system, presentations, editor, hypertext documentation, Inspector, cross-reference, profiler), the libraries for mathematics,
statistics and bioinformatics, and the safe subprocess layer are extensions and have their own manuals (`:doc`).
