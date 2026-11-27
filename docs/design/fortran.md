# The FORTRAN Tool Kit

Status: implemented in `lisp/fortran/`. User documentation: `docs/manual/fortran-manual.md` (`:doc fortran-manual`).

A FORTRAN 77 compiler that, like the Symbolics FORTRAN of Genera, produces Lisp: the system compiler turns that into bytecode,
so FORTRAN shares Lisp's integers, numeric library, debugger and editor. It follows the structure of the Pascal Tool Kit
(`docs/design/pascal.md`); the files carry an `ftn` infix because the build names compiled files by their base name, which must
be unique across directories.

| File | Contents |
|---|---|
| `00-ftn-lexer.lisp` | fixed-format cards, logical statements (continuations, INCLUDE), `%f7-squeeze`, tokens |
| `10-ftn-parser.lisp` | statement classification and parsing to `FNODE`s; program units (`FAST`) |
| `20-ftn-runtime.lisp` | storage spaces, undefined checks, arithmetic, characters, bit operations, the conditions |
| `25-ftn-io.lisp` | number formatting, FORMAT parsing, units and files, the formatted/list-directed/unformatted state machine |
| `30-ftn-compiler.lisp` | types, implicit typing, arrays, COMMON and EQUIVALENCE layout, expressions, intrinsics, calls |
| `35-ftn-statements.lisp` | declarations, the statement tree, statements, input/output, DATA, units, `f77-translate` |
| `40-ftn-toolkit.lisp` | programs, `f77-execute`, compiling strings and files, warnings, libraries, the `F77` package, commands |
| `50-ftn-editor.lisp` | FORTRAN mode |

## Reading source

`f77-read-statements` turns fixed-format cards into logical statements (`FSTMT`: label, text of columns 7-72, byte range, line).
Continuation cards are joined (the earlier card is padded to column 72 so a character constant can run across cards), `INCLUDE`
statements are spliced in at read time, and the editor's byte offsets are those of the first and last cards. A statement is then
*squeezed* (blanks removed outside character constants, upper-cased, Hollerith turned into quoted strings) and classified:
first as a DO statement, then as an assignment (`name[(...)][(...)] =` at parenthesis depth 0), then by its leading keyword.
That order is what makes FORTRAN's lack of reserved words work. Expressions are lists; statements are `FNODE`s; a unit is a
`FAST` holding its statements. Syntax errors are collected per statement.

## Representation and storage

Every variable is one entry of a simple vector at an integer offset. There are two spaces per frame: numbers and characters.
`INTEGER`/`REAL`/`LOGICAL` take one entry, `DOUBLE`/`COMPLEX` two, `DOUBLE COMPLEX` four, and `CHARACTER*n` takes *n* entries of
character space, its string value living in the first. Values are Lisp integers (unbounded), doubles (a `REAL` is rounded to
24 bits when stored by `%f7-single`), Lisp complex numbers, `T`/`NIL` and strings. An entry nothing was assigned holds
`f77-undefined`; reads go through `%f7-rd`, which signals the error that names the variable.

* **Locals** are static: `%f7-frame` returns the routine's pair of vectors from the running program's environment (`FENV`),
  creating them (and filling with typed zeros under Init to Zero, then running DATA initialisation) the first time.
* **COMMON blocks** are pair-of-vector entries of the environment, grown to the largest declaration; `%f7-common` returns them.
* **Arguments** are references `(vector . offset)`. A variable is passed as `(cons vec off)`, an array as its base, an array
  element as its address (so an array dummy sees a section of the actual), an expression as a fresh one-entry box, and a
  procedure as its symbol or a function object. `%f7-arg` boxes plain Lisp values from Lisp callers.
* **EQUIVALENCE** merges groups that share a variable and lays each out by offset (or at its COMMON member's address).
* Array subscripts compile to `%f7-ix`, which checks the bounds and gives the zero-based offset; offsets are multiplied by
  the element's width in entries. Adjustable and assumed-size dummies bind their bounds in the prologue.

## Code generation

`f77-translate` parses, then compiles each unit with `%fc-compile-unit`:

1. dummy arguments and the function result get variables; the declaration statements are applied in order (`%fc-declare-all`),
   statement functions recognised before the first executable statement;
2. COMMON blocks and EQUIVALENCE groups are laid out; DATA is turned into initialisation forms;
3. the executable statements become a tree (`%fc-build-tree`): block IF with ELSE IF/ELSE, labeled DO (several loops may share
   one terminal statement), ENDDO, DO WHILE; each sequence compiles to a `TAGBODY` whose tags are the statement labels, so
   GO TO, computed and assigned GO TO, arithmetic IF and alternate returns are plain `go`s (only jumps out of, never into,
   blocks compile, as FORTRAN requires);
4. the prologue is wrapped around the body: the frame, COMMON bindings, dummy bindings, adjustable-bound variables, `DATA`
   initial values (when the frame is new).

A main program becomes `(%f77-define-program name package lambda file)`, a subroutine or function `(defun NAME (args...) ...)` in
the program's package (never an inherited Lisp symbol: such a name gets a `%` suffix), and a BLOCK DATA unit
`(%f77-define-blockdata ...)`. Calls are `(%f7-call 'name refs...)`, which also finds *additional externals*.
Every statement sets `*f77-line*`; a routine binds `*f77-routine*`, so run-time errors (and Lisp errors, rewrapped by
`f77-execute`) name the routine and line. `*f77-debug-info*` turns this off. `*f77-signatures*` records each compiled routine's
argument and result types for the call checks of units compiled later (the stale entry of a routine being recompiled is removed
first).

DO loops use FORTRAN's iteration count, computed once (`%f7-trips`); the DO variable is set at the top of each iteration and
holds start + trips * step afterwards. Arithmetic uses Lisp's generic operators once the operand kinds are known at compile
time; integer `/` truncates, `**` has integer and real forms, and complex numbers are Lisp's.

## Input and output

A READ/WRITE/PRINT compiles to `%f7-begin`, one `%f7-put` or `%f7-get` per item as the compiled loops (implied DOs, whole
arrays) reach it, and `%f7-end`; END=, ERR= and IOSTAT= are handled by `%f7-guard` around the whole statement. The state
(`FST`) walks the parsed format as items arrive, so an implied-DO bound read by the same statement works, and applies format
reversion when the format runs out. Units (`FIO`) are the console, files held as vectors of records (read when opened,
written when closed, so REWIND, BACKSPACE, ENDFILE and REC= are simple), and internal files.
Number formatting works on exact rationals (`%f7-decompose`) so rounding is always half away from zero on the true value.

## Editor integration

FORTRAN mode uses the mode properties of the Pascal mode (`mode-keymap`, `mode-name`, `line-syntax`,
`after-self-insert`) and `*auto-mode-alist*`. It parses the buffer with `f77-parse` (cached on its text) for routines and
statements; indentation and wrapping work on the cards themselves, so they work on text that does not parse. Commands that
other modes also define (`compile-region`, `compile-buffer`, `compile-file`, `compiler-warnings`, `set-package`,
`edit-changed-definitions-of-buffer`, ...) are *chained*: `%f77-chain` keeps the previous definition and runs it in
non-FORTRAN buffers. Changed-definition baselines are kept per buffer (`f77-reset-baselines`, called from
`buffer-reset-baselines`).
