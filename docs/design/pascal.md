# The Pascal Tool Kit

Status: implemented in `lisp/pascal/`. User documentation: `docs/manual/pascal-manual.md` (`:doc pascal-manual`).

A Pascal compiler that, like the Symbolics Pascal Tool Kit, produces Lisp: the system compiler turns that into bytecode,
so Pascal shares Lisp's data, numeric library, debugger and editor.

| File | Contents |
|---|---|
| `00-lexer.lisp` | tokens (case-insensitive identifiers, `{ }` and `(* *)` comments, template items `⊂...⊃`, `%include`) |
| `10-parser.lisp` | recursive descent to a list AST `(kind start end . args)`; syntax errors are collected with recovery |
| `20-runtime.lisp` | the run-time library: checks, sets, values, files, `write` formatting, interactive input |
| `30-compiler.lisp` | types, scopes, checking and code generation |
| `40-toolkit.lisp` | programs, `pascal-execute`, compiling strings and files, warnings, `def-program`, commands, package `PASCAL` |
| `50-editor.lisp` | Pascal mode |

## Representation

Integers are Lisp integers and sets are integers used as bit masks (`logior`, `logbitp`), so neither has a size limit.
Reals are doubles, booleans `T`/`NIL`, characters Lisp characters, enumerations their ordinals. Arrays are simple
vectors indexed from 0 (the compiler subtracts the lower bound, after a range check); records are simple vectors with the
type's name in element 0, so `%pcopy` (value semantics on assignment and value parameters) is a deep `copy-seq` of nested
simple vectors, and typed files can print and read them. A pointer is a `pbox`; `dispose` marks it so a dangling pointer
is detected. An undefined variable holds the symbol `pascal-undefined`; reads of scalar variables are wrapped in `%pchk`,
an inline `eq` test.

## Code generation

Every AST node carries its source offsets; errors become `pascal-syntax-error` conditions with line and column and are
collected per statement and declaration so one compilation reports many.

* A **main program** P becomes `(%pascal-define-program "P" package (lambda () body) globals ...)`. Its variables are
  special variables `X<-P` (declared by `defvar` before its routines are compiled); `pascal-execute` binds them with
  `progv` to fresh values each run (or the saved ones, for `:save-environment`).
* A **top-level routine** F becomes `(defun F<-P ...)`. Since calls go through symbols, compiling one routine alone
  (`pascal-translate :only-at`) replaces it everywhere: the editor's incremental compilation. **Nested routines** are
  `labels` functions inside their parent's body, closing over its variables.
* **VAR parameters** receive a reference `(getter . setter)` built by `%pc-ref`, which evaluates the containers and
  subscripts of the actual parameter once. **Procedural parameters** are function objects. **Conformant arrays** receive
  the array and its bounds as extra arguments.
* A routine binds `*pascal-routine*` and sets `*pascal-line*` before each statement, so run-time errors (and Lisp errors,
  rewrapped by `pascal-execute`) name the routine and line. `*pascal-debug-info*` turns this off.
* `goto` becomes `go` within a `tagbody` made for any statement sequence holding labels (non-local gotos work because the
  Lisp compiler handles `go` across closures). Loops are `loop` inside `block %pleave`, with the body in
  `block %pcontinue` for Pascal/VS's `leave` and `continue`.
* **Units**: a specification's environment is kept in `*pascal-units*`, and its text goes into the compiled code so that
  loading a compiled unit in a fresh world rebuilds it; programs and implementations import it.

## Editor integration

Pascal mode uses the editor's mode properties (`mode-keymap`, `mode-name`, `line-syntax`, `match-offsets`,
`before-self-insert`, `after-self-insert`) and `*auto-mode-alist*`. Language-unit commands parse the buffer (cached on its
text) and work from the AST's offsets; indentation and construct matching scan code tokens line by line, using the
colouring's cached comment state, so they work on text that does not parse.
