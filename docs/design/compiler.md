# Compiler

Status: implemented in `lisp/compiler/` (written in NeoGenera Lisp; ~900 lines).

Two passes, `ANALYZE` then `GENERATE`, in four files:

| File | Contents |
|---|---|
| `00-defs.lisp` | structures (`cvar`, `clam`, `cblock`, `ctb`, `aenv`), opcode table |
| `10-analyze.lisp` | macroexpansion, scope analysis, AST construction |
| `20-codegen.lisp` | AST to bytecode, assembler |
| `30-driver.lisp` | `defun`/`defmacro`/… forms, `compile`, `eval`, `compile-file`, fasl loader, `disassemble` |
| `40-build.lisp` | helpers for the self-hosting build |

## Pass 1: analysis

Source forms become an AST of lists (`(:const v)`, `(:lref var)`, `(:call fn args…)`, …) in
which every lexical variable is a `cvar` structure. Analysis does all the semantic work:

* **Macros** are expanded with `macro-function`; `macrolet` is supported.
* **Special variables** (`defvar`, local `declare special`) are bound with `BIND`/`UNBIND`.
* **Lambda lists** are rewritten at the source level into plain required/optional/rest
  parameters plus prologue forms (`%unsupplied-p`, `%getf-key`, `%check-keys`).
* **Closures**: referencing a variable of an outer lambda marks it *captured* and adds it to the
  `free` list of every lambda between the use and the owner. Assigned variables are marked
  *mutated*. A variable that is both captured and mutated is **boxed** (a one-cell cons).
* **CATCH, UNWIND-PROTECT, MULTIPLE-VALUE-CALL, PROGV** become calls to primitives that take
  thunks (`%catch`, `%unwind-protect`, …), so the VM needs no special instructions for them.
* **BLOCK/RETURN-FROM and TAGBODY/GO** are first analysed *optimistically* as local jumps.
  If a `return-from` or `go` is found inside a nested lambda (a closure, or one of the thunks
  above) the analyzer throws, abandons that form and redoes it using dynamic frames
  (`%block`/`%return-from`; `tagbody` becomes a state machine over `block` and `labels`).
  Aborted attempts may over-approximate captured/mutated flags, which is safe.

## Pass 2: code generation

`GEN` leaves one value on the stack, or, in tail position, emits `RET`/`TAILCALL` itself.
The generator tracks the stack depth precisely, since a `let` addresses its variables by
frame slot.

* `let`: push all init values, assign slots, box captured+assigned variables, `BIND` specials;
  at exit `UNBIND` and `SLIDE` the locals away.
* Local `return-from`/`go` emit `UNBIND`, `POP`/`SLIDE` to the target depth and `JMP`.
* A `block` in tail position compiles its body in tail mode with a join label that `RET`s;
  this is what keeps `defun` bodies (which are wrapped in `block`) tail-callable.
* `labels` variables are boxed when captured, so mutually recursive closures see each other.
* Calls to `car cdr cons eq not null + - < > <= >= =` with the right argument count are open-coded.

## Definition forms and compile-time effects

`defun`, `defmacro`, `defvar`, `defparameter`, `defconstant` and `declaim` are macros that
expand into calls to `%defun`, `%defmacro-install`, `%defvar-init`, … . `defmacro`,
`defvar` and `defconstant` also take effect **at macroexpansion time** so later forms in the
same file see them. `compile-file` additionally evaluates `defstruct`, `deftype`, `defsetf`,
`in-package`, `defconstant`, `defmacro`, `declaim` forms at compile time (always on a
`copy-tree` of the form: the stage-0 evaluator memoizes macro expansions *in place* and would
otherwise rewrite the source being compiled).

## Fasl format

Text, one form per top-level form:
`(:code NAME NREQ NOPT REST (instruction words…) ((:k const) | (:c code) | (:u)…))`.
It is printed with package-qualified symbols and read back with the normal reader, which keeps it
diffable. The self-hosting check compares fasl files byte for byte.

## Tail position and dynamic bindings

At a tail position every dynamic (special) binding made in the lambda is still active. The generator
therefore emits `UNBIND n` before `RET`, and for a call in that position `CALL; UNBIND n; RETV`
(RETV returns without resetting the multiple-value count) instead of `TAILCALL`. A `RETURN-FROM` whose
block is in tail position leaves the function directly. This preserves multiple values through
`(let ((*special* ...)) (if x (values ..) ...))`. Only the dynamic-binding case loses proper tail
calls.

## Also compiled

* `symbol-macrolet` (used by `with-slots`, `with-accessors`); setting a symbol macro expands to `setf`.
* Lambda lists are stored in code objects when they contain only interned symbols, and
  source locations are written to the fasl (see `tools.md`).

## Known gaps

* A local `return-from` out of a scope that is **not** in tail position forwards only the primary value.
* No type inference or inlining beyond the open-coded primitives, no native code.
* `compile` of an evaluator closure loses its lexical environment.
* `declare ignore/type/optimize` and `&environment` are accepted but ignored.
