# Compiler

Status: implemented in `lisp/compiler/` (written in NeoGenera Lisp).

Three passes, `ANALYZE`, `SPECIALIZE`, then `GENERATE`, in these files:

| File | Contents |
|---|---|
| `00-defs.lisp` | structures (`cvar`, `clam`, `cblock`, `ctb`, `aenv`), opcode table |
| `05-types.lisp` | type descriptors, declaration parsing, the compilation policy, `declaim`/`proclaim` |
| `10-analyze.lisp` | macroexpansion, scope analysis, AST construction |
| `15-specialize.lisp` | type inference, specialised instructions, inserted type checks, policy |
| `20-codegen.lisp` | AST to bytecode, assembler |
| `30-driver.lisp` | `defun`/`defmacro`/… forms, `compile`, `eval`, `compile-file`, fasl loader, `disassemble`, `dump-fasl` |
| `40-build.lisp` | helpers for the self-hosting build |
| `src/fasl.c` | the binary fasl encoder, decoder and checksum |

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
* Calls to `car cdr cons eq not null + - * < > <= >= =` with the right argument count are open-coded.

## Definition forms and compile-time effects

`defun`, `defmacro`, `defvar`, `defparameter`, `defconstant` and `declaim` are macros that
expand into calls to `%defun`, `%defmacro-install`, `%defvar-init`, … . `defmacro`,
`defvar` and `defconstant` also take effect **at macroexpansion time** so later forms in the
same file see them. `compile-file` additionally evaluates `defstruct`, `deftype`, `defsetf`,
`in-package`, `defconstant`, `defmacro`, `declaim` forms at compile time (always on a
`copy-tree` of the form: the stage-0 evaluator memoizes macro expansions *in place* and would
otherwise rewrite the source being compiled).

## Declarations, policy and type specialisation

Declarations are honoured. `analyze` parses them (`parse-declarations`, `05-types.lisp`) and records the result in the
tree; `specialize` (`15-specialize.lisp`) then walks the tree once, bottom-up, inferring a *ty* for every node and rewriting
it.

**What is recognised.** `(declare (type T v...))` and the shorthand `(declare (fixnum v...))` on `let`, `let*`, `do`,
`do*`, `dotimes` and every lambda list (required, `&optional`, `&key` and `&aux` variables); `the`; `(declaim (type T
*var*))`; `(declaim (ftype (function (...) result) f))`; `optimize`, `inline` and `notinline` anywhere (`declaim`,
`proclaim`, `declare` in a lambda, `let` or `locally`, scoped to that form); `special`. Anything else is accepted and
ignored, as before.

**Types.** A ty is `NIL` (no information), `(:int lo hi)` (a NIL bound is unbounded), or one of `:flonum :cons :null :list
:symbol :string :svec :char :real :number`. Only a type the VM can test exactly (or, for integer ranges, a superset)
becomes a ty: `fixnum`, `integer` and `(integer lo hi)`, `(unsigned-byte n)`, `(signed-byte n)`, `(mod n)`, `bit`, every
float type (there is only one float format), `real`, `number`, `cons`, `list`, `null`, `symbol`, `simple-string`,
`simple-vector`, `(simple-array * (*))`, `character`, and `or` of those. `string`, `vector`, user `deftype`s and the like
are ignored, because adjustable strings and vectors are different objects from the simple ones. Array element types are
never trusted (`make-array` does not specialise storage), only the container.

**Inference.** Constants, declared variables and `the` give types. A `let` variable that is never assigned takes the
type of its initial value. Integer results carry intervals, so `(+ a b)` of two `(integer 0 1000)` values is known to be a
fixnum, `(logand x 255)` lies in 0..255 even when `x` is unknown, and `(mod x 10)` of a fixnum lies in 0..9. `length` and
`char-code` have known result ranges; a `declaim`ed `ftype` gives a call its result type.

**Specialised instructions.** When operand types allow, `+ - * 1+ 1- < > <= >= = zerop plusp minusp logand logior logxor
rem mod ash` on fixnums become the `…FX` instructions, the same on floats (and mixed float/fixnum, converting the fixnum)
become `…FL` instructions, `car`/`cdr` of a known cons become `CARX`/`CDRX`, and `svref`, `aref`, `char`, `schar` and
`(setf …)` of them are open-coded (`SVREF`, `SCHAR`, `SVSET`; anything but a simple vector or string takes the generic
path, so behaviour is unchanged). A fixnum operation is only used when the result is *known* to fit a fixnum, or when the
context trusts it (below); otherwise the generic instruction, which falls back to bignums, is used.

**Policy.** The policy is an alist in `*policy*`: `(speed safety debug space compilation-speed)`, all 1 by default.
`safety` decides whether declarations are *checked* or *trusted*:

* `safety` 1, 2, 3: a declared parameter type is checked on entry (`CHKFIX`, `CHKRANGE`, `CHKTYPE`), a `the` is checked, a
  typed variable is checked when it is bound or assigned, a declaimed global variable when it is read or assigned, and a
  declaimed function result after the call (not on a tail call, which stays a tail call). A check that the compiler can
  prove redundant is not emitted. A failed check signals an ordinary type error.
* `safety` 0: declarations are trusted, no checks are made, and `(the fixnum (+ a b))` of fixnums may wrap modulo 2^61 and
  use the unchecked fixnum instructions. Typed fast instructions are used *only* where some declaration or check
  established the operand types; untyped code is never made unsafe. Declaring something that is not true under `safety 0`
  is undefined behaviour, as in other Lisps (a wrong `double-float` declaration can crash the process).
* `debug` 3: calls in tail position are not merged into jumps, so the caller stays on the stack for the debugger.
* `notinline` stops open-coding of the named functions.

`compile-file`, `load` and `load-fasl` rebind `*policy*`, so a `declaim` in one file does not leak into the next.

**Known limits.** A checked `the` (and a checked declared function result) delivers only the primary value of a
multiple-value form. `loop` clauses have no `of-type`. `ftype` does not type a function's own parameters. `dotimes` and
`do` hoist their variable declarations; a fixnum declaration for a `dotimes` variable also types its counter and limit.
There is no inlining and no native code; the gain is the removal of dispatch and call overhead, about 2-3x on tight fixnum
and float loops at `safety 0`, and none on code that is already dominated by calls.

## Fasl format

`compile-file` writes a **binary** fasl file by default (`:format :text` still writes the old, diffable text format;
`load` and `load-fasl` read either, recognising them by the first byte). The encoder and decoder are in `src/fasl.c`; the
Lisp side (`30-driver.lisp`) only collects entries and decodes them one at a time.

    bytes 0-7    7F 'N' 'F' 'A' 'S' 'L' 0D 0A      magic (the CR LF catches text-mode damage)
    byte  8      format version (3)
    bytes 9-11   zero
    bytes 12-15  entry count (u32 little-endian)
    entries      u32 length, then that many bytes: one encoded code object per top-level form
    last 4       CRC-32 of everything before

An object is a tag byte then data (LEB128 varints, zigzag for signed): NIL and T, fixnum, bignum (decimal text), ratio,
complex, flonum (8 bytes, so every float round-trips exactly, including -0.0), narrow or wide string, character, list (a run of
conses with its tail, so dotted lists work), simple vector, structure, multi-dimensional array, symbol (package name and
name, or uninterned) and code object. Each entry has its own symbol table (a repeated symbol is a back-reference, so the
identity of an uninterned symbol survives within an entry), which lets entries decode independently, in order, as the
file loads: an early form may create the package that a later form's symbols live in. Objects that cannot be written
(functions, streams, hash tables) signal an error at compile time, as they did in the text format.

The output depends only on the code objects, so the self-hosting fixed-point check still compares fasl files byte for byte.

**Reading is defensive.** `%fasl-entries` checks magic, version, checksum and the entry table before anything is decoded;
the decoder bounds every length by the bytes that remain and every nesting by the C stack check, requires each entry to be
consumed exactly, and verifies every instruction stream (`vm_validate_code` in `src/vm.c`): opcodes and operand counts,
constant indices, jump targets that land on instruction boundaries, no way to run off the end, `CLOSURE` constants that
are code, `CHKRANGE` constants that are fixnum pairs. It does not check stack depth, so a fasl file is still code: loading
one is as trusted as loading source (docs/SECURITY.md).

`(dump-fasl path)` disassembles every entry of a binary fasl file.

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
* No inlining and no native code (see "Declarations, policy and type specialisation" for what is specialised).
* `compile` of an evaluator closure loses its lexical environment.
* `&environment`, `declare ignore` and `dynamic-extent` are accepted but ignored.
