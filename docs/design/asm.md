# The Bytecode Assembler

Status: implemented in `lisp/asm/` (no C changes). User documentation: `docs/manual/bytecode-manual.md` (`:doc bytecode-manual`).

`defasm` lets a user write a function as raw instructions for the VM of `docs/design/vm.md`. The design question is memory
safety: the interpreter loop (`src/vm.c`) does no checking of stack depth, frame slots or the operand types of the specialised
instructions, because the compiler guarantees them. The assembler therefore has to guarantee the same things for hand-written code.
The answer is a verifier in the style of a class-file verifier, run on every routine before a code object exists.

| File | Contents |
|---|---|
| `00-asm-instructions.lisp` | the instruction reference table (operands, stack effect, group, text), mnemonic lookup, help |
| `10-asm-assembler.lisp` | `asm-error`, parsing, the verifier, emission, `asm-assemble`, `defasm` |
| `20-asm-listing.lisp` | `%asm-decode`, `asm-decompile`, `asm-listing` |
| `50-asm-editor.lisp` | ASM mode: syntax colouring, formatting, compile/diagnostic commands |

Files carry an `asm` infix because the build names compiled files by base name (see the FORTRAN notes).

## Pipeline

1. **Parse** (`%asm-parse-items`): each item becomes an `asmi` record (index, source form, kind, operands). Kinds are the opcode
   keywords plus `:label`, `:var`, `:doc` and `:directive`. Operand counts and shapes are checked here (`%asm-check-operands`);
   `chktype` names become type codes. The lambda list is limited to required, `&optional` and `&rest` plain variables, because
   `&key` and defaults are compiler rewrites, not VM features.
2. **Nested lambdas** (`closure` operands) are assembled first, recursively, with their captured-variable count taken from the
   `closure` instruction; their errors are re-signalled at the `closure` item.
3. **Verify** (`%asm-verify`, `%asm-step`): a work-list abstract interpretation. The state at an instruction is the stack as a list of
   types (top first; the parameters are the initial `:any` slots) plus the number of dynamic bindings. Merging two states requires
   equal depth and binding count and joins types pointwise (`:any` unless equal). The lattice is `:any :fixnum :flonum :cons`, so
   the iteration terminates quickly. `(:var name)` binds a name to the slot (`depth - 1`) at that point; names are resolved while the
   flow runs, so a use that no binding dominates is an error. Instructions that trust their operand types demand `:fixnum`, `:flonum`
   or `:cons` unless the routine is `(:unsafe)`; `svrefx` and `svsetx` cannot be proved and need `(:unsafe)`.
4. **Emit** (`%asm-emit`): addresses, then words, with a constant vector that shares equal symbols and numbers (`eql`); `%make-code`
   and `%code-closure` build the function. Unreachable instructions are emitted (with a warning) so listings round-trip, but they
   cannot execute: reachability is exactly what the flow computed.

## What is and is not guaranteed

Without `(:unsafe)`: no stack underflow or growth beyond what the verifier saw, no read or write outside the frame's slots, jumps only
to instruction boundaries, no falling off the end, balanced dynamic bindings at `ret`/`retv`/`tailcall`, and every unchecked
instruction applied to operands of the type it assumes. Run-time errors the VM itself signals (undefined function, unbound variable,
division by zero, wrong argument count, a failed `chkrange`) remain ordinary Lisp errors. With `(:unsafe)`, only the first group of
checks (stack, slots, jumps, bindings) holds; a wrong type assumption can crash the process.

Two choices worth knowing: `ret` is allowed to leave values under the result (the VM discards the frame) but warns when it leaves more
than the parameters, because that is usually a miscount; and `lset` may not target the slot being popped, since that is always a bug.

## Listing and decompiling

`%asm-decode` reads the instruction words, makes labels `L<address>` at jump targets, names parameters from the declared lambda
list when it is plain (else `p0`, `p1`...), turns `closure` constants that are code objects into nested `(:lambda ...)` operands (with
their own `(:unsafe)` where their own verification needs it), `chktype` codes back into names and `chkrange` constants into bounds.
`:unsafe` is decided by trial: a routine whose decoded items do not assemble gets the directive. A top-level function that uses
`fref` (it captured variables when it was made) cannot be re-assembled on its own and the listing says so. Unreadable constants (a
function object, say) are listed as `nil`.

Property tested in `tests/lisp/asm-tests.lisp`: for a sample of the system's compiled functions, listing, reading the text, assembling
and listing again gives the same text. The whole-system run (all ~4,700) was clean when this was written.

## ASM mode

Line-oriented rather than parse-oriented (compare Pascal mode): `%asm-line-depths` computes paren depth and string state for each line;
`%asm-target-column` gives a column by role (depth 0: 0; depth 1: label or directive or documentation 2, instruction 4; deeper: Lisp
indentation via `lisp-indent-for-line`); `%asm-format-line` also collapses blanks outside strings and aligns the trailing comment.
Compiling reads each top-level form with offsets (`%asm-read-defasm`), assembles `defasm` forms itself so an `asm-error`'s item index maps
to a buffer offset, and evaluates other forms. Diagnostics live in `*asm-diagnostics*`, listed by `C-c C-w` through the editor's
possibilities list. `compile-region`, `compile-buffer` and `edit-compiler-warnings` are chained (`%asm-chain`) like the other language
modes: in an ASM buffer they assemble, elsewhere they do what they did before.

## Not done

* `defasm` assembles when the form is evaluated, not at macroexpansion time, so `compile-file` stores the assembly and a fasl assembles
  again at load. Code objects are not embedded in fasl files by this path.
* Types are not tracked through calls, `fref` or global variables (all `:any`); a captured box needs `(chktype cons)` before `unbox`.
* No macros or pseudo-instructions beyond `:var`; the assembler is deliberately a thin layer over the machine.
