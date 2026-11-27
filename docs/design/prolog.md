# The Prolog Tool Kit

Status: implemented in `lisp/prolog/`. User documentation: `docs/manual/prolog-manual.md` (`:doc prolog-manual`).

Prolog integrated into the Lisp system in the way the Pascal and FORTRAN Tool Kits integrate those languages, with a different
implementation strategy: Pascal and FORTRAN *compile to Lisp*; Prolog is *interpreted by a machine written in Lisp* whose terms are
Lisp objects. The files carry a `pl` infix because the build names compiled files by their base name, which must be unique across
directories.

| File | Contents |
|---|---|
| `00-pl-terms.lisp` | atoms (symbols of `PL-ATOMS`), `PLV`/`PLS` structures, the trail, unification, copying, standard order, errors, Lisp conversion |
| `10-pl-reader.lisp` | flags, the operator table, tokens, the operator-precedence parser, reading one clause from a string or a stream |
| `15-pl-writer.lisp` | `write`/`writeq`/`print`/`write_canonical`, `portray_clause`, the `format/2` engine |
| `20-pl-engine.lisp` | predicates and clauses, template compilation, the machine, control constructs, exceptions, the solving API, the tracer, DCG translation |
| `22-pl-arith.lisp` | arithmetic evaluation |
| `25-pl-builtins.lisp` | control registration, type checks, term inspection, atoms and strings, lists, global variables, all-solutions, the database |
| `30-pl-io.lisp` | streams, term input and output, messages, flags and operators, statistics, listing, tracing predicates |
| `40-pl-toolkit.lisp` | warnings, consulting, the top level, the Lisp interface, the `PROLOG` package, systems, Listener commands, loading the library |
| `50-pl-editor.lisp` | PROLOG mode |
| `library.pl` | the library in Prolog, embedded at build time (like the manuals) |

## Terms

A term is a Lisp object. Atoms are symbols interned in the package `PL-ATOMS` (case preserved, so `foo` and `Foo` are different symbols
and `[]` is `PL-ATOMS::|[]|`); integers are Lisp integers; floats are doubles; strings are Lisp strings; a variable is a `PLV` whose
`REF` is `NIL` while unbound; a compound is a `PLS` holding a functor symbol and a simple-vector (lists are `'.'/2`); anything else is an
opaque atomic blob. `PL-FROM-LISP` and `PL-TO-LISP` convert at the boundary (`NIL` is `[]`, `T` is `true`, symbols map to lower-case
atoms and back to upper-case symbols).

Nothing recurses on the last argument of a compound (unify, copy, compare, build, write all loop), so a list of a million elements is
safe; the Lisp stack (about 10,000 frames in this system) only limits nesting in the other argument positions.

## The trail and the epoch

There is one global trail. A binding is trailed only when the variable is older than the youngest choicepoint. Every variable carries
the *epoch* at which it was made (a counter bumped by every choicepoint push) and `*PL-HB*` holds the epoch of the youngest
choicepoint; `PL-BIND` trails when `(< (plv-epoch v) *pl-hb*)`. Popping or cutting a choicepoint lowers `*PL-HB*` to the new top's epoch,
so a deterministic computation leaves nothing on the trail and long loops run in constant trail space. Sub-machines (`findall`, `bagof`, `setof`, `aggregate_all`, `with_output_to` and the
Lisp solving API) push a barrier choicepoint first, so every variable older than the sub-run is trailed and the
sub-run can be undone completely.

## Clauses

`PL-COMPILE-CLAUSE` turns a clause into templates: a variable is a `PLTV` (an index into a per-call vector), a compound with variables
a `PLTC`, and any ground subterm is kept as a shared `PLS`. Head unification (`%PL-UNIFY-HEAD`) works directly against the head
templates: the first occurrence of a variable stores the goal's argument in the vector without creating anything. Body goals are
templates too and are built (`%PL-BUILD`) only when they run. The body is flattened at conjunctions into a list of goals; variable goals are
wrapped in `call/1` at compile time so that cut inside them is local. Each clause records the key of its first argument (an atomic
value or functor and arity), and `%PL-NEXT-CLAUSE` looks ahead for the next matching clause so that no choicepoint is pushed when the
current one is the last candidate: list recursions are deterministic.

**All variables of a clause body are created at clause entry** (`%PL-TRY-CLAUSE`), after the head is unified and before any body
goal runs. The binding vector is shared by all frames of the body and survives backtracking into goals of the body; a variable made
lazily after a choicepoint would be newer than that choicepoint, so binding it would not be trailed, and after backtracking a retried goal
would find the slot holding a variable still bound on the abandoned branch. (This was a real bug, found by the zebra puzzle and now
covered by `prolog-body-variables-and-backtracking`.)

The database keeps each predicate's clauses in a list with a tail pointer; every clause has `born` and `died` generations, and a call
sees the clauses with `born <= generation < died` for the generation current when it started (the logical update view). Retracting
unlinks the clause but the iterating goal's current cell keeps its `cdr`, so it continues unharmed. Predicates are found through the
symbol's property list (`%pl-preds`, an alist by arity); library predicates carry a flag, and a user clause for one wipes the library
definition and takes over.

## The machine

A machine (`PLM`) has a continuation (a chain of `PLFR` frames: a goal template or term, the binding vector, the *cut barrier* and the next
frame) and a stack of choicepoints (`PLCP`, linked by `PREV`). `%PL-RUN` pops a frame, `%PL-EXEC` calls its goal and a failure is `%PL-BACKTRACK`.
Dispatch is in `%PL-CALL`: control constructs by keyword in `%PL-CONTROL`; Lisp built-ins (a function of the argument terms returning
true or false, or for non-deterministic ones a function returning a *generator* closure that is called once per solution and returns
`:LAST`, true or `NIL`); user predicates by clause selection.

* **Cut** is "set the choicepoint stack to the barrier": the barrier is the top of the stack when the predicate was called (before its
  own clause choicepoint is pushed). `call/N`, `\+`, the condition of `->` and `findall` establish their own barrier, so cut is local to them.
* **If-then-else** pushes an `:ALT` choicepoint for the else part, runs the condition with that choicepoint as its barrier, and a
  special `:CUTTO` frame after the condition cuts back to before the push and continues with the then part. `*->` marks the alternative
  dead instead of cutting. `\+` is `(G -> fail ; true)` with a `:CUTFAIL` frame.
* **catch/3** pushes a `:CATCH` choicepoint; a `:POPCATCH` frame deactivates it on exit (a trail function re-activates it when
  backtracking re-enters the goal). Exceptions are Lisp conditions (`PL-ERROR`); `%PL-DRIVE` catches them outside the loop (so the Lisp
  stack is unwound), `%PL-HANDLE-THROW` unwinds the choicepoint stack to an active catch whose catcher unifies with a copy of the ball, and
  if there is none the condition is re-signalled to the enclosing machine after the sub-run's bindings are undone. Lisp errors in
  built-ins become `error(system_error(Message), _)`; `halt` is a separate condition that passes through everything.
* **Sub-machines**: `findall/bagof/setof/aggregate_all`, `with_output_to` and the solving API run a goal in a fresh `PLM` with a barrier
  (`PL-DO-SOLUTIONS` for all solutions, `PL-SOLVE-ONCE` keeping the bindings, `PL-PROVE` undoing them).
* **Tracing**: when `*PL-TRACE*` or a spy point is set, a user call pushes a `:TRACEFAIL` choicepoint (printing Fail when backtracked into) and
  its body ends with a `:TRACEEXIT` frame that prints Exit and pushes a `:TRACEREDO` choicepoint (printing Redo). The tracer never stops to
  ask.

## Reading and writing

The reader works on strings: `%PL-LEX-CLAUSE` tokenizes up to the end token (a full stop followed by layout), `PL-PARSE-TOKENS` is the
usual operator-precedence parser (prefix, infix and postfix operators from `*PL-OPS*`, with the standard handling of an operator as an
atom). Arguments and list elements are read up to priority 1200 but without a top-level comma or bar, as SWI-Prolog does. A syntax error
skips the clause and reading continues; the editor needs clause start and end offsets, so reading returns them. From a stream,
`PL-READ-CLAUSE-TEXT` cuts one clause's text out first.

The writer keeps the invariant that *what it writes quoted reads back as a variant of the term*: operator atoms as operands are bracketed,
a prefix operator whose operand is an operator atom is written canonically (`-(+)`), a space follows a prefix operator before a parenthesis
so that it is not read as functional notation, `- 1` keeps its space, quoted `'|'` stays an atom, and `[](..)` and `{}(..)` read back. The fuzz
test `fuzz-prolog` checks this on random terms. `format/2` collects each line in a buffer so that column stops (`~t ~| ~+`) can pad
the current segment.

## Consulting and the editor

`PL-CONSULT-STRING` reads, expands (`term_expansion/2`, DCG), checks singletons, adds clauses and runs directives; a predicate that was
defined from the same *source* (a file name or a buffer name) before is replaced when its first clause is met, which is what makes both file
reconsulting and region compilation in the editor replace exactly the predicates concerned. Warnings go to the compiler-warnings database
(`PLWARNING`: source, line, column, message, kind) as in the Fortran Tool Kit. PROLOG mode scans clauses with the reader (cached per buffer
text) into `PLUNIT`s, groups consecutive clauses of one name and arity into predicates for compilation, listing and the changed-definition
commands, and indents by the bracket stack from the start of the clause.

## Limits and resources

The kernel does not survive exhausting its heap (a fatal signal or an exit, not a Lisp error), so the Tool Kit keeps memory under control itself.
`%PL-RUN` counts frames and every 2048 steps calls `%PL-CHECK-MEMORY`: when the heap in use is above the flag `stack_limit` (128 MB) plus a margin,
it forces a full collection and, if the live data is still above the limit, raises `resource_error(memory)`. Built-ins that allocate from a
user-supplied count (`length/2`, `numlist/3`, `functor/3`, `tab/1`, the repeat counts of `format`) are capped at a million, and the copy and
resolve loops stop after three million cells so that a cyclic term cannot be copied until the heap is gone. `sub_atom/5` and `sub_string/5`
produce candidates lazily. List traversal (`PL-LIST-TO-LISP`, `PL-LIST-P`) detects cyclic lists with Brent's method. Unification, comparison
and `ground/1` do not check for cycles. `fuzz-prolog` found each of these (it runs mutated goals and random terms through every text and term
built-in under ASan).

## Not done

Modules and the module-qualified forms beyond ignoring `M:`; tabling; coroutining and attributed variables; dicts; threads; stream
positions, binary streams and most of `stream_property`; `setup_call_cleanup/3` runs its goal as `once`; `setarg/3` on terms shared with
clauses; garbage collection of atoms. Speed is about 70,000 logical inferences per second on the bytecode machine (`bench(500)` in
`nrev`), which is adequate for programs of the size this Tool Kit is meant for; the obvious next steps are caching the predicate in
goal templates and compiling clause bodies to closures.
