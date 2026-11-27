# Bytecode virtual machine

Status: implemented in `src/vm.c` (interpreter) and `lisp/compiler/` (code generator).

## Machine model

A stack machine over the shared value stack `vstack`. A call pushes the function, then the
arguments; the callee's frame base is the first argument. Locals (including `let`-bound
variables) occupy slots above the arguments and are addressed `base + n`.

VM-to-VM calls and returns do not recurse in C (explicit `vframes` stack), so tail calls are
real and deep recursion is bounded only by the value stack. A call into a primitive or
evaluator closure recurses in C through `apply_stack`.

`&optional` parameters that were not supplied receive the `UNSUPPLIED` marker; `&rest`
collects the remaining arguments into a list. `&key`, defaults and supplied-p variables
are compiled into the function prologue (they are source-level rewrites, not VM features).

## Multiple values

`values` stores its arguments in `mv_vals` and sets `mv_count`. Every `RET` resets the count
to 1; a `TAILCALL` forwards whatever the callee set. The compiler therefore **must** compile
a call in tail position as `TAILCALL`, including the last form of a function body inside
`block`. Consumers (`multiple-value-call`, `%multiple-value-call`) read the registers
immediately after the call returns.

## Instruction set

Operands are `int32` words following the opcode.

| Op | Operands | Effect |
|---|---|---|
| `CONST k` | constant index | push constant |
| `LREF i` / `LSET i` | frame slot | push local / pop into local |
| `FREF i` | free-variable index | push captured variable from the closure |
| `GREF k` / `GSET k` | constant index of symbol | global (special) variable access |
| `GFUN k` | symbol constant | push the symbol's function (error if undefined) |
| `POP`, `DUP` | | |
| `JMP t`, `JNIL t`, `JNNIL t` | address | jumps (`JNIL`/`JNNIL` pop the test) |
| `CALL n`, `TAILCALL n` | argument count | function and `n` arguments are on the stack |
| `RET` | | return top of stack |
| `CLOSURE k n` | code constant, free count | pop `n` captured values, push closure |
| `BOX` / `UNBOX` | | wrap / read a one-cell box |
| `BOXSET` | | stack `box, value` becomes `value`; stores into the box |
| `BIND k` / `UNBIND n` | symbol constant / count | dynamic binding |
| `SLIDE n` | | drop `n` values *below* the top (used at `let` exit) |
| `CAR CDR CONS EQ NOT` | | open-coded primitives |
| `ADD SUB MUL LT GT LE GE NUMEQ` | | open-coded two-argument arithmetic with fixnum fast path |
| `ADDFX SUBFX MULFX INCFX DECFX` | | fixnum arithmetic with no tests; wraps modulo 2^61 (see below) |
| `LTFX GTFX LEFX GEFX NUMEQFX` | | fixnum comparison with no tests |
| `LOGANDFX LOGIORFX LOGXORFX` | | bitwise operations on fixnums |
| `REMFX MODFX` | | fixnum `rem`/`mod`; signal `Division by zero` |
| `ASHFX n` | shift | fixnum shift by the constant `n` (negative: right) |
| `ADDFL SUBFL MULFL DIVFL` | | flonum arithmetic on two flonums (`DIVFL` signals `Division by zero`) |
| `LTFL GTFL LEFL GEFL NUMEQFL` | | flonum comparison |
| `FIXTOFL` | | fixnum to flonum |
| `CARX CDRX` | | `car`/`cdr` of a value known to be a cons |
| `SVREF SVSET SCHAR` | | `aref`/`svref`, `(setf aref)`/`%aset` and `char` with one subscript; a simple vector or string in range takes the fast path, anything else the generic `aref` |
| `SVREFX SVSETX` | | the same with the vector, the fixnum index and the range all trusted (`safety 0`) |
| `CHKFIX` | | signal a type error unless the top of stack is a fixnum (it stays) |
| `CHKTYPE tc` | type code | the same for `integer double-float real number cons list symbol null string simple-vector character` |
| `CHKRANGE k` | constant | the same for a fixnum in the range `(lo . hi)` held in constant `k` |

The `…FX`, `…FL`, `CARX`, `CDRX`, `SVREFX` and `SVSETX` instructions do no checking of their own: the compiler emits them only
where operand types are established (see compiler.md, "Declarations, policy and type specialisation"). `ADDFX`, `SUBFX`,
`MULFX`, `INCFX`, `DECFX` and `ASHFX` wrap modulo 2^61 on overflow.

`(+ a b c)` folds into `ADD ADD`; `1+`/`1-` become `CONST 1; ADD/SUB`. Open-coding assumes the
standard definitions of these functions are not redefined.

## Writing instructions by hand

`defasm` (docs/design/asm.md, manual `:doc bytecode-manual`) assembles routines from instruction lists. It verifies stack depth, frame
slots, jump targets, dynamic bindings and the operand types of the unchecked instructions before building the code object, so a
hand-written routine cannot violate the assumptions this document makes ("the compiler emits them only where operand types are
established").

## Safe points

`CALL` and `TAILCALL` poll `gc_pending`. After a collection or after any call that may have
run Lisp code, the interpreter loop reloads its cached `ip` / `consts` pointers from the
current frame (`RELOAD`), because the code object may have moved.

`CALL`, `TAILCALL` and backward `JMP`s also poll `ng_attention`, the request to switch processes
(preemption, an interrupt or an arrest; see `processes.md`). Another process may collect while
this one is switched out, so the loop reloads afterwards as it does after a collection.
