# The Bytecode Assembler

NeoGenera compiles Lisp into instructions for a small stack machine, the **bytecode virtual machine** described in
`docs/design/vm.md`. The Bytecode Assembler lets you write those instructions yourself, the way you would write assembly
language, and install the result as an ordinary function. Use it to see exactly what the machine does, to write a hot
loop by hand, to experiment with the instruction set, or to take code the compiler produced and edit it.

(Read this manual in the system with `:doc bytecode-manual`.)

The assembler has these parts:

* **`defasm`**, a definition form whose body is a list of labels and instructions, and **`asm-assemble`**, the function
  behind it;
* a **verifier** that checks every routine before it exists: stack depth, local slots, jump targets, dynamic bindings,
  and the types the unchecked instructions rely on. A routine that passes, and is not declared `(:unsafe)`, cannot corrupt the machine's
  stack or make it misread memory;
* **`asm-listing`** and **`asm-decompile`**, which turn any compiled function, yours or the compiler's, back into assembly
  source that assembles to the same code;
* **ASM mode** in the editor, with assembler-style layout, compile commands and diagnostics that point at the offending
  instruction;
* the instruction reference in this manual, which is generated from the same table the editor's instruction help uses.

## A first routine

```lisp
(defasm fact (n)
  "The factorial of N."
    (const 1) (:var acc)        ; acc is the slot just above the parameter
  top
    (lref n) (const 1) (le)     ; n <= 1 ?
    (jnnil done)
    (lref acc) (lref n) (mul) (lset acc)
    (lref n) (const 1) (sub) (lset n)
    (jmp top)
  done
    (lref acc)
    (ret))

(fact 20)    ; => 2432902008176640000
```

A routine is a lambda list and then **items**. An item is a **label** (a plain symbol such as `top`), an
**instruction** (a list whose first element names the instruction, such as `(lref n)`), or a **directive** (a list whose
first element is a keyword, such as `(:var acc)`). A string as the first item is the documentation. Everything else is a
mistake, and the assembler says so. Instruction names and labels are not case sensitive for the reader, and the
assembler recognises an instruction by its name, whatever package it was read in.

`(const 1)` pushes the number 1. `(:var acc)` gives the name `acc` to the value on top of the stack, so later
`(lref acc)` and `(lset acc)` mean that slot. `(le)` pops two numbers and pushes true or false, `(jnnil done)` pops that
and jumps if it is true, and `(ret)` returns the value on top of the stack. The rest of this manual explains the machine
well enough to write such routines, then lists every instruction.

## The machine

**The stack and the frame.** The machine has one value stack. A function's **frame** begins at its first argument. The
arguments, then everything the routine pushes, are the frame's **slots**, numbered from 0. `(lref 0)` pushes slot 0
and `(lset 0)` pops the top of the stack into slot 0. A local variable is just a slot you pushed a value into: push the
initial value, and give it a name with `(:var name)`. Every instruction pops its operands from the top of the stack and
pushes its result.

**Parameters.** With the lambda list `(a b)` the frame starts with `a` in slot 0 and `b` in slot 1, so a routine
begins with a stack depth of 2. With `&optional`, an argument that was not supplied arrives as the machine's
*unsupplied* marker, which you can test by calling `(%unsupplied)` and comparing with `eq`. With `&rest`, the remaining
arguments arrive as a list in the last parameter slot. `&key`, defaults and supplied-p variables are not machine
features, they are rewrites the compiler performs, and an assembly lambda list does not accept them.

**Calls.** To call a function, push the function, push the arguments in order, and `(call n)`. The function and its `n`
arguments are replaced by the first value it returned. `(gfun name)` pushes the function that the symbol `name`
names. To return the result of a call as your own result, use `(tailcall n)` in place of `(call n)` followed by `(ret)`:
the frame is reused, so a loop written as a tail call runs in constant stack.

**Returning.** `(ret)` returns the value on top of the stack and discards the rest of the frame, parameters included.
Anything left under the top is simply thrown away, so you do not have to clean up, but the assembler warns when a routine
leaves values beyond its parameters, since that is usually a miscount.

**Multiple values.** A function's values travel in registers beside the stack. `(ret)` returns exactly one. `(tailcall n)`
passes on whatever the callee returned, and `(retv)` returns the value on top of the stack *and keeps the multiple values
the previous call left in the registers*, so `(gfun values) (const 1) (const 2) (call 2) (retv)` returns two values.

**Dynamic bindings.** `(bind symbol)` pops a value and binds the special variable `symbol` to it until a matching
`(unbind n)` undoes the `n` most recent bindings of the routine. A routine must have unbound everything it bound by the time
it executes `ret`, `retv` or `tailcall`; the verifier checks this. If an error unwinds through the routine, the system
undoes the bindings for you.

**Closures and boxes.** `(closure (:lambda (params) ...) n)` makes a function from a nested routine and the `n` values
on top of the stack, which become the nested routine's **captured variables**: inside it, `(fref 0)` pushes the first.
A captured variable is a value, copied when the closure is made. To share a variable that changes, as the compiler does for
a variable that is assigned, capture a **box**: `(box)` wraps the top of the stack in a fresh one-cell box, `(unbox)`
reads it, and `(boxset)` stores into it. The nested routine may name its captured variables with `(:free total)`, after
which `(fref total)` means the same as `(fref 0)`. The example `make-accumulator` in `examples/asm/numeric.ngasm` is a
complete counter.

**Safe points.** Calls and backward jumps are places where the system may collect garbage, interrupt the routine or switch
to another process. A routine that loops without a call is still interruptible, because a backward `jmp` checks.

## Writing a routine

### The form

`(defasm name lambda-list . items)` assembles the routine when the form is evaluated and installs it as the global function
`name`; it returns `name`. `(asm-assemble name lambda-list items)` does the same without installing it: it returns the function
and a list of warnings. Both signal `asm-error` if the routine is not sound. The assembler keeps no state: assembling the same
items twice gives two equivalent functions.

Because `defasm` assembles when it is evaluated, a file full of `defasm` forms is an ordinary Lisp source file. `load` runs it,
and `compile-file` followed by `load` of the fasl works too (the fasl holds the assembly and assembles at load time).

### Operands

| Operand | Written as | Examples |
|---|---|---|
| a frame slot | a number, or a name | `(lref 0)`, `(lref acc)` |
| a captured variable | a number, or a name given by `(:free ...)` | `(fref 0)`, `(fref total)` |
| a constant | any datum, **not evaluated** | `(const 42)`, `(const "text")`, `(const foo)`, `(const (a b))` |
| a global | a symbol | `(gref *print-base*)`, `(gset *x*)`, `(gfun length)`, `(bind *x*)` |
| a jump target | a label in the same routine | `(jmp top)`, `(jnil done)` |
| a count | a non-negative integer | `(call 2)`, `(unbind 1)`, `(slide 3)` |
| a type | a type name | `(chktype cons)` |
| a nested routine | `(:lambda (params) items...)` | `(closure (:lambda (x) (lref x) (ret)) 0)` |

The operand of `const` is data, so `(const foo)` pushes the *symbol* `foo`, and `(const (quote x))` pushes the list `(quote x)`.
Equal numbers and symbols share one entry in the routine's constant vector.

### Directives

| Directive | Meaning |
|---|---|
| `(:var name)` | Name the slot of the value on top of the stack. Parameters already have the names in the lambda list. A name always means one slot: naming two different slots with it is an error. |
| `(:free name ...)` | In a nested `(:lambda ...)`: names for the captured variables, in order. The count must equal the number given to `closure`. |
| `(:unsafe)` | Skip the type proofs (see below) for this routine. The stack and control-flow checks still run. `(:unsafe nil)` turns it off. |

Each nested `(:lambda ...)` is a routine of its own and has its own directives; `:unsafe` in the outer routine does not
extend to it, and the other way round.

### Labels

A label is a symbol that stands on its own. It names the next instruction. Labels need not be used, may come one after
another, and must not be keywords, `nil` or `t`. A label that has the name of an instruction draws a warning (it is allowed).

## What the assembler checks

The verifier follows every path through the routine from the first instruction, tracking the stack as a list of types, the
way a class-file verifier does. It reports an error, and the routine is not installed, when:

* an instruction would pop more values than the stack holds (**stack underflow**);
* `lref` or `lset` names a slot that does not exist at that point, or `lset` would store into the value being stored;
* two paths reach the same label with different stack depths, or with different numbers of active dynamic bindings;
* execution can run off the end of the routine: the last reachable instruction must be `ret`, `retv`, `tailcall` or `jmp`;
* a jump names a label that is not defined, or a label is defined twice;
* `ret`, `retv` or `tailcall` is reached with dynamic bindings still in force, or `unbind` undoes more than were made;
* an operand is malformed: the wrong number of operands, a bad count, an unknown type name, empty bounds in `chkrange`;
* the routine uses an instruction the machine would not check by itself on operands whose type the assembler cannot
  establish, as described next.

Unreachable instructions are not an error, but draw a warning.

### Typed instructions and proofs

A handful of instructions do no checking of their own, because the compiler uses them only when it knows the operands.
Giving them the wrong kind of object would make the machine misread memory. The assembler therefore allows each of them
only when it can **prove** the type of its operands on every path:

| Instruction group | Operands that must be proved |
|---|---|
| `addfx subfx mulfx ltfx gtfx lefx gefx numeqfx logandfx logiorfx logxorfx remfx modfx incfx decfx ashfx fixtofl` | fixnums |
| `addfl subfl mulfl divfl ltfl gtfl lefl gefl numeqfl` | flonums |
| `carx cdrx unbox boxset` (the box) | conses |
| `svrefx svsetx` | cannot be proved: only in a routine declared `(:unsafe)` |

The assembler learns a type from:

* a literal: `(const 3)` is a fixnum, `(const 1.5)` a flonum, `(const (a))` a cons;
* a check: after `(chkfix)`, `(chkrange lo hi)` or `(chktype fixnum)` the value on top is a fixnum, after
  `(chktype double-float)` a flonum, after `(chktype cons)` a cons;
* the result of an instruction: `addfx` and the other fixnum arithmetic give fixnums, `addfl` and the other flonum
  arithmetic and `fixtofl` give flonums, `(cons)` and `(box)` give conses;
* a copy: `dup`, `lref` of a slot, and `lset` of a value into a slot carry the type along.

Where two paths meet at a label, a slot keeps its type only if both paths agree. A parameter is of unknown type until you
check it; the usual way is to check it once and store the checked value back, as `gcd2` does:

```lisp
(lref a) (chkfix) (lset a)      ; a is a known fixnum from here on, on every path that passes through this
```

If the routine is correct by construction and you want no checks, declare it `(:unsafe)`. The machine then trusts every type,
and a wrong guess can crash it, exactly as `(safety 0)` does in compiled Lisp.

### Errors and warnings

An `asm-error` carries a message, the routine's name, the offending item and the **index** of that item in the list of
items (`asm-error-message`, `asm-error-routine`, `asm-error-item`, `asm-error-index`). The editor uses the index to put the
cursor on the instruction. An error in a nested lambda is reported at the `closure` instruction that contains it. Warnings are
returned by `asm-assemble` as `(index . message)` pairs.

## Worked examples

All of these are in `examples/asm/numeric.ngasm` and run in `examples/15-bytecode-assembly.lisp`.

**A loop in fixnum arithmetic** (`gcd2`) checks both parameters once, then uses the unchecked `numeqfx` and `remfx`. The
three-instruction shuffle `(lref b) (lset a)` and `(lset b)` is the assembly-language way to write `a, b = b, a rem b`: the
remainder is already on the stack, so `(lset b)` stores it after `a` has taken `b`'s old value.

**A bounded recurrence** (`fib-fixnum`) shows `chkrange` establishing both the type and the range: after
`(chkrange 0 88)` the argument is a fixnum, so `numeqfx`, `decfx` and `addfx` are all accepted.

**A vector loop** (`vector-sum`) uses the checked `svref`, which takes a fast path for a simple vector in range and signals an
ordinary error otherwise, so it needs no proof and no `:unsafe`.

**A closure over a box** (`make-accumulator`) holds the running total in a box, which both the outer routine and every
closure share.

**A dynamic binding** (`scaled-by`) binds `*scale*` around a call and unbinds it before returning.

**A tail call.** This runs in constant stack for any `n`:

```lisp
(defasm count-up (n acc)
  (lref n) (const 0) (numeq) (jnil more)
  (lref acc) (ret)
  more
  (gfun count-up) (lref n) (const 1) (sub) (lref acc) (lref n) (add) (tailcall 2))

(count-up 100000 0)    ; => 5000050000
```

## Reading compiled code

`(asm-listing function)` prints the assembly source of any compiled function, or of a symbol that names one. Jump targets
become labels `L<address>`, the parameters keep their names (or become `p0`, `p1`... when the compiler's lambda list was
not plain), and each instruction's address is a comment. With `:addresses nil` the comments are left out. Called with a
stream it prints there; without one it returns the text.

```lisp
(defun triple (x) (* 3 x))
(compile 'triple)
(asm-listing 'triple *standard-output*)
; (defasm TRIPLE (X)
;     (const 3)                       ; 0000
;     (lref X)                        ; 0002
;     (mul)                           ; 0004
;     (ret)                           ; 0005
;     (ret))                          ; 0006
```

(The second `ret` is dead code the compiler leaves behind; assembling the listing draws a warning for it.)

`(asm-decompile function)` returns the same thing as a `defasm` form, as data. The listing is **faithful**: assembling it gives
a function whose listing is the same text. The assembler's test suite checks this against a large sample of the system's own
functions.

Code the compiler wrote under `(safety 0)`, and the compiler's own idioms for closures (a captured box is `fref` then
`unbox`, whose type nothing proves) cannot pass the verifier's proofs; their listings carry `(:unsafe)` on the routine that needs
it. A function that captured variables when it was made, such as a closure returned by a function, is listed with a comment: its
code uses `fref`, and it can only be assembled as a nested `(:lambda ...)` of a `closure` instruction.

The instruction numbers (opcodes) are not part of the interface and change when instructions are added. Write assembly, not
numbers: the names are stable.

The function `disassemble` also shows the instructions of a function, in a compact format with addresses and constants.

## ASM mode

Files whose names end in `.ngasm` open in ASM mode, as does any buffer after `M-x asm-mode`. The mode line shows
**(Assembly)**. The file is ordinary Lisp source made of `defasm` forms (and any other forms you like, such as `defvar` and
`defun`).

The mode keeps the layout assemblers use. Inside a `defasm`, labels and directives and the documentation string stand in
column 2, instructions in column 4, and the trailing comments line up in one column. Continuation lines of a long instruction
(a nested lambda, say) are indented as Lisp.

```lisp
(defasm square (x)
  "The square of X."
    (lref x)                            ; the argument
    (dup)
    (mul)
  done
    (ret))
```

| Key | Command |
|---|---|
| `TAB` | format the line: indent it by its role, collapse extra blanks, align its comment |
| `RET`, `C-j` | end the line and open a correctly indented one |
| `M-;` | align this line's comment, or start one |
| `C-c C-f` | format the region, or the whole buffer when there is no region |
| `C-M-q` | format the routine at point |
| `M-<tab>` | complete an instruction name; with several candidates, list them |
| `C-c ?` | describe the instruction at point: operands, stack effect, what it does |
| `C-c C-c` | assemble the region, or the routine at point |
| `C-c C-b` | assemble the whole buffer (other forms in it are evaluated as they come) |
| `C-M-x` | assemble the routine at point |
| `C-c C-w` | edit the assembler's diagnostics: go to the first, then `C-.` to the next |
| `C-c C-a` | show the assembly listing of the assembled routine at point, in a new ASM mode buffer |
| `C-c C-x` | call the routine at point, asking for the arguments (Lisp forms) |
| `M-.` | go to the routine or label named at point |
| `C-M-a`, `C-M-e` | beginning and end of the routine; `C-M-f`, `C-M-b`, `C-M-u`, `C-M-d`, `C-M-k` move over and kill forms |

A diagnostic is a pair of a place and a message. An error puts the cursor on the instruction at fault, not at the start of
the routine, and a routine with an error is not installed while the other routines in the buffer still are. A warning, such as
unreachable code, does not stop a routine being installed; `C-c C-w` lists both.

The colours: instructions and `defasm` in the built-in colour, directives as keywords, routine names and labels as names, comments
dimmed, numbers and `t` and `nil` as constants.

## Limits and notes

* A routine cannot call itself by label; call it through `(gfun name)` as `count-up` does.
* `gfun` signals an error at run time for a symbol that names a macro or nothing.
* The assembler does not let you build the raw instruction words or constants vector by hand. That is the point: everything it
  accepts passes the checks in this manual. `%make-code` exists, but it is what the assembler itself uses, and it does no checking.
* Typing is by proof, not by declaration: `(:unsafe)` is the only way to say "trust me".
* The verifier reasons about one routine at a time. A call is a call: it pushes one value of unknown type, whatever the callee
  declared.
* Not provided because the machine has no such feature: `&key` arguments, default values and supplied-p variables (the compiler
  rewrites them into code in the function's prologue; write that code yourself if you want it).

## The instruction reference

Stack effects are written `( before -- after )` with the top of the stack on the right. `bool` is `t` or `nil`.

### Stack and variables

| Instruction | Stack | What it does |
|---|---|---|
| `(const datum)` | `( -- x )` | Push the literal datum.  The operand is data, not a form: (const foo) pushes the symbol FOO. |
| `(lref slot)` | `( -- x )` | Push the value of frame slot SLOT.  A slot is a number or a name bound by a parameter or by (:var name). |
| `(lset slot)` | `( x -- )` | Pop the top of the stack into frame slot SLOT. |
| `(fref index)` | `( -- x )` | Push captured variable INDEX of the running closure (only in a (:lambda ...) with (:free ...)). |
| `(pop)` | `( x -- )` | Discard the top of the stack. |
| `(dup)` | `( x -- x x )` | Duplicate the top of the stack. |
| `(slide n)` | `( x1 ... xn y -- y )` | Drop the N values below the top of the stack, keeping the top. |

### Global variables, functions and dynamic binding

| Instruction | Stack | What it does |
|---|---|---|
| `(gref symbol)` | `( -- x )` | Push the global (special) value of SYMBOL; an error if it is unbound. |
| `(gset symbol)` | `( x -- )` | Pop the top of the stack into the global value of SYMBOL; an error for a constant. |
| `(gfun symbol)` | `( -- f )` | Push the global function named SYMBOL; an error if it is undefined or a macro. |
| `(bind symbol)` | `( x -- )` | Pop X and bind SYMBOL dynamically to it until a matching UNBIND. |
| `(unbind n)` | `( -- )` | Undo the N most recent dynamic bindings of this function. |

### Control transfer

| Instruction | Stack | What it does |
|---|---|---|
| `(jmp label)` | `( -- )` | Jump to LABEL.  A backward jump is a safe point where the process may be switched. |
| `(jnil label)` | `( x -- )` | Pop X; jump to LABEL if it is NIL. |
| `(jnnil label)` | `( x -- )` | Pop X; jump to LABEL if it is not NIL. |
| `(call n)` | `( f a1 ... an -- r )` | Call the function F with the N arguments above it; push its first value. |
| `(tailcall n)` | `( f a1 ... an -- )` | Call F with N arguments and return its values as this function's values; the frame is reused. |
| `(ret)` | `( x -- )` | Return X as the single value of the function. |
| `(retv)` | `( x -- )` | Return X and keep the multiple values the preceding call left behind. |

### Closures and boxes

| Instruction | Stack | What it does |
|---|---|---|
| `(closure (:lambda ...) n)` | `( v1 ... vn -- fn )` | Make a closure of the nested (:lambda ...) capturing the N values on the stack. |
| `(box)` | `( x -- box )` | Wrap X in a fresh one-cell box (a cons); the way mutated captured variables are held. |
| `(unbox)` | `( box -- x )` | Read the contents of a box.  The operand must be known to be a cons. |
| `(boxset)` | `( box x -- x )` | Store X in BOX and leave X.  BOX must be known to be a cons. |

### Lists

| Instruction | Stack | What it does |
|---|---|---|
| `(car)` | `( x -- y )` | CAR of a list (an error for an atom other than NIL). |
| `(cdr)` | `( x -- y )` | CDR of a list. |
| `(cons)` | `( a d -- c )` | Make a cons. |
| `(carx)` | `( c -- a )` | CAR of a value known to be a cons, with no check. |
| `(cdrx)` | `( c -- d )` | CDR of a value known to be a cons, with no check. |

### Comparison and equality

| Instruction | Stack | What it does |
|---|---|---|
| `(eq)` | `( a b -- bool )` | T if A and B are the same object, else NIL. |
| `(not)` | `( x -- bool )` | T if X is NIL, else NIL. |
| `(lt)` | `( a b -- bool )` | T if A < B (any reals). |
| `(gt)` | `( a b -- bool )` | T if A > B (any reals). |
| `(le)` | `( a b -- bool )` | T if A <= B (any reals). |
| `(ge)` | `( a b -- bool )` | T if A >= B (any reals). |
| `(numeq)` | `( a b -- bool )` | T if A = B (any numbers). |

### Generic arithmetic

| Instruction | Stack | What it does |
|---|---|---|
| `(add)` | `( a b -- sum )` | A + B with a fixnum fast path; any numbers. |
| `(sub)` | `( a b -- diff )` | A - B with a fixnum fast path; any numbers. |
| `(mul)` | `( a b -- prod )` | A * B with a fixnum fast path; any numbers. |

### Fixnum operations

| Instruction | Stack | What it does |
|---|---|---|
| `(addfx)` | `( a b -- sum )` | Fixnum A + B, no tests; wraps modulo 2^61.  Both operands must be known fixnums. |
| `(subfx)` | `( a b -- diff )` | Fixnum A - B; wraps modulo 2^61.  Both operands must be known fixnums. |
| `(mulfx)` | `( a b -- prod )` | Fixnum A * B; wraps modulo 2^61.  Both operands must be known fixnums. |
| `(incfx)` | `( a -- a+1 )` | Fixnum A + 1; wraps modulo 2^61.  A must be a known fixnum. |
| `(decfx)` | `( a -- a-1 )` | Fixnum A - 1; wraps modulo 2^61.  A must be a known fixnum. |
| `(ltfx)` | `( a b -- bool )` | Fixnum A < B.  Both operands must be known fixnums. |
| `(gtfx)` | `( a b -- bool )` | Fixnum A > B.  Both operands must be known fixnums. |
| `(lefx)` | `( a b -- bool )` | Fixnum A <= B.  Both operands must be known fixnums. |
| `(gefx)` | `( a b -- bool )` | Fixnum A >= B.  Both operands must be known fixnums. |
| `(numeqfx)` | `( a b -- bool )` | Fixnum A = B.  Both operands must be known fixnums. |
| `(logandfx)` | `( a b -- r )` | Bitwise AND of two known fixnums. |
| `(logiorfx)` | `( a b -- r )` | Bitwise inclusive OR of two known fixnums. |
| `(logxorfx)` | `( a b -- r )` | Bitwise exclusive OR of two known fixnums. |
| `(remfx)` | `( a b -- r )` | REM of two known fixnums; "Division by zero" if B is 0. |
| `(modfx)` | `( a b -- r )` | MOD of two known fixnums; "Division by zero" if B is 0. |
| `(ashfx shift)` | `( a -- r )` | Shift the known fixnum A by the constant SHIFT bits (negative: right). |

### Floating-point operations

| Instruction | Stack | What it does |
|---|---|---|
| `(addfl)` | `( a b -- sum )` | Flonum A + B.  Both operands must be known flonums. |
| `(subfl)` | `( a b -- diff )` | Flonum A - B.  Both operands must be known flonums. |
| `(mulfl)` | `( a b -- prod )` | Flonum A * B.  Both operands must be known flonums. |
| `(divfl)` | `( a b -- quot )` | Flonum A / B; "Division by zero" if B is 0.0.  Both operands must be known flonums. |
| `(ltfl)` | `( a b -- bool )` | Flonum A < B.  Both operands must be known flonums. |
| `(gtfl)` | `( a b -- bool )` | Flonum A > B.  Both operands must be known flonums. |
| `(lefl)` | `( a b -- bool )` | Flonum A <= B.  Both operands must be known flonums. |
| `(gefl)` | `( a b -- bool )` | Flonum A >= B.  Both operands must be known flonums. |
| `(numeqfl)` | `( a b -- bool )` | Flonum A = B.  Both operands must be known flonums. |
| `(fixtofl)` | `( a -- f )` | Convert a known fixnum to a flonum. |

### Vectors and strings

| Instruction | Stack | What it does |
|---|---|---|
| `(svref)` | `( v i -- x )` | AREF of a vector and one subscript; a simple vector in range takes the fast path, anything else generic AREF. |
| `(svset)` | `( v i x -- x )` | Store X at subscript I of V (the generic (setf aref) when V is not a simple vector in range); leave X. |
| `(schar)` | `( s i -- c )` | The character at subscript I of the string S. |
| `(svrefx)` | `( v i -- x )` | SVREF with the vector, the index and the range all trusted.  Only in a routine declared (:unsafe). |
| `(svsetx)` | `( v i x -- x )` | SVSET with everything trusted.  Only in a routine declared (:unsafe). |

### Type checks

| Instruction | Stack | What it does |
|---|---|---|
| `(chkfix)` | `( x -- x )` | Signal a type error unless X is a fixnum.  Afterwards the assembler knows X is a fixnum. |
| `(chktype type)` | `( x -- x )` | Signal a type error unless X has TYPE: fixnum integer double-float real number cons list symbol null string simple-vector character. |
| `(chkrange lo hi)` | `( x -- x )` | Signal a type error unless X is a fixnum between LO and HI.  Afterwards X is a known fixnum. |


## See also

* [The Editor](topic:editor-manual) for the editor's commands in general.
* [The Language Reference](topic:language-reference) for `compile`, `disassemble` and the compiler's declarations.
* `docs/design/vm.md` for the machine, `docs/design/compiler.md` for how the compiler uses the instructions, and
  `docs/design/asm.md` for how the assembler works.
* `examples/asm/numeric.ngasm` and `examples/15-bytecode-assembly.lisp`.
