# The Prolog Tool Kit

The Prolog Tool Kit puts Prolog inside NeoGenera, in the way the Pascal and FORTRAN Tool Kits put those languages there: you edit
Prolog in a language mode of the editor, compile it incrementally into the running world, run it from the Listener, and call
Prolog and Lisp from each other. Prolog here is not a separate program. Its terms are Lisp objects (integers are Lisp integers
of any size, floats are Lisp doubles, strings are Lisp strings), its database lives in the same image as your Lisp code, and the
solver is written in Lisp, so a Prolog program can call any Lisp function and Lisp can ask Prolog questions and use the answers.

The Tool Kit has these parts:

* an **Edinburgh/ISO Prolog** with the usual built-in predicates: control (cut, if-then-else, soft cut, negation, `catch/3`),
  arithmetic with unbounded integers, term inspection, atoms and strings, `findall/3`, `bagof/3`, `setof/3`, the database,
  streams, `format/2`, operators and flags;
* a **library** written in Prolog: lists, `maplist` and `foldl`, pairs, ordered sets, association lists (AVL trees), `yall` lambdas,
  `must_be/2`, and `phrase/2,3` with **definite clause grammars**;
* the **`:Prolog`** top level and the commands **`:Consult Prolog`**, **`:Prolog Query`**, **`:Show Prolog Predicates`**,
  **`:Trace Prolog`** and **`:Reset Prolog`**;
* a **tracer** with the four ports (Call, Exit, Redo, Fail) and spy points;
* the **`PROLOG` package** for calling Prolog from Lisp, and **`lisp/1`**, **`lisp/2`** and **`lisp_call/3`** for calling Lisp from Prolog;
* **PROLOG mode** in the editor: colouring, clause motion, layout, incremental compilation, queries, and the usual
  Edit Changed Definitions commands.

## A first session

Type `:prolog` in the Listener. The prompt changes to `?-`. Enter a clause as a query by asserting it, or load a file first;
here a few facts go in with `assertz`:

```
?- assertz(parent(tom, bob)), assertz(parent(tom, liz)), assertz(parent(bob, ann)).
true.

?- parent(tom, X).
X = bob ;
X = liz.

?- parent(P, ann), parent(G, P).
P = bob,
G = tom.

?- halt.
```

After an answer that is not known to be the last, the top level waits for a line: `;` asks for the next answer, anything else
(just Return) accepts the answer and stops. `halt.` leaves the top level and returns to the Listener.

Normally programs live in files. Open a new file whose name ends in `.pl`, `.pro` or `.prolog` (`:edit family.pl`). The mode
line shows **(PROLOG)**:

```prolog
parent(tom, bob).
parent(tom, liz).
parent(bob, ann).

grandparent(X, Z) :-
    parent(X, Y),
    parent(Y, Z).
```

`C-c C-b` compiles the buffer; the echo area says `4 clauses compiled`. `C-c C-q` asks for a query and shows its answers in
a buffer. `:consult prolog family.pl` loads the same file from the Listener.

## How it differs from other Prolog systems

| Elsewhere | In NeoGenera |
|---|---|
| A program is loaded from files and `consult` replaces everything of the file. | The same, and the editor also compiles single predicates: the clauses of a region replace the earlier definitions of exactly those predicates from that buffer. |
| Integers overflow into floats or errors. | Integers are Lisp integers: `X is 2**200` is exact. |
| The system is a separate process with its own foreign interface. | Prolog and Lisp share one image: `lisp/2` calls Lisp; `prolog:query` calls Prolog. |
| Modules separate libraries from user code. | There is one predicate space. Library predicates are marked, and a program may define its own `append/3`, which then replaces the library one. |
| `"text"` is a string (SWI-Prolog) or a code list (ISO). | A string by default; `:- set_prolog_flag(double_quotes, codes).` gives ISO behaviour. |

## The language

### Syntax

The syntax is Edinburgh/ISO Prolog: atoms (`foo`, `'Hello world'`, `[]`, `+`), variables (`X`, `_Tmp`, `_`), integers of any size
(also `0'c`, `0xFF`, `0o17`, `0b101`, `16'ff` and `1_000_000`), floats (`3.14`, `1.5e10`), compound terms, lists, curly terms
`{a,b}`, and `"strings"`. `%` starts a comment to the end of the line, and `/* ... */` a block comment. Quoted items understand
the escapes `\n \t \\ \' \" \xHH\ \NNN\ \uXXXX` and a backslash at the end of a line.

Operators work as the standard table says and can be changed with `op/3`; `current_op/3` lists them. Arguments of compound terms
and list elements are read up to priority 1200 except for a top-level comma, so `f(a:-b)` and `findall(X, (a;b), L)` both read as
you expect.

A syntax error is reported with its line and column, the rest of that clause is skipped, and reading continues.

### Control

Conjunction `,`, disjunction `;`, if-then-else `( C -> T ; E )`, soft cut `( C *-> T ; E )`, negation `\+`, `call/1` to `call/8`,
`once/1`, `ignore/1`, `forall/2`, `catch/3` and `throw/1`, and the cut `!` work as ISO prescribes. A cut inside `call/1`, `\+`,
an if-then-else condition or `findall/3` is local to it. Exceptions are the ISO error terms, `error(Formal, Context)`: instantiation
errors, type errors, domain errors, existence errors (an unknown procedure is `existence_error(procedure, Name/Arity)` unless the flag
`unknown` is `fail`), permission errors, evaluation errors and syntax errors.

Runaway recursion cannot take the system down: while a computation runs, the live heap is checked, and when it passes the flag `stack_limit` (128 MB
by default, `set_prolog_flag(stack_limit, Bytes)`) the computation stops with `resource_error(memory)`, which `catch/3` can handle. Terms are not checked for
cycles when unified (there is no occurs check unless you use `unify_with_occurs_check/2`), so `X = f(X)` makes a cyclic term; list built-ins notice a
cyclic list and raise a type error, but printing or copying other cyclic terms ends in an error as well. Single allocations (`length/2`, `numlist/3`,
`functor/3`, `tab/1`) are limited to a million elements.

Deep recursion is cheap: a conjunction's continuation and the choicepoint stack are on the heap, so recursing a hundred thousand levels
(for example `len/2` on a long list) does not exhaust the Lisp stack. Choicepoints are avoided where the first argument tells the
clauses apart, so list recursions leave none behind.

### Arithmetic

`is/2` and the comparison predicates evaluate expressions with `+ - * / // mod rem div min max abs sign gcd msb`, the shifts and bit
operations, `** ^`, `sqrt exp log log2 sin cos tan asin acos atan atan2 sinh cosh tanh`, the rounding functions `truncate round ceiling floor
integer float float_integer_part float_fractional_part`, the constants `pi e inf nan epsilon max_tagged_integer`, `random/1`,
`random_float` and `cputime`. `/` gives an integer when both sides are integers and the division is exact, otherwise a float. `2 ** 3`
is the integer 8 and `2 ** -1` is 0.5; `2 ^ -1` is an error because `^` keeps integers integral. Overflow, division by zero and
results outside the reals raise `evaluation_error`.

### Text

Atoms and strings convert freely: `atom_codes/2`, `atom_chars/2`, `char_code/2`, `atom_length/2`, `atom_number/2`, `number_codes/2`,
`atom_string/2`, `string_concat/3` (and `atom_concat/3`, which enumerates the splits when both parts are unbound), `sub_atom/5` and
`sub_string/5`, `atomic_list_concat/2,3` (also in split mode), `split_string/4`, `upcase_atom/2`, `string_chars/2`, `string_codes/2`,
`term_to_atom/2`, `term_string/2`, `char_type/2` and `code_type/2`.

### Solutions

`findall/3,4`, `bagof/3` and `setof/3` (with `Var^Goal` and backtracking over the free variables), and `aggregate_all/3` with `count`,
`sum`, `max`, `min`, `bag` and `set`. `between/3`, `succ/2`, `plus/3`, `length/2` (which also generates lists), `msort/2`, `sort/2,4`,
`predsort/3` and `keysort/2` are built in.

### The database

`assert/1`, `asserta/1`, `assertz/1`, `retract/1`, `retractall/1`, `abolish/1`, `clause/2`, `dynamic/1`, `discontiguous/1`,
`current_predicate/1,2` and `predicate_property/2`. Changes follow the *logical update view*: a goal sees the clauses that existed
when it started, so a loop that asserts into the predicate it is iterating does not run forever. `listing/0,1` and
`portray_clause/1` print clauses in standard layout, with singleton variables as `_`:

```
?- listing(grandparent/2).
grandparent(A, B) :-
    parent(A, C),
    parent(C, B).
```

### Streams and input

`write/1`, `print/1`, `writeq/1`, `write_canonical/1`, `write_term/2`, `nl/0`, `tab/1`, `put_char/1`, and the same with a stream
argument; `read/1`, `read_term/2`, `get_char/1`, `peek_char/1`, `get_code/1` and `read_line_to_string/2`; `open/3,4`, `close/1`, `current_output/1`,
`set_output/1`, `flush_output/0`; and the Edinburgh `see/1`, `seen/0`, `tell/1`, `told/0` and `append/1`. `user_input`, `user_output` and
`user_error` name the Listener's streams. `with_output_to/2` and `format(atom(A), ...)` collect output in a string.

`format/1,2,3` knows `~w ~p ~q ~a ~d ~D ~s ~e ~f ~g ~n ~c ~r ~R ~i ~* ~~` and column handling with `~t` (fill), `~|` (column stop) and `~+`
(column relative to the previous stop): `format("~w~t~12|~w~n", [name, value])`.

### Definite clause grammars

A rule `Head --> Body` is translated into an ordinary clause with two extra arguments, the difference list. Terminals are lists
(`[a,b]`) or strings (`"ab"`, read as code lists inside a rule), `{Goal}` calls Prolog, `!`, `\+`, `->`, `;` and `|` work, a pushback
`Head, [x] --> Body` is allowed, and `call//N` passes the two arguments on. `phrase/2,3` runs a grammar, and `expand_term/2` and
`dcg_translate_rule/2` show the translation:

```prolog
greeting --> [hello], name.
name --> [world].
name --> [prolog].

?- phrase(greeting, [hello, X]).
X = world ;
X = prolog.
```

### Not provided

Modules, tabling (`:- table` is accepted and ignored), coroutining (`dif/2` is only `\==`, and there is no `freeze/2`), attributed
variables, constraint libraries, dicts, threads, and the full ISO stream layer (binary streams, `stream_property` is a small
subset). `setup_call_cleanup/3` runs its goal as `once`. `setarg/3` is backtrackable but must not be used on a term that came from a
clause (such terms are shared by every call).

## Consulting

`consult/1` (also `[file]`, `ensure_loaded/1`, `:Consult Prolog`) reads a file clause by clause. A clause is added to the database; a
directive `:- Goal.` is run. `:- initialization(Goal).` runs the goal after the whole file is loaded. If you consult a file again, the
predicates it defines are replaced, not extended. Relative file names are looked for next to the file being consulted, and the
types `.pl`, `.pro` and `.prolog` may be left out.

Two kinds of warning are collected: **singleton variables** (a named variable that occurs once, as in `p(X, Y) :- q(X).`; start the name with `_` to say
it is deliberate) and **directive failures or errors**. Syntax and other errors are reported too. Each is recorded with its file and line in the
*compiler warnings database*, which `:Show Prolog Warnings` and the editor's `C-c C-w` show:

```
Warning: family.pl:12:
Warning:    Singleton variables: [Y]
```

The user hook `term_expansion/2` is applied to each term read: declare it `:- dynamic term_expansion/2.`, and its result (a term or a
list of terms) is added instead.

`consult` of a source whose clauses were read from a string or an editor buffer works the same way with the buffer's name as the
source. Systems (`defsystem`, `load-system`, `compile-system`) accept `.pl` files; `prolog:compile-file` writes a compiled-code file
that consults the program when loaded.

## The Listener commands

| Command | Does |
|---|---|
| `:Prolog` | Enters the Prolog top level (`?-`). `halt.` returns. |
| `:Prolog Query goal` | Runs the goal and prints up to a hundred answers. |
| `:Consult Prolog file` | Loads a Prolog file (also `:Compile Prolog`, `:Load Prolog`). |
| `:Show Prolog Predicates` | Lists the user's predicates with their clause counts. |
| `:Show Prolog Warnings` | Lists the pending compiler warnings. |
| `:Trace Prolog on` / `off` | Turns tracing of every call on or off. |
| `:Reset Prolog` | Removes the user's predicates, operators and flags (the library stays). |

## Tracing

`trace/0` turns on the tracer, and `notrace/0` turns it off. Every call of a user predicate prints its four ports with a call number
and the goal, so you can follow the proof:

```
?- trace, parent(tom, X), notrace.
Call: (1) parent(tom, _G1)
Exit: (1) parent(tom, bob)
...
```

`spy(Name/Arity)` traces only that predicate, `nospy/1` and `nospyall/0` remove the spy points, and `tracing/0` tests whether tracing is
on. Redo is printed when the proof backtracks into a call, and Fail when a call has no more solutions. The tracer prints and
continues (it does not stop to ask), so use it with the Listener's scrolling or `with_output_to/2`.

## PROLOG mode

The mode is selected by the file type (`.pl`, `.pro`, `.prolog`) or `M-x prolog mode`. The editor colours comments, quoted text,
variables, the names of clause heads, built-in predicates and the neck `:-`, and it knows where clauses begin and end.

| Keys | Does |
|---|---|
| `TAB` | Indents the line: heads at the margin, body goals four columns in, `(   If -> Then ; Else )` blocks laid out, arguments aligned after an open parenthesis |
| `C-j` | Ends the line and indents the next |
| `C-M-a`, `C-M-e` | Start and end of the clause |
| `C-M-f`, `C-M-b` | Next and previous clause |
| `C-M-h` | Marks the clause |
| `C-c C-c` | Compiles the region, or else the whole predicate at point (all its consecutive clauses) |
| `C-c C-b` | Compiles the buffer |
| `C-c C-q` | Asks for a query, runs it, and shows the answers in `*Prolog Output*` |
| `C-c C-e` | Runs the goal on the current line (the text after `?-` if there is one) |
| `C-c C-l` | Lists the clauses of the predicate named at point |
| `C-c C-t` | Toggles tracing |
| `C-c C-w` | Visits the compiler warnings one by one (`C-.` goes on) |
| `M-.` | Edits the definition of the predicate named at point (in this buffer, else in the file it was consulted from) |

`M-x list definitions` lists the buffer's predicates. `M-x edit changed definitions of buffer`, `list changed definitions` and
`compile changed definitions of buffer` work on predicates, as the Lisp commands of the same names work on functions: with no argument they
show what changed since the buffer was last compiled, with an argument of 1 since the file was read, and with 2 since it was saved.

Compiling a region does not disturb the rest of the program: only the predicates that appear in the region are replaced.

## Calling Prolog from Lisp

The package `PROLOG` has the functions below. A *goal* is a string of Prolog text or a Prolog term.

| Function | Result |
|---|---|
| `(prolog:consult "file.pl")` | Loads a file; returns the number of clauses and the warnings |
| `(prolog:consult-string text)` | The same for a string |
| `(prolog:query goal &key limit raw)` | A list with one element per solution: an alist of (variable-name . value), values converted to Lisp. A query without variables that succeeds gives `(NIL)`; one that fails gives `NIL` |
| `(prolog:prove goal)` | True if the goal has a solution |
| `(prolog:do-query (bindings goal) body...)` | Runs the body for each solution |
| `(prolog:assert "likes(mary, wine)")` | Adds a clause |
| `(prolog:define-predicate "name" arity function)` | Defines a Prolog predicate in Lisp: the function gets the argument terms and returns true or false |
| `(prolog:define-solutions "name" arity function)` | Defines a non-deterministic predicate: the function returns a list of solutions, each a list of values to unify with the arguments |
| `(prolog:parse "foo(X)")`, `(prolog:term-string term)` | Text to term and back |
| `(prolog:from-lisp x)`, `(prolog:to-lisp term)` | Convert values (see below) |
| `(prolog:unify a b)`, `(prolog:deref term)`, `(prolog:make-variable)` | Work on terms directly |
| `(prolog:reset)` | Removes the user's predicates, operators and flags |
| `(prolog:toplevel)` | Runs the interactive top level |

Values are converted as follows. Integers, floats and strings are themselves. A Lisp list is a Prolog list and `NIL` is `[]`; `T` is `true`;
another symbol is the atom with its lower-case name (`FOO` is `foo`). A Prolog atom is a Lisp symbol with the upper-case name in the current
package, a Prolog list a Lisp list, and another compound `f(a,b)` the list `(F A B)`. A Lisp object of any other kind (a structure, a
stream) passes into Prolog unchanged as an opaque value.

```lisp
(prolog:consult-string "parent(tom, bob). parent(tom, liz).")
(prolog:query "parent(tom, X)")          ; => ((("X" . BOB)) (("X" . LIZ)))
(prolog:prove "parent(tom, bob)")        ; => T

(prolog:define-predicate "double" 2
  (lambda (x y) (and (numberp (prolog:deref x)) (prolog:unify y (* 2 (prolog:deref x))))))
(prolog:query "double(21, X)")           ; => ((("X" . 42)))
```

## Calling Lisp from Prolog

| Predicate | Does |
|---|---|
| `lisp(Form)` | Evaluates the Lisp form written in the atom or string Form; succeeds unless the value is `NIL` |
| `lisp(Result, Form)` | Evaluates the form and unifies Result with its value, converted to Prolog |
| `lisp_call(Name, Args, Result)` | Calls the Lisp function Name with the arguments (converted to Lisp) and unifies Result |

```
?- lisp(X, '(+ 1 2 3)').
X = 6.

?- lisp_call(reverse, [[a, b, c]], R).
R = [c, b, a].
```

A Lisp error becomes the Prolog exception `error(system_error(Message), _)`.

## Programs in this distribution

`examples/16-prolog.lisp` runs the programs in `examples/prolog/`: family relations, the N queens puzzle, a small expert system, a DCG
for arithmetic expressions, DNA translation with a grammar (using the bioinformatics extensions), and the zebra puzzle. Run it with
`make examples`, or look at the sources to see how Prolog files are loaded and queried from Lisp.
