# The Pascal Tool Kit

The Pascal Tool Kit lets you write, compile, run and debug Pascal programs inside NeoGenera, as the Symbolics Pascal
Tool Kit did on Genera. Pascal here is not a separate world: the compiler translates Pascal into Lisp, which the system
compiler then compiles like any other Lisp, so a Pascal program uses Lisp's integers, Lisp's numeric functions and Lisp's
I/O, and Pascal and Lisp code can call each other freely.

The Tool Kit has these parts:

* a compiler for two dialects of Pascal, **ISO Pascal** (ISO 7185, "standard Pascal") and **Pascal/VS** (IBM's dialect,
  with strings and loop exits), plus a set of extensions for working in a Lisp world;
* a run-time library: text and typed files, formatted reads and writes, sets, pointers, and checks on every access;
* **units**, separately compiled collections of routines, and **`def-program`**, which declares a system of Pascal (and
  Lisp) files for `load-system` and `compile-system`;
* **Pascal mode** in the editor: colouring, indentation, motion and deletion by language unit, templates, completion,
  syntax-error finding, and compiling the routine at the cursor;
* `pascal-execute` and the `:Execute Pascal` command for running programs, with Pascal-level error messages that enter the
  debugger.

## How it differs from a conventional Pascal

| Elsewhere | In NeoGenera |
|---|---|
| A file holds one main program. | A file or buffer can hold as many main programs and units as you like. |
| You compile whole files. | You can compile one routine (`C-c C-c` in the editor); the rest stays as it was. |
| You leave the editor to compile. | You compile from the editor; errors become compiler warnings you can step through. |
| A link step builds an executable. | Compiled programs are runnable at once and stay in memory; run them again without reloading. |
| Integers overflow at 2^31 or 2^63. | Integers are Lisp integers: they never overflow. |
| Uninitialised variables hold garbage. | Reading a variable that was never given a value is an error that names it. |
| Errors stop the program with a code. | Errors name the routine and line, and enter the debugger with the program's state intact. |

## Using the Tool Kit for the first time

(In the documentation viewer, clicking any Pascal example in this manual copies it; `C-y` pastes it in the editor.)

Open the editor on a new file whose name ends in `.pas` (`:edit quadratic.pas`, or `C-x C-f` in the editor). The mode
line shows **(Pascal)**. Turn on Electric Pascal mode with `M-x electric pascal mode` if you want reserved words put in
capitals as you type. Type the program:

```pascal
{-*- Mode: PASCAL; Dialect: ISO; Package: PASCAL-USER -*- }
PROGRAM quadratic;
VAR a,b,c,discriminant,x1,x2 : real;
BEGIN
  write('Enter values for A, B, and C:  ');
  readln(a,b,c);
  discriminant := b*b - 4*a*c;
  x1 := (-b + sqrt(discriminant)) / (2*a);
  x2 := (-b - sqrt(discriminant)) / (2*a);
  writeln('The roots are:');
  writeln(x1,' and ',x2)
END.
```

The first line is the **attribute list**: it says the buffer is Pascal, in the ISO dialect, compiled into the package
`PASCAL-USER`. It is optional (`M-x update attribute list` writes it for you).

With the cursor anywhere in the program, press `C-c C-c` (Genera's `c-sh-C`). The echo area says
`program quadratic compiled`, or reports the first compiler warning; `C-.` then visits each warning in turn.

Run the program with `C-c C-x` (or compile and run in one step with `C-c C-e`): it runs in the Listener, where it reads
its input. Or type to the Listener:

```lisp
(pascal-execute 'pascal-user::quadratic)
```

```
Enter values for A, B, and C:  1 0 -9
The roots are:
 3.000000000000E+000 and -3.000000000000E+000
=> NIL
```

Run it again and type `1 2 3`. The square root of a negative number is an error, and the Listener's debugger reports
where it happened:

```
Error: Attempt to take the square root of -8.0, which is negative
  (in Pascal routine quadratic, line 8)
```

Choose `0` (or press Escape) to return to the Listener, correct the program, recompile and run it again.

## Dialects

Each file or buffer is compiled in one dialect, named by the `Dialect:` entry of its attribute list (`ISO` or `VS`);
without one, the dialect is the value of `*pascal-default-dialect*` (`:iso` or `:vs`, initially `:iso`). In the editor,
`M-x set dialect` changes the attribute list. Routines compiled in different dialects call each other freely.

Every ISO facility exists in Pascal/VS. Pascal/VS adds:

* the **`string(n)`** type: strings of at most *n* characters (`string` alone is `string(255)`), assignable from string
  constants, characters and other strings; `s[i]` is a character; `||` (or `+`) concatenates; and the functions
  `length`, `maxlength`, `substr(s, start[, length])`, `index(s, part)`, `trim`, `ltrim`, `compress` and `str(x)`
  (any value as a string). A string longer than its variable's maximum is an error;
* the statements **`leave`** (exit the innermost loop), **`continue`** (start its next iteration) and **`return`**
  (leave the routine);
* the operator `xor` on booleans.

Both dialects have the Tool Kit's extensions: `otherwise` in `case` statements and variant records, `%include`,
`shortreal`, `single` and `double`, `lispobject` and the `lisp` directive, the second parameter of `reset` and `rewrite`,
units, conformant array parameters, `&` and `|` as other spellings of `and` and `or`, and long identifiers with
underscores. `otherwise` is therefore a reserved word in ISO Pascal too. A `case`
statement may also use `else` for `otherwise`, and label ranges `1..5`.

Not implemented: Pascal/VS's separate compilation (`def`, `ref`, `segment`, `static`,
`space`), the routine directives `external`, `fortran`, `main` and `reentrant`, the routines `parms`, `pdsin`, `pdsout`,
`retcode`, `token`, `seek` and `update`, and `%` directives other than `%include`.

## Extensions to Pascal

**Arbitrary-precision integers.** `integer` is Lisp's integer type, so arithmetic never overflows and `write` prints
numbers of any size. `maxint` is 2147483647 for programs that test it, but nothing is limited to it.

```pascal
FUNCTION factorial (n : integer) : integer;
VAR i : integer;
BEGIN
  factorial := 1;
  FOR i := n DOWNTO 1 DO factorial := factorial * i
END;
```

`factorial(30)` is `265252859812191058636308480000000`. (Inside a function that has parameters, the function's name used
without arguments is the value assigned to it so far, as in the Genera Tool Kit; in a function without parameters it is a
recursive call, as in ISO Pascal.)

**Uninitialised variables are detected.** A variable starts out *undefined*, and reading it before it has been given a
value is an error that names it: `The variable x is used before it has been given a value`. To start variables at zero
(0, 0.0, false, chr(0), the empty set or string, nil) instead, set `*pascal-init-to-zero*` to true before compiling
(for the variables of routines) and run the program with `:init-to-zero t` (for the program's own variables, arrays and
records). After a `for` loop its control variable is undefined again, as ISO Pascal says.

**Checks.** Assignments to subrange variables, array subscripts, `succ` and `pred`, `chr`, `case` selectors with no
matching label, `div` and `mod` by zero, `sqrt` and `ln` of numbers out of their domain, following `nil` or a pointer
whose storage was disposed of, reading past the end of a file and a function that returns without assigning its result
are all run-time errors. Constant subscripts and constants out of range are reported when the program is compiled.

**`shortreal`, `single` and `double`.** `real` is a double-precision float (53 bits). `shortreal` is accepted as a type
and is also represented as a double, since NeoGenera has only one float type; `single(x)` rounds `x` to 24 significant bits,
and `double(x)` converts to `real`.

**`%include`.** `%include name` or `%include 'file'` on a line of its own reads the named file (`name`, `name.pas` or
`name.pascal`, relative to the file being compiled) in its place.

**Opening files by name.** `reset` and `rewrite` take a second parameter, a string of `option = value` pairs:
`name` (or `ddname`) gives the file name, and `access` is accepted. Without it, a file variable `f` opens the file `f`.
Names are relative to `:pathname-defaults` (see below).

```pascal
reset(f, 'name = /data/results.txt');
rewrite(g, 'name=out.data, access=sequential');
```

**`close`.** `close(f)` closes a file; files still open when a program ends are closed for it.

**Interaction with Lisp:** see "Calling Lisp from Pascal" and "Calling Pascal from Lisp".

## Packages

Pascal names live in Lisp packages, so two programs called `primes` can exist in different packages. The default is
`PASCAL-USER`, which inherits nothing from Lisp, so Pascal names never clash with Lisp ones. Declare others with

```lisp
(package-declare-with-no-superiors "PAYROLL")
```

and name the package in the attribute list (`Package: PAYROLL`) or with `M-x set package`. Inside the package, a main
program `p` is known by the symbol `P`; its first-level routine `f` is the Lisp function `F<-P`; its variable `x` is the
special variable `X<-P`. A unit `u`'s routines and variables are `F<-U` and `V<-U`.

## Units

A **unit** is a separately compiled group of declarations and routines that programs (and other units) use. It has two
parts. The **unit specification** is the public interface: constants, types, variables and routine headings, ending with
`END.` (and no `BEGIN`):

```pascal
UNIT_SPECIFICATION stacks;
TYPE stack = RECORD top : integer; items : ARRAY [1..100] OF integer END;
PROCEDURE init (VAR s : stack);
PROCEDURE push (VAR s : stack; x : integer);
FUNCTION pop (VAR s : stack) : integer;
END.
```

The **unit implementation** gives the routines' bodies, without repeating their parameters; it may have private
declarations and routines of its own:

```pascal
UNIT_IMPLEMENTATION stacks;
PROCEDURE init; BEGIN s.top := 0 END;
PROCEDURE push; BEGIN s.top := s.top + 1; s.items[s.top] := x END;
FUNCTION pop; BEGIN pop := s.items[s.top]; s.top := s.top - 1 END;
END.
```

A program names the units it uses in a `using_units` clause right after its heading; their declarations (and those of the
units they use) are then visible in it:

```pascal
PROGRAM rpn;
using_units stacks;
VAR s : stack;
BEGIN init(s); push(s, 3); writeln(pop(s):1) END.
```

Compile a unit specification before the programs and implementations that use it; compile the implementation before
the program runs. Any routine of an implementation can then be recompiled by itself. `pascal-units-depended-on` and
`pascal-dependent-units` (and the editor's `M-x show units depended on` and `M-x show dependent units`) list the
relationships.

## Pascal mode in the editor

Files whose names end in `.pas`, `.pascal` or `.p` are edited in Pascal mode; `M-x pascal mode` selects it for any buffer
(with a numeric argument it also writes the attribute list). Pascal mode understands Pascal: it parses the buffer as you
work, and its commands act on statements, routine definitions, expressions and tokens. It works on incomplete and
incorrect programs, but the more correct the program, the more helpful it is.

The Genera Tool Kit's keys used the Symbolics keyboard; Pascal mode puts the same commands on keys a terminal can send:

| Genera | NeoGenera | Command |
|---|---|---|
| `c-sh-C` | `C-c C-c` | Compile the region's routines and units, or the routine at the cursor (`M-x compile region`) |
| | `C-c C-b` | Compile the buffer (`M-x compile buffer`; with an argument, from the cursor on) |
| | `M-x compile file` | Compile the file to a compiled-code (`.nfasl`) file, saving it first |
| | `C-c C-e` | Compile and execute the program at the cursor, in the Listener |
| | `C-c C-x` | Execute the program at the cursor (`M-x execute pascal program`) |
| `c-.` | `C-.` | The next compiler warning (`C-c C-w` or `M-x edit compiler warnings` starts at the first) |
| `c-m-A` `c-m-E` | `C-M-a` `C-M-e` | Beginning and end of the routine (or program) |
| `c-m-H` | `C-M-h` | Mark the routine |
| `c-m-F` `c-m-B` | `C-M-f` `C-M-b` | End of the current or next statement; start of the current or previous one |
| `c-m-K` `c-m-RUBOUT` | `C-M-k` `C-M-<backspace>` | Kill to the end (start) of the statement |
| `c-sh-K` | `C-c k` | Kill the statement on this line (all of a `for`, `if` or compound statement) |
| `c-sh-F` `c-sh-B` | `C-c f` `C-c b` | Forward and backward over an expression (a parenthesised one as a whole) |
| `c-sh-X` `m-sh-X` | `C-c x` `C-c X` | Kill the expression after (before) the cursor |
| `c-sh-T` `m-sh-T` | `C-c t` `C-c T` | Kill the token after (before) the cursor |
| `c-sh-N` `c-sh-P` | `C-c C-n` `C-c C-p` | The next (previous) syntax error; with an argument, the last (first) |
| `LINE` | `C-j` | End the line, indent the next, and report a syntax error in the line just ended |
| `TAB` | `TAB` | Indent the line (or the region) |
| `END` | `C-c C-t` | Replace the keyword before the cursor by its template; elsewhere, the menu below |
| `c-HELP` | `C-c ?` | A menu of the language constructs valid at the cursor; `RET` on one inserts its template |
| `COMPLETE` | `M-TAB` | Complete the reserved word or predeclared name before the cursor, or refine the template item at it |
| `c-?` | `C-c /` | The predeclared identifiers that complete the name before the cursor |
| `c-END` | `C-c ]` | Insert what closes the innermost open construct: `END`, `UNTIL`, `)` or `]` |
| `c-m-N` `c-m-P` | `C-M-n` `C-M-p` | The next (previous) template item |
| `SPACE` at an item | `SPC` at an item | Delete the template item |

A deletion refuses to delete a statement that contains a syntax error ("No balanced language construct to delete").

**Templates.** A template shows the pattern of a construct, with **template items** between horseshoes for the parts you
fill in. `for` followed by `C-c C-t` gives

```
FOR ⊂control variable⊃ := ⊂initial value⊃ ⊂to or downto⊃ ⊂final value⊃ DO
  ⊂statement⊃
```

with the cursor at the first item. Typing at an item replaces it; `SPC` deletes it (for an optional part you do not
want); `C-M-n` moves to the next. An item between braces, `⊂{, identifier}⊃`, may be repeated or omitted; one between
brackets, `⊂[ formal part ]⊃`, is optional. `M-TAB` at an item refines it: at `⊂initial value⊃` it becomes
`⊂expression⊃`; at `⊂to or downto⊃` and `⊂statement⊃` it offers a menu. A template item is only text — you can save it
in a file — but the compiler reports any that is left: `The template item ⊂statement⊃ has not been filled in`.

**Electric Pascal mode** (`M-x electric pascal mode`, a minor mode shown as `ELECTRIC` on the mode line) gives each word
its case as you finish typing it: reserved words in capitals and other identifiers in lower case, by default.
`M-x adjust face and case` chooses the cases for the buffer (`upper`, `lower`, `capitalize`, `capitalize-words` or
`leave`), and `M-x format language region` applies them to the region or the current routine (with a numeric
argument it puts the reserved words back in lower case). A terminal has no fonts, so where Genera also changed the face
(bold reserved words, italic comments), Pascal mode colours reserved words, comments, strings, predeclared names and the
names of routines being declared.

**Indentation.** `TAB` and `C-j` indent by `*pascal-indent*` columns (2) per level: the body of `begin`, `repeat`,
`record` and `case`; the statement after `then`, `else` and `do`; declarations after `var`, `const` and `type`; nested
routines. `end` and `until` line up with what they close. `M-x blink matching construct` toggles highlighting, with the
cursor after an `END` or `UNTIL`, of the `BEGIN`, `CASE`, `RECORD` or `REPEAT` it closes.

**The attribute list.** `M-x update attribute list` writes `{-*- Mode: PASCAL; Dialect: ISO; Package: PASCAL-USER -*- }`
as the first line; `M-x set dialect` and `M-x set package` change one entry (`set package` creates the package if it does not exist);
`M-x reparse attribute list` shows what the buffer will be compiled with.

## Compiling

The compiler produces Lisp, which the system compiler compiles to bytecode, so there is no loss in compiling through
Lisp, and Pascal's library is largely Lisp's (`sin`, `sqrt` and the rest are the Lisp functions). Every top-level routine
is a Lisp function of its own; nested routines are local functions of the routine that declares them. So compiling a
routine by itself (`C-c C-c`) replaces just that routine, and calls from routines already compiled reach the new
definition at once. Compiling a routine nested in another recompiles the top-level routine that contains it. Change a
program's declarations and recompile the whole program.

From Lisp:

| Function | |
|---|---|
| `(pascal-load-file path)` | Compile a Pascal file into the running world; returns the names defined |
| `(pascal-compile-file path &key output-file load)` | Compile to a compiled-code file, loaded later with `load` |
| `(pascal-compile-string text &key package dialect only-at)` | Compile text: `(values names errors)` |
| `(pascal-run-string text &rest options)` | Compile text and run its main program |
| `(pascal-translate text &key package dialect)` | The Lisp forms for the text, without compiling them |
| `(pascal-compiler-warnings)` | The warnings of the last compilation of each file or buffer |

and the Listener command `:Compile Pascal File` (or just `:Compile Pascal test.pas`). The warnings of the last compilation of each file or buffer are kept;
`:Show Pascal Warnings` lists them and the editor's `C-c C-w` visits them. Compiler options:

| Variable | Default | |
|---|---|---|
| `*pascal-default-dialect*` | `:iso` | The dialect of files without a `Dialect:` attribute |
| `*pascal-init-to-zero*` | `nil` | Compile routines' variables starting at zero instead of undefined |
| `*pascal-check-undefined*` | `t` | Check every read of a variable for an undefined value |
| `*pascal-debug-info*` | `t` | Record the routine and line being executed, for error messages |

## Systems of Pascal programs

`def-program` declares a system of Pascal (and Lisp) files; it expands into a `defsystem`, so `load-system`,
`compile-system` and the `:Load System` and `:Compile System` commands handle it, compiling only the files that changed.

```lisp
(package-declare-with-no-superiors "PLOT")
(def-program plot (:default-pathname "~/plot/" :default-package "PLOT")
  (:using "axislabels" "twodplot"
     "onedplot"))
```

A file name without a type is looked up as `.pas`, `.pascal`, `.p` and `.lisp`. `(:using spec-files files...)` says that
the first files contain unit specifications the others use, so they are compiled and loaded first; `:using` forms nest.
Pascal files whose attribute list names no package are compiled into `:default-package`.

## Executing programs

A compiled main program stays in the world until it is recompiled; run it as often as you like with

```lisp
(pascal-execute 'pascal-user::mean :input "abc.data" :init-to-zero t)
```

The program name is a symbol or a string, with or without its package (`"mean"`, `"pascal-user:mean"`). `xqt` and
`pascal:execute` are other names for `pascal-execute`. The Listener command is `:Execute Pascal mean yes` (the second
argument is `Init_to_zero`). The options are:

| Option | |
|---|---|
| `:input`, `:output` | The standard files: a file name, a pathname or a stream. By default the Listener: the program's prompts appear there and it reads lines typed to it. A stream is not closed at the end. |
| `:init-to-zero` | Start the program's variables (arrays and records included) at zero instead of undefined |
| `:pathname-defaults` | The directory that file names in `reset`, `rewrite`, `:input` and `:output` are relative to |
| `:save-environment` | Keep the program's variables from one run to the next |
| `:reload` | Discard a saved environment and start afresh |
| `:streams` | An alist `((pascal-file-name . lisp-stream) ...)` connecting the program's file variables to Lisp streams |
| `:stack-size`, `:trap-underflow`, `:use-abort-mode` | Accepted for compatibility; ignored |

In the editor, `C-c C-x` runs the program at the cursor in the Listener and `C-c C-e` compiles it first. `halt` ends a
program at once.

## Errors and debugging

A run-time error is a `pascal-runtime-error` whose message names the Pascal routine and source line that were running:

```
Error: Index 11 is out of the array bounds 1..10
  (in Pascal routine smooth, line 23)
```

In the Listener it enters the debugger, where the backtrace shows the Lisp functions of the routines (`SMOOTH<-SIGNAL`),
forms are evaluated in the context of the error, and `:abort` (or `0`) returns. Errors that Lisp itself signals inside a
Pascal program (a Lisp function called through the `lisp` directive getting a bad argument, say) are given the Pascal
context too; the original condition is kept (`pascal-error-original`).

## Implementation-dependent values

| Type | Representation |
|---|---|
| `integer` | Lisp integers, of any size |
| `real`, `shortreal` | IEEE double floats: about 2.2e-308 to 1.8e308, 53 bits (about 16 digits) |
| `char` | Lisp characters; `ord` is the character code (ASCII, or a Unicode code point) |
| `boolean` | `T` and `NIL`; `ord(false)` is 0 |
| enumerations | Their ordinal numbers (`write` prints the name of the value) |
| `set of ...` | An integer used as a bit mask: a set may hold any non-negative ordinals, with no size limit |
| arrays | Simple vectors (a multi-dimensional array is a vector of vectors) |
| records | Simple vectors whose element 0 is the record type's name |
| pointers | Boxes holding the value pointed to |
| `lispobject` | Any Lisp object |

`write` gives integers a field of `*pascal-integer-field-width*` (10) characters unless a width is written (`x:1`);
reals are written in floating-point form in `*pascal-real-field-width*` (20) characters, ` 3.000000000000E+000`, or
with `x:w:d` in fixed-point form; booleans as `TRUE` and `FALSE`.

Reading from the Listener, `eoln` is true at the end of the line typed and `eof` is never true; a program that reads to
the end of its input should read from a file or a string stream (`:input`).

The source file types are `.pas`, `.pascal` and `.p`. Compiled files are `.nfasl`, like Lisp's.

## Calling Lisp from Pascal

The type **`lispobject`** holds any Lisp object. Its values can only be assigned, passed as arguments and written
(with `princ`); they are not compared or computed with, and no other type converts to it.

The routine directive **`lisp`**, like `forward`, declares a routine whose body is a Lisp function:

```pascal
FUNCTION length (list : lispobject) : integer; lisp 'global:length';
PROCEDURE ball (i, j, k : integer); lisp 'cl-user::ball';
```

The string names the Lisp function, with its package (`global:`, `lisp:` and `cl:` mean `COMMON-LISP`; `user:` and
`cl-user:` mean `COMMON-LISP-USER`); without it the function has the routine's name in the program's package. Arguments
are passed by value, even `var` ones: integers, reals, characters and booleans as themselves, arrays and records as their
vectors (a packed array of characters as a string). A function's result is taken as its declared type; for `boolean`,
any non-`NIL` value is true.

```pascal
PROGRAM revarray;
TYPE intarray = ARRAY [1..10] OF integer;
VAR a : intarray; list : lispobject; i : integer;
  FUNCTION reverse (list : lispobject) : lispobject; lisp 'global:reverse';
  FUNCTION listarray (VAR arr : intarray) : lispobject; lisp 'cl-user::vector-to-list';
BEGIN
  FOR i := 1 TO 10 DO a[i] := i;
  list := reverse(listarray(a));
  writeln(list)                    { prints (10 9 8 7 6 5 4 3 2 1) }
END.
```

Here `vector-to-list` is a Lisp function of your own: `(defun vector-to-list (v) (coerce v 'list))`.

## Calling Pascal from Lisp

A Lisp function calls a main program with `pascal-execute`:

```lisp
(defun callwrites (file) (pascal-execute 'pascal-user::writeones :output file))
```

A first-level routine of a program is the Lisp function `ROUTINE<-PROGRAM`, called with its arguments (Genera's extra
"display" argument is not needed, and reals are passed as ordinary Lisp floats, not unboxed):

```lisp
(pascal-user::factorial<-factor 25)   ; => 15511210043330985984000000
```

A routine that uses the program's variables or its standard files should run inside `pascal-execute`; outside, its
variables are undefined and `output` is the Lisp `*standard-output*`.

## The PASCAL package

The names the Genera Tool Kit used are also available in the package `PASCAL`: `pascal:execute`, `pascal:xqt`,
`pascal:def-program`, `pascal:package-declare-with-no-superiors`, `pascal:*pascal-init-to-zero*` and
`pascal:*pascal-default-dialect*`.

## Differences from the Genera Tool Kit

* Keys are on terminal-friendly bindings (table above); mouse menus are possibilities lists (choose with `RET`).
* There are no fonts: Electric Pascal mode changes case only, and colour stands for faces.
* `shortreal` has double precision (`single` rounds to 24 bits).
* Sets are unbounded (Genera's integer sets held 256 members).
* Incremental compilation works per top-level routine: compiling a nested routine recompiles the routine that contains it.
* There is no source-level display of Pascal frames in the debugger; errors give the routine and line, and the debugger shows
  the Lisp frames of the routines.
* Not implemented: the Metering Interface, run-time systems for delivery, `Save Indentation`, Pascal/VS separate
  compilation and random access (`seek`, `update`), and the routines listed under "Dialects".
