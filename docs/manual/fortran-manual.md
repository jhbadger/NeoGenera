# The FORTRAN Tool Kit

The FORTRAN Tool Kit lets you write, compile, run and edit FORTRAN programs inside NeoGenera, as Symbolics FORTRAN did on
Genera (the product dates from 1982). FORTRAN here is not a separate world: the compiler translates FORTRAN into Lisp,
which the system compiler then compiles like any other Lisp, so a FORTRAN program uses Lisp's integers, Lisp's numeric
functions and Lisp's streams, and FORTRAN and Lisp code can call each other freely.

The Tool Kit has these parts:

* a compiler for **ANSI FORTRAN 77** (X3.9-1978) with the Symbolics extensions: arbitrary-size integers, detection of
  uninitialised values, stronger type checks, packages, integer/logical coercions, selected Department of Defense
  extensions (`DO WHILE`, `ENDDO`, `INCLUDE`, `IMPLICIT NONE`, bit functions), binary patterns, logical and shift
  operators and bit substrings;
* a run-time library: formatted, list-directed and unformatted input and output, sequential and direct-access files,
  internal files, and checks on every subscript, substring and variable read;
* **`f77:execute`** and the **`:Execute FORTRAN`** command, **`f77:def-library`** and **`f77:def-program`** for systems of
  FORTRAN (and Lisp) files, and **`f77:with-fortran-number-data`** for handing Lisp storage to FORTRAN;
* **FORTRAN mode** in the editor, a fixed-format mode that knows statement fields, continuation cards, sequence numbers and
  the column-72 limit.

## How it differs from a conventional FORTRAN 77

| Elsewhere | In NeoGenera |
|---|---|
| A file holds one main program and a link step builds an executable. | A file or buffer holds any number of main programs and routines; compiled programs are runnable at once and stay in memory. |
| You compile whole files. | You can compile one routine (`C-c C-c` in the editor); the rest stays as it was. |
| `INTEGER` overflows at 2**31. | Integers are Lisp integers: they never overflow. |
| An uninitialised variable holds garbage. | Reading a variable that was never given a value is an error that names it (unless you ask for **Init to Zero**). |
| A call with the wrong arguments is discovered when the program crashes. | A call is checked against the routine when the routine was compiled earlier: argument count and types, function or subroutine. |
| Errors stop the program with a code. | Errors name the routine and line and enter the debugger with the program's state intact. |
| Names are global in one link. | Routines live in a **package**, `FTN-USER` by default. |

## Using the Tool Kit for the first time

Open the editor on a new file whose name ends in `.f`, `.for`, `.ftn`, `.f77` or `.fortran` (`:edit quadratic.f`, or
`C-x C-f` in the editor). The mode line shows **(FORTRAN)**. Type the program. `TAB` in the label field moves to the
statement field (column 7), `C-j` (Genera's `LINE`) ends a line and indents the next by its nesting, and `C-M-j` opens a
continuation card:

```fortran
C -*- Mode: FORTRAN; Package: FTN-USER -*-
      PROGRAM QUADRATIC
      REAL A, B, C, D
      COMPLEX X1, X2
      READ (*, *) A, B, C
      D = B*B - 4.0*A*C
      IF (D .GE. 0.0) THEN
         X1 = CMPLX((-B + SQRT(D)) / (2.0*A), 0.0)
         X2 = CMPLX((-B - SQRT(D)) / (2.0*A), 0.0)
      ELSE
         X1 = CMPLX(-B / (2.0*A), SQRT(-D) / (2.0*A))
         X2 = CONJG(X1)
      END IF
      PRINT *, 'The roots are', X1, ' and', X2
      END
```

The first line is the **attribute list**, written as a comment: it says the buffer is FORTRAN and is compiled into the
package `FTN-USER`. It is optional (`M-x update attribute list` writes it).

With the cursor anywhere in the program press `C-c C-c`. The echo area says `quadratic compiled`, or reports the first
compiler warning; `C-.` then visits each warning in turn. Run the program with `C-c C-x` (or compile and run in one step
with `C-c C-e`); it runs in the Listener, where it reads its input. Or type to the Listener:

```lisp
(f77:execute 'ftn-user::quadratic)
```

```
1 0 -9
 The roots are (3.0,0.0)  and (-3.0,0.0)
=> NIL
```

Run it again and type `1 2 5` for complex roots. A run-time error names the routine and line:

```
The subscript 4 of a is outside the array bounds 1:3
  (in FORTRAN routine .main., line 3)
```

## The language

**Fixed format.** Columns 1-5 hold the statement label, column 6 a continuation mark (any character but a blank or `0`),
and columns 7-72 the statement. Columns 73-80, the sequence field, are ignored; so is any text after them, and when a
statement fails to parse because its text runs past column 72 the error message says so. A `C`, `c`, `*` or `!` in column
1 makes a comment line; a `!` outside a character constant starts a trailing comment. A tab in the first six columns ends
the label field, and a digit after it is a continuation mark (the DEC convention). Blanks mean nothing outside character
constants, and source is **case-insensitive except in character constants and Hollerith data**: `do 10 i = 1, n` is
`DO10I=1,N`. There are no reserved words; the statement `DO10I = 1.5` assigns to the variable `DO10I`.

**The language of FORTRAN 77.** All of ANSI X3.9-1978 is accepted except `ENTRY` and the items listed under "Not
implemented" below: `PROGRAM`, `SUBROUTINE`, `FUNCTION`, `BLOCK DATA`, the type statements for `INTEGER`, `REAL`,
`DOUBLE PRECISION`, `COMPLEX`, `LOGICAL` and `CHARACTER*n`, `IMPLICIT`, `PARAMETER`, `DIMENSION`, `COMMON`, `EQUIVALENCE`,
`EXTERNAL`, `INTRINSIC`, `SAVE`, `DATA` (with repeat counts and implied `DO`s), statement functions, `FORMAT`, assignment,
`ASSIGN` and assigned `GO TO`, unconditional, computed and arithmetic `GO TO` and `IF`, block `IF`/`ELSE IF`/`ELSE`,
`DO` with a label (loops may share a terminal statement), `CONTINUE`, `PAUSE`, `STOP`, `CALL` with alternate returns
(`CALL S(X, *10)`, `RETURN 1`), `RETURN`, `READ`, `WRITE`, `PRINT`, `OPEN`, `CLOSE`, `INQUIRE`, `REWIND`, `BACKSPACE`,
`ENDFILE`, all the intrinsic functions (generic and specific names), character substrings and concatenation, and
`COMPLEX` arithmetic.

**Arbitrary-size integers.** `INTEGER` is Lisp's integer type, so arithmetic never overflows and `PRINT` writes numbers of
any size; `PRINT *, 2**100` gives `1267650600228229401496703205376`, and 30! is a loop. Integer division truncates; `**`
with a negative integer exponent follows FORTRAN (`2**(-1)` is 0).

**Uninitialised values are detected.** A variable starts out *undefined*; reading it before it has been given a value is an
error that names it: `The variable n is used before it has been given a value`. This covers scalars, array elements,
variables in `COMMON` and the value of a function that never assigned its result. The Execute option **Init to Zero**
(`:init-to-zero t`) starts all storage at zero of the right type (`0`, `0.0`, `.FALSE.`, a blank string) and turns the
detection off for that run.

**Stronger type checks.** The compiler reports, with the line of the statement: assigning a character value to a number or
the reverse; arithmetic on characters; comparing a character with a number; a wrong argument type to an intrinsic function
(`SQRT('A')`, `IABS(2.5)`, `DSQRT` of a real); an undeclared name under `IMPLICIT NONE`; a wrong number of subscripts;
assigning to a `PARAMETER`; a missing `END IF`, `END DO` or terminal statement; a label that is not defined or defined
twice; and, for routines compiled earlier, a call with the wrong number of arguments, an argument whose type differs
from the dummy's, `CALL` of a function and a function declared with another type than it has. Every error in a program is
reported, not only the first.

**Integer/logical coercions.** A `LOGICAL` value is accepted where an integer is and the reverse: assigning an integer to a
logical makes it true when it is not zero; a logical in arithmetic is 1 or 0. `IF (I)` with an integer tests it against
zero.

**Binary patterns, logical and shift operators.** Integer constants may be written in other radixes: `B'1010'`, `O'17'`,
`Z'FF'` (or `X'FF'`), and also `'FF'X`, `'17'O`, `'101'B`. `Z`, `O` and `B` are edit descriptors for output and input
(`Z8`, `O4`, `B6.6`). The operators `.AND.`, `.OR.`, `.XOR.`, `.EQV.`, `.NEQV.` and `.NOT.` work on two logicals, and on
two integers they are bit-wise (`5 .AND. 3` is 1). The Department of Defense bit functions are intrinsic: `IAND`, `IOR`,
`IEOR`, `NOT`, `ISHFT` (also `SHIFT`; the shift is arithmetic and unlimited), `ISHFTC`, `IBITS`, `IBSET`, `IBCLR`,
`BTEST` and the subroutine `MVBITS`.

**Bit substrings.** `I(first:last)`, where `I` is an `INTEGER` scalar variable, is the field of bits *first* through
*last*, bit 0 being the least significant; it can be assigned: `I(4:7) = 11` stores 11 in bits 4 to 7 and leaves the others.
*This is a reconstruction: the manual that this Tool Kit was written from names bit substrings but not their syntax, so
the substring spelling and the bit numbering here are assumptions.*

**Other extensions.** `DO WHILE (condition)` and `ENDDO` (also `END DO`; a labeled `ENDDO` ends a labeled `DO`), `INCLUDE
'file'` (searched in the source's directory and then in `*f77-include-path*`), `IMPLICIT NONE`, double-quoted character
constants, `!` comments, `REAL*8`, `COMPLEX*16`, `INTEGER*4` and similar kind suffixes (accepted; only `REAL*8` and
`COMPLEX*16` change the type), `DOUBLE COMPLEX`, and initial values in type statements (`INTEGER N /5/`).

## Types and storage

Storage is kept in two spaces, as on Genera: the **number space**, made of one-word entries, and the **character space**,
made of one-character entries. `INTEGER`, `REAL` and `LOGICAL` take one word, `DOUBLE PRECISION` and `COMPLEX` two,
`DOUBLE COMPLEX` four, and a `CHARACTER*n` variable *n* characters (character data is 8-bit). `COMMON` blocks, `EQUIVALENCE`
and array arguments that are sections of other arrays all work by offsets into these spaces, so a `REAL` array may be
equivalenced to a `DOUBLE PRECISION` one and `CALL S(A(3))` passes the rest of `A`.

Values are Lisp values: integers, double-precision floats, Lisp `complex` numbers, `T` and `NIL`, and strings. A `REAL` is a
double that is **rounded to single precision when it is stored** (so `X = 0.1` prints `0.1` and `D = X` gives
`0.10000000149011612`); intermediate results inside an expression are not rounded, which differs from a machine that rounds
every operation. An entry that was never given a value holds the symbol `f77-undefined`. A value stored through an
equivalence is not converted bit for bit: equivalencing an `INTEGER` to a `REAL` does not reinterpret the bits.

A routine's local variables are **static** (as if every routine said `SAVE`), kept in storage belonging to the running
program; they are fresh for each run of `f77:execute` unless you ask for **Save Environment**. Routines may therefore not
call themselves.

## Programs, routines and packages

A main program is `PROGRAM name`; an unnamed one is named `.MAIN.`, so **unnamed main programs reuse that name and replace
one another**: give every program an explicit `PROGRAM` name. A subroutine or function `F` becomes the Lisp function `F` in
the program's package, and a `BLOCK DATA` unit becomes initialisation code that `f77:execute` runs before the program (by
default all those in the program's package; the **Blockdata** option names which).

The normal package is **`FTN-USER`**, which has no superiors, so a FORTRAN routine named `LIST` or `CAR` does not clash with
the Lisp function. The package of a buffer or file comes from its attribute list (`C -*- Mode: FORTRAN; Package: P -*-`),
else from the `:default-package` of its `f77:def-program`, else `FTN-USER`; `M-x set package` changes it. A package named
by the program but not yet existing is created with no superiors. **`CL-USER` can be named, but a package that sees Lisp's
symbols can have name conflicts between FORTRAN names and Lisp names**: a conflicting routine name is given a `%` suffix.

```lisp
(f77:compile-string "      SUBROUTINE ADDTO(X, Y)
      X = X + Y
      END
")
(let ((x (f77:make-ref 1.5)))
  (ftn-user::addto x 2.0)
  (f77:ref-value x))                                   ; => 3.5
```

## Input and output

Units 5 and 6 are the standard input and output (in the Listener, reading and writing the listener; `*` means 5 for
`READ` and 6 for `WRITE` and `PRINT`); unit 0 writes to unit 6's stream. `OPEN` connects other units to files, `STATUS=`
`OLD`, `NEW`, `SCRATCH`, `REPLACE` or `UNKNOWN`, `ACCESS=` `SEQUENTIAL` or `DIRECT`, `FORM=` `FORMATTED` or `UNFORMATTED`;
a unit used without `OPEN` is connected to the file `fort.N`. File names are relative to the **Pathname Default** of the
run. A file is read into memory when it is opened and written back when it is closed or the program ends, which is what
lets `REWIND`, `BACKSPACE`, `ENDFILE` and direct access (`REC=`) work on any file. `READ`, `WRITE` and the file statements
take `IOSTAT=`, `ERR=` and, for `READ`, `END=`. An internal file is a `CHARACTER` variable used as the unit
(`WRITE (BUF, '(I3)') N`, `READ (BUF, *) X`); character arrays as internal files are not supported. Unformatted records
are written as Lisp data, one record per line, and can be read back only by this Tool Kit.

All the edit descriptors are implemented: `I`, `F`, `E`, `D`, `G` (with `Ew.dEe` and the scale factor `kP`), `A`, `L`, the
radix descriptors `Z`, `O` and `B`, `X`, `T`, `TL`, `TR`, `/`, `:`, quoted strings and Hollerith constants, `S`, `SP`,
`SS`, `BN` and `BZ`, repeat counts and nested groups, and **format reversion** (when the items outnumber the descriptors
the format is used again from its last group, on a new record). A format may be a `FORMAT` label, a character constant, or
any character expression, parsed when the statement runs. A data item of the wrong type for its descriptor (`F5.1` for a
character) is an error. **No carriage-control character is interpreted**: the first character of a record is printed.

List-directed output (`PRINT *,`) writes each item preceded by a blank; reals use the shortest digits that read back as
the same number (`0.1`, `3.1415927`, `1.0D-05`); complex values as `(re,im)`; logicals `T` and `F`; character strings
without quotes. List-directed input accepts blanks, commas and end of record as separators, null values (`1,,3`),
repeat counts (`3*7`), quoted strings, `T`/`F`, complex `(re,im)` and a slash that ends the statement.

## Running programs

**`f77:execute`** (or `f77-execute`; `f77:xqt` is a synonym) runs a compiled main program:

```lisp
(f77:execute 'ftn-user::quadratic :input (make-string-input-stream "1 0 -9"))
```

| Option | Keyword | Effect |
|---|---|---|
| Additional Externals | `:additional-externals` | An alist of (fortran-name . function): Lisp functions FORTRAN may `CALL` or use as functions. They receive references. |
| Blockdata | `:blockdata` | Names the `BLOCK DATA` units to run (by default all in the program's package). |
| Extra Number Space, Extra Character Space | `:extra-number-space`, `:extra-character-space` | Reserve that many more entries, available to Lisp as `*f77-extra-number-space*` and `*f77-extra-character-space*` (references to pass to FORTRAN). |
| Init to Zero | `:init-to-zero` | Initialise storage to zero instead of undefined. |
| Libraries | `:libraries` | Systems to load first (see `f77:def-library`). |
| Pathname Default | `:pathname-defaults` | The directory file names in `OPEN` are relative to. |
| Reload | `:reload` | Discard the saved environment and start afresh. |
| Save Environment | `:save-environment` | Keep the program's storage from one run to the next. |
| System | `:system` | A system to load first. |
| Trap Underflow | `:trap-underflow` | Accepted and ignored: underflow does not trap. |
| Units | `:units` | Routines that must be defined before the program runs. |

Also `:input` and `:output` (a file name, pathname or stream for units 5 and 6; by default the Listener) and `:streams`
(an alist of unit numbers or file names and Lisp streams that `OPEN`, or a plain `READ` on that unit, uses instead of a
file).

In the Listener the same program runs with the **`:Execute FORTRAN`** command, whose options follow the program name:

```
:Execute FORTRAN quadratic Init to Zero yes, Pathname Default /tmp/
```

An option may be abbreviated by its leading words; a Boolean option with no value means yes. The other commands are
`:Compile FORTRAN File` (or `:Compile FORTRAN`), which compiles a file into the running world, `:Show FORTRAN Programs`
and `:Show FORTRAN Warnings`.

From Lisp, `f77:compile-string`, `f77:load-file` and `f77:compile-file` compile source (the last writes a compiled-code
file that defines the routines when loaded); each returns the names defined and, for strings, the compile errors.
`(f77:translate source)` returns the Lisp forms. `f77-compiler-warnings` lists the warnings of the last compilation of
each file.

## Calling between FORTRAN and Lisp

FORTRAN passes arguments by reference, and so do these functions: a routine takes **references**, `(vector . offset)`
conses, for its arguments. When Lisp calls a routine, a plain Lisp value is boxed for it; to receive a result in an
argument, pass a cell made by `f77:make-ref` and read it with `f77:ref-value`. A `FUNCTION` returns its value.

```lisp
(ftn-user::twice 21)                                   ; INTEGER FUNCTION TWICE(K) => 42
```

To hand FORTRAN a region of storage addressed by integer offset, use the macros the Genera Tool Kit named:

```lisp
(f77:with-fortran-number-data (squares 6)              ; FORTRAN sees INTEGER V(6)
  (ftn-user::fill squares 6)
  (loop for i below 6 collect (f77-ref-element squares i)))
(f77:with-fortran-character-data (names 2 :length 8 :initial "none")      ; CHARACTER*8 NAMES(2)
  ...)
```

FORTRAN calls Lisp through **Additional Externals**:

```lisp
(f77:execute 'ftn-user::prog
             :additional-externals (list (cons "SQUARE" (lambda (a r) (setf (svref (car r) (cdr r)) (* (f77-ref-value a) (f77-ref-value a)))))))
```

## Libraries, programs and systems

`f77:def-library` declares a library of FORTRAN (and Lisp) files and `f77:def-program` a program made of files and libraries;
both define systems for `load-system` and `compile-system`, which compile `.f` files with the FORTRAN compiler and Lisp files
with the Lisp compiler (a system may also hold Pascal files, in dependency order).

```lisp
(f77:def-library numerics (:default-pathname "~/num/" :default-package "FTN-USER") "matmul" "solve")
(f77:def-program solver (:default-pathname "~/solver/" :libraries (numerics)) "main" "io")
(load-system 'solver)
(f77:execute 'ftn-user::solver)
```

File names without a type are looked up with `.f`, `.for`, `.ftn`, `.f77`, `.fortran` and `.lisp`. *The Genera call-tree
syntax of these two forms is not recorded in the material this Tool Kit was written from; these definitions are a
reconstruction that gives the same effect: files are loaded in order, after the libraries.*

## Checks at run time

Subscripts and substrings are checked against their bounds (`The subscript 4 of a is outside the array bounds 1:3`), as are
bit substrings; reading an undefined variable is an error; integer division and `MOD` by zero, `SQRT`, `LOG` and `ASIN` out of
their domains, `0.0**(-1)` and a function that returns without a value are errors; a data item of the wrong type for its
edit descriptor, a malformed format, a number that does not parse, end of file, and files that do not exist or already exist
are I/O errors (`IOSTAT=` values: -1 at the end of a file, a positive number otherwise); a call of a routine that is not
defined is an error that names it. Every error is an `f77-runtime-error` that names the routine and line, and I/O errors
are `f77-io-error`s (a subtype with `f77-error-code`).

## FORTRAN mode

Files ending in `.f`, `.for`, `.ftn`, `.f77` or `.fortran` are edited in FORTRAN mode. It colours comments, labels,
continuation marks, character constants, statement keywords, intrinsic functions and routine names. The keys (Genera's in
brackets):

| Key | Effect |
|---|---|
| `TAB` | In columns 0 through 6, pad to the statement field; elsewhere indent the line by its block IF / DO nesting [`TAB`]. |
| `C-j` | End the line and open an indented statement line [`LINE`]. |
| `C-M-j` | Open a continuation card, with the continuation character [`c-LINE`]. |
| `C-M-a`, `C-M-e` | Move to the beginning or end of the current routine. |
| `C-M-f`, `C-M-b` | Move to the next or previous statement. |
| `C-M-h` | Mark the current routine. |
| `C-c C-a` | Show the compiled routine's name and argument types [`c-sh-A`]. |
| `C-c C-c` | Compile the region or the routine at point [`c-sh-C`]. |
| `C-c C-b`, `C-c C-e`, `C-c C-x` | Compile the buffer; compile and execute the program; execute it. |
| `C-c C-w`, `C-.` | Edit compiler warnings; visit the next one. |
| `C-^` | Merge this line with the previous one (a continuation card loses its label and continuation fields) [`c-^`]. |
| `M-.` | Edit the definition of the routine named at point. |

**Electric FORTRAN mode** (`M-x electric fortran mode`, a minor mode) wraps text typed past the **wraparound column**:
the rest of the statement moves to a continuation card that begins with the **continuation character**. A line is broken
after a blank, comma or operator outside a character constant; text inside a constant is never split before column 72, and
the card never grows past column 72. The two variables are settable:

```lisp
(setq f77:wraparound-column 60)           ; documented default 40; wrapping never goes beyond column 72
(setq f77:continuation-character "+")     ; documented default "$"
```

The named commands, run with `M-x`:

| Command | Purpose |
|---|---|
| FORTRAN Mode, Electric FORTRAN Mode | Select fixed-format editing; Electric adds automatic wrapping and continuation. |
| Compile and Execute FORTRAN Program, Execute FORTRAN Program | Compile the program at point and run it; run a compiled one. |
| Compile Buffer, Compile Region, Compile File | Compile the buffer's routines, the region's or the routine at point, or the file on disk. |
| Compile Changed Definitions of Buffer | Compile the routines changed since last compiled (numeric argument 1: since read, 2: since saved, 3: since compiled). |
| Edit Changed Definitions of Buffer, List Changed Definitions | Visit or list the changed routines (same arguments). |
| Compiler Warnings, Load Compiler Warnings, Edit Compiler Warnings | Display, reload or visit the warnings of the last compilation. |
| Edit Definition (`M-.`) | Visit the source of a routine, in this buffer or in the file it was compiled from. |
| List Definitions | Display the routines in the buffer. |
| Find Next FORTRAN Sequence Number, Remove Sequence Numbers | Move to the next non-blank sequence field; blank the sequence fields (of the region or buffer). |
| Replace Tabs | Replace tab characters with fixed-format spacing. |
| Reparse Attribute List, Update Attribute List, Set Package | Recompute the buffer's metadata, write the attribute list, or set the package. |
| Set Breakpoint, List Breakpoints, Clear All Breakpoints | Manage language breakpoints. They are recorded and listed; **the run time does not stop at them**. |

## Not implemented, and what is unverified

* `ENTRY`, `NAMELIST`, recursion (local storage is static), `INTRINSIC` functions beyond the list in `(f77-intrinsic-names)`,
  character arrays as internal files and `CHARACTER*(*)` arrays (their element length is not known).
* `REAL` arithmetic is rounded to single precision when stored, not at each operation; `COMPLEX` parts likewise.
  Equivalencing values of different types does not reinterpret bits.
* The exact exported routine inventory of the Genera FORTRAN libraries (`libraries`, `io-libraries`, `intrinsic-inits`) was
  not recovered, so none of its names is provided beyond the standard intrinsics and the Department of Defense bit
  functions. Genera- and network-compatible I/O is available as ordinary Lisp streams passed with `:streams`.
* The syntax of bit substrings, the Genera spelling of `def-library` and `def-program`, the meaning of Extra Number Space
  and Extra Character Space, and the effect of breakpoints at run time are reconstructions or are not enforced, as marked
  above.
* The documented default of `f77:wraparound-column` is 40 even though the source limit is column 72; the editor honours
  the variable but never wraps past column 72.
