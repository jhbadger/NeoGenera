# The NeoGenera Language

NeoGenera implements a large subset of Common Lisp. This chapter lists what is there and where it differs.

## Numbers

* Fixnums are 61 bits; larger integers are **bignums**. Arithmetic switches automatically and results are
  normalized.
* Ratios are exact: [/](fn:/) of two integers is a ratio unless the division is exact.
* Floats are double-precision only.
* Complex numbers have rational or float parts: `(complex 1 2)` or `#C(1 2)`. Arithmetic, `expt`, `sqrt`, `exp`, `log` and the
  trigonometric functions accept them, and [sqrt](fn:sqrt) and [log](fn:log) of a negative number return one.
  [realpart](fn:realpart), [imagpart](fn:imagpart), [conjugate](fn:conjugate), [phase](fn:phase), [cis](fn:cis) and `abs` take them apart.
* Integer division functions return two values: [floor](fn:floor), [ceiling](fn:ceiling), [truncate](fn:truncate),
  [round](fn:round). [expt](fn:expt) is exact for integer powers, [isqrt](fn:isqrt) and [gcd](fn:gcd) work on bignums.
* Bit operations: [ash](fn:ash), [logand](fn:logand), `logior`, `logxor`, [integer-length](fn:integer-length).

## Strings and characters

Strings hold characters, which are code points up to U+10FFFF: [length](fn:length), [char](fn:char)
and [subseq](fn:subseq) count characters, and `(setf (char s i) #\λ)` works on any string. Files, the
terminal and the operating system exchange UTF-8, which is decoded on input and encoded on output.

## Arrays

`(make-array '(2 3) :initial-element 0)` makes a two-dimensional array; `(aref a 1 2)` and `(setf (aref a 1 2) x)` use it, and it
prints and reads as `#2A((0 0 0) (0 0 0))`. `array-rank`, `array-dimensions`, `array-dimension`, `array-total-size`,
`array-row-major-index`, `row-major-aref`, `array-in-bounds-p` and `adjust-array` work on arrays of any rank. A one-dimensional array is a
vector; vectors can have a fill pointer and be adjustable.

## Sequences and hash tables

The sequence functions work on lists, vectors and strings: [remove](fn:remove), [find](fn:find),
[position](fn:position), [subseq](fn:subseq), [concatenate](fn:concatenate), [coerce](fn:coerce),
[map](fn:map). [make-hash-table](fn:make-hash-table) supports `eq`, `eql`, `equal` and `equalp` tests, and
`:weakness` (`:key`, `:value`, `:key-and-value` or `:key-or-value`) for tables whose entries the garbage collector
may remove: a `:key` table, for example, forgets an entry once nothing else refers to its key.

[make-weak-pointer](fn:make-weak-pointer) makes a reference that does not keep its object alive;
[weak-pointer-value](fn:weak-pointer-value) returns the object, or NIL and NIL once it is collected.
`(finalize object function)` calls `function` (with no arguments) after `object` has been collected; the function
must not itself refer to the object. [cancel-finalization](fn:cancel-finalization) removes it.

## Control and binding

Special forms are implemented by the compiler: `quote function if progn let let* setq flet labels macrolet
symbol-macrolet block return-from tagbody go catch unwind-protect multiple-value-call progv`. Everything else,
including [loop](fn:loop), [case](fn:case), [destructuring-bind](fn:destructuring-bind) and the iteration
macros, is a macro. Tail calls are proper except inside dynamic (special-variable) bindings.

## Objects (CLOS)

Classes are defined with [defclass](fn:defclass) and generic functions with [defgeneric](fn:defgeneric) and
[defmethod](fn:defmethod). Dispatch is on all required arguments, supports `(eql x)` specializers, the
standard method combination with `:before`, `:after` and `:around`, and [call-next-method](fn:call-next-method).
Built-in objects have classes: `(class-of 1)` is `fixnum`. Redefining a class updates it in place; existing
instances are **not** updated. There are no metaclasses and no `:allocation :class` slots.

## Conditions

NeoGenera has the full handler and restart model: [handler-case](fn:handler-case),
[handler-bind](fn:handler-bind), [restart-case](fn:restart-case), [signal](fn:signal), [error](fn:error),
[warn](fn:warn), [cerror](fn:cerror), [ignore-errors](fn:ignore-errors). Errors raised by the kernel are
typed conditions such as `type-error`, `division-by-zero`, `undefined-function` and `unbound-variable`.

## Streams

The standard stream functions work on native streams (files, strings, the terminal) and on **Gray streams**:
subclass `fundamental-character-output-stream` and define `stream-write-char`, or
`fundamental-character-input-stream` and `stream-read-char`. Synonym, broadcast, two-way, echo and concatenated
streams are provided.

## Reading

The reader consults `*readtable*`. [set-macro-character](fn:set-macro-character) makes a character call a function when it is read,
[make-dispatch-macro-character](fn:make-dispatch-macro-character) and [set-dispatch-macro-character](fn:set-dispatch-macro-character) add
`#x`-style syntax, [read-delimited-list](fn:read-delimited-list) reads up to a closing character, and `(setf (readtable-case rt) :invert)`
changes how symbol names are converted. Work on a copy: `(let ((*readtable* (copy-readtable))) ...)`.

```lisp
(set-macro-character #\[ (lambda (stream char) (declare (ignore char)) (coerce (read-delimited-list #\] stream t) 'vector)))
(set-macro-character #\] (get-macro-character #\)))
```

Only characters below 128 can be macro characters. `#P"/tmp/x"` reads a pathname, `#C(1 2)` a complex number and `#2A((1 2) (3 4))` an
array.

## Printing

[format](fn:format) has the standard directives, among them `~F ~E ~G ~$` for numbers with all their parameters, `~R` for English
words and Roman numerals, `~T` for tabs, `~<...~>` for justification and `~/name/` to call a function.

The pretty printer lays out *logical blocks*: [pprint-logical-block](fn:pprint-logical-block) prints a list with
[pprint-pop](fn:pprint-pop), and [pprint-newline](fn:pprint-newline) marks where a line may break: `:linear` (all of the block's breaks
happen if it does not fit), `:fill` (as many items per line as fit), `:miser` or `:mandatory`. [pprint-indent](fn:pprint-indent) sets the
indentation and [pprint-tab](fn:pprint-tab) moves to a column. In `format`, `~<...~:>` is a logical block, `~_` a conditional newline and
`~I` an indentation; `~<...~:@>` fills. [pprint-fill](fn:pprint-fill), [pprint-linear](fn:pprint-linear) and [pprint-tabular](fn:pprint-tabular) print lists.

```lisp
(let ((*print-right-margin* 20) (*print-miser-width* nil))
  (format nil "~<~@{~A~^ ~:_~}~:>" '(aaaa bbbb cccc dddd eeee ffff)))
;; => "AAAA BBBB CCCC DDDD
;;     EEEE FFFF"
```

## Files and pathnames

File functions accept namestrings and pathnames. A pathname has a directory, a name and a type: `(pathname "/usr/lib/foo.tar.gz")` has the
directory `(:absolute "usr" "lib")`, the name `"foo.tar"` and the type `"gz"`. [merge-pathnames](fn:merge-pathnames) fills in missing
parts, [make-pathname](fn:make-pathname) builds one, [namestring](fn:namestring) turns it back into a string. Wildcards `*` and `**` (any
number of directories) work in [directory](fn:directory), [pathname-match-p](fn:pathname-match-p) and
[translate-pathname](fn:translate-pathname).

A **logical pathname** names files through a host you define:

```lisp
(setf (logical-pathname-translations "SRC") '(("**;*.*" "/home/me/project/src/**/*.*")))
(probe-file "SRC:UI;MAIN.LISP")     ; => #P"/home/me/project/src/ui/main.lisp"
```

[probe-file](fn:probe-file) and [truename](fn:truename) return the absolute pathname, and
[ensure-directories-exist](fn:ensure-directories-exist) creates missing directories.

## Compiling and loading

Every form is compiled to bytecode before it runs. [compile-file](fn:compile-file) writes a `.nfasl` file
and [load](fn:load) reads source or compiled files. The system compiles itself: `make selfhost` checks that the
compiled compiler produces byte-identical output.

## Not implemented

Displaced arrays and specialised array element types, and making characters whitespace with `set-syntax-from-char`.

The garbage collector is generational: new objects are collected cheaply while they are young, and a full collection
copies the old ones only when they have grown. Collection never runs inside a primitive, so a pause is short and
happens between calls.
