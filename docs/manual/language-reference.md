# The NeoGenera Language

NeoGenera implements a large subset of Common Lisp. This chapter lists what is there and where it differs.

## Numbers

* Fixnums are 61 bits; larger integers are **bignums**. Arithmetic switches automatically and results are
  normalized.
* Ratios are exact: [/](fn:/) of two integers is a ratio unless the division is exact.
* Floats are double-precision only. There are no complex numbers: [sqrt](fn:sqrt) of a negative number signals
  an error.
* Integer division functions return two values: [floor](fn:floor), [ceiling](fn:ceiling), [truncate](fn:truncate),
  [round](fn:round). [expt](fn:expt) is exact for integer powers, [isqrt](fn:isqrt) and [gcd](fn:gcd) work on bignums.
* Bit operations: [ash](fn:ash), [logand](fn:logand), `logior`, `logxor`, [integer-length](fn:integer-length).

## Strings and characters

Strings are byte vectors: UTF-8 text passes through reading, printing and files unchanged, but
[length](fn:length) counts bytes. Characters are code points up to U+10FFFF.

## Sequences and hash tables

The sequence functions work on lists, vectors and strings: [remove](fn:remove), [find](fn:find),
[position](fn:position), [subseq](fn:subseq), [concatenate](fn:concatenate), [coerce](fn:coerce),
[map](fn:map). [make-hash-table](fn:make-hash-table) supports `eq`, `eql`, `equal` and `equalp` tests.

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

## Compiling and loading

Every form is compiled to bytecode before it runs. [compile-file](fn:compile-file) writes a `.nfasl` file
and [load](fn:load) reads source or compiled files. The system compiles itself: `make selfhost` checks that the
compiled compiler produces byte-identical output.

## Not implemented

Complex numbers, multi-dimensional arrays, pathnames as objects, `pprint-logical-block`, the `~R ~E ~G` and
`~<` format directives, readtables, and a generational garbage collector.
