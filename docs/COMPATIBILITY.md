# Compatibility with ANSI Common Lisp

NeoGenera implements a large, practical subset of Common Lisp, written to be read and changed rather than to be
a drop-in replacement for SBCL or CCL. This document lists the deliberate deviations. `make conformance` runs
`tests/conformance/cases.lisp` (about 600 cases written against the specification) and writes
[`CONFORMANCE.md`](CONFORMANCE.md); the ANSI `ansi-test` suite is not used (it could not be fetched offline), so no
ansi-test pass rate is claimed.

## What is implemented

Numbers (fixnums, bignums with Karatsuba multiplication, exact ratios, double floats with IEEE infinities and NaN), conses and lists,
sequences, strings and characters, vectors, hash tables, structures, symbols and packages, `loop`, `format`, `defstruct`, conditions and
restarts, CLOS (multiple dispatch, `call-next-method`, `:before`/`:after`/`:around`, EQL specializers, C3 class precedence), Gray streams,
a printer with `pprint`, a compiler to bytecode that compiles itself, an interactive debugger, and `setf` for the common places
(including `getf`).

## Deliberate deviations

**Numbers**
* Complex numbers are in the tower (rational or float parts); `abs` of a complex number is always a float, and the transcendental
  functions use the C library's branch cuts.
* Floats are a single type (double); there are no single-float, short-float or long-float distinctions, and `float` takes no prototype.
* Division by zero with `/` signals `division-by-zero` for floats too; infinities and NaN arise from overflow and `+infinity+`, `+nan+`.
* `*print-base*` and `*print-radix*` are honoured for integers and ratios (not floats); `~D ~B ~O ~X` bind them as ANSI requires.

**Strings and characters**
* Strings hold characters: `length`, `char`, `schar`, `subseq` and `position` count code points. Files, the terminal,
  the OS and `run-program` exchange UTF-8, decoded on input and encoded on output; a malformed byte reads as the
  character with that code. There are no external formats other than UTF-8. Case conversion and the character
  predicates cover Latin, Greek, Cyrillic and the other scripts of `lisp/core/63-unicode.lisp`; a mapping that changes
  a string's length (`ß` → `SS`) is not made. `base-char` is the same type as `character`.
* `format` implements the standard directives, including `~E ~F ~G ~$` with all their parameters, `~<...~>` justification and
  `~<...~:>` logical blocks, `~_ ~I ~W ~/name/` and `~T`. `~R` spells numbers in English up to the quintillions.

**Arrays**
* Arrays of any rank exist (`make-array` with a list of dimensions, `aref` with several subscripts, `#2A(...)`).
* **Specialised element types.** `:element-type` is upgraded as in SBCL (`upgraded-array-element-type`): `bit`, `(unsigned-byte n)` for
  n up to 64 (upgraded to 1, 2, 4, 8, 16, 32 or 64 bits), `(signed-byte 8/16/32/64)`, `fixnum`, `single-float`, `double-float`,
  `character` (a string), and `t` for anything else (`(integer 0 *)`, `float`, `(unsigned-byte 65)`, ...). Such arrays default their
  elements to 0 / 0.0, check every store, `:initial-element`, `:initial-contents` and `vector-push-extend` against the element type
  (a violation is a `type-error`), report it from `array-element-type`, keep it through `subseq`/`copy-seq`/`reverse`/`adjust-array`,
  and match `(array (unsigned-byte 8) (*))`, `bit-vector` and friends in `typep`. **Storage is not packed**: every element still takes a
  machine word, so the benefit is checking and semantics, not memory. There is no single/double distinction in the float
  implementation, so both float types hold the same flonums. A specialised vector is not a `simple-vector`; `bit`/`sbit`, `#*0101`
  and `#5*1` read and print bit vectors.
* **Displaced arrays.** `(make-array dims :displaced-to array :displaced-index-offset n)` shares the elements of any array (vector,
  string, adjustable or multi-dimensional, or another displaced array), in row-major order, so a vector can be displaced to a matrix
  and the reverse; writes through either side are seen by the other, and `array-displacement` returns the target and offset. The
  offset and size are checked against the target when the array is made. Differences: `:element-type`, when omitted, is inherited from
  the target (CL's default of `t` would make displacing to a specialised array an error), and giving a different one is an error; a
  displaced vector with a fill pointer cannot be extended with `vector-push-extend`; a displaced array shares the target's *current*
  length when the target has a fill pointer.
* `adjust-array` changes an adjustable vector (one made `:adjustable t` or with a fill pointer, and possibly displaced) in place and
  returns it, including to displace it with `:displaced-to`; any other array (a simple vector, a string, a multi-dimensional array) is
  not changed, and a new array with the elements that fit is returned. The rank cannot change. Multi-dimensional arrays are never adjustable
  in place.

**Evaluation and the compiler**
* There is one lexical environment model. `compile-file` writes a binary `.nfasl` (`:format :text` writes the older text one); the file format is
  specific to this system and its version (a file from another version is refused with a message that says to recompile it).
* Declarations are acted on by the compiler: `type`, `ftype`, `the`, `optimize` (`safety` and `debug`; `speed`, `space` and
  `compilation-speed` are accepted and have no effect), `inline`/`notinline` (only `notinline` does anything) and `special`.
  At `safety` 1-3 declared types are checked and a violation signals a `type-error`; at `safety` 0 they are trusted, and a false
  declaration is undefined behaviour. Types outside the compiler's lattice (`string`, `vector`, user `deftype`s, `member`, ...) are
  accepted and ignored, not checked. A checked `the` returns only the primary value of a multiple-value form.
  `loop` has no `of-type`. See docs/design/compiler.md.
* `(make-array n :adjustable t)` without `:fill-pointer` and without a specialised element type gives a vector of fill pointer 0, and
  ignores `:initial-element` and `:initial-contents` (use `:fill-pointer t` or `vector-push-extend`). With a specialised element type
  it is a vector of length n without a fill pointer.
* Redefinition of a function replaces it everywhere at once, including calls inside already-compiled code, because calls go through the symbol.
* `defmacro` expansion is memoised in place by the evaluator, so macros with side effects at expansion time run once per source form.

**CLOS**
* Supported: multiple dispatch with `eql` specializers, `:before`/`:after`/`:around`, `call-next-method`, `:allocation :class` slots,
  the built-in and user-defined (short and long form) method combinations, class redefinition that updates existing instances
  (`update-instance-for-redefined-class`, `make-instances-obsolete`), `change-class` with `update-instance-for-different-class`,
  and metaclasses (`(:metaclass m)` for subclasses of `standard-class`; their slots live in a hidden instance), with
  `standard-class`, `built-in-class` and `structure-class` usable as types for class objects.
* The meta-object protocol covers the protocols programs usually extend, with these limits. Method combinations are objects
  (`find-method-combination`, `generic-function-method-combination`, `method-combination-name`/`-options`), and
  `compute-effective-method` is a generic function that returns the effective method as a form using `call-method` and
  `make-method`; adding a method to it (specialize on `(eql #'a-generic-function)`) changes how that function's methods are
  combined. `validate-superclass` is called by `defclass`, `compute-class-precedence-list`, `compute-slots` and
  `compute-effective-slot-definition` are called for classes with their own metaclass, and `slot-value-using-class`,
  `(setf slot-value-using-class)`, `slot-boundp-using-class` and `slot-makunbound-using-class` are called by the slot
  functions once a program has defined a method on one of them (until then the slot functions take the direct path).
  Also provided: `funcallable-standard-class` and `funcallable-standard-object` (with `set-funcallable-instance-function`),
  `standard-generic-function`, `standard-method` and the slot-definition classes, `add-method`, `compute-applicable-methods-using-classes`,
  `generic-function-name`/`-lambda-list`/`-argument-precedence-order`/`-method-class`, `method-generic-function`, `method-function`,
  `method-lambda-list`, `specializer-direct-methods`, `specializer-direct-generic-functions`, EQL specializers as `(:eql . object)`
  (`intern-eql-specializer`), `slot-definition-location`, `standard-instance-access`, `add-direct-subclass`/`remove-direct-subclass`
  and the dependents protocol (`add-dependent`, `update-dependent`, ... called when a method is added or removed).
  Not provided: generic functions are not instances, so a program cannot subclass `standard-generic-function`, use
  `:generic-function-class`, or customize `compute-discriminating-function` (it only returns the function);
  `ensure-class-using-class`, `make-method-lambda`, `reader-method-class`/`writer-method-class`, the slot-definition initargs
  protocol, `finalize-inheritance` as a generic function, and a metaclass inherited by subclasses (each class names its own).
  A method added to `compute-effective-method` applies to every generic function unless it specializes on one, and
  discards every cached effective method.

**Flavors** (the Symbolics/Lisp Machine object system, a layer over CLOS; see `docs/manual/flavors-manual.md`)
* Supported, following the Lisp Machine Manual's Flavors chapter: `defflavor` with the common options (including `:abstract-flavor`),
  `(defmethod (flavor [type] message) ...)` including `:case`, `send` and friends, daemons, `:around` and `:inverse-around` with
  the manual's calling convention, `defwrapper`, `defwhopper` (a Symbolics addition), the `:daemon-with-or/and/override`, `:progn :or :and
  :append :nconc :list :max :min :+ :case` combinations, `:init`, `vanilla-flavor` and `property-list-mixin`.
* Also supported: instances are funcallable (`(funcall instance :message ...)` sends the message; `functionp` is true), the
  `:pass-on` and `:inverse-list` combinations, `:alias-flavor`, `:run-time-alternatives`, `:instantiation-flavor-function`,
  `:special-instance-variables` and `with-self-variables-bound`. **These were written from memory of the Lisp Machine
  manual, which could not be consulted**: the `:run-time-alternatives` syntax and the list `:inverse-list` returns are this
  system's reading of it, and special instance variables are copied in and out of the instance, not aliases of its slots.
  Not supported: `(declare (:self-flavor ...))`, `si:print-readably-mixin` and the `:break` message. `describe` and `:print-self`
  follow the manual's format except that instances print without an address.

**Conditions and the debugger**
* The interactive debugger with restarts runs in both the line-mode and the window Listener. `break` enters it with a CONTINUE restart, and it honours `*debugger-hook*`.
* `with-condition-restarts` and `invoke-restart-interactively` are implemented. The debugger can return values from a frame (`:return`) or run it again (`:retry`); it shows argument values but not local variables.

**Packages, reading and printing**
* The standard package API is implemented: `make-package`, `defpackage` (`:nicknames :use :shadow :shadowing-import-from :import-from :export :intern :documentation :size`, plus SBCL's `:lock`), `in-package` (an unknown package is an error), `rename-package`, `delete-package` (with a `continue` restart when the package is still used), `use-package`/`unuse-package`, `export`/`unexport`, `import`/`shadowing-import`, `shadow`, `unintern`, `find-symbol`, `find-all-symbols`, `package-nicknames`, `package-use-list`, `package-used-by-list`, `package-shadowing-symbols`, `do-symbols`, `do-external-symbols`, `do-all-symbols` and `with-package-iterator`. `use-package`, `export` and `import` signal a `package-error` on a name conflict (shadowing resolves it). Limits: the conflict errors have no restarts (no "unintern the old one" choice); `*package*` may be bound to a package that is later deleted; `delete-package` does not unintern the symbols' function or value cells.
* **Package locks** follow SBCL's model. A locked package cannot be modified (interning a new symbol, `import`, `export`, `unexport`, `unintern`, `shadow`, `shadowing-import`, `use-package`/`unuse-package` *by* it, `rename-package`, `delete-package`; no-op requests are allowed), and its symbols cannot be given a function, macro, special-variable or constant definition, or have them removed (`defun`, `defmacro`, `defgeneric`, `defvar`, `defparameter`, `defconstant`, `(setf symbol-function)`, `(setf macro-function)`, `fmakunbound`, `makunbound`). Reading, calling, binding, `setf` of the value and property lists are never restricted, and `(SETF x)` function-name symbols and new keywords are exempt from the intern check. The violation is a `package-lock-violation` (a `package-error`). `lock-package`, `unlock-package` and `package-locked-p` manage locks; `without-package-locks` and `with-unlocked-packages` are the escape hatches (also `*package-locks-disabled*`). `defstruct`, `defclass`, `define-condition` and `deftype` are checked too, when the form is macroexpanded (so at compile time, as in SBCL). **`COMMON-LISP` is not locked in the shipped image**, because the system sources, the tests and the example programs all live in it; run `(lock-package :cl)` to protect it (anything you then define in `COMMON-LISP`, including the test files, needs `without-package-locks`). Most library symbols live in `COMMON-LISP` (they are not exported from it in the strict sense).
* The reader is written in C and consults `*readtable*`: macro characters, dispatch characters and the readtable case are supported for
  characters below 128. `set-syntax-from-char` copies whitespace, macro (function, terminating-ness, dispatch table) or constituent syntax, so
  `(set-syntax-from-char #\, #\Space rt)` makes the comma separate tokens, and a standard whitespace character can be made a constituent; it
  also works across readtables. The to-character must be below 128 (above that every character is a constituent, and asking for anything else is an
  error). `peek-char t` skips what the current readtable calls whitespace. `#C`, `#P`, `#A` and `#*` read.
* `pprint` is a line-filling printer for code; `pprint-logical-block`, `pprint-newline`, `pprint-indent`, `pprint-tab` and `pprint-fill`/`-linear`/`-tabular`
  lay out logical blocks. `*print-pretty*` makes `prin1`/`princ`/`print`/`write` lay out lists with the `pprint` algorithm (not the logical-block machinery, and not `print-object` methods or vectors); it is off by default.
* `*print-length*`, `*print-level*`, `*print-circle*` and `*print-case*` are honoured by `prin1`/`princ`/`print`/`write` for conses, vectors and structures (`write` takes `:length :level :circle :case`). `*print-circle*` labels shared as well as circular conses, vectors and structures (`#1=`/`#1#`); strings and atoms are never labelled. When `*print-pretty*` is also true, `*print-level*` or `*print-circle*` makes the list print flat rather than through the pretty printer. `*print-case*` changes only all-uppercase symbol names (and package prefixes); a name with lowercase letters is left alone. `*print-lines*` and `*print-readably*` are not implemented.

**System interface**
* Pathnames are Unix paths with Common Lisp components; logical pathnames translate through `logical-pathname-translations`. `directory`
  handles `*` and `**`. `with-open-file` supports `:direction`, `:if-exists`, `:if-does-not-exist` for text files only.
* The garbage collector is a generational copier (a nursery and a two-space old generation). `make-hash-table` takes
  `:weakness` (`:key`, `:value`, `:key-and-value`, `:key-or-value`, as in SBCL); `:key` entries are ephemerons, so
  a value that refers to its own key does not keep the entry. `make-weak-pointer`, `weak-pointer-value`,
  `finalize` (with `:dont-save`) and `cancel-finalization` follow SBCL. An object in the old generation is found
  dead only by a major collection, so a weak entry or finalizer may wait for one; `(gc)` forces it. Finalizers run at
  the next safe point after the collection, not during it. `eq` hash tables hash structure and CLOS instances to a
  single bucket (there are no address-based hashes in a moving collector), so a large `eq` table keyed by instances
  is slow, weak or not.

## Extensions

The Genera-flavoured environment (window system, presentations, editor, hypertext documentation, Inspector, cross-reference, profiler), the libraries for mathematics,
statistics and bioinformatics, and the safe subprocess layer are extensions and have their own manuals (`:doc`).
