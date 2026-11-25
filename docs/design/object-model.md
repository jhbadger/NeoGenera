# Object model

Status: implemented in `src/neogenera.h`, `src/memory.c`, `src/symbols.c`.

Every Lisp value is one 64-bit machine word (`Obj`). The low three bits are a tag.

| Tag | Name | Meaning |
|---|---|---|
| `000` | fixnum | signed integer in the upper 61 bits (`value << 3`), range ±2^60 |
| `001` | cons | pointer to a headerless two-word cell in the **cons space** |
| `010` | object | pointer to a header-prefixed object in the **object space** |
| `011` | immediate | character (`kind 0`, code point in bits 8+) or special marker (`kind 1`) |
| `111` | header | only ever appears in an object's first word, never as a value |

Special markers: `UNBOUND`, `EOF_OBJ`, `UNSUPPLIED` (an omitted `&optional`), and `FWD_MARK`
(used only by the collector to forward conses).

## Objects

The header word is `(nwords << 16) | (type << 8) | 7`. `nwords` counts payload words, so
the collector can size any object without knowing its type.

| Type | Payload | Notes |
|---|---|---|
| symbol | name, value, function, plist, package, flags | `flags` holds SPECIAL/CONSTANT bits and, in bits 8+, the special-form id the evaluator dispatches on |
| string | raw byte length, then bytes, NUL padded | not scanned by the GC; strings are **bytes**, not Unicode code points |
| vector | elements | simple-vector |
| flonum | one raw `double` | boxed; not scanned |
| evaluator closure | name, lambda list, body, lexical vars, lexical functions, arity cache | stage-0 interpreter |
| bytecode closure | code object, then captured variables | produced by the compiler |
| code | name, nreq, nopt, rest flag, instruction bytes, constants vector, arglist | instructions are an `int32` array in a raw string |
| primitive | table index, name, min/max args | C function pointers live in `prim_table`, never in the heap |
| stream | kind, file index, buffer, length, position, column, peek | stdio, file and string streams |
| package | name, nicknames, symbol table, external table, use list, used-by | tables are 1021-bucket vectors of symbol lists |
| struct | descriptor `(NAME . SLOTS)`, then slot values | `defstruct` instances |
| adjustable vector | data vector or string, fill pointer | `make-array :fill-pointer` |

## NIL and image portability

`NIL` is an ordinary symbol, but it is allocated outside both semispaces so it never moves.
It lives in a page **at a fixed virtual address** just past the heap (`HEAP_HINT + 4·semispace`).
A heap image stores raw `NIL` pointers, so the address must be identical in every process;
an earlier version used a static C array and broke under ASLR. The same rule applies to
anything else that would put a process-specific address into the heap: primitives are stored
as table indices and file streams as descriptor indices for this reason.

## Numbers

Fixnums and flonums only. Fixnum overflow signals an error. Bignums and ratios are planned
(roadmap Phases 4 and 8). `/` returns an integer when exact and a flonum otherwise, which is
a documented deviation from ANSI Common Lisp until ratios exist.

## Characters and strings

Characters are immediates holding a code point up to U+10FFFF. Strings are byte vectors:
UTF-8 text passes through reading, printing and file I/O unchanged, but `length`
counts bytes. Proper Unicode strings are future work.
