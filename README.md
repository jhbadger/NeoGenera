# NeoGenera

A Lisp environment in the spirit of the Symbolics Genera workstations: a small C kernel, and a
system written in Lisp itself. The long-term plan (text-mode window system, Zmacs-style editor,
hypertext documentation, bioinformatics / statistics / mathematics libraries) is in
[`ROADMAP.md`](ROADMAP.md).

**Status:** roadmap phases 0–3 are complete. NeoGenera has a C kernel (tagged objects, copying garbage
collector, reader, printer, stage-0 evaluator, bytecode VM, heap images), a core Common Lisp subset
written in Lisp, a Lisp-to-bytecode compiler written in Lisp, and it **compiles itself to a fixed
point**. See [`ROADMAP.md`](ROADMAP.md#status) for what is and is not done.

## Quick start

```sh
make world          # build kernel + the self-hosted system image (needs a C compiler and make)
bin/neogenera       # start the Listener
make test           # C and Lisp test suites
```

```
NeoGenera 0.3.0 -- type :help for help, :quit to leave.
CL-USER> (defun fact (n) (if (<= n 1) 1 (* n (fact (- n 1)))))
FACT
CL-USER> (fact 20)
2432902008176640000
CL-USER> (loop for i from 1 to 5 collect (* i i))
(1 4 9 16 25)
CL-USER> (disassemble (compile nil '(lambda (x) (+ x 1))))
```

## Layout

| Path | Contents |
|---|---|
| `src/` | the C kernel |
| `lisp/core/` | the Lisp library: macros, places, sequences, structures, hash tables, `format`, `loop`, errors, Listener, test framework |
| `lisp/compiler/` | the compiler, `compile-file`, fasl loader, build helpers |
| `tests/c`, `tests/lisp` | kernel unit tests; Lisp test suites and benchmarks |
| `docs/design/` | object model, GC, VM, compiler, bootstrap |

## How it builds itself

`make world` produces `stage0` (the C evaluator loads the system), `stage1` (the interpreted compiler
compiles the system), then `stage2` (the *compiled* compiler recompiles the system) and checks that
stage 1 and stage 2 are byte-identical. Details in
[`docs/design/bootstrap.md`](docs/design/bootstrap.md).

## Contributing

See [`CONTRIBUTING.md`](CONTRIBUTING.md) and [`STYLE.md`](STYLE.md). Licensed under the MIT License.
