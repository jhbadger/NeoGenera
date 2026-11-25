# NeoGenera

A Lisp environment in the spirit of the Symbolics Genera workstations: a small C kernel, and a
system written in Lisp itself. The long-term plan (text-mode window system, Zmacs-style editor,
hypertext documentation, bioinformatics / statistics / mathematics libraries) is in
[`ROADMAP.md`](ROADMAP.md).

**Status:** roadmap phases 0-6 are implemented. NeoGenera has a C kernel (tagged objects, bignums and ratios, a
copying garbage collector, bytecode VM, heap images, terminal backend), a Common Lisp subset written in Lisp
(CLOS, conditions and restarts, Gray streams, `loop`, `format`, `defstruct`), a bytecode compiler written in
Lisp that **compiles itself to a fixed point**, an interactive debugger, a text-mode window system with
mouse-sensitive output (presentations), an Emacs-style editor with an undo tree, and developer tools
(Inspector, cross-reference, trace, stepper, profiler, browsers). See [`ROADMAP.md`](ROADMAP.md#status) for
what is and is not done.

## Quick start

```sh
make world             # build kernel + the self-hosted system image (needs a C compiler and make)
bin/neogenera          # line-mode Listener with the interactive debugger
bin/neogenera --ui     # the window system: Listener, editor, Inspector, browsers (needs a terminal)
make test              # C, Lisp and real-terminal (pty) tests
```

In the window system: type Lisp forms or `:commands` (Tab completes, `:help`), click results (left = default
action, middle = copy to input, right = menu), `:edit mapcar` opens the editor on a function's source,
`:inspect expr`, `:who-calls fn`, `:browse-class name`. Keys: F6 next window, F7 tile, F8 theme, F10 quit.

```
NeoGenera 0.6.0 -- type :help for help, :quit to leave.
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
| `lisp/core/` | the Lisp library: macros, places, sequences, structures, hash tables, `format`, `loop`, CLOS, conditions, streams, debugger, Listener, test framework |
| `lisp/compiler/` | the compiler, `compile-file`, fasl loader, build helpers |
| `lisp/ui/` | screen, window manager, presentations, command processor, Listener window, tool windows |
| `lisp/editor/` | buffers with undo tree, the editor window, Lisp mode, files, Dired, version control |
| `lisp/tools/` | cross reference, trace, stepper, profiler |
| `tests/c`, `tests/lisp`, `tests/ui` | kernel unit tests; Lisp suites and benchmarks; a pty test with a VT emulator |
| `docs/design/` | object model, GC, VM, compiler, bootstrap, window system, editor, tools |

## How it builds itself

`make world` produces `stage0` (the C evaluator loads the system), `stage1` (the interpreted compiler
compiles the system), then `stage2` (the *compiled* compiler recompiles the system) and checks that
stage 1 and stage 2 are byte-identical. Details in
[`docs/design/bootstrap.md`](docs/design/bootstrap.md).

## Contributing

See [`CONTRIBUTING.md`](CONTRIBUTING.md) and [`STYLE.md`](STYLE.md). Licensed under the MIT License.
