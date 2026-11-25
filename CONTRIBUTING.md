# Contributing to NeoGenera

NeoGenera is a Lisp environment in the spirit of Symbolics Genera: a small C kernel and a large
system written in Lisp itself. Start with `ROADMAP.md` and `docs/design/`.

## Building and testing

```sh
make world        # kernel, then stage0 -> stage1 -> stage2, fixed-point check, final image
make test         # C unit tests + Lisp tests + pty tests (against build/neogenera.img)
make test-pty     # the window system in a real pseudo-terminal (needs python3)
make test-stress  # Lisp tests with a collection at every GC safe point (slow)
SAN=1 sh tests/c/run.sh        # C unit tests under ASan/UBSan
make asan                      # build/neogenera-asan, with dead-semispace poisoning
bin/neogenera                  # start the Listener
```

A change is not ready until `make world` reaches the fixed point and `make test` passes. If the
fixed-point check fails, the compiler or VM behaves differently when interpreted and when compiled:
treat it as a bug, not as noise.

## Rules that matter

* **C code that calls back into Lisp must follow the GC rules** in `docs/design/gc.md`
  (`PROTECT` live locals, restore `nroots`, read primitive arguments through `argv`).
  Run `make test-stress` and the ASan build for any change to `src/`.
* **No process-specific addresses in the heap** (`docs/design/object-model.md`).
* Lisp sources are loaded in the order of `lisp/sources.lisp`. Top-level forms in `lisp/core` should be
  definitions, so that load order matters only at call time.
* Every new Lisp feature needs a test in `tests/lisp/`; every new C facility needs a check in
  `tests/c/kernel_tests.c`.
* UI and editor code must work headless: drive it with `WM-FEED` and assert on the screen buffer; add a pty
  check in `tests/ui/pty_test.py` for anything that depends on the real terminal.
* Document design decisions in `docs/design/` when they change an invariant.
* Public functions, macros, variables and classes in `lisp/core/` need a docstring (or a manual mention); `docs-tests.lisp` enforces 100% coverage. Manuals live in `docs/manual/*.md` and are compiled into the image.

See `STYLE.md` for formatting conventions.
