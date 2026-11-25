# NeoGenera Roadmap

NeoGenera is a Lisp environment in the spirit of Symbolics Genera: a single, live, introspectable system with integrated hypertext documentation, a text-mode window system, and a family of editors and tools, extended with bioinformatics, statistics, and mathematics libraries.

**Assumptions:** one developer, roughly 2–3 years (about 10 phases of 3 months), small C kernel with everything else written in NeoGenera Lisp, Common Lisp compatibility as the language target. Dates are relative (Month 1 = project start) and should be treated as budgets, not promises.

## Guiding principles

1. **Small C kernel, large Lisp system.** C holds only what must be native: memory, GC, the VM or native-code emitter, I/O primitives, and the bootstrap. Everything else (compiler, reader extensions, editor, windows, docs, science libraries) is Lisp, so the system can inspect and modify itself.
2. **Always runnable.** Every milestone ends with a bootable image and a working Listener (REPL). No long periods with a broken system.
3. **Self-hosting early.** Moving the compiler and tools into Lisp by Phase 3 means every later feature is dogfooded.
4. **Documentation is part of the system, not an afterthought.** Every function ships with a docstring that appears in the hypertext viewer. Features are not "done" until documented.
5. **Common Lisp compatibility as a target, not a cage.** Aim for ANSI CL behavior where practical; track conformance with the ANSI test suite (e.g. `ansi-test`) and publish the pass rate.
6. **Genera feel without Genera complexity.** Borrow the interaction model (Listener, Zmacs-like editor, Inspector, Document Examiner, mouse-sensitive output, command processor), but render in a text-based window system.

## Architecture at a glance

| Layer | Written in | Contents |
|---|---|---|
| Kernel | C | Object representation, allocator, GC, VM/loader, syscalls, image save/load |
| Core runtime | Lisp | Reader, printer, packages, conditions, CLOS, sequences, numerics, streams |
| Compiler | Lisp | Lisp-to-bytecode (later native) compiler, macroexpander, optimizer |
| System tools | Lisp | Listener, debugger, Inspector, Editor, file system browser, Flavor/CLOS tools |
| Windowing | Lisp (C for terminal I/O) | Text window manager, panes, menus, mouse-sensitive presentations |
| Documentation | Lisp | Hypertext document format, viewer, search, cross-references |
| Science | Lisp (C for hot loops) | Bioinformatics, statistics, math libraries |

## Phase overview

| Phase | Months | Theme | Exit criterion |
|---|---|---|---|
| 0 | 1–2 | Foundations | Repo, build, test harness, design docs |
| 1 | 2–5 | C kernel | Evaluator runs a minimal Lisp in C |
| 2 | 5–8 | Core Lisp in Lisp | Boots to a Listener; basic CL data types |
| 3 | 8–11 | Compiler and self-hosting | NeoGenera compiles itself |
| 4 | 11–15 | Conditions, CLOS, streams | Usable debugger; CL-style object system |
| 5 | 15–19 | Text window system | Multi-pane environment with mouse support |
| 6 | 19–23 | Editor and developer tools | Zmacs-like editor, Inspector, cross-reference tools |
| 7 | 23–27 | Hypertext documentation | Document Examiner-style viewer; full reference docs |
| 8 | 27–31 | Math and statistics | Numerical tower, linear algebra, distributions |
| 9 | 31–35 | Bioinformatics | Sequence I/O, alignment, annotation tools |
| 10 | 35–36+ | Hardening and release 1.0 | Packaging, docs, conformance, community |

Phases 8–9 can start in parallel with Phases 6–7 once the compiler is stable, if time allows. The numeric tower in Phase 8 depends only on Phase 4.

---

## Phase 0: Foundations (Months 1–2)

**Goal:** make decisions that are expensive to change later.

- Write a design document covering object representation (tagged pointers, fixnum width, immediate characters), memory layout, calling convention, and the image format.
- Decide VM vs. native code. Recommendation: **bytecode VM first**, with native x86-64/ARM64 emission as a later optimization once semantics are stable.
- Choose the GC strategy. Recommendation: precise, copying, generational (start with a simple two-space Cheney collector, add generations in Phase 4+).
- Set up the repository, build system (Make or CMake), CI, a C unit-test harness, and a Lisp-level test framework stub.
- Pick license and naming conventions; write `CONTRIBUTING.md` and a style guide.
- Target platforms: macOS (arm64, x86-64) and Linux first; Windows deferred.

**Deliverable:** `docs/design/` with object model, GC, VM, and image-format specs.

## Phase 1: C kernel (Months 2–5)

**Goal:** a tiny but correct Lisp evaluator in C.

- Tagged object representation: fixnums, characters, conses, symbols, strings, vectors, functions.
- Allocator and the first garbage collector (two-space copying, with a precise root set).
- Reader and printer in C (minimal; will be replaced by Lisp versions in Phase 2).
- Special forms: `quote`, `if`, `lambda`, `let`, `setq`, `progn`, `block/return-from`, `tagbody/go`, `catch/throw`, `unwind-protect`, `function`, `flet/labels`.
- Lexical closures, special (dynamic) variables, proper tail calls where feasible.
- Bytecode design and a VM interpreter loop; a stub assembler.
- Primitive library: arithmetic on fixnums/flonums, list operations, symbol/package primitives, basic file and terminal I/O.
- Image save/load so startup does not re-read source each time.
- A bare C-hosted REPL.

**Exit criteria:** the C test suite passes under sanitizers (ASan/UBSan); a GC stress test runs for hours without corruption; the REPL evaluates classic programs (fib, tak, naive reverse, a metacircular evaluator).

**Risks:** GC bugs are the most common source of lost time. Mitigate with a "GC on every allocation" debug mode from day one.

## Phase 2: Core Lisp written in Lisp (Months 5–8)

**Goal:** the system starts to be written in itself.

- Bootstrap process: a stage-0 C-hosted evaluator loads Lisp source files that define the rest of the runtime.
- Lisp-written reader (with readtables and reader macros) and printer (with `*print-*` variables and `print-object` hooks planned).
- Package system, symbols, `intern`, `export`, `use-package`.
- Macros: `defmacro`, `macrolet`, `symbol-macrolet`, backquote; `destructuring-bind` and full lambda lists.
- Standard control and binding macros: `cond`, `case`, `do`, `dolist`, `dotimes`, `loop` (start with a subset, grow toward ANSI `loop`), `multiple-value-*`.
- Data types: lists and sequences, strings, characters, arrays, hash tables, structures (`defstruct`).
- Basic pathname and file-system layer.
- **First Listener:** a line-oriented REPL written in Lisp, with history and basic error reporting.

**Exit criteria:** image boots into the Lisp-written Listener; the C reader and printer are removed from the critical path.

## Phase 3: Compiler and self-hosting (Months 8–11)

**Goal:** NeoGenera compiles NeoGenera.

- Compiler front end: macroexpansion, lexical analysis, closure conversion, and a simple intermediate representation.
- Bytecode back end with basic optimizations: constant folding, inlining of primitives, tail-call elimination, type inference for fixnum arithmetic.
- `compile`, `compile-file`, `load`, `eval-when`, `declaim/declare`, `defconstant`, `load-time-value`.
- **Self-hosting milestone:** the compiler compiles its own source, and the result compiles it again to a bit-identical image (fixed-point check).
- Source-location tracking in compiled code to feed the debugger and editor later.
- Benchmarks suite (Gabriel benchmarks) tracked over time to catch performance regressions.

**Exit criteria:** build the full system from C kernel plus Lisp source with one command; fixed-point test in CI.

## Phase 4: Conditions, CLOS, streams (Months 11–15)

**Goal:** the features that make Common Lisp usable for large programs.

- Condition system: `handler-case`, `handler-bind`, `restart-case`, `signal`, `error`, `warn`, `invoke-debugger`, and the standard condition hierarchy.
- **Debugger:** backtraces with arguments and local variables, restarts menu, frame selection, "return from frame" and "retry".
- CLOS: classes, generic functions, method combination, MOP subset sufficient for the tools (class finalization, slot access, `compute-applicable-methods`). Optimize dispatch with caching.
- Streams: character and binary streams, string streams, synonym/broadcast/two-way streams, gray-stream-style extension protocol (needed by the window system).
- `format` with directives, `pprint` and the pretty printer.
- Full numeric support begins here: bignums, ratios, then floats (see Phase 8 for complex and higher math).
- Generational GC and weak hash tables; finalization hooks.
- Conformance checkpoint: run `ansi-test`, record the baseline pass rate, and set a target (for example 80% by Phase 6, 90% by 1.0).

**Exit criteria:** you can port a mid-sized portable CL library (e.g., a parser or test framework) with minimal changes.

## Phase 5: Text window system (Months 15–19)

**Goal:** the Genera look and feel, in a terminal.

- Terminal backend in C: raw mode, resize handling, 256-color and truecolor, UTF-8, mouse reporting.
- Window manager in Lisp: panes, frames, overlapping and tiled windows, status line, "who-line" (the Genera-style status bar at the bottom), menus, pop-ups, and scrollbars.
- **Presentation system:** output objects remain mouse-sensitive and typed, so that clicking a printed object offers context-appropriate commands (a simplified CLIM/Dynamic Windows-style model).
- **Command processor:** typed commands with completion and argument prompting, alongside raw Lisp forms (the Genera Listener experience).
- **Listener v2:** scrollable history, presentations for returned values, input editing, and a `*` / `**` / `***` style history with clickable results.
- Flicker-free redraw (damage tracking, double buffering) and a theming layer for the 1980s aesthetic (monochrome, amber, green phosphor, classic black-on-white).
- Optional later: an SDL or native GUI backend sharing the same window abstraction.

**Exit criteria:** a multi-window session with a Listener, a file browser, and a pop-up menu, usable all day over SSH.

## Phase 6: Editor and developer tools (Months 19–23)

**Goal:** the integrated environment that is the heart of the Genera experience.

- **Editor (Zmacs-like):** Emacs-style keybindings, buffers, kill ring, incremental search, regions, rectangle commands, undo tree, major and minor modes, extensible entirely in Lisp.
- Lisp mode: indentation, paren matching, structured navigation, `Meta-.` for definitions, evaluation of the form at point, compile region, and arglist display.
- Mini-buffer with completion; command-line integration with the Listener.
- **Inspector:** browse and edit any object (lists, structures, CLOS instances, hash tables, functions) with presentations.
- **Flavor/class examiner**, **function and package browsers**, **cross-reference** ("who calls", "who references", "who defines"), and an **apropos** tool.
- Tracing and stepping: `trace`, a stepper, and profiling (statistical sampling) integrated with presentations.
- **File system tools:** directory editor (Dired-like), version-aware save, simple version control integration (git).
- Source-level debugging in the editor: jump from backtrace frame to source line.
- Test runner UI for the Lisp test framework with clickable failures.

**Exit criteria:** NeoGenera's own source can be edited, compiled, debugged, and tested entirely from within NeoGenera.

## Phase 7: Hypertext documentation (Months 23–27)

**Goal:** a Document Examiner for NeoGenera.

- **Document format:** a structured, Lisp-native markup (s-expression based) with sections, cross-references, code examples, figures (text-mode), and index entries. Supports import from Markdown to lower the authoring cost.
- **Viewer:** history, bookmarks, "Candidates" (a Document Examiner-style list of related topics), table of contents, full-text search with ranking, and live-linked examples (run this snippet in the Listener).
- **Docstring integration:** every `defun`, `defclass`, `defmacro`, and variable links to its documentation automatically; the viewer and the Inspector cross-link each other.
- **Reference documentation:** author the NeoGenera Language Reference, User's Guide, Window System Manual, and Editor Manual. Treat doc coverage as a CI metric (percentage of exported symbols documented).
- Export: render the documentation to HTML and PDF from the same source so it is readable outside the system.
- Tutorial mode: guided interactive lessons running in the Listener.

**Exit criteria:** all exported symbols documented; a new user can follow the in-system tutorial from start to first program without leaving NeoGenera.

## Phase 8: Math and statistics (Months 27–31)

**Goal:** make NeoGenera a credible environment for quantitative work.

- **Numerics:** complete numeric tower (complex numbers, correct rounding, `rational`/`float` conversions, IEEE 754 edge cases), arbitrary-precision arithmetic with fast multiplication (Karatsuba/Toom or GMP-style kernels in C).
- **Linear algebra:** dense matrices and vectors with typed storage, BLAS-like kernels (C for hot loops), LU, QR, Cholesky, SVD, eigenvalues, linear solvers. Optionally bind to system BLAS/LAPACK.
- **Symbolic math (stretch):** simplification, differentiation, polynomial arithmetic, and a small computer-algebra layer in the Maxima/Macsyma tradition (a natural fit for a Lisp environment and a nod to Symbolics history).
- **Statistics:** descriptive statistics, probability distributions (pdf/cdf/quantile/sampling), hypothesis tests (t, chi-square, ANOVA, nonparametric), linear and generalized linear models, resampling (bootstrap, permutation), multiple-testing corrections.
- **Random numbers:** seedable, reproducible generators (e.g., PCG, xoshiro) with a stable cross-platform stream.
- **Data frames:** a small tabular data type with CSV/TSV I/O, grouping, joins, and summaries.
- **Plotting:** text-mode plots (braille or block characters) in windows for histograms, scatter, line plots, and heatmaps; export to SVG.
- Validation against reference results (R, SciPy, NIST StRD datasets) as automated tests.

**Exit criteria:** an end-to-end statistical analysis (load CSV, fit a model, test, plot) runs inside NeoGenera, and numeric test suites match reference output to documented tolerances.

## Phase 9: Bioinformatics (Months 31–35)

**Goal:** core bioinformatics tooling that exploits the live, interactive environment.

- **Sequence types:** compact DNA/RNA/protein sequence representations (2-bit and 4-bit packing), alphabets, translation tables, reverse complement, k-mer iteration.
- **File formats:** FASTA, FASTQ, GenBank, GFF3/GTF, BED, SAM/BAM (read-only first), VCF; streaming parsers with gzip support.
- **Alignment:** Needleman–Wunsch, Smith–Waterman, affine gaps, scoring matrices (BLOSUM, PAM, nucleotide), banded variants; optimized inner loops in C.
- **Search and indexing:** k-mer and suffix-array indexes, a simple BLAST-like seeded search, motif scanning with PWMs and regular expressions.
- **Phylogenetics (initial):** distance matrices, UPGMA/neighbor-joining, tree data structures and text-mode tree rendering.
- **Genomic intervals:** interval trees, overlap queries, coverage.
- **Presentations for biology:** sequences, alignments, features, and trees as mouse-sensitive objects in the Listener and Inspector (click a residue, a feature, or a clade for commands). This is the signature capability that distinguishes NeoGenera from a library collection.
- **Interoperability:** call out to external tools (BLAST+, samtools, minimap2) via a safe subprocess layer; import their output as first-class objects.
- Worked, documented example workflows (variant calling summary, simple RNA-seq QC, primer design).

**Exit criteria:** load a bacterial genome, annotate ORFs, align a protein against a set, and display a tree, all interactively, with documentation and tests against published benchmarks.

**Dependencies:** Phase 8 statistics (for enrichment tests and QC), Phase 5 presentations, Phase 7 documentation system.

## Phase 10: Hardening and 1.0 (Months 35–36+)

- Performance pass: profile real workloads, add native code generation for the hottest paths if the bytecode VM is a bottleneck.
- Memory and stability: long-running soak tests, GC tuning, crash-safe image saves.
- Packaging: reproducible builds, Homebrew/apt packages, container image, and a downloadable prebuilt world image.
- Security review of file, network, and subprocess facilities.
- Conformance report (ANSI CL pass rate) and a published compatibility document listing deliberate deviations.
- Community: website, contribution guide, issue templates, example gallery, and a short screencast tour.
- Release 1.0 announcement.

---

## Cross-cutting workstreams

| Workstream | Practice |
|---|---|
| Testing | C unit tests, Lisp regression suite, ANSI conformance, GC stress, fuzzing of the reader and file parsers |
| CI | Build matrix (macOS/Linux, debug/release, sanitizers), nightly full-world rebuild and fixed-point check |
| Performance | Benchmarks tracked per commit; regressions flagged above 10% |
| Documentation | Docstring coverage gate; design docs updated with each architectural decision (ADR log) |
| Versioning | Milestone releases `0.1` through `0.9` at the end of the phases noted below; `1.0` at Phase 10 |

## Milestone releases

| Version | Phase | Headline |
|---|---|---|
| 0.1 | 1 | C kernel REPL |
| 0.2 | 2 | Lisp-written core, boots to Listener |
| 0.3 | 3 | Self-hosting compiler |
| 0.4 | 4 | Conditions, CLOS, debugger |
| 0.5 | 5 | Text window system and Listener v2 |
| 0.6 | 6 | Editor and developer tools |
| 0.7 | 7 | Hypertext documentation |
| 0.8 | 8 | Math and statistics |
| 0.9 | 9 | Bioinformatics |
| 1.0 | 10 | Stable release |

## Top risks and mitigations

| Risk | Likelihood | Impact | Mitigation |
|---|---|---|---|
| GC or memory-corruption bugs consume months | High | High | Stress mode from day one, sanitizers, simple collector first, heap verifier |
| Scope creep across so many domains | High | High | Hold the phase order; only start science libraries after tools are stable; cut symbolic math and native codegen first |
| Solo-developer burnout or stalls | Medium | High | Always-runnable milestones, short phases, public progress notes, early community contributors for science libraries |
| Bootstrap deadlocks (compiler needs features it implements) | Medium | Medium | Keep a minimal stage-0 subset documented; fixed-point CI check |
| Performance too slow for interactive tools | Medium | Medium | Bytecode design reviewed early; benchmark suite; native backend as a Phase 10 option |
| Window system complexity (terminal quirks) | Medium | Medium | Constrain to a well-tested terminal baseline; abstract the backend so a GUI one can follow |
| Common Lisp conformance gaps | Medium | Medium | Track `ansi-test` continuously; document deviations |

## What to cut if time runs short

In order: native code generation, symbolic algebra, SAM/BAM and phylogenetics, GUI backend, PDF export, tutorial mode. The core (Phases 1–7) is what makes NeoGenera a Genera-like system; Phases 8–9 are what make it distinctive, so keep at least a minimal numeric/statistics layer and sequence/alignment support.

## Immediate next steps (first 30 days)

1. Create the repository skeleton, build system, and CI.
2. Write the object-representation and GC design notes.
3. Implement the allocator, fixnums, conses, and symbols in C with tests.
4. Implement a minimal reader and printer in C and a REPL that reads and echoes.
5. Evaluate `(+ 1 2)` end to end, then `(defun fib ...)`.
