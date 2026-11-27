# NeoGenera

A Lisp environment in the spirit of the Symbolics Genera workstations: a small C kernel, and a system written in
Lisp itself, with a text-mode window system, an Emacs-style editor, integrated hypertext documentation and libraries for
mathematics, statistics and bioinformatics. Version **1.0.0**. A scripted tour recorded from the real program is in
`docs/tour.cast` (`asciinema play docs/tour.cast`).

## What it is

* **A Lisp you can read.** A C kernel (tagged objects, bignums with Karatsuba multiplication, exact ratios, a copying garbage collector, a bytecode VM,
  checksummed heap images, a terminal backend) and a Common Lisp subset written in Lisp: CLOS, conditions and restarts, Gray streams, `loop`, `format`,
  `defstruct`. The bytecode compiler is written in Lisp and **compiles itself to a byte-identical fixed point**. See
  [`docs/COMPATIBILITY.md`](docs/COMPATIBILITY.md) for the differences from ANSI Common Lisp.
* **A Genera-style environment.** A text window system with mouse-sensitive output (presentations: click any printed object for commands that apply to it),
  an Emacs-style editor with an undo tree, Inspector, cross-reference, trace, stepper, profiler, class and package browsers, and an interactive debugger.
  Several Lisp **processes** run at once (preemptive, with locks and `process-wait`), and **Peek** shows them live.
* **Documentation inside the system.** Manuals, docstrings and cross-references in one viewer (`:doc`, `:search-doc`), runnable examples, HTML and PDF
  export, and an interactive tutorial (`:tutorial`).
* **Quantitative work.** Linear algebra, probability distributions, hypothesis tests, linear and generalized linear models, data frames, text and SVG plots,
  seedable random numbers and a symbolic algebra layer, validated against R and NIST reference values.
* **The Pascal Tool Kit.** As on Genera, Pascal (ISO and Pascal/VS) compiles into Lisp: bignum integers, checked
  uninitialised variables and subscripts, units, calls between Pascal and Lisp, and a syntax-directed Pascal mode in the
  editor with templates, completion and incremental compilation. See `:doc pascal-manual` and `examples/11-pascal.lisp`.
* **The FORTRAN Tool Kit.** ANSI FORTRAN 77 with the Symbolics extensions compiles into Lisp: arbitrary-size integers, detection of
  uninitialised values, stronger type checks, `DO WHILE`/`ENDDO`/`INCLUDE`, bit operations and binary patterns, packages, full
  formatted and list-directed I/O, and a fixed-format FORTRAN mode in the editor. See `:doc fortran-manual` and `examples/12-fortran.lisp`.
* **The Bytecode Assembler.** Write a function as raw instructions for the bytecode machine with `defasm`: labels, a full instruction set,
  and a verifier that checks stack depth, slots, jumps, dynamic bindings and the types the unchecked instructions assume before the
  routine exists. `asm-listing` turns any compiled function back into assembly source that assembles to the same code, and ASM mode in
  the editor lays routines out like an assembler, assembles them from the buffer and points errors at the instruction. See
  `:doc bytecode-manual` and `examples/15-bytecode-assembly.lisp`.
* **Bioinformatics.** Sequences, FASTA/FASTQ/GenBank/GFF3/GTF/BED/VCF/SAM/BAM, pairwise alignment, suffix arrays, seeded search, motifs, phylogenetics, genomic
  intervals and a safe layer for BLAST, minimap2 and samtools, with clickable sequence, alignment and tree viewers.
* **Hardened.** Every parser is fuzzed under ASan/UBSan; memory and stack exhaustion are recoverable errors; heap images are checksummed and saved atomically;
  builds are reproducible. See [`docs/SECURITY.md`](docs/SECURITY.md).

## Quick start

```sh
make world             # build the kernel and the self-hosted system image (needs a C compiler and make)
bin/neogenera          # line-mode Listener with the interactive debugger
bin/neogenera --ui     # the window system: Listener, editor, Inspector, documentation viewer (needs a terminal)
make test              # C, Lisp and real-terminal (pty) tests
make examples          # run the example gallery
```

In the window system: type Lisp forms or `:commands` (Tab completes, `:help`), click results (left = default action, middle = copy to input, right = menu),
`:edit mapcar` opens the editor on a function's source, `:inspect expr`, `:who-calls fn`, `:doc topic`, `:search-doc words`, `:tutorial`, `:settings`.
Keys: F6 next window, F7 tile, F8 theme, F10 quit.

```
NeoGenera 1.0.0 -- type :help for help, :quit to leave.
CL-USER> (defun fact (n) (if (<= n 1) 1 (* n (fact (- n 1)))))
FACT
CL-USER> (fact 20)
2432902008176640000
CL-USER> (translate "ATGGCCATTGTAATGGGCCGCTGAAAGGGTGCCCGATAG")
#<PROTEIN 13aa MAIVMGR*KGAR*>
CL-USER> (princ (t-test '(0.7 -1.6 -0.2 -1.2 -0.1 3.4 3.7 0.8 0.0 2.0) :y '(1.9 0.8 1.1 0.1 -0.1 4.4 5.5 1.6 4.6 3.4)))
Welch Two Sample t-test
statistic = -1.86081, df = 17.77647, p-value = 0.07939
```

More in [`examples/`](examples/) (fifteen annotated programs, including a Mandelbrot renderer and a boids flocking simulation; `make examples` runs them and writes a gallery).

## Install and packages

```sh
make install PREFIX=/usr/local      # binary, image, sources, HTML manuals, man page
make dist                           # build/dist/neogenera-1.0.0-<os>-<arch>.tar.gz + SHA256SUMS (a prebuilt world)
make deb                            # build/deb/neogenera_1.0.0_<arch>.deb
docker build -t neogenera . && docker run -it --rm neogenera --ui
brew install --build-from-source packaging/homebrew/neogenera.rb
```

The binary and the world image travel together (the image is a heap dump for that binary). `make repro-check` builds twice in different directories and requires
byte-identical images and binaries.

## Layout

| Path | Contents |
|---|---|
| `src/` | the C kernel (memory and GC, reader, printer, evaluator, VM, bignums, primitives, terminal, bioinformatics kernels) |
| `lisp/core/` | the Lisp library: macros, places, sequences, structures, hash tables, `format`, `loop`, CLOS, conditions, streams, debugger, Listener, test framework |
| `lisp/compiler/` | the compiler, `compile-file`, fasl loader, build helpers |
| `lisp/ui/`, `lisp/editor/`, `lisp/tools/` | window system, presentations, settings, editor, developer tools |
| `lisp/docs/` | the documentation system: viewer, search, export, tutorial, API docstrings |
| `lisp/math/`, `lisp/bio/` | numerics, linear algebra, statistics, models, data frames, plots, symbolic math; sequences, formats, alignment, phylogeny, viewers |
| `docs/manual/`, `docs/design/` | the manuals (compiled into the image) and the design documents |
| `tests/` | C unit tests, Lisp suites, conformance cases, fuzz and soak tests, a pty test with a VT emulator, data fixtures |
| `bench/`, `examples/`, `packaging/` | benchmarks, the example gallery, install and release tooling |

## Quality

| Command | What it checks |
|---|---|
| `make test` | 74 C checks, the Lisp suites (about 3,700 checks), real-terminal tests |
| `make fuzz` | the whole suite, including the fuzzers, under AddressSanitizer and UBSan |
| `make conformance` | about 600 cases written against the Common Lisp specification; writes `docs/CONFORMANCE.md` |
| `make soak` | a mixed workload that watches the live heap for leaks (`NG_SOAK_SECONDS=600` for a long run) |
| `make bench` | the performance workloads in `bench/bench.lisp` |
| `make repro-check` | reproducible builds |
| `make release-check` | tests plus reproducibility |

## How it builds itself

`make world` produces `stage0` (the C evaluator loads the system), `stage1` (the interpreted compiler compiles the system), then `stage2` (the *compiled*
compiler recompiles the system) and checks that stage 1 and stage 2 are byte-identical. Details in [`docs/design/bootstrap.md`](docs/design/bootstrap.md).
Design notes for every part are in [`docs/design/`](docs/design/); the project history is in [`ROADMAP.md`](ROADMAP.md) and [`CHANGELOG.md`](CHANGELOG.md).

## Contributing and license

See [`CONTRIBUTING.md`](CONTRIBUTING.md) and [`CODE_OF_CONDUCT.md`](CODE_OF_CONDUCT.md). Security issues: [`SECURITY.md`](SECURITY.md).
NeoGenera is released under the MIT license ([`LICENSE`](LICENSE)).
