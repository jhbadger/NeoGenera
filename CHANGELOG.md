# Changelog

## Unreleased

* **Window system: Select key and a quit confirmation.** F9 then a letter selects an activity, like Genera's SELECT key (`L` Listener,
  `E` Editor, `P` Peek, `D` Documentation, `I` Inspector, `S` Settings; `define-select-activity` adds more). F10 now asks before quitting,
  naming modified buffers and background processes; F10 again or `y` quits.

* **Processes and Peek.** Preemptive Lisp processes on threads that share one interpreter lock: `process-run-function`, `process-wait`
  (and `-with-timeout`), `process-sleep`, `process-join`, `process-interrupt`, `process-kill`, `process-arrest`, locks with `with-lock-held`,
  `without-interrupts`. Special bindings are per process; background output is kept per process; errors are recorded and announced.
  `sleep`, terminal input, `run-program` and the line Listener's prompt let other processes run. `:peek` opens Peek (processes, memory,
  windows, refreshed every second, with clickable processes); `:show-processes`, `:kill-process`; the who-line counts processes.

* **Editor: Zmacs definition commands.** Edit, List and Compile Changed Definitions (since compiled, saved or read in); Edit and List Callers;
  a possibilities list stepped with `` C-x ` `` / `M-g n` / `C-.`; Quick Arglist (`C-c C-a`), Documentation (`C-c C-d`) and Macro Expand
  Expression (`C-c RET`), also bound to Zmacs's `c-sh-A`, `c-sh-D` and `c-sh-M`.

## 1.0.0

First stable release. All ten roadmap phases are implemented.

* **Kernel and core (phases 0-4)**: tagged-object C kernel with a copying garbage collector, bytecode VM, heap images; Common Lisp subset written in Lisp with
  bignums, ratios, CLOS, conditions and restarts, Gray streams, `loop`, `format`; a compiler that compiles itself to a byte-identical fixed point; interactive debugger.
* **Environment (phases 5-7)**: text window system with mouse-sensitive output (presentations), Emacs-style editor with an undo tree, Inspector, cross-reference, trace,
  stepper, profiler; hypertext documentation with search, bookmarks, runnable examples, HTML and PDF export and an interactive tutorial.
* **Mathematics and statistics (phase 8)**: Karatsuba multiplication, complex numbers, seedable random numbers, linear algebra (LU, QR, Cholesky, eigen, SVD), eleven
  distribution families, hypothesis tests, linear and generalized linear models, data frames, text and SVG plots, a symbolic algebra layer.
* **Bioinformatics (phase 9)**: sequences, file formats (FASTA, FASTQ, GenBank, GFF3/GTF, BED, VCF, SAM, BAM), alignment with a C kernel, suffix arrays,
  seeded search, motifs, phylogenetics, intervals, a safe subprocess layer, and clickable viewers.
* **Hardening (phase 10)**: crash-safe image saves with checksums; recoverable out-of-memory and stack-exhaustion errors; a subprocess I/O deadlock fixed; fuzzing of every parser;
  soak, benchmark and conformance suites; reproducible builds; install, `dist`, Debian, Homebrew and Docker packaging; a security review; an example gallery and a recorded tour.
* Fixed while hardening: `loop` with several `collect` clauses, `format` `~@[`, `~:;`, `~R`, `~?` and `~E`, `remove-duplicates` order, `parse-integer :junk-allowed`,
  `/=` with more than two arguments, `(setf getf)`, and missing `signum`, `lcm`, `rationalize`, `logcount`, `logbitp`, `sublis`, `mapcon`, `type-of`.

## 0.7.0 and earlier

Development releases, one per roadmap phase: see the git history.
