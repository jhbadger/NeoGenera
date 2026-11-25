# Changelog

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
