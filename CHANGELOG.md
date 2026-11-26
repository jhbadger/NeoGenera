# Changelog

## Unreleased

* **Demos.** `:demo` pops up a menu of animated graphics demos, each in its own window: Colour Circles (shaded circles
  animated by colour-map cycling, or rippling rings), Munching Squares, Lissajous, Bouncing Lines and Life. Space pauses,
  `N` restarts, `+`/`-` change the speed. Windows can now be animated (`window-frame-interval`, `window-animate`), and
  palette images (`%idx-disc`, `%idx-map`) and `image-changed` redraw an image in place. `define-demo` adds more.
* The window system draws each frame as a synchronized update (DEC mode 2026), so terminals that support it (kitty,
  Ghostty, iTerm2, WezTerm, ...) never show a half-drawn screen: animations no longer flicker.
* **The Pascal Tool Kit**, after the Symbolics one: ISO Pascal and Pascal/VS compiled into Lisp (and then to bytecode), with
  Lisp's integers (no overflow), sets as unbounded bit masks, detection of uninitialised variables, range, subscript and
  pointer checks, and run-time errors that name the routine and line and enter the debugger. Units
  (`unit_specification`, `unit_implementation`, `using_units`), `def-program` systems, `%include`, conformant arrays,
  `lispobject` and the `lisp` routine directive for calling Lisp, and `ROUTINE<-PROGRAM` functions for calling Pascal.
  `pascal-execute` (with `:input`, `:output`, `:init-to-zero`, `:save-environment`, ...) runs a program, reading its input
  in the Listener; `:Execute Pascal`, `:Compile Pascal` (or `:Compile Pascal File`), `:Show Pascal Programs`. Manual: `:doc pascal-manual`.
* **Pascal mode** in the editor (`.pas`, `.pascal`, `.p`): colouring, indentation, Electric Pascal mode, motion and
  deletion by statement, routine, expression and token, syntax-error finding, templates with template items and their
  refinement, completion of reserved words, closing constructs, compiling the routine at point or the buffer, compiler
  warnings (`C-.`), compiling and executing the program at point, and the attribute list (dialect and package).
* **Copying code from the documentation.** Every code block in the viewer has *Copy* (the kill ring, so `C-y` pastes it in
  the editor, and the terminal's clipboard via OSC 52); a left click copies code in languages other than Lisp. Fenced code
  blocks keep their language.
* Editor modes are extensible: a mode's keymap, name, colouring, construct matching and self-insert hooks are properties of
  its symbol, and `*auto-mode-alist*` maps file types to modes. `defsystem` systems may contain non-Lisp files
  (`*system-file-handlers*`).
* **Systems and patches.** `defsystem` describes a program's files and the systems it needs. `load-system` loads changed files
  only, and with `:compile` compiles them. Numbered patch files (`make-patch`, `load-patches`) update a loaded system, and their
  number is part of the version. Commands: `:load system`, `:compile system`, `:load patches`, `:make patch`, `:describe system`,
  `:show systems`.
* **Saving the world.** `save-world` (and `:save world`) writes the running world to an image for `bin/neogenera -i`.
* **Advice and dribble.** `advise` adds `:before`, `:after` and `:around` code to existing functions (`unadvise`,
  `describe-advice`). `dribble` records a session to a file.
* **`format-textual-list` and `format-graph-from-root`**, for lists in prose and for trees and graphs drawn with lines, sideways or
  top-down, with clickable nodes.
* **The file browser** (`:files`, F9 then F): directory listings with a preview, opening, viewing, renaming, deleting and making
  directories. Clicking an entry answers a request for a file name.
* **Generational garbage collection.** New objects are allocated in a nursery and survivors are promoted, with a write barrier and
  remembered sets, so most collections take a millisecond or two. `--gc-verify` (or `NG_GC_VERIFY`) checks the barrier.
* **Complex numbers** in the numeric tower (`#C(1 2)`, `complex`, `realpart`, `phase`, ..., and complex results from `sqrt`,
  `log`, `expt`, `asin`). The math library's own complex type is gone; `c+` and the other old names remain.
* **Multi-dimensional arrays:** `make-array` with a list of dimensions, `aref` with several subscripts, `#2A(...)`,
  `array-dimensions`, `row-major-aref`, `adjust-array`.
* **Readtables:** `*readtable*`, `copy-readtable`, `set-macro-character`, `make-dispatch-macro-character`,
  `set-dispatch-macro-character`, `read-delimited-list`, `readtable-case`, `set-syntax-from-char`, `with-standard-io-syntax`.
  Fixed: a comment or skipped `#+` form just before `)` was a read error.
* **Pathnames:** pathname objects and their components, `merge-pathnames`, `make-pathname`, wildcards (`*` and `**`) in
  `directory`, `pathname-match-p` and `translate-pathname`, logical pathnames, `truename`, `ensure-directories-exist`, `#P"..."`.
  The file functions accept pathnames.
* **`format` and the pretty printer:** `~F ~E ~G ~$ ~R ~T` with all their parameters, `~<...~>` justification, `~<...~:>` logical
  blocks, `~_ ~I ~W ~/name/`; `pprint-logical-block`, `pprint-newline`, `pprint-indent`, `pprint-tab`, `pprint-fill`,
  `pprint-linear`, `pprint-tabular`. `~E` now prints the exponent's sign (`1.0e+3`), as standard Common Lisp does.

* **Typed input.** `accept` reads an object of a presentation type: the answer can be typed (checked, completed with Tab, with a
  default), or given by clicking any matching presentation on the screen. The types include `integer` and other classes,
  `boolean`, `expression`, `pathname`, `(member ...)`, `(integer lo hi)`, `(or ...)`, `(sequence type)` and `(null-or-type type)`.
  `define-presentation-translator` lets presentations of one type answer for another; frames give their function's name,
  conditions their message, processes their name. `accepting-values` shows a body's `accept`s as one dialog, run again after
  each change. Command arguments can have any presentation type and can be answered by clicking (`:describe`, then click an
  object). Outside the window system `accept` reads a line.

* **Editor: search and replace.** Regular-expression search (`C-M-s`, `C-M-r`, or `M-r` while searching). Searches ignore case
  unless the text has a capital. Other matches are underlined, and `C-w` in isearch adds the word at point. `M-%` query replace,
  with `y n ! . q` and case kept in replacements; `query-replace-regexp`, `replace-regexp`, `count-matches` and `occur`. A
  regular expression engine (`regex-compile`, `regex-find`, `regex-expand`, `regex-replace-all`, `string-match-p`): classes,
  groups, alternation, lazy repetition and back-references, on UTF-8, optionally ignoring case.
* **Editor: views.** `C-x 2` and `C-x 3` split the editor window into views, each with its own point; `C-x o`, `C-x 0`, `C-x 1`,
  `C-M-v`, `C-x 4 b`, `C-x 4 f`; clicking selects a view.
* **Editor: keyboard macros.** `C-x (` and `C-x )` (or F3 and F4) record, and `C-x e` plays back (then `e` again); `C-u N` repeats,
  `C-u 0` repeats until a command fails. `name-last-kbd-macro`.
* **Editor: completion and hints.** `M-TAB` completes symbols, including `pkg:sym` and keywords; `TAB` completes on a line that is
  already indented; `M-/` expands words from the buffers. In Lisp mode the echo area shows the lambda list of the call around
  point, with the current argument highlighted.
* **Editor: syntax colouring** of comments, strings, characters, keywords, special forms and macros, and defined names in Lisp
  mode, and of diffs.
* **Unicode.** Wide (East Asian, emoji) characters take two columns and combining marks none, on the screen, in the editor and in
  Listener output. Character motion keeps combining marks with their letter. Word motion knows Unicode punctuation. Case commands
  and case-insensitive search handle accented, Greek, Cyrillic and Armenian letters. `C-x =` describes the character at point,
  `C-x 8 RET` inserts one by code point, and `C-x C-u` / `C-x C-l` change the case of the region. New functions: `char-display-width`,
  `string-display-width`, `unicode-upcase`, `unicode-downcase`, `unicode-fold`, `unicode-word-char-p`, `utf8-upcase`,
  `utf8-downcase`, `utf8-capitalize`, `utf8-codes`, `utf8-from-codes`.
* Fixed: `C-t` swapped bytes of a UTF-8 character; control characters were counted as one column in the editor.

* **The debugger in the window Listener.** An error no longer unwinds. The Listener shows the error, the restarts and the
  backtrace, all clickable, and the prompt becomes `debug N>`. Type or click a restart (with a value if it takes one: `1 42`), or use
  `:abort`, `:continue`, `:restarts`, `:frame N` and `:bt`. Forms typed there run in the environment of the error, and errors nest.
  Other windows keep working meanwhile. Added `break`. `restart-case` restarts record their lambda list (`restart-arguments`).
  `(abort)` in the window Listener now returns to it instead of leaving the window system. The backtrace no longer shows the
  Listener's own frames.

* **Graphics in kitty and Ghostty.** The window system detects terminals that show images (kitty graphics protocol) and draws images
  there at full resolution through Unicode placeholder cells, so they scroll, clip and overlap like text; elsewhere images use colour
  half blocks. New images API (`make-image`, `image-line`, `image-disc`, `image-fill-rect`, `draw-image`) and pixel graphs
  (`graph-scatter`, `graph-line`, `graph-function`, `graph-histogram`, `graph-heatmap`) that fall back to text plots. `graphics` setting,
  `:graphics` command, `NG_GRAPHICS`.
* Fixed: methods specialized on `T` did not apply to instances of structures defined in the system image (the second load of the
  self-hosting build replaced the built-in classes).

* **Local time.** The who-line clock showed UTC; it now shows local time. Added `decode-universal-time`, `encode-universal-time` and
  `get-decoded-time`, using the operating system's time zone and daylight saving rules (or a given `time-zone`).

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
