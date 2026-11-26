# The editor

Status: implemented in `lisp/editor/`.

An Emacs/Zmacs-style editor that runs inside a window (`EDITOR-WINDOW`) of the window system.

## Buffers (`00-buffer.lisp`)

A buffer is a vector of byte strings; positions are `(line . column)` with byte columns. Character
motion treats a UTF-8 sequence as one character and display columns expand tabs, but the editor does
not otherwise understand Unicode (no case mapping of non-ASCII text, no width handling).

All edits go through `BUF-INSERT` and `BUF-DELETE`, which record the change in the **undo tree**:
`BUF-UNDO` / `BUF-REDO` walk the tree, an edit made after an undo starts a new branch, and the older
branch stays reachable with `UNDO-TREE-SELECT` (`M-x undo-tree-select-branch`, `undo-tree-visualize`).
Edits are grouped into one undo step per command (self-inserted characters coalesce in runs of 20).

Also here: the kill ring (consecutive kills append), the mark and region, rectangles
(`kill`/`yank`/`string`/`open`/`clear`), forward and backward incremental search primitives, and the
Lisp structure functions `SEXP-END`, `SEXP-START-BEFORE`, `SEXP-CONTEXT`, `LIST-END` (strings,
comments, `#\x`, `|sym|` and `#| |#` are understood) and `LISP-INDENT-FOR-LINE`.

## Indentation

The indenter finds the innermost open parenthesis and the elements already on previous lines.
A head symbol with an entry in `*INDENT-SPECS*` indents its first N arguments by 4 and the body by 2
(`defun`, `let`, `when`, `dolist`, ...); other calls align under the first argument; data lists
align under their first element. It does not look at the user's own `declare`d indentation.

## The window (`10-editor.lisp`)

Keymaps are nested alists keyed by normalized keys; a mode keymap is consulted before the global one.
Commands are defined with `DEFCMD` and are callable with `M-x` (with completion). Prefix arguments
(`C-u`, `C-u 5`, `M-5`), a minibuffer with completion, and incremental search (`C-s`/`C-r`, failing
search, wrap-around, `C-g` returns to the start) are built in. The display highlights the region,
the current search match and the parenthesis matching the one at point; lines starting with `;` are
dimmed in Lisp mode (there is no other syntax colouring).

Default bindings include `C-f C-b C-n C-p C-a C-e M-f M-b M-< M-> C-v M-v`, `C-k M-d C-w M-w C-y M-y`,
`C-SPC C-x C-x C-x h`, `C-/ C-_ C-x u` (undo) and `M-_` (redo), `C-x r k/y/t/o/c` (rectangles),
`C-x C-f C-s C-w C-b b k C-c`, `C-x d` (Dired), `M-x`. Lisp mode adds `TAB` and `RET` (indent),
`C-M-f/b/u/d/k/a/e/q`, `C-x C-e` (eval last sexp), `C-M-x` (eval defun, recording its source
location), `C-c C-c`, `C-c C-r`, `C-c C-b`, `M-.` (find definition), `M-,` (back). Typing a space
after `(name` shows the lambda list of `name` in the echo area.

## Files (`20-files.lisp`)

* **Version-aware save:** the first save of an existing file in a session writes a numbered backup
  `file.~N~`; at most `*BACKUP-KEEP*` (3) are kept.
* **Dired:** `n p RET f d u x R + g ^ q` (flag/unflag/execute deletions, rename, create directory).
* **Version control:** `C-x v s` (status), `C-x v =` (diff), `C-x v l` (log), `C-x v c` (add and
  commit the file) via the `git` command line (no shell: `RUN-PROGRAM` uses `execvp`). Other
  version-control systems are not supported.
* `OPEN-EDITOR`, `EDIT-DEFINITION`: open the editor from the Listener (`:edit file-or-function`),
  from the debugger (`:edit N`) and from presentations (frames, test failures, symbols).

## Definitions (`30-definitions.lisp`)

Zmacs-style commands that work on whole definitions.

* **Sections.** `TEXT-SECTIONS` divides Lisp text into definitions: forms starting in column 0 whose
  head is a definer known to `%DEFINITION-INFO` (`defun`, `defmacro`, `defmethod`, `defvar`, ...).
  Only the head and the name are read, with `*READ-EVAL*` off, so nothing is evaluated; `in-package`
  forms set the package for the names after them and `#| |#` blocks are skipped. A section's key is
  `(kind name n)`, where `n` numbers definitions with the same kind and name (methods).
* **Baselines.** Each Lisp buffer keeps the text of every section at three moments: `:read` (visit or
  revert), `:saved` (save) and `:compiled` (`C-M-x`, `C-c C-r`, `C-c C-b`, Compile Changed). A section
  has changed when its text differs from the baseline, so an edit that is undone is not a change.
  A buffer that never had a baseline (a new buffer) counts every definition as changed.
* **Changed definitions.** `M-x edit-changed-definitions` (all Lisp buffers) and
  `edit-changed-definitions-of-buffer`, `list-changed-definitions[-of-buffer]`,
  `compile-changed-definitions[-of-buffer]`. No argument: changed since last compiled; `C-u`: since
  last saved; `C-u C-u`: since the file was read.
* **Possibilities.** Commands that find several places leave a possibilities list; `C-.` (terminals
  cannot send it, so also `` C-x ` `` and `M-g n`) visits the next, `M-g p` the previous, and
  `C-x C-.` shows the list in a `*Possibilities*` buffer (`RET` visits, `n` `p` move, `q` quits).
  Entries for changed definitions find their section again by key, so they survive later edits.
* **Edit Callers.** `M-x edit-callers` / `list-callers` (default: the symbol at point) rebuild the
  cross reference and list every definition whose code calls, references, sets or binds the name,
  then visit each one with point on the use. Open-coded primitives are not seen (see Tools).
* **At point.** Quick Arglist (`c-sh-A`, `C-c C-a`) shows the lambda list of the innermost enclosing
  call that names a function. Documentation (`c-sh-D`, `C-c C-d`) shows the first line of the
  docstring; with `C-u`, or `m-sh-D` / `C-c M-d`, everything in a `*Documentation*` buffer. Macro Expand
  Expression (`c-sh-M`, `C-c RET`) expands the form after point (or around it) once; `m-sh-M` /
  `C-c M-RET` expands it until its head is not a macro (subforms are not expanded). Short results go to
  the echo area, long ones to a `*Macroexpansion*` buffer. The `c-sh-` and `m-sh-` keys are bound but
  only reach the editor from terminals that report shifted control keys.

## Not implemented

Query-replace, regexp search, multiple windows or frames within one editor, syntax colouring beyond
comment lines, auto-save, an interactive undo-tree browser, abbrevs, keyboard macros.
