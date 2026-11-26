# The editor

Status: implemented in `lisp/editor/`.

An Emacs/Zmacs-style editor that runs inside a window (`EDITOR-WINDOW`) of the window system.

## Buffers (`00-buffer.lisp`)

A buffer is a vector of byte strings; positions are `(line . column)` with byte columns. Character
motion (`CHAR-END`, `CHAR-START`) steps over a UTF-8 character and the combining marks after it.
Display columns (`DISPLAY-COL`, `BYTE-COL-FOR-DISPLAY`) expand tabs and use `CHAR-DISPLAY-WIDTH`: wide
characters take two columns, combining marks none, control characters two (`^X`). Word motion and case
commands use the Unicode functions of `lisp/core/63-unicode.lisp` (`UNICODE-WORD-CHAR-P`, `UTF8-UPCASE`, ...).
`%RAW-INSERT` and `%RAW-DELETE` call `%NOTE-CHANGE`, which drops cached per-line state (the syntax states)
after the changed line.

All edits go through `BUF-INSERT` and `BUF-DELETE`, which record the change in the **undo tree**:
`BUF-UNDO` / `BUF-REDO` walk the tree, an edit made after an undo starts a new branch, and the older
branch stays reachable with `UNDO-TREE-SELECT` (`M-x undo-tree-select-branch`, `undo-tree-visualize`).
Edits are grouped into one undo step per command (self-inserted characters coalesce in runs of 20).

Also here: the kill ring (consecutive kills append), the mark and region, rectangles
(`kill`/`yank`/`string`/`open`/`clear`), forward and backward incremental search primitives, and the
Lisp structure functions `SEXP-END`, `SEXP-START-BEFORE`, `SEXP-CONTEXT`, `LIST-END` (strings,
comments, `#\x`, `|sym|` and `#| |#` are understood) and `LISP-INDENT-FOR-LINE`.

## Regular expressions (`05-regex.lisp`)

`REGEX-COMPILE` parses a pattern (Perl/POSIX extended syntax: classes, `\d \w \s \b`, groups, `(?:)`,
alternation, greedy and lazy repetition, back-references) into a tree; `REGEX-MATCH`, `REGEX-FIND` and
`REGEX-FIND-BACKWARD` run a backtracking matcher written in continuation-passing style. A greedy repetition
of a single-character node collects its positions in a loop and tries them longest first, so `.*` on a long
line does not recurse deeply. Patterns and subjects are UTF-8, so `.` and classes take a whole character;
`:CASE-FOLD` compares with `UNICODE-FOLD`. `REGEX-EXPAND` fills in `\&` and `\1`..`\9`;
`REGEX-REPLACE-ALL` and `STRING-MATCH-P` are conveniences. (The motif functions of the bioinformatics module,
`regex-search` and `regex-find-all`, are a separate matcher with longest-match semantics.)

## Indentation

The indenter finds the innermost open parenthesis and the elements already on previous lines.
A head symbol with an entry in `*INDENT-SPECS*` indents its first N arguments by 4 and the body by 2
(`defun`, `let`, `when`, `dolist`, ...); other calls align under the first argument; data lists
align under their first element. It does not look at the user's own `declare`d indentation.

## The window (`10-editor.lisp`)

Keymaps are nested alists keyed by normalized keys; a mode keymap is consulted before the global one.
Commands are defined with `DEFCMD` and are callable with `M-x` (with completion). Prefix arguments
(`C-u`, `C-u 5`, `M-5`), a minibuffer with completion, and incremental search (`C-s`/`C-r`, failing
search, wrap-around, `C-g` returns to the start) are built in.

**Views.** The window's layout is a tree whose leaves are `VIEW`s and whose nodes are `(:below a b)` or
`(:right a b)`. The selected view's buffer point is the live point; the other views keep their own line and
column, restored by `ED-SELECT-VIEW`. `ED-BUFFER`, `ED-TOP` and `ED-HSCROLL` are functions on the selected
view, so commands do not know about views. `DRAW-LAYOUT` divides the window evenly, and each view draws its
own mode line; the echo area is shared.

**Searching.** Every search compiles a regex (`SEARCH-REGEX`; a literal search quotes its string), folding
case when the string has no upper-case letter, and `BUF-FIND` looks for it line by line, forward or
backward. Incremental search, query replace (`ED-QUERY`, a modal state like isearch, with Emacs's keys and
case preservation), replace-regexp, count-matches and occur are built on it. The display underlines the
other matches of the search in progress.

**Keyboard macros.** `WINDOW-KEY` records each key while a macro is being defined (but not while one is
running); `END-KBD-MACRO` drops the keys of its own command (`ED-COMMAND-KEYS`). `EXECUTE-KBD-MACRO` feeds
the keys back through `WINDOW-KEY` and stops when a command fails: an error, an undefined key or a failing
search sets `*ED-COMMAND-FAILED*`.

**Completion and hints.** `M-TAB` and `TAB` complete symbols with `SYMBOL-COMPLETIONS` (package prefixes and
keywords included); `M-/` (dabbrev) collects matching words from the buffer, nearest first, then from the
other buffers. After every command in Lisp mode `ED-UPDATE-HINT` finds the innermost call around point,
reading only from the start of the top-level form, and `ARGLIST-HINT` prints its lambda list, recording the
byte range of the parameter for the argument at point. The echo area shows the hint when there is no message.

**Display.** `%LINE-CELLS` turns a line into screen cells and the byte offset of each: tabs expanded,
`^X` for control characters, a wide character followed by `+WIDE-CONT+`, combining marks merged into a
cluster cell. **Syntax colouring** (`LISP-LINE-SYNTAX`) scans one line from a starting state (`NIL`,
`:STRING` or `(:BLOCK . depth)`); `%SYNTAX-STATE-AT` caches the state at the start of each line and is
recomputed from the first changed line. Classes are `:COMMENT :STRING :CONSTANT :KEYWORD :BUILTIN :DEFNAME`,
drawn with the theme's `:SYN-...` colours when it has them and otherwise with its dim, value, link and
prompt colours. Diff buffers colour added and removed lines. The display also highlights the region, the
current match and the parenthesis matching the one at point.

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

Matches across lines, auto-save, an interactive undo-tree browser, abbrevs, saving keyboard macros to a
file, Unicode character names and case mappings that change length.
