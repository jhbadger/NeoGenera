# The Editor

The editor is an Emacs-style editor that runs in a window. Open it with `:edit file`, `:edit function-name`
or `:editor`. Press `C-x C-c` to close it. Keys are written Emacs-style: `C-` is Control, `M-` is Meta (Alt or
Escape).

## Moving

| Keys | Action |
|---|---|
| `C-f` `C-b` `C-n` `C-p` | character and line motion (also the arrow keys) |
| `C-a` `C-e` | start and end of line |
| `M-f` `M-b` | word motion |
| `M-<` `M->` | start and end of buffer |
| `C-v` `M-v` | page down and up |
| `M-g g` | go to a line |

## Editing

| Keys | Action |
|---|---|
| `C-k` | kill to end of line |
| `M-d` | kill word |
| `C-w` `M-w` | kill and copy the region |
| `C-y` `M-y` | yank, then replace with an earlier kill |
| `C-SPC` | set the mark; the region is between mark and point |
| `C-/` or `C-x u` | undo |
| `M-_` | redo |
| `C-u 5 x` | repeat a command with a prefix argument |
| `M-u` `M-l` `M-c` | upper-case, lower-case or capitalize a word; `C-x C-u` and `C-x C-l` do the region |
| `M-/` | complete the word before point from words in the buffers (nearest first; again for the next one) |

### The undo tree

Undo is a tree. If you undo and then edit, the old future is kept as another branch. `M-x undo-tree-visualize`
shows the tree and `M-x undo-tree-select-branch` picks which branch redo follows.

### Rectangles

`C-x r k` kills the rectangle between mark and point, `C-x r y` yanks it, `C-x r t` replaces it with a string,
`C-x r o` opens blank space, and `C-x r c` clears it.

## Searching

`C-s` searches forward and `C-r` backward incrementally. Type the text; `C-s` or `C-r` again find the next
match; Return stops at the match (the mark ring remembers where you started); `C-g` returns to where you
started. A failing search wraps when you repeat it. While searching, `C-w` adds the rest of the word at point
and `M-r` switches between plain text and a regular expression. The other matches on the screen are
underlined.

A search ignores case unless what you type has an upper-case letter. This works for accented, Greek and
Cyrillic letters too.

`C-M-s` and `C-M-r` search for a regular expression. The syntax is the usual one:

| Pattern | Matches |
|---|---|
| `.` | any character |
| `[abc]` `[^a-z]` `[[:alpha:]]` | a character in (or not in) a set; classes `alpha digit alnum space upper lower punct word blank xdigit` |
| `\d` `\w` `\s` (`\D` `\W` `\S`) | a digit, a word character, a space (or not) |
| `^` `$` `\b` `\B` | start and end of the line, a word boundary (or not) |
| `(...)` `(?:...)` | a group, numbered or not; a vertical bar between two patterns matches either |
| `*` `+` `?` `{n}` `{n,}` `{n,m}` | repetition; add `?` for the shortest match (`*?`) |
| `\1` ... `\9` | the text a group matched |

A match lies within one line.

## Replacing

`M-%` replaces text after point, asking about each match: `y` or space replaces it, `n` or Delete skips it,
`!` replaces all the rest, `.` replaces this one and stops, `q` or Return stops. Any other key stops and does
what it normally does. When the search ignores case and the replacement is in lower case, a match in capitals
gets a capitalized replacement, and a capitalized match a capitalized one: replacing `cat` with `dog` turns
`Cat CAT` into `Dog DOG`.

`M-x query-replace-regexp` does the same with a regular expression. In the replacement, `\&` is the whole match
and `\1` to `\9` are the groups. `M-x replace-regexp` and `M-x replace-string` replace every match without
asking. `M-x count-matches` counts the matches after point, and `M-x occur` lists the lines that match: step
through them with `` C-x ` `` (see *Working with definitions*). Each replacement is one undo step.

## Views

An editor window can show several views, of one buffer or of different ones. Each view has its own point.

| Keys | Action |
|---|---|
| `C-x 2` `C-x 3` | split the view: one above the other, or side by side |
| `C-x o` | go to the next view (a click also selects a view) |
| `C-x 0` `C-x 1` | close this view; close the others |
| `C-M-v` | scroll the other view |
| `C-x 4 b` `C-x 4 f` | switch buffer or find a file in the other view |

## Keyboard macros

`C-x (` (or F3) starts recording keys, and `C-x )` (or F4) stops; the mode line shows `Def` meanwhile.
`C-x e` (or F4) plays the macro back, and each further `e` plays it again. A prefix argument repeats it:
`C-u 5 C-x e` runs it five times, and `C-u 0 C-x e` runs it until a command fails (a search finds nothing, for
example). A failing command always stops the macro. `M-x name-last-kbd-macro` makes the macro a command you
can run with `M-x`.

## Files and buffers

| Keys | Action |
|---|---|
| `C-x C-f` | find or create a file (Tab completes) |
| `C-x C-s` | save; the first save in a session keeps a numbered backup `file.~1~` (the last three are kept) |
| `C-x C-w` | save as |
| `C-x b` `C-x k` `C-x C-b` | switch, kill and list buffers |
| `C-x d` | Dired, the directory editor: `n p` move, `RET` open, `d` flag, `u` unflag, `x` delete flagged, `R` rename, `+` new directory |
| `M-x` | run any command by name, with completion |

## Version control

The editor drives `git`: `C-x v s` status, `C-x v =` diff of the current file, `C-x v l` log, and `C-x v c`
commits the current file with a message.

## Lisp mode

Files ending in `.lisp` are edited in Lisp mode. Comments, strings, character literals, keywords, `t` and `nil`,
special forms and common macros, and the names after `defun`, `defmacro` and the other definers are coloured.
Diff buffers (`C-x v =`) colour added and removed lines.

| Keys | Action |
|---|---|
| `TAB`, `RET` | indent the line; Return indents the new line. On a line that is already indented, `TAB` after a symbol completes it |
| `M-TAB` (`ESC TAB`) | complete the symbol before point: a unique completion is inserted, otherwise the common part, and the echo area lists the choices. `pkg:sym` and `:keyword` complete too |
| `C-M-f` `C-M-b` | forward and backward over an expression |
| `C-M-u` `C-M-d` | up out of and down into a list |
| `C-M-k` | kill the expression after point |
| `C-M-a` `C-M-e` | start and end of the definition |
| `C-M-q` | re-indent the definition |
| `C-x C-e` | evaluate the expression before point and show the value |
| `C-M-x` or `C-c C-c` | evaluate the definition at point |
| `M-.` | jump to the definition of the symbol at point; `M-,` returns |
| `C-c C-a` | show the lambda list of the function being called around point (Zmacs `c-sh-A`) |
| `C-c C-d` | show the first line of the documentation of the symbol near point; `C-u C-c C-d` shows all of it (`c-sh-D`, `m-sh-D`) |
| `C-c RET` | macroexpand the expression after point once; `C-c M-RET` expands until the head is not a macro (`c-sh-M`, `m-sh-M`) |

While point is inside a call, the echo area shows the lambda list of the function, with the parameter for the
argument at point highlighted (for `&key` arguments, the one named by the keyword before it). The parenthesis
matching the one at point is highlighted. See [Developer Tools](topic:tools-manual) for how source locations are
found.

## Pascal mode

Files ending in `.pas`, `.pascal` or `.p` are edited in Pascal mode, a syntax-directed mode for the Pascal Tool Kit:
it moves over and deletes statements, routines, expressions and tokens, finds syntax errors, inserts templates of
constructs, completes reserved words, puts them in capitals in Electric Pascal mode, indents, and compiles the routine
at point with `C-c C-c`. See [The Pascal Tool Kit](topic:pascal-manual).

## FORTRAN mode

Files ending in `.f`, `.for`, `.ftn`, `.f77` or `.fortran` are edited in FORTRAN mode, a fixed-format mode for the FORTRAN
Tool Kit: `TAB` pads the label and continuation fields and indents statements by their block nesting, `C-j` opens an
indented statement line, `C-M-j` opens a continuation card, Electric FORTRAN mode wraps long statements onto continuation
cards, `C-M-a`/`C-M-e`/`C-M-f`/`C-M-b`/`C-M-h` move over and mark routines and statements, and `C-c C-c` compiles the
routine at point. See [The FORTRAN Tool Kit](topic:fortran-manual).

## ASM mode

Files ending in `.ngasm` are edited in ASM mode, an assembler-style mode for `defasm` bytecode routines: `TAB` puts labels,
instructions and trailing comments in their columns, `RET` opens a correctly indented line, `M-<tab>` completes an instruction
name, `C-c ?` describes the instruction at point, `C-c C-c` assembles the routine at point, and an error puts the cursor on the
offending instruction (`C-c C-w`, then `C-.`). `C-c C-a` shows the assembly listing of the compiled routine and `C-c C-x` calls
it. See [The Bytecode Assembler](topic:bytecode-manual).

Other language modes can be added the same way: a mode is a symbol whose property list gives its `mode-keymap`,
`mode-name`, `line-syntax` function (for colouring), `match-offsets` function and `before-self-insert` and
`after-self-insert` hooks, and `*auto-mode-alist*` maps file name suffixes to modes.

## Unicode

Text is UTF-8. Point moves over whole characters, and an accent written as a separate combining mark moves
with its letter. Chinese, Japanese and Korean characters and emoji take two columns, and combining marks none.
Word motion treats punctuation such as dashes and quotation marks as separating words. Case commands and
case-insensitive search know accented, Greek, Cyrillic and Armenian letters, one letter for one (so `ß`
stays `ß`).

`C-x =` shows the character at point: its code points, its size in bytes, its width and the position.
`C-x 8 RET` inserts a character by its code point in hexadecimal, such as `3bb` or `U+1F600`.

## Working with definitions

The editor knows which top-level forms are definitions and remembers their text when the file was read,
when it was last saved and when each definition was last compiled.

| Command | Action |
|---|---|
| `M-x edit-changed-definitions` | visit each definition changed since it was last compiled, in every Lisp buffer |
| `M-x compile-changed-definitions` | compile just those definitions |
| `M-x list-changed-definitions` | list them in a buffer |
| `M-x edit-callers` | visit each function that calls (or uses) a name, with point on the use |
| `M-x list-callers` | list the callers in a buffer |

Give `C-u` to the changed-definition commands for changes since the file was last saved, `C-u C-u` for
changes since it was read. Each command also has an `-of-buffer` form for the current buffer only.

These commands leave a list of places. `` C-x ` `` (or `M-g n`, or `C-.` where the terminal can send it)
goes to the next place, `M-g p` to the previous one, and `C-x C-.` shows the whole list: `RET` visits the
entry on the line, `q` leaves.

## Limits

Regular expressions match within a line. Several views share one editor window, which is a window of the window
system. Character names (for `C-x 8 RET`) and case mappings that change the length of a word (`ß` to `SS`) are not
known. How wide an emoji sequence joined with zero-width joiners looks depends on the terminal.
