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

### The undo tree

Undo is a tree. If you undo and then edit, the old future is kept as another branch. `M-x undo-tree-visualize`
shows the tree and `M-x undo-tree-select-branch` picks which branch redo follows.

### Rectangles

`C-x r k` kills the rectangle between mark and point, `C-x r y` yanks it, `C-x r t` replaces it with a string,
`C-x r o` opens blank space, and `C-x r c` clears it.

## Searching

`C-s` searches forward and `C-r` backward incrementally. Type the text; `C-s` or `C-r` again find the next
match; Return stops at the match; `C-g` returns to where you started. A failing search wraps when you repeat it.
`M-x replace-string` replaces every occurrence after point.

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

Files ending in `.lisp` are edited in Lisp mode.

| Keys | Action |
|---|---|
| `TAB`, `RET` | indent the line; Return indents the new line |
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

Typing a space after `(name` shows the lambda list of `name` in the echo area, and the parenthesis matching
the one at point is highlighted. See [Developer Tools](topic:tools-manual) for how source locations are found.

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

The editor works on bytes: UTF-8 characters move as one character but nothing else is Unicode-aware. There is
no query-replace, regexp search or syntax colouring beyond comment lines.
