# Window system, presentations and the Listener

Status: implemented in `src/term.c` and `lisp/ui/`.

## Layers

| Layer | File | Contents |
|---|---|---|
| Terminal | `src/term.c` | raw mode, alternate screen, SGR mouse (1006, any-motion 1003), SIGWINCH, UTF-8 output, key/mouse/resize event parser |
| Screen | `lisp/ui/00-screen.lisp` | double-buffered cell grid, clipping, themes, key normalization, diff flush |
| Window manager | `lisp/ui/10-wm.lisp` | windows (CLOS), overlap and tiled layouts, drag/resize, scrollbars, popups, who-line |
| Presentations | `lisp/ui/20-present.lisp` | output records, presentation types and commands, pane output stream |
| Commands | `lisp/ui/30-commands.lisp` | `DEFINE-COMMAND`, argument parsing and prompting, completion |
| Listener | `lisp/ui/40-listener.lisp` | the Listener window |
| Tools UI | `lisp/ui/60-tools-ui.lisp` | report windows, Inspector, browsers (see `tools.md`) |

`bin/neogenera --ui` runs `NG-UI`. Everything above the C layer also runs **headless**: events
are fed with `WM-FEED` (raw terminal bytes through the real parser) or `WM-RUN-EVENTS`, and the
result is read from the screen buffer (`SCREEN-FIND-TEXT`, `SCREEN-TEXT`). The test suite works
this way, plus `tests/ui/pty_test.py`, which runs the program in a pseudo-terminal and replays its
output through a small VT emulator.

## Input events

`(:KEY code mods)`, `(:MOUSE kind col row button mods)`, `(:RESIZE rows cols)`, `(:EOF)`.
`mods` is a bit mask (1 shift, 2 meta, 4 control). The parser handles CSI/SS3 sequences with
modifiers, SGR mouse reports, UTF-8, Meta as an ESC prefix, and a lone ESC (30 ms timeout).
`NORMALIZE-KEY` maps control codes to `(letter ctrl)` and names the special keys, so a key is
`(code mods)` where code is a character code or a keyword; `PARSE-KEY "C-x"` builds one.

## Screen and output

A cell is (code point, fg, bg, attributes). Colours are a palette index, -1 (default) or
`(+ #x1000000 rgb)`. `SCREEN-FLUSH` compares the back buffer with what the terminal is known to
show and emits only the differences (cursor moves, one SGR per style change); an unchanged screen
costs a cursor sequence. Output mode is `:truecolor`, `:256` or `:mono` (attributes only), chosen
from `COLORTERM`/`TERM`. Themes: `:classic` (black on white), `:green`, `:amber`, `:mono`; F8 cycles.
**Limitation:** every code point is assumed to be one column wide (no East Asian wide or combining
characters).

## Windows

`WINDOW` is a CLOS class; subclasses implement `WINDOW-DRAW-CONTENT`, `WINDOW-KEY`, `WINDOW-MOUSE`,
`WINDOW-HOVER-DOC`, `WINDOW-SCROLL-BY`, `WINDOW-CLOSE`, `WINDOW-RESIZED`. The manager provides:

* title bar (drag to move, `[x]` to close), bottom-right grip (drag to resize), scrollbar (drag the thumb),
* overlapping windows with a focus ring (F6) and tiling (F7 cycles horizontal, vertical, main+stack, grid),
* popup menus (arrow keys, first letter, mouse),
* the **mouse documentation line** and the Genera-style **who-line** (clock, user, package, run light, window),
* errors in event handlers are caught and shown in the documentation line; the UI keeps running.

## Presentations

Everything a pane prints is stored in an output buffer as styled segments; a segment may carry a
`PRES` (object + presentation type), which makes it mouse-sensitive. `PRESENT`,
`WITH-OUTPUT-AS-PRESENTATION` and `WITH-STYLE` write to the pane stream (a Gray stream). Presentation
types inherit through the CLOS class precedence list. `DEFINE-PRESENTATION-COMMAND (name type ...)`
registers an action; left click runs the default for the type, the middle button copies the object to
the input line (as `(result n)` when it has no readable form), the right button pops up a menu of all
applicable commands, most specific type first.

## Command processor and Listener

Input beginning with `:` is a command; abbreviations work word by word (`:sh pack`), and the
hyphenated form is accepted (`:who-calls foo`). Missing arguments are prompted for. Tab completes
commands, package names and symbols. The Listener has Emacs-style line editing, history, multi-line
input (an unbalanced form continues on the next line), `*` `**` `***`, and scrolling (wheel, PageUp).

**Errors in the window Listener are post-mortem:** the backtrace and condition are captured at the
point of the error and the evaluation is unwound before they are displayed (frames are clickable).
Restarts other than abort and in-place debugging are available only in the line-mode Listener
(`bin/neogenera`). Evaluation blocks the UI while it runs, and the UI Listener cannot read from
stdin.
