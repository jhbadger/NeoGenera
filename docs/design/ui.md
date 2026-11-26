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
**Wide and combining characters.** `SCREEN-PUT` gives a character of `CHAR-DISPLAY-WIDTH` 2 two cells, the
second holding `+WIDE-CONT+`. Overwriting either half blanks the other, and a wide character that does not
fit before the clip edge becomes a space. `SCREEN-FLUSH` never sends the second half; it advances its idea
of the cursor by two. A combining mark (width 0) is merged with the character before it into a *cluster*
cell, a code from `#x180000` that names an interned string (`SCREEN-CLUSTER`). Image cells start at
`#x200000`. Pane layout (`UTF8-LENGTH`, `UTF8-PREFIX-END`) measures display columns, so Listener output
wraps correctly.

**Images** (`05-graphics.lisp`, `src/prims_gfx.c`). An image is an RGB byte string drawn by C primitives
(coverage anti-aliased lines and discs, rectangles). In an output buffer an image row is a segment of
private-use characters U+F0000+column with the style `(:image key row)`, where the key names a *placement*
(an image shown on a given number of cells); wrapping, scrolling and hit-testing treat it like text. When it is
drawn, kitty and Ghostty (`*graphics*` is `:kitty`, set by `DETECT-GRAPHICS` from the terminal's answer to a
graphics query, XTVERSION and the environment) get cells whose code is above Unicode; `SCREEN-FLUSH` turns each
into U+10EEEE with the terminal's image number as a 256-colour foreground and the row and column as combining
marks (the kitty protocol's Unicode placeholders), sending the image first with a virtual placement
(`a=T,U=1`) if the terminal does not have it. 255 image numbers are recycled least-recently-used, never taking
one that is on screen. Other terminals get half-block cells with the average colours of each half cell.
Detection only runs where the environment points at kitty or Ghostty (or `NG_GRAPHICS=on`), so other terminals
never see the queries; replies that arrive late are dropped by the input parser.

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

**Typed input** (`45-accept.lisp`). `ACCEPT` reads an object of a presentation type. Types are CLOS classes
and type names, `DEFINE-PRESENTATION-TYPE` types, and the compound forms `(member ...)`, `(integer lo hi)`,
`(or ...)`, `(sequence type)`, `(null-or-type type)`, plus `boolean`, `expression` and `pathname`.
`ACCEPT-FROM-STRING` parses text (with `*read-eval*` off), `PRESENTATION-TYPEP` checks objects,
`DESCRIBE-PRESENTATION-TYPE` words the prompt and `ACCEPT-COMPLETIONS` completes. In the window Listener an
`ACCEPT` pushes an `ACCEPT-STATE` on the Listener (which takes over its prompt and Return) and an *input
context* on `*INPUT-CONTEXTS*`, then serves the window system in a nested loop (`%NESTED-SERVE`, shared with
the debugger) until it is answered. A click on a presentation in any Listener-like window first asks
`PRESENTATION-ACCEPTABLE-P` whether it answers the innermost context: its object is of the type, or a
translator (`DEFINE-PRESENTATION-TRANSLATOR`, with an optional tester) turns it into one. A command waiting for
an argument is a context too (`*PENDING-LISTENER*`). The debugger hides waiting `ACCEPT`s until it is left.
`ACCEPTING-VALUES` runs its body with `*ACCEPTING-VALUES*` bound, so each `ACCEPT` becomes a field, keyed by
prompt and occurrence, that returns its current value. An `AVV-WINDOW` edits the fields; after each change
the body runs again, with its output discarded, so the set of fields can change. Done runs it once more for
real and returns its values.

## Command processor and Listener

Input beginning with `:` is a command; abbreviations work word by word (`:sh pack`), and the
hyphenated form is accepted (`:who-calls foo`). Missing arguments are prompted for. Tab completes
commands, package names and symbols. The Listener has Emacs-style line editing, history, multi-line
input (an unbalanced form continues on the next line), `*` `**` `***`, and scrolling (wheel, PageUp).

**The window debugger runs a nested event loop.** The Listener's handler (`%window-debugger`, in
`42-debugger.lisp`) runs where the error was signalled, so the stack, restarts and special bindings stay
intact. It prints the error, the restarts and the frames as presentations. Then it serves the whole window
system (`wm-serve`) until a non-local exit ends the level: a restart, quitting, or, headless, the end of the
scripted input (`*wm-script*`, which the nested loop reads from too). Each Listener evaluation sets up an
ABORT restart (*Return to the Listener* or *Return to debug level N*), recorded in `*listener-aborts*` so a
window lists only its own restarts. The nested loop puts back the stream and package bindings from outside
the evaluation, so other windows are not affected by them. Nesting is a stack: choosing a restart in an outer
level, even in another window, unwinds the levels above it. A window whose level is unwound this way says so.
The backtrace stops at `%listener-call`, so the Listener's own frames are not shown. Evaluation blocks the UI
while it runs, and the UI Listener cannot read from stdin.
