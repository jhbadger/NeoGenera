# The Window System

`bin/neogenera --ui` starts a text window system in the terminal: overlapping or tiled windows, a mouse,
colour themes and a Genera-style status line.

## Keys and mouse

| Key | Action |
|---|---|
| F6 or Meta-o | next window |
| F7 or Meta-t | re-tile: horizontal, vertical, main and stack, grid |
| F8 | next colour theme: classic, green, amber, mono |
| F9, then a letter | **select** an activity, like Genera's SELECT key: `L` Listener, `E` Editor, `P` Peek, `D` Documentation, `I` Inspector, `S` Settings |
| F10 | quit: asks first (and mentions modified buffers and background processes); F10 again or `y` quits, any other key cancels |

F9 followed by a letter brings the most recent window of that kind to the front; if one is already selected
it goes to the next one, and if there is none it opens one (the Inspector only opens with `:inspect`).
`define-select-activity` registers more.

A terminal does not report a modifier key pressed on its own, and sends the same codes for both Alt keys,
so Alt cannot serve as the Select key: Alt+letter arrives as Meta+letter, which the editor uses.

Drag a title bar to move a window, the bottom-right corner to resize it, and the scrollbar thumb to scroll.
Click `[x]` to close. The mouse wheel scrolls the window under the pointer.

## Presentations

Everything printed in a window remembers what it was. Move the pointer over a result and the line below the
windows (the *mouse documentation line*) tells you what a click will do.

* **Left click** runs the default action for the object's type, for example *Describe*.
* **Middle click** copies the object to the Listener input line (as `(result 3)` if it has no readable form).
* **Right click** opens a menu of every action that applies, most specific first: edit definition, who calls,
  inspect, trace, and so on.

Frames of a backtrace, test failures, manual references and examples in the documentation are presentations too.

## Commands

A line that starts with a colon is a command. Abbreviate each word (`:sh pack` is `:show packages`). Missing
arguments are prompted for. Tab completes command names, package names and symbols. `:help` lists them all.

## The Listener window

Emacs-style editing keys work on the input line: Control-a/e/b/f/k/u/w/y, Meta-b/f, Up/Down or Meta-p/n for
history. An unbalanced form continues on the next line. Page Up and Page Down scroll the output.

## The debugger

An error in the Listener stops where it happened and enters the debugger in the same window. The window
shows the error, the restarts and the top of the backtrace, and the prompt becomes `CL-USER debug 1>`.

* Type a restart number and Return to invoke it, or click the restart. A restart that takes a value (such as
  `use-value`) wants a form after the number: `1 42`.
* `:abort` leaves this level (restart *Return to the Listener*, or *Return to debug level N*); `:continue`
  continues, for example from `break` or `cerror`; `:restarts` lists the restarts again.
* Forms typed now are evaluated where the error happened: special variables have the values they had there.
  An error among them enters level 2, and so on.
* `:bt` shows the whole backtrace and `:frame N` one frame with its arguments; clicking a frame does the same.
  Either way the frame's arguments are put in `*frame-args*`. Right-click a frame for *Edit definition*.

While a Listener is in the debugger the other windows keep working, and the who-line spinner stops. Quitting
with F10 leaves the debugger too. `(break)` enters the debugger on purpose. With `NG_NODEBUG` set, errors are
only reported, and the evaluation is abandoned.

## Settings

`:settings` opens the Settings window. Click a value to use it at once: the colour theme, how windows are
tiled, and whether your command history is kept. A `*` marks a change that is not saved yet. Click
`[Save settings]` (or press `s`) to keep your choices for next time; `r` reverts to the saved file and `d`
restores the defaults.

The same things are available from the Listener: `:set` lists the settings, `:set theme green` changes one
(a unique prefix of the name is enough), and `:save-settings` writes the file.

Settings are stored in `~/.neogenera/settings.lisp`, or in `$NEOGENERA_HOME/settings.lisp` if that variable is
set. The file is data, one `(:name value)` entry per line; it is never evaluated, and an entry that is
unreadable or invalid is skipped. It is only written when you save. Once it exists, your command history and
documentation bookmarks are refreshed in it each time you quit; turn `save-history` off to stop keeping the
history. Command history can contain anything you typed, so check who can read your home directory.

## Colours and terminals

The output uses truecolor when the terminal supports it, 256 colours otherwise, and plain attributes in the
`mono` theme. Only the cells that changed are redrawn. Every character is assumed to be one column wide.

## Images and graphics

When the window system starts in **kitty** or **Ghostty**, it asks the terminal whether it accepts the kitty graphics
protocol and how big its cells are in pixels. If it does, images and graphs (see `graph-function` and the other graph
functions in the [Mathematics manual](topic:math-manual)) are drawn by the terminal at full resolution. They are part
of the window's text, so they scroll with it, are clipped by the window and are hidden by windows in front of them.
In other terminals an image is drawn with half-block characters, two coloured pixels per cell, and a graph prints as
a text plot. `:graphics` says which is in use.

The `graphics` setting chooses: `auto` (the default: kitty and Ghostty), `on` (any terminal that answers the kitty
protocol's query) or `off`. The environment variable `NG_GRAPHICS=on` or `off` overrides it, for example when the
terminal is not recognized over ssh. Inside tmux graphics stay off.

Images can be made and drawn on directly: `(make-image 200 100)`, then `image-line`, `image-disc`,
`image-fill-rect` and `image-polyline` with colours from `(rgb r g b)`; `image-pixel` reads a pixel back. Return an
image to the Listener, or call `(draw-image img)`, to see it. In the line-mode Listener in kitty or Ghostty, `draw-image`
shows it at the cursor.

See also the [Editor Manual](topic:editor-manual) and the [Developer Tools](topic:tools-manual).
