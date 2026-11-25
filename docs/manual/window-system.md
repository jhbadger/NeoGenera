# The Window System

`bin/neogenera --ui` starts a text window system in the terminal: overlapping or tiled windows, a mouse,
colour themes and a Genera-style status line.

## Keys and mouse

| Key | Action |
|---|---|
| F6 or Meta-o | next window |
| F7 or Meta-t | re-tile: horizontal, vertical, main and stack, grid |
| F8 | next colour theme: classic, green, amber, mono |
| F10 | quit |

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

Errors are shown in the window together with a clickable backtrace; the evaluation is abandoned first
(the interactive debugger with restarts is in the line-mode Listener).

## Colours and terminals

The output uses truecolor when the terminal supports it, 256 colours otherwise, and plain attributes in the
`mono` theme. Only the cells that changed are redrawn. Every character is assumed to be one column wide.

See also the [Editor Manual](topic:editor-manual) and the [Developer Tools](topic:tools-manual).
