# The Window System

`bin/neogenera --ui` starts a text window system in the terminal: overlapping or tiled windows, a mouse,
colour themes and a Genera-style status line.

## Keys and mouse

| Key | Action |
|---|---|
| F6 or Meta-o | next window |
| F7 or Meta-t | re-tile: horizontal, vertical, main and stack, grid |
| F8 | next colour theme: classic, green, amber, mono |
| F9, then a letter | **select** an activity, like Genera's SELECT key: `L` Listener, `E` Editor, `P` Peek, `F` Files, `D` Documentation, `I` Inspector, `S` Settings |
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
arguments are prompted for: type the answer, or click any object on the screen that fits (the mouse
documentation line says *L: use this for ...* over the ones that do). Tab completes command names, package
names and symbols. `:help` lists them all.

## Lists and trees in output

`format-textual-list` prints a list as prose, and each element is a presentation:

```lisp
(format-textual-list '(1 2 3) #'princ :conjunction "and")      ; 1, 2, and 3
```

`format-graph-from-root` draws a tree or a graph. Give it the root, a function that prints a node, and a function
that returns a node's children:

```lisp
(format-graph-from-root 'number #'princ
                        (lambda (c) (mapcar #'class-name (class-direct-subclasses (find-class c)))))
```

It draws the tree sideways, with lines like a directory tree; `:orientation :vertical` draws it top-down. A node
reached a second time is drawn with `...` and without its children, so graphs with shared nodes or cycles are drawn
once. `:depth-limit` stops the drawing at that depth. Each node is a presentation, so it can be clicked.

## The file browser

`:files`, or F9 then `F`, opens a window on a directory. It lists directories first, with sizes and dates, and shows a
preview of the selected file or directory on the right.

| Key | Action |
|---|---|
| Up/Down, Page Up/Down, Home/End | move; a letter jumps to the next name starting with it |
| Return or Right | open a directory, or edit a file; a second click does the same |
| Backspace or Left | the parent directory |
| `v` `e` | view the file, edit it |
| `d` `r` `+` | delete (asks first), rename, make a directory |
| `c` | copy the path to the Listener's input |
| `s` `.` `p` `g` | sort by name, size or date; show hidden files; the preview; reread the directory |
| `q` | close |

When something asks for a file name (an `accept` of `pathname`, or a command argument), clicking an entry answers it.

## Asking for input

Programs ask for typed input with `accept`:

```lisp
(accept 'integer :prompt "How many")
(accept '(member :red :green :blue) :prompt "Colour" :default :red)
```

In the window Listener the prompt replaces the Listener's prompt: `How many (an integer):`. Type the answer
and press Return, or click a presentation that fits: an integer printed earlier, for example. Text of the wrong
type is reported, and you are asked again. Return on an empty line takes the default, Tab completes the names
of members, packages and symbols, and C-g on an empty line gives up the evaluation. Outside the window system
`accept` reads a line.

The types are classes and type names such as `integer`, `string`, `symbol`, `keyword`, `character`,
`package`, `number` and `function`, as well as presentation types like `frame`. There are also `boolean`
(yes or no), `expression` (any form, not evaluated) and `pathname`, and the compound types `(integer 1 10)`,
`(member a b c)`, `(or integer string)`, `(sequence integer)` (separated by commas) and `(null-or-type type)`.
`define-command` arguments can have any of these types.

A presentation that is not of the type can still answer if a **translator** turns it into one. A backtrace frame
can answer for a symbol (its function's name), a condition for a string (its message), and a process for a
string (its name). Define more with `define-presentation-translator`:

```lisp
(define-presentation-translator package-to-name (package string :documentation "its name") (p)
  (package-name p))
```

`accepting-values` turns the `accept`s in its body into one dialog:

```lisp
(accepting-values (*standard-output* :label "Plot")
  (list (accept 'integer :prompt "Points" :default 100)
        (accept '(member :line :scatter) :prompt "Kind" :default :line)
        (accept 'boolean :prompt "Grid" :default t)))
```

Up and Down (or Tab) choose a field, and Return changes it. For a choice or a yes/no field, Return steps
through the values, and a click picks one; for other fields, type the new value and press Return. A click on a
presentation anywhere fills the selected field. The body runs again after each change, so later fields can
depend on earlier ones. End (or `[Done]`) returns the body's values, and Escape (or `[Abort]`) gives up. Outside
the window system the `accept`s simply ask one after the other.

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
`mono` theme. Only the cells that changed are redrawn. Chinese, Japanese and Korean characters and emoji take
two columns, and combining accents join the letter before them.

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

## Demos

`:demo` pops up a menu of graphics demos, in the spirit of the Lisp Machine's display hacks; choose one and it runs in its
own window. `:demo circles` (or any demo's name, abbreviated) starts one directly.

| Demo | What it shows |
|---|---|
| Colour Circles | shaded circles, drawn once; only the colour map changes, so the colours flow from circle to circle. `S` turns the spheres into rings that ripple outwards |
| Munching Squares | the PDP-1 hack: for each x, flip the square at y = x xor t |
| Lissajous | Lissajous figures turning as their phase drifts, changing ratio now and then |
| Bouncing Lines | a line whose ends bounce off the walls, leaving a fading trail |
| Life | Conway's Game of Life on a torus; `R` starts a new random world, and a world that has settled starts again |
| Mandelbrot | the Mandelbrot set painted a few rows at a time; when finished it rests, then dives into the next view (the seahorse valley, a spiral, a miniature copy of the set). `N` goes to the next view now |
| Boids | Reynolds' flocking: each bird steers by separation, alignment and cohesion with the birds nearby, and flocks form and drift across a torus. `R` scatters the flock |

In every demo, Space (or a click) pauses and resumes, `N` starts again, `+` and `-` change the speed, and `Q` closes the
window. The demos use terminal graphics in kitty and Ghostty and half blocks elsewhere.

Colour Circles is colour-map animation, as colour framebuffers did it: the picture is a grid of palette indices, and each
frame only computes a new palette of 256 colours and maps the indices through it. The primitives are `%idx-fill`,
`%idx-disc` (a disc of indices, shaded as a sphere or numbered in rings) and `%idx-map`.

A new demo is a subclass of `demo-window` with methods on `demo-setup` (start on a fresh image, `demo-image`) and
`demo-step` (draw the next frame), and optionally `demo-key`, `demo-keys-help` and `demo-base-interval`; `define-demo` adds it
to the menu. Any window can be animated the same way: `window-frame-interval` returns how many milliseconds apart its
frames are, and `window-animate` draws one. After drawing on an image that is already shown, call `image-changed`.
