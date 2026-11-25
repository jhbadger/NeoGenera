# NeoGenera User's Guide

NeoGenera is a Lisp environment in the spirit of the Symbolics Genera workstations: one live,
introspectable system with a Listener, an editor, an Inspector, a debugger and integrated documentation.
This guide gets you started; the other manuals cover each part in depth.

## Starting

Build the system once with `make world`, then start it:

```
bin/neogenera          # line-mode Listener with the interactive debugger
bin/neogenera --ui     # the window system (needs a terminal)
```

In the window system type Lisp forms at the prompt and press Return. Results are *presentations*:
click one to act on it. The [Window System Manual](topic:window-system) describes the mouse and window keys.

## The Listener

Type a form and press Return; an unbalanced form continues on the next line. The value is printed after `=>`.
`*`, `**` and `***` hold the last three values. Press Tab to complete symbols and commands, Up and Down to
recall earlier input.

Lines that start with a colon are *commands*. They can be abbreviated word by word:

| Command | What it does |
|---|---|
| `:help` | list the commands, with the documentation commands (`:tutorial`, `:doc`, `:search-doc`) first |
| `:doc topic` | show a manual topic, or the documentation of a symbol |
| `:search-doc words` | search the documentation |
| `:edit name` | open a file or the source of a function in the editor |
| `:inspect expr` | open an Inspector on a value |
| `:who-calls fn` | list the callers of a function |
| `:tutorial` | start the interactive lessons |

## Numbers and data

Integers never overflow: `(expt 2 100)` is exact. Division of integers is exact too, so `(/ 1 3)` is the
ratio `1/3`; use [float](fn:float) for a float. The usual list functions are there:
[mapcar](fn:mapcar), [remove-if](fn:remove-if), [reduce](fn:reduce), [sort](fn:sort), and the
[loop](fn:loop) macro.

```
(loop for i from 1 to 5 collect (* i i))
(mapcar #'1+ '(1 2 3))
```

## Defining things

[defun](fn:defun), [defvar](fn:defvar), [defstruct](fn:defstruct) and [defclass](fn:defclass) define functions,
variables, structures and CLOS classes. Put a docstring right after the lambda list and it becomes part of
the documentation system:

```
(defun square (x)
  "Return X multiplied by itself."
  (* x x))
```

Then `(documentation 'square 'function)` returns the string and `:doc square` shows it with the signature and
the place where it is defined.

## When something goes wrong

In the line-mode Listener an unhandled error enters the debugger: it shows the error, the restarts and a
backtrace with the argument values. Type a restart number to invoke it, `:bt` for more frames, `:frame 2` to
select one, `:edit` to jump to the source of the selected function, `:abort` to return. In the window
Listener the error is shown with a clickable backtrace instead. See [Developer Tools](topic:tools-manual).

## Where next

* [The Editor](topic:editor-manual) for editing and Lisp mode.
* [Developer Tools](topic:tools-manual) for the Inspector, cross-reference, trace, stepper and profiler.
* [The Language](topic:language-reference) for what NeoGenera implements of Common Lisp.
* [The Documentation System](topic:documentation-system) for writing and exporting documentation.
