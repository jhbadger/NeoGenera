# Developer Tools

## The Inspector

`:inspect expr` (or `(inspect obj)`) opens an Inspector window. It shows the components of lists, vectors,
structures, CLOS instances, hash tables, functions, symbols, packages, classes and numbers. **Click a value** to
descend into it; Backspace goes back. Change a component with `:set-entry N expression`.

## Finding code

* `M-.` in the editor, or `:edit name`, opens the source of a definition. The locations of the whole system are
  recorded, so this works for built-in functions too.
* [definition-locations](fn:definition-locations) returns the recorded file and offset of a name.
* [arglist-string](fn:arglist-string) shows a function's lambda list.

## Cross reference

`:who-calls fn`, `:who-references var`, `:who-sets var` and `:callees fn` list the callers, readers, writers and
callees of a name; [who-calls](fn:who-calls) is the function. The answers come from the bytecode of every compiled
function and method, so calls the compiler open-codes (`car`, `cons`, `+`, comparisons) are not reported.
`:browse-class name` shows a class with its superclasses, slots and methods; `:browse-packages` lists packages;
`:apropos-window text` browses symbols by name.

## Tracing

[trace](fn:trace) wraps a function to print its arguments and values; [untrace](fn:untrace) removes the wrapper.
In the window system use `:trace fn` or right-click a function name.

## Advice

`advise` wraps extra code around a function without changing its definition, as in Genera:

```lisp
(advise solve :before log nil (format t "~&solve ~S~%" arglist))
(advise solve :after round nil (setq values (mapcar #'round values)))
(advise solve :around guard nil (handler-case :do-it (error () :failed)))
```

In `:before` advice `arglist` is the list of arguments, and changing it changes what the function gets. In
`:after` advice `values` is the list of values the function returned, and changing it changes what the caller gets.
In `:around` advice `:do-it` calls the function (and the inner advice) and returns its values. The third argument
names the advice, so the same class and name replace it, and the fourth places it among the others (`nil`: last).
`(unadvise solve)` removes the advice of `solve`, `(unadvise solve :after)` removes one class, and `(unadvise)`
removes all advice. `describe-advice` and `*advised-functions*` show what is advised. Advice and `trace` can be
combined in any order.

## Recording a session

`(dribble "session.txt")` records what you type and what is printed to the file, appending to it; `(dribble)` stops.
In the window Listener it records that Listener's window.

## The stepper

[step](fn:step) evaluates a form and pauses at every function call. At the prompt, `s` steps into a function
whose source is known, `n` runs it as a unit, `c` continues, `q` quits. Functions defined interactively keep
their source; built-in functions are always called as a unit. `:step-trace expr` prints every call and result
without stopping.

## The profiler

[with-profiling](fn:with-profiling) samples the running compiled code and [profile-report](fn:profile-report)
prints, per function, how many samples had it on top of the stack (*self*) and anywhere on the stack
(*cumulative*). `:profile expr` does both. Code that neither calls functions nor loops is never sampled, so the
profile favours call-heavy code.

## The debugger

When an error is not handled the debugger shows the condition, the restarts and a backtrace with the argument
values. The commands are the same in both Listeners: `N` (invoke restart N), `:bt`, `:frame N`, `:abort`,
`:continue`; the line-mode Listener also has `:edit`. Forms typed in the debugger are evaluated in the
environment of the error. In the window Listener restarts and frames are presentations: click a restart to
invoke it, and right-click a frame for *Edit definition* (see [The Window System](topic:window-system)).
`(break)` enters the debugger with a *Return from BREAK* restart.

A frame can also be returned from or run again. In the line-mode Listener select it with `:frame N`, then
`:return FORM` makes it return FORM's values to its caller and `:retry` runs it again from the start, with its
arguments as they are now. In the window Listener the commands are `:Return From Frame N FORM` and `:Retry Frame N`,
and a frame's right-click menu has *Retry this frame*. Either way the frames above it are unwound: their
`unwind-protect` cleanups run and their special bindings are undone. From Lisp, `debugger-return-from-frame` and
`debugger-restart-frame` take a frame of the backtrace. Compiled code only: the stage-0 evaluator's frames (code
being loaded from source) are not in the backtrace, and a tail call replaces its caller's frame.

## Systems and patches

A program of several files is described with `defsystem`, usually in a file of its own:

```lisp
(defsystem my-app (:version "1.0" :description "My application" :depends-on (my-utils))
  "package" "model" "ui")
```

The files (`package.lisp` ...) are in the directory of the file that defines the system, or in `:pathname`.
`(load-system 'my-app)` loads the systems it depends on, then its files in order. Later it loads only the files
that have changed since they were loaded, so editing a file and loading the system again is quick. With
`:compile t`, or `compile-system`, changed files are compiled first and the compiled files loaded; `:reload t`
loads everything. `find-system` also finds a system that has not been loaded yet, from `name.system` in a
directory on `*system-search-path*` (the current directory and `~/.neogenera/systems/`).

**Patches** change a system that is already loaded without reloading it, as in Genera. `(make-patch 'my-app
"Fix the totals")` creates the next numbered file, `patches/my-app-1.lisp`, in the system's directory. Write the
corrected definitions into it, then `(load-patches 'my-app)` loads every patch not loaded yet. `load-system` loads
new patches too, so every world that loads the system catches up. The patch level is part of the version:
`(system-version-string 'my-app)` is `"MY-APP 1.0.3"` after three patches. `describe-system` lists the files, which
of them have changed, and the patches.

From the Listener: `:load system my-app`, `:compile system`, `:load patches`, `:make patch` (which opens the new
patch in the editor), `:describe system` and `:show systems`.

## Saving the world

`(save-world "mywork.img")`, or `:save world mywork.img`, writes everything in the running world to an image file:
every function, variable, loaded system and definition. Start it later with `bin/neogenera -i mywork.img` (add
`--ui` for the window system); the Listener says it is a saved world. A world cannot be saved while background
processes are running, since threads cannot be written to a file; open files are not saved either.

## Tests

`(deftest name ...)` defines a test and [run-tests](fn:run-tests) runs them. In the window system `:run-tests`
prints each failure as a presentation: click it to open the editor at the test. A jump from a frame or a failure
goes to the *definition* of the function or test, not the exact line.
