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

## Tests

`(deftest name ...)` defines a test and [run-tests](fn:run-tests) runs them. In the window system `:run-tests`
prints each failure as a presentation: click it to open the editor at the test. A jump from a frame or a failure
goes to the *definition* of the function or test, not the exact line.
