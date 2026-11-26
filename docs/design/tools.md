# Developer tools

Status: implemented in `lisp/tools/` and `lisp/ui/60-tools-ui.lisp`.

## Source locations

Every definition read by `LOAD` or compiled by `COMPILE-FILE` records `(kind file offset)` on the
symbol (`DEFINITION-LOCATIONS`). `COMPILE-FILE` writes an extra fasl entry per definition form (kinds:
function, macro, variable, structure, class, generic, method, condition, test, command, type), so the
locations of the whole system live in the image: `M-.` on `mapcar` opens `lisp/core/00-base.lisp` at the
form. The offset comes from the reader (`%LAST-READ-START`, the stream position where the datum began).
Paths are relative to the build directory; `*SOURCE-ROOT*` (set when the image is finalized) is the fallback.
Locations are **definition level**: "jump from a backtrace frame to source" opens the definition of the
frame's function, not the line being executed.

Compiled code objects also carry their lambda list (`FUNCTION-ARGLIST`, `ARGLIST-STRING`); macros record
theirs on the symbol.

## Cross reference (`00-xref.lisp`)

`XREF-BUILD` disassembles every compiled function, macro expander and generic-function method and
indexes the operands of `GFUN` (calls), `GREF` (references), `GSET` (assignments) and `BIND` (special
bindings). `WHO-CALLS`, `WHO-REFERENCES`, `WHO-SETS`, `WHO-BINDS`, `CALLEES`, `WHO-SPECIALIZES`,
`WHO-DEFINES`, `ALL-FUNCTIONS`, `APROPOS-LIST`. Limits: calls the compiler open-codes (`car`, `cdr`,
`cons`, `eq`, `+`, `-`, comparisons, `null`, `not`, `1+`, `1-`) leave no call instruction and are not
reported; functions only reachable through data structures are not scanned.

## Trace, step, profile (`10-trace.lisp`)

* `TRACE`/`UNTRACE` replace the function cell with a wrapper that prints arguments and values with
  indentation by depth. Calls compiled as open-coded primitives cannot be traced.
* `STEP` / `STEP-FORM` / `STEP-TRACE`: a small evaluator written in Lisp that pauses at each function
  call (`s` step into, `n` next, `c` continue, `q` quit). Macros are expanded transparently. It steps
  *into* only functions whose source was recorded (`*RECORD-LAMBDA-EXPRESSIONS*`, true for definitions
  made interactively in the finished image); everything else is called as a unit.
* `WITH-PROFILING` / `PROFILE-REPORT`: `SIGPROF` sets a flag and the VM records the innermost 16
  function names at its next safe point (a call or a backward jump). Only compiled frames are visible,
  and code that neither calls nor loops is never sampled, so the profile is biased towards call-heavy code.

## Advice, dribble, systems (`15-advise.lisp`, `20-systems.lisp`)

`advise` installs one wrapper per function, which reads the advice list from the symbol's plist on each call:
`:before` functions take and return the argument list, `:around` functions get a continuation (the `:do-it` form is
replaced by a call to it), and `:after` functions take and return the list of values. Removing the last advice
restores the original function. `dribble` copies the window Listener's output buffer (`*dribble-pane*`, checked in
`ob-write`), or for the line Listener binds `*standard-output*` to a broadcast stream and records each form read.

`defsystem` records the components; `load-system` keeps each file's write date as loaded and reloads a file when it
differs. Patches are `patches/NAME-N.lisp`, loaded in order past the system's patch level. `save-world` asks the
kernel to save (`%request-save`); the image is written at the next safe point (`gc()`), where the VM reloads its
cached pointers after the heap has been compacted, never inside the primitive itself.

## Windows

Report windows (`REPORT-WINDOW`) are read-only presentation windows produced by a function. Built on
them: the **Inspector** (lists, vectors, structures, CLOS instances, hash tables, functions, symbols,
packages, classes, numbers; click a value to descend, BACKSPACE goes back, `:set-entry N expr` edits a
component), the package, function and class browsers, cross-reference and apropos windows, and
clickable test failures (`:run-tests`). Presentation commands exist for symbols (edit definition,
who calls, who references, callees, trace) and for backtrace frames and test failures (edit definition).
