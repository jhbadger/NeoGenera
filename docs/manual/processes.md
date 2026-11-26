# Processes and Peek

NeoGenera runs several Lisp **processes** at once, as Genera did. A process is a thread of control with its own
stacks and its own special bindings; global values, functions and the heap are shared. The process you type at
(the Listener, or the window system) is the *initial process*.

## Starting a process

```lisp
(process-run-function "Counter"
  (lambda () (dotimes (i 10) (print i) (process-sleep 1))))
```

`process-run-function` takes a name (or a list of options: `:name`, and `:output` for the stream the process
prints to) and a function with its arguments, starts the process and returns it at once. A background process
prints into a buffer of its own: `process-output-text` returns what it printed, and Peek shows the last line.
It reads from an empty stream, and starts in the package of the process that created it.

`current-process` (or the variable `*current-process*` inside a process) is the process running the code,
`all-processes` lists the live ones, and `find-process` finds one by name or number. `show-processes` prints a table,
`*recent-processes*` holds the ones that finished lately (at most `*recent-process-limit*`), and
`*initial-process*` is the initial process once it has been named.

A `process` is a structure: `process-name`, `process-state` (`:active`, `:done`, `:killed`, `:error` or
`:aborted`), `process-whostate` (what it is doing: "Run", "Sleep", or the reason it waits),
`process-state-summary`, `process-active-p`, `process-run-time` (seconds of processor time), `process-id`,
`process-result` (the values of the function, as a list), `process-error` and `process-backtrace` (for a process
that got an error), `process-function`, `process-args`, `process-output`, `process-initial`, `process-arrested`,
`process-interrupts`, `process-created` and `process-finished` (universal times). `process-p` recognizes one. The
output buffer is a `process-output-stream`.

## Scheduling

Processes take turns. The running process is **preempted** every 20 milliseconds at a safe point (a function
call or a loop iteration), and gives up the processor whenever it waits, sleeps, reads the terminal or waits for
a subprocess. So a process in an endless loop never freezes the window system.

| Function | Meaning |
|---|---|
| `process-wait` | `(process-wait "Input" #'pred args...)` returns when the predicate is true |
| `process-wait-with-timeout` | the same with a limit in seconds; NIL on timeout |
| `process-sleep` | sleep for some seconds (Genera counted 60ths), staying responsive to kill and interrupt |
| `process-allow-schedule` | let other processes run now; `process-yield` is a synonym |
| `process-join` | wait for a process to finish and return its values |
| `without-interrupts` | run a short body without being preempted or interrupted |

`process-wait` polls its predicate: when nothing else can run it checks every few milliseconds without using the
processor. Plain `sleep` also lets the other processes run, but cannot be interrupted until it returns.

## Controlling processes

* `process-kill` makes a process unwind (its `unwind-protect` cleanups run) and finish as `:killed`.
* `process-interrupt` makes a process call a function at its next safe point and then carry on.
* `process-arrest` stops scheduling a process until `process-unarrest`.

The initial process cannot be killed or arrested.

An error that a process does not handle ends it in state `:error`; the condition and a backtrace are kept, and
in the window system the mouse documentation line announces it. The variable `*process-error-hook*` holds the
function that is told.

## Locks

```lisp
(defvar *lock* (make-lock "Table"))
(with-lock-held (*lock*) (incf (gethash key *table* 0)))
```

`make-lock` makes a lock (`lock-name`, `lock-owner`, `lock-p`); a `lock` is a structure. `with-lock-held` takes
it around a body; a process that already holds the lock just runs the body. `process-lock` and `process-unlock`
take and release it explicitly. Waiting for a lock is a `process-wait`, so Peek shows the waiting process as
"Lock".

## Peek

`:peek` opens Peek, a window that redraws itself every second. Its modes:

| Key | Mode |
|---|---|
| `P` | processes: number, name, state or whostate, processor time, switches and the last line of output; then the processes that finished recently |
| `M` | memory: heap use, collections, packages, generic functions, documents |
| `W` | windows: every window with its kind, size and position (click one to select it) |

Click a process for **Describe**, **Show output**, **Show error and backtrace**, **Kill**, **Arrest** and
**Un-arrest**. `G` redraws at once and `Q` closes the window. The who-line shows how many processes exist when
there is more than one.

Listener commands: `:peek [mode]`, `:show-processes`, `:kill-process name-or-number`.

## Limits

* Only one process runs Lisp at a time (there is one interpreter lock), so processes give concurrency, not
  parallel speed. Subprocesses started with `run-program` do run in parallel.
* A process is preempted only at safe points. Code that runs a long time inside one primitive (a large sort, a
  BLAST search) holds the processor until it returns.
* Streams, hash tables and other objects are not locked: two processes writing the same stream or table at the
  same time should use a lock.
* Processes are not saved in heap images.
