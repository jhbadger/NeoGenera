# Processes

Status: implemented in `src/proc.c` (kernel), `lisp/core/62-process.lisp` (the Lisp interface) and
`lisp/ui/70-peek.lisp` (Peek). The user documentation is `docs/manual/processes.md`.

## Model

A Lisp process is a POSIX thread with its own C stack, value stack, VM frames, special-binding stack,
root stack and catch frames. Only one process runs Lisp at a time: it holds the **interpreter lock**.
The interpreter's globals (`vstack`, `vsp`, `vframes`, `vfp`, `specpdl`, `spsp`, `root_stack`,
`nroots`, `frame_top`, `eval_depth`, `gc_inhibit`, the stack limits and the multiple-value registers)
always describe the running process; a switch saves them into the outgoing `Proc` and loads the
incoming one. The initial process keeps the large static stacks; other processes get smaller
`calloc`ed ones (1M values, 128K frames, 64K bindings, 128K roots, a 256 MB C stack, all reserved
lazily by the OS).

Threads with a lock were chosen over coroutines (`ucontext`) because `swapcontext` is missing on
Android/bionic, and because a thread keeps its C stack, so a process can be switched out deep inside
primitives that call back into Lisp.

## When a process gives up the lock

* At a **safe point** (`CALL`, `TAILCALL`, a backward `JMP`, and every `eval` step of the stage-0
  evaluator) when `ng_attention` is set. A ticker thread sets it every 20 ms while more than one
  process exists (preemption); an interrupt or arrest request, or a switch-in with one pending, sets
  it too. Nothing switches while `gc_inhibit` is set or inside `WITHOUT-INTERRUPTS`.
* Around a **blocking system call**: `sleep`, the terminal's `poll`, `run-program` while the child
  runs, and `%stdin-wait` (the line Listener waits for input this way). The code between
  `proc_blocking_begin` and `proc_blocking_end` must hold no unprotected `Obj`.
* When it **finishes**.

These are exactly the places a collection could already happen, so the GC's rules are unchanged:
whenever a process is switched out, every live `Obj` it has is in rooted memory.

## Special bindings

Bindings are shallow (the value cell holds the innermost binding). Switching out walks the
outgoing process's `specpdl` from the top, swapping each symbol's value with the entry's saved
value, which leaves the global values in the symbols and the process's own values in its `specpdl`;
switching in walks it from the bottom and swaps back. Global values are therefore shared, and a
`let` of a special variable is private to its process.

## Scheduling

Round robin over the processes waiting for the lock that are not arrested (`pick_ready`). The
releasing process hands the lock directly to the next one (`current = next`, signal its condition
variable), so a busy process cannot take it straight back. A process returning from a blocking call
takes the lock if nobody holds it, else queues.

`PROCESS-WAIT` polls its predicate. Each time the predicate is false it calls
`%process-wait-yield`: if some process that is not itself waiting can run, it yields to it;
otherwise it sleeps 5 ms without the lock. Waiting processes therefore cost a few hundred cheap
wake-ups a second, not a busy loop.

## GC roots

`gc()` calls `gc_forward_roots`, which forwards, for every process that is not running, its saved
value stack, `specpdl` (symbols and values), root-stack entries (pointers into that thread's C stack,
which stays valid while the thread is blocked), VM frame closures, saved multiple values and catch
frames, and for every process its Lisp object and initial function.

## Interrupts, kill, arrest

`%process-interrupt` sets a flag on the target; at its next safe point the target calls
`%process-run-interrupts`, which runs the queued functions (the multiple-value registers are saved
around the call). `PROCESS-KILL` is an interrupt that throws to the `%process-kill` catch around the
process's function, so `unwind-protect` cleanups run. An arrested process is skipped by the
scheduler; arresting the running process makes it yield at once.

## Limits

* One interpreter lock: concurrency, not parallel speed. A long primitive holds the processor.
* Streams, hash tables and other shared objects are not locked.
* Processes are not saved in heap images (the Lisp objects are; `%process-alive-p` maps a Lisp
  process to a kernel process by identity, so stale ones read as finished).
* At most 256 processes at once.
