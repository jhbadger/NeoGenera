---
name: Bug report
about: Something crashed, gave a wrong answer or behaved unexpectedly
labels: bug
---

**What happened**

**What you expected**

**How to reproduce** (the smallest Lisp form or file that shows it; paste it here)

```lisp

```

**Environment**
- NeoGenera version (`(princ *ng-version*)`):
- OS and compiler (`uname -a`, `cc --version`):
- Window system or line-mode Listener:

If the process crashed, please include the backtrace it printed, and run the same input with `make asan` if you can.
Security problems: do not file them here; see SECURITY.md.
