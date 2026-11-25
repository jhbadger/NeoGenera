;;;; run-all.lisp -- load and run every Lisp test file; exit status reflects the result.
(in-package :cl)
(load "tests/lisp/core-tests.lisp")
(load "tests/lisp/compiler-tests.lisp")
(let ((ok (run-tests)))
  (exit (if ok 0 1)))
