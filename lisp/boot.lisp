;;;; boot.lisp -- stage 0: load the whole system with the C evaluator.
;;;; Used by `make stage0`.  After this the compiler exists but EVAL is still
;;;; the interpreter (see %USE-COMPILER-EVAL).

(setq *package* (find-package "COMMON-LISP"))
(%load-file "lisp/sources.lisp")
(labels ((load-all (files)
           (if files
               (progn (%load-file (car files)) (load-all (cdr files))))))
  (load-all *system-sources*))
