;;;; bench.lisp -- stage-0 evaluator vs. the bytecode compiler/VM.
;;;; Run:  build/neogenera -i build/neogenera.img --no-toplevel --load tests/lisp/bench.lisp

(in-package :cl)

(defmacro bench (name form)
  `(let ((start (get-internal-real-time)))
     (let ((r ,form))
       (format t "~A~34T~6D ms   => ~S~%" ,name (floor (- (get-internal-real-time) start) 1000) r))))

(defmacro both (name args &body body)
  ;; defines NAME-I (interpreted by the C evaluator) and NAME-C (compiled)
  (let ((i (%sym-concat name "-I")) (c (%sym-concat name "-C")))
    `(progn
       (%eval '(defun ,i ,args ,@(%rename body name i)))
       (compiled-eval '(defun ,c ,args ,@(%rename body name c))))))

(defun %rename (form from to)
  (cond ((eq form from) to)
        ((consp form) (cons (%rename (car form) from to) (%rename (cdr form) from to)))
        (t form)))

(both fib (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(both tak (x y z) (if (not (< y x)) z (tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y))))
(both sumloop (n) (let ((s 0)) (dotimes (i n) (setq s (+ s i))) s))
(both consing (n) (let ((l nil)) (dotimes (i n) (setq l (cons i l))) (length l)))
(both sieve (n)
  (let ((v (make-array (+ n 1) :initial-element t)) (count 0))
    (do ((i 2 (+ i 1))) ((> i n) count)
      (when (aref v i)
        (setq count (+ count 1))
        (do ((j (* i i) (+ j i))) ((> j n)) (%aset v j nil))))))

(format t "~&--- interpreter (C evaluator) ---~%")
(bench "fib 25" (fib-i 25))
(bench "tak 18 12 6" (tak-i 18 12 6))
(bench "sum loop 1e6" (sumloop-i 1000000))
(bench "cons 1e6" (consing-i 1000000))
(bench "sieve 1e6" (sieve-i 1000000))
(format t "~&--- bytecode compiler + VM ---~%")
(bench "fib 25" (fib-c 25))
(bench "tak 18 12 6" (tak-c 18 12 6))
(bench "sum loop 1e6" (sumloop-c 1000000))
(bench "cons 1e6" (consing-c 1000000))
(bench "sieve 1e6" (sieve-c 1000000))
(format t "~&--- compiled only (larger) ---~%")
(bench "fib 32" (fib-c 32))
(bench "sum loop 1e8" (sumloop-c 100000000))
(bench "compile the compiler's own sources (analysis+codegen, no I/O)"
       (let ((n 0))
         (dolist (f (%read-all-forms "lisp/compiler/20-codegen.lisp"))
           (%compile-root (macroexpand f) nil) (incf n))
         n))
