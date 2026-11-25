;;;; compiler-tests.lisp -- tests aimed at the bytecode compiler and VM (Phase 3).

(in-package :cl)

(defun %compile-eval (form) (funcall (compile nil (list 'lambda nil form))))

;; Iteration counts shrink under GC stress (NG_STRESS=1) where every call collects.
(defvar *scale* (if (getenv "NG_STRESS") 1 200))
(defun scaled (n) (* n *scale*))

(defvar *ct-special* :global)
(defun ct-read-special () *ct-special*)

(deftest compiler-basics
  (is-equal 3 (%compile-eval '(+ 1 2)))
  (is-equal 6 (%compile-eval '(+ 1 2 3)))
  (is-equal -1 (%compile-eval '(- 1 2)))
  (is-equal t (%compile-eval '(< 1 2)))
  (is-equal nil (%compile-eval '(>= 1 2)))
  (is-equal '(1 . 2) (%compile-eval '(cons 1 2)))
  (is-equal 8 (%compile-eval '(1+ 7)))
  (is-equal 'b (%compile-eval '(if nil 'a 'b)))
  (is-equal '(1 2 3) (%compile-eval '(let* ((a 1) (b (+ a 1)) (c (+ b 1))) (list a b c))))
  (is-equal 15 (funcall (compile nil '(lambda (&rest xs) (apply #'+ xs))) 1 2 3 4 5)))

(deftest lambda-lists
  (let ((f (compile nil '(lambda (a &optional (b 10) (c (+ a b) c-p)) (list a b c c-p)))))
    (is-equal '(1 10 11 nil) (funcall f 1))
    (is-equal '(1 2 3 t) (funcall f 1 2 3))
    (is-equal '(1 2 3 nil) (funcall f 1 2)))
  (let ((f (compile nil '(lambda (&key (x 1) (y 2) z) (list x y z)))))
    (is-equal '(1 2 nil) (funcall f))
    (is-equal '(9 2 nil) (funcall f :x 9))
    (is-equal '(1 5 7) (funcall f :z 7 :y 5))
    (signals-error (funcall f :bogus 1)))
  (let ((f (compile nil '(lambda (a &rest r &key k) (list a r k)))))
    (is-equal '(1 (:k 2) 2) (funcall f 1 :k 2)))
  (let ((f (compile nil '(lambda (x &aux (y (* x 2))) (list x y)))))
    (is-equal '(3 6) (funcall f 3)))
  (signals-error (funcall (compile nil '(lambda (a b) a)) 1)))

(deftest closures
  (let ((counter (%compile-eval '(let ((n 0)) (lambda () (setq n (+ n 1)) n)))))
    (is-equal '(1 2 3) (list (funcall counter) (funcall counter) (funcall counter))))
  (is-equal '(10 20)
            (%compile-eval '(let ((a 10) (b 20)) (let ((f (lambda () (list a b)))) (funcall f)))))
  (is-equal 7 (%compile-eval '(let ((x 1)) (let ((f (lambda () (setq x 7)))) (funcall f) x))))
  (is-equal '(3 3 3)
            (%compile-eval '(let ((fs nil) (i 0))
                             (loop repeat 3 do (setq i (+ i 1)) (push (lambda () i) fs))
                             (mapcar #'funcall fs))))
  (is-equal 120 (%compile-eval '(labels ((fact (n acc) (if (<= n 1) acc (fact (- n 1) (* acc n))))) (fact 5 1))))
  (is-equal '(t nil)
            (%compile-eval '(labels ((ev (n) (if (= n 0) t (od (- n 1)))) (od (n) (if (= n 0) nil (ev (- n 1)))))
                             (list (ev 10) (ev 7))))))

(deftest tail-calls
  ;; 1,000,000 iterations normally: would overflow the stack without proper TCO
  (is-equal 0 (%compile-eval `(labels ((loop-down (n) (if (= n 0) 0 (loop-down (- n 1))))) (loop-down ,(scaled 5000)))))
  (let ((n (scaled 5000)))
    (is-equal (floor (* n (+ n 1)) 2)
              (%compile-eval `(labels ((sum (n acc) (if (= n 0) acc (sum (- n 1) (+ acc n))))) (sum ,n 0)))))
  (is-equal :done (%compile-eval `(labels ((a (n) (if (= n 0) :done (b (- n 1)))) (b (n) (a n))) (a ,(scaled 1500))))))

(deftest blocks-and-tagbody
  (is-equal 3 (%compile-eval '(block b (dolist (x '(1 2 3 4)) (when (= x 3) (return-from b x))) nil)))
  (is-equal '(3 2 1) (%compile-eval '(let ((acc nil) (i 0))
                                      (tagbody top (setq i (+ i 1)) (push i acc) (if (< i 3) (go top)))
                                      acc)))
  ;; non-local exit from a closure
  (is-equal :out (%compile-eval '(block b (funcall (lambda () (return-from b :out))) :in)))
  ;; GO from inside a closure
  (is-equal 5 (%compile-eval '(let ((n 0))
                               (tagbody top
                                  (funcall (lambda () (setq n (+ n 1)) (if (< n 5) (go top)))))
                               n)))
  (is-equal '(1 2) (%compile-eval '(block nil (let ((a 1)) (return (list a 2))))))
  (is-equal 10 (%compile-eval '(let ((s 0)) (dotimes (i 5 s) (setq s (+ s i)))))))

(deftest dynamic-extent
  (is-equal :global (ct-read-special))
  (is-equal :bound (%compile-eval '(let ((*ct-special* :bound)) (ct-read-special))))
  (is-equal :global (progn (%compile-eval '(let ((*ct-special* :tmp)) nil)) (ct-read-special)))
  (is-equal :global (progn (%compile-eval '(block b (let ((*ct-special* :tmp)) (return-from b 1)))) (ct-read-special)))
  (is-equal :global (progn (ignore-errors (%compile-eval '(let ((*ct-special* :tmp)) (error "x")))) (ct-read-special)))
  (is-equal :inner (%compile-eval '(let ((*ct-special* :a)) (let ((*ct-special* :inner)) (ct-read-special))))))

(deftest unwind-and-catch
  (is-equal '(:cleanup 1)
            (let ((log nil))
              (let ((r (%compile-eval '(catch 'x 1))))
                (setq log (list :cleanup r)))
              log))
  (is-equal :ran (let ((flag nil))
                   (catch 'k (unwind-protect (throw 'k 1) (setq flag :ran)))
                   flag))
  (is-equal 2 (%compile-eval '(catch 'a (catch 'b (throw 'a 2)) 3)))
  (is-equal '(1 2 3) (%compile-eval '(multiple-value-list (unwind-protect (values 1 2 3) (list 9))))))

(deftest multiple-values
  (is-equal '(1 2 3) (multiple-value-list (funcall (compile nil '(lambda () (values 1 2 3))))))
  (is-equal '(2 1) (%compile-eval '(multiple-value-bind (a b) (values 1 2) (list b a))))
  (is-equal '(1 nil) (%compile-eval '(multiple-value-bind (a b) (values 1) (list a b))))
  (is-equal 1 (%compile-eval '(values 1 2)))
  (is-equal '(5 1) (%compile-eval '(multiple-value-list (floor 11 2))))
  (is-equal 6 (%compile-eval '(multiple-value-call #'+ (values 1 2) (values 3))))
  (is-equal '(1 2) (%compile-eval '(multiple-value-list (let ((x 1)) (values x 2)))))
  (is-equal '(7 8) (multiple-value-list (funcall (compile nil '(lambda () (block b (return-from b (values 7 8)))))))))

(deftest return-from-values
  ;; multiple values and tail calls through a RETURN-FROM nested in loops
  (is-equal '(1 2) (multiple-value-list (funcall (compile nil '(lambda () (loop (return (values 1 2))))))))
  (let ((f (compile nil '(lambda (l) (dolist (x l) (when (> x 2) (return-from nil (values x (+ x 1)))))))))
    (is-equal '(3 4) (multiple-value-list (funcall f '(1 2 3)))))
  (is-equal '(5 6) (multiple-value-list (%compile-eval '(block b (let ((x 5)) (return-from b (values x 6)))))))
  ;; with a dynamic binding in between the value is still returned correctly
  (is-equal '(:in :bound) (multiple-value-list (%compile-eval '(block b (let ((*ct-special* :bound)) (return-from b (values :in *ct-special*)))))))
  (is-equal '(1 2) (multiple-value-list (%compile-eval '(let ((*ct-special* :b)) (values 1 2)))))
  (is-equal '(1 2) (multiple-value-list (%compile-eval '(let ((*ct-special* :b)) (let ((*ct-special* :c)) (values 1 2))))))
  (is-equal '(7) (multiple-value-list (%compile-eval '(let ((a (values 1 2))) (let ((*ct-special* :b)) (+ a 6))))))
  (is-equal '(1 2) (multiple-value-list (%compile-eval '(let ((*ct-special* :b)) (if t (values 1 2) 3)))))
  (is-equal '(3 4) (multiple-value-list (%compile-eval '(let ((*ct-special* :b)) (cond ((null nil) (values 3 4)) (t 5))))))
  (is-equal '(9) (multiple-value-list (%compile-eval '(let ((*ct-special* :b)) (if nil 1 9)))))
  (is-equal '(:global) (progn (%compile-eval '(let ((*ct-special* :b)) (if t (values 1 2) 3))) (list (ct-read-special))))
  (is-equal :global (ct-read-special))
  (is-equal 100000 (%compile-eval '(labels ((f (n) (block nil (when (= n 0) (return 100000)) (return (f (- n 1)))))) (f 5000)))))

(deftest compiled-defuns
  (%compile-eval '(defun ct-fib (n) (if (< n 2) n (+ (ct-fib (- n 1)) (ct-fib (- n 2))))))
  (is-equal 832040 (funcall 'ct-fib 30))
  (%compile-eval '(defun ct-opt (a &optional (b 5)) (+ a b)))
  (is-equal 6 (ct-opt 1))
  (%compile-eval '(defmacro ct-twice (x) `(progn ,x ,x)))
  (is-equal 2 (%compile-eval '(let ((n 0)) (ct-twice (setq n (+ n 1))) n)))
  (%compile-eval '(defstruct ct-pt x y))
  (is-equal 5 (%compile-eval '(ct-pt-x (make-ct-pt :x 5 :y 6))))
  (is-equal 9 (%compile-eval '(let ((p (make-ct-pt :x 1 :y 2))) (setf (ct-pt-y p) 9) (ct-pt-y p)))))

(deftest gc-under-compiled-code
  ;; allocate heavily from compiled code so collections happen mid-execution
  (let ((n (%compile-eval `(let ((acc nil))
                            (dotimes (i ,(scaled 1000)) (push (cons i (list i i)) acc) (when (> (length acc) 100) (setq acc nil)))
                            (length acc)))))
    (is (<= n 101)))
  (let ((n (scaled 500)))
    (is-equal (floor (* n (- n 1)) 2)
              (%compile-eval `(let ((s 0)) (dotimes (i ,n) (setq s (+ s i))) s)))))

(deftest fasl-roundtrip
  (let ((src "build/ct-test-src.lisp") (out "build/ct-test.nfasl"))
    (with-open-file (s src :direction :output)
      (write-line "(in-package :cl)" s)
      (write-line "(defvar *ct-fasl-var* 41)" s)
      (write-line "(defun ct-fasl-fn (x) (+ x *ct-fasl-var* 1))" s)
      (write-line "(defmacro ct-fasl-mac (a) (list 'quote (list a a)))" s))
    (compile-file src :output-file out)
    (load out)
    (is-equal 43 (ct-fasl-fn 1))
    (is-equal '(z z) (%compile-eval '(ct-fasl-mac z)))))
