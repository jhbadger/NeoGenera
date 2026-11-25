;;;; 90-test.lisp -- a small test framework used by tests/lisp and the build.

(in-package :cl)

(defvar *tests* nil)
(defvar *test-failures* nil)
(defvar *test-checks* 0)
(defvar *current-test* nil)

(defmacro deftest (name &body body)
  `(progn (%register-test ',name (lambda () ,@body)) ',name))

(defun %register-test (name fn)
  (let ((cell (assq name *tests*)))
    (if cell (rplacd cell fn) (setq *tests* (append *tests* (list (cons name fn)))))))

(defun %check (ok form-text detail)
  (setq *test-checks* (+ *test-checks* 1))
  (unless ok
    (setq *test-failures* (cons (list *current-test* form-text detail) *test-failures*)))
  ok)

(defmacro is (form)
  `(%check ,form ',form nil))

(defmacro is-equal (expected form)
  (let ((e (gensym)) (v (gensym)))
    `(let ((,e ,expected) (,v ,form))
       (%check (equal ,e ,v) ',form (list :expected ,e :got ,v)))))

(defmacro signals-error (form)
  (let ((r (gensym)))
    `(let ((,r (handler-case (progn ,form :no-error) (error (e) e))))
       (%check (stringp ,r) ',form "expected an error"))))

(defun run-tests (&optional names)
  (setq *test-failures* nil)
  (setq *test-checks* 0)
  (let ((ran 0))
    (dolist (entry *tests*)
      (when (or (null names) (member (car entry) names))
        (setq ran (+ ran 1))
        (let ((*current-test* (car entry)))
          (handler-case (funcall (cdr entry))
            (error (e) (%check nil (list 'test-body (car entry)) (list :error e)))))))
    (dolist (f (reverse *test-failures*))
      (format t "FAIL ~A: ~S~@[ ~S~]~%" (car f) (cadr f) (caddr f)))
    (format t "~D tests, ~D checks, ~D failures~%" ran *test-checks* (length *test-failures*))
    (null *test-failures*)))
