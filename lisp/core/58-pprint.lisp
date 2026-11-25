;;;; 58-pprint.lisp -- a small pretty printer (PPRINT).
;;;;
;;;; Fills to *PRINT-RIGHT-MARGIN*: forms that fit are printed flat; longer lists
;;;; break one element per line, aligned under the first argument (calls), with a
;;;; two-space body indent for definition/binding forms, or under the first
;;;; element (data lists).  PPRINT-LOGICAL-BLOCK and the ~< format directives
;;;; are not implemented.

(in-package :cl)

(defvar *print-right-margin* 80)
(defvar *print-pretty* nil)

(defparameter *pp-body-ops*
  '(defun defmacro defvar defparameter defconstant defstruct defclass defgeneric defmethod
    lambda let let* flet labels macrolet symbol-macrolet when unless dolist dotimes block progn
    prog1 handler-case handler-bind restart-case destructuring-bind multiple-value-bind
    with-open-file with-slots with-accessors with-output-to-string unwind-protect catch loop do do*
    case ecase typecase etypecase define-condition deftest eval-when))

(defun %proper-list-p (x) (and (listp x) (>= (list-length-safe x) 0)))
(defun list-length-safe (x)
  (let ((n 0))
    (loop (cond ((null x) (return n)) ((atom x) (return -1)) (t (setq x (cdr x)) (setq n (+ n 1)))))))

(defun %pp-indent (out n) (dotimes (i n) (write-char #\Space out)))

(defun %pp-distinguished (head)
  (case head
    ((defun defmacro defmethod defgeneric destructuring-bind multiple-value-bind define-condition deftest) 2)
    ((progn prog1 block catch unwind-protect loop eval-when) 0)
    (t 1)))

(defun %pp-string (obj col width)
  (let ((flat (prin1-to-string obj)))
    (cond ((or (atom obj) (<= (+ col (length flat)) width) (not (%proper-list-p obj))) flat)
          ((and (symbolp (car obj)) (member (car obj) *pp-body-ops*)) (%pp-body obj col width))
          (t (%pp-call obj col width flat)))))

(defun %pp-call (obj col width flat)
  (let ((head (car obj)) (args (cdr obj)))
    (cond ((null args) flat)
          ((symbolp head)
           (let* ((hs (prin1-to-string head)) (acol (+ col 2 (length hs))))
             (with-output-to-string (o)
               (write-string "(" o) (write-string hs o) (write-char #\Space o)
               (write-string (%pp-string (car args) acol width) o)
               (dolist (a (cdr args))
                 (terpri o) (%pp-indent o acol)
                 (write-string (%pp-string a acol width) o))
               (write-char #\) o))))
          (t (with-output-to-string (o)
               (write-char #\( o)
               (write-string (%pp-string head (+ col 1) width) o)
               (dolist (a args)
                 (terpri o) (%pp-indent o (+ col 1))
                 (write-string (%pp-string a (+ col 1) width) o))
               (write-char #\) o))))))

(defun %pp-body (obj col width)
  (let* ((head (car obj)) (hs (prin1-to-string head)) (nd (%pp-distinguished head)) (n 0) (icol (+ col 2)))
    (with-output-to-string (o)
      (write-string "(" o) (write-string hs o)
      (dolist (a (cdr obj))
        (if (< n nd)
            (progn (write-char #\Space o) (write-string (%pp-string a (+ col 2 (length hs)) width) o))
            (progn (terpri o) (%pp-indent o icol) (write-string (%pp-string a icol width) o)))
        (setq n (+ n 1)))
      (write-char #\) o))))

(defun pprint (obj &optional stream)
  (let ((s (%out stream)))
    (terpri s)
    (write-string (%pp-string obj (%stream-column s) *print-right-margin*) s)
    (values)))

(defun pretty-string (obj &optional (width *print-right-margin*)) (%pp-string obj 0 width))
