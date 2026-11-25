;;;; 00-xref.lisp -- lambda lists, source locations and cross-reference over compiled code.
;;;;
;;;; Cross-references come from disassembling the bytecode of every compiled function,
;;;; macro expander and generic-function method: the operands of GFUN (calls), GREF (variable
;;;; references), GSET (assignments) and BIND (special bindings).  Calls the compiler
;;;; open-codes (CAR, CONS, +, ...) leave no trace and are not reported.

(in-package :cl)

(defun function-arglist (fn)
  (and (functionp fn) (compiled-function-p fn) (nth 6 (%code-parts fn))))

(defun symbol-arglist (sym)
  "Returns (values lambda-list found-p) for the function, generic function or macro named SYM."
  (let ((gf (%find-gf sym)) (m (get sym '%macro-ll)))
    (cond (gf (values (gf-lambda-list gf) t))
          (m (values m t))
          ((and (fboundp sym) (not (macro-function sym)))
           (let ((ll (function-arglist (symbol-function sym))))
             (if (or ll (and (compiled-function-p (symbol-function sym)) (eql 0 (nth 1 (%code-parts (symbol-function sym))))))
                 (values ll t)
                 (values nil nil))))
          (t (values nil nil)))))

(defun arglist-string (sym)
  (multiple-value-bind (ll found) (symbol-arglist sym)
    (and found (let ((*package* (or (symbol-package sym) *package*))) (format nil "(~A~{ ~A~})" (string-downcase (symbol-name sym)) (mapcar (lambda (x) (string-downcase (princ-to-string x))) ll))))))

;;; ---- bytecode walking ---------------------------------------------------------------------

(defun %code-references (code)
  "List of (kind . symbol) for CODE (a code object or closure) and all lambdas nested in it."
  (let* ((parts (%code-parts code)) (instrs (nth 4 parts)) (consts (nth 5 parts)) (refs nil))
    (loop
      (when (null instrs) (return))
      (let* ((entry (find (car instrs) *opcodes* :key #'cadr)) (op (car entry)) (n (caddr entry)))
        (cond ((member op '(:gfun :gref :gset :bind))
               (let ((sym (aref consts (cadr instrs)))) (when (symbolp sym) (setq refs (cons (cons op sym) refs)))))
              ((eq op :closure)
               (let ((nested (aref consts (cadr instrs)))) (when (%codep nested) (setq refs (append (%code-references nested) refs))))))
        (setq instrs (nthcdr (+ 1 n) instrs))))
    refs))

(defvar *xref* nil)

(defun %xref-add (table owner fn)
  (when (and (functionp fn) (compiled-function-p fn))
    (dolist (r (%code-references fn))
      (let ((cell (gethash (cdr r) table)))
        (unless (member-if (lambda (e) (and (eq (car e) owner) (eq (cdr e) (car r)))) cell)
          (setf (gethash (cdr r) table) (cons (cons owner (car r)) cell)))))))

(defun xref-build ()
  "Scan every compiled function in every package; returns the number of functions scanned."
  (let ((table (make-hash-table :test 'eq)) (n 0))
    (dolist (p (list-all-packages))
      (dolist (sym (%package-symbols p nil))
        (when (and (symbol-package sym) (eq (symbol-package sym) p))
          (when (fboundp sym)
            (let ((f (symbol-function sym)))
              (%xref-add table sym f)
              (setq n (+ n 1))))
          (let ((m (macro-function sym))) (when m (%xref-add table sym m) (setq n (+ n 1)))))))
    (dolist (gf *generic-functions*)
      (dolist (m (gf-methods gf))
        (%xref-add table (let ((nm (gf-name gf))) (if (consp nm) (%fn-symbol nm) nm)) (method-fn m))))
    (setq *xref* table)
    n))

(defun xref-ensure () (unless *xref* (xref-build)) *xref*)

(defun %xref-query (sym kind)
  (let ((acc nil))
    (dolist (e (gethash sym (xref-ensure)))
      (when (eq (cdr e) kind) (setq acc (adjoin (car e) acc))))
    (sort acc (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(defun who-calls (sym) (%xref-query sym :gfun))
(defun who-references (sym) (%xref-query sym :gref))
(defun who-sets (sym) (%xref-query sym :gset))
(defun who-binds (sym) (%xref-query sym :bind))

(defun callees (sym)
  "Functions called by the compiled function named SYM."
  (let ((acc nil) (f (and (fboundp sym) (symbol-function sym))))
    (when (and f (compiled-function-p f))
      (dolist (r (%code-references f)) (when (eq (car r) :gfun) (setq acc (adjoin (cdr r) acc)))))
    (sort acc (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(defun who-specializes (class-name)
  (let ((c (find-class class-name)) (acc nil))
    (dolist (gf *generic-functions*)
      (dolist (m (gf-methods gf))
        (when (member c (method-specializers m)) (setq acc (cons m acc)))))
    acc))

(defun who-defines (sym) (definition-locations sym))

(defun symbols-matching (string &optional package)
  (apropos-list string package))

(defun all-functions (&optional package)
  "Sorted list of the symbols in PACKAGE (default: all packages) that name functions."
  (let ((acc nil))
    (dolist (p (if package (list (find-package package)) (list-all-packages)))
      (dolist (s (%package-symbols p nil))
        (when (and (symbol-package s) (eq (symbol-package s) p) (or (fboundp s) (macro-function s))) (setq acc (cons s acc)))))
    (sort (remove-duplicates acc) (lambda (a b) (string< (symbol-name a) (symbol-name b))))))
