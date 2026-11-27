;;;; package-tests.lisp -- the standard package API (lisp/core/65-packages.lisp, src/symbols.c) and package locks.

(in-package :cl)

;;; Helpers.  Each test uses its own PKT-* package names and deletes them afterwards, so tests are independent.

(defun %pkt-cleanup (&rest names)
  (dolist (n names)
    (let ((p (find-package n)))
      (when p
        (ignore-errors (unlock-package p))
        (dolist (u (package-used-by-list p)) (unuse-package p u))
        (delete-package p)))))

(defun %pkt-status (name package)
  (multiple-value-bind (s status) (find-symbol name package) (declare (ignore s)) status))

(defmacro %pkt-condition-type (form)
  "The class name of the error FORM signals, or :no-error."
  `(handler-case (progn ,form :no-error)
     (package-lock-violation () :lock)
     (package-error () :package-error)
     (error () :other-error)))

;;; ---- make-package, find-package, nicknames, rename, delete -------------------------------------------

(deftest make-package-basics
  (%pkt-cleanup "PKT-A" "PKT-A2")
  (let ((p (make-package "PKT-A" :nicknames '("PKT-A2"))))
    (is (packagep p))
    (is-equal "PKT-A" (package-name p))
    (is-equal '("PKT-A2") (package-nicknames p))
    (is (eq p (find-package "PKT-A")))
    (is (eq p (find-package "PKT-A2")))
    (is (eq p (find-package :pkt-a)))
    (is (eq p (find-package p)))
    (is (member p (list-all-packages)))
    ;; the default :use is COMMON-LISP, and :use nil really means nothing
    (is-equal (list (find-package "COMMON-LISP")) (package-use-list p))
    (is-equal nil (package-use-list (make-package "PKT-A3" :use nil)))
    (is (member p (package-used-by-list (find-package "COMMON-LISP"))))
    ;; names must be unique, including nicknames
    (is-equal :package-error (%pkt-condition-type (make-package "PKT-A")))
    (is-equal :package-error (%pkt-condition-type (make-package "PKT-A4" :nicknames '("PKT-A2"))))
    (is (null (find-package "PKT-A4"))))
  (%pkt-cleanup "PKT-A" "PKT-A2" "PKT-A3" "PKT-A4"))

(deftest rename-and-delete-package
  (%pkt-cleanup "PKT-R" "PKT-R-NEW" "PKT-R-NICK" "PKT-R-USER")
  (let* ((p (make-package "PKT-R" :use nil))
         (sym (intern "THING" p)))
    (is (eq p (rename-package p "PKT-R-NEW" '("PKT-R-NICK"))))
    (is-equal "PKT-R-NEW" (package-name p))
    (is-equal '("PKT-R-NICK") (package-nicknames p))
    (is (null (find-package "PKT-R")))
    (is (eq p (find-package "PKT-R-NICK")))
    (is-equal :package-error (%pkt-condition-type (rename-package p "COMMON-LISP")))
    ;; deleting a used package is a correctable error
    (let ((u (make-package "PKT-R-USER" :use (list p))))
      (is-equal :package-error (%pkt-condition-type (delete-package p)))
      (is (find-package "PKT-R-NEW"))
      (is (handler-bind ((package-error (lambda (c) (declare (ignore c)) (invoke-restart 'continue))))
            (delete-package p)))
      (is-equal nil (package-use-list u))
      (is (null (find-package "PKT-R-NEW")))
      (is (null (find-package "PKT-R-NICK"))))
    ;; the symbols of a deleted package are uninterned; the package object prints as deleted
    (is (null (symbol-package sym)))
    (is (null (package-name p)))
    (is (null (delete-package p)))
    (is (search "deleted" (prin1-to-string p)))
    (is (null (find-symbol "THING" (make-package "PKT-R-OTHER" :use nil)))))
  (%pkt-cleanup "PKT-R-USER" "PKT-R-OTHER"))

;;; ---- intern, shadow, unintern ---------------------------------------------------------------------

(deftest shadow-and-unintern
  (%pkt-cleanup "PKT-S")
  (let ((p (make-package "PKT-S" :use '("COMMON-LISP"))))
    (is (eq 'car (find-symbol "CAR" p)))
    (is (eq :inherited (%pkt-status "CAR" p)))
    (is (shadow "CAR" p))
    (let ((mine (find-symbol "CAR" p)))
      (is (not (eq mine 'car)))
      (is (eq p (symbol-package mine)))
      (is (eq :internal (%pkt-status "CAR" p)))
      (is-equal (list mine) (package-shadowing-symbols p))
      ;; shadowing again is a no-op
      (shadow '("CAR") p)
      (is-equal 1 (length (package-shadowing-symbols p)))
      ;; uninterning the shadowing symbol uncovers the inherited one again
      (is (unintern mine p))
      (is (null (symbol-package mine)))
      (is (eq 'car (find-symbol "CAR" p)))
      (is-equal nil (package-shadowing-symbols p)))
    ;; unintern of an inherited or absent symbol does nothing
    (is (null (unintern 'cdr p)))
    (is (eq 'cdr (find-symbol "CDR" p)))
    ;; shadow accepts symbols and lists
    (shadow '(cdr "FOO") p)
    (is (not (eq 'cdr (find-symbol "CDR" p))))
    (is (find-symbol "FOO" p)))
  (%pkt-cleanup "PKT-S"))

(deftest intern-and-find-symbol
  (%pkt-cleanup "PKT-I")
  (let ((p (make-package "PKT-I" :use nil)))
    (multiple-value-bind (s status) (intern "PKT-ALPHA-ZZ" p)
      (is (null status))                                  ; newly created
      (is (eq p (symbol-package s)))
      (multiple-value-bind (s2 status2) (intern "PKT-ALPHA-ZZ" p)
        (is (eq s s2))
        (is (eq :internal status2))))
    (is (eq :internal (%pkt-status "PKT-ALPHA-ZZ" p)))
    (is (null (%pkt-status "PKT-BETA-ZZ" p)))
    (export (find-symbol "PKT-ALPHA-ZZ" p) p)
    (is (eq :external (%pkt-status "PKT-ALPHA-ZZ" p)))
    (let ((found (find-all-symbols "PKT-ALPHA-ZZ")))
      (is-equal 1 (length found))
      (is (eq p (symbol-package (car found))))))
  (%pkt-cleanup "PKT-I"))

;;; ---- export, unexport, import, use-package conflicts ----------------------------------------------

(deftest export-unexport
  (%pkt-cleanup "PKT-E1" "PKT-E2")
  (let* ((p1 (make-package "PKT-E1" :use nil))
         (p2 (make-package "PKT-E2" :use (list p1)))
         (s (intern "SHARED" p1)))
    (is (null (%pkt-status "SHARED" p2)))
    (export s p1)
    (is (eq :external (%pkt-status "SHARED" p1)))
    (is (eq :inherited (%pkt-status "SHARED" p2)))
    (is (eq s (find-symbol "SHARED" p2)))
    (is (eq s (read-from-string "pkt-e1:shared")))
    (unexport s p1)
    (is (eq :internal (%pkt-status "SHARED" p1)))
    (is (null (%pkt-status "SHARED" p2)))
    (is-equal :other-error (%pkt-condition-type (let ((*package* p2)) (read-from-string "pkt-e1:shared"))))
    ;; exporting a symbol that clashes with a different one accessible in a using package is an error
    (let ((mine (intern "SHARED" p2)))
      (is-equal :package-error (%pkt-condition-type (export s p1)))
      (is (eq :internal (%pkt-status "SHARED" p1)))
      (is (eq mine (find-symbol "SHARED" p2))))
    ;; unexporting something that is only inherited is an error
    (is-equal :package-error (%pkt-condition-type (unexport 'car p2))))
  (%pkt-cleanup "PKT-E2" "PKT-E1"))

(deftest use-package-conflicts
  (%pkt-cleanup "PKT-U1" "PKT-U2")
  (let* ((p1 (make-package "PKT-U1" :use nil))
         (p2 (make-package "PKT-U2" :use nil)))
    (export (intern "CLASH" p1) p1)
    (intern "CLASH" p2)
    (is-equal :package-error (%pkt-condition-type (use-package p1 p2)))
    (is-equal nil (package-use-list p2))
    ;; shadowing resolves the conflict
    (shadow "CLASH" p2)
    (is-equal :no-error (%pkt-condition-type (use-package p1 p2)))
    (is-equal (list p1) (package-use-list p2))
    (is (not (eq (find-symbol "CLASH" p2) (find-symbol "CLASH" p1))))
    ;; using twice is harmless; unuse removes it from both lists
    (use-package p1 p2)
    (is-equal 1 (length (package-use-list p2)))
    (unuse-package p1 p2)
    (is-equal nil (package-use-list p2))
    (is-equal nil (package-used-by-list p1)))
  (%pkt-cleanup "PKT-U1" "PKT-U2"))

(deftest import-and-shadowing-import
  (%pkt-cleanup "PKT-M1" "PKT-M2")
  (let* ((p1 (make-package "PKT-M1" :use nil))
         (p2 (make-package "PKT-M2" :use nil))
         (a (intern "ITEM" p1)))
    (import a p2)
    (is (eq a (find-symbol "ITEM" p2)))
    (is (eq :internal (%pkt-status "ITEM" p2)))
    (is (eq p1 (symbol-package a)))             ; the home package does not change
    (import a p2)                               ; importing the same symbol again is fine
    ;; a different symbol of the same name conflicts ...
    (let ((b (make-symbol "OTHER")) (c (intern "OTHER" p2)))
      (declare (ignore c))
      (is-equal :package-error (%pkt-condition-type (import b p2)))
      ;; ... unless the import is a shadowing one, which displaces the old symbol
      (shadowing-import b p2)
      (is (eq b (find-symbol "OTHER" p2)))
      (is (member b (package-shadowing-symbols p2)))
      (is (eq p2 (symbol-package b)))))
  (%pkt-cleanup "PKT-M1" "PKT-M2"))

;;; ---- defpackage ----------------------------------------------------------------------------------------

(deftest defpackage-options
  (%pkt-cleanup "PKT-D" "PKT-D-SRC" "PKT-D-NICK")
  (let ((src (make-package "PKT-D-SRC" :use nil)))
    (export (intern "FROM-SRC" src) src)
    (intern "INTERNAL-SRC" src)
    (let ((p (eval '(defpackage :pkt-d
                      (:nicknames :pkt-d-nick)
                      (:use :common-lisp)
                      (:shadow #:car "CDR")
                      (:import-from :pkt-d-src #:from-src #:internal-src)
                      (:shadowing-import-from :pkt-d-src #:from-src)
                      (:intern #:private)
                      (:export #:public #:from-src)
                      (:documentation "a test package")
                      (:size 10)))))
      (is (packagep p))
      (is (eq p (find-package "PKT-D-NICK")))
      (is-equal (list (find-package "COMMON-LISP")) (package-use-list p))
      (is-equal 3 (length (package-shadowing-symbols p)))     ; CAR, CDR and the shadowing-imported FROM-SRC
      (is (not (eq 'car (find-symbol "CAR" p))))
      (is (eq (find-symbol "FROM-SRC" src) (find-symbol "FROM-SRC" p)))
      (is (eq (find-symbol "INTERNAL-SRC" src) (find-symbol "INTERNAL-SRC" p)))
      (is (eq :internal (%pkt-status "PRIVATE" p)))
      (is (eq :external (%pkt-status "PUBLIC" p)))
      (is (eq :external (%pkt-status "FROM-SRC" p)))
      ;; evaluating the same definition again changes nothing
      (let ((n (length (%package-symbols p nil))))
        (eval '(defpackage :pkt-d (:nicknames :pkt-d-nick) (:use :common-lisp) (:export #:public)))
        (is-equal n (length (%package-symbols p nil))))
      ;; a later defpackage adds to the package; the package is not recreated
      (is (eq p (eval '(defpackage :pkt-d (:export #:later)))))
      (is (eq :external (%pkt-status "LATER" p)))))
  ;; the default :use is COMMON-LISP; (:use) means none
  (is-equal (list (find-package "COMMON-LISP")) (package-use-list (eval '(defpackage :pkt-d-nick2))))
  (is-equal nil (package-use-list (eval '(defpackage :pkt-d-nick3 (:use)))))
  ;; errors: unknown option, missing symbol in :import-from
  (signals-error (macroexpand '(defpackage :pkt-bad (:bogus 1))))
  (is-equal :package-error (%pkt-condition-type (eval '(defpackage :pkt-d-bad (:use) (:import-from :pkt-d-src #:nonexistent)))))
  (%pkt-cleanup "PKT-D" "PKT-D-SRC" "PKT-D-NICK" "PKT-D-NICK2" "PKT-D-NICK3" "PKT-D-BAD"))

(deftest defpackage-lock-option
  (%pkt-cleanup "PKT-DL")
  (let ((p (eval '(defpackage :pkt-dl (:use) (:export #:x) (:lock t)))))
    (is (package-locked-p p))
    ;; re-evaluating an identical definition of a locked package is fine; changing it is not
    (is-equal :no-error (%pkt-condition-type (eval '(defpackage :pkt-dl (:use) (:export #:x)))))
    (is-equal :lock (%pkt-condition-type (eval '(defpackage :pkt-dl (:export #:y))))))
  (%pkt-cleanup "PKT-DL"))

(deftest in-package-unknown
  (is-equal :package-error (%pkt-condition-type (eval '(in-package :pkt-no-such-package))))
  (is (eq (find-package "COMMON-LISP")
          (let ((*package* (find-package "COMMON-LISP-USER"))) (eval '(in-package :cl)) *package*)))
  (is-equal :package-error (%pkt-condition-type (let ((*package* *package*)) (eval '(in-package :pkt-no-such-package))))))

;;; ---- iteration ------------------------------------------------------------------------------------

(deftest package-iteration
  (%pkt-cleanup "PKT-X1" "PKT-X2")
  (let* ((p1 (make-package "PKT-X1" :use nil))
         (p2 (make-package "PKT-X2" :use (list p1))))
    (export (intern "PKT-IT-E1" p1) p1)
    (intern "PKT-IT-I1" p1)
    (intern "PKT-IT-OWN" p2)
    (export (intern "PKT-IT-E2" p2) p2)
    (let ((acc nil) (ext nil))
      (do-symbols (s p2) (setq acc (cons (symbol-name s) acc)))
      (do-external-symbols (s p2) (setq ext (cons (symbol-name s) ext)))
      (is-equal '("PKT-IT-E1" "PKT-IT-E2" "PKT-IT-OWN") (sort (copy-list acc) #'string<))        ; I1 is not accessible
      (is-equal '("PKT-IT-E2") ext))
    (is-equal :done (do-symbols (s p1 :done) s))
    (is-equal "FOUND" (do-symbols (s p1) (when (equal (symbol-name s) "PKT-IT-I1") (return "FOUND"))))
    (let ((names nil))
      (do-all-symbols (s) (when (member (symbol-name s) '("PKT-IT-E1" "PKT-IT-OWN") :test #'string=) (setq names (cons (symbol-name s) names))))
      (is-equal '("PKT-IT-E1" "PKT-IT-OWN") (sort names #'string<)))
    ;; with-package-iterator reports the accessibility of each symbol
    (let ((seen nil))
      (with-package-iterator (next p2 :internal :external :inherited)
        (loop (multiple-value-bind (more sym access pkg) (next)
                (unless more (return))
                (setq seen (cons (list (symbol-name sym) access (package-name pkg)) seen)))))
      (is-equal '(("PKT-IT-E1" :inherited "PKT-X2") ("PKT-IT-E2" :external "PKT-X2") ("PKT-IT-OWN" :internal "PKT-X2"))
                (sort (copy-list seen) (lambda (a b) (string< (car a) (car b))))))
    (let ((only-external nil))
      (with-package-iterator (next p2 :external)
        (loop (multiple-value-bind (more sym) (next) (unless more (return)) (setq only-external (cons (symbol-name sym) only-external)))))
      (is-equal '("PKT-IT-E2") only-external)))
  (%pkt-cleanup "PKT-X2" "PKT-X1"))

;;; ---- package locks --------------------------------------------------------------------------------

(deftest package-locks-package-operations
  (%pkt-cleanup "PKT-L" "PKT-L-OTHER")
  (let* ((p (make-package "PKT-L" :use nil))
         (other (make-package "PKT-L-OTHER" :use nil))
         (existing (intern "EXISTING" p))
         (foreign (intern "FOREIGN" other)))
    (export existing p)
    (is (not (package-locked-p p)))
    (is (lock-package p))
    (is (package-locked-p p))
    ;; reading and finding existing symbols is fine; creating new ones is not
    (is (eq existing (intern "EXISTING" p)))
    (is (eq existing (read-from-string "pkt-l:existing")))
    (is-equal :lock (%pkt-condition-type (intern "BRAND-NEW" p)))
    (is (null (find-symbol "BRAND-NEW" p)))
    (is-equal :lock (%pkt-condition-type (let ((*package* p)) (read-from-string "another-new-symbol"))))
    ;; "(SETF x)" function-name symbols are bookkeeping and never trip a lock
    (is-equal :no-error (%pkt-condition-type (intern "(SETF EXISTING)" p)))
    ;; every modification is refused
    (is-equal :lock (%pkt-condition-type (import foreign p)))
    (is-equal :lock (%pkt-condition-type (shadowing-import foreign p)))
    (is-equal :lock (%pkt-condition-type (shadow "SOMETHING" p)))
    (is-equal :lock (%pkt-condition-type (unexport existing p)))
    (is-equal :lock (%pkt-condition-type (unintern existing p)))
    (is-equal :lock (%pkt-condition-type (use-package other p)))
    (is-equal :lock (%pkt-condition-type (rename-package p "PKT-L-RENAMED")))
    (is-equal :lock (%pkt-condition-type (delete-package p)))
    (is-equal :lock (%pkt-condition-type (export (intern "HIDDEN" p) p)))
    ;; a no-op is not a modification
    (is-equal :no-error (%pkt-condition-type (export existing p)))
    ;; the package is unchanged
    (is (eq :external (%pkt-status "EXISTING" p)))
    (is-equal "PKT-L" (package-name p))
    ;; locking affects only that package: others can still use it and import from it
    (is-equal :no-error (%pkt-condition-type (use-package p other)))
    (is-equal :no-error (%pkt-condition-type (import existing other)))
    (is-equal :no-error (%pkt-condition-type (intern "FREE" other)))
    ;; unlocking restores everything
    (is (unlock-package p))
    (is (not (package-locked-p p)))
    (is-equal :no-error (%pkt-condition-type (intern "BRAND-NEW" p)))
    (is-equal :no-error (%pkt-condition-type (unexport existing p))))
  (%pkt-cleanup "PKT-L" "PKT-L-OTHER"))

(deftest package-locks-symbol-definitions
  (%pkt-cleanup "PKT-LS")
  (let* ((p (make-package "PKT-LS" :use nil))
         (f (intern "F" p)) (m (intern "M" p)) (v (intern "V" p)) (k (intern "K" p)) (g (intern "G" p)))
    (eval `(defun ,f (x) x))
    (lock-package p)
    (is-equal :lock (%pkt-condition-type (eval `(defun ,f (x) (list x)))))
    (is-equal 5 (funcall f 5))                                 ; the old definition survives
    (is-equal :lock (%pkt-condition-type (eval `(defun ,g () 1))))
    (is-equal :lock (%pkt-condition-type (eval `(defgeneric ,g (x)))))
    (is-equal :lock (%pkt-condition-type (eval `(defmacro ,m () 1))))
    (is-equal :lock (%pkt-condition-type (eval `(defvar ,v 1))))
    (is-equal :lock (%pkt-condition-type (eval `(defparameter ,v 1))))
    (is-equal :lock (%pkt-condition-type (eval `(defconstant ,k 1))))
    (is-equal :lock (%pkt-condition-type (setf (symbol-function g) (lambda () 1))))
    (is-equal :lock (%pkt-condition-type (setf (macro-function g) (lambda (form env) (declare (ignore form env)) 1))))
    (is-equal :lock (%pkt-condition-type (fmakunbound f)))
    (is-equal :lock (%pkt-condition-type (eval `(defun (setf ,f) (new x) (declare (ignore x)) new))))
    (is (fboundp f))
    (is (not (boundp v)))
    (is (not (cl::%special-p v)))
    ;; ordinary uses of the symbols are unaffected: values, plists, function calls, local bindings
    (is-equal :no-error (%pkt-condition-type (progn (setf (get f 'color) :red) (setf (symbol-value v) 7))))
    (is-equal :red (get f 'color))
    (is-equal 7 (symbol-value v))
    (is-equal 3 (eval `(let ((,f 3)) ,f)))
    ;; the escape hatches
    (is-equal :no-error (%pkt-condition-type (without-package-locks (eval `(defun ,f (x) (list x))))))
    (is-equal '(5) (funcall f 5))
    (is-equal :no-error (%pkt-condition-type (with-unlocked-packages (p) (eval `(defvar ,v 1)))))
    (is (package-locked-p p))                                  ; and the lock is back afterwards
    (is-equal :lock (%pkt-condition-type (eval `(defun ,g () 1))))
    ;; the lock is restored even if the body exits non-locally
    (ignore-errors (with-unlocked-packages (p) (error "boom")))
    (is (package-locked-p p))
    ;; a package that was already unlocked stays unlocked
    (unlock-package p)
    (with-unlocked-packages (p) nil)
    (is (not (package-locked-p p))))
  (%pkt-cleanup "PKT-LS"))

(deftest package-locks-condition-class
  (%pkt-cleanup "PKT-LC")
  (let ((p (make-package "PKT-LC" :use nil)))
    (lock-package p)
    (let ((c (handler-case (intern "NOPE" p) (error (e) e))))
      (is (typep c 'package-lock-violation))
      (is (typep c 'package-error))
      (is (typep c 'simple-package-error))
      (is (search "locked package PKT-LC" (condition-report-string c)))
      (is (search "NOPE" (condition-report-string c))))
    (intern "(SETF NOPE)" p)
    (unlock-package p)
    (let ((s (intern "SYM" p)))
      (lock-package p)
      (let ((c (handler-case (eval `(defun ,s ())) (error (e) e))))
        (is (typep c 'package-lock-violation))
        (is (search "define a function" (condition-report-string c))))))
  (%pkt-cleanup "PKT-LC"))

(deftest package-locks-keyword-and-common-lisp
  ;; KEYWORD may always grow, even when locked
  (let ((kw (find-package "KEYWORD")))
    (is-equal :no-error (%pkt-condition-type (intern "PKT-FRESH-KEYWORD" kw)))
    (lock-package kw)
    (unwind-protect
         (is-equal :no-error (%pkt-condition-type (intern "PKT-FRESH-KEYWORD-2" kw)))
      (unlock-package kw)))
  ;; COMMON-LISP is not locked in the shipped image, but locking it works like any other package
  (is (not (package-locked-p "COMMON-LISP")))
  (unwind-protect
       (progn
         (lock-package "COMMON-LISP")
         (is-equal :lock (%pkt-condition-type (eval '(defun car (x) x))))
         (is-equal :lock (%pkt-condition-type (eval '(defmacro when (&rest r) r))))
         (is-equal :lock (%pkt-condition-type (intern "PKT-NOT-A-CL-SYMBOL" "COMMON-LISP")))
         (is-equal :lock (%pkt-condition-type (unexport 'car "COMMON-LISP")))
         (is-equal 1 (car '(1 2)))
         (is-equal :no-error (%pkt-condition-type (without-package-locks (intern "PKT-CL-TEMP" "COMMON-LISP")))))
    (unlock-package "COMMON-LISP")
    (let ((s (find-symbol "PKT-CL-TEMP" "COMMON-LISP")))
      (when s (unintern s "COMMON-LISP"))))
  (is (not (package-locked-p "COMMON-LISP")))
  (is-equal :no-error (%pkt-condition-type (intern "PKT-CL-TEMP-2" "COMMON-LISP")))
  (unintern (find-symbol "PKT-CL-TEMP-2" "COMMON-LISP") "COMMON-LISP"))

(deftest package-locks-type-definitions
  (%pkt-cleanup "PKT-LT")
  (let* ((p (make-package "PKT-LT" :use nil))
         (s (intern "S" p)) (c (intern "C" p)) (ty (intern "TY" p)) (cond-name (intern "CND" p))
         (free (intern "FREE" p)))
    (lock-package p)
    (is-equal :lock (%pkt-condition-type (eval `(defstruct ,s a b))))
    (is-equal :lock (%pkt-condition-type (eval `(defstruct (,s (:include ,free)) a))))
    (is-equal :lock (%pkt-condition-type (eval `(defclass ,c () ()))))
    (is-equal :lock (%pkt-condition-type (eval `(deftype ,ty () 'integer))))
    (is-equal :lock (%pkt-condition-type (eval `(define-condition ,cond-name (error) ()))))
    (is (null (find-class c nil)))
    (is (null (get ty '%deftype)))
    ;; a package-lock error is a condition whose report names the operation
    (let ((e (handler-case (eval `(defclass ,c () ())) (error (e) e))))
      (is (search "define a class named" (condition-report-string e))))
    (is-equal :no-error (%pkt-condition-type (without-package-locks (eval `(defstruct ,s a b)))))
    (is-equal :no-error (%pkt-condition-type (with-unlocked-packages (p) (eval `(defclass ,c () ())))))
    (is (find-class c nil))
    ;; types defined in other packages may use symbols of the locked one as slot names and supertypes
    (let ((o (make-package "PKT-LT-OTHER" :use nil)))
      (is-equal :no-error (%pkt-condition-type (eval `(defclass ,(intern "MINE" o) (,c) ((,s :initarg :s))))))
      (delete-package o)))
  (%pkt-cleanup "PKT-LT"))
