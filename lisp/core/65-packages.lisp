;;;; 65-packages.lisp -- the standard package API (MAKE-PACKAGE, DEFPACKAGE, SHADOW, DO-SYMBOLS, ...)
;;;; and package locks.  The primitives (FIND-PACKAGE, INTERN, EXPORT, IMPORT, USE-PACKAGE, UNUSE-PACKAGE,
;;;; UNEXPORT, UNINTERN, SHADOWING-IMPORT and the %-prefixed helpers used below) live in src/symbols.c and
;;;; src/prims_core.c; this file is the layer that accepts string designators and implements the macros.
;;;;
;;;; Package locks follow SBCL's model.  A locked package may not be modified (interning a new symbol, import,
;;;; export, unexport, unintern, shadow, use, unuse, rename, delete), and its symbols may not be defined as
;;;; functions, macros, special variables or constants.  COMMON-LISP is locked when the system image is built.
;;;; The violation is a PACKAGE-LOCK-VIOLATION error; WITHOUT-PACKAGE-LOCKS and WITH-UNLOCKED-PACKAGES are the
;;;; escape hatches.

(in-package :cl)

(defun %package-arg (designator)
  "The package named by DESIGNATOR (a package, string or symbol); a PACKAGE-ERROR if there is none."
  (or (and (packagep designator) designator)
      (find-package designator)
      (error 'simple-package-error :package nil :format-control "There is no package named ~S"
                                   :format-arguments (list designator))))

(defun %names (x)
  "A list of strings from a string designator or a list of them."
  (mapcar #'string (if (listp x) x (list x))))

;;; ---- MAKE-PACKAGE, RENAME-PACKAGE, DELETE-PACKAGE -------------------------------------------------

(defun make-package (name &key nicknames (use nil use-p))
  "Create a package called NAME (a string designator) with the given NICKNAMES.  USE is a list of packages to
use; it defaults to COMMON-LISP.  A PACKAGE-ERROR is signalled if the name or a nickname is taken."
  (%make-package (string name) (%names nicknames) (if use-p use (list "COMMON-LISP"))))

(defun rename-package (package new-name &optional new-nicknames)
  "Change the name and nicknames of PACKAGE.  Returns the package."
  (%rename-package (%package-arg package) (string new-name) (%names new-nicknames)))

(defun delete-package (package)
  "Delete PACKAGE: its name is released and its symbols lose their home package.  If other packages use it, a
correctable PACKAGE-ERROR is signalled (the CONTINUE restart unuses it first).  Returns true, or NIL if the
package was already deleted."
  (let ((p (%package-arg package)))
    (when (and (package-name p) (package-used-by-list p))
      (restart-case
          (error 'simple-package-error :package p :format-control "Package ~A is used by ~{~A~^, ~}"
                                       :format-arguments (list (package-name p) (mapcar #'package-name (package-used-by-list p))))
        (continue () :report "Unuse the package in its users, then delete it"
          (dolist (u (package-used-by-list p)) (unuse-package p u)))))
    (%delete-package p)))

(defun shadow (symbol-names &optional (package *package*))
  "Make symbols named SYMBOL-NAMES (a string designator or a list of them) present in PACKAGE and shadowing
any inherited symbols of those names.  Returns true."
  (let ((p (%package-arg package)))
    (dolist (n (%names symbol-names)) (%shadow n p))
    t))

(defun find-all-symbols (name)
  "A list of all symbols named NAME (a string designator) that are present in some package."
  (let ((n (string name)) (found nil))
    (dolist (p (list-all-packages) found)
      (multiple-value-bind (s status) (find-symbol n p)
        (when (and (member status '(:internal :external)) (not (member s found)))
          (setq found (cons s found)))))))

;;; ---- iteration --------------------------------------------------------------------------------------

(defun %accessible-symbols (package)
  "Every symbol accessible in PACKAGE: those present in it and the external symbols of the packages it uses
(minus the ones that a present symbol hides)."
  (let ((p (%package-arg package)) (seen (make-hash-table :test 'eq)) (acc nil))
    (dolist (s (%package-symbols p nil)) (setf (gethash s seen) t) (setq acc (cons s acc)))
    (dolist (u (package-use-list p))
      (dolist (s (%package-symbols u t))
        (when (and (not (gethash s seen)) (eq (find-symbol (symbol-name s) p) s))
          (setf (gethash s seen) t)
          (setq acc (cons s acc)))))
    acc))

(defmacro do-symbols ((var &optional package result-form) &body body)
  "Run BODY with VAR bound to each symbol accessible in PACKAGE (default: the current package), then return the
value of RESULT-FORM.  `return` leaves the loop."
  `(dolist (,var (%accessible-symbols ,(or package '*package*)) ,result-form) ,@body))

(defmacro do-external-symbols ((var &optional package result-form) &body body)
  "Like `do-symbols`, but only over the external symbols of PACKAGE."
  `(dolist (,var (%package-symbols (%package-arg ,(or package '*package*)) t) ,result-form) ,@body))

(defmacro do-all-symbols ((var &optional result-form) &body body)
  "Like `do-symbols`, over the symbols present in every package.  A symbol present in several packages may be
visited more than once."
  `(dolist (,var (let ((all nil)) (dolist (p (list-all-packages) all) (setq all (append (%package-symbols p nil) all)))) ,result-form)
     ,@body))

(defun %package-iterator-list (packages types)
  (let ((acc nil))
    (dolist (p (if (listp packages) packages (list packages)) acc)
      (let ((pkg (%package-arg p)))
        (dolist (s (%accessible-symbols pkg))
          (multiple-value-bind (found status) (find-symbol (symbol-name s) pkg)
            (declare (ignore found))
            (when (member status types) (setq acc (cons (list s status pkg) acc)))))))))

(defmacro %package-iterator-next (var)
  `(let ((%entry (pop ,var))) (if %entry (values t (car %entry) (cadr %entry) (caddr %entry)) nil)))

(defmacro with-package-iterator ((name package-list &rest types) &body body)
  "Within BODY, calling (NAME) returns four values: true, a symbol, how it is accessible (:internal,
:external or :inherited) and its package; when the symbols run out it returns NIL.  TYPES select which kinds of
accessibility to visit."
  (let ((rest (gensym "SYMBOLS")))
    `(let ((,rest (%package-iterator-list ,package-list ',types)))
       (macrolet ((,name () (list '%package-iterator-next ',rest)))
         ,@body))))

;;; ---- DEFPACKAGE -------------------------------------------------------------------------------------

(defun %defpackage-symbols (package names source)
  "The symbols named NAMES as found in the package SOURCE; a PACKAGE-ERROR if one is missing."
  (let ((src (%package-arg source)))
    (mapcar (lambda (n)
              (multiple-value-bind (s status) (find-symbol n src)
                (unless status
                  (error 'simple-package-error :package package :format-control "Symbol ~A not found in package ~A"
                                               :format-arguments (list n (package-name src))))
                s))
            names)))

(defun %defpackage (name nicknames use shadows shadowing-imports imports interns exports lock)
  (let* ((existing (find-package name))
         (pkg (or existing (%make-package name nil nil))))
    ;; an existing package keeps what it has; only missing nicknames are added (a no-op leaves a locked package alone)
    (let ((missing (remove-if (lambda (n) (member n (package-nicknames pkg) :test #'string=)) nicknames)))
      (when missing (%rename-package pkg (package-name pkg) (append (package-nicknames pkg) missing))))
    (shadow shadows pkg)
    (dolist (spec shadowing-imports)
      (shadowing-import (%defpackage-symbols pkg (cdr spec) (car spec)) pkg))
    (dolist (u use) (use-package (%package-arg u) pkg))
    (dolist (spec imports)
      (import (%defpackage-symbols pkg (cdr spec) (car spec)) pkg))
    (dolist (n interns) (intern n pkg))
    (dolist (n exports) (export (intern n pkg) pkg))
    (when lock (lock-package pkg))
    pkg))

(defmacro defpackage (name &rest options)
  "Define (or update) the package NAME.  Options: (:nicknames name...), (:use package...), (:shadow name...),
(:shadowing-import-from package name...), (:import-from package name...), (:export name...), (:intern name...),
(:documentation string), (:size n) and (:lock bool).  The default for :use is COMMON-LISP.  Returns the package."
  (let ((nicknames nil) (use '("COMMON-LISP")) (use-given nil) (shadows nil) (shadowing-imports nil) (imports nil)
        (interns nil) (exports nil) (lock nil))
    (dolist (o options)
      (unless (consp o) (error "Malformed DEFPACKAGE option ~S" o))
      (case (car o)
        (:nicknames (setq nicknames (append nicknames (%names (cdr o)))))
        (:use (unless use-given (setq use nil use-given t))
              (setq use (append use (%names (cdr o)))))
        (:shadow (setq shadows (append shadows (%names (cdr o)))))
        (:shadowing-import-from (setq shadowing-imports (append shadowing-imports (list (cons (string (cadr o)) (%names (cddr o)))))))
        (:import-from (setq imports (append imports (list (cons (string (cadr o)) (%names (cddr o)))))))
        (:export (setq exports (append exports (%names (cdr o)))))
        (:intern (setq interns (append interns (%names (cdr o)))))
        (:lock (setq lock (cadr o)))
        ((:documentation :size) nil)
        (t (error "Unknown DEFPACKAGE option ~S" (car o)))))
    `(%defpackage ,(string name) ',nicknames ',use ',shadows ',shadowing-imports ',imports ',interns ',exports ',lock)))

;;; ---- package locks ----------------------------------------------------------------------------------

(defun lock-package (package)
  "Lock PACKAGE: it can no longer be modified, and its symbols cannot be defined as functions, macros, special
variables or constants, except under `without-package-locks`.  Returns true."
  (%set-package-lock (%package-arg package) t)
  t)

(defun unlock-package (package)
  "Unlock PACKAGE.  Returns true."
  (%set-package-lock (%package-arg package) nil)
  t)

(defmacro without-package-locks (&body body)
  "Run BODY with every package lock ignored."
  `(let ((*package-locks-disabled* t)) ,@body))

(defun %call-with-unlocked-packages (packages thunk)
  (let ((saved nil))
    (unwind-protect
         (progn (dolist (p packages)
                  (let ((pkg (%package-arg p)))
                    (setq saved (cons (cons pkg (%set-package-lock pkg nil)) saved))))
                (funcall thunk))
      (dolist (s saved) (%set-package-lock (car s) (cdr s))))))

(defmacro with-unlocked-packages ((&rest packages) &body body)
  "Run BODY with the listed packages unlocked; their previous lock state is restored afterwards."
  `(%call-with-unlocked-packages (list ,@packages) (lambda () ,@body)))
