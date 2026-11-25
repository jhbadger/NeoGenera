;;;; 30-driver.lisp -- COMPILE, EVAL, COMPILE-FILE, fasl LOAD and definition forms.

(in-package :cl)

;;; ---- definition forms (macros used by the compiler) ----------------------------
;;; The stage-0 evaluator implements DEFUN & co. natively; these macros give the
;;; compiler the same forms.

(defun %split-body (body)
  ;; returns (values declarations docstring forms)
  (let ((decls nil) (doc nil))
    (block nil
      (tagbody top
         (cond ((and (consp body) (consp (car body)) (eq (caar body) 'declare))
                (setq decls (cons (car body) decls)) (setq body (cdr body)) (go top))
               ((and (consp body) (stringp (car body)) (cdr body) (null doc))
                (setq doc (car body)) (setq body (cdr body)) (go top)))))
    (values (nreverse decls) doc body)))

(defun %defun (name fn)
  (%set-function-name fn name)
  (%set-symbol-function (%fn-symbol name) fn)
  name)

(defvar *record-lambda-expressions* nil)
(defmacro defun (name ll &body body)
  (multiple-value-bind (decls doc forms) (%split-body body)
    (declare (ignore doc))
    (let ((def (list '%defun (list 'quote name)
                     (list 'function
                           (list* 'lambda ll
                                  (append decls
                                          (list (list* 'block (if (consp name) (cadr name) name) forms))))))))
      (if (and *record-lambda-expressions* (symbolp name))
          (list 'progn def (list '%put (list 'quote name) ''%lambda-expr (list 'quote (list* 'lambda ll body))) (list 'quote name))
          def))))

(defun %defmacro-install (name fn) (%set-macro-function name fn) name)

(defmacro defmacro (name ll &body body)
  (multiple-value-bind (decls doc forms) (%split-body body)
    (declare (ignore doc))
    (let* ((whole (gensym "WHOLE")) (env (gensym "ENV"))
           (env-var (let ((p (member '&environment ll))) (and p (cadr p))))
           (ll2 (let ((acc nil) (skip nil))
                  (dolist (x ll)
                    (cond (skip (setq skip nil))
                          ((eq x '&environment) (setq skip t))
                          (t (setq acc (cons x acc)))))
                  (nreverse acc)))
           (fn-form (list 'lambda (list whole env)
                          (list 'declare (list 'ignorable env))
                          (list* 'let (if env-var (list (list env-var nil)) nil)
                                 (list 'declare (list 'ignorable (or env-var env)))
                                 (list (list* 'destructuring-bind ll2 (list 'cdr whole)
                                              (append decls forms)))))))
      ;; Install at macroexpansion time so later forms in the same file can use
      ;; it.  The evaluator's native DEFMACRO is used (C destructuring) to avoid
      ;; a bootstrap cycle through DESTRUCTURING-BIND when redefining macros
      ;; such as DOLIST that the destructuring code itself uses.
      (%eval (copy-tree (list* 'defmacro name ll body)))
      (list 'progn (list '%put (list 'quote name) ''%macro-ll (list 'quote ll2))
            (list '%defmacro-install (list 'quote name) (list 'function fn-form))))))

(defun %defvar-init (name thunk has-init always)
  (%proclaim-special name)
  (when (and has-init (or always (not (boundp name))))
    (set name (funcall thunk)))
  name)

(defmacro defvar (name &optional (value nil has-value) doc)
  (declare (ignore doc))
  (%proclaim-special name)
  (list '%defvar-init (list 'quote name)
        (if has-value (list 'lambda nil value) nil) has-value nil))

(defmacro defparameter (name value &optional doc)
  (declare (ignore doc))
  (%proclaim-special name)
  (list '%defvar-init (list 'quote name) (list 'lambda nil value) t t))

(defun %defconstant (name value)
  (cond ((and (%constant-p name) (boundp name))
         (unless (eql (symbol-value name) value)
           (error "The constant ~S is already defined with a different value" name)))
        (t (set name value)
           (%proclaim-constant name)))
  name)

(defmacro defconstant (name value &optional doc)
  (declare (ignore doc))
  (%proclaim-special name)
  (list '%defconstant (list 'quote name) value))

(defmacro declaim (&rest specs)
  (%declaim specs)
  (list '%declaim (list 'quote specs)))

;;; ---- compile / eval --------------------------------------------------------------

(defun %compile-root (form-or-lambda lambda-p)
  ;; returns a code object
  (let* ((*lam* nil) (env (make-aenv)))
    (if lambda-p
        (gen-lambda (an-lambda nil (cadr form-or-lambda) (cddr form-or-lambda) env))
        (let ((root (make-clam nil nil)))
          (let ((*lam* root))
            (setf (clam-body root) (an form-or-lambda env)))
          (gen-lambda root)))))

(defun %compile-toplevel (form)
  (%code-closure (%compile-root form nil) nil))

(defun compiled-eval (form) (funcall (%compile-toplevel form)))

(defun compile (name &optional definition)
  (let* ((def (or definition (and name (symbol-function name))))
         (fn (cond ((and (consp def) (eq (car def) 'lambda))
                    (%code-closure (%compile-root def t) nil))
                   ((functionp def)
                    (let ((le (%function-lambda-expression def)))
                      (if le
                          (%code-closure (%compile-root (cons 'lambda le) t) nil)
                          def)))
                   (t (error "COMPILE: cannot compile ~S" def)))))
    (when name (%set-function-name fn name) (%set-symbol-function name fn))
    (values (or fn def) nil nil)))

(defun ng-use-compiler-eval ()
  (%set-symbol-function 'eval (symbol-function 'compiled-eval))
  t)

(defun ng-use-evaluator-eval ()
  (%set-symbol-function 'eval (symbol-function '%eval))
  t)

;;; ---- disassembler ---------------------------------------------------------------------

(defun disassemble (fn)
  (let* ((parts (%code-parts fn))
         (instrs (nth 4 parts)) (consts (nth 5 parts)) (pc 0))
    (format t "~&; code ~S  nreq=~D nopt=~D rest=~S~%" (nth 0 parts) (nth 1 parts) (nth 2 parts) (nth 3 parts))
    (block nil
      (tagbody top
         (when (null instrs) (return))
         (let* ((entry (find (car instrs) *opcodes* :key #'cadr)) (n (caddr entry)))
           (format t "~&~4D  ~A" pc (string-downcase (symbol-name (car entry))))
           (dotimes (i n) (format t " ~S" (nth (+ i 1) instrs)))
           (when (and (member (car entry) '(:const :gref :gset :gfun :bind)) (aref consts (cadr instrs)))
             (format t "    ; ~S" (aref consts (cadr instrs))))
           (terpri)
           (setq pc (+ pc 1 n))
           (setq instrs (nthcdr (+ 1 n) instrs)))
         (go top)))
    nil))

;;; ---- fasl files ------------------------------------------------------------------------

(defun %code->sexp (code)
  (let ((parts (%code-parts code)))
    (list :code (nth 0 parts) (nth 1 parts) (nth 2 parts) (nth 3 parts) (nth 4 parts)
          (mapcar (lambda (c)
                    (cond ((%codep c) (list :c (%code->sexp c)))
                          ((eq c (%unsupplied)) (list :u))
                          (t (list :k c))))
                  (%vector-to-list (nth 5 parts)))
          (nth 6 parts))))

(defun %sexp->code (s)
  (%make-code (nth 1 s) (nth 2 s) (nth 3 s) (nth 4 s) (nth 5 s)
              (%list-to-vector
               (mapcar (lambda (c)
                         (case (car c)
                           (:c (%sexp->code (cadr c)))
                           (:u (%unsupplied))
                           (t (cadr c))))
                       (nth 6 s)))
              (nth 7 s)))

(defvar *fasl-header* ";;; NeoGenera fasl 1")

(defun %write-fasl-entry (stream code)
  (write-string (%readable-to-string (%code->sexp code)) stream)
  (terpri stream))

(defun %toplevel-compile-time-p (form)
  (and (consp form)
       (member (car form) '(defstruct deftype defsetf in-package defconstant defmacro declaim proclaim defclass
                            define-condition defgeneric))))

;;; Definition source tracking: (%note-definition NAME KIND FILE POSITION) is recorded for every
;;; definition form loaded from a file or compiled by COMPILE-FILE (the note is part of the fasl).
(defvar *form-pos* 0)
(defvar *compile-file-name* nil)

(defun %definition-info (form)
  "If FORM is a definition, returns (values name kind)."
  (when (and (consp form) (symbolp (car form)) (consp (cdr form)))
    (let ((kind (case (car form)
                  (defun :function) (defmacro :macro) ((defvar defparameter defconstant) :variable)
                  (defstruct :structure) (defclass :class) (defgeneric :generic) (defmethod :method)
                  (define-condition :condition) (deftest :test) (define-command :command) (deftype :type)
                  (t nil)))
          (name (cadr form)))
      (when (and kind (consp name))
        (setq name (if (eq (car name) 'setf) name (car name))))
      (when (and kind name (or (symbolp name) (and (consp name) (eq (car name) 'setf))))
        (values (if (and (consp name) (eq (car name) 'setf)) (%fn-symbol name) name) kind)))))

(defun %note-definition (name kind file pos)
  (let ((entry (list kind file pos)))
    (%put name '%source
          (cons entry (remove-if (lambda (e) (and (eq (car e) kind) (equal (cadr e) file) (not (eq kind :method))))
                                 (get name '%source))))
    name))

(defun definition-locations (name)
  "Recorded source locations of NAME: a list of (kind file position)."
  (get (if (consp name) (%fn-symbol name) name) '%source))

(defun %process-toplevel (form stream)
  (cond ((and (consp form) (eq (car form) 'progn))
         (dolist (f (cdr form)) (%process-toplevel f stream)))
        ((and (consp form) (eq (car form) 'eval-when))
         (let ((sits (cadr form)))
           (when (memq :compile-toplevel sits) (dolist (f (cddr form)) (eval (copy-tree f))))
           (when (or (memq :load-toplevel sits) (memq :execute sits))
             (dolist (f (cddr form)) (%process-toplevel f stream)))))
        (t (when (%toplevel-compile-time-p form) (eval (copy-tree form)))
           (multiple-value-bind (name kind) (%definition-info form)
             (let ((expanded (macroexpand form)))
               (if (and (consp expanded) (eq (car expanded) 'progn) (not (eq expanded form)))
                   (%process-toplevel expanded stream)
                   (%write-fasl-entry stream (%compile-root expanded nil)))
               ;; the note is written for the ORIGINAL form: macros, structures, classes and generic
               ;; functions expand into PROGN and would otherwise lose their source location
               (when (and name *compile-file-name*)
                 (%write-fasl-entry stream
                                    (%compile-root (list '%note-definition (list 'quote name) kind
                                                         *compile-file-name* *form-pos*) nil))))))))

(defun compile-file (path &key output-file)
  (let* ((out (or output-file
                  (let ((p (%string-position-char (reverse path) #\. 0)))
                    (if p (%string-concat (subseq path 0 (- (length path) p 1)) ".nfasl") (%string-concat path ".nfasl")))))
         (*package* *package*) (*compile-file-name* path))
    (with-open-file (in path)
      (with-open-file (o out :direction :output)
        (write-line *fasl-header* o)
        (let ((eof (list nil)))
          (block nil
            (tagbody top
               (let ((form (read in nil eof)))
                 (when (eq form eof) (return))
                 (let ((*form-pos* (%last-read-start)))
                   (%process-toplevel form o)))
               (go top))))))
    out))

(defun load-fasl (path)
  (let ((*package* *package*))
    (with-open-file (s path)
      (let ((eof (list nil)) (header (read-line s nil nil)))
        (unless (and header (%starts-with header *fasl-header*))
          (error "Not a NeoGenera fasl file: ~A" path))
        (block nil
          (tagbody top
             (let ((form (read s nil eof)))
               (when (eq form eof) (return))
               (funcall (%code-closure (%sexp->code form) nil)))
             (go top))))))
  t)

(defun %ends-with (s suffix)
  (let ((n (length suffix)) (m (length s)))
    (and (>= m n) (string= (subseq s (- m n)) suffix))))

(defun load (path &key verbose print if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (if (%ends-with path ".nfasl")
      (load-fasl path)
      (let ((*package* *package*) (*load-pathname* path) (*compile-file-name* path))
        (with-open-file (s path)
          (let ((eof (list nil)))
            (block nil
              (tagbody top
                 (let ((form (read s nil eof)))
                   (when (eq form eof) (return))
                   (let ((pos (%last-read-start)))
                     (let ((v (eval form)))
                       (multiple-value-bind (name kind) (%definition-info form)
                         (when name (%note-definition name kind path pos)))
                       (when print (format t "~&; ~S~%" v)))))
                 (go top)))))
        (when verbose (format *error-output* "~&; loaded ~A~%" path))
        t)))
