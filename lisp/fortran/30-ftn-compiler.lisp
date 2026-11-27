;;;; 30-compiler.lisp -- the FORTRAN Tool Kit: types, scope and expressions.
;;;;
;;;; Each program unit becomes Lisp forms, compiled by the system compiler like any Lisp:
;;;;   * a main program P becomes an F77PROGRAM (run by F77:EXECUTE);
;;;;   * a SUBROUTINE or FUNCTION F becomes a global function F in the program's package, so recompiling one routine
;;;;     replaces it everywhere (the editor's incremental compilation), and Lisp can call it;
;;;;   * a BLOCK DATA unit becomes a thunk that initialises COMMON storage.
;;;; Every variable is an entry of a vector (see 20-runtime.lisp).  Arguments are passed as references (VECTOR . OFFSET).
;;;; This file holds the compile-time symbol table, implicit typing, storage layout (COMMON and EQUIVALENCE),
;;;; expression compilation with FORTRAN's type rules, and the intrinsic functions.  Statements are in 35-statements.lisp.

(in-package :cl)

;;; ---- compile-time state --------------------------------------------------------------------------------------

(defvar *fc-package* nil "The package FORTRAN names are interned in.")
(defvar *fc-unit* nil "The CUNIT being compiled.")
(defvar *fc-errors* nil "Compile errors collected so far.")
(defvar *fc-node* nil "The statement (FNODE) being compiled, for error positions.")
(defvar *fc-file* nil)
(defvar *fc-temp* 0)
(defvar *fc-scope* nil "An alist (name . FVAR) of statement-function parameters, searched before the unit's variables.")
(defvar *fc-const-mode* nil "True while compiling a constant expression: only PARAMETER names are allowed.")
(defvar *fc-const-env* nil "An alist (name . value) of loop variables known to constant expressions (DATA implied DOs).")
(defvar *fc-zero* nil)
(defvar *f77-signatures* (make-hash-table :test 'equal)
  "For each routine compiled, \"PACKAGE:NAME\" -> (kind result-kind params), PARAMS a list of (kind . array-p) or :STAR.")

(defstruct (cunit (:conc-name cu-) (:copier nil))
  kind name params type (vars (make-hash-table :test 'equal)) (implicit (make-array 26 :initial-element nil)) implicit-none
  (formats nil) (labels nil) (commons nil) (equivs nil) (nn 0) (nc 0) (zspecs nil) (datas nil) (assigned-labels nil)
  (dim-binds nil) (exec-seen nil) (result nil) key (init-forms nil) star-params (stmtfuns nil) (intrinsics nil) (common-decls nil))

(defstruct (fvar (:conc-name fv-) (:copier nil))
  name kind len dims class value space vec off params body declared used (strides nil) common star-index)

(defun %fc-error (fmt &rest args)
  (let ((n *fc-node*))
    (error 'f77-syntax-error :message (apply #'format nil fmt args)
                             :start (and n (fn-start n)) :end (and n (fn-end n)) :line (and n (fn-line n)) :col 7 :file *fc-file*)))

(defmacro %fc-collecting (&body body)
  "Run BODY; a compile error is recorded and BODY's value is NIL."
  `(handler-case (progn ,@body)
     (f77-syntax-error (c) (push c *fc-errors*) nil)))

(defun %fc-sym (fmt &rest args)
  "A symbol in the target package named by FORMAT of FMT and ARGS."
  (intern (apply #'format nil fmt args) *fc-package*))

(defun %fc-temp (base)
  (setq *fc-temp* (+ *fc-temp* 1))
  (%fc-sym "%~A~D" base *fc-temp*))

(defun %fc-routine-sym (name &optional (package *fc-package*))
  "The symbol of the routine NAME in PACKAGE, never an inherited Lisp symbol."
  (let ((s (intern name package)))
    (if (eq (symbol-package s) package) s (intern (%string-concat name "%") package))))

(defun %fc-label-tag (n) (%fc-sym "%L~D" n))

;;; ---- types ----------------------------------------------------------------------------------------------------

(defun %fc-words (kind len)
  "Entries a variable of KIND occupies: numbers in the number space, characters in the character space."
  (case kind
    ((:integer :real :logical) 1)
    ((:double :complex) 2)
    (:dcomplex 4)
    (:character (if (integerp len) (max len 1) 1))
    (t 1)))

(defun %fc-space (kind) (if (eq kind :character) :chr :num))
(defun %fc-numeric-p (kind) (member kind '(:integer :real :double :complex :dcomplex)))
(defun %fc-complex-p (kind) (member kind '(:complex :dcomplex)))
(defun %fc-rank (kind) (case kind ((:integer :logical) 1) (:real 2) (:double 3) (:complex 4) (:dcomplex 5) (t 0)))
(defun %fc-kind-name (kind)
  (case kind (:integer "INTEGER") (:real "REAL") (:double "DOUBLE PRECISION") (:complex "COMPLEX") (:dcomplex "DOUBLE COMPLEX")
        (:logical "LOGICAL") (:character "CHARACTER") (t (string-upcase (princ-to-string kind)))))

(defun %fc-a-kind (kind)
  "KIND's name with its indefinite article: an INTEGER, a REAL."
  (let ((n (%fc-kind-name kind))) (%string-concat (if (member (char n 0) '(#\A #\E #\I #\O #\U)) "an " "a ") n)))

(defun %fc-max-kind (a b)
  (let ((a (if (eq a :logical) :integer a)) (b (if (eq b :logical) :integer b)))
    (cond ((eq a b) a)
          ((and (%fc-complex-p a) (member b '(:double :dcomplex))) :dcomplex)
          ((and (%fc-complex-p b) (member a '(:double :dcomplex))) :dcomplex)
          ((>= (%fc-rank a) (%fc-rank b)) a)
          (t b))))

(defun %fc-coerce (form from to)
  "FORM, of kind FROM, converted to kind TO as FORTRAN's assignment conversions do."
  (cond
    ((eq from to) form)
    ((and (eq from :character) (not (eq to :character))) (%fc-error "A CHARACTER value cannot be converted to ~A" (%fc-a-kind to)))
    ((and (eq to :character) (not (eq from :character))) (%fc-error "~@(~A~) value cannot be converted to CHARACTER" (%fc-a-kind from)))
    ((eq from :logical) (%fc-coerce `(%f7-ltoi ,form) :integer to))
    (t
     (ecase to
       (:integer (case from
                   ((:real :double) `(values (truncate ,form)))
                   ((:complex :dcomplex) `(values (truncate (realpart ,form))))
                   (t form)))
       (:real (case from
                (:integer `(%f7-single (float ,form 1d0)))
                (:double `(%f7-single ,form))
                ((:complex :dcomplex) `(%f7-single (realpart ,form)))
                (t form)))
       (:double (case from
                  (:integer `(float ,form 1d0))
                  (:real form)
                  ((:complex :dcomplex) `(realpart ,form))
                  (t form)))
       (:complex (case from
                   ((:integer :real :double :dcomplex) `(%f7-cxs ,form))
                   (t form)))
       (:dcomplex (case from
                    ((:integer :real :double :complex) `(%f7-cx ,form))
                    (t form)))
       (:logical (case from
                   ((:integer) `(/= ,form 0))
                   (t (%fc-error "~@(~A~) value cannot be converted to LOGICAL" (%fc-a-kind from)))))))))

(defun %fc-convert-constant (value kind len)
  "The compile-time VALUE (a Lisp number, string or boolean) converted to KIND for a variable of LEN characters."
  (case kind
    (:integer (cond ((integerp value) value) ((floatp value) (values (truncate value))) ((complexp value) (values (truncate (realpart value))))
                    ((eq value t) 1) ((null value) 0) (t (%fc-error "~S is not an INTEGER value" value))))
    (:real (cond ((realp value) (%f7-single value)) ((complexp value) (%f7-single (realpart value))) (t (%fc-error "~S is not a REAL value" value))))
    (:double (cond ((realp value) (float value 1d0)) ((complexp value) (realpart value)) (t (%fc-error "~S is not a DOUBLE PRECISION value" value))))
    (:complex (cond ((numberp value) (%f7-cxs value)) (t (%fc-error "~S is not a COMPLEX value" value))))
    (:dcomplex (cond ((numberp value) (%f7-cx value)) (t (%fc-error "~S is not a COMPLEX value" value))))
    (:logical (cond ((or (eq value t) (null value)) value) ((integerp value) (/= value 0)) (t (%fc-error "~S is not a LOGICAL value" value))))
    (:character (cond ((stringp value) (if (integerp len) (%f7-cfit value len) value)) (t (%fc-error "~S is not a CHARACTER value" value))))
    (t value)))

;;; ---- implicit typing and the symbol table -----------------------------------------------------------------------

(defun %fc-implicit (name)
  "(kind . len) of the undeclared NAME by the unit's IMPLICIT rules."
  (let* ((c (char-upcase (char name 0))) (i (- (char-code c) 65)))
    (or (and (<= 0 i 25) (svref (cu-implicit *fc-unit*) i))
        (if (and (char>= c #\I) (char<= c #\N)) (cons :integer nil) (cons :real nil)))))

(defun %fc-find (name)
  (or (cdr (assoc name *fc-scope* :test #'string=))
      (gethash name (cu-vars *fc-unit*))))

(defun %fc-create (name &key class kind len)
  (when (and (cu-implicit-none *fc-unit*) (not kind))
    (%fc-error "The name ~A is not declared (the unit says IMPLICIT NONE)" name))
  (let* ((ty (%fc-implicit name))
         (v (make-fvar :name name :kind (or kind (car ty)) :len (or len (cdr ty)) :class (or class :local))))
    (setf (gethash name (cu-vars *fc-unit*)) v)
    v))

(defun %fc-var (name)
  (or (%fc-find name) (%fc-create name)))

;;; ---- constants --------------------------------------------------------------------------------------------------

(defun %fc-const (node)
  "The value of the constant expression NODE (literals, PARAMETER names, operators, intrinsics)."
  (let ((*fc-const-mode* t))
    (multiple-value-bind (form kind len) (%fc-expr node)
      (declare (ignore len))
      (values (handler-case (eval (copy-tree form))
                (f77-syntax-error (c) (error c))
                (error (e) (%fc-error "A constant expression cannot be evaluated: ~A" e)))
              kind))))

(defun %fc-const-int (node what)
  (let ((v (handler-case (%fc-const node)
             (f77-syntax-error (c) (error c))
             (error () (%fc-error "~A must be a constant expression" what)))))
    (unless (integerp v) (%fc-error "~A must be an INTEGER constant" what))
    v))

;;; ---- arrays: dimensions, strides ----------------------------------------------------------------------------------

(defun %fc-bound-form (node what v k tag)
  "A form for a dimension bound: an integer, or a symbol bound in the prologue when the bound is not constant."
  (if (null node)
      1
      (let ((c (handler-case (let ((x (%fc-const node))) (and (integerp x) x)) (error () nil))))
        (or c
            (let ((s (%fc-sym "~A%~A~D" (fv-name v) tag k)))
              (push (list s node what v) (cu-dim-binds *fc-unit*))
              s)))))

(defun %fc-prepare-array (v)
  "Make V's dimension list (lo hi) of forms and its stride list; idempotent.  FV-DIMS holds AST dims until then."
  (when (and (fv-dims v) (not (fv-strides v)))
    (let ((dims nil) (k 0) (stride 1) (strides nil) (rank (length (fv-dims v))))
      (dolist (d (fv-dims v))
        (setq k (+ k 1))
        (let* ((lo (%fc-bound-form (car d) "An array bound" v k "L"))
               (hi (if (eq (cadr d) :star)
                       (if (= k rank) :star (%fc-error "Only the last dimension of ~A can be assumed-size (*)" (fv-name v)))
                       (%fc-bound-form (cadr d) "An array bound" v k "H"))))
          (push stride strides)
          (push (list lo hi) dims)
          (setq stride (cond ((eq hi :star) 0)
                             ((and (integerp stride) (integerp lo) (integerp hi)) (* stride (max 0 (+ (- hi lo) 1))))
                             (t (let ((s (%fc-sym "~A%S~D" (fv-name v) (+ k 1))))
                                  (push (list s `(* ,stride (max 0 (+ (- ,hi ,lo) 1))) :stride v) (cu-dim-binds *fc-unit*))
                                  s))))))
      (setf (fv-dims v) (nreverse dims) (fv-strides v) (nreverse strides)))))

(defun %fc-array-p (v) (and (fv-dims v) t))

(defun %fc-array-size (v)
  "The number of elements of V as a form (an integer when its bounds are constant)."
  (%fc-prepare-array v)
  (let ((size 1))
    (dolist (d (fv-dims v) size)
      (when (eq (cadr d) :star) (%fc-error "The assumed-size array ~A cannot be used as a whole here" (fv-name v)))
      (let ((n (if (and (integerp (car d)) (integerp (cadr d))) (max 0 (+ (- (cadr d) (car d)) 1)) `(max 0 (+ (- ,(cadr d) ,(car d)) 1)))))
        (setq size (if (and (integerp size) (integerp n)) (* size n) `(* ,size ,n)))))))

(defvar *f77-maximum-array-size* 268435456 "The most elements the compiler accepts in a local or COMMON array.")

(defun %fc-const-array-size (v)
  (let ((n (%fc-array-size v)))
    (unless (integerp n) (%fc-error "The array ~A needs constant bounds (it is not a dummy argument)" (fv-name v)))
    (when (> n *f77-maximum-array-size*) (%fc-error "The array ~A is too large (~D elements)" (fv-name v) n))
    n))

(defun %fc-subscript-offset (v subs)
  "Form for the zero-based offset of the element of V with subscript expressions SUBS."
  (%fc-prepare-array v)
  (unless (= (length subs) (length (fv-dims v)))
    (%fc-error "The array ~A has ~D dimension~:P but is given ~D subscript~:P" (fv-name v) (length (fv-dims v)) (length subs)))
  (let ((terms nil) (name (string-downcase (fv-name v))))
    (do ((ds (fv-dims v) (cdr ds)) (ss (fv-strides v) (cdr ss)) (xs subs (cdr xs))) ((null ds))
      (multiple-value-bind (f k) (%fc-expr (car xs))
        (unless (member k '(:integer :logical)) (%fc-error "A subscript of ~A must be an INTEGER expression, not ~A" (fv-name v) (%fc-kind-name k)))
        (let* ((d (car ds)) (ix `(%f7-ix ,(%fc-coerce f k :integer) ,(car d) ,(if (eq (cadr d) :star) nil (cadr d)) ,name))
               (stride (car ss)))
          (push (if (eql stride 1) ix `(* ,stride ,ix)) terms))))
    (let* ((sum (if (cdr terms) `(+ ,@(nreverse terms)) (car terms)))
           (words (%fc-words (fv-kind v) (fv-len v))))
      ;; offsets are in entries: DOUBLE PRECISION and COMPLEX elements take two, CHARACTER*n elements n
      (if (= words 1) sum `(* ,words ,sum)))))

;;; ---- storage -------------------------------------------------------------------------------------------------------

(defun %fc-dummy-p (v) (member (fv-class v) '(:dummy :procdummy)))

(defun %fc-alloc-local (v)
  "Give the local variable V (or its EQUIVALENCE group) static storage."
  (let* ((u *fc-unit*) (kind (fv-kind v)) (space (%fc-space kind)) (words (%fc-words kind (fv-len v)))
         (count (if (fv-dims v) (%fc-const-array-size v) 1)))
    (when (and (eq kind :character) (not (integerp (fv-len v)))) (%fc-error "The character variable ~A needs a constant length" (fv-name v)))
    (setf (fv-space v) space)
    (if (eq space :chr)
        (progn (setf (fv-vec v) (%fc-sym "%CV") (fv-off v) (cu-nc u))
               (push (list :chr (cu-nc u) count (fv-len v) words) (cu-zspecs u))
               (setf (cu-nc u) (+ (cu-nc u) (* count words))))
        (progn (setf (fv-vec v) (%fc-sym "%NV") (fv-off v) (cu-nn u))
               (push (list :num (cu-nn u) count kind words) (cu-zspecs u))
               (setf (cu-nn u) (+ (cu-nn u) (* count words)))))))

(defun %fc-storage (v)
  "Ensure V has storage: (values vector-form offset-form)."
  (case (fv-class v)
    ((:dummy :procdummy)
     (unless (fv-vec v)
       (setf (fv-vec v) (%fc-sym "~A%V" (fv-name v)) (fv-off v) (%fc-sym "~A%I" (fv-name v)) (fv-space v) (%fc-space (fv-kind v)))))
    ((:param) nil)
    (t (unless (fv-vec v) (%fc-alloc-local v))))
  (values (fv-vec v) (fv-off v)))

;;; ---- COMMON and EQUIVALENCE -----------------------------------------------------------------------------------------

(defstruct (ccommon (:conc-name cm-) (:copier nil)) name (nn 0) (nc 0) vars sym-n sym-c (zspecs nil))

(defun %fc-layout-common (name vars)
  "Assign the variables VARS (fvars, in order) offsets in the COMMON block NAME."
  (let* ((u *fc-unit*) (cb (or (find name (cu-commons u) :key #'cm-name :test #'string=)
                               (let ((new (make-ccommon :name name :sym-n (%fc-sym "%CM~D-N" (length (cu-commons u)))
                                                       :sym-c (%fc-sym "%CM~D-C" (length (cu-commons u))))))
                                 (setf (cu-commons u) (append (cu-commons u) (list new)))
                                 new))))
    (dolist (v vars)
      (when (member (fv-class v) '(:dummy :procdummy :param)) (%fc-error "The ~A ~A cannot be in a COMMON block" (if (eq (fv-class v) :param) "constant" "argument") (fv-name v)))
      (when (fv-vec v) (%fc-error "The variable ~A is already in COMMON or has storage" (fv-name v)))
      (let* ((kind (fv-kind v)) (words (%fc-words kind (fv-len v))) (count (if (fv-dims v) (%fc-const-array-size v) 1)))
        (setf (fv-class v) :common (fv-common v) name (fv-space v) (%fc-space kind))
        (if (eq (fv-space v) :chr)
            (progn (setf (fv-vec v) (cm-sym-c cb) (fv-off v) (cm-nc cb))
                   (push (list :chr (cm-nc cb) count (fv-len v) words) (cm-zspecs cb))
                   (setf (cm-nc cb) (+ (cm-nc cb) (* count words))))
            (progn (setf (fv-vec v) (cm-sym-n cb) (fv-off v) (cm-nn cb))
                   (push (list :num (cm-nn cb) count kind words) (cm-zspecs cb))
                   (setf (cm-nn cb) (+ (cm-nn cb) (* count words)))))
        (push v (cm-vars cb))))))

(defun %fc-equiv-member (node)
  "(values fvar word-offset) for an EQUIVALENCE member: a name, or an array element with constant subscripts."
  (ecase (car node)
    (:name (values (%fc-var (cadr node)) 0))
    (:ref (let* ((v (%fc-var (cadr node))) (subs (caddr node)))
            (unless (fv-dims v) (%fc-error "~A in EQUIVALENCE is not an array" (fv-name v)))
            (%fc-prepare-array v)
            (let ((off 0))
              (do ((ds (fv-dims v) (cdr ds)) (ss (fv-strides v) (cdr ss)) (xs subs (cdr xs))) ((or (null ds) (null xs)))
                (let* ((i (%fc-const-int (car xs) "An EQUIVALENCE subscript")) (lo (car (car ds))))
                  (unless (and (integerp lo) (integerp (car ss))) (%fc-error "EQUIVALENCE needs constant array bounds"))
                  (setq off (+ off (* (- i lo) (car ss))))))
              (values v (* off (%fc-words (fv-kind v) (fv-len v)))))))
    (:substr (%fc-error "Substrings are not supported in EQUIVALENCE"))))

(defun %fc-layout-equivalences ()
  (let ((u *fc-unit*) (groups nil))
    ;; each group: list of (fvar . start) with start relative to the group origin
    (dolist (g (cu-equivs u))
      (let ((members nil))
        (dolist (m g)
          (multiple-value-bind (v off) (%fc-equiv-member m)
            (when (member (fv-class v) '(:dummy :procdummy :param)) (%fc-error "~A cannot be in an EQUIVALENCE" (fv-name v)))
            (push (cons v (- off)) members)))
        (push (nreverse members) groups)))
    (setq groups (nreverse groups))
    ;; merge groups sharing a variable
    (let ((changed t))
      (loop
        (unless changed (return))
        (setq changed nil)
        (block merge
          (dolist (a groups)
            (dolist (b groups)
              (unless (eq a b)
                (let ((shared (find-if (lambda (m) (assoc (car m) b)) a)))
                  (when shared
                    (let* ((in-b (assoc (car shared) b)) (shift (- (cdr shared) (cdr in-b))) (merged (copy-list a)))
                      (dolist (m b)
                        (let ((existing (assoc (car m) merged)))
                          (if existing
                              (unless (= (cdr existing) (+ (cdr m) shift))
                                (%fc-error "EQUIVALENCE places ~A at two different offsets" (fv-name (car m))))
                              (push (cons (car m) (+ (cdr m) shift)) merged))))
                      (setq groups (cons (nreverse merged) (remove b (remove a groups))))
                      (setq changed t)
                      (return-from merge))))))))))
    ;; allocate each group
    (dolist (g groups)
      (let* ((common (remove-if-not (lambda (m) (eq (fv-class (car m)) :common)) g)))
        (when (cdr common) (%fc-error "Two COMMON variables (~A and ~A) cannot be made equivalent" (fv-name (car (car common))) (fv-name (car (cadr common)))))
        (let* ((space (%fc-space (fv-kind (car (car g))))))
          (dolist (m g)
            (unless (eq (%fc-space (fv-kind (car m))) space)
              (%fc-error "CHARACTER and non-CHARACTER variables (~A and ~A) cannot be made equivalent" (fv-name (car (car g))) (fv-name (car m)))))
          (let* ((min (apply #'min (mapcar #'cdr g)))
                 (origin (if common
                             (- (fv-off (car (car common))) (cdr (car common)))
                             nil)))
            (if common
                (let* ((cv (car (car common))) (cb (find (fv-common cv) (cu-commons u) :key #'cm-name :test #'string=)))
                  (when (< (+ origin min) 0) (%fc-error "EQUIVALENCE extends the COMMON block ~A backwards" (fv-common cv)))
                  (dolist (m g)
                    (unless (eq (car m) cv)
                      (let* ((v (car m)) (kind (fv-kind v)) (words (%fc-words kind (fv-len v))) (count (if (fv-dims v) (%fc-const-array-size v) 1)))
                        (setf (fv-class v) :common (fv-common v) (fv-common cv) (fv-space v) space
                              (fv-vec v) (fv-vec cv) (fv-off v) (+ origin (cdr m)))
                        (push (list space (fv-off v) count (if (eq space :chr) (fv-len v) kind) words) (cm-zspecs cb))
                        (if (eq space :chr)
                            (setf (cm-nc cb) (max (cm-nc cb) (+ (fv-off v) (* count words))))
                            (setf (cm-nn cb) (max (cm-nn cb) (+ (fv-off v) (* count words)))))))))
                (let* ((base (if (eq space :chr) (cu-nc u) (cu-nn u))) (end 0))
                  (dolist (m g)
                    (let* ((v (car m)) (kind (fv-kind v)) (words (%fc-words kind (fv-len v))) (count (if (fv-dims v) (%fc-const-array-size v) 1)))
                      (setf (fv-space v) space (fv-off v) (+ base (- (cdr m) min)))
                      (setf (fv-vec v) (if (eq space :chr) (%fc-sym "%CV") (%fc-sym "%NV")))
                      (setq end (max end (+ (- (cdr m) min) (* count words))))
                      (push (list space (fv-off v) count (if (eq space :chr) (fv-len v) kind) words) (cu-zspecs u))))
                  (if (eq space :chr) (setf (cu-nc u) (+ base end)) (setf (cu-nn u) (+ base end)))))))))))

;;; ---- expressions -----------------------------------------------------------------------------------------------------

(defun %fc-read-form (v)
  "Form reading the scalar variable V (checked for an undefined value)."
  (setf (fv-used v) t)
  (case (fv-class v)
    (:param (%fc-literal (fv-value v)))
    (t (multiple-value-bind (vec off) (%fc-storage v)
         (if *f77-check-undefined*
             `(%f7-rd ,vec ,off ,(string-downcase (fv-name v)))
             `(svref ,vec ,off))))))

(defun %fc-literal (x)
  (if (or (numberp x) (stringp x) (eq x t) (null x)) x `(quote ,x)))

(defun %fc-expect-scalar (v)
  (when (fv-dims v) (%fc-error "The array ~A is used without subscripts" (fv-name v)))
  (when (member (fv-class v) '(:external :stmtfun)) (%fc-error "The ~A ~A is used as a variable" (if (eq (fv-class v) :stmtfun) "statement function" "procedure") (fv-name v)))
  (when (eq (fv-class v) :procdummy) (%fc-error "The procedure ~A is used as a variable" (fv-name v))))

(defun %fc-expr (node)
  "Compile the expression NODE: (values form kind len)."
  (ecase (car node)
    (:int (values (cadr node) :integer nil))
    (:real (values (%f7-single (cadr node)) :real nil))
    (:dreal (values (cadr node) :double nil))
    (:logical (values (cadr node) :logical nil))
    (:str (values (cadr node) :character (length (cadr node))))
    (:paren (%fc-expr (cadr node)))
    (:complex
     (multiple-value-bind (rf rk) (%fc-expr (cadr node))
       (multiple-value-bind (if2 ik) (%fc-expr (caddr node))
         (unless (and (%fc-numeric-p rk) (%fc-numeric-p ik) (not (%fc-complex-p rk)) (not (%fc-complex-p ik)))
           (%fc-error "The parts of a complex constant must be real numbers"))
         (let ((dbl (or (eq rk :double) (eq ik :double))))
           (values (if dbl `(complex (float ,rf 1d0) (float ,if2 1d0)) `(%f7-cxs (complex (float ,rf 1d0) (float ,if2 1d0))))
                   (if dbl :dcomplex :complex) nil)))))
    (:name (%fc-name-expr (cadr node)))
    (:ref (%fc-ref-expr node))
    (:substr (%fc-substr-expr node))
    (:un (%fc-unary (cadr node) (caddr node)))
    (:bin (%fc-binary (cadr node) (caddr node) (cadddr node)))))

(defun %fc-name-expr (name)
  (let ((v (%fc-find name)))
    (when (and *fc-const-mode* (not (and v (eq (fv-class v) :param))))
      (let ((c (assoc name *fc-const-env* :test #'string=)))
        (if c
            (return-from %fc-name-expr (values (cdr c) :integer nil))
            (%fc-error "~A is not a constant (PARAMETER) in a constant expression" name))))
    (let ((v (or v (%fc-create name))))
      (%fc-expect-scalar v)
      (values (%fc-read-form v) (fv-kind v) (fv-len v)))))

(defun %fc-substr-expr (node)
  "(:substr base lo hi): characters of an array element or function value."
  (multiple-value-bind (bf bk) (%fc-expr (cadr node))
    (unless (eq bk :character) (%fc-error "A substring needs a CHARACTER value"))
    (let ((lo (if (caddr node) (%fc-int-form (caddr node) "A substring bound") 1))
          (hi (and (cadddr node) (%fc-int-form (cadddr node) "A substring bound"))))
      (let ((s (%fc-temp "S")))
        (values `(let ((,s ,bf)) (%f7-substr ,s ,lo ,(or hi `(length ,s)))) :character nil)))))

(defun %fc-int-form (node what)
  (multiple-value-bind (f k) (%fc-expr node)
    (unless (member k '(:integer :logical)) (%fc-error "~A must be an INTEGER expression, not ~A" what (%fc-kind-name k)))
    (%fc-coerce f k :integer)))

(defun %fc-unary (op a)
  (multiple-value-bind (f k len) (%fc-expr a)
    (declare (ignore len))
    (ecase op
      (:+ (values f k nil))
      (:- (unless (or (%fc-numeric-p k) (eq k :logical)) (%fc-error "Unary minus needs a number, not ~A" (%fc-kind-name k)))
       (values `(- ,(if (eq k :logical) (%fc-coerce f k :integer) f)) (if (eq k :logical) :integer k) nil))
      (:not (case k
              (:logical (values `(not ,f) :logical nil))
              (:integer (values `(lognot ,f) :integer nil))
              (t (%fc-error ".NOT. needs a LOGICAL or INTEGER operand, not ~A" (%fc-kind-name k))))))))

(defun %fc-binary (op a b)
  (multiple-value-bind (fa ka la) (%fc-expr a)
    (multiple-value-bind (fb kb lb) (%fc-expr b)
      (case op
        ((:+ :- :* :/ :**) (%fc-arith op fa ka fb kb))
        (:// (unless (and (eq ka :character) (eq kb :character)) (%fc-error "// needs two CHARACTER operands"))
         (values `(%string-concat ,fa ,fb) :character (and (integerp la) (integerp lb) (+ la lb))))
        ((:eq :ne :lt :le :gt :ge) (%fc-relational op fa ka fb kb))
        ((:and :or :xor :eqv :neqv) (%fc-logical op fa ka fb kb))
        (t (%fc-error "Unknown operator ~A" op))))))

(defun %fc-arith (op fa ka fb kb)
  (unless (and (or (%fc-numeric-p ka) (eq ka :logical)) (or (%fc-numeric-p kb) (eq kb :logical)))
    (%fc-error "The operator ~A needs numeric operands, not ~A and ~A" (case op (:+ "+") (:- "-") (:* "*") (:/ "/") (t "**")) (%fc-kind-name ka) (%fc-kind-name kb)))
  (let* ((fa (if (eq ka :logical) (%fc-coerce fa ka :integer) fa)) (fb (if (eq kb :logical) (%fc-coerce fb kb :integer) fb))
         (ka (if (eq ka :logical) :integer ka)) (kb (if (eq kb :logical) :integer kb))
         (kind (%fc-max-kind ka kb)))
    (ecase op
      (:+ (values `(+ ,fa ,fb) kind nil))
      (:- (values `(- ,fa ,fb) kind nil))
      (:* (values `(* ,fa ,fb) kind nil))
      (:/ (if (eq kind :integer)
              (values `(%f7-idiv ,fa ,fb) :integer nil)
              (values `(/ ,fa ,fb) kind nil)))
      (:** (cond ((and (eq ka :integer) (eq kb :integer)) (values `(%f7-ipow ,fa ,fb) :integer nil))
                 ((%fc-complex-p ka) (values `(%f7-cpow ,fa ,fb) ka nil))
                 ((%fc-complex-p kb) (values `(expt ,(%fc-coerce fa ka :dcomplex) ,fb) kb nil))
                 ((eq kb :integer) (values `(%f7-rpow ,(if (eq ka :integer) `(float ,fa 1d0) fa) ,fb) ka nil))
                 (t (values `(%f7-rpow ,(if (eq ka :integer) `(float ,fa 1d0) fa) ,fb) kind nil)))))))

(defun %fc-relational (op fa ka fb kb)
  (cond
    ((and (eq ka :character) (eq kb :character))
     (values `(,(ecase op (:eq '=) (:ne '/=) (:lt '<) (:le '<=) (:gt '>) (:ge '>=)) (%f7-ccmp ,fa ,fb) 0) :logical nil))
    ((and (or (%fc-numeric-p ka) (eq ka :logical)) (or (%fc-numeric-p kb) (eq kb :logical)))
     (when (and (or (%fc-complex-p ka) (%fc-complex-p kb)) (not (member op '(:eq :ne))))
       (%fc-error "COMPLEX values can be compared only with .EQ. and .NE."))
     (let ((fa (if (eq ka :logical) (%fc-coerce fa ka :integer) fa)) (fb (if (eq kb :logical) (%fc-coerce fb kb :integer) fb)))
       (values (case op
                 (:eq `(= ,fa ,fb)) (:ne `(/= ,fa ,fb)) (:lt `(< ,fa ,fb)) (:le `(<= ,fa ,fb)) (:gt `(> ,fa ,fb)) (t `(>= ,fa ,fb)))
               :logical nil)))
    (t (%fc-error "Cannot compare ~A with ~A" (%fc-kind-name ka) (%fc-kind-name kb)))))

(defun %fc-logical (op fa ka fb kb)
  (cond
    ((and (eq ka :logical) (eq kb :logical))
     (values (ecase op
               (:and `(and ,fa ,fb t)) (:or `(if (or ,fa ,fb) t nil))
               (:xor `(not (eq (not ,fa) (not ,fb)))) (:neqv `(not (eq (not ,fa) (not ,fb)))) (:eqv `(eq (not ,fa) (not ,fb))))
             :logical nil))
    ((and (member ka '(:logical :integer)) (member kb '(:logical :integer)))
     ;; integer operands: bit-wise (the logical operators on binary patterns)
     (let ((fa (%fc-coerce fa ka :integer)) (fb (%fc-coerce fb kb :integer)))
       (values (ecase op (:and `(logand ,fa ,fb)) (:or `(logior ,fa ,fb)) ((:xor :neqv) `(logxor ,fa ,fb)) (:eqv `(%f7-eqv ,fa ,fb)))
               :integer nil)))
    (t (%fc-error "The logical operator needs LOGICAL or INTEGER operands, not ~A and ~A" (%fc-kind-name ka) (%fc-kind-name kb)))))

(defun %fc-condition (node what)
  "A form that is true when the LOGICAL (or INTEGER) expression NODE is true."
  (multiple-value-bind (f k) (%fc-expr node)
    (case k
      (:logical f)
      (:integer `(/= ,f 0))
      (t (%fc-error "The ~A must be LOGICAL, not ~A" what (%fc-kind-name k))))))

;;; ---- references: array elements, calls, statement functions ----------------------------------------------------------

(defun %fc-ref-expr (node)
  (let* ((name (cadr node)) (items (caddr node)) (v (%fc-find name)))
    (cond
      ((and v (fv-dims v))
       (if (and (eq (fv-kind v) :character) (= (length items) 1) (eq (car (car items)) :slice) (not (= (length (fv-dims v)) 1)))
           (%fc-error "A substring of ~A needs array subscripts first" name)
           (%fc-element-expr v items)))
      ((and v (eq (fv-class v) :stmtfun)) (%fc-stmtfun-call v items))
      ((and v (eq (fv-kind v) :character) (null (fv-dims v)) (not (member (fv-class v) '(:external :procdummy)))
            (= (length items) 1) (eq (car (car items)) :slice))
       ;; substring of a character scalar
       (%fc-substring-of-var v (car items)))
      ((%fc-bit-substring-var-p v items)
       ;; bits lo through hi of an INTEGER scalar
       (%fc-bit-substring v (car items)))
      (t (%fc-function-call name items v)))))

(defun %fc-bit-substring-var-p (v items)
  "True when V(ITEMS) is a bit substring: an INTEGER scalar variable with one range."
  (and v (eq (fv-kind v) :integer) (null (fv-dims v)) (not (member (fv-class v) '(:external :procdummy :stmtfun)))
       (= (length items) 1) (eq (car (car items)) :slice)))

(defun %fc-bit-bounds (slice)
  (unless (and (cadr slice) (caddr slice)) (%fc-error "A bit substring needs both bounds, I(first:last)"))
  (values (%fc-int-form (cadr slice) "A bit substring bound") (%fc-int-form (caddr slice) "A bit substring bound")))

(defun %fc-bit-substring (v slice)
  "I(lo:hi): the field of bits LO through HI of the INTEGER variable (bit 0 is the least significant)."
  (multiple-value-bind (lo hi) (%fc-bit-bounds slice)
    (values `(%f7-bits ,(%fc-read-form v) ,lo ,hi) :integer nil)))

(defun %fc-substring-of-var (v slice)
  (%fc-expect-scalar v)
  (let ((lo (if (cadr slice) (%fc-int-form (cadr slice) "A substring bound") 1))
        (hi (and (caddr slice) (%fc-int-form (caddr slice) "A substring bound")))
        (s (%fc-temp "S")))
    (values `(let ((,s ,(%fc-read-form v))) (%f7-substr ,s ,lo ,(or hi `(length ,s)))) :character nil)))

(defun %fc-element-place (v items)
  "(values vector-form offset-form) of the array element V(ITEMS)."
  (setf (fv-used v) t)
  (multiple-value-bind (vec off) (%fc-storage v)
    (values vec `(+ ,off ,(%fc-subscript-offset v items)))))

(defun %fc-element-expr (v items)
  (when (some (lambda (i) (eq (car i) :slice)) items)
    (%fc-error "~A is an array: its subscripts must be INTEGER expressions" (fv-name v)))
  (multiple-value-bind (vec off) (%fc-element-place v items)
    (values (if *f77-check-undefined* `(%f7-rd ,vec ,off ,(string-downcase (fv-name v))) `(svref ,vec ,off))
            (fv-kind v) (fv-len v))))

;;; statement functions are expanded in line: their dummy arguments become one-entry boxes
(defun %fc-stmtfun-call (v items)
  (unless (= (length items) (length (fv-params v)))
    (%fc-error "The statement function ~A takes ~D argument~:P, not ~D" (fv-name v) (length (fv-params v)) (length items)))
  (let ((binds nil) (scope nil))
    (do ((p (fv-params v) (cdr p)) (x items (cdr x))) ((null p))
      (multiple-value-bind (f k len) (%fc-expr (car x))
        (let* ((entry (car p)) (box (%fc-temp "A")) (name (car entry)) (pkind (cadr entry))
               (pv (make-fvar :name name :kind pkind :len (caddr entry) :class :local :vec box :off 0 :space (%fc-space pkind) :declared t)))
          (push (list box `(vector ,(%fc-coerce-to f k len pkind))) binds)
          (push (cons name pv) scope))))
    (let ((*fc-scope* (append scope *fc-scope*)))
      (multiple-value-bind (bf bk blen) (%fc-expr (fv-body v))
        (values `(let ,(nreverse binds) ,(%fc-coerce-to bf bk blen (fv-kind v))) (fv-kind v) (fv-len v))))))

(defun %fc-coerce-to (form from len to)
  (declare (ignorable len))
  (%fc-coerce form from to))

;;; ---- intrinsic functions --------------------------------------------------------------------------------------------

(defvar *fc-intrinsics* (make-hash-table :test 'equal) "Intrinsic function name -> handler (name args) -> (values form kind len).")

(defun %fc-intrinsic-name-p (name) (and (gethash name *fc-intrinsics*) t))

(defmacro def-f77-intrinsic (names (name args) &body body)
  `(let ((h (lambda (,name ,args) (declare (ignorable ,name ,args)) ,@body)))
     (dolist (n ',names) (setf (gethash n *fc-intrinsics*) h))))

(defun %fc-arg-count (name args lo &optional (hi lo))
  (unless (<= lo (length args) hi)
    (%fc-error "The intrinsic function ~A takes ~D~:[ to ~D~;~*~] argument~:P, not ~D" name lo (eql lo hi) hi (length args))))

(defun %fc-need-kinds (name args kinds what)
  "Every argument of the intrinsic NAME must be of one of KINDS."
  (dolist (a args)
    (unless (member (cadr a) kinds)
      (%fc-error "The argument of ~A must be ~A, not ~A" name what (%fc-kind-name (cadr a))))))

(defun %fc-arg-as (a kind) (%fc-coerce (car a) (cadr a) kind))

(defun %fc-float-arg (a)
  "The argument A as a double (or complex) form, and its result kind."
  (case (cadr a)
    (:integer (values `(float ,(car a) 1d0) :real))
    (:logical (values `(float ,(%fc-coerce (car a) :logical :integer) 1d0) :real))
    (t (values (car a) (cadr a)))))

(defun %fc-round-result (form kind)
  (case kind (:real `(%f7-single ,form)) (:complex `(%f7-cxs ,form)) (t form)))

(defparameter *fc-math-table*
  '(("SQRT" %f7-sqrt) ("EXP" exp) ("LOG" %f7-log) ("LOG10" %f7-log10) ("SIN" sin) ("COS" cos) ("TAN" tan)
    ("ASIN" %f7-asin) ("ACOS" %f7-acos) ("ATAN" atan) ("SINH" sinh) ("COSH" cosh) ("TANH" tanh)))

(defparameter *fc-math-aliases*
  '(("DSQRT" "SQRT" :double) ("CSQRT" "SQRT" :complex) ("DEXP" "EXP" :double) ("CEXP" "EXP" :complex)
    ("ALOG" "LOG" :real) ("DLOG" "LOG" :double) ("CLOG" "LOG" :complex) ("ALOG10" "LOG10" :real) ("DLOG10" "LOG10" :double)
    ("DSIN" "SIN" :double) ("CSIN" "SIN" :complex) ("DCOS" "COS" :double) ("CCOS" "COS" :complex) ("DTAN" "TAN" :double)
    ("DASIN" "ASIN" :double) ("DACOS" "ACOS" :double) ("DATAN" "ATAN" :double)
    ("DSINH" "SINH" :double) ("DCOSH" "COSH" :double) ("DTANH" "TANH" :double)))

(defun %fc-math-intrinsic (name args)
  (%fc-arg-count name args 1)
  (let* ((alias (assoc name *fc-math-aliases* :test #'string=)) (base (if alias (cadr alias) name)) (forced (and alias (caddr alias)))
         (fn (cadr (assoc base *fc-math-table* :test #'string=))) (a (car args)))
    (unless (or (%fc-numeric-p (cadr a)) (eq (cadr a) :logical)) (%fc-error "The argument of ~A must be a number, not ~A" name (%fc-kind-name (cadr a))))
    (when (and forced (not (eq forced (cadr a))))
      (%fc-error "The specific function ~A needs an argument of type ~A, not ~A" name (%fc-kind-name forced) (%fc-kind-name (cadr a))))
    (multiple-value-bind (f k) (%fc-float-arg a)
      (when (and (%fc-complex-p k) (member base '("LOG10" "ASIN" "ACOS" "TAN" "SINH" "COSH" "TANH") :test #'string=) (not (member base '("TAN") :test #'string=)))
        (%fc-error "~A has no COMPLEX form" name))
      (values (%fc-round-result `(,fn ,f) k) k nil))))

(def-f77-intrinsic ("SQRT" "DSQRT" "CSQRT" "EXP" "DEXP" "CEXP" "LOG" "ALOG" "DLOG" "CLOG" "LOG10" "ALOG10" "DLOG10"
                    "SIN" "DSIN" "CSIN" "COS" "DCOS" "CCOS" "TAN" "DTAN" "ASIN" "DASIN" "ACOS" "DACOS" "ATAN" "DATAN"
                    "SINH" "DSINH" "COSH" "DCOSH" "TANH" "DTANH")
    (name args)
  (%fc-math-intrinsic name args))

(def-f77-intrinsic ("ATAN2" "DATAN2") (name args)
  (%fc-arg-count name args 2)
  (%fc-need-kinds name args '(:real :double :integer) "a real number")
  (let ((k (if (string= name "DATAN2") :double (%fc-max-kind (%fc-max-kind (cadr (car args)) (cadr (cadr args))) :real))))
    (when (and (string= name "DATAN2") (not (and (eq (cadr (car args)) :double) (eq (cadr (cadr args)) :double))))
      (%fc-error "DATAN2 needs DOUBLE PRECISION arguments"))
    (values (%fc-round-result `(%f7-atan2 ,(car (car args)) ,(car (cadr args))) k) k nil)))

(def-f77-intrinsic ("ABS" "IABS" "DABS" "CABS") (name args)
  (%fc-arg-count name args 1)
  (let* ((a (car args)) (k (cadr a)))
    (unless (or (%fc-numeric-p k) (eq k :logical)) (%fc-error "The argument of ~A must be a number, not ~A" name (%fc-kind-name k)))
    (let ((need (cond ((string= name "IABS") :integer) ((string= name "DABS") :double) ((string= name "CABS") :complex) (t nil))))
      (when (and need (not (eq need (if (eq k :logical) :integer k)))) (%fc-error "The specific function ~A needs an argument of type ~A, not ~A" name (%fc-kind-name need) (%fc-kind-name k))))
    (let ((k (if (eq k :logical) :integer k)) (f (%fc-arg-as a (if (eq (cadr a) :logical) :integer (cadr a)))))
      (case k
        (:complex (values `(%f7-single (abs ,f)) :real nil))
        (:dcomplex (values `(abs ,f) :double nil))
        (t (values `(abs ,f) k nil))))))

(def-f77-intrinsic ("MOD" "AMOD" "DMOD") (name args)
  (%fc-arg-count name args 2)
  (dolist (a args) (unless (member (cadr a) '(:integer :real :double)) (%fc-error "The arguments of ~A must be INTEGER or REAL, not ~A" name (%fc-kind-name (cadr a)))))
  (let ((k (%fc-max-kind (cadr (car args)) (cadr (cadr args)))))
    (when (and (string= name "AMOD") (not (eq k :real))) (%fc-error "AMOD needs REAL arguments"))
    (when (and (string= name "DMOD") (not (eq k :double))) (%fc-error "DMOD needs DOUBLE PRECISION arguments"))
    (values `(%f7-mod ,(car (car args)) ,(car (cadr args))) k nil)))

(def-f77-intrinsic ("SIGN" "ISIGN" "DSIGN") (name args)
  (%fc-arg-count name args 2)
  (let ((k (%fc-max-kind (cadr (car args)) (cadr (cadr args)))))
    (unless (member k '(:integer :real :double)) (%fc-error "The arguments of ~A must be INTEGER or REAL" name))
    (when (and (string= name "ISIGN") (not (eq k :integer))) (%fc-error "ISIGN needs INTEGER arguments"))
    (when (and (string= name "DSIGN") (not (eq k :double))) (%fc-error "DSIGN needs DOUBLE PRECISION arguments"))
    (values `(%f7-sign ,(%fc-arg-as (car args) k) ,(%fc-arg-as (cadr args) k)) k nil)))

(def-f77-intrinsic ("DIM" "IDIM" "DDIM") (name args)
  (%fc-arg-count name args 2)
  (let ((k (%fc-max-kind (cadr (car args)) (cadr (cadr args)))))
    (unless (member k '(:integer :real :double)) (%fc-error "The arguments of ~A must be INTEGER or REAL" name))
    (values `(%f7-dim ,(%fc-arg-as (car args) k) ,(%fc-arg-as (cadr args) k)) k nil)))

(defun %fc-minmax (name args fn)
  (%fc-arg-count name args 2 100)
  (dolist (a args) (unless (member (cadr a) '(:integer :real :double :logical)) (%fc-error "The arguments of ~A must be INTEGER or REAL, not ~A" name (%fc-kind-name (cadr a)))))
  (let* ((arg-kind (let ((k :integer)) (dolist (a args k) (setq k (%fc-max-kind k (cadr a))))))
         (result (cond ((member name '("MAX0" "MIN0") :test #'string=) :integer)
                       ((member name '("MAX1" "MIN1") :test #'string=) :integer)
                       ((member name '("AMAX0" "AMIN0") :test #'string=) :real)
                       ((member name '("AMAX1" "AMIN1") :test #'string=) :real)
                       ((member name '("DMAX1" "DMIN1") :test #'string=) :double)
                       (t arg-kind))))
    (let ((need (cond ((member name '("MAX0" "MIN0" "AMAX0" "AMIN0") :test #'string=) :integer)
                      ((member name '("MAX1" "MIN1" "AMAX1" "AMIN1") :test #'string=) :real)
                      ((member name '("DMAX1" "DMIN1") :test #'string=) :double) (t nil))))
      (when (and need (not (eq (%fc-max-kind need arg-kind) need)))
        (%fc-error "The specific function ~A needs ~A arguments" name (%fc-kind-name need))))
    (let ((forms (mapcar (lambda (a) (%fc-arg-as a (if (eq (cadr a) :logical) :integer (cadr a)))) args)))
      (values (cond ((eq result arg-kind) `(,fn ,@forms))
                    (t (%fc-coerce `(,fn ,@forms) arg-kind result)))
              result nil))))

(def-f77-intrinsic ("MAX" "MAX0" "MAX1" "AMAX0" "AMAX1" "DMAX1") (name args) (%fc-minmax name args 'max))
(def-f77-intrinsic ("MIN" "MIN0" "MIN1" "AMIN0" "AMIN1" "DMIN1") (name args) (%fc-minmax name args 'min))

(def-f77-intrinsic ("INT" "IFIX" "IDINT") (name args)
  (%fc-arg-count name args 1)
  (let ((a (car args)))
    (unless (or (%fc-numeric-p (cadr a)) (eq (cadr a) :logical)) (%fc-error "The argument of ~A must be a number" name))
    (when (and (string= name "IFIX") (not (eq (cadr a) :real))) (%fc-error "IFIX needs a REAL argument"))
    (when (and (string= name "IDINT") (not (eq (cadr a) :double))) (%fc-error "IDINT needs a DOUBLE PRECISION argument"))
    (values (%fc-arg-as a :integer) :integer nil)))

(def-f77-intrinsic ("REAL" "FLOAT" "SNGL") (name args)
  (%fc-arg-count name args 1)
  (let ((a (car args)))
    (unless (or (%fc-numeric-p (cadr a)) (eq (cadr a) :logical)) (%fc-error "The argument of ~A must be a number" name))
    (when (and (string= name "FLOAT") (not (eq (cadr a) :integer))) (%fc-error "FLOAT needs an INTEGER argument"))
    (when (and (string= name "SNGL") (not (eq (cadr a) :double))) (%fc-error "SNGL needs a DOUBLE PRECISION argument"))
    (values (%fc-arg-as a :real) :real nil)))

(def-f77-intrinsic ("DBLE") (name args)
  (%fc-arg-count name args 1)
  (unless (or (%fc-numeric-p (cadr (car args))) (eq (cadr (car args)) :logical)) (%fc-error "The argument of DBLE must be a number"))
  (values (%fc-arg-as (car args) :double) :double nil))

(def-f77-intrinsic ("CMPLX" "DCMPLX") (name args)
  (%fc-arg-count name args 1 2)
  (dolist (a args) (unless (%fc-numeric-p (cadr a)) (%fc-error "The arguments of ~A must be numbers" name)))
  (let ((dbl (string= name "DCMPLX")))
    (if (null (cdr args))
        (values (%fc-arg-as (car args) (if dbl :dcomplex :complex)) (if dbl :dcomplex :complex) nil)
        (progn
          (when (some (lambda (a) (%fc-complex-p (cadr a))) args) (%fc-error "~A with two arguments needs real arguments" name))
          (values (if dbl
                      `(complex ,(%fc-arg-as (car args) :double) ,(%fc-arg-as (cadr args) :double))
                      `(%f7-cxs (complex ,(%fc-arg-as (car args) :double) ,(%fc-arg-as (cadr args) :double))))
                  (if dbl :dcomplex :complex) nil)))))

(def-f77-intrinsic ("AIMAG" "DIMAG") (name args)
  (%fc-arg-count name args 1)
  (unless (%fc-complex-p (cadr (car args))) (%fc-error "The argument of ~A must be COMPLEX" name))
  (values `(imagpart ,(car (car args))) (if (eq (cadr (car args)) :dcomplex) :double :real) nil))

(def-f77-intrinsic ("CONJG" "DCONJG") (name args)
  (%fc-arg-count name args 1)
  (unless (%fc-complex-p (cadr (car args))) (%fc-error "The argument of ~A must be COMPLEX" name))
  (values `(conjugate ,(car (car args))) (cadr (car args)) nil))

(def-f77-intrinsic ("AINT" "DINT" "ANINT" "DNINT") (name args)
  (%fc-arg-count name args 1)
  (let ((k (cadr (car args))))
    (unless (member k '(:real :double)) (%fc-error "The argument of ~A must be REAL, not ~A" name (%fc-kind-name k)))
    (when (and (member name '("DINT" "DNINT") :test #'string=) (not (eq k :double))) (%fc-error "~A needs a DOUBLE PRECISION argument" name))
    (values `(,(if (member name '("AINT" "DINT") :test #'string=) '%f7-aint '%f7-anint) ,(car (car args))) k nil)))

(def-f77-intrinsic ("NINT" "IDNINT") (name args)
  (%fc-arg-count name args 1)
  (let ((k (cadr (car args))))
    (unless (member k '(:real :double)) (%fc-error "The argument of ~A must be REAL, not ~A" name (%fc-kind-name k)))
    (values `(%f7-nint ,(car (car args))) :integer nil)))

(def-f77-intrinsic ("DPROD") (name args)
  (%fc-arg-count name args 2)
  (values `(* (float ,(%fc-arg-as (car args) :real) 1d0) (float ,(%fc-arg-as (cadr args) :real) 1d0)) :double nil))

(def-f77-intrinsic ("LEN") (name args)
  (%fc-arg-count name args 1)
  (unless (eq (cadr (car args)) :character) (%fc-error "The argument of LEN must be CHARACTER"))
  (values `(length ,(car (car args))) :integer nil))

(def-f77-intrinsic ("INDEX") (name args)
  (%fc-arg-count name args 2)
  (%fc-need-kinds name args '(:character) "CHARACTER")
  (values `(%f7-index ,(car (car args)) ,(car (cadr args))) :integer nil))

(def-f77-intrinsic ("ICHAR") (name args)
  (%fc-arg-count name args 1)
  (%fc-need-kinds name args '(:character) "CHARACTER")
  (values `(%f7-ichar ,(car (car args))) :integer nil))

(def-f77-intrinsic ("CHAR") (name args)
  (%fc-arg-count name args 1)
  (%fc-need-kinds name args '(:integer) "INTEGER")
  (values `(%f7-char ,(car (car args))) :character 1))

(def-f77-intrinsic ("LGE" "LGT" "LLE" "LLT") (name args)
  (%fc-arg-count name args 2)
  (%fc-need-kinds name args '(:character) "CHARACTER")
  (values `(,(cond ((string= name "LGE") '>=) ((string= name "LGT") '>) ((string= name "LLE") '<=) (t '<))
            (%f7-ccmp ,(car (car args)) ,(car (cadr args))) 0)
          :logical nil))

(defun %fc-int-args (name args)
  (dolist (a args)
    (unless (member (cadr a) '(:integer :logical)) (%fc-error "The arguments of ~A must be INTEGER, not ~A" name (%fc-kind-name (cadr a)))))
  (mapcar (lambda (a) (%fc-arg-as a :integer)) args))

(def-f77-intrinsic ("IAND") (name args) (%fc-arg-count name args 2) (let ((f (%fc-int-args name args))) (values `(logand ,@f) :integer nil)))
(def-f77-intrinsic ("IOR") (name args) (%fc-arg-count name args 2) (let ((f (%fc-int-args name args))) (values `(logior ,@f) :integer nil)))
(def-f77-intrinsic ("IEOR") (name args) (%fc-arg-count name args 2) (let ((f (%fc-int-args name args))) (values `(logxor ,@f) :integer nil)))
(def-f77-intrinsic ("NOT") (name args) (%fc-arg-count name args 1) (let ((f (%fc-int-args name args))) (values `(lognot ,@f) :integer nil)))
(def-f77-intrinsic ("ISHFT" "SHIFT") (name args) (%fc-arg-count name args 2) (let ((f (%fc-int-args name args))) (values `(%f7-ishft ,@f) :integer nil)))
(def-f77-intrinsic ("ISHFTC") (name args) (%fc-arg-count name args 3) (let ((f (%fc-int-args name args))) (values `(%f7-ishftc ,@f) :integer nil)))
(def-f77-intrinsic ("IBITS") (name args) (%fc-arg-count name args 3) (let ((f (%fc-int-args name args))) (values `(%f7-ibits ,@f) :integer nil)))
(def-f77-intrinsic ("IBSET") (name args) (%fc-arg-count name args 2) (let ((f (%fc-int-args name args))) (values `(%f7-ibset ,@f) :integer nil)))
(def-f77-intrinsic ("IBCLR") (name args) (%fc-arg-count name args 2) (let ((f (%fc-int-args name args))) (values `(%f7-ibclr ,@f) :integer nil)))
(def-f77-intrinsic ("BTEST") (name args) (%fc-arg-count name args 2) (let ((f (%fc-int-args name args))) (values `(%f7-btest ,@f) :logical nil)))

(defun f77-intrinsic-names ()
  "The names of the intrinsic functions the FORTRAN compiler knows, sorted."
  (let ((acc nil)) (maphash (lambda (k v) (declare (ignore v)) (push k acc)) *fc-intrinsics*) (sort acc #'string<)))

;;; ---- actual arguments and function references ----------------------------------------------------------------------------

(defun %fc-arg-form (node)
  "Compile the actual argument NODE: (values form kind len array-p) -- a reference for variables, a boxed value otherwise."
  (case (car node)
    (:name
     (let* ((name (cadr node)) (v (%fc-find name)))
       (cond
         ((and v (member (fv-class v) '(:external :procdummy)))
          (values (if (eq (fv-class v) :procdummy) (progn (%fc-storage v) (%fc-sym "~A%P" name)) `(quote ,(%fc-routine-sym name))) :procedure nil nil))
         ((and (null v) (%fc-declared-intrinsic-p name))
          (values (%fc-intrinsic-as-function name) :procedure nil nil))
         ((and v (eq (fv-class v) :param)) (values `(%f7-box ,(%fc-literal (fv-value v))) (fv-kind v) (fv-len v) nil))
         (t
          (let ((v (or v (%fc-create name))))
            (when (eq (fv-class v) :stmtfun) (%fc-error "The statement function ~A cannot be an argument" name))
            (setf (fv-used v) t)
            (multiple-value-bind (vec off) (%fc-storage v)
              (values `(cons ,vec ,off) (fv-kind v) (fv-len v) (and (fv-dims v) t))))))))
    (:ref
     (let* ((name (cadr node)) (v (%fc-find name)))
       (if (and v (fv-dims v) (notany (lambda (i) (eq (car i) :slice)) (caddr node)))
           (multiple-value-bind (vec off) (%fc-element-place v (caddr node))
             (values `(cons ,vec ,off) (fv-kind v) (fv-len v) nil))
           (multiple-value-bind (f k len) (%fc-expr node)
             (values `(%f7-box ,f) k len nil)))))
    (t (multiple-value-bind (f k len) (%fc-expr node)
         (values `(%f7-box ,f) k len nil)))))

(defun %fc-declared-intrinsic-p (name)
  (and (%fc-intrinsic-name-p name) (member name (cu-intrinsics *fc-unit*) :test #'string=) t))

(defun %fc-intrinsic-as-function (name)
  "A function object calling the intrinsic NAME on a reference argument (for INTRINSIC names passed as arguments)."
  (let* ((a (%fc-sym "%A")) (alias (assoc name *fc-math-aliases* :test #'string=))
         (kind (cond ((and alias (caddr alias)) (caddr alias)) ((string= (subseq name 0 1) "D") :double) (t :real))))
    (multiple-value-bind (f k) (funcall (gethash name *fc-intrinsics*) name (list (list `(svref (car ,a) (cdr ,a)) kind nil)))
      (declare (ignore k))
      `(lambda (,a) ,f))))

(defun %fc-known-signature (name)
  (gethash (%string-concat (package-name *fc-package*) ":" name) *f77-signatures*))

(defun %fc-check-call (name arg-kinds)
  "Compare a call of NAME with the signature of the routine if it was already compiled: stronger type checks."
  (let ((sig (%fc-known-signature name)))
    (when sig
      (let ((params (remove :star (caddr sig))))
        (unless (= (length params) (length arg-kinds))
          (%fc-error "~A takes ~D argument~:P but is called with ~D" (string-downcase name) (length params) (length arg-kinds)))
        (do ((p params (cdr p)) (a arg-kinds (cdr a)) (i 1 (+ i 1))) ((null p))
          (let ((pk (car (car p))) (ak (car a)))
            (when (and pk ak (not (eq ak :procedure)) (not (eq pk :procedure)) (not (eq pk ak))
                       (not (and (member pk '(:integer :logical)) (member ak '(:integer :logical)))))
              (%fc-error "Argument ~D of ~A is ~A but the routine declares it ~A" i (string-downcase name) (%fc-kind-name ak) (%fc-kind-name pk)))))))))

(defun %fc-call-form (sym v arg-forms)
  "A form calling the routine SYM (or the dummy procedure V) with the argument forms."
  (if (and v (eq (fv-class v) :procdummy))
      `(funcall ,(progn (%fc-storage v) (%fc-sym "~A%P" (fv-name v))) ,@arg-forms)
      `(%f7-call (quote ,sym) ,@arg-forms)))

(defun %fc-function-call (name items v)
  "A reference NAME(ITEMS) that is not an array element: an intrinsic or an external function."
  (when (some (lambda (i) (eq (car i) :slice)) items) (%fc-error "~A is not an array or CHARACTER variable: its arguments cannot be ranges" name))
  (cond
    ;; an intrinsic function, unless the name was declared EXTERNAL or is a variable of this unit
    ((and (%fc-intrinsic-name-p name) (or (null v) (eq (fv-class v) :intrinsic)))
     (when *fc-const-mode* nil)
     (let ((args (mapcar (lambda (n) (multiple-value-bind (f k len) (%fc-expr n) (list f k len))) items)))
       (funcall (gethash name *fc-intrinsics*) name args)))
    (*fc-const-mode* (%fc-error "The function ~A cannot be used in a constant expression" name))
    (t
     (let ((v (or v (%fc-create name :class :external))) (forms nil) (kinds nil))
       (when (and (null (fv-dims v)) (member (fv-class v) '(:local :common :result)) (fv-used v) (not (eq (fv-class v) :external)))
         (%fc-error "~A is a variable but is used as a function" name))
       (unless (member (fv-class v) '(:procdummy :external))
         (if (eq (fv-class v) :dummy)
             (setf (fv-class v) :procdummy)
             (setf (fv-class v) :external)))
       (dolist (n items)
         (multiple-value-bind (f k len arrayp) (%fc-arg-form n) (declare (ignore len arrayp)) (push f forms) (push k kinds)))
       (setq forms (nreverse forms) kinds (nreverse kinds))
       (let ((sig (%fc-known-signature name)))
         (when (and sig (not (eq (car sig) :function))) (%fc-error "~A is a subroutine, not a function" name))
         (%fc-check-call name kinds)
         (let ((kind (if (and sig (not (fv-declared v))) (cadr sig) (fv-kind v))))
           (when (and sig (fv-declared v) (not (eq kind (cadr sig))))
             (%fc-error "The function ~A is declared ~A here but is defined as ~A" name (%fc-kind-name (fv-kind v)) (%fc-kind-name (cadr sig))))
           (when (and sig (not (fv-declared v))) (setf (fv-kind v) kind))
           (values (%fc-call-form (%fc-routine-sym name) v forms) kind (fv-len v))))))))
