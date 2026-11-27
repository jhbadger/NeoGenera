;;;; 05-types.lisp -- type descriptors, declarations and the compilation policy.
;;;;
;;;; The compiler keeps a small lattice of *type descriptors* ("tys") for the specialisation pass
;;;; (15-specialize.lisp).  A ty is one of
;;;;
;;;;     NIL                 no information (the type T)
;;;;     (:int LO HI)        an integer from LO to HI; a NIL bound is unbounded
;;;;     :flonum :cons :null :list :symbol :string :svec :char :real :number
;;;;
;;;; :flonum is every float (there is only one float format), :string a SIMPLE string, :svec a SIMPLE-VECTOR,
;;;; :list is a cons or NIL, :real any real, :number any number.  Declarations that name a type outside this
;;;; lattice (STRING, VECTOR, user DEFTYPEs, MEMBER ...) are accepted and ignored: a ty is only made for a
;;;; type whose test the VM can perform exactly (or, for a range, a superset of the declared type).
;;;;
;;;; The compilation policy is an alist of (QUALITY . VALUE) plus (:NOTINLINE . names).  SAFETY 0 makes the compiler
;;;; trust declarations; at any other level it checks them.

(in-package :cl)

(defun ty-int (lo hi) (list :int lo hi))
(defun ty-intp (ty) (and (consp ty) (eq (car ty) :int)))
(defun ty-lo (ty) (cadr ty))
(defun ty-hi (ty) (caddr ty))

(defparameter *ty-fixnum* (list :int most-negative-fixnum most-positive-fixnum))

(defun ty-fixnum-p (ty)
  "True when TY is an integer type inside the fixnum range."
  (and (ty-intp ty) (ty-lo ty) (ty-hi ty)
       (>= (ty-lo ty) most-negative-fixnum) (<= (ty-hi ty) most-positive-fixnum)))

(defun ty-full-fixnum-p (ty)
  (and (ty-intp ty) (eql (ty-lo ty) most-negative-fixnum) (eql (ty-hi ty) most-positive-fixnum)))

(defun ty-subtype-p (a b)
  "True when every value of ty A is a value of ty B."
  (cond ((null b) t)
        ((null a) nil)
        ((and (ty-intp a) (ty-intp b))
         (and (or (null (ty-lo b)) (and (ty-lo a) (>= (ty-lo a) (ty-lo b))))
              (or (null (ty-hi b)) (and (ty-hi a) (<= (ty-hi a) (ty-hi b))))))
        ((ty-intp b) nil)
        ((ty-intp a) (if (member b '(:real :number)) t nil))
        ((eq a b) t)
        (t (if (case b
                 (:list (member a '(:cons :null)))
                 (:symbol (eq a :null))
                 (:real (eq a :flonum))
                 (:number (member a '(:flonum :real)))
                 (t nil))
               t nil))))

(defun ty-join (a b)
  "The smallest ty containing both A and B (NIL when there is none in the lattice)."
  (cond ((or (null a) (null b)) nil)
        ((ty-subtype-p a b) b)
        ((ty-subtype-p b a) a)
        ((and (ty-intp a) (ty-intp b))
         (list :int
               (and (ty-lo a) (ty-lo b) (min (ty-lo a) (ty-lo b)))
               (and (ty-hi a) (ty-hi b) (max (ty-hi a) (ty-hi b)))))
        (t (dolist (c '(:list :symbol :real :number) nil)
             (when (and (ty-subtype-p a c) (ty-subtype-p b c)) (return c))))))

(defun ty-meet (a b)
  "A ty that every value of both A and B has (used when a variable has several type declarations)."
  (cond ((null a) b)
        ((null b) a)
        ((ty-subtype-p a b) a)
        ((ty-subtype-p b a) b)
        ((and (ty-intp a) (ty-intp b))
         (let ((lo (cond ((null (ty-lo a)) (ty-lo b)) ((null (ty-lo b)) (ty-lo a)) (t (max (ty-lo a) (ty-lo b)))))
               (hi (cond ((null (ty-hi a)) (ty-hi b)) ((null (ty-hi b)) (ty-hi a)) (t (min (ty-hi a) (ty-hi b))))))
           (if (and lo hi (> lo hi)) a (list :int lo hi))))
        (t a)))

(defun ty-of-constant (v)
  (cond ((null v) :null)
        ((eq v t) :symbol)
        ((integerp v) (list :int v v))
        ((floatp v) :flonum)
        ((rationalp v) :real)
        ((numberp v) :number)
        ((consp v) :cons)
        ((symbolp v) :symbol)
        ((characterp v) :char)
        ((stringp v) :string)
        ((simple-vector-p v) :svec)
        (t nil)))

;;; ---- parsing type specifiers ------------------------------------------------------------------

(defun %int-bound (x)
  (cond ((eq x '*) nil) ((integerp x) x) (t :bad)))

(defun parse-type (spec)
  "The ty for the type specifier SPEC, or NIL when it is outside the lattice."
  (cond ((symbolp spec)
         (case spec
           (fixnum *ty-fixnum*)
           (integer (list :int nil nil))
           (bit (list :int 0 1))
           (unsigned-byte (list :int 0 nil))
           (signed-byte (list :int nil nil))
           ((float single-float double-float short-float long-float) :flonum)
           (real :real)
           (number :number)
           (cons :cons)
           (list :list)
           (null :null)
           ((symbol keyword) :symbol)
           ((simple-string simple-base-string) :string)
           (simple-vector :svec)
           ((character base-char standard-char) :char)
           (t nil)))
        ((not (consp spec)) nil)
        (t (let ((head (car spec)) (args (cdr spec)))
             (case head
               (integer (let ((lo (%int-bound (car args))) (hi (%int-bound (if (cdr args) (cadr args) '*))))
                          (if (or (eq lo :bad) (eq hi :bad) (and lo hi (> lo hi))) nil (list :int lo hi))))
               (unsigned-byte (if (integerp (car args)) (list :int 0 (- (ash 1 (car args)) 1))
                                  (if (or (null args) (eq (car args) '*)) (list :int 0 nil) nil)))
               (signed-byte (if (and (integerp (car args)) (> (car args) 0))
                                (list :int (- (ash 1 (- (car args) 1))) (- (ash 1 (- (car args) 1)) 1))
                                (if (or (null args) (eq (car args) '*)) (list :int nil nil) nil)))
               (mod (if (and (integerp (car args)) (> (car args) 0)) (list :int 0 (- (car args) 1)) nil))
               ((float single-float double-float short-float long-float) :flonum)
               (real :real)
               (cons :cons)
               (simple-vector :svec)
               (simple-array (parse-simple-array args))
               (or (let ((tys (mapcar #'parse-type args)) (r :none))
                     (dolist (ty tys)
                       (when (null ty) (return-from parse-type nil))
                       (setq r (if (eq r :none) ty (ty-join r ty))))
                     (if (eq r :none) nil r)))
               (t nil))))))

(defun parse-simple-array (args)
  ;; (simple-array element-type dimensions): a vector type only when one-dimensional.  Arrays are not specialised
  ;; by element type, so only the container is described (never the elements).
  (let ((elt (if args (car args) '*)) (dims (if (cdr args) (cadr args) '*)))
    (cond ((not (or (eq dims '*) (and (consp dims) (null (cdr dims))) (eql dims 1))) nil)
          ((member elt '(character base-char standard-char)) :string)
          (t :svec))))

;;; ---- declarations -------------------------------------------------------------------------------

(defun normalize-decl-spec (d)
  "A declaration specifier as a list of normalised ones: (TYPE spec var), (SPECIAL var), (OPTIMIZE ...) ..."
  (cond ((atom d) nil)
        ((eq (car d) 'type) (mapcar (lambda (v) (list 'type (cadr d) v)) (cddr d)))
        ((member (car d) '(special ignore ignorable dynamic-extent))
         (mapcar (lambda (v) (list (car d) v)) (cdr d)))
        ((member (car d) '(optimize inline notinline ftype declaration)) (list d))
        ((and (symbolp (car d)) (not (keywordp (car d))))
         (mapcar (lambda (v) (list 'type (car d) v)) (cdr d)))     ; shorthand: (fixnum x y)
        (t nil)))

(defun parse-declarations (body)
  "Strip the leading DECLARE forms of BODY.  Returns (values normalised-specifiers remaining-forms)."
  (let ((specs nil))
    (block nil
      (tagbody top
         (when (and (consp body) (consp (car body)) (eq (caar body) 'declare))
           (dolist (d (cdar body)) (setq specs (append specs (normalize-decl-spec d))))
           (setq body (cdr body))
           (go top))))
    (values specs body)))

(defun decl-spec-var (spec)
  "The variable a normalised variable declaration is about, or NIL for a free declaration."
  (case (car spec)
    (type (caddr spec))
    ((special ignore ignorable dynamic-extent) (cadr spec))
    (t nil)))

(defun decl-var-type (specs var)
  "The ty declared for VAR in SPECS, or NIL."
  (let ((ty :none))
    (dolist (s specs)
      (when (and (eq (car s) 'type) (eq (caddr s) var))
        (let ((p (parse-type (cadr s))))
          (when p (setq ty (if (eq ty :none) p (ty-meet ty p)))))))
    (if (eq ty :none) nil ty)))

(defun decl-specials (specs)
  (let ((acc nil))
    (dolist (s specs) (when (eq (car s) 'special) (setq acc (cons (cadr s) acc))))
    (nreverse acc)))

;;; ---- policy --------------------------------------------------------------------------------------

(defparameter *default-policy*
  '((speed . 1) (safety . 1) (debug . 1) (space . 1) (compilation-speed . 1)))

(defvar *policy* *default-policy*
  "The compilation policy in effect: an alist of (quality . value) and (:notinline . names).")

(defun policy-quality (q) (let ((e (assq q *policy*))) (if e (cdr e) 1)))

(defun parse-policy-delta (specs)
  "The changes to the policy that the OPTIMIZE / INLINE / NOTINLINE specifiers in SPECS ask for, or NIL."
  (let ((delta nil))
    (dolist (s specs)
      (case (car s)
        (optimize
         (dolist (q (cdr s))
           (let ((name (if (consp q) (car q) q)) (val (if (consp q) (cadr q) 3)))
             (when (and (member name '(speed safety debug space compilation-speed)) (integerp val) (<= 0 val 3))
               (setq delta (cons (cons name val) delta))))))
        (notinline (setq delta (cons (cons :notinline (remove-if-not #'symbolp (cdr s))) delta)))
        (inline (setq delta (cons (cons :inline (remove-if-not #'symbolp (cdr s))) delta)))))
    (nreverse delta)))

(defun apply-policy (delta policy)
  (dolist (d delta)
    (cond ((eq (car d) :notinline)
           (setq policy (acons :notinline (append (cdr d) (cdr (assq :notinline policy))) policy)))
          ((eq (car d) :inline)
           (setq policy (acons :notinline (remove-if (lambda (n) (memq n (cdr d))) (cdr (assq :notinline policy))) policy)))
          (t (setq policy (acons (car d) (cdr d) policy)))))
  policy)

(defun policy-inline-p (name)
  (not (memq name (cdr (assq :notinline *policy*)))))

;;; ---- DECLAIM / PROCLAIM ----------------------------------------------------------------------------

(defun ftype-result-ty (spec)
  "The ty of the (first) value of a (FUNCTION arglist result) type specifier, or NIL."
  (and (consp spec) (eq (car spec) 'function) (consp (cdr spec)) (consp (cddr spec))
       (let ((r (caddr spec)))
         (parse-type (if (and (consp r) (eq (car r) 'values)) (cadr r) r)))))

(defun %declaim (specs)
  (dolist (s specs)
    (when (consp s)
      (case (car s)
        (special (dolist (v (cdr s)) (%proclaim-special v)))
        (optimize (setq *policy* (apply-policy (parse-policy-delta (list s)) *policy*)))
        ((notinline inline) (setq *policy* (apply-policy (parse-policy-delta (list s)) *policy*)))
        (type (let ((ty (parse-type (cadr s))))
                (dolist (v (cddr s)) (%put v '%type ty))))
        (ftype (let ((ty (ftype-result-ty (cadr s))))
                 (dolist (f (cddr s)) (when (symbolp f) (%put f '%ftype-result ty))))))))
  nil)

;;; ---- support for checked declarations -------------------------------------------------------------

(defun proclaim (spec)
  "Make a declaration at run time."
  (%declaim (list spec))
  nil)

(defun %check-int-range (v lo hi)
  "Return V if it is an integer between LO and HI (NIL: unbounded); otherwise signal a type error."
  (if (and (integerp v) (or (null lo) (>= v lo)) (or (null hi) (<= v hi)))
      v
      (error 'type-error :datum v :expected-type (list 'integer (or lo '*) (or hi '*)))))
