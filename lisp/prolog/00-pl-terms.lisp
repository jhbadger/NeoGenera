;;;; 00-pl-terms.lisp -- the Prolog Tool Kit: terms, unification, the trail, standard order, errors and the Lisp bridge.
;;;;
;;;; A Prolog term is a Lisp object:
;;;;   atom       a symbol in the package PL-ATOMS (case preserved: foo is PL-ATOMS::|foo|, [] is PL-ATOMS::|[]|)
;;;;   integer    a Lisp integer (arbitrary size)         float  a Lisp double-float
;;;;   string     a Lisp string (Prolog "text" under the double_quotes=string flag)
;;;;   variable   a PLV: a binding cell, REF is NIL while unbound
;;;;   compound   a PLS: functor name (an atom) and a simple-vector of arguments; lists are '.'(Head, Tail)
;;;;   anything else (streams, Lisp structures...) is an opaque atomic "blob"
;;;; Bindings are recorded on one global trail only when the variable is older than the youngest choicepoint: every
;;;; variable carries the epoch (the count of choicepoints pushed so far) at which it was made, and *PL-HB* is the epoch
;;;; of the youngest choicepoint.  Nothing here recurses on the last argument of a compound, so long lists are safe.

(in-package :cl)

;;; ---- atoms ----------------------------------------------------------------------------------------------------

(defvar *pl-atom-package* (or (find-package "PL-ATOMS") (make-package "PL-ATOMS" :use nil))
  "The package whose symbols are Prolog atoms.")

(defun pl-atom (name)
  "The Prolog atom named NAME (a string)."
  (intern name *pl-atom-package*))

(defvar *pl-nil* (pl-atom "[]"))
(defvar *pl-true* (pl-atom "true"))
(defvar *pl-false* (pl-atom "false"))
(defvar *pl-fail* (pl-atom "fail"))
(defvar *pl-dot* (pl-atom "."))
(defvar *pl-comma* (pl-atom ","))
(defvar *pl-semi* (pl-atom ";"))
(defvar *pl-arrow* (pl-atom "->"))
(defvar *pl-softarrow* (pl-atom "*->"))
(defvar *pl-neck* (pl-atom ":-"))
(defvar *pl-dcg-arrow* (pl-atom "-->"))
(defvar *pl-cut* (pl-atom "!"))
(defvar *pl-curly* (pl-atom "{}"))
(defvar *pl-minus* (pl-atom "-"))
(defvar *pl-plus* (pl-atom "+"))
(defvar *pl-slash* (pl-atom "/"))
(defvar *pl-colon* (pl-atom ":"))
(defvar *pl-equals* (pl-atom "="))
(defvar *pl-bar* (pl-atom "|"))
(defvar *pl-call* (pl-atom "call"))
(defvar *pl-error* (pl-atom "error"))
(defvar *pl-eof* (pl-atom "end_of_file"))
(defvar *pl-var-functor* (pl-atom "$VAR"))
(defvar *pl-caret* (pl-atom "^"))
(defvar *pl-empty* (pl-atom ""))

;;; ---- structures -----------------------------------------------------------------------------------------------------

(defstruct (plv (:constructor %make-plv (epoch)) (:copier nil)) (ref nil) epoch (id nil))
(defstruct (pls (:constructor %make-pls (name args)) (:copier nil)) name args)
(defstruct (pltv (:constructor %make-pltv (index)) (:copier nil)) index)

(defvar *pl-var-counter* 0)
(defvar *pl-epoch* 0 "The number of choicepoints pushed so far.")
(defvar *pl-hb* 0 "The epoch of the youngest choicepoint: older variables must be trailed when bound.")
(defvar *pl-trail* (make-array 4096))
(defvar *pl-tn* 0 "The number of entries on the trail.")

(defmacro %deref (form)
  (let ((v (gensym)))
    `(let ((,v ,form)) (loop (if (and (plv-p ,v) (plv-ref ,v)) (setq ,v (plv-ref ,v)) (return ,v))))))

(defun pl-deref (x) (%deref x))

(defun pl-new-var () (%make-plv *pl-epoch*))

(defun pl-var-id (v)
  (or (plv-id v) (setf (plv-id v) (setq *pl-var-counter* (+ *pl-var-counter* 1)))))

(defun %pl-trail-push (x)
  (let ((tr *pl-trail*) (n *pl-tn*))
    (when (>= n (length tr))
      (let ((nt (make-array (* 2 (length tr)) :initial-element nil)))
        (replace nt tr)
        (setq *pl-trail* nt tr nt)))
    (setf (svref tr n) x)
    (setq *pl-tn* (+ n 1))))

(defun pl-bind (v val)
  "Bind the unbound variable V to VAL, trailing the binding when V is older than the youngest choicepoint."
  (setf (plv-ref v) val)
  (when (< (plv-epoch v) *pl-hb*) (%pl-trail-push v)))

(defun pl-trail-undo-function (fn)
  "Record FN to be called when backtracking past this point."
  (%pl-trail-push fn))

(defun %pl-undo-to (mark)
  "Undo every binding made since the trail had MARK entries."
  (let ((tr *pl-trail*))
    (loop
      (when (<= *pl-tn* mark) (return))
      (let* ((n (- *pl-tn* 1)) (x (svref tr n)))
        (setf (svref tr n) nil)
        (setq *pl-tn* n)
        (if (plv-p x) (setf (plv-ref x) nil) (funcall x))))))

;;; ---- constructors and tests -----------------------------------------------------------------------------------------

(defun pl-s (name &rest args)
  "The compound NAME(ARGS...); NAME is a string or an atom.  With no arguments the atom."
  (let ((n (if (stringp name) (pl-atom name) name)))
    (if args (%make-pls n (apply #'vector args)) n)))

(defun pl-cons (head tail) (%make-pls *pl-dot* (vector head tail)))

(defun pl-compound-p (x) (pls-p x))
(defun pl-atom-p (x) (and x (symbolp x)))
(defun pl-callable-p (x) (or (pls-p x) (and x (symbolp x))))
(defun pl-atomic-p (x) (not (or (plv-p x) (pls-p x))))
(defun pl-arity (x) (if (pls-p x) (length (pls-args x)) 0))
(defun pl-name (x) (if (pls-p x) (pls-name x) x))

(defun pl-arg (x i) (svref (pls-args x) i))

(defun pl-is-cons (x) (and (pls-p x) (eq (pls-name x) *pl-dot*) (= (length (pls-args x)) 2)))
(defun pl-head (x) (svref (pls-args x) 0))
(defun pl-tail (x) (svref (pls-args x) 1))

(defun pl-functor-p (x name arity)
  "True when X (dereferenced) is a compound with functor NAME (an atom) and ARITY."
  (and (pls-p x) (eq (pls-name x) name) (= (length (pls-args x)) arity)))

(defun pl-list-to-lisp (term)
  "The elements of the Prolog list TERM (not dereferenced), and its status: :PROPER, :PARTIAL (it ends in an unbound
variable), :IMPROPER or :CYCLIC (a list that loops back on itself; found by Brent's method)."
  (let ((acc nil) (slow nil) (steps 0) (limit 2))
    (loop
      (setq term (%deref term))
      (cond ((eq term *pl-nil*) (return (values (nreverse acc) :proper)))
            ((pl-is-cons term)
             (when (and (> steps 0) (eq term slow)) (return (values (nreverse acc) :cyclic)))
             (push (svref (pls-args term) 0) acc)
             (setq term (svref (pls-args term) 1))
             (setq steps (+ steps 1))
             (when (>= steps limit) (setq slow (%deref term) steps 0 limit (* 2 limit))))
            ((plv-p term) (return (values (nreverse acc) :partial)))
            (t (return (values (nreverse acc) :improper)))))))

(defun pl-list-p (term)
  (let ((slow nil) (steps 0) (limit 2))
    (loop
      (setq term (%deref term))
      (cond ((eq term *pl-nil*) (return t))
            ((pl-is-cons term)
             (when (and (> steps 0) (eq term slow)) (return nil))
             (setq term (svref (pls-args term) 1))
             (setq steps (+ steps 1))
             (when (>= steps limit) (setq slow (%deref term) steps 0 limit (* 2 limit))))
            (t (return nil))))))

(defun lisp-to-pl-list (list &optional (tail *pl-nil*))
  "A Prolog list of the elements of the Lisp list LIST (taken as terms), ending in TAIL."
  (let ((acc tail))
    (dolist (x (reverse list) acc) (setq acc (pl-cons x acc)))))

(defun pl-ground-p (term)
  (loop
    (setq term (%deref term))
    (cond ((plv-p term) (return nil))
          ((pls-p term)
           (let* ((args (pls-args term)) (n (length args)))
             (dotimes (i (- n 1)) (unless (pl-ground-p (svref args i)) (return-from pl-ground-p nil)))
             (setq term (svref args (- n 1)))))
          (t (return t)))))

(defun pl-term-variables (term &optional (acc nil))
  "The distinct unbound variables of TERM in depth-first left-to-right order (ACC is the reversed list so far)."
  (loop
    (setq term (%deref term))
    (cond ((plv-p term) (return (if (memq term acc) acc (cons term acc))))
          ((pls-p term)
           (let* ((args (pls-args term)) (n (length args)))
             (dotimes (i (- n 1)) (setq acc (pl-term-variables (svref args i) acc)))
             (setq term (svref args (- n 1)))))
          (t (return acc)))))

(defun pl-variables-of (term) (nreverse (pl-term-variables term nil)))

;;; ---- unification --------------------------------------------------------------------------------------------------

(defun pl-unify (a b)
  "Unify A and B (no occurs check), binding variables and trailing as needed.  True on success; bindings made before a
failure are left for the caller to undo."
  (loop
    (setq a (%deref a) b (%deref b))
    (cond ((eq a b) (return t))
          ((plv-p a) (pl-bind a b) (return t))
          ((plv-p b) (pl-bind b a) (return t))
          ((pls-p a)
           (unless (and (pls-p b) (eq (pls-name a) (pls-name b))) (return nil))
           (let* ((aa (pls-args a)) (ba (pls-args b)) (n (length aa)))
             (unless (= n (length ba)) (return nil))
             (dotimes (i (- n 1)) (unless (pl-unify (svref aa i) (svref ba i)) (return-from pl-unify nil)))
             (setq a (svref aa (- n 1)) b (svref ba (- n 1)))))
          ((numberp a) (return (and (numberp b) (eql a b))))
          ((stringp a) (return (and (stringp b) (string= a b))))
          (t (return nil)))))

(defun pl-occurs-in (v term)
  (loop
    (setq term (%deref term))
    (cond ((eq term v) (return t))
          ((pls-p term)
           (let* ((args (pls-args term)) (n (length args)))
             (dotimes (i (- n 1)) (when (pl-occurs-in v (svref args i)) (return-from pl-occurs-in t)))
             (setq term (svref args (- n 1)))))
          (t (return nil)))))

(defun pl-unify-oc (a b)
  "Unification with the occurs check."
  (loop
    (setq a (%deref a) b (%deref b))
    (cond ((eq a b) (return t))
          ((plv-p a) (when (pl-occurs-in a b) (return nil)) (pl-bind a b) (return t))
          ((plv-p b) (when (pl-occurs-in b a) (return nil)) (pl-bind b a) (return t))
          ((pls-p a)
           (unless (and (pls-p b) (eq (pls-name a) (pls-name b))) (return nil))
           (let* ((aa (pls-args a)) (ba (pls-args b)) (n (length aa)))
             (unless (= n (length ba)) (return nil))
             (dotimes (i (- n 1)) (unless (pl-unify-oc (svref aa i) (svref ba i)) (return-from pl-unify-oc nil)))
             (setq a (svref aa (- n 1)) b (svref ba (- n 1)))))
          ((numberp a) (return (and (numberp b) (eql a b))))
          ((stringp a) (return (and (stringp b) (string= a b))))
          (t (return nil)))))

;;; ---- copying ------------------------------------------------------------------------------------------------------

(defun %pl-copy (term vars)
  "A copy of TERM with fresh variables; VARS is a cons whose car is the alist old-variable -> new-variable."
  (let ((head nil) (parent nil) (count 0))
    (loop
      (setq term (%deref term))
      (setq count (+ count 1))
      (when (> count 3000000) (pl-resource-error "memory"))      ; a cyclic term would otherwise copy until the heap is gone
      (let ((new term) (next nil) (done t))
        (cond ((plv-p term)
               (let ((c (assq term (car vars))))
                 (if c
                     (setq new (cdr c))
                     (let ((nv (pl-new-var))) (setf (car vars) (cons (cons term nv) (car vars))) (setq new nv)))))
              ((pls-p term)
               (let* ((args (pls-args term)) (n (length args)) (nargs (make-array n)))
                 (dotimes (i (- n 1)) (setf (svref nargs i) (%pl-copy (svref args i) vars)))
                 (setq new (%make-pls (pls-name term) nargs) next (svref args (- n 1)) done nil)
                 (if parent (setf (svref parent (- (length parent) 1)) new) (setq head new))
                 (setq parent nargs))))
        (cond (done
               (if parent (setf (svref parent (- (length parent) 1)) new) (setq head new))
               (return head))
              (t (setq term next)))))))

(defun pl-copy-term (term)
  "A copy of TERM with fresh variables (shared variables stay shared)."
  (%pl-copy term (list nil)))

(defun pl-resolve (term)
  "TERM with all bindings substituted, keeping its unbound variables (used for exception balls and results)."
  (let ((head nil) (parent nil) (count 0))
    (loop
      (setq term (%deref term))
      (setq count (+ count 1))
      (when (> count 3000000) (pl-resource-error "memory"))
      (let ((next nil) (done t))
        (cond ((pls-p term)
               (let* ((args (pls-args term)) (n (length args)) (nargs (make-array n)))
                 (dotimes (i (- n 1)) (setf (svref nargs i) (pl-resolve (svref args i))))
                 (let ((new (%make-pls (pls-name term) nargs)))
                   (if parent (setf (svref parent (- (length parent) 1)) new) (setq head new))
                   (setq parent nargs next (svref args (- n 1)) done nil)))))
        (when done
          (if parent (setf (svref parent (- (length parent) 1)) term) (setq head term))
          (return head))
        (setq term next)))))

;;; ---- standard order of terms ----------------------------------------------------------------------------------------

(defun %pl-rank (x)
  (cond ((plv-p x) 0) ((numberp x) 1) ((symbolp x) 3) ((stringp x) 4) ((pls-p x) 5) (t 2)))

(defun %pl-string-compare (a b)
  (cond ((string= a b) 0) ((string< a b) -1) (t 1)))

(defun pl-compare-numbers (a b)
  (cond ((= a b) (cond ((and (floatp a) (not (floatp b))) -1) ((and (floatp b) (not (floatp a))) 1) (t 0)))
        ((< a b) -1)
        (t 1)))

(defun pl-compare (a b)
  "Compare A and B in the standard order of terms: -1, 0 or 1.  Var < Number < Atom < String < Compound."
  (loop
    (setq a (%deref a) b (%deref b))
    (when (eq a b) (return 0))
    (let ((ra (%pl-rank a)) (rb (%pl-rank b)))
      (cond ((/= ra rb) (return (if (< ra rb) -1 1)))
            ((= ra 0) (return (let ((x (pl-var-id a)) (y (pl-var-id b))) (cond ((< x y) -1) ((> x y) 1) (t 0)))))
            ((= ra 1) (return (pl-compare-numbers a b)))
            ((= ra 3) (return (%pl-string-compare (symbol-name a) (symbol-name b))))
            ((= ra 4) (return (%pl-string-compare a b)))
            ((= ra 5)
             (let* ((aa (pls-args a)) (ba (pls-args b)) (n (length aa)) (m (length ba)))
               (cond ((< n m) (return -1)) ((> n m) (return 1)))
               (let ((c (%pl-string-compare (symbol-name (pls-name a)) (symbol-name (pls-name b)))))
                 (unless (= c 0) (return c)))
               (dotimes (i (- n 1))
                 (let ((c (pl-compare (svref aa i) (svref ba i)))) (unless (= c 0) (return-from pl-compare c))))
               (setq a (svref aa (- n 1)) b (svref ba (- n 1)))))
            (t (return (%pl-string-compare (princ-to-string a) (princ-to-string b))))))))

(defun pl-identical (a b) (= 0 (pl-compare a b)))

;;; ---- errors ----------------------------------------------------------------------------------------------------------

(define-condition pl-error (error)
  ((ball :initarg :ball :reader pl-error-ball))
  (:report (lambda (c s) (format s "~A" (pl-error-message (pl-error-ball c))))))

(define-condition pl-halt (error)
  ((code :initarg :code :reader pl-halt-code))
  (:report (lambda (c s) (format s "halt(~A)" (pl-halt-code c)))))

(defun pl-throw (ball)
  "Throw the Prolog exception BALL (a copy of it is thrown)."
  (error 'pl-error :ball (pl-copy-term ball)))

(defun pl-error-term (formal &optional context)
  (%make-pls *pl-error* (vector formal (or context (pl-new-var)))))

(defun pl-instantiation-error ()
  (pl-throw (pl-error-term (pl-atom "instantiation_error"))))
(defun pl-type-error (type culprit)
  (pl-throw (pl-error-term (pl-s "type_error" (pl-atom type) culprit))))
(defun pl-domain-error (domain culprit)
  (pl-throw (pl-error-term (pl-s "domain_error" (pl-atom domain) culprit))))
(defun pl-existence-error (kind what)
  (pl-throw (pl-error-term (pl-s "existence_error" (pl-atom kind) what))))
(defun pl-permission-error (action type culprit)
  (pl-throw (pl-error-term (pl-s "permission_error" (pl-atom action) (pl-atom type) culprit))))
(defun pl-representation-error (what)
  (pl-throw (pl-error-term (pl-s "representation_error" (pl-atom what)))))
(defun pl-evaluation-error (what)
  (pl-throw (pl-error-term (pl-s "evaluation_error" (pl-atom what)))))
(defun pl-resource-error (what)
  (pl-throw (pl-error-term (pl-s "resource_error" (pl-atom what)))))
(defun pl-syntax-error-term (message &optional (line nil) (col nil) (file nil))
  (pl-error-term (pl-s "syntax_error" (pl-atom message))
                 (pl-s "context" (pl-s "position" (or file *pl-nil*) (or line 0) (or col 0)) (pl-new-var))))
(defun pl-syntax-error (message &optional line col file)
  (pl-throw (pl-syntax-error-term message line col file)))

(defun pl-indicator (name arity) (%make-pls *pl-slash* (vector name arity)))

;;; ---- text and numbers --------------------------------------------------------------------------------------------------

(defun pl-format-float (x)
  "The text of the float X as Prolog prints it: always with a decimal point (1.0, 1.5, 1.0e20)."
  (let ((s (prin1-to-string x)))
    (cond ((find #\d s) (substitute #\e #\d s))
          (t s))))

(defun pl-number-string (n)
  (cond ((integerp n) (format nil "~D" n))
        ((floatp n) (pl-format-float n))
        (t (princ-to-string n))))

(defun pl-code-list-string (term)
  "The string of the Prolog list TERM of character codes or one-character atoms, or NIL if it is not one."
  (multiple-value-bind (items status) (pl-list-to-lisp term)
    (and (eq status :proper)
         (let ((out (make-string (length items))) (i 0))
           (dolist (x items out)
             (setq x (%deref x))
             (cond ((and (integerp x) (>= x 0) (< x 1114112)) (setf (char out i) (code-char x)))
                   ((and (symbolp x) x (= (length (symbol-name x)) 1)) (setf (char out i) (char (symbol-name x) 0)))
                   (t (return-from pl-code-list-string nil)))
             (setq i (+ i 1)))))))

(defun pl-text (x)
  "The text (a string) denoted by X: an atom, string, number or list of codes/characters; NIL otherwise."
  (setq x (%deref x))
  (cond ((stringp x) x)
        ((eq x *pl-nil*) "")
        ((symbolp x) (and x (symbol-name x)))
        ((numberp x) (pl-number-string x))
        ((pl-is-cons x) (pl-code-list-string x))
        (t nil)))

(defun pl-string-codes (s)
  (let ((acc nil)) (dotimes (i (length s)) (push (char-code (char s i)) acc)) (lisp-to-pl-list (nreverse acc))))

(defun pl-string-chars (s)
  (let ((acc nil)) (dotimes (i (length s)) (push (pl-atom (string (char s i))) acc)) (lisp-to-pl-list (nreverse acc))))

;;; ---- the Lisp bridge ------------------------------------------------------------------------------------------------------

(defun pl-from-lisp (x)
  "Convert the Lisp value X to a Prolog term: NIL is [], T is true, a symbol the atom of its lower-case name, a list a
Prolog list, strings and numbers themselves; any other object is passed as an opaque blob."
  (cond ((null x) *pl-nil*)
        ((eq x t) *pl-true*)
        ((or (plv-p x) (pls-p x)) x)
        ((symbolp x) (if (eq (symbol-package x) *pl-atom-package*) x (pl-atom (string-downcase (symbol-name x)))))
        ((consp x)
         (let ((acc nil) (tail *pl-nil*))
           (loop (cond ((consp x) (push (pl-from-lisp (car x)) acc) (setq x (cdr x)))
                       (t (unless (null x) (setq tail (pl-from-lisp x))) (return))))
           (lisp-to-pl-list (nreverse acc) tail)))
        (t x)))

(defun pl-to-lisp (x)
  "Convert the Prolog term X to a Lisp value (the inverse of PL-FROM-LISP): [] is NIL, true is T, other atoms are symbols
with the upper-cased name in the current package, lists are lists, other compounds are lists (FUNCTOR ARG...), an unbound
variable a keyword :_Gn."
  (setq x (%deref x))
  (cond ((eq x *pl-nil*) nil)
        ((eq x *pl-true*) t)
        ((plv-p x) (intern (format nil "_G~D" (pl-var-id x)) "KEYWORD"))
        ((symbolp x) (intern (string-upcase (symbol-name x)) *package*))
        ((pl-is-cons x)
         (multiple-value-bind (items status) (pl-list-to-lisp x)
           (when (eq status :cyclic) (pl-type-error "list" x))
           (let ((r (mapcar #'pl-to-lisp items)))
             (if (eq status :proper)
                 r
                 ;; a partial or improper list: the dotted tail is the converted last tail
                 (let ((tail x)) (dolist (i items) (declare (ignore i)) (setq tail (%deref (svref (pls-args tail) 1))))
                   (append r (pl-to-lisp tail)))))))
        ((pls-p x) (cons (intern (string-upcase (symbol-name (pls-name x))) *package*)
                         (map 'list #'pl-to-lisp (pls-args x))))
        (t x)))
