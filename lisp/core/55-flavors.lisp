;;;; 55-flavors.lisp -- Flavors: the Lisp Machine / Symbolics message-passing object system, layered on CLOS.
;;;;
;;;; A flavor is a CLOS class (so instances are ordinary standard instances: TYPEP, MAKE-INSTANCE, the Inspector and
;;;; DESCRIBE all work) plus a %FLAVOR record kept on the flavor name's %FLAVOR-DEF property.  The record holds what
;;;; CLOS has no place for: the flavor's own methods (a flavor method belongs to a flavor and a message, not to a
;;;; generic function), its method-combination declarations and its init-plist defaults.
;;;;
;;;; Differences from CLOS that this file implements on purpose:
;;;;  * Component order is the Flavors one: depth-first, left to right, a flavor that appears twice is kept at its LAST
;;;;    position (so a shared base flavor follows every flavor that includes it), included flavors after that and
;;;;    VANILLA-FLAVOR last.  %FINALIZE-CLASS (52-clos.lisp) asks %FLAVOR-CLASS-CPL for it instead of using C3.
;;;;  * Under the default :DAEMON combination only the MOST SPECIFIC primary method runs; there is no CALL-NEXT-METHOD.
;;;;    :BEFORE methods run most specific first, :AFTER methods least specific first, :AROUND methods and whoppers
;;;;    wrap the lot.  Other combinations (:PROGN :OR :AND :APPEND :NCONC :LIST :MAX :MIN :+) come from the
;;;;    :METHOD-COMBINATION defflavor option.
;;;;  * Instance variables are variables inside a method body (symbol macros over SLOT-VALUE) and SELF is the instance.
;;;;
;;;; Also here: instances are funcallable ((funcall instance :message args...) is SEND; FUNCTIONP is true), the
;;;; :PASS-ON and :INVERSE-LIST combinations, :ALIAS-FLAVOR, :RUN-TIME-ALTERNATIVES, :INSTANTIATION-FLAVOR-FUNCTION,
;;;; :SPECIAL-INSTANCE-VARIABLES and WITH-SELF-VARIABLES-BOUND.  These were written from memory of the Lisp Machine
;;;; manual (the manual could not be consulted): the exact syntax of :RUN-TIME-ALTERNATIVES and the return value of
;;;; :INVERSE-LIST in particular are this system's reading of it (see docs/manual/flavors-manual.md).
;;;;
;;;; What is not here: instance variables are visible to ordinary functions only through :SPECIAL-INSTANCE-VARIABLES or
;;;; WITH-SELF-VARIABLES-BOUND, which copy the values in and out (they are not aliases of the slots).

(in-package :cl)

(defvar self nil
  "The instance a message is being handled for.  Inside a method body SELF is lexical and is captured by closures;
elsewhere it is the special variable that SEND binds, so a function called from a method can use it too.")

(defvar *all-flavor-names* nil "The names of all the flavors that have been defined, most recent first.")
(defvar *flavor-epoch* 0 "Counts changes to flavors and methods; a flavor's combined methods are rebuilt when it moves.")
(defvar *flavors* nil "The names of the defined flavors, most recently defined first.")

(defstruct (%flavor (:conc-name %flv-) (:constructor %make-flavor) (:copier nil))
  name (ivars nil) (components nil) (included nil) (required-flavors nil) (required-ivars nil) (required-methods nil)
  (required-keywords nil) (gettable nil) (settable nil) (inittable nil) (init-keywords nil) (default-init-plist nil)
  (default-handler nil) (abstract nil) (documentation nil) (combinations nil) (no-vanilla nil)
  (methods nil) (cache nil) (epoch -1) (checked -1)
  (special nil) (alternatives nil) (instantiation-function nil))

(define-condition unclaimed-message (cell-error)
  ((instance :initarg :instance :initform nil :reader unclaimed-message-object)
   (arguments :initarg :arguments :initform nil :reader unclaimed-message-arguments))
  (:report (lambda (c s)
             (format s "The object ~S received a ~S message, which went unhandled."
                     (unclaimed-message-object c) (cell-error-name c)))))

(defun %find-flavor (name &optional (errorp t))
  (or (and (symbolp name) name (get name '%flavor-def))
      (and errorp (error "There is no flavor named ~S" name))))

(defun %flavor-changed () (setq *flavor-epoch* (+ *flavor-epoch* 1)))

(defun %keyword-of (symbol) (intern (symbol-name symbol) "KEYWORD"))

;;; ---- component order -------------------------------------------------------------------------------------

(defun %flavor-walk (name)
  "NAME and its components: depth first, a flavor before the flavors it is made of, each flavor only at its first
occurrence (so a cycle ends)."
  (let ((seen nil))
    (labels ((w (n)
               (unless (memq n seen)
                 (setq seen (cons n seen))
                 (dolist (c (%flv-components (%find-flavor n))) (w c)))))
      (w name))
    (reverse seen)))

(defun %missing-included (lin)
  "The first included flavor that is not in LIN, as (flavor . position of the last flavor of LIN that includes it)."
  (dolist (n lin nil)
    (dolist (inc (%flv-included (%find-flavor n)))
      (unless (memq inc lin)
        (let ((pos 0) (i 0))
          (dolist (m lin)
            (when (memq inc (%flv-included (%find-flavor m))) (setq pos i))
            (setq i (+ i 1)))
          (return-from %missing-included (cons inc pos)))))))

(defun %flavor-lin (name)
  "The flavors NAME is made of, most specific first: components depth first (first occurrence kept), each included
flavor right after the last flavor that includes it, VANILLA-FLAVOR last."
  (setq name (%flv-name (%find-flavor name)))            ; an alias flavor stands for its target
  (let ((lin (%flavor-walk name)))
    (loop
      (let ((m (%missing-included lin)))
        (when (null m) (return))
        (let ((new (remove-if (lambda (x) (memq x lin)) (%flavor-walk (car m)))))
          (setq lin (append (subseq lin 0 (+ (cdr m) 1)) new (nthcdr (+ (cdr m) 1) lin))))))
    (if (%flv-no-vanilla (%find-flavor name))
        lin
        (append (remove 'vanilla-flavor lin) '(vanilla-flavor)))))

(defun %flavor-all-ivars (name)
  "Every instance variable of flavor NAME, base flavors' first."
  (let ((acc nil))
    (dolist (n (reverse (%flavor-lin name)))
      (dolist (v (%flv-ivars (%find-flavor n)))
        (unless (memq (car v) acc) (setq acc (cons (car v) acc)))))
    (nreverse acc)))

(defun %flavor-class-cpl (class)
  "The class precedence list of a flavor's class: its flavors in Flavors order, then STANDARD-OBJECT and T."
  (append (mapcar #'find-class (%flavor-lin (%cls-name class)))
          (list (find-class 'standard-object) (find-class 't))))

(defun %instance-flavor (x)
  (and (structp x) (%std-instance-p x) (get (%cls-name (%struct-type x)) '%flavor-def)))

(defun flavor-instance-p (x)
  "True if X is an instance of a flavor."
  (if (%instance-flavor x) t nil))

(defun flavor-components (flavor)
  "The names of the flavors FLAVOR is made of, most specific first (FLAVOR itself first, VANILLA-FLAVOR last)."
  (%flavor-lin (if (symbolp flavor) flavor (%cls-name (class-of flavor)))))

;;; ---- DEFFLAVOR ------------------------------------------------------------------------------------------

(defmacro defflavor (name ivars components &rest options)
  "Define the flavor NAME.  IVARS are symbols or (symbol default-form); COMPONENTS are the flavors it is built from.
Options: :gettable-instance-variables :settable-instance-variables :inittable-instance-variables (each alone for all
of the flavor's variables, or with a list of names), :init-keywords, :default-init-plist, :required-instance-variables,
:required-methods, :required-flavors, :required-init-keywords, :included-flavors, :default-handler, :method-combination,
:no-vanilla-flavor, :abstract-flavor, :special-instance-variables, :run-time-alternatives,
:instantiation-flavor-function, :alias-flavor (the flavor is another name for a flavor), :documentation."
  (let ((specs (mapcar (lambda (v)
                         (if (consp v)
                             (list 'cons (list 'quote (car v)) (if (cdr v) (list 'lambda nil (cadr v)) nil))
                             (list 'cons (list 'quote v) nil)))
                       ivars))
        (opts (mapcar (lambda (o)
                        (if (and (consp o) (eq (car o) :default-init-plist))
                            (let ((acc nil))
                              (do ((p (cdr o) (cddr p))) ((null p))
                                (setq acc (cons (list 'cons (list 'quote (car p)) (list 'lambda nil (cadr p))) acc)))
                              (cons 'list (cons :default-init-plist (nreverse acc))))
                            (list 'quote o)))
                      options)))
    (list '%defflavor (list 'quote name) (cons 'list specs) (list 'quote components) (cons 'list opts))))

(defparameter *flavor-combination-types*
  '(:daemon :daemon-with-or :daemon-with-and :daemon-with-override :progn :or :and :append :nconc :list :inverse-list :pass-on :max :min :+ :case)
  "The method combination types a :METHOD-COMBINATION option of DEFFLAVOR accepts.")

(defun %opt-names (args own current)
  (let ((names (if args args own)))
    (let ((acc current))
      (dolist (n names) (unless (memq n acc) (setq acc (append acc (list n)))))
      acc)))

(defvar *flavor-aliases* nil "Alist (alias . flavor) of the flavors defined with :ALIAS-FLAVOR.")

(defun %define-alias-flavor (name target)
  (let ((rec (%find-flavor target)))
    (when (and (get name '%flavor-def) (not (assq name *flavor-aliases*)))
      (error "DEFFLAVOR ~S: cannot be an alias of ~S, there is already a flavor of that name" name target))
    (setf (get name '%flavor-def) rec)
    (%put name '%class (find-class (%flv-name rec)))
    (setq *flavor-aliases* (cons (cons name (%flv-name rec)) (remove name *flavor-aliases* :key #'car)))
    (%flavor-changed)
    name))

(defun %defflavor (name specs components options)
  (let ((alias (find :alias-flavor options :key (lambda (o) (and (consp o) (car o))))))
    (when alias (return-from %defflavor (%define-alias-flavor name (cadr alias)))))
  (dolist (c components) (%find-flavor c))
  (let* ((old (%find-flavor name nil))
         (f (or old (%make-flavor :name name :cache (make-hash-table :test 'eq))))
         (own (mapcar #'car specs)))
    (setf (%flv-ivars f) specs (%flv-components f) components (%flv-included f) nil (%flv-required-flavors f) nil
          (%flv-required-ivars f) nil (%flv-required-methods f) nil (%flv-required-keywords f) nil (%flv-gettable f) nil
          (%flv-settable f) nil (%flv-inittable f) nil (%flv-init-keywords f) nil (%flv-default-init-plist f) nil
          (%flv-default-handler f) nil (%flv-documentation f) nil (%flv-combinations f) nil (%flv-no-vanilla f) nil (%flv-abstract f) nil
          (%flv-special f) nil (%flv-alternatives f) nil (%flv-instantiation-function f) nil)
    (dolist (o options)
      (let ((key (if (consp o) (car o) o)) (args (if (consp o) (cdr o) nil)))
        (case key
          ((:gettable-instance-variables) (setf (%flv-gettable f) (%opt-names args own (%flv-gettable f))))
          ((:settable-instance-variables) (setf (%flv-settable f) (%opt-names args own (%flv-settable f))))
          ((:inittable-instance-variables :initable-instance-variables)
           (setf (%flv-inittable f) (%opt-names args own (%flv-inittable f))))
          (:init-keywords (setf (%flv-init-keywords f) (append (%flv-init-keywords f) args)))
          (:default-init-plist (setf (%flv-default-init-plist f) (append (%flv-default-init-plist f) args)))
          (:required-instance-variables (setf (%flv-required-ivars f) (append (%flv-required-ivars f) args)))
          (:required-methods (setf (%flv-required-methods f) (append (%flv-required-methods f) args)))
          (:required-flavors (setf (%flv-required-flavors f) (append (%flv-required-flavors f) args)))
          (:required-init-keywords (setf (%flv-required-keywords f) (append (%flv-required-keywords f) args)))
          (:included-flavors (setf (%flv-included f) (append (%flv-included f) args)))
          (:default-handler (setf (%flv-default-handler f) (car args)))
          (:method-combination
           (dolist (spec args)
             (let* ((type (car spec)) (rest (cdr spec))
                    ;; the ordering is a keyword, or the first element of a list that also gives the arguments of the
                    ;; combined method, as in (:pass-on (:base-flavor-last arg1 arg2) :message)
                    (order (cond ((member (car rest) '(:base-flavor-first :base-flavor-last)) (car rest))
                                 ((and (consp (car rest)) (member (caar rest) '(:base-flavor-first :base-flavor-last))) (caar rest))
                                 (t nil)))
                    (msgs (if order (cdr rest) rest)))
               (unless (member type *flavor-combination-types*)
                 (error "DEFFLAVOR ~S: ~S is not a method combination type" name type))
               (setf (%flv-combinations f) (append (%flv-combinations f) (list (list* type order msgs)))))))
          (:no-vanilla-flavor (setf (%flv-no-vanilla f) t))
          (:documentation (setf (%flv-documentation f) (car args)))
          (:abstract-flavor (setf (%flv-abstract f) t))
          (:special-instance-variables
           ;; alone: all of the flavor's own variables; otherwise the named ones
           (setf (%flv-special f) (if args (append (if (listp (%flv-special f)) (%flv-special f) nil) args) :all)))
          (:run-time-alternatives (setf (%flv-alternatives f) (append (%flv-alternatives f) args)))
          (:instantiation-flavor-function (setf (%flv-instantiation-function f) (car args)))
          ((:ordered-instance-variables :outside-accessible-instance-variables) nil)
          (t (error "DEFFLAVOR ~S: unknown option ~S" name key)))))
    ;; settable variables are gettable and inittable too
    (dolist (v (%flv-settable f))
      (unless (memq v (%flv-gettable f)) (setf (%flv-gettable f) (append (%flv-gettable f) (list v))))
      (unless (memq v (%flv-inittable f)) (setf (%flv-inittable f) (append (%flv-inittable f) (list v)))))
    (when (and (eq name 'vanilla-flavor) (not (%flv-no-vanilla f))) (setf (%flv-no-vanilla f) t))
    (setf (get name '%flavor-def) f)
    (setq *flavor-aliases* (remove name *flavor-aliases* :key #'car))
    (dolist (v (%flavor-own-special-ivars f)) (%proclaim-special v))
    (unless (memq name *flavors*) (setq *flavors* (cons name *flavors*)))
    (setq *all-flavor-names* *flavors*)
    (%flavor-lin name)                     ; signals an error now if a component or included flavor is missing
    (let ((slotds (mapcar (lambda (spec)
                            (%make-slotd :name (car spec)
                                         :initargs (if (memq (car spec) (%flv-inittable f)) (list (%keyword-of (car spec))) nil)
                                         :initfn (cdr spec) :allocation :instance :type t))
                          specs)))
      ;; an inittable variable inherited from a component gets its initarg through a slot with no initform here
      (dolist (v (%flv-inittable f))
        (unless (memq v own)
          (setq slotds (append slotds (list (%make-slotd :name v :initargs (list (%keyword-of v))
                                                         :allocation :instance :type t))))))
      (%ensure-class name
                     (append components (if (or (%flv-no-vanilla f) (memq 'vanilla-flavor components)) nil '(vanilla-flavor)))
                     slotds
                     (mapcar (lambda (d) (cons (car d) (cdr d))) (%flv-default-init-plist f))
                     nil (%flv-documentation f) nil))
    ;; flavors that include this one without being its subclass need their class order recomputed
    (dolist (other *flavors*)
      (unless (eq other name)
        (when (memq name (%flavor-lin other)) (%finalize-class (find-class other)))))
    (dolist (v (%flv-gettable f))
      (%install-method name (%keyword-of v) nil
                       (let ((v v)) (lambda (obj) (slot-value obj v))) t))
    (dolist (v (%flv-settable f))
      (%install-method name (intern (%string-concat "SET-" (symbol-name v)) "KEYWORD") nil
                       (let ((v v)) (lambda (obj value) (setf (slot-value obj v) value))) t))
    (%flavor-changed)
    name))

;;; ---- methods ----------------------------------------------------------------------------------------------

(defparameter *flavor-method-types* '(:before :after :around :inverse-around :whopper :default :override)
  "The method types every method combination style accepts or that DAEMON combination adds.")

(defun %valid-method-type-p (type)
  (or (null type) (member type *flavor-method-types*) (member type *flavor-combination-types*)
      (and (consp type) (eq (car type) :case))))

(defun %same-method-p (e msg type) (and (eq (car e) msg) (equal (cadr e) type)))

(defun %install-method (fname msg type fn auto)
  "Install FN (a function of the instance and the message's arguments) as FNAME's method for MSG of TYPE.  A method made
by a DEFFLAVOR option (AUTO) never replaces one the user wrote."
  (let* ((f (%find-flavor fname))
         (old (find-if (lambda (e) (%same-method-p e msg type)) (%flv-methods f))))
    (unless (%valid-method-type-p type)
      (error "~S is not a method type (flavor ~S, message ~S)" type fname msg))
    (unless (and auto old (not (cadddr old)))
      (setf (%flv-methods f) (cons (list msg type fn auto) (remove old (%flv-methods f))))
      (%flavor-changed))
    msg))

(defun %install-method-checked (fname type msg fn)
  (%install-method fname msg type fn nil)
  (list fname type msg))

(defun %parse-method-spec (spec)
  "(flavor message), (flavor type message) or (flavor :case message suboperation) -> (values flavor type message)."
  (let ((n (if (listp spec) (length spec) 0)))
    (unless (and (>= n 2) (<= n 4) (symbolp (car spec)))
      (error "Bad flavor method name ~S: expected (flavor [type] message)" spec))
    (cond ((= n 2) (values (car spec) nil (cadr spec)))
          ((= n 3) (values (car spec) (if (eq (cadr spec) :wrapper) :whopper (cadr spec)) (caddr spec)))
          ((eq (cadr spec) :case) (values (car spec) (list :case (cadddr spec)) (caddr spec)))
          (t (error "Bad flavor method name ~S: only :CASE methods take a suboperation" spec)))))

(defun %flavor-own-special-ivars (f)
  "The instance variables flavor record F declares special (:SPECIAL-INSTANCE-VARIABLES)."
  (let ((sp (%flv-special f)))
    (cond ((eq sp :all) (mapcar #'car (%flv-ivars f)))
          ((listp sp) sp)
          (t nil))))

(defun %flavor-special-ivars (fname)
  "The special instance variables of flavor FNAME and its components."
  (let ((acc nil))
    (dolist (n (%flavor-lin fname))
      (dolist (v (%flavor-own-special-ivars (%find-flavor n))) (unless (memq v acc) (setq acc (cons v acc)))))
    (nreverse acc)))

(defun %call-with-ivars-bound (instance names thunk)
  "Call THUNK with the instance variables NAMES of INSTANCE bound as special variables to their values, and copy
the values that changed back into the instance when THUNK returns or exits (the variables are copies, not aliases of
the slots)."
  (let ((bound nil) (vals nil))
    (dolist (n names)
      (when (slot-boundp instance n) (setq bound (cons n bound)) (setq vals (cons (slot-value instance n) vals))))
    (let ((originals vals))
      (progv bound vals
        (unwind-protect (funcall thunk)
          ;; only a variable that was changed is copied back, so a slot set in the meantime is not overwritten
          (let ((o originals))
            (dolist (n bound)
              (unless (eq (symbol-value n) (car o)) (setf (slot-value instance n) (symbol-value n)))
              (setq o (cdr o)))))))))

(defmacro with-self-variables-bound (&body body)
  "Run BODY with the instance variables of SELF bound as special variables, so that ordinary functions BODY calls can
refer to them.  The values are copied in, and back into the instance when BODY exits."
  `(%call-with-ivars-bound self (%flavor-all-ivars (%flv-name (%need-flavor-instance self 'with-self-variables-bound)))
                           (lambda () ,@body)))

(defun %ivar-bindings (fname object-form ll)
  "Symbol-macro bindings for SELF and the instance variables of FNAME that the lambda list LL does not rebind (and that
are not special: those are variables bound for the method, see %WRAP-SPECIAL-IVARS)."
  (let ((specials (%flavor-special-ivars fname)))
    (cons (list 'self object-form)
          (mapcar (lambda (v) (list v (list 'slot-value object-form (list 'quote v))))
                  (remove-if (lambda (v) (or (%tree-contains ll v) (memq v specials))) (%flavor-all-ivars fname))))))

(defun %wrap-special-ivars (fname object-form ll form)
  "FORM, run with the special instance variables of FNAME that LL does not rebind bound (a no-op without any)."
  (let ((names (remove-if (lambda (v) (%tree-contains ll v)) (%flavor-special-ivars fname))))
    (if names
        (list '%call-with-ivars-bound object-form (list 'quote names) (list 'lambda nil form))
        form)))

(defun %flavor-defmethod (spec rest)
  "Expand (DEFMETHOD (flavor [type] message) lambda-list . body), or (DEFMETHOD (flavor message) function-name)."
  (multiple-value-bind (fname type msg) (%parse-method-spec spec)
    (if (and (car rest) (symbolp (car rest)) (null (cdr rest)))
        (list '%install-method-checked (list 'quote fname) (list 'quote type) (list 'quote msg)
              (list 'lambda '(%o &rest %a)
                    (list 'let '((self %o)) (list 'apply (list 'function (car rest)) (list 'quote msg) '%a))))
        (let ((ll (car rest)) (body (cdr rest)) (s (gensym "SELF")))
          (list '%install-method-checked (list 'quote fname) (list 'quote type) (list 'quote msg)
                (list 'lambda (cons s ll)
                      (%wrap-special-ivars fname s ll
                                           (list* 'symbol-macrolet (%ivar-bindings fname s ll) body))))))))

(defun %spec-parts (spec what)
  "The flavor and message of SPEC, written (flavor message) or (message flavor)."
  (unless (and (consp spec) (consp (cdr spec)) (null (cddr spec)))
    (error "Bad ~A name ~S: expected (flavor message)" what spec))
  (if (and (keywordp (car spec)) (not (keywordp (cadr spec))))
      (values (cadr spec) (car spec))
      (values (car spec) (cadr spec))))

(defmacro defwhopper (spec lambda-list &body body)
  "Define a whopper on (flavor message): a method that wraps the others, calling them with CONTINUE-WHOPPER (or
LEXPR-CONTINUE-WHOPPER, whose last argument is a list of arguments).  Whoppers are outside :AROUND methods."
  (multiple-value-bind (fname msg) (%spec-parts spec "whopper")
    (let ((s (gensym "SELF")) (cont (gensym "CONT")))
      (list '%install-method-checked (list 'quote fname) :whopper (list 'quote msg)
            (list 'lambda (list* s cont lambda-list)
                  (%wrap-special-ivars
                   fname s lambda-list
                   (list 'symbol-macrolet (%ivar-bindings fname s lambda-list)
                         (list 'flet (list (list 'continue-whopper '(&rest %a) (list 'apply cont '%a))
                                           (list 'lexpr-continue-whopper '(&rest %a)
                                                 (list 'apply cont '(apply (function list*) %a))))
                               (cons 'progn body)))))))))

(defun %lambda-list-names (ll)
  (let ((acc nil) (rest nil) (mode nil))
    (dolist (x ll)
      (cond ((member x '(&optional &key &aux)) (setq mode x))
            ((eq x '&rest) (setq mode x))
            ((eq mode '&rest) (setq rest x mode nil))
            ((eq mode '&key) nil)
            (t (setq acc (cons (if (consp x) (car x) x) acc)))))
    (values (nreverse acc) rest)))

(defmacro defwrapper (spec pattern &body wrapper-body)
  "Define a wrapper on (flavor message).  PATTERN is ((argument...) . body-variable), or (argument-list-variable .
body-variable); WRAPPER-BODY computes, as a macro does, the code of the wrapper from the body variable's value, a list
of the forms that run the rest of the message.  The arguments are variables inside that code.  A wrapper is outside the
:AROUND methods and the daemons; here it is a whopper whose body is built when it is defined."
  (multiple-value-bind (fname msg) (%spec-parts spec "wrapper")
    (let ((arglist (car pattern)) (bodyvar (cdr pattern)))
      (when (and arglist (atom arglist)) (setq arglist (list '&rest arglist)))
      (multiple-value-bind (names rest) (%lambda-list-names arglist)
        (let* ((call (if rest
                         (cons 'lexpr-continue-whopper (append names (list rest)))
                         (cons 'continue-whopper names)))
               (maker (eval (list 'lambda (list bodyvar) (cons 'progn wrapper-body))))
               (form (funcall maker (list call))))
          (list 'defwhopper (list fname msg) arglist form))))))

(defmacro undefmethod (spec)
  "Remove the method named by SPEC, (flavor [type] message); the type :WRAPPER removes a wrapper or whopper."
  (multiple-value-bind (fname type msg) (%parse-method-spec spec)
    (list '%remove-flavor-method (list 'quote fname) (list 'quote type) (list 'quote msg))))

(defun %remove-flavor-method (fname type msg)
  (let* ((f (%find-flavor fname))
         (old (find-if (lambda (e) (%same-method-p e msg type)) (%flv-methods f))))
    (when old (setf (%flv-methods f) (remove old (%flv-methods f))) (%flavor-changed))
    (if old t nil)))

(defun undefflavor (flavor)
  "Remove the flavor FLAVOR, its methods and its class (for an alias flavor: just the alias)."
  (%find-flavor flavor)
  (when (assq flavor *flavor-aliases*)
    (setq *flavor-aliases* (remove flavor *flavor-aliases* :key #'car))
    (remprop flavor '%flavor-def)
    (remprop flavor '%class)
    (%flavor-changed)
    (return-from undefflavor flavor))
  (remprop flavor '%flavor-def)
  (remprop flavor '%class)
  (setq *flavors* (remove flavor *flavors*))
  (setq *all-flavor-names* *flavors*)
  (%flavor-changed)
  flavor)

(defmacro declare-flavor-instance-variables (flavors &body body)
  "Make the instance variables of the first of FLAVORS, as (FLAVOR), visible in BODY as variables of SELF; for functions
that are not methods but are called with SELF bound."
  (let ((s (gensym "SELF")))
    (list 'let (list (list s 'self))
          (list* 'symbol-macrolet (%ivar-bindings (car flavors) s nil) body))))

(defmacro compile-flavor-methods (&rest flavors)
  "Accepted for compatibility: methods are combined on first use."
  (list 'quote flavors))

(defun recompile-flavor (flavor &rest options)
  "Accepted for compatibility; discards the combined methods of every flavor so they are rebuilt."
  (declare (ignore options))
  (%flavor-changed)
  flavor)

(defun funcall-with-mapping-table (function mapping-table &rest arguments)
  "Call FUNCTION on ARGUMENTS; MAPPING-TABLE is ignored (the continuation of an :AROUND method is called with it)."
  (declare (ignore mapping-table))
  (apply function arguments))

(defun lexpr-funcall-with-mapping-table (function mapping-table &rest arguments)
  "Like FUNCALL-WITH-MAPPING-TABLE, but the last of ARGUMENTS is a list of further arguments."
  (declare (ignore mapping-table))
  (apply function (apply #'list* arguments)))

(defun apply-with-mapping-table (function mapping-table arguments)
  "Apply FUNCTION to the list ARGUMENTS; MAPPING-TABLE is ignored."
  (declare (ignore mapping-table))
  (apply function arguments))

;;; ---- combining methods --------------------------------------------------------------------------------

(defun %combination-spec (lin msg)
  (dolist (n lin nil)
    (dolist (s (%flv-combinations (%find-flavor n)))
      (when (or (null (cddr s)) (memq msg (cddr s)))
        (return-from %combination-spec s)))))

(defun %combine-methods (type ms)
  (case type
    (:progn (lambda (obj args)
              (let ((v nil)) (dolist (m ms) (setq v (multiple-value-list (apply m obj args)))) (values-list v))))
    (:or (lambda (obj args)
           (let ((v nil))
             (dolist (m ms) (setq v (multiple-value-list (apply m obj args))) (when (car v) (return)))
             (values-list v))))
    (:and (lambda (obj args)
            (let ((v nil))
              (dolist (m ms) (setq v (multiple-value-list (apply m obj args))) (unless (car v) (return)))
              (values-list v))))
    (:pass-on (lambda (obj args)
                ;; each method's values are the arguments of the next; the last method's values are returned
                (let ((vals args))
                  (dolist (m ms) (setq vals (multiple-value-list (apply m obj vals))))
                  (values-list vals))))
    (:inverse-list (lambda (obj args)
                     ;; the one argument is a list: method N gets element N; the values come back as a list
                     (let ((l (car args)))
                       (mapcar (lambda (m) (prog1 (funcall m obj (car l)) (setq l (cdr l)))) ms))))
    (:append (lambda (obj args) (apply #'append (mapcar (lambda (m) (apply m obj args)) ms))))
    (:nconc (lambda (obj args) (apply #'nconc (mapcar (lambda (m) (apply m obj args)) ms))))
    (:list (lambda (obj args) (mapcar (lambda (m) (apply m obj args)) ms)))
    (:max (lambda (obj args) (apply #'max (mapcar (lambda (m) (apply m obj args)) ms))))
    (:min (lambda (obj args) (apply #'min (mapcar (lambda (m) (apply m obj args)) ms))))
    (:+ (lambda (obj args) (apply #'+ (mapcar (lambda (m) (apply m obj args)) ms))))
    (t (error "Unknown method combination ~S" type))))

(defun %build-handler (f msg)
  "The combined method of flavor F for MSG, a function of the instance and the argument list; NIL if F does not handle it."
  (let* ((lin (%flavor-lin (%flv-name f)))
         (spec (%combination-spec lin msg))
         (style (or (car spec) :daemon))
         (first-p (eq (cadr spec) :base-flavor-first))
         (arounds nil) (invs nil) (whoppers nil) (befores nil) (afters nil) (primaries nil) (defaults nil)
         (typed nil) (overrides nil) (ors nil) (ands nil) (cases nil))
    (dolist (n lin)
      (dolist (e (%flv-methods (%find-flavor n)))
        (when (eq (car e) msg)
          (let ((type (cadr e)) (fn (caddr e)))
            (cond ((eq type :around) (setq arounds (cons fn arounds)))
                  ((eq type :inverse-around) (setq invs (cons fn invs)))
                  ((eq type :whopper) (setq whoppers (cons fn whoppers)))
                  ((eq type :before) (setq befores (cons fn befores)))
                  ((eq type :after) (setq afters (cons fn afters)))
                  ((eq type :default) (setq defaults (cons fn defaults)))
                  ((null type) (setq primaries (cons fn primaries)))
                  ((and (consp type) (eq (car type) :case) (eq style :case))
                   (unless (assoc (cadr type) cases) (setq cases (cons (cons (cadr type) fn) cases))))
                  ((and (eq type :or) (member style '(:daemon-with-or :case))) (setq ors (cons fn ors)))
                  ((and (eq type :and) (eq style :daemon-with-and)) (setq ands (cons fn ands)))
                  ((and (eq type :override) (eq style :daemon-with-override)) (setq overrides (cons fn overrides)))
                  ((and (eq type style) (member style '(:progn :or :and :append :nconc :list :inverse-list :pass-on :max :min :+)))
                   (setq typed (cons fn typed)))
                  (t (error "The ~S method type does not fit the ~S method combination of message ~S of flavor ~S~@[ (declare one with :METHOD-COMBINATION)~]"
                            type style msg (%flv-name f) (eq style :daemon))))))))
    ;; the lists were built last-flavor-first: put them most specific first
    (setq arounds (nreverse arounds) invs (nreverse invs) whoppers (nreverse whoppers) befores (nreverse befores)
          afters (nreverse afters) primaries (nreverse primaries) defaults (nreverse defaults) typed (nreverse typed)
          overrides (nreverse overrides) ors (nreverse ors) ands (nreverse ands))
    (flet ((ord (l) (if first-p (reverse l) l)))
      (let* ((primary (or (car primaries) (car defaults)))
             (core
               (cond
                 ((member style '(:daemon :daemon-with-or :daemon-with-and :daemon-with-override))
                  (let ((guards (ord (if (eq style :daemon-with-or) ors ands))) (and-p (eq style :daemon-with-and)))
                    (and (or primary guards)
                         (lambda (obj args)
                           (block core
                             (dolist (g guards)
                               (let ((v (multiple-value-list (apply g obj args))))
                                 (when (if and-p (null (car v)) (car v)) (return-from core (values-list v)))))
                             (if primary (apply primary obj args) nil))))))
                 ((eq style :case)
                  (and (or cases primary ors)
                       (lambda (obj args)
                         (block core
                           (dolist (g (ord ors))
                             (let ((v (multiple-value-list (apply g obj args))))
                               (when (car v) (return-from core (values-list v)))))
                           (let ((c (assoc (car args) cases)))
                             (cond (c (apply (cdr c) obj (cdr args)))
                                   (primary (apply primary obj args))
                                   (t nil)))))))
                 (t (let ((ms (append (ord typed) (ord (or primaries defaults)))))
                      (and ms (%combine-methods style ms)))))))
        (when core
          (let ((body (if (or befores afters)
                          (lambda (obj args)
                            (dolist (b befores) (apply b obj args))
                            (let ((vals (multiple-value-list (funcall core obj args))))
                              (dolist (a (reverse afters)) (apply a obj args))
                              (values-list vals)))
                          core)))
            (when overrides
              (setq body (let ((next body) (ovs (ord overrides)))
                           (lambda (obj args)
                             (block ov
                               (dolist (o ovs)
                                 (let ((v (multiple-value-list (apply o obj args))))
                                   (when (car v) (return-from ov (values-list v)))))
                               (funcall next obj args))))))
            ;; :AROUND methods: the continuation takes the operation and its arguments, as in the Lisp Machine
            (dolist (a (reverse arounds))
              (setq body (let ((a a) (next body))
                           (lambda (obj args)
                             (apply a obj (lambda (&rest more) (funcall next obj (cdr more))) nil (cons msg args) args)))))
            (dolist (w (reverse whoppers))
              (setq body (let ((w w) (next body))
                           (lambda (obj args)
                             (apply w obj (lambda (&rest more) (funcall next obj more)) args)))))
            ;; :INVERSE-AROUND methods are outside everything, the most basic flavor outermost
            (dolist (a invs)
              (setq body (let ((a a) (next body))
                           (lambda (obj args)
                             (apply a obj (lambda (&rest more) (funcall next obj (cdr more))) nil (cons msg args) args)))))
            body))))))

(defun %flavor-handler (f msg)
  (unless (eql (%flv-epoch f) *flavor-epoch*)
    (clrhash (%flv-cache f))
    (setf (%flv-epoch f) *flavor-epoch*))
  (let ((h (gethash msg (%flv-cache f) :unset)))
    (if (eq h :unset)
        (let ((new (%build-handler f msg)))
          (setf (gethash msg (%flv-cache f)) (or new :none))
          new)
        (if (eq h :none) nil h))))

(defun %need-flavor-instance (object message)
  (or (%instance-flavor object)
      (error "~S is not a flavor instance, so it cannot be sent the message ~S" object message)))

;;; ---- sending messages ---------------------------------------------------------------------------------

(defun %unclaimed (object message args)
  (let ((dh (dolist (n (%flavor-lin (%flv-name (%instance-flavor object))) nil)
              (let ((d (%flv-default-handler (%find-flavor n)))) (when d (return d))))))
    (if dh
        (apply (if (symbolp dh) (symbol-function dh) dh) message args)
        (error 'unclaimed-message :name message :instance object :arguments args))))

(defun %funcallable-instance-p (x)
  "True for an instance that can be called as a function: a flavor instance, or an instance of a funcallable class."
  (if (or (%funcallable-clos-p x) (%instance-flavor x)) t nil))

(defun %funcall-instance (instance &rest args)
  "What the kernel calls when INSTANCE is called as a function."
  (cond ((%funcallable-clos-p instance) (apply (%struct-ref instance 3) args))
        ((%instance-flavor instance)
         (if args
             (%send instance (car args) (cdr args))
             (error "~S is a flavor instance: call it with a message, as (funcall instance :message ...)" instance)))
        (t (error 'simple-type-error :datum instance :expected-type 'function
                  :format-control "The value ~S is not of type FUNCTION" :format-arguments (list instance)))))

(defun %send (object message args)
  (if (and (functionp object) (not (%instance-flavor object)))
      (apply object message args)
      (let* ((f (%need-flavor-instance object message))
             (h (%flavor-handler f message))
             (self object))
        (if h (funcall h object args) (%unclaimed object message args)))))

(defun send (object message &rest arguments)
  "Send OBJECT the MESSAGE (a keyword) with ARGUMENTS; the values are those of the method.  An unhandled message goes
to the flavor's :DEFAULT-HANDLER, or signals UNCLAIMED-MESSAGE.  A function is called with the message and arguments."
  (%send object message arguments))

(defun lexpr-send (object message &rest arguments)
  "Like SEND, but the last of ARGUMENTS is a list of further arguments."
  (%send object message (apply #'list* arguments)))

(defun operation-handled-p (object message)
  "True if OBJECT, a flavor instance, has a method for MESSAGE."
  (if (%flavor-handler (%need-flavor-instance object message) message) t nil))

(defun send-if-handles (object message &rest arguments)
  "SEND, but return NIL instead of signalling an error if OBJECT does not handle MESSAGE."
  (let ((h (%flavor-handler (%need-flavor-instance object message) message)))
    (if h (let ((self object)) (funcall h object arguments)) nil)))

(defun lexpr-send-if-handles (object message &rest arguments)
  "Like SEND-IF-HANDLES, but the last of ARGUMENTS is a list of further arguments."
  (apply #'send-if-handles object message (apply #'list* arguments)))

(defun get-handler-for (object message)
  "A function that sends OBJECT MESSAGE with the arguments it is called on, or NIL if OBJECT does not handle MESSAGE."
  (if (operation-handled-p object message)
      (lambda (&rest arguments) (%send object message arguments))
      nil))

(defun instantiate-flavor (flavor init-plist &optional send-init-message-p return-unhandled-keywords-p)
  "Make an instance of FLAVOR from the init plist INIT-PLIST (the :INIT message is always sent)."
  (declare (ignore send-init-message-p return-unhandled-keywords-p))
  (apply #'make-instance flavor init-plist))

;;; ---- initialization --------------------------------------------------------------------------------------

(defun flavor-allowed-init-keywords (flavor)
  "The init keywords MAKE-INSTANCE accepts for FLAVOR: those of its inittable instance variables, :INIT-KEYWORDS options
and :DEFAULT-INIT-PLISTs, and :ALLOW-OTHER-KEYS."
  (let ((acc (list :allow-other-keys)))
    (dolist (n (%flavor-lin flavor))
      (let ((f (%find-flavor n)))
        (dolist (v (%flv-inittable f)) (unless (memq (%keyword-of v) acc) (setq acc (cons (%keyword-of v) acc))))
        (dolist (k (%flv-init-keywords f)) (unless (memq k acc) (setq acc (cons k acc))))
        (dolist (k (%flv-required-keywords f)) (unless (memq k acc) (setq acc (cons k acc))))
        (dolist (c (%flv-alternatives f)) (unless (memq (car c) acc) (setq acc (cons (car c) acc))))
        (dolist (d (%flv-default-init-plist f)) (unless (memq (car d) acc) (setq acc (cons (car d) acc))))))
    (nreverse acc)))

(defun flavor-allows-init-keyword-p (flavor keyword)
  "True if MAKE-INSTANCE of FLAVOR accepts KEYWORD."
  (if (memq keyword (flavor-allowed-init-keywords flavor)) t nil))

(defun %check-flavor-instantiable (f)
  (when (%flv-abstract f) (error "The abstract flavor ~S cannot be instantiated" (%flv-name f)))
  (unless (eql (%flv-checked f) *flavor-epoch*)
    (let ((lin (%flavor-lin (%flv-name f))) (ivars (%flavor-all-ivars (%flv-name f))))
      (dolist (n lin)
        (let ((c (%find-flavor n)))
          (dolist (r (%flv-required-flavors c))
            (unless (memq r lin) (error "Flavor ~S requires flavor ~S, which ~S does not include" n r (%flv-name f))))
          (dolist (v (%flv-required-ivars c))
            (unless (memq v ivars) (error "Flavor ~S requires instance variable ~S, which ~S lacks" n v (%flv-name f))))
          (dolist (m (%flv-required-methods c))
            (unless (%flavor-handler f m) (error "Flavor ~S requires a method for ~S, which ~S lacks" n m (%flv-name f)))))))
    (setf (%flv-checked f) *flavor-epoch*)))

(defun %check-init-plist (f plist)
  (let ((allowed (flavor-allowed-init-keywords (%flv-name f))))
    (unless (getf plist :allow-other-keys)
      (do ((p plist (cddr p))) ((null p))
        (unless (memq (car p) allowed)
          (error "~S is not an init keyword of flavor ~S; the keywords are ~S" (car p) (%flv-name f) allowed))))
    (dolist (n (%flavor-lin (%flv-name f)))
      (dolist (k (%flv-required-keywords (%find-flavor n)))
        (when (%unsupplied-p (%getf-key plist k))
          (error "Flavor ~S requires the init keyword ~S" (%flv-name f) k))))))

;;; ---- choosing the flavor to instantiate ---------------------------------------------------------------------
;;; :INSTANTIATION-FLAVOR-FUNCTION and :RUN-TIME-ALTERNATIVES.  (:run-time-alternatives (key (value flavor...) ...) ...)
;;; says that the init keyword KEY chooses among the listed values (the first is the default); the instance is made of
;;; the chosen flavors mixed in front of the flavor, as a flavor named NAME/KEY=VALUE that is defined on first use.

(defun %alternatives-flavor (name plist)
  (let ((clauses nil))
    (dolist (n (%flavor-lin name)) (setq clauses (append clauses (%flv-alternatives (%find-flavor n)))))
    (if (null clauses)
        name
        (let ((mixins nil) (tag nil))
          (dolist (c clauses)
            (let* ((key (car c)) (given (%getf-key plist key))
                   (value (if (%unsupplied-p given) (car (cadr c)) given))
                   (alt (assoc value (cdr c))))
              (unless alt
                (error "~S is not one of the alternatives for ~S of flavor ~S: ~S" value key name (mapcar #'car (cdr c))))
              (setq mixins (append mixins (cdr alt)))
              (setq tag (cons (format nil "~A=~A" key value) tag))))
          (if (null mixins)
              name
              (let ((derived (intern (format nil "~A/~{~A~^,~}" (symbol-name name) (reverse tag))
                                     (or (symbol-package name) *package*)))
                    (components (append mixins (list name))))
                (unless (and (get derived '%flavor-def) (equal (%flv-components (get derived '%flavor-def)) components))
                  (%defflavor derived nil components nil)
                  (setf (get derived '%flavor-derived) t))
                derived))))))

(defun %instantiation-flavor (f plist)
  "The flavor to make an instance of when flavor record F is instantiated with the init plist PLIST."
  (let ((name (%flv-name f)))
    (if (get name '%flavor-derived)
        name
        (let ((target name))
          (dolist (n (%flavor-lin name))
            (let ((fn (%flv-instantiation-function (%find-flavor n))))
              (when fn
                (let ((r (funcall fn name plist)))
                  (when (and r (symbolp r) (not (eq r name))) (setq target r)))
                (return))))
          (if (eq target name) (%alternatives-flavor name plist) target)))))

(defmethod make-instance :around ((c class) &rest initargs)
  (let ((f (and (symbolp (%cls-name c)) (get (%cls-name c) '%flavor-def))))
    (if f
        (let ((target (%instantiation-flavor f initargs)))
          (if (eq target (%flv-name f))
              (call-next-method)
              (apply #'make-instance target initargs)))
        (call-next-method))))

;;; ---- VANILLA-FLAVOR -------------------------------------------------------------------------------------------

(defflavor vanilla-flavor () ()
  :no-vanilla-flavor
  (:documentation "The base of every flavor: the messages every instance understands."))

(defmethod initialize-instance :before ((o vanilla-flavor) &rest initargs)
  (let ((f (%instance-flavor o)))
    (when f
      (%check-flavor-instantiable f)
      (%check-init-plist f initargs))))

(defmethod initialize-instance :after ((o vanilla-flavor) &rest initargs)
  (when (%instance-flavor o) (send o :init initargs)))

(defmethod print-object ((o vanilla-flavor) stream)
  (send o :print-self stream 0 *print-escape*))

(defun %flavor-operations (f)
  (let ((acc nil))
    (dolist (n (%flavor-lin (%flv-name f)))
      (dolist (e (%flv-methods (%find-flavor n)))
        (when (and (not (memq (car e) acc)) (%flavor-handler f (car e)))
          (setq acc (cons (car e) acc)))))
    (nreverse acc)))

(defmethod (vanilla-flavor :init) (plist) (declare (ignore plist)) nil)

(defmethod (vanilla-flavor :print-self) (stream depth slashify)
  (declare (ignore depth slashify))
  (write-string "#<" stream)
  (write-string (symbol-name (%cls-name (class-of self))) stream)
  (write-string ">" stream)
  nil)

(defmethod (vanilla-flavor :describe) ()
  (let ((class (class-of self)))
    (format t "~&~S, an object of flavor ~S,~% has instance variable values:~%" self (%cls-name class))
    (dolist (sd (%cls-slots class))
      (format t "   ~A: ~A~%" (symbol-name (slotd-name sd))
              (if (slot-boundp self (slotd-name sd)) (prin1-to-string (slot-value self (slotd-name sd))) "void")))
    self))

(defmethod (vanilla-flavor :set) (keyword value)
  (send self (intern (%string-concat "SET-" (symbol-name keyword)) "KEYWORD") value))

(defmethod (vanilla-flavor :which-operations) () (%flavor-operations (%instance-flavor self)))

(defmethod (vanilla-flavor :operation-handled-p) (message) (operation-handled-p self message))

(defmethod (vanilla-flavor :send-if-handles) (message &rest arguments)
  (apply #'send-if-handles self message arguments))

(defmethod (vanilla-flavor :get-handler-for) (message) (get-handler-for self message))

(defmethod (vanilla-flavor :eval-inside-yourself) (form)
  (eval (list 'symbol-macrolet
              (mapcar (lambda (v) (list v (list 'slot-value (list 'quote self) (list 'quote v))))
                      (%flavor-all-ivars (%flv-name (%instance-flavor self))))
              form)))

(defmethod (vanilla-flavor :funcall-inside-yourself) (function &rest arguments)
  (apply function arguments))

;;; ---- describing flavors -----------------------------------------------------------------------------------

(defun describe-flavor (flavor &optional (stream *standard-output*))
  "Print what FLAVOR is made of, its instance variables and keywords, and the messages it handles."
  (let* ((name (if (symbolp flavor) flavor (%cls-name (class-of flavor))))
         (f (%find-flavor name)))
    (format stream "~&Flavor ~S~%" name)
    (when (%flv-documentation f) (format stream "  ~A~%" (%flv-documentation f)))
    (format stream "  Components: ~S~%" (cdr (%flavor-lin name)))
    (format stream "  Instance variables: ~S~%" (%flavor-all-ivars name))
    (format stream "  Init keywords: ~S~%" (cdr (flavor-allowed-init-keywords name)))
    (format stream "  Messages: ~S~%" (%flavor-operations f))
    (values)))

;;; ---- more of the Lisp Machine interface ---------------------------------------------------------------------

(defun instancep (object)
  "True if OBJECT is an instance (of a flavor or a class)."
  (if (and (structp object) (%std-instance-p object)) t nil))

(defun symeval-in-instance (instance symbol &optional no-error-p)
  "The value of the instance variable SYMBOL of INSTANCE; an error if there is none, unless NO-ERROR-P (then NIL)."
  (if (and (instancep instance) (slot-exists-p instance symbol))
      (slot-value instance symbol)
      (if no-error-p nil (error "~S has no instance variable ~S" instance symbol))))

(defun set-in-instance (instance symbol value)
  "Set the instance variable SYMBOL of INSTANCE to VALUE."
  (if (and (instancep instance) (slot-exists-p instance symbol))
      (setf (slot-value instance symbol) value)
      (error "~S has no instance variable ~S" instance symbol)))

(defun funcall-self (operation &rest arguments)
  "Send SELF the message OPERATION with ARGUMENTS (obsolete: use SEND)."
  (%send self operation arguments))

(defun lexpr-funcall-self (operation &rest arguments)
  "Like FUNCALL-SELF, but the last of ARGUMENTS is a list of further arguments (obsolete: use LEXPR-SEND)."
  (%send self operation (apply #'list* arguments)))

;;; PROPERTY-LIST-MIXIN: a property list for any flavor
(defun %plist-cell (object property)
  (do ((p (slot-value object 'property-list) (cddr p))) ((null p) nil)
    (when (eq (car p) property) (return p))))

(defflavor property-list-mixin ((property-list nil)) ()
  :gettable-instance-variables :settable-instance-variables :inittable-instance-variables
  (:documentation "A mixin that gives a flavor a property list: the :GET, :PUTPROP, :REMPROP, :GETL and :PUSH-PROPERTY messages."))

(defmethod (property-list-mixin :get) (property &optional default)
  (let ((c (%plist-cell self property))) (if c (cadr c) default)))

(defmethod (property-list-mixin :getl) (properties)
  (do ((p (slot-value self 'property-list) (cddr p))) ((null p) nil)
    (when (memq (car p) properties) (return p))))

(defmethod (property-list-mixin :putprop) (value property)
  (let ((c (%plist-cell self property)))
    (if c
        (rplaca (cdr c) value)
        (setf (slot-value self 'property-list) (list* property value (slot-value self 'property-list))))
    value))

(defmethod (property-list-mixin :remprop) (property)
  (let ((c (%plist-cell self property)))
    (when c
      (let ((new nil) (old (cdr c)))
        (do ((p (slot-value self 'property-list) (cddr p))) ((null p))
          (unless (eq p c) (setq new (append new (list (car p) (cadr p))))))
        (setf (slot-value self 'property-list) new)
        old))))

(defmethod (property-list-mixin :push-property) (value property)
  (let ((c (%plist-cell self property)))
    (if c
        (rplaca (cdr c) (cons value (cadr c)))
        (setf (slot-value self 'property-list) (list* property (list value) (slot-value self 'property-list))))
    (send self :get property)))
