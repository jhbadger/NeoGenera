;;;; mop-tests.lisp -- the CLOS meta-object protocol additions (lisp/core/53-mop.lisp) and the Flavors additions
;;;; (funcallable instances, :pass-on, :inverse-list, :alias-flavor, :run-time-alternatives,
;;;; :instantiation-flavor-function, special instance variables, with-self-variables-bound).

(in-package :cl)

;;; ---- method combination objects and COMPUTE-EFFECTIVE-METHOD -------------------------------------------

(defgeneric mop-sum (x) (:method-combination +))
(defmethod mop-sum + ((x integer)) 1)
(defmethod mop-sum + ((x number)) 10)

(defgeneric mop-std (x))
(defmethod mop-std ((x integer)) (list :int))
(defmethod mop-std :around ((x integer)) (cons :around (call-next-method)))
(defmethod mop-std :before ((x integer)) nil)
(defmethod mop-std :after ((x integer)) nil)

(deftest method-combination-objects
  (let ((mc (generic-function-method-combination #'mop-sum)))
    (is (typep mc 'method-combination))
    (is-equal '+ (method-combination-name mc))
    (is-equal nil (method-combination-options mc))
    (is (search "METHOD-COMBINATION" (prin1-to-string mc))))
  (is-equal 'standard (method-combination-name (generic-function-method-combination #'mop-std)))
  (is-equal 'standard (method-combination-name (find-method-combination #'mop-std 'standard nil)))
  (is-equal '(:most-specific-last) (method-combination-options (find-method-combination #'mop-sum '+ '(:most-specific-last))))
  (signals-error (find-method-combination #'mop-std 'no-such-combination nil))
  (is (typep (class-of (generic-function-method-combination #'mop-sum)) 'class))
  (is-equal 'method-combination (class-name (class-of (generic-function-method-combination #'mop-sum)))))

(deftest compute-effective-method-forms
  (let* ((methods (compute-applicable-methods #'mop-sum '(1)))
         (form (compute-effective-method #'mop-sum (generic-function-method-combination #'mop-sum) methods)))
    (is-equal '+ (car form))
    (is-equal 2 (length (cdr form)))
    (is (every (lambda (f) (eq (car f) 'call-method)) (cdr form))))
  (let* ((methods (compute-applicable-methods #'mop-std '(1)))
         (form (compute-effective-method #'mop-std (generic-function-method-combination #'mop-std) methods)))
    ;; an :around method wraps a MAKE-METHOD of the rest
    (is-equal 'call-method (car form))
    (is-equal 'make-method (car (car (cadr (cdr form))))))
  (is-equal 11 (mop-sum 1))
  (is-equal '(:around :int) (mop-std 1)))

(defgeneric mop-hooked (x))
(defmethod mop-hooked ((x integer)) (* x 2))

(deftest compute-effective-method-hook
  (is-equal 8 (mop-hooked 4))
  (let ((m (defmethod compute-effective-method :around ((gf (eql #'mop-hooked)) (mc method-combination) methods)
             (list 'list :wrapped (call-next-method)))))
    (unwind-protect
         (progn
           (is-equal '(:wrapped 8) (mop-hooked 4))        ; the cached effective method was discarded
           (is-equal 11 (mop-sum 1))                      ; other generic functions are not affected
           (is-equal '(:around :int) (mop-std 1)))
      (remove-method #'compute-effective-method m)))
  (is-equal 8 (mop-hooked 4)))

(deftest generic-function-and-method-readers
  (is (typep #'mop-std 'standard-generic-function))
  (is (typep #'mop-std 'generic-function))
  (is (typep #'mop-std 'function))
  (is-equal 'standard-generic-function (class-name (class-of #'mop-std)))
  (is-equal 'mop-std (generic-function-name #'mop-std))
  (is-equal '(x) (generic-function-lambda-list #'mop-std))
  (is-equal '(x) (generic-function-argument-precedence-order #'mop-std))
  (is-equal 'standard-method (class-name (generic-function-method-class #'mop-std)))
  (let ((m (find-method #'mop-std nil (list (find-class 'integer)))))
    (is (typep m 'standard-method))
    (is (typep m 'method))
    (is (eq (symbol-function 'mop-std) (method-generic-function m)))
    (is (functionp (method-function m)))
    (is-equal '(x) (method-lambda-list m))
    (is-equal nil (method-qualifiers m))
    (is-equal (list (find-class 'integer)) (method-specializers m)))
  (is-equal 4 (length (generic-function-methods #'mop-std))))

(deftest add-method-and-applicability
  (defgeneric mop-am (x))
  (defmethod mop-am ((x integer)) :int)
  (let ((m (find-method #'mop-am nil (list (find-class 'integer)))))
    (remove-method #'mop-am m)
    (signals-error (mop-am 1))
    (add-method #'mop-am m)
    (is-equal :int (mop-am 1)))
  (multiple-value-bind (methods definitive) (compute-applicable-methods-using-classes #'mop-am (list (find-class 'fixnum)))
    (is definitive)
    (is-equal 1 (length methods)))
  (multiple-value-bind (methods definitive) (compute-applicable-methods-using-classes #'mop-am (list (find-class 'string)))
    (is definitive)
    (is-equal 0 (length methods)))
  (defmethod mop-am ((x (eql 5))) :five)
  (multiple-value-bind (methods definitive) (compute-applicable-methods-using-classes #'mop-am (list (find-class 'fixnum)))
    (is (null definitive))
    (is (null methods)))
  (is (eql-specializer-p (intern-eql-specializer 5)))
  (is-equal 5 (eql-specializer-object (intern-eql-specializer 5)))
  (is (member (find-method #'mop-am nil (list (find-class 'integer))) (specializer-direct-methods (find-class 'integer))))
  (is (member (symbol-function 'mop-am) (specializer-direct-generic-functions (intern-eql-specializer 5))))
  (is-equal (symbol-function 'mop-am) (compute-discriminating-function #'mop-am)))

;;; ---- the class protocol -------------------------------------------------------------------------------------

(defclass mop-meta (standard-class) ())
(defclass mop-base () ((a :initarg :a :initform 1)) (:metaclass mop-meta))

(deftest validate-superclass-protocol
  (is-equal 'mop-meta (class-name (class-of (find-class 'mop-base))))
  (is (validate-superclass (find-class 'mop-base) (find-class 'standard-object)))
  (is (validate-superclass (find-class 'mop-base) (find-class 't)))
  ;; a class and a superclass whose metaclasses derive from STANDARD-CLASS are compatible
  (is (validate-superclass (find-class 'mop-base) (find-class 'mop-base)))
  (defclass mop-sub (mop-base) ())
  (is (subtypep 'mop-sub 'mop-base))
  ;; a funcallable class and a plain one are not
  (defclass mop-fun () () (:metaclass funcallable-standard-class))
  (is (not (validate-superclass (find-class 'mop-base) (find-class 'mop-fun))))
  (signals-error (eval '(defclass mop-bad (mop-fun) () (:metaclass mop-meta))))
  (signals-error (eval '(defclass mop-bad2 (mop-base mop-fun) ())))
  ;; a method on VALIDATE-SUPERCLASS can allow what the default refuses
  (let ((m (defmethod validate-superclass ((c mop-meta) (s funcallable-standard-class)) t)))
    (unwind-protect (is (validate-superclass (find-class 'mop-base) (find-class 'mop-fun)))
      (remove-method #'validate-superclass m)))
  (is (not (validate-superclass (find-class 'mop-base) (find-class 'mop-fun)))))

(defvar *mop-trace* nil)
(defclass mop-meta2 (standard-class) ())
(defmethod compute-class-precedence-list ((class mop-meta2))
  (setq *mop-trace* (cons (list :cpl (class-name class)) *mop-trace*))
  (call-next-method))
(defmethod compute-effective-slot-definition ((class mop-meta2) name direct-slot-definitions)
  (setq *mop-trace* (cons (list :eff name (length direct-slot-definitions)) *mop-trace*))
  (call-next-method))

(deftest class-finalization-protocol
  (setq *mop-trace* nil)
  (eval '(defclass mop-p1 () ((x :initarg :x) (y :initform 7)) (:metaclass mop-meta2)))
  (is (member '(:cpl mop-p1) *mop-trace* :test #'equal))
  (is (member '(:eff x 1) *mop-trace* :test #'equal))
  (is (member '(:eff y 1) *mop-trace* :test #'equal))
  (is-equal 7 (slot-value (make-instance 'mop-p1) 'y))
  (is-equal '(x y) (mapcar #'slot-definition-name (class-slots (find-class 'mop-p1))))
  (is (typep (first (class-slots (find-class 'mop-p1))) 'standard-effective-slot-definition))
  (is-equal 'standard-direct-slot-definition (class-name (direct-slot-definition-class (find-class 'mop-p1))))
  (is-equal 'standard-effective-slot-definition (class-name (effective-slot-definition-class (find-class 'mop-p1))))
  ;; a subclass overriding a slot has two direct slot definitions to merge
  (setq *mop-trace* nil)
  (eval '(defclass mop-p2 (mop-p1) ((y :initform 8)) (:metaclass mop-meta2)))
  (is (member '(:eff y 2) *mop-trace* :test #'equal))
  (is-equal 8 (slot-value (make-instance 'mop-p2) 'y))
  (is (integerp (slot-definition-location (find 'x (class-slots (find-class 'mop-p2)) :key #'slot-definition-name))))
  (let ((i (make-instance 'mop-p2 :x 3)))
    (is-equal 3 (standard-instance-access i (slot-definition-location (find 'x (class-slots (find-class 'mop-p2)) :key #'slot-definition-name))))))

(defclass mop-sv () ((a :initarg :a) (b :initform 2)))

(deftest slot-access-protocol
  (let ((i (make-instance 'mop-sv :a 1)))
    (is-equal 1 (slot-value i 'a))
    (let ((log nil))
      (let ((m1 (defmethod slot-value-using-class ((c t) (o mop-sv) slot) (push (list :get (slot-definition-name slot)) log) (call-next-method)))
            (m2 (defmethod (setf slot-value-using-class) (new (c t) (o mop-sv) slot)
                  (push (list :set (slot-definition-name slot) new) log) (call-next-method)))
            (m3 (defmethod slot-boundp-using-class ((c t) (o mop-sv) slot) (push (list :boundp (slot-definition-name slot)) log) (call-next-method)))
            (m4 (defmethod slot-makunbound-using-class ((c t) (o mop-sv) slot) (push (list :unbind (slot-definition-name slot)) log) (call-next-method))))
        (unwind-protect
             (progn
               (is-equal 1 (slot-value i 'a))
               (setf (slot-value i 'b) 5)
               (is (slot-boundp i 'b))
               (slot-makunbound i 'b)
               (is (not (slot-boundp i 'b)))
               (is-equal '((:get a) (:set b 5) (:boundp b) (:unbind b) (:boundp b)) (reverse log))
               ;; other classes are untouched
               (setq log nil)
               (is-equal 3 (slot-value (make-instance 'mop-p1 :x 3) 'x))
               (is-equal nil log))
          (remove-method #'slot-value-using-class m1)
          (remove-method '(setf slot-value-using-class) m2)
          (remove-method #'slot-boundp-using-class m3)
          (remove-method #'slot-makunbound-using-class m4))))
    ;; with the methods gone the slot functions behave as before
    (is-equal 1 (slot-value i 'a))))

(defvar *mop-deps* nil)
(defclass mop-dep () ())
(defmethod update-dependent ((m t) (d mop-dep) &rest initargs) (push initargs *mop-deps*))

(deftest dependents-protocol
  (defgeneric mop-dg (x))
  (let ((d (make-instance 'mop-dep)))
    (setq *mop-deps* nil)
    (add-dependent #'mop-dg d)
    (let ((count 0)) (map-dependents #'mop-dg (lambda (x) (declare (ignore x)) (setq count (+ count 1)))) (is-equal 1 count))
    (defmethod mop-dg ((x integer)) 1)
    (is-equal 1 (length *mop-deps*))
    (is-equal 'add-method (car (car *mop-deps*)))
    (remove-dependent #'mop-dg d)
    (defmethod mop-dg ((x string)) 2)
    (is-equal 1 (length *mop-deps*))))

(deftest add-and-remove-direct-subclass
  (defclass mop-ds1 () ())
  (defclass mop-ds2 (mop-ds1) ())
  (is (member (find-class 'mop-ds2) (class-direct-subclasses (find-class 'mop-ds1))))
  (remove-direct-subclass (find-class 'mop-ds1) (find-class 'mop-ds2))
  (is (not (member (find-class 'mop-ds2) (class-direct-subclasses (find-class 'mop-ds1)))))
  (add-direct-subclass (find-class 'mop-ds1) (find-class 'mop-ds2))
  (is (member (find-class 'mop-ds2) (class-direct-subclasses (find-class 'mop-ds1)))))

;;; ---- funcallable instances (CLOS) ------------------------------------------------------------------------

(defclass mop-fn () ((n :initarg :n :initform 5)) (:metaclass funcallable-standard-class))
(defclass mop-fn-sub (mop-fn) ((m :initform 1)))

(deftest funcallable-instances
  (let ((i (make-instance 'mop-fn :n 3)))
    (is (typep i 'mop-fn))
    (is (typep i 'funcallable-standard-object))
    (is (typep i 'function))
    (is (functionp i))
    (is-equal 'funcallable-standard-class (class-name (class-of (find-class 'mop-fn))))
    (is (eq i (set-funcallable-instance-function i (lambda (x) (+ x (slot-value i 'n))))))
    (is-equal 13 (funcall i 10))
    (is-equal 13 (apply i '(10)))
    (is-equal '(4 5) (mapcar i '(1 2)))
    (is (functionp (funcallable-instance-function i)))
    ;; slots work as for any instance
    (setf (slot-value i 'n) 100)
    (is-equal 101 (funcall i 1))
    ;; call through a symbol's function cell
    (setf (symbol-function 'mop-fn-caller) i)
    (is-equal 102 (mop-fn-caller 2))
    (fmakunbound 'mop-fn-caller))
  ;; no function yet: an error that says so
  (signals-error (funcall (make-instance 'mop-fn) 1))
  ;; subclasses are funcallable too
  (let ((s (make-instance 'mop-fn-sub)))
    (is (functionp s))
    (set-funcallable-instance-function s (lambda () :sub))
    (is-equal :sub (funcall s))
    (is-equal 5 (slot-value s 'n)))
  ;; ordinary instances and structures are not functions
  (is (not (functionp (make-instance 'mop-sv))))
  (signals-error (funcall (make-instance 'mop-sv)))
  (signals-error (set-funcallable-instance-function (make-instance 'mop-sv) #'car))
  (is (not (functionp (make-hash-table)))))

;;; ---- Flavors: funcallable instances, alias flavors ------------------------------------------------------------

(defflavor mf-widget ((size 3) (color :red)) ()
  :gettable-instance-variables :settable-instance-variables :inittable-instance-variables)
(defmethod (mf-widget :area) () (* size size))

(deftest flavor-instances-are-funcallable
  (let ((w (make-instance 'mf-widget :size 4)))
    (is (functionp w))
    (is (typep w 'function))
    (is-equal 4 (funcall w :size))
    (is-equal 16 (funcall w :area))
    (is-equal 5 (funcall w :set-size 5))
    (is-equal 25 (send w :area))
    (is-equal :red (apply w :color nil))
    (is-equal '(4) (mapcar (lambda (n) (funcall w :set-size n) (funcall w :area)) '(2)))
    (signals-error (funcall w))                             ; needs a message
    (signals-error (funcall w :no-such-message))
    ;; sending a function a message still calls it
    (is-equal '(:a 1) (send (lambda (op x) (list op x)) :a 1)))
  (is (not (functionp (make-plain-struct-for-test))))
  (signals-error (funcall (make-plain-struct-for-test))))

(defstruct mop-plain a)
(defun make-plain-struct-for-test () (make-mop-plain :a 1))

(defflavor mf-gadget () () (:alias-flavor mf-widget))

(deftest alias-flavor
  (let ((g (make-instance 'mf-gadget :size 3)))
    (is (typep g 'mf-widget))
    (is (typep g 'mf-gadget))
    (is-equal 'mf-widget (class-name (class-of g)))
    (is-equal 9 (send g :area))
    (is (eq (find-class 'mf-gadget) (find-class 'mf-widget)))
    (is-equal (flavor-components 'mf-widget) (flavor-components 'mf-gadget)))
  ;; methods defined on the alias belong to the flavor
  (eval '(defmethod (mf-gadget :double) () (* 2 size)))
  (is-equal 8 (send (make-instance 'mf-widget :size 4) :double))
  ;; undefining the alias leaves the flavor
  (undefflavor 'mf-gadget)
  (is (null (find-class 'mf-gadget nil)))
  (is-equal 9 (send (make-instance 'mf-widget) :area))
  (signals-error (eval '(defflavor mf-alias-of-nothing () () (:alias-flavor no-such-flavor))))
  (signals-error (eval '(defflavor mf-widget () () (:alias-flavor mf-widget)))))

;;; ---- Flavors: more method combinations ---------------------------------------------------------------------

(defflavor mf-pa () () (:method-combination (:pass-on (:base-flavor-last a b) :go)))
(defflavor mf-pb () (mf-pa))
(defmethod (mf-pa :go) (x) (list :pa x))
(defmethod (mf-pb :go) (x) (list :pb x))

(defflavor mf-pc () () (:method-combination (:pass-on :base-flavor-first :go)))
(defflavor mf-pd () (mf-pc))
(defmethod (mf-pc :go) (x) (list :pc x))
(defmethod (mf-pd :go) (x) (list :pd x))

(defflavor mf-ia () () (:method-combination (:inverse-list :restore)))
(defflavor mf-ib () (mf-ia))
(defmethod (mf-ia :restore) (v) (list :a v))
(defmethod (mf-ib :restore) (v) (list :b v))

(defflavor mf-ja () () (:method-combination (:pass-on :split)))
(defflavor mf-jb () (mf-ja))
(defmethod (mf-ja :split) (a b) (values (+ a b) (- a b)))
(defmethod (mf-jb :split) (a b) (values (* a b) (+ a b)))

(deftest flavor-pass-on-and-inverse-list
  ;; :pass-on: the values of each method are the arguments of the next, most specific first
  (is-equal '(:pa (:pb 1)) (send (make-instance 'mf-pb) :go 1))
  (is-equal '(:pd (:pc 1)) (send (make-instance 'mf-pd) :go 1))
  (is-equal '(:pc 1) (send (make-instance 'mf-pc) :go 1))
  ;; several values are passed on as several arguments
  (is-equal '(19 5) (multiple-value-list (send (make-instance 'mf-jb) :split 4 3)))   ; (12 7) -> (+ 12 7) (- 12 7)
  ;; :inverse-list: one list argument, one element for each method
  (is-equal '((:b 1) (:a 2)) (send (make-instance 'mf-ib) :restore '(1 2)))
  (is-equal '((:a 1)) (send (make-instance 'mf-ia) :restore '(1)))
  ;; the combination types are declared valid method types
  (is (member :pass-on *flavor-combination-types*))
  (is (member :inverse-list *flavor-combination-types*)))

;;; ---- Flavors: choosing the flavor to instantiate ---------------------------------------------------------------

(defflavor mf-base-w () () (:init-keywords :kind))
(defflavor mf-wa () (mf-base-w))
(defflavor mf-wb () (mf-base-w))
(defvar *mf-picks* nil)
(defun mf-pick-flavor (name plist)
  (setq *mf-picks* (cons name *mf-picks*))
  (case (getf plist :kind) (:b 'mf-wb) (:a 'mf-wa) (t nil)))
(defflavor mf-chooser () () (:instantiation-flavor-function mf-pick-flavor) (:init-keywords :kind))

(defflavor mf-mix-x () ())
(defflavor mf-mix-y () ())
(defmethod (mf-mix-x :who) () :x)
(defmethod (mf-mix-y :who) () :y)
(defflavor mf-rta ((v 1)) () (:run-time-alternatives (:flavor (:x mf-mix-x) (:y mf-mix-y)))
  :gettable-instance-variables :inittable-instance-variables)

(deftest flavor-instantiation-flavor-function
  (setq *mf-picks* nil)
  (is-equal 'mf-wb (class-name (class-of (make-instance 'mf-chooser :kind :b))))
  (is-equal 'mf-wa (class-name (class-of (make-instance 'mf-chooser :kind :a))))
  (is-equal 'mf-chooser (class-name (class-of (make-instance 'mf-chooser))))   ; NIL: the flavor itself
  (is-equal '(mf-chooser mf-chooser mf-chooser) *mf-picks*)
  (is (typep (instantiate-flavor 'mf-chooser '(:kind :b)) 'mf-wb)))

(deftest flavor-run-time-alternatives
  (is-equal :x (send (make-instance 'mf-rta) :who))                  ; the first alternative is the default
  (is-equal :y (send (make-instance 'mf-rta :flavor :y) :who))
  (is-equal :x (send (make-instance 'mf-rta :flavor :x) :who))
  (is-equal 5 (send (make-instance 'mf-rta :flavor :y :v 5) :v))
  (let ((i (make-instance 'mf-rta :flavor :y)))
    (is (typep i 'mf-rta))
    (is (typep i 'mf-mix-y))
    (is (not (typep i 'mf-mix-x)))
    (is (eq (class-of i) (class-of (make-instance 'mf-rta :flavor :y)))))   ; the derived flavor is made once
  (signals-error (make-instance 'mf-rta :flavor :z))
  (is (flavor-allows-init-keyword-p 'mf-rta :flavor)))

;;; ---- Flavors: special instance variables ---------------------------------------------------------------------

(defun mf-peek-count () (symbol-value 'mf-ctr))
(defun mf-bump-count () (setq mf-ctr (+ (symbol-value 'mf-ctr) 100)))
(defflavor mf-counter ((mf-ctr 10) (mf-other 1)) () (:special-instance-variables mf-ctr)
  :gettable-instance-variables)
(defmethod (mf-counter :look) () (setq mf-ctr (+ mf-ctr 1)) (mf-peek-count))
(defmethod (mf-counter :bump) () (mf-bump-count) mf-ctr)
(defmethod (mf-counter :other) () (setq mf-other (+ mf-other 1)) mf-other)

(defflavor mf-all-special ((aa 1) (bb 2)) () (:special-instance-variables))
(defmethod (mf-all-special :sum) () (+ (symbol-value 'aa) (symbol-value 'bb)))

(defflavor mf-plain-iv ((q 1) (r 2)) ())
(defun mf-set-q () (setq q 42) q)
(defmethod (mf-plain-iv :via) () (with-self-variables-bound (list (symbol-value 'q) (mf-set-q) (symbol-value 'r))))
(defmethod (mf-plain-iv :mixed) () (with-self-variables-bound (setq r 7) (setf (slot-value self 'r) 9) (symbol-value 'q)))

(deftest flavor-special-instance-variables
  (let ((c (make-instance 'mf-counter)))
    ;; a function called from a method sees the variable, and changes are copied back
    (is-equal 11 (send c :look))
    (is-equal 11 (symeval-in-instance c 'mf-ctr))
    (is-equal 111 (send c :bump))
    (is-equal 111 (symeval-in-instance c 'mf-ctr))
    ;; ordinary (non-special) variables still work as before
    (is-equal 2 (send c :other))
    (is-equal 2 (symeval-in-instance c 'mf-other)))
  (is (cl::%special-p 'mf-ctr))
  (is (not (cl::%special-p 'mf-other)))
  (is (not (boundp 'mf-ctr)))                        ; the binding lasts only for the method
  (is-equal 3 (send (make-instance 'mf-all-special) :sum))
  (let ((p (make-instance 'mf-plain-iv)))
    (is-equal '(1 42 2) (send p :via))
    (is-equal 42 (symeval-in-instance p 'q))
    (is-equal 2 (symeval-in-instance p 'r))
    (is-equal 42 (send p :mixed))
    ;; the slot set in the body is not overwritten by the unchanged variable
    (is-equal 9 (symeval-in-instance p 'r)))
  (signals-error (with-self-variables-bound 1)))
