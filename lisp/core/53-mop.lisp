;;;; 53-mop.lisp -- the CLOS meta-object protocol beyond the basics of 52-clos.lisp.
;;;;
;;;; Method combination objects and FIND-METHOD-COMBINATION; COMPUTE-EFFECTIVE-METHOD (which returns a form that uses
;;;; CALL-METHOD and MAKE-METHOD, as the standard says); VALIDATE-SUPERCLASS; the class-finalization protocol
;;;; (COMPUTE-CLASS-PRECEDENCE-LIST, COMPUTE-SLOTS, COMPUTE-EFFECTIVE-SLOT-DEFINITION) for classes with their own
;;;; metaclass; SLOT-VALUE-USING-CLASS and its relatives; readers for generic functions and methods; dependents; and
;;;; funcallable instances (the metaclass FUNCALLABLE-STANDARD-CLASS).  The system-wide hooks are in 52-clos.lisp and
;;;; stay out of the way (no generic dispatch) until a program defines a method on the generic function concerned.
;;;;
;;;; Not here: generic functions are not themselves instances (a program cannot subclass STANDARD-GENERIC-FUNCTION or
;;;; pass :GENERIC-FUNCTION-CLASS), so COMPUTE-DISCRIMINATING-FUNCTION cannot be customized.

(in-package :cl)

;;; ---- method combinations as objects -----------------------------------------------------------------------

(defstruct (method-combination (:conc-name %mc-) (:constructor %make-mc) (:predicate %method-combination-p) (:copier nil))
  name (options nil) (function nil) (documentation nil))

(defun %method-combination-object (name options)
  (%make-mc :name name :options options :function (get name '%method-combination)))

(defun find-method-combination (generic-function name options)
  "The method combination object for the combination type NAME (STANDARD, a short-form name such as + or AND,
or a DEFINE-METHOD-COMBINATION name) with OPTIONS."
  (declare (ignore generic-function))
  (if (eq name 'standard)
      (%make-mc :name 'standard :options options)
      (progn
        (unless (get name '%method-combination) (error "~S is not a method combination" name))
        (%method-combination-object name options))))

(defun generic-function-method-combination (generic-function)
  "The method combination object of GENERIC-FUNCTION (the standard one unless it was declared otherwise)."
  (let ((c (gf-combination (%as-gf generic-function))))
    (if c (%method-combination-object (car c) (cdr c)) (%make-mc :name 'standard))))

(defun method-combination-name (method-combination)
  "The name of the combination type of METHOD-COMBINATION, such as STANDARD or +."
  (%mc-name method-combination))
(defun method-combination-options (method-combination)
  "The options METHOD-COMBINATION was made with, such as (:most-specific-last)."
  (%mc-options method-combination))

(defmethod print-object ((m method-combination) stream)
  (format stream "#<METHOD-COMBINATION ~S~{ ~S~}>" (%mc-name m) (%mc-options m)))

;;; ---- COMPUTE-EFFECTIVE-METHOD ------------------------------------------------------------------------------

(defun %standard-effective-form (gf methods)
  "The effective method of the STANDARD method combination, as a form."
  (let ((around nil) (before nil) (primary nil) (after nil))
    (dolist (m methods)
      (let ((q (car (method-qualifiers m))))
        (case q
          ((nil) (setq primary (cons m primary)))
          (:around (setq around (cons m around)))
          (:before (setq before (cons m before)))
          (:after (setq after (cons m after)))
          (t (invalid-method-error m "The standard method combination does not accept the qualifiers ~S"
                                   (method-qualifiers m))))))
    (setq primary (nreverse primary) around (nreverse around) before (nreverse before))
    (if (null primary)
        (list '%no-applicable-method (list 'quote (%as-gf gf)) '%args)
        (let* ((call (list 'call-method (car primary) (cdr primary)))
               (main (if (or before after)
                         (append (list 'progn)
                                 (mapcar (lambda (m) (list 'call-method m)) before)
                                 (list (append (list 'multiple-value-prog1 call)
                                               (mapcar (lambda (m) (list 'call-method m)) after))))   ; :after: least specific first
                         call)))
          (if around
              (list 'call-method (car around) (append (cdr around) (list (list 'make-method main))))
              main)))))

(defgeneric compute-effective-method (generic-function method-combination methods))

(defmethod compute-effective-method ((gf generic-function) (mc method-combination) methods)
  (if (eq (%mc-name mc) 'standard)
      (%standard-effective-form gf methods)
      (let ((fn (or (%mc-function mc) (error "~S is not a method combination" (%mc-name mc)))))
        (funcall fn gf methods (%mc-options mc)))))

(setq *cem-generic* (%find-gf 'compute-effective-method))

;;; ---- the class protocol ----------------------------------------------------------------------------------------

(defgeneric validate-superclass (class superclass))

(defun %funcallable-metaclass-p (mc) (%subclass-p mc (find-class 'funcallable-standard-class)))

(defmethod validate-superclass ((class class) (superclass class))
  (let ((cm (class-of class)) (sm (class-of superclass)) (sn (%cls-name superclass)))
    (if (or (eq cm sm)
            (eq sn 't)
            ;; the built-in classes (STANDARD-OBJECT, STANDARD-CLASS, ... ) are not instances of STANDARD-CLASS here,
            ;; so they are accepted as superclasses; FUNCALLABLE-STANDARD-OBJECT only for funcallable classes
            (and (eq (%cls-kind superclass) :builtin) (not (eq sn 'funcallable-standard-object)))
            (and (eq sn 'funcallable-standard-object) (eq (%cls-kind class) :funcallable))
            ;; two metaclasses that both derive from STANDARD-CLASS are compatible unless exactly one is funcallable
            (and (%subclass-p cm (find-class 'standard-class)) (%subclass-p sm (find-class 'standard-class))
                 (eq (and (%funcallable-metaclass-p cm) t) (and (%funcallable-metaclass-p sm) t))))
        t
        nil)))

(defgeneric compute-class-precedence-list (class))
(defmethod compute-class-precedence-list ((class class)) (%c3 class (%cls-supers class)))

(defgeneric compute-effective-slot-definition (class name direct-slot-definitions))
(defmethod compute-effective-slot-definition ((class class) name direct-slot-definitions)
  (declare (ignore name))
  ;; DIRECT-SLOT-DEFINITIONS are most specific first; the merged slot starts from the least specific
  (let ((merged nil))
    (dolist (sd (reverse direct-slot-definitions))
      (if merged
          (progn
            (setf (slotd-initargs merged) (union (slotd-initargs merged) (slotd-initargs sd)))
            (when (slotd-initfn sd) (setf (slotd-initfn merged) (slotd-initfn sd)))
            (setf (slotd-allocation merged) (slotd-allocation sd) (slotd-cell merged) (slotd-cell sd)))
          (setq merged (copy-structure sd))))
    merged))

(defgeneric compute-slots (class))
(defmethod compute-slots ((class class))
  (let ((names nil) (cpl (%cls-cpl class)))
    (dolist (c (reverse cpl))
      (dolist (sd (%cls-direct-slots c))
        (unless (memq (slotd-name sd) names) (setq names (cons (slotd-name sd) names)))))
    (mapcar (lambda (n)
              (let ((directs nil))
                (dolist (c cpl)
                  (let ((sd (find n (%cls-direct-slots c) :key #'slotd-name)))
                    (when sd (setq directs (append directs (list sd))))))
                (compute-effective-slot-definition class n directs)))
            (nreverse names))))

(defgeneric direct-slot-definition-class (class &rest initargs))
(defmethod direct-slot-definition-class ((class class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'standard-direct-slot-definition))
(defgeneric effective-slot-definition-class (class &rest initargs))
(defmethod effective-slot-definition-class ((class class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'standard-effective-slot-definition))

(defun add-direct-subclass (superclass subclass)
  "Record SUBCLASS as a direct subclass of SUPERCLASS."
  (unless (memq subclass (%cls-subclasses superclass))
    (setf (%cls-subclasses superclass) (cons subclass (%cls-subclasses superclass))))
  subclass)
(defun remove-direct-subclass (superclass subclass)
  "Forget that SUBCLASS is a direct subclass of SUPERCLASS."
  (setf (%cls-subclasses superclass) (remove subclass (%cls-subclasses superclass)))
  subclass)

;;; ---- slot access protocol -----------------------------------------------------------------------------------

(defgeneric slot-value-using-class (class object slot))
(defgeneric (setf slot-value-using-class) (new-value class object slot))
(defgeneric slot-boundp-using-class (class object slot))
(defgeneric slot-makunbound-using-class (class object slot))

(defmethod slot-value-using-class ((class t) object slot) (%slot-value-raw object (slotd-name slot)))
(defmethod (setf slot-value-using-class) (new-value (class t) object slot)
  (%set-slot-value-raw object (slotd-name slot) new-value))
(defmethod slot-boundp-using-class ((class t) object slot) (%slot-boundp-raw object (slotd-name slot)))
(defmethod slot-makunbound-using-class ((class t) object slot) (%slot-makunbound-raw object (slotd-name slot)) object)

(defun slot-definition-location (slot)
  "Where the slot lives: a vector index for a local slot, the shared cell for a class slot."
  (slotd-location slot))

(defun standard-instance-access (instance location)
  "The value at LOCATION (see SLOT-DEFINITION-LOCATION) of the standard instance INSTANCE, without any checks."
  (%loc-ref instance location))
(defun %set-standard-instance-access (instance location value) (%loc-set instance location value))
(%def-setf 'standard-instance-access '%set-standard-instance-access)

;;; ---- generic functions and methods ------------------------------------------------------------------------------

(defun generic-function-name (generic-function)
  "The name of GENERIC-FUNCTION."
  (gf-name (%as-gf generic-function)))
(defun generic-function-lambda-list (generic-function)
  "The lambda list of GENERIC-FUNCTION as given to DEFGENERIC."
  (gf-lambda-list (%as-gf generic-function)))
(defun generic-function-argument-precedence-order (generic-function)
  "The required parameters of GENERIC-FUNCTION in the order dispatch considers them (left to right)."
  (let ((ll (gf-lambda-list (%as-gf generic-function))))
    (subseq ll 0 (%required-count ll))))
(defun generic-function-declarations (generic-function)
  "The declarations of GENERIC-FUNCTION (always NIL)."
  (declare (ignore generic-function))
  nil)
(defun generic-function-method-class (generic-function)
  "The class of the methods of GENERIC-FUNCTION: STANDARD-METHOD."
  (declare (ignore generic-function))
  (find-class 'standard-method))
(defun method-generic-function (method)
  "The generic function METHOD belongs to."
  (gf-fn (method-gf method)))
(defun method-function (method)
  "The function that runs METHOD (it takes the argument list and the next-method function)."
  (method-fn method))

(defun add-method (generic-function method)
  "Add the method object METHOD to GENERIC-FUNCTION, replacing a method with the same qualifiers and specializers."
  (let ((g (%as-gf generic-function)))
    (setf (gf-methods g)
          (cons method (remove-if (lambda (o) (and (equal (method-qualifiers o) (method-qualifiers method))
                                                   (equal (method-specializers o) (method-specializers method))))
                                  (gf-methods g))))
    (setf (method-gf method) g)
    (%recompute-gf g)
    (%notify-dependents g 'add-method method)
    generic-function))

(defun compute-applicable-methods-using-classes (generic-function classes)
  "The methods of GENERIC-FUNCTION applicable to arguments of the given CLASSES, most specific first, and true if that
is definitive.  When a method has an EQL specializer the answer depends on the arguments themselves: (values nil nil)."
  (let ((g (%as-gf generic-function)))
    (if (gf-has-eql g)
        (values nil nil)
        (let ((acc nil))
          (dolist (m (gf-methods g))
            (when (let ((cs classes))
                    (dolist (s (method-specializers m) t)
                      (unless (and cs (%subclass-p (car cs) s)) (return nil))
                      (setq cs (cdr cs))))
              (setq acc (cons m acc))))
          (values (sort acc (lambda (a b) (%more-specific-p a b classes))) t)))))

(defun compute-discriminating-function (generic-function)
  "The function that is GENERIC-FUNCTION's definition: it finds the effective method and calls it."
  (gf-fn (%as-gf generic-function)))

(defun intern-eql-specializer (object)
  "The EQL specializer for OBJECT (as it appears in METHOD-SPECIALIZERS: the cons (:EQL . OBJECT))."
  (cons :eql object))
(defun eql-specializer-object (eql-specializer)
  "The object an EQL specializer specializes on."
  (cdr eql-specializer))
(defun eql-specializer-p (x)
  "True for an EQL specializer."
  (and (consp x) (eq (car x) :eql)))

(defun specializer-direct-methods (specializer)
  "The methods that have SPECIALIZER (a class, or an EQL specializer) among their specializers."
  (let ((acc nil))
    (dolist (g *generic-functions*)
      (dolist (m (gf-methods g))
        (when (member specializer (method-specializers m) :test #'equal) (setq acc (cons m acc)))))
    acc))
(defun specializer-direct-generic-functions (specializer)
  "The generic functions that have a method specialized on SPECIALIZER."
  (let ((acc nil))
    (dolist (m (specializer-direct-methods specializer))
      (unless (memq (gf-fn (method-gf m)) acc) (setq acc (cons (gf-fn (method-gf m)) acc))))
    acc))

;;; ---- dependents ---------------------------------------------------------------------------------------------------

(defvar *dependents* (make-hash-table :test 'eq) "Metaobject -> its dependents (ADD-DEPENDENT).")

(defgeneric update-dependent (metaobject dependent &rest initargs))
(defmethod update-dependent ((metaobject t) dependent &rest initargs)
  (declare (ignore dependent initargs))
  nil)

(defun %dependents-key (metaobject)
  "A generic function is keyed by its record, whether it is given as the function or the record."
  (if (functionp metaobject) (or (gethash metaobject *gf-by-function*) metaobject) metaobject))

(defun add-dependent (metaobject dependent)
  "Register DEPENDENT to be told (UPDATE-DEPENDENT) when the class or generic function METAOBJECT changes."
  (let ((k (%dependents-key metaobject)))
    (unless (memq dependent (gethash k *dependents*))
      (setf (gethash k *dependents*) (cons dependent (gethash k *dependents*)))))
  nil)
(defun remove-dependent (metaobject dependent)
  "Stop telling DEPENDENT about changes to METAOBJECT."
  (let ((k (%dependents-key metaobject)))
    (setf (gethash k *dependents*) (remove dependent (gethash k *dependents*))))
  nil)
(defun map-dependents (metaobject function)
  "Call FUNCTION on each dependent of METAOBJECT."
  (dolist (d (gethash (%dependents-key metaobject) *dependents*)) (funcall function d))
  nil)
(defun %notify-dependents (metaobject &rest initargs)
  (let ((k (%dependents-key metaobject)))
    (when (gethash k *dependents*)
      (let ((object (if (%gf-p k) (gf-fn k) k)))
        (dolist (d (gethash k *dependents*)) (apply #'update-dependent object d initargs))))))

;;; ---- funcallable instances --------------------------------------------------------------------------------------

(defun set-funcallable-instance-function (funcallable-instance function)
  "Make calling FUNCALLABLE-INSTANCE (an instance of a class whose metaclass is FUNCALLABLE-STANDARD-CLASS) run FUNCTION."
  (unless (%funcallable-clos-p funcallable-instance)
    (error "~S is not a funcallable instance" funcallable-instance))
  (%struct-set funcallable-instance 3 function)
  funcallable-instance)

(defun funcallable-instance-function (funcallable-instance)
  "The function calling FUNCALLABLE-INSTANCE runs."
  (unless (%funcallable-clos-p funcallable-instance)
    (error "~S is not a funcallable instance" funcallable-instance))
  (%struct-ref funcallable-instance 3))

(setq *mop-loaded* t)
