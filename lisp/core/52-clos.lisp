;;;; 52-clos.lisp -- CLOS: classes, generic functions, methods, instances, TYPEP.
;;;;
;;;; An instance is a structure (type T_STRUCT) of three slots: its class, its layout and a vector of its local
;;;; slot values (UNSUPPLIED marks "unbound").  The layout is the class's index alist -- slot name -> vector index
;;;; for a local slot, or the shared cell (a cons whose car is the value) for an :allocation :class slot -- as it was
;;;; when the instance was made or last updated.  Redefining a class so that its slots change gives it a new index;
;;;; an instance whose layout is not its class's current index is updated (UPDATE-INSTANCE-FOR-REDEFINED-CLASS) the
;;;; next time one of its slots is touched.  CHANGE-CLASS replaces the class and the vector.
;;;;
;;;; A class is a %CLASS structure.  Its metaclass is STANDARD-CLASS (or BUILT-IN-CLASS, STRUCTURE-CLASS); a class
;;;; defined with (:metaclass M) keeps the values of M's slots in a hidden instance of M, its META.
;;;;
;;;; Generic functions are ordinary closures stored in the function cell; dispatch results are cached per generic
;;;; function keyed on argument class ids.  Method combinations: STANDARD, the built-in short forms (+ AND APPEND
;;;; LIST MAX MIN NCONC OR PROGN) and DEFINE-METHOD-COMBINATION (short and long forms).

(in-package :cl)

(defvar *print-escape* t)
(defvar *class-counter* 0)

(defstruct (%class (:constructor %make-class) (:predicate classp) (:conc-name %cls-) (:copier nil))
  name (supers nil) (direct-slots nil) (cpl nil) (slots nil) (index nil) (id 0)
  (subclasses nil) (default-initargs nil) (kind :standard) (nlocal 0) (meta nil) (documentation nil))

(defstruct (slot-definition (:conc-name slotd-) (:constructor %make-slotd) (:copier nil))
  name (initargs nil) (initfn nil) (readers nil) (writers nil) (allocation :instance) (cell nil) (type t)
  (documentation nil))

(defstruct (%gf (:conc-name gf-) (:constructor %make-gf) (:copier nil))
  name (methods nil) cache (dispatch-n 0) (nreq 0) (has-eql nil) (lambda-list nil) fn (combination nil)
  (documentation nil))

(defstruct (%method (:conc-name method-) (:constructor %make-method) (:copier nil))
  gf qualifiers specializers fn)

(defun %structp (x) (structp x))

;;; ---- class registry ------------------------------------------------------------

(defun %make-struct-class (name)
  (let* ((parent (get name '%struct-parent))
         (c (%make-class :name name :kind :struct
                         :supers (list (if parent (find-class parent) (find-class 'structure-object))))))
    (setf (%cls-id c) (setq *class-counter* (+ *class-counter* 1)))
    (%finalize-class c)
    (%put name '%class c)
    c))

(defun find-class (name &optional (errorp t) env)
  (declare (ignore env))
  (or (and (symbolp name) (get name '%class))
      (and (symbolp name) (get name '%struct-desc) (%make-struct-class name))
      (and errorp (error "There is no class named ~S" name))))

(defun class-name (c) (%cls-name c))
(defun class-precedence-list (c) (%cls-cpl c))
(defun class-direct-superclasses (c) (%cls-supers c))
(defun class-direct-subclasses (c) (%cls-subclasses c))
(defun class-slots (c) (%cls-slots c))
(defun class-direct-slots (c) (%cls-direct-slots c))
(defun class-finalized-p (c) (declare (ignore c)) t)
(defun finalize-inheritance (c) (%finalize-class c) c)
(defun slot-definition-name (s) (slotd-name s))
(defun slot-definition-initargs (s) (slotd-initargs s))
(defun slot-definition-allocation (s) (slotd-allocation s))
(defun slot-definition-initform (s) (if (slotd-initfn s) (funcall (slotd-initfn s)) nil))
(defun slot-definition-initfunction (s) (slotd-initfn s))
(defun slot-definition-readers (s) (slotd-readers s))
(defun slot-definition-writers (s) (slotd-writers s))
(defun slot-definition-type (s) (slotd-type s))
(defun class-direct-default-initargs (c) (mapcar (lambda (d) (list (car d) nil (cdr d))) (%cls-default-initargs c)))
(defun class-default-initargs (c) (mapcar (lambda (d) (list (car d) nil (cdr d))) (get-default-initargs-cache c)))
(defun class-prototype (c) (allocate-instance c))

(defun %c3 (class supers)
  (let ((seqs (append (mapcar #'%cls-cpl supers) (list supers)))
        (result (list class)))
    (block nil
      (tagbody top
         (setq seqs (remove nil seqs))
         (when (null seqs) (return))
         (let ((cand nil))
           (dolist (s seqs)
             (let ((c (car s)))
               (unless (some (lambda (s2) (member c (cdr s2))) seqs)
                 (setq cand c)
                 (return))))
           (unless cand (error "Inconsistent class precedence list for ~S" (%cls-name class)))
           (setq result (cons cand result))
           (setq seqs (mapcar (lambda (s) (if (eq (car s) cand) (cdr s) s)) seqs)))
         (go top)))
    (nreverse result)))

(defun %compute-slots (cpl)
  ;; Superclass slots come first; a more specific class overrides a slot's initform.
  (let ((order nil) (table nil))
    (dolist (c (reverse cpl))
      (dolist (sd (%cls-direct-slots c))
        (let ((cell (assq (slotd-name sd) table)))
          (if cell
              (let ((m (cdr cell)))
                (setf (slotd-initargs m) (union (slotd-initargs m) (slotd-initargs sd)))
                (when (slotd-initfn sd) (setf (slotd-initfn m) (slotd-initfn sd)))
                (setf (slotd-allocation m) (slotd-allocation sd) (slotd-cell m) (slotd-cell sd)))
              (let ((m (copy-structure sd)))
                (setq table (cons (cons (slotd-name sd) m) table))
                (setq order (cons (slotd-name sd) order)))))))
    (mapcar (lambda (n) (cdr (assq n table))) (nreverse order))))

(defun %finalize-class (class)
  (let* ((cpl (if (get (%cls-name class) '%flavor-def)
                  (%flavor-class-cpl class)                 ; a flavor: Flavors component order (55-flavors.lisp)
                  (%c3 class (%cls-supers class))))
         (slots (%compute-slots cpl))
         (i -1)
         (index (mapcar (lambda (sd) (cons (slotd-name sd)
                                           (if (eq (slotd-allocation sd) :class) (slotd-cell sd) (setq i (+ i 1)))))
                        slots))
         (old (%cls-index class)))
    (setf (%cls-cpl class) cpl)
    (setf (%cls-slots class) slots)
    (setf (%cls-nlocal class) (+ i 1))
    ;; the same slots in the same places keep the old index, so existing instances need no update
    (unless (and old (= (length old) (length index))
                 (every (lambda (a b) (and (eq (car a) (car b)) (eql (cdr a) (cdr b)))) old index))
      (setf (%cls-index class) index))
    (let ((inits nil))
      (dolist (c cpl)
        (dolist (di (%cls-default-initargs c))
          (unless (assq (car di) inits) (setq inits (cons di inits)))))
      (set-default-initargs-cache class (nreverse inits)))
    (dolist (s (%cls-supers class))
      (unless (memq class (%cls-subclasses s))
        (setf (%cls-subclasses s) (cons class (%cls-subclasses s)))))
    (dolist (sub (%cls-subclasses class)) (%finalize-class sub))
    class))

;; default-initargs: the DIRECT alist lives in the class; the merged alist is cached on the name's plist
(defun get-default-initargs-cache (class) (get (%cls-name class) '%all-initargs))
(defun set-default-initargs-cache (class v) (%put (%cls-name class) '%all-initargs v))

(defun %register-class (c) (%put (%cls-name c) '%class c) c)

(defun %make-builtin (name supers)
  ;; When the system is loaded a second time (the self-hosting build loads it into the stage-1 image), keep the
  ;; existing class: structure classes made by the first load hold it in their precedence lists, and a new T would
  ;; leave them out of every method specialized on T.
  (let ((old (get name '%class)))
    (if (and old (eq (%cls-kind old) :builtin))
        old
        (let ((c (%make-class :name name :kind :builtin :supers (mapcar #'find-class supers))))
          (setf (%cls-id c) (setq *class-counter* (+ *class-counter* 1)))
          (%register-class c)
          (%finalize-class c)
          c))))

(%make-builtin 't nil)
(%make-builtin 'standard-object '(t))
(%make-builtin 'structure-object '(t))
(%make-builtin 'sequence '(t))
(%make-builtin 'list '(sequence))
(%make-builtin 'cons '(list))
(%make-builtin 'symbol '(t))
(%make-builtin 'null '(symbol list))
(%make-builtin 'number '(t))
(%make-builtin 'real '(number))
(%make-builtin 'rational '(real))
(%make-builtin 'integer '(rational))
(%make-builtin 'fixnum '(integer))
(%make-builtin 'bignum '(integer))
(%make-builtin 'ratio '(rational))
(%make-builtin 'float '(real))
(%make-builtin 'complex '(number))
(%make-builtin 'character '(t))
(%make-builtin 'array '(t))
(%make-builtin 'vector '(array sequence))
(%make-builtin 'string '(vector))
(%make-builtin 'function '(t))
(%make-builtin 'stream '(t))
(%make-builtin 'package '(t))
(%make-builtin 'hash-table '(structure-object))
;; metaclasses: a class object is a %CLASS structure, so %CLASS is in their precedence lists
(%make-builtin 'class '(%class standard-object))
(%make-builtin 'built-in-class '(class))
(%make-builtin 'structure-class '(class))
(%make-builtin 'standard-class '(class))
(%make-builtin 'method-combination '(t))

(defun %class-metaclass (c)
  (cond ((%cls-meta c) (%struct-type (%cls-meta c)))
        ((eq (%cls-kind c) :builtin) (find-class 'built-in-class))
        ((eq (%cls-kind c) :struct) (find-class 'structure-class))
        (t (find-class 'standard-class))))

(defun type-of (x)
  "The name of the most specific type of X (the name of its class)."
  (cond ((null x) 'null) ((eq x t) 'boolean) (t (%cls-name (class-of x)))))

(defun class-of (x)
  (cond ((fixnump x) (find-class 'fixnum))
        ((null x) (find-class 'null))
        ((consp x) (find-class 'cons))
        ((symbolp x) (find-class 'symbol))
        ((stringp x) (find-class 'string))
        ((characterp x) (find-class 'character))
        ((bignump x) (find-class 'bignum))
        ((ratiop x) (find-class 'ratio))
        ((floatp x) (find-class 'float))
        ((complexp x) (find-class 'complex))
        ((classp x) (%class-metaclass x))
        ((structp x)
         (let ((d (%struct-type x)))
           (cond ((classp d) d)
                 ((consp d) (find-class (car d)))
                 (t (find-class 'structure-object)))))
        ((functionp x) (find-class 'function))
        ((vectorp x) (find-class 'vector))
        ((%md-array-p x) (find-class 'array))
        ((packagep x) (find-class 'package))
        ((%native-stream-p x) (find-class 'stream))
        (t (find-class 't))))

(defun %subclass-p (class super) (if (memq super (%cls-cpl class)) t nil))

;;; ---- TYPEP / SUBTYPEP -----------------------------------------------------------

(defun typep (x type)
  (cond ((eq type t) t)
        ((null type) nil)
        ((symbolp type)
         (case type
           (null (null x)) (symbol (symbolp x)) (keyword (keywordp x))
           (list (listp x)) (cons (consp x)) (atom (atom x))
           (integer (integerp x)) (fixnum (fixnump x)) (bignum (bignump x))
           (ratio (ratiop x)) (rational (rationalp x)) (number (numberp x)) (real (realp x)) (complex (complexp x))
           ((float single-float double-float short-float long-float) (floatp x))
           (string (stringp x)) (simple-string (stringp x)) (character (characterp x))
           (vector (vectorp x)) (simple-vector (simple-vector-p x)) ((array simple-array) (arrayp x))
           ((function compiled-function) (functionp x))
           (package (packagep x)) (stream (streamp x))
           (hash-table (%hash-table-p x))
           (sequence (or (listp x) (vectorp x)))
           (t (let ((c (find-class type nil)))
                (cond (c (%subclass-p (class-of x) c))
                      ((get type '%deftype) (typep x (funcall (get type '%deftype))))
                      (t (error "Unknown type specifier: ~S" type)))))))
        ((consp type)
         (let ((h (car type)))
           (case h
             (or (some (lambda (ty) (typep x ty)) (cdr type)))
             (and (every (lambda (ty) (typep x ty)) (cdr type)))
             (not (not (typep x (cadr type))))
             (member (if (memql x (cdr type)) t nil))
             (eql (eql x (cadr type)))
             (satisfies (funcall (cadr type) x))
             ((array simple-array)
              (and (arrayp x)
                   (let ((dims (caddr type)))
                     (cond ((or (null (cddr type)) (eq dims '*)) t)
                           ((integerp dims) (= (array-rank x) dims))
                           (t (and (= (length dims) (array-rank x))
                                   (every (lambda (d a) (or (eq d '*) (eql d a))) dims (array-dimensions x))))))))
             ((vector simple-vector) (and (vectorp x) (or (null (cddr type)) (eq (caddr type) '*) (eql (caddr type) (length x)))))
             (complex (and (complexp x) (or (null (cdr type)) (eq (cadr type) '*)
                                             (and (typep (realpart x) (cadr type)) (typep (imagpart x) (cadr type))))))
             ((integer rational real float)
              (and (typep x h)
                   (or (null (cdr type)) (eq (cadr type) '*) (>= x (cadr type)))
                   (or (null (cddr type)) (eq (caddr type) '*) (<= x (caddr type)))))
             (t (error "Unknown type specifier: ~S" type)))))
        ((classp type) (%subclass-p (class-of x) type))
        (t (error "Bad type specifier: ~S" type))))

(defun subtypep (a b)
  (cond ((equal a b) (values t t))
        ((eq b t) (values t t))
        ((null a) (values t t))
        ((and (symbolp a) (symbolp b) (find-class a nil) (find-class b nil))
         (values (%subclass-p (find-class a) (find-class b)) t))
        (t (values nil nil))))

;;; ---- slots ---------------------------------------------------------------------------

(defun %slot-missing-error (class obj name operation)
  (error "The slot ~S is missing from the object ~S of class ~S (during ~S)" name obj
         (if (classp class) (%cls-name class) class) operation))
;; (SLOT-MISSING, SLOT-UNBOUND and UPDATE-INSTANCE-FOR-REDEFINED-CLASS are generic functions defined below)

(defun %std-instance-p (x) (and (structp x) (classp (%struct-type x))))

(defun %slot-holder (obj)
  "The standard instance that holds OBJ's slots: OBJ itself, or a class's metaclass instance."
  (if (and (classp obj) (%cls-meta obj)) (%cls-meta obj) obj))

(defun %current-layout (inst)
  "INST's class's index, after updating INST if its class was redefined."
  (let ((index (%cls-index (%struct-type inst))))
    (unless (eq (%struct-ref inst 1) index) (%update-obsolete-instance inst))
    index))

(defun %slot-location (obj name)
  "A local slot's vector index or a shared slot's cell; NIL if OBJ has no such slot.  For a DEFSTRUCT instance,
the structure index."
  (let ((o (%slot-holder obj)))
    (if (%std-instance-p o)
        (cdr (assq name (%current-layout o)))
        (let ((d (%struct-type o)))
          (and (consp d) (let ((p (position name (cdr d)))) (and p (+ p 1))))))))

(defun %loc-ref (o loc)
  (cond ((consp loc) (car loc)) ((%std-instance-p o) (aref (%struct-ref o 2) loc)) (t (%struct-ref o loc))))
(defun %loc-set (o loc v)
  (cond ((consp loc) (rplaca loc v) v) ((%std-instance-p o) (%aset (%struct-ref o 2) loc v) v) (t (%struct-set o loc v))))

(defun slot-value (obj name)
  (let ((loc (and (structp obj) (%slot-location obj name))))
    (if (null loc)
        (slot-missing (class-of obj) obj name 'slot-value)
        (let ((v (%loc-ref (%slot-holder obj) loc)))
          (if (%unsupplied-p v) (slot-unbound (class-of obj) obj name) v)))))

(defun %set-slot-value (obj name value)
  (let ((loc (and (structp obj) (%slot-location obj name))))
    (if (null loc)
        (progn (slot-missing (class-of obj) obj name 'setf value) value)
        (%loc-set (%slot-holder obj) loc value))))
(%def-setf 'slot-value '%set-slot-value)

(defun slot-boundp (obj name)
  (let ((loc (and (structp obj) (%slot-location obj name))))
    (if loc
        (not (%unsupplied-p (%loc-ref (%slot-holder obj) loc)))
        (slot-missing (class-of obj) obj name 'slot-boundp))))
(defun slot-exists-p (obj name) (and (structp obj) (%slot-location obj name) t))
(defun slot-makunbound (obj name)
  (let ((loc (and (structp obj) (%slot-location obj name))))
    (if loc (%loc-set (%slot-holder obj) loc (%unsupplied)) (slot-missing (class-of obj) obj name 'slot-makunbound))
    obj))

(defun %update-obsolete-instance (inst)
  "Give INST the current layout of its (redefined) class, keeping the values of the slots still local, then call
UPDATE-INSTANCE-FOR-REDEFINED-CLASS."
  (let* ((class (%struct-type inst)) (old (%struct-ref inst 1)) (oldvec (%struct-ref inst 2))
         (new (%cls-index class)) (vec (%make-vector (%cls-nlocal class) (%unsupplied)))
         (added nil) (discarded nil) (plist nil))
    (dolist (e new)
      (when (integerp (cdr e))
        (let ((o (assq (car e) old)))
          (cond ((null o) (setq added (cons (car e) added)))
                ((integerp (cdr o)) (%aset vec (cdr e) (aref oldvec (cdr o))))
                (t (%aset vec (cdr e) (car (cdr o))))))))          ; shared -> local keeps the value
    (dolist (o old)
      (when (integerp (cdr o))
        (let ((e (assq (car o) new)))
          (unless (and e (integerp (cdr e)))
            (setq discarded (cons (car o) discarded))
            (let ((v (aref oldvec (cdr o))))
              (unless (%unsupplied-p v) (setq plist (list* (car o) v plist))))))))
    (%struct-set inst 1 new)
    (%struct-set inst 2 vec)
    (update-instance-for-redefined-class inst (nreverse added) (nreverse discarded) plist)
    inst))

(defun make-instances-obsolete (class)
  "Make every instance of CLASS (and of its subclasses) be updated the next time it is used."
  (let ((c (if (classp class) class (find-class class))))
    (setf (%cls-index c) (copy-alist (%cls-index c)))
    (dolist (sub (%cls-subclasses c)) (make-instances-obsolete sub))
    c))

;;; ---- DEFCLASS ----------------------------------------------------------------------------

(defun %tree-contains (tree sym)
  (cond ((eq tree sym) t)
        ((consp tree) (or (%tree-contains (car tree) sym) (%tree-contains (cdr tree) sym)))
        (t nil)))

(defun %slotd-form (spec)
  ;; SPEC: name | (name . options)  ->  form constructing a slot-definition
  (let* ((name (if (consp spec) (car spec) spec))
         (opts (if (consp spec) (cdr spec) nil))
         (initargs nil) (readers nil) (writers nil) (initform nil) (has-init nil) (alloc :instance) (type t) (doc nil))
    (do-list-tails (o opts)
      (let ((k (car o)) (v (cadr o)))
        (cond ((eq k :initarg) (setq initargs (append initargs (list v))))
              ((eq k :reader) (setq readers (append readers (list v))))
              ((eq k :writer) (setq writers (append writers (list v))))
              ((eq k :accessor) (setq readers (append readers (list v)))
               (setq writers (append writers (list (list 'setf v)))))
              ((eq k :initform) (setq initform v has-init t))
              ((eq k :allocation) (setq alloc v))
              ((eq k :type) (setq type v))
              ((eq k :documentation) (setq doc v))))
      (setq o (cdr o)))
    (list 'list :name (list 'quote name) :initargs (list 'quote initargs)
          :readers (list 'quote readers) :writers (list 'quote writers)
          :allocation (list 'quote alloc) :type (list 'quote type) :documentation doc
          :initfn (if has-init (list 'lambda nil initform) nil))))

(defun %make-slotd-from-plist (plist)
  (%make-slotd :name (getf plist :name) :initargs (getf plist :initargs) :readers (getf plist :readers)
               :writers (getf plist :writers) :allocation (getf plist :allocation) :initfn (getf plist :initfn)
               :type (getf plist :type) :documentation (getf plist :documentation)))

(defmacro defclass (name supers slots &rest options)
  (let ((accessor-forms nil) (default-initargs nil) (metaclass nil) (doc nil) (extra nil))
    (%symbol-lock-check name "define a class named")
    (dolist (spec slots)
      (let ((sname (if (consp spec) (car spec) spec)) (opts (if (consp spec) (cdr spec) nil)))
        (do-list-tails (o opts)
          (let ((k (car o)) (v (cadr o)))
            (when (or (eq k :reader) (eq k :accessor))
              (setq accessor-forms
                    (cons (list 'defmethod v (list (list 'o name)) (list 'slot-value 'o (list 'quote sname)))
                          accessor-forms)))
            (when (or (eq k :writer) (eq k :accessor))
              (setq accessor-forms
                    (cons (list 'defmethod (if (eq k :writer) v (list 'setf v)) (list 'new-value (list 'o name))
                                (list 'setf (list 'slot-value 'o (list 'quote sname)) 'new-value))
                          accessor-forms))))
          (setq o (cdr o)))))
    (dolist (o options)
      (case (car o)
        (:default-initargs
         (do-list-tails (p (cdr o))
           (setq default-initargs (cons (list 'cons (car p) (list 'lambda nil (cadr p))) default-initargs))
           (setq p (cdr p))))
        (:metaclass (setq metaclass (cadr o)))
        (:documentation (setq doc (cadr o)))
        ;; any other option is an initarg for the metaclass: (:option value) passes VALUE, (:option v1 v2 ...) the list
        (t (setq extra (list* (list 'quote (if (cddr o) (cdr o) (cadr o))) (car o) extra)))))
    (list* 'progn
           (list '%ensure-class (list 'quote name) (list 'quote supers)
                 (cons 'list (mapcar (lambda (s) (list '%make-slotd-from-plist (%slotd-form s))) slots))
                 (cons 'list (nreverse default-initargs))
                 (list 'quote metaclass) doc (cons 'list (nreverse extra)))
           (append (nreverse accessor-forms) (list (list 'quote name))))))

(defun %ensure-class (name supers slotds default-initargs &optional metaclass doc meta-initargs)
  (let* ((old (get name '%class))
         (class (if (and old (not (eq (%cls-kind old) :builtin))) old (%make-class :name name)))
         (meta (and metaclass (not (eq metaclass 'standard-class))
                    (or (find-class metaclass nil) (error "The metaclass ~S of ~S is not defined" metaclass name)))))
    (when (eql (%cls-id class) 0) (setf (%cls-id class) (setq *class-counter* (+ *class-counter* 1))))
    (when (and meta (not (%subclass-p meta (find-class 'standard-class))))
      (error "The metaclass ~S of ~S is not a subclass of STANDARD-CLASS" metaclass name))
    (setf (%cls-supers class)
          (mapcar (lambda (s) (or (find-class s nil) (error "Superclass ~S of ~S is not defined" s name)))
                  (or supers '(standard-object))))
    ;; a shared slot keeps its cell (and value) when the class is redefined; a new one is initialized now
    (dolist (sd slotds)
      (when (eq (slotd-allocation sd) :class)
        (let ((o (find (slotd-name sd) (%cls-direct-slots class) :key #'slotd-name)))
          (setf (slotd-cell sd)
                (if (and o (slotd-cell o))
                    (slotd-cell o)
                    (list (if (slotd-initfn sd) (funcall (slotd-initfn sd)) (%unsupplied))))))))
    (setf (%cls-direct-slots class) slotds)
    (setf (%cls-default-initargs class) default-initargs)
    (setf (%cls-documentation class) doc)
    (%register-class class)
    (%finalize-class class)
    ;; the metaclass's slots live in a hidden instance of the metaclass
    (cond ((null meta) (setf (%cls-meta class) nil))
          ((and (%cls-meta class) (eq (%struct-type (%cls-meta class)) meta))
           (%shared-init (%cls-meta class) nil meta-initargs))
          (t (let ((m (%allocate-std-instance meta)))
               (setf (%cls-meta class) m)
               (%shared-init m t (%add-default-initargs meta meta-initargs)))))
    class))

(defun ensure-class (name &key direct-superclasses direct-slots direct-default-initargs metaclass documentation
                          &allow-other-keys)
  "Define or redefine the class NAME (the functional form of DEFCLASS).  DIRECT-SLOTS is a list of plists such as
(:name x :initargs (:x) :initform 0)."
  (%ensure-class name (mapcar (lambda (s) (if (classp s) (%cls-name s) s)) direct-superclasses)
                 (mapcar (lambda (pl)
                           (let ((init (getf pl :initfunction)) (form (getf pl :initform '%none)))
                             (%make-slotd-from-plist
                              (list* :initfn (or init (if (eq form '%none) nil (lambda () (eval form)))) pl))))
                         direct-slots)
                 (mapcar (lambda (d) (cons (car d) (or (caddr d) (let ((f (cadr d))) (lambda () (eval f))))))
                         direct-default-initargs)
                 (if (classp metaclass) (%cls-name metaclass) metaclass) documentation nil))

(defun %set-find-class (name class)
  (if class (%put name '%class class) (remprop name '%class))
  class)
(%def-setf 'find-class '%set-find-class)

;;; ---- generic functions ---------------------------------------------------------------------

(defun %find-gf (name) (get (%fn-symbol name) '%gf))

(defvar *generic-functions* nil)

(defun %required-count (ll)
  (let ((n 0))
    (dolist (x ll) (when (member x '(&optional &rest &key &aux &body &allow-other-keys)) (return)) (setq n (+ n 1)))
    n))

(defun %ensure-gf (name nreq &optional lambda-list)
  (or (let ((gf (%find-gf name)))
        ;; (a DEFUN of the same name since may have replaced the function)
        (when (and gf (not (eq (symbol-function (%fn-symbol name)) (gf-fn gf))))
          (%set-symbol-function (%fn-symbol name) (gf-fn gf)))
        gf)
      (let* ((sym (%fn-symbol name))
             (gf (%make-gf :name name :cache (make-hash-table :test 'eql) :nreq nreq :lambda-list lambda-list))
             (fn (lambda (&rest args) (%call-gf gf args))))
        (setf (gf-fn gf) fn)
        (%put sym '%gf gf)
        (%set-symbol-function sym fn)
        (setq *generic-functions* (cons gf *generic-functions*))
        gf)))

(defun ensure-generic-function (name &key lambda-list &allow-other-keys)
  (%ensure-gf name (%required-count lambda-list) lambda-list)
  (symbol-function (%fn-symbol name)))

(defmacro defgeneric (name ll &rest options)
  (let ((mc (cdr (assoc :method-combination options))) (doc (cadr (assoc :documentation options))))
    (list* 'progn
           (list '%ensure-gf (list 'quote name) (%required-count ll) (list 'quote ll))
           (list '%set-gf-options (list 'quote name) (list 'quote mc) doc)
           (append (mapcar (lambda (o) (if (eq (car o) :method) (cons 'defmethod (cons name (cdr o))) nil)) options)
                   (list (list 'quote name))))))

(defun %set-gf-options (name combination doc)
  (let ((gf (%find-gf name)))
    (when (and combination (not (eq (car combination) 'standard)) (null (get (car combination) '%method-combination)))
      (error "~S is not a method combination" (car combination)))
    (setf (gf-combination gf) (if (eq (car combination) 'standard) nil combination)
          (gf-documentation gf) doc)
    (%recompute-gf gf)))

(defun %as-gf (x) (cond ((%gf-p x) x) ((functionp x) (find x *generic-functions* :key #'gf-fn)) (t (%find-gf x))))
(defun generic-function-methods (gf) (gf-methods (%as-gf gf)))
(defun %gf-p (x) (%struct-typep x '%gf))
(defun method-qualifiers-of (m) (method-qualifiers m))

(defmacro defmethod (name &rest rest)
  ;; (defmethod (flavor [type] message) ...) defines a Flavors method (55-flavors.lisp); (setf f) names are CLOS's
  (if (and (consp name) (not (eq (car name) 'setf)))
      (%flavor-defmethod name rest)
      (%clos-defmethod name rest)))

(defun %clos-defmethod (name rest)
  (let ((qualifiers nil))
    (block nil
      (tagbody top
         (when (and rest (not (listp (car rest))))
           (setq qualifiers (cons (car rest) qualifiers))
           (setq rest (cdr rest))
           (go top))))
    (let* ((ll (car rest)) (body (cdr rest))
           (plain nil) (specs nil) (in-required t))
      (dolist (p ll)
        (cond ((and in-required (member p '(&optional &rest &key &aux &body &allow-other-keys)))
               (setq in-required nil)
               (setq plain (cons p plain)))
              ((and in-required (consp p))
               (setq plain (cons (car p) plain))
               (let ((s (cadr p)))
                 (setq specs (cons (if (and (consp s) (eq (car s) 'eql)) (list 'cons :eql (cadr s)) (list 'quote s)) specs))))
              (in-required (setq plain (cons p plain)) (setq specs (cons ''t specs)))
              (t (setq plain (cons p plain)))))
      (setq plain (nreverse plain))
      (setq specs (nreverse specs))
      (let ((fn (if (or (%tree-contains body 'call-next-method) (%tree-contains body 'next-method-p))
                    (list 'lambda '(%args %next)
                          (list 'flet '((call-next-method (&rest cnm-args)
                                         (if %next
                                             (funcall %next (if cnm-args cnm-args %args))
                                             (error "CALL-NEXT-METHOD: no next method")))
                                        (next-method-p () (if %next t nil)))
                                (list 'apply (list 'function (list* 'lambda plain body)) '%args)))
                    (list 'lambda '(%args %next)
                          (list 'apply (list 'function (list* 'lambda plain body)) '%args)))))
        (list '%add-method (list 'quote name) (list 'quote (nreverse qualifiers))
              (cons 'list specs) (list 'function fn))))))

(defun %resolve-spec (s) (if (symbolp s) (find-class s) s))

(defun %add-method (name qualifiers specs fn)
  (let* ((gf (or (%find-gf name) (%ensure-gf name (length specs))))
         (specs (mapcar #'%resolve-spec specs))
         (m (%make-method :gf gf :qualifiers qualifiers :specializers specs :fn fn)))
    (setf (gf-methods gf)
          (cons m (remove-if (lambda (o) (and (equal (method-qualifiers o) qualifiers)
                                              (equal (method-specializers o) specs)))
                             (gf-methods gf))))
    (%recompute-gf gf)
    (%set-function-name fn name)
    m))

(defun %recompute-gf (gf)
  (let ((n 0) (has-eql nil) (tc (find-class 't)))
    (dolist (m (gf-methods gf))
      (let ((i 0))
        (dolist (s (method-specializers m))
          (setq i (+ i 1))
          (when (consp s) (setq has-eql t))
          (unless (eq s tc) (when (> i n) (setq n i))))))
    (setf (gf-dispatch-n gf) n)
    (setf (gf-has-eql gf) has-eql)
    (clrhash (gf-cache gf))
    gf))

(defun remove-method (gf m)
  (let ((g (%as-gf gf)))
    (setf (gf-methods g) (remove m (gf-methods g)))
    (%recompute-gf g)
    gf))

(defun find-method (gf qualifiers specs &optional (errorp t))
  (let* ((g (%as-gf gf))
         (specs (mapcar #'%resolve-spec specs))
         (m (find-if (lambda (o) (and (equal (method-qualifiers o) qualifiers) (equal (method-specializers o) specs)))
                     (gf-methods g))))
    (or m (and errorp (error "No such method")))))

(defun %spec-applicable-p (spec arg)
  (if (consp spec) (eql arg (cdr spec)) (%subclass-p (class-of arg) spec)))

(defun %method-applicable-p (m args)
  (let ((a args))
    (dolist (s (method-specializers m) t)
      (unless (%spec-applicable-p s (car a)) (return nil))
      (setq a (cdr a)))))

(defun %spec-rank (spec class)
  (if (consp spec) -1 (or (position spec (%cls-cpl class)) 99999)))

(defun %more-specific-p (a b classes)
  (let ((sa (method-specializers a)) (sb (method-specializers b)) (cl classes))
    (block nil
      (tagbody top
         (when (or (null sa) (null sb)) (return nil))
         (let ((ra (%spec-rank (car sa) (or (car cl) (find-class 'null)))) (rb (%spec-rank (car sb) (or (car cl) (find-class 'null)))))
           (cond ((< ra rb) (return t)) ((> ra rb) (return nil))))
         (setq sa (cdr sa) sb (cdr sb) cl (cdr cl))
         (go top)))))

(defun compute-applicable-methods (gf args)
  (let* ((g (%as-gf gf))
         (acc nil)
         (classes (mapcar #'class-of args)))
    (dolist (m (gf-methods g)) (when (%method-applicable-p m args) (setq acc (cons m acc))))
    (sort acc (lambda (a b) (%more-specific-p a b classes)))))

(defun %chain (methods final)
  (if (null methods)
      final
      (let ((m (car methods)) (next (%chain (cdr methods) final)))
        (lambda (args) (funcall (method-fn m) args next)))))

(defun %compute-effective-method (gf args)
  (let ((comb (gf-combination gf)))
    (if comb
        (let ((methods (compute-applicable-methods gf args)))
          (if (null methods)
              (lambda (a) (%no-applicable-method gf a))
              (funcall (get (car comb) '%method-combination) gf methods (cdr comb))))
        (%standard-effective-method gf args))))

(defun %standard-effective-method (gf args)
  (let ((methods (compute-applicable-methods gf args))
        (around nil) (before nil) (primary nil) (after nil))
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
        (lambda (a) (%no-applicable-method gf a))
        (let* ((chain (%chain primary nil))
               (main (if (or before after)
                         (lambda (a)
                           (dolist (b before) (funcall (method-fn b) a nil))
                           (let ((vals (multiple-value-list (funcall chain a))))
                             (dolist (f after) (funcall (method-fn f) a nil))
                             (values-list vals)))
                         chain)))
          (if around (%chain around main) main)))))

;;; ---- other method combinations --------------------------------------------------------------
;;; A method combination is a function (gf methods options) -> effective method, a function of the argument list,
;;; kept on the combination name's %METHOD-COMBINATION property.  The long form builds a Lisp form, as the
;;; standard says, and compiles it with CALL-METHOD and MAKE-METHOD defined.

(defun invalid-method-error (method format-control &rest args)
  (error "Invalid method ~S: ~A" method (apply #'format nil format-control args)))
(defun method-combination-error (format-control &rest args)
  (error "Method combination error: ~A" (apply #'format nil format-control args)))

(defun %next-function (methods)
  "The next-method function for METHODS (method objects or (MAKE-METHOD form)s), or NIL."
  (if (null methods)
      nil
      (let ((m (car methods)) (next (%next-function (cdr methods))))
        (if (and (consp m) (eq (car m) 'make-method))
            (%effective-form-function (cadr m))
            (lambda (args) (funcall (method-fn m) args next))))))

(defun %call-method-form (m next)
  (if (and (consp m) (eq (car m) 'make-method))
      (cadr m)
      (list 'funcall (list 'quote (method-fn m)) '%args (list '%next-function (list 'quote next)))))

(defun %effective-form-function (form)
  (eval (list 'lambda '(%args)
              (list 'macrolet '((call-method (m &optional next) (%call-method-form m next))
                                (make-method (form) (list 'quote (list 'make-method form))))
                    form))))

(defun %short-method-combination (name operator identity-with-one-argument)
  (lambda (gf methods options)
    (let ((order (or (car options) :most-specific-first)) (around nil) (primary nil))
      (unless (member order '(:most-specific-first :most-specific-last))
        (method-combination-error "~S is not :MOST-SPECIFIC-FIRST or :MOST-SPECIFIC-LAST" order))
      (dolist (m methods)
        (let ((q (method-qualifiers m)))
          (cond ((equal q '(:around)) (setq around (cons m around)))
                ((equal q (list name)) (setq primary (cons m primary)))
                (t (invalid-method-error m "The ~S method combination accepts only the qualifiers ~S and :AROUND"
                                         name name)))))
      (setq around (nreverse around))
      (unless (eq order :most-specific-last) (setq primary (nreverse primary)))
      (if (null primary)
          (lambda (a) (%no-applicable-method gf a))
          (let ((main (%effective-form-function
                       (if (and identity-with-one-argument (null (cdr primary)))
                           (list 'call-method (car primary))
                           (cons operator (mapcar (lambda (m) (list 'call-method m)) primary))))))
            (if around (%chain around main) main))))))

(defmacro define-method-combination (name &rest rest)
  "Define a method combination.  The short form: (define-method-combination name [:operator op]
[:identity-with-one-argument b] [:documentation s]).  The long form: (define-method-combination name lambda-list
(method-group-specifier...) [(:arguments . args)] [(:generic-function var)] body...)."
  (if (or (null rest) (keywordp (car rest)))
      (let ((operator (getf rest :operator name)) (identity (getf rest :identity-with-one-argument)))
        `(progn (%put ',name '%method-combination (%short-method-combination ',name ',operator ',identity)) ',name))
      (destructuring-bind (lambda-list groups &rest body) rest
        (let ((arguments nil) (gf-var (gensym "GF")))
          (loop (cond ((and (consp (car body)) (eq (caar body) :arguments)) (setq arguments (cdar body) body (cdr body)))
                      ((and (consp (car body)) (eq (caar body) :generic-function))
                       (setq gf-var (cadar body) body (cdr body)))
                      (t (return))))
          (when (stringp (car body)) (setq body (cdr body)))
          (let ((vars (mapcar #'car groups))
                (matchers (mapcar (lambda (g)
                                    (let ((sel (cdr g)) (pats nil))
                                      (loop (when (or (null sel) (keywordp (car sel))) (return))
                                            (setq pats (cons (car sel) pats) sel (cdr sel)))
                                      (setq pats (nreverse pats))
                                      (if (and (= (length pats) 1) (symbolp (car pats)) (car pats) (not (eq (car pats) '*)))
                                          `(list :predicate (function ,(car pats)))
                                          `(list :patterns ',pats))))
                                  groups))
                (checks (mapcan (lambda (g)
                                  (let* ((sel (member-if #'keywordp (cdr g))) (var (car g)))
                                    (append
                                     (when (getf sel :required)
                                       (list `(when (and ,(getf sel :required) (null ,var))
                                                (method-combination-error "No ~S methods" ',var))))
                                     (list `(when (eq ,(getf sel :order :most-specific-first) :most-specific-last)
                                              (setq ,var (reverse ,var)))))))
                                groups)))
            `(progn
               (%put ',name '%method-combination
                     (lambda (,gf-var %methods %options)
                       (declare (ignorable ,gf-var))
                       (destructuring-bind ,lambda-list %options
                         (destructuring-bind ,vars (%partition-methods %methods (list ,@matchers))
                           ,@checks
                           (%effective-form-function
                            (list 'let (list ,@(let ((i -1))
                                                 (mapcar (lambda (a) (setq i (+ i 1)) `(list ',a '(nth ,i %args)))
                                                         (remove-if (lambda (a) (member a '(&rest &optional &whole))) arguments))))
                                  (progn ,@body)))))))
               ',name))))))

(defun %qualifiers-match-p (pattern qualifiers)
  (cond ((eq pattern '*) t)
        ((null pattern) (null qualifiers))
        ((atom pattern) nil)
        ((eq (car pattern) '*) t)
        ((null qualifiers) nil)
        ((eq (car pattern) (car qualifiers)) (%qualifiers-match-p (cdr pattern) (cdr qualifiers)))
        (t nil)))

(defun %partition-methods (methods matchers)
  "Each of METHODS (most specific first) in the first group whose matcher accepts its qualifiers."
  (let ((groups (mapcar (lambda (m) (declare (ignore m)) nil) matchers)))
    (dolist (m methods)
      (let ((q (method-qualifiers m)) (i 0) (placed nil))
        (dolist (mt matchers)
          (when (if (eq (car mt) :predicate)
                    (funcall (cadr mt) q)
                    (some (lambda (p) (%qualifiers-match-p p q)) (cadr mt)))
            (setf (nth i groups) (cons m (nth i groups))) (setq placed t) (return))
          (setq i (+ i 1)))
        (unless placed (invalid-method-error m "No method group accepts the qualifiers ~S" q))))
    (mapcar #'nreverse groups)))

(define-method-combination + :identity-with-one-argument t)
(define-method-combination and :identity-with-one-argument t)
(define-method-combination append :identity-with-one-argument nil)
(define-method-combination list :identity-with-one-argument nil)
(define-method-combination max :identity-with-one-argument t)
(define-method-combination min :identity-with-one-argument t)
(define-method-combination nconc :identity-with-one-argument t)
(define-method-combination or :identity-with-one-argument t)
(define-method-combination progn :identity-with-one-argument t)

(defun %no-applicable-method (gf args)
  (error "No applicable method for generic function ~S with arguments ~S" (gf-name gf) args))

(defun %dispatch-key (gf args)
  (let ((n (gf-dispatch-n gf)))
    (cond ((or (gf-has-eql gf) (> n 3)) nil)
          (t (let ((k 0) (m 1) (a args))
               (dotimes (i n)
                 (setq k (+ k (* m (%cls-id (class-of (car a))))))
                 (setq m (* m 65536))
                 (setq a (cdr a)))
               k)))))

(defun %call-gf (gf args)
  (let* ((key (%dispatch-key gf args))
         (em (and key (gethash key (gf-cache gf)))))
    (unless em
      (setq em (%compute-effective-method gf args))
      (when key (setf (gethash key (gf-cache gf)) em)))
    (funcall em args)))

(defun call-next-method (&rest args) (declare (ignore args)) (error "CALL-NEXT-METHOD called outside of a method"))

;;; ---- instances -------------------------------------------------------------------------------

(defun %allocate-std-instance (c)
  (let ((inst (%make-instance c 2)))
    (%struct-set inst 1 (%cls-index c))
    (%struct-set inst 2 (%make-vector (%cls-nlocal c) (%unsupplied)))
    inst))

(defun %add-default-initargs (class initargs)
  (let ((extra nil))
    (dolist (di (get-default-initargs-cache class))
      (when (%unsupplied-p (%getf-key initargs (car di)))
        (setq extra (append extra (list (car di) (funcall (cdr di)))))))
    (if extra (append initargs extra) initargs)))

(defun %shared-init (obj slot-names initargs)
  "SHARED-INITIALIZE for standard instances: each slot gets the value of its first initarg present in INITARGS;
otherwise a slot named by SLOT-NAMES (T: all of them) that is unbound gets its initform."
  (let* ((o (%slot-holder obj)) (class (%struct-type o)) (index (%current-layout o)))
    (dolist (sd (%cls-slots class))
      (let ((found nil) (val nil) (loc (cdr (assq (slotd-name sd) index))))
        (dolist (ia (slotd-initargs sd))
          (let ((probe (%getf-key initargs ia)))
            (unless (%unsupplied-p probe) (setq val probe) (setq found t) (return))))
        (cond (found (%loc-set o loc val))
              ((and (slotd-initfn sd)
                    (or (eq slot-names t) (memq (slotd-name sd) slot-names))
                    (%unsupplied-p (%loc-ref o loc)))
               (%loc-set o loc (funcall (slotd-initfn sd)))))))
    obj))

(defgeneric allocate-instance (class &rest initargs))
(defgeneric initialize-instance (instance &rest initargs))
(defgeneric reinitialize-instance (instance &rest initargs))
(defgeneric shared-initialize (instance slot-names &rest initargs))
(defgeneric make-instance (class &rest initargs))
(defgeneric change-class (instance new-class &rest initargs))
(defgeneric update-instance-for-different-class (previous current &rest initargs))
(defgeneric update-instance-for-redefined-class (instance added-slots discarded-slots property-list &rest initargs))
(defgeneric slot-missing (class instance slot-name operation &optional new-value))
(defgeneric slot-unbound (class instance slot-name))
(defgeneric print-object (object stream))

(defmethod allocate-instance ((c class) &rest initargs)
  (declare (ignore initargs))
  (when (eq (%cls-kind c) :builtin) (error "Cannot instantiate the built-in class ~S" (%cls-name c)))
  (%allocate-std-instance c))
(defmethod allocate-instance ((c symbol) &rest initargs) (apply #'allocate-instance (find-class c) initargs))

(defmethod shared-initialize ((obj t) slot-names &rest initargs) (%shared-init obj slot-names initargs))
(defmethod initialize-instance ((obj t) &rest initargs) (apply #'shared-initialize obj t initargs))
(defmethod reinitialize-instance ((obj t) &rest initargs) (apply #'shared-initialize obj nil initargs))

(defmethod make-instance ((c class) &rest initargs)
  (let ((inst (allocate-instance c)))
    (apply #'initialize-instance inst (%add-default-initargs c initargs))
    inst))
(defmethod make-instance ((c symbol) &rest initargs) (apply #'make-instance (find-class c) initargs))

(defmethod update-instance-for-redefined-class ((inst t) added discarded plist &rest initargs)
  (declare (ignore discarded plist))
  (apply #'shared-initialize inst added initargs))

(defmethod update-instance-for-different-class ((previous t) (current t) &rest initargs)
  ;; the slots CURRENT has that PREVIOUS did not have as local slots are initialized
  (let ((old (%struct-ref previous 1)))
    (apply #'shared-initialize current
           (remove-if (lambda (n) (let ((e (assq n old))) (and e (integerp (cdr e)))))
                      (mapcar #'car (remove-if-not (lambda (e) (integerp (cdr e))) (%current-layout current))))
           initargs)))

(defmethod change-class ((obj t) (new symbol) &rest initargs) (apply #'change-class obj (find-class new) initargs))
(defmethod change-class ((obj t) (new class) &rest initargs)
  (unless (%std-instance-p obj) (error "CHANGE-CLASS: ~S is not a standard instance" obj))
  (when (eq (%cls-kind new) :builtin) (error "CHANGE-CLASS: cannot change to the built-in class ~S" (%cls-name new)))
  (let* ((old-index (%current-layout obj)) (oldvec (%struct-ref obj 2))
         (previous (%allocate-std-instance (%struct-type obj)))
         (vec (%make-vector (%cls-nlocal new) (%unsupplied))))
    ;; PREVIOUS: a copy of the instance as it was
    (%struct-set previous 2 (copy-seq oldvec))
    ;; local slots of the same name keep their values
    (dolist (e (%cls-index new))
      (when (integerp (cdr e))
        (let ((o (assq (car e) old-index)))
          (when (and o (integerp (cdr o))) (%aset vec (cdr e) (aref oldvec (cdr o)))))))
    (%struct-set obj 0 new)
    (%struct-set obj 1 (%cls-index new))
    (%struct-set obj 2 vec)
    (apply #'update-instance-for-different-class previous obj initargs)
    obj))

(defmethod slot-missing ((class t) obj name operation &optional value)
  (declare (ignore value))
  (%slot-missing-error class obj name operation))
(defmethod slot-unbound ((class t) obj name) (error 'unbound-slot :name name :instance obj))

(defun copy-structure (x)
  (let ((c (%copy-struct x)))
    (when (%std-instance-p c) (%struct-set c 2 (copy-seq (%struct-ref c 2))))
    c))

(defmethod print-object ((o standard-object) stream)
  (write-string "#<" stream)
  (write-string (symbol-name (%cls-name (class-of o))) stream)
  (write-string ">" stream))

(defmacro print-unreadable-object ((object stream &key type identity) &body body)
  (declare (ignore identity))
  `(progn (write-string "#<" ,stream)
          ,@(if type `((write-string (symbol-name (%cls-name (class-of ,object))) ,stream)
                       ,@(if body `((write-char #\Space ,stream)) nil))
                nil)
          ,@body
          (write-string ">" ,stream)
          nil))

(defmethod print-object ((c %class) stream)
  (format stream "#<CLASS ~A>" (symbol-name (%cls-name c))))
(defmethod print-object ((s slot-definition) stream)
  (format stream "#<SLOT ~A>" (symbol-name (slotd-name s))))
(defmethod print-object ((g %gf) stream)
  (format stream "#<GENERIC-FUNCTION ~S>" (gf-name g)))
(defmethod print-object ((m %method) stream)
  (format stream "#<METHOD ~S~{ ~S~} ~S>" (gf-name (method-gf m)) (method-qualifiers m)
          (mapcar (lambda (s) (if (consp s) (list 'eql (cdr s)) (%cls-name s))) (method-specializers m))))
(defmethod print-object ((h hash-table) stream)
  (format stream "#<HASH-TABLE :TEST ~S :COUNT ~D>" (hash-table-test h) (hash-table-count h)))

(defun %print-hook (obj escape)
  (let ((class (class-of obj)))
    (if (and (eq (%cls-kind class) :struct)
             (null (compute-applicable-methods (%find-gf 'print-object) (list obj nil))))
        nil
        (let ((s (make-string-output-stream)) (*print-escape* escape))
          (print-object obj s)
          (get-output-stream-string s)))))

;;; ---- WITH-SLOTS / WITH-ACCESSORS -----------------------------------------------------------------

(defmacro with-slots (slots instance &body body)
  (let ((o (gensym "O")))
    `(let ((,o ,instance))
       (symbol-macrolet ,(mapcar (lambda (s)
                                   (if (consp s)
                                       (list (car s) (list 'slot-value o (list 'quote (cadr s))))
                                       (list s (list 'slot-value o (list 'quote s)))))
                                 slots)
         ,@body))))

(defmacro with-accessors (bindings instance &body body)
  (let ((o (gensym "O")))
    `(let ((,o ,instance))
       (symbol-macrolet ,(mapcar (lambda (b) (list (car b) (list (cadr b) o))) bindings)
         ,@body))))

(defun describe-object-slots (obj &optional (stream *standard-output*))
  (let ((class (class-of obj)))
    (format stream "~&~S is an instance of ~S~%" obj (%cls-name class))
    (dolist (sd (%cls-slots class))
      (format stream "  ~S: ~S~%" (slotd-name sd)
              (if (slot-boundp obj (slotd-name sd)) (slot-value obj (slotd-name sd)) '#:unbound)))))
