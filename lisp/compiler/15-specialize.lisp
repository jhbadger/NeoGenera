;;;; 15-specialize.lisp -- pass 1.5: type inference and specialisation of the AST.
;;;;
;;;; SPEC rewrites the tree that ANALYZE built, bottom-up, and returns (values new-node ty).  It
;;;;  * infers a ty for every node from constants, declarations (variables, THE, DECLAIM TYPE / FTYPE) and a few
;;;;    known functions, with interval arithmetic for integers;
;;;;  * replaces calls whose operand types are known by specialised instructions (:OP nodes) -- fixnum and float
;;;;    arithmetic and comparison, unchecked CAR/CDR, open-coded SVREF / SCHAR / %ASET;
;;;;  * inserts type checks (:CHECK nodes, parameter checks on the clam, :RCALL for declared function results) at
;;;;    safety 1 and above, and none at safety 0, where declarations are trusted;
;;;;  * applies the policy: DEBUG 3 stops tail-call merging (:NCALL), NOTINLINE stops open-coding.
;;;; The policy lives in *POLICY*, rebound by :POLICY nodes (OPTIMIZE declarations) and by each lambda.
;;;;
;;;; New AST nodes:  (:op (opcode operand...) arg-node...)   (:check ty node)   (:ncall notail fn arg...)
;;;;                 (:rcall ty notail fn arg...)  -- a call whose result is checked unless it is a tail call
;;;; :THE and :POLICY nodes (from the analyzer) never reach the code generator.
;;;;
;;;; Memory safety never depends on a declaration being true unless safety is 0: untyped operands always get the
;;;; generic (checked) instructions, and a typed fast instruction is used only where a check was inserted or the
;;;; user asked for SAFETY 0.

(in-package :cl)

(defun sp-trusting-p () (eql (policy-quality 'safety) 0))
(defun sp-checking-p () (not (eql (policy-quality 'safety) 0)))
(defun sp-node (node &optional fx) (values (spec node fx)))

;;; ---- intervals -------------------------------------------------------------------------------------

(defun iv-add (a b)
  (list :int (and (ty-lo a) (ty-lo b) (+ (ty-lo a) (ty-lo b))) (and (ty-hi a) (ty-hi b) (+ (ty-hi a) (ty-hi b)))))
(defun iv-sub (a b)
  (list :int (and (ty-lo a) (ty-hi b) (- (ty-lo a) (ty-hi b))) (and (ty-hi a) (ty-lo b) (- (ty-hi a) (ty-lo b)))))
(defun iv-mul (a b)
  (if (and (ty-lo a) (ty-hi a) (ty-lo b) (ty-hi b))
      (let ((p (list (* (ty-lo a) (ty-lo b)) (* (ty-lo a) (ty-hi b)) (* (ty-hi a) (ty-lo b)) (* (ty-hi a) (ty-hi b)))))
        (list :int (apply #'min p) (apply #'max p)))
      (list :int nil nil)))

(defun iv-nonneg-p (a) (and (ty-lo a) (>= (ty-lo a) 0) (ty-hi a)))

(defun iv-logand (a b)
  (cond ((and (iv-nonneg-p a) (iv-nonneg-p b)) (list :int 0 (min (ty-hi a) (ty-hi b))))
        ((iv-nonneg-p a) (list :int 0 (ty-hi a)))
        ((iv-nonneg-p b) (list :int 0 (ty-hi b)))
        (t (list :int nil nil))))

(defun iv-logior (a b)               ; also LOGXOR: for fixnum operands the result is a fixnum
  (if (and (iv-nonneg-p a) (iv-nonneg-p b))
      (list :int 0 (- (ash 1 (max (integer-length (ty-hi a)) (integer-length (ty-hi b)))) 1))
      (list :int nil nil)))

(defun iv-abs-max (b) (max (abs (ty-lo b)) (abs (ty-hi b))))

(defun iv-mod (a b)                  ; A and B fixnum-typed
  (declare (ignore a))
  (let ((m (iv-abs-max b)))
    (cond ((= m 0) (list :int 0 0))
          ((> (ty-lo b) 0) (list :int 0 (- (ty-hi b) 1)))
          ((< (ty-hi b) 0) (list :int (+ (ty-lo b) 1) 0))
          (t (list :int (- 1 m) (- m 1))))))

(defun iv-rem (a b)
  (let ((m (iv-abs-max b)))
    (cond ((= m 0) (list :int 0 0))
          ((>= (ty-lo a) 0) (list :int 0 (- m 1)))
          ((<= (ty-hi a) 0) (list :int (- 1 m) 0))
          (t (list :int (- 1 m) (- m 1))))))

;;; ---- operand conversion --------------------------------------------------------------------------------

(defun sp-to-flonum (node ty)
  "NODE (of ty TY, a flonum or a fixnum type) as a node that yields a flonum."
  (cond ((eq ty :flonum) node)
        ((and (eq (car node) :const) (integerp (cadr node))) (list :const (float (cadr node))))
        (t (list :op (list :fixtofl) node))))

(defun sp-float-operands (a ta b tb)
  "If A and B can be added, compared ... as flonums (a flonum with a flonum or fixnum), their converted nodes as (A . B)."
  (cond ((and (eq ta :flonum) (eq tb :flonum)) (cons a b))
        ((and (eq ta :flonum) (ty-fixnum-p tb)) (cons a (sp-to-flonum b tb)))
        ((and (ty-fixnum-p ta) (eq tb :flonum)) (cons (sp-to-flonum a ta) b))
        (t nil)))

;;; ---- calls ---------------------------------------------------------------------------------------------

(defun ty-checkable (ty)
  "TY weakened to what the VM can test directly (CHKFIX / CHKRANGE / CHKTYPE)."
  (cond ((ty-intp ty) (if (ty-fixnum-p ty) ty (list :int nil nil)))
        (t ty)))

(defun sp-open-codable-p (name nargs)
  (and name (policy-inline-p name)
       (or (inline-spec name nargs)
           (and (memq name '(+ -)) (> nargs 2))
           (and (memq name '(1+ 1-)) (= nargs 1)))))

(defun sp-plain-call (fn nodes name)
  "A call to FN (a node) with already specialised arguments NODES; NAME is the global function's name, if known."
  (let* ((rt (and name (get name '%ftype-result)))
         (notail (> (policy-quality 'debug) 2))
         (open (sp-open-codable-p name (length nodes)))
         (plain (if (and (not open) (or notail (and name (not (policy-inline-p name)))))
                    (list* :ncall notail fn nodes)
                    (list* :call fn nodes))))
    (cond ((null rt) (values plain nil))
          ((sp-trusting-p) (values plain rt))
          (t (let ((ty (ty-checkable rt)))
               (values (list* :rcall ty notail fn nodes) ty))))))

(defun sp-mk-call (name nodes)
  "A real call to the global function NAME (not open-coded), for typed results of ordinary functions."
  (if (> (policy-quality 'debug) 2)
      (list* :ncall t (list :gfun name) nodes)
      (list* :call (list :gfun name) nodes)))

(defun sp-call (node fx)
  (let ((fn (cadr node)) (args (cddr node)))
    (if (eq (car fn) :gfun)
        (sp-global-call (cadr fn) args fx)
        (let ((f2 (sp-node fn)) (nodes (mapcar (lambda (a) (sp-node a)) args)))
          (sp-plain-call f2 nodes nil)))))

(defun sp-global-call (name args fx)
  (let* ((arith (memq name '(+ - * 1+ 1-)))
         (pairs (mapcar (lambda (a) (multiple-value-bind (n ty) (spec a (and fx arith)) (cons n ty))) args))
         (nodes (mapcar #'car pairs)) (tys (mapcar #'cdr pairs)))
    (if (not (policy-inline-p name))
        (sp-plain-call (list :gfun name) nodes name)
        (multiple-value-bind (node ty) (sp-open-code name (length nodes) nodes tys fx)
          (if node
              (values node ty)
              (sp-plain-call (list :gfun name) nodes name))))))

(defun sp-arith (name a ta b tb fx)
  (let ((op1 (and (eq (car b) :const) (eql (cadr b) 1) (if (eq name '+) :incfx (if (eq name '-) :decfx nil)))))
    (cond ((and (ty-fixnum-p ta) (ty-fixnum-p tb))
           (let* ((r (cond ((eq name '+) (iv-add ta tb)) ((eq name '-) (iv-sub ta tb)) (t (iv-mul ta tb))))
                  (fits (ty-fixnum-p r)))
             (cond ((or fits fx)
                    (values (if op1
                                (list :op (list op1) a)
                                (list :op (list (cond ((eq name '+) :addfx) ((eq name '-) :subfx) (t :mulfx))) a b))
                            (if fits r *ty-fixnum*)))
                   (t (values (list :call (list :gfun name) a b) r)))))
          (t (let ((fl (sp-float-operands a ta b tb)))
               (if fl
                   (values (list :op (list (cond ((eq name '+) :addfl) ((eq name '-) :subfl) (t :mulfl))) (car fl) (cdr fl))
                           :flonum)
                   (values (list :call (list :gfun name) a b) (sp-generic-arith-ty name ta tb))))))))

(defun sp-generic-arith-ty (name ta tb)
  (cond ((and (ty-intp ta) (ty-intp tb))
         (cond ((eq name '+) (iv-add ta tb)) ((eq name '-) (iv-sub ta tb)) (t (iv-mul ta tb))))
        ((and ta tb (ty-subtype-p ta :number) (ty-subtype-p tb :number))
         (if (and (ty-subtype-p ta :real) (ty-subtype-p tb :real) (or (eq ta :flonum) (eq tb :flonum)))
             :flonum
             :number))
        (t nil)))

(defun sp-compare (name a ta b tb)
  (let ((fxop (cdr (assq name '((< . :ltfx) (> . :gtfx) (<= . :lefx) (>= . :gefx) (= . :numeqfx)))))
        (flop (cdr (assq name '((< . :ltfl) (> . :gtfl) (<= . :lefl) (>= . :gefl) (= . :numeqfl))))))
    (if (and (ty-fixnum-p ta) (ty-fixnum-p tb))
        (values (list :op (list fxop) a b) nil)
        (let ((fl (sp-float-operands a ta b tb)))
          (if fl
              (values (list :op (list flop) (car fl) (cdr fl)) nil)
              (values (list :call (list :gfun name) a b) nil))))))

(defun sp-const-int (node) (and (eq (car node) :const) (integerp (cadr node)) (cadr node)))

(defun sp-open-code (name n nodes tys fx)
  "Specialise a call to the global function NAME when its operand types allow.  (values node ty), or (values NIL NIL)."
  (let ((a (first nodes)) (ta (first tys)) (b (second nodes)) (tb (second tys)))
    (case name
      ((+ - *)
       (cond ((= n 2) (sp-arith name a ta b tb fx))
             ((> n 2) (let ((acc a) (aty ta))
                        (do ((ns (cdr nodes) (cdr ns)) (ts (cdr tys) (cdr ts))) ((null ns))
                          (multiple-value-bind (nn nt) (sp-arith name acc aty (car ns) (car ts) fx)
                            (setq acc nn) (setq aty nt)))
                        (values acc aty)))
             (t (values nil nil))))
      ((1+ 1-)
       (if (= n 1)
           (sp-arith (if (eq name '1+) '+ '-) a ta (list :const 1) (list :int 1 1) fx)
           (values nil nil)))
      ((< > <= >= =) (if (= n 2) (sp-compare name a ta b tb) (values nil nil)))
      (/ (let ((fl (and (= n 2) (sp-float-operands a ta b tb))))
           (if fl (values (list :op (list :divfl) (car fl) (cdr fl)) :flonum) (values nil nil))))
      ((logand logior logxor)
       (cond ((/= n 2) (values nil nil))
             ((and (ty-fixnum-p ta) (ty-fixnum-p tb))
              (values (list :op (list (cond ((eq name 'logand) :logandfx) ((eq name 'logior) :logiorfx) (t :logxorfx))) a b)
                      (let ((r (if (eq name 'logand) (iv-logand ta tb) (iv-logior ta tb)))) (if (ty-fixnum-p r) r *ty-fixnum*))))
             ((eq name 'logand)
              (values (list :call (list :gfun name) a b)
                      (let ((r (iv-logand (if (ty-intp ta) ta (list :int nil nil)) (if (ty-intp tb) tb (list :int nil nil)))))
                        (if (and (ty-lo r) (ty-hi r)) r nil))))
             (t (values nil nil))))
      ((rem mod)
       (if (and (= n 2) (ty-fixnum-p ta) (ty-fixnum-p tb))
           (values (list :op (list (if (eq name 'rem) :remfx :modfx)) a b)
                   (if (eq name 'rem) (iv-rem ta tb) (iv-mod ta tb)))
           (values nil nil)))
      (ash
       (let ((k (and (= n 2) (sp-const-int b))))
         (if (and k (<= -62 k 62) (ty-fixnum-p ta))
             (let* ((r (list :int (ash (ty-lo ta) k) (ash (ty-hi ta) k))) (fits (ty-fixnum-p r)))
               (if (or fits (and fx (>= k 0)))
                   (values (list :op (list :ashfx k) a) (if fits r *ty-fixnum*))
                   (values (list :call (list :gfun 'ash) a b) r)))
             (values nil nil))))
      ((zerop plusp minusp)
       (cond ((/= n 1) (values nil nil))
             ((ty-fixnum-p ta)
              (values (list :op (list (cond ((eq name 'zerop) :numeqfx) ((eq name 'plusp) :gtfx) (t :ltfx))) a (list :const 0)) nil))
             ((eq ta :flonum)
              (values (list :op (list (cond ((eq name 'zerop) :numeqfl) ((eq name 'plusp) :gtfl) (t :ltfl))) a (list :const 0.0)) nil))
             (t (values nil nil))))
      (float
       (cond ((/= n 1) (values nil nil))
             ((eq ta :flonum) (values a :flonum))
             ((ty-fixnum-p ta) (values (sp-to-flonum a ta) :flonum))
             (t (values nil nil))))
      ((car cdr)
       (cond ((/= n 1) (values nil nil))
             ((ty-subtype-p ta :cons) (values (list :op (list (if (eq name 'car) :carx :cdrx)) a) nil))
             (t (values (list :call (list :gfun name) a) nil))))
      (cons (if (= n 2) (values (list :call (list :gfun 'cons) a b) :cons) (values nil nil)))
      (list (if (>= n 1) (values (sp-mk-call 'list nodes) :cons) (values nil nil)))
      (length (if (= n 1) (values (sp-mk-call 'length nodes) (list :int 0 most-positive-fixnum)) (values nil nil)))
      ((char-code char-int) (if (= n 1) (values (sp-mk-call name nodes) (list :int 0 1114111)) (values nil nil)))
      ((svref aref char schar)
       (cond ((/= n 2) (values nil nil))
             ((or (eq name 'char) (eq name 'schar) (and (eq name 'aref) (ty-subtype-p ta :string)))
              (values (list :op (list :schar) a b) nil))
             ((and (sp-trusting-p) (ty-subtype-p ta :svec) (ty-fixnum-p tb))
              (values (list :op (list :svrefx) a b) nil))
             (t (values (list :op (list :svref) a b) nil))))
      (%aset
       (cond ((/= n 3) (values nil nil))
             ((and (sp-trusting-p) (ty-subtype-p ta :svec) (ty-fixnum-p tb))
              (values (list :op (list :svsetx) a b (third nodes)) (third tys)))
             (t (values (list :op (list :svset) a b (third nodes)) (third tys)))))
      (t (values nil nil)))))

;;; ---- checks --------------------------------------------------------------------------------------------

(defun sp-check-node (ty node)
  (if (and (ty-intp ty) (not (or (ty-fixnum-p ty) (and (null (ty-lo ty)) (null (ty-hi ty))))))
      (list :call (list :gfun '%check-int-range) node (list :const (ty-lo ty)) (list :const (ty-hi ty)))
      (list :check ty node)))

(defun sp-coerce (node nty declared)
  "NODE, of inferred ty NTY, as a value of the declared ty DECLARED: checked unless the declaration is trusted."
  (cond ((null declared) node)
        ((ty-subtype-p nty declared) node)
        ((sp-trusting-p) node)
        (t (sp-check-node declared node))))

;;; ---- the pass ----------------------------------------------------------------------------------------------

(defun spec (node &optional fx)
  "Specialise NODE.  FX is true where the value is going to be used as a trusted fixnum (so fixnum arithmetic may wrap)."
  (case (car node)
    (:const (values node (ty-of-constant (cadr node))))
    (:lref (values node (cvar-type (cadr node))))
    (:gref (let ((ty (get (cadr node) '%type)))
             (cond ((null ty) (values node nil))
                   ((sp-trusting-p) (values node ty))
                   (t (values (sp-check-node ty node) ty)))))
    (:gfun (values node nil))
    (:lset (let* ((var (cadr node)) (declared (cvar-type var)))
             (multiple-value-bind (n ty) (spec (caddr node) (and declared (sp-trusting-p) (ty-fixnum-p declared)))
               (values (list :lset var (sp-coerce n ty declared)) (or declared ty)))))
    (:gset (let* ((declared (get (cadr node) '%type)))
             (multiple-value-bind (n ty) (spec (caddr node) (and declared (sp-trusting-p) (ty-fixnum-p declared)))
               (values (list :gset (cadr node) (sp-coerce n ty declared)) (or declared ty)))))
    (:if (multiple-value-bind (tn tt) (spec (cadr node))
           (declare (ignore tt))
           (multiple-value-bind (an at) (spec (caddr node) fx)
             (multiple-value-bind (bn bt) (spec (cadddr node) fx)
               (values (list :if tn an bn) (ty-join at bt))))))
    (:progn (let ((nodes nil) (ty nil))
              (do ((l (cdr node) (cdr l))) ((null l))
                (multiple-value-bind (n nty) (spec (car l) (and fx (null (cdr l))))
                  (setq nodes (cons n nodes)) (setq ty nty)))
              (values (cons :progn (nreverse nodes)) ty)))
    (:let (sp-let node fx))
    ((:flet :labels)
     (dolist (l (caddr node)) (sp-lambda l))
     (multiple-value-bind (bn bt) (spec (cadddr node) fx)
       (values (list (car node) (cadr node) (caddr node) bn) bt)))
    (:lambda (sp-lambda (cadr node)) (values node nil))
    (:call (sp-call node fx))
    (:block (values (list :block (cadr node) (sp-node (caddr node))) nil))
    (:return-from (values (list :return-from (cadr node) (sp-node (caddr node))) nil))
    (:tagbody (values (cons :tagbody (cons (cadr node)
                                           (mapcar (lambda (it) (if (ctag-p it) it (sp-node it))) (cddr node))))
                      nil))
    (:go (values node nil))
    (:the (let ((ty (cadr node)))
            (multiple-value-bind (n nty) (spec (caddr node) (and (sp-trusting-p) (ty-fixnum-p ty)))
              (cond ((and nty (ty-subtype-p nty ty)) (values n nty))
                    ((sp-trusting-p) (values n ty))
                    (t (values (sp-check-node ty n) ty))))))
    (:policy (let ((*policy* (apply-policy (cadr node) *policy*)))
               (spec (caddr node) fx)))
    (t (error "spec: unknown node ~S" (car node)))))

(defun sp-let (node fx)
  (let ((bindings (cadr node)) (inits (caddr node)) (new nil))
    (do ((bs bindings (cdr bs)) (is inits (cdr is))) ((null bs))
      (let* ((b (car bs)) (declared (and (cvar-p b) (cvar-type b))))
        (multiple-value-bind (n ty) (spec (car is) (and declared (sp-trusting-p) (ty-fixnum-p declared)))
          (cond ((not (cvar-p b)))
                (declared (setq n (sp-coerce n ty declared)))
                ((not (cvar-mutated b)) (setf (cvar-type b) ty)))
          (setq new (cons n new)))))
    (multiple-value-bind (bn bt) (spec (cadddr node) fx)
      (values (list :let bindings (nreverse new) bn) bt))))

(defun sp-lambda (lam)
  (let ((*policy* (apply-policy (clam-policy lam) *policy*)) (checks nil))
    (dolist (p (clam-params lam))
      (when (cvar-type p)
        (when (sp-checking-p)
          (setf (cvar-type p) (ty-checkable (cvar-type p)))
          (setq checks (cons (cons p (cvar-type p)) checks)))))
    (setf (clam-checks lam) (nreverse checks))
    (setf (clam-body lam) (sp-node (clam-body lam)))
    lam))
