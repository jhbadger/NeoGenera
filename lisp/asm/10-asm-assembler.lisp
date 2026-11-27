;;;; 10-asm-assembler.lisp -- the bytecode assembler: DEFASM, ASM-ASSEMBLE and the verifier.
;;;;
;;;; A routine is written as a list of items: labels (symbols) and instructions ((mnemonic operand...)), with a few
;;;; directives ((:unsafe), (:var name), (:free name...)).  The assembler
;;;;   1. parses the items into instruction records and checks every operand,
;;;;   2. verifies the routine by abstract interpretation (the way a class-file verifier does): every path must reach
;;;;      an instruction with the same stack depth, never underflow, never read a slot that does not exist, never fall
;;;;      off the end, leave dynamic bindings balanced, and use the unchecked instructions (ADDFX CARX UNBOX ...)
;;;;      only on operands it can prove have the right type -- that is the memory-safety guarantee the VM relies on,
;;;;   3. resolves labels and names and emits the instruction words and the constant vector.
;;;; A routine declared (:unsafe) skips only the type proofs; the stack and control-flow checks always run.
;;;; See docs/manual/bytecode-manual.md.

(in-package :cl)

(define-condition asm-error (error)
  ((message :initarg :message :reader asm-error-message)
   (index :initarg :index :initform nil :reader asm-error-index)
   (item :initarg :item :initform nil :reader asm-error-item)
   (routine :initarg :routine :initform nil :reader asm-error-routine))
  (:report (lambda (c s)
             (format s "Assembly error~@[ in ~A~]: ~A~@[ (at ~S)~]"
                     (asm-error-routine c) (asm-error-message c) (asm-error-item c))))
  (:documentation "Signalled when a routine cannot be assembled.  ASM-ERROR-INDEX is the position of the offending item in the routine's item list."))

(defvar *asm-routine* nil "The name of the routine being assembled, for error messages.")
(defvar *asm-warnings* nil "Warnings collected while assembling: (item-index . message), most recent first.")

(defstruct (asmi (:constructor make-asmi (index form kind args)) (:copier nil))
  index form kind args
  (addr 0) (seen nil) (types nil) (spd 0)   ; the verifier's state on entry: types top first, dynamic binding count
  (target nil)                              ; for jumps, the record index of the label
  (slot nil))                               ; for :var, the slot it names

(defun %asm-fail (ins fmt &rest args)
  (error 'asm-error :message (apply #'format nil fmt args) :index (and ins (asmi-index ins))
         :item (and ins (asmi-form ins)) :routine *asm-routine*))

(defun %asm-warn (ins fmt &rest args)
  (setq *asm-warnings* (cons (cons (and ins (asmi-index ins)) (apply #'format nil fmt args)) *asm-warnings*)))

;;; ---- the lambda list ----------------------------------------------------------------------------------------------------

(defun %asm-parse-lambda-list (ll)
  "(values nreq nopt restp parameter-names): only required, &OPTIONAL and &REST plain variables are allowed."
  (let ((mode :req) (nreq 0) (nopt 0) (rest nil) (names nil) (want-rest nil))
    (unless (listp ll) (%asm-fail nil "The lambda list ~S is not a list" ll))
    (dolist (x ll)
      (cond ((eq x '&optional)
             (unless (eq mode :req) (%asm-fail nil "&OPTIONAL must come first in ~S" ll))
             (setq mode :opt))
            ((eq x '&rest)
             (when (eq mode :rest) (%asm-fail nil "Two &REST markers in ~S" ll))
             (setq mode :rest want-rest t))
            ((and (symbolp x) (> (length (symbol-name x)) 0) (char= (char (symbol-name x) 0) #\&))
             (%asm-fail nil "~A is not supported in an assembly lambda list: only &OPTIONAL and &REST (keywords and defaults are compiler rewrites, not VM features)" x))
            ((and (symbolp x) x (not (keywordp x)))
             (when (member x names) (%asm-fail nil "The parameter ~S appears twice" x))
             (setq names (cons x names))
             (case mode
               (:req (setq nreq (+ nreq 1)))
               (:opt (setq nopt (+ nopt 1)))
               (t (when rest (%asm-fail nil "Only one variable may follow &REST in ~S" ll))
                  (setq rest t))))
            (t (%asm-fail nil "~S is not a parameter name (defaults and destructuring are not supported)" x))))
    (when (and want-rest (not rest)) (%asm-fail nil "&REST needs a variable in ~S" ll))
    (values nreq nopt rest (nreverse names))))

;;; ---- parsing the items -----------------------------------------------------------------------------------------------------

(defparameter *asm-type-codes*
  '((fixnum . 0) (integer . 1) (double-float . 2) (flonum . 2) (float . 2) (real . 3) (number . 4) (cons . 5) (list . 6)
    (symbol . 7) (null . 8) (string . 9) (simple-vector . 10) (character . 11))
  "The type names CHKTYPE accepts and their codes (the TC_ enum of src/neogenera.h).")

(defun %asm-operand-count (ins n)
  (unless (= (length (asmi-args ins)) n)
    (%asm-fail ins "~(~A~) takes ~D operand~:P, not ~D" (asmi-kind ins) n (length (asmi-args ins)))))

(defun %asm-nonneg (ins x what)
  (unless (and (integerp x) (typep x 'fixnum) (>= x 0) (< x 1000000)) (%asm-fail ins "~A must be a non-negative integer, not ~S" what x)))

(defun %asm-check-operands (ins)
  "Check the operands of the instruction INS and rewrite the ones that need it (type names, nested lambdas)."
  (let ((k (asmi-kind ins)) (a (asmi-args ins)))
    (case k
      ((:const) (%asm-operand-count ins 1))
      ((:lref :lset :fref)
       (%asm-operand-count ins 1)
       (let ((x (car a)))
         (cond ((and (integerp x) (typep x 'fixnum)) (when (< x 0) (%asm-fail ins "The slot ~D is negative" x)))
               ((and (symbolp x) x (not (keywordp x))))
               (t (%asm-fail ins "The operand of ~(~A~) must be a slot number or a name, not ~S" k x)))))
      ((:gref :gset :gfun :bind)
       (%asm-operand-count ins 1)
       (unless (symbolp (car a)) (%asm-fail ins "The operand of ~(~A~) must be a symbol, not ~S" k (car a))))
      ((:jmp :jnil :jnnil)
       (%asm-operand-count ins 1)
       (unless (and (symbolp (car a)) (car a) (not (keywordp (car a)))) (%asm-fail ins "The operand of ~(~A~) must be a label, not ~S" k (car a))))
      ((:call :tailcall :unbind :slide) (%asm-operand-count ins 1) (%asm-nonneg ins (car a) "The count"))
      ((:ashfx) (%asm-operand-count ins 1)
       (unless (and (integerp (car a)) (typep (car a) 'fixnum) (< (abs (car a)) 1000000)) (%asm-fail ins "The shift must be an integer, not ~S" (car a))))
      ((:closure)
       (%asm-operand-count ins 2)
       (%asm-nonneg ins (cadr a) "The number of captured values")
       (let ((f (car a)))
         (unless (and (consp f) (eq (car f) :lambda) (consp (cdr f)))
           (%asm-fail ins "The first operand of closure must be (:lambda (parameters) items...), not ~S" f))))
      ((:chktype)
       (%asm-operand-count ins 1)
       (let ((e (and (symbolp (car a)) (assoc (string-upcase (symbol-name (car a))) *asm-type-codes* :key #'symbol-name :test #'string=))))
         (unless e (%asm-fail ins "~S is not a type chktype knows (fixnum integer double-float real number cons list symbol null string simple-vector character)" (car a)))
         (setf (asmi-args ins) (list (cdr e)))))
      ((:chkrange)
       (%asm-operand-count ins 2)
       (let ((lo (car a)) (hi (cadr a)))
         (unless (and (integerp lo) (integerp hi) (typep lo 'fixnum) (typep hi 'fixnum)) (%asm-fail ins "chkrange needs two fixnum bounds"))
         (when (> lo hi) (%asm-fail ins "chkrange: the lower bound ~D is above the upper bound ~D" lo hi))))
      (t (%asm-operand-count ins 0)))))

(defun %asm-parse-items (items)
  "(values records free-names unsafe): one record per item, labels and directives included."
  (let ((recs nil) (index 0) (labels nil) (free-names nil) (unsafe nil))
    (dolist (item items)
      (cond
        ((and (stringp item) (= index 0)) (setq recs (cons (make-asmi index item :doc nil) recs)))
        ((stringp item) (%asm-fail (make-asmi index item :doc nil) "A string is only allowed as the first item (the documentation)"))
        ((and item (symbolp item))
         (let ((ins (make-asmi index item :label nil)))
           (when (keywordp item) (%asm-fail ins "~S is a keyword, not a label" item))
           (when (member item labels) (%asm-fail ins "The label ~S is defined twice" item))
           (when (gethash (symbol-name item) *asm-mnemonics*)
             (%asm-warn ins "The label ~S has the name of an instruction" item))
           (setq labels (cons item labels))
           (setq recs (cons ins recs))))
        ((and (consp item) (symbolp (car item)) (car item))
         (let* ((head (car item)) (ins (make-asmi index item nil (cdr item))))
           (unless (listp (cdr item)) (%asm-fail ins "The instruction ~S is not a proper list" item))
           (cond
             ((eq head :unsafe)
              (setq unsafe (if (cdr item) (and (cadr item) t) t))
              (setf (asmi-kind ins) :directive))
             ((eq head :free)
              (dolist (n (cdr item))
                (unless (and (symbolp n) n (not (keywordp n))) (%asm-fail ins "~S is not a name for a captured variable" n))
                (when (member n free-names) (%asm-fail ins "The captured variable ~S is named twice" n)))
              (setq free-names (append free-names (cdr item)))
              (setf (asmi-kind ins) :directive))
             ((eq head :var)
              (unless (and (= (length (cdr item)) 1) (symbolp (cadr item)) (cadr item) (not (keywordp (cadr item))))
                (%asm-fail ins "(:var name) takes one variable name"))
              (setf (asmi-kind ins) :var))
             ((keywordp head) (%asm-fail ins "Unknown directive ~S (the directives are :unsafe, :free and :var)" head))
             (t (let ((op (gethash (string-upcase (symbol-name head)) *asm-mnemonics*)))
                  (unless op
                    (%asm-fail ins "~S is not an instruction.  Labels are plain symbols, instructions are lists like (lref 0); see (asm-mnemonics)" head))
                  (setf (asmi-kind ins) op)
                  (%asm-check-operands ins))))
           (setq recs (cons ins recs))))
        (t (%asm-fail (make-asmi index item :bad nil) "~S is neither a label (a symbol) nor an instruction (a list starting with a mnemonic)" item)))
      (setq index (+ index 1)))
    (values (nreverse recs) free-names unsafe)))

;;; ---- the verifier --------------------------------------------------------------------------------------------------------------

(defun %asm-literal-type (x)
  (cond ((typep x 'fixnum) :fixnum) ((floatp x) :flonum) ((consp x) :cons) (t :any)))

(defun %asm-join (a b) (mapcar (lambda (x y) (if (eq x y) x :any)) a b))

(defun %asm-type-name (ty) (case ty (:fixnum "a fixnum") (:flonum "a flonum") (:cons "a cons") (t "of unknown type")))

(defun %asm-need (ins types n)
  (when (< (length types) n)
    (%asm-fail ins "Stack underflow: ~(~A~) needs ~D value~:P on the stack but there ~:[are only ~D~;is nothing there~*~]"
               (asmi-kind ins) n (null types) (length types))))

(defun %asm-require (ins ty got unsafe fix)
  (unless (or unsafe (eq ty got))
    (%asm-fail ins "~(~A~) needs ~A, but this operand ~A.  Insert ~A first, or declare the routine (:unsafe)"
               (asmi-kind ins) (%asm-type-name ty) (if (eq got :any) "is of unknown type" (format nil "is ~A" (%asm-type-name got))) fix)))

(defparameter *asm-fixnum-binary* '(:addfx :subfx :mulfx :logandfx :logiorfx :logxorfx :remfx :modfx))
(defparameter *asm-fixnum-compare* '(:ltfx :gtfx :lefx :gefx :numeqfx))
(defparameter *asm-fixnum-unary* '(:incfx :decfx :ashfx))
(defparameter *asm-flonum-binary* '(:addfl :subfl :mulfl :divfl))
(defparameter *asm-flonum-compare* '(:ltfl :gtfl :lefl :gefl :numeqfl))
(defparameter *asm-any-binary* '(:add :sub :mul :lt :gt :le :ge :numeq :eq :cons))

(defun %asm-slot-of (ins operand table kind)
  "The slot number named by OPERAND (a number, or a name looked up in TABLE)."
  (cond ((integerp operand) operand)
        (t (let ((e (assoc operand table)))
             (unless e (%asm-fail ins "~S is not ~A: no parameter or (:var ...) of that name is in scope here" operand
                                  (if (eq kind :free) "a captured variable" "a variable")))
             (cdr e)))))

(defun %asm-free-table (names)
  (let ((ft nil) (i 0)) (dolist (f names) (setq ft (cons (cons f i) ft)) (setq i (+ i 1))) ft))

(defun %asm-step (ins types spd ctx)
  "The effect of INS on the state (TYPES, top first; SPD, dynamic bindings).  Returns (values types spd flow), FLOW
being :NEXT, :BOTH (a conditional jump), :JUMP or :END."
  (let* ((k (asmi-kind ins)) (a (asmi-args ins)) (depth (length types)) (unsafe (getf ctx :unsafe))
         (nfree (getf ctx :nfree)))
    (flet ((nxt (ts &optional (s spd)) (values ts s :next)))
      (case k
        ((:label :doc :directive) (nxt types))
        (:var
         (when (< depth 1) (%asm-fail ins "(:var ~S): there is nothing on the stack to name" (car a)))
         (let* ((slot (- depth 1)) (e (assoc (car a) (funcall (getf ctx :table)))))
           (cond ((null e) (funcall (getf ctx :bind) (car a) slot))
                 ((/= (cdr e) slot)
                  (%asm-fail ins "The name ~S already stands for slot ~D; here the top of the stack is slot ~D" (car a) (cdr e) slot)))
           (setf (asmi-slot ins) slot)
           (nxt types)))
        (:const (nxt (cons (%asm-literal-type (car a)) types)))
        (:lref
         (let ((i (%asm-slot-of ins (car a) (funcall (getf ctx :table)) :var)))
           (unless (< -1 i depth) (%asm-fail ins "lref ~D: only slots 0 to ~D exist here (the stack has ~D value~:P)" i (- depth 1) depth))
           (nxt (cons (nth (- depth 1 i) types) types))))
        (:lset
         (%asm-need ins types 1)
         (let ((i (%asm-slot-of ins (car a) (funcall (getf ctx :table)) :var)))
           (unless (< -1 i (- depth 1)) (%asm-fail ins "lset ~D: only slots 0 to ~D can be assigned here (the stack has ~D value~:P)" i (- depth 2) depth))
           (let ((r (copy-list (cdr types))))
             (setf (nth (- (length r) 1 i) r) (car types))
             (nxt r))))
        (:fref
         (let ((i (%asm-slot-of ins (car a) (getf ctx :free-table) :free)))
           (unless (< -1 i nfree) (%asm-fail ins "fref ~D: this function has ~D captured variable~:P~:[ (only a (:lambda ...) given to closure can have any)~;~]" i nfree (> nfree 0)))
           (nxt (cons :any types))))
        ((:gref :gfun) (nxt (cons :any types)))
        (:gset (%asm-need ins types 1) (nxt (cdr types)))
        (:pop (%asm-need ins types 1) (nxt (cdr types)))
        (:dup (%asm-need ins types 1) (nxt (cons (car types) types)))
        (:slide
         (let ((n (car a)))
           (%asm-need ins types (+ n 1))
           (nxt (cons (car types) (nthcdr (+ n 1) types)))))
        (:jmp (values types spd :jump))
        ((:jnil :jnnil) (%asm-need ins types 1) (values (cdr types) spd :both))
        (:call (%asm-need ins types (+ (car a) 1)) (nxt (cons :any (nthcdr (+ (car a) 1) types))))
        (:tailcall
         (%asm-need ins types (+ (car a) 1))
         (unless (= spd 0) (%asm-fail ins "tailcall with ~D dynamic binding~:P still in force: unbind first (the frame is about to be reused)" spd))
         (values nil spd :end))
        ((:ret :retv)
         (%asm-need ins types 1)
         (unless (= spd 0) (%asm-fail ins "~(~A~) with ~D dynamic binding~:P still in force: unbind first" k spd))
         (when (> depth (+ (getf ctx :nparams) 1))
           (%asm-warn ins "~(~A~) leaves ~D extra value~:P on the stack beyond the parameters" k (- depth (+ (getf ctx :nparams) 1))))
         (values nil spd :end))
        (:closure
         (%asm-need ins types (cadr a))
         (nxt (cons :any (nthcdr (cadr a) types))))
        (:box (%asm-need ins types 1) (nxt (cons :cons (cdr types))))
        (:unbox
         (%asm-need ins types 1)
         (%asm-require ins :cons (car types) unsafe "(chktype cons)")
         (nxt (cons :any (cdr types))))
        (:boxset
         (%asm-need ins types 2)
         (%asm-require ins :cons (cadr types) unsafe "a box made by (box)")
         (nxt (cons (car types) (cddr types))))
        (:bind (%asm-need ins types 1) (nxt (cdr types) (+ spd 1)))
        (:unbind
         (when (> (car a) spd) (%asm-fail ins "unbind ~D, but only ~D dynamic binding~:P in force in this function" (car a) spd))
         (nxt types (- spd (car a))))
        ((:car :cdr) (%asm-need ins types 1) (nxt (cons :any (cdr types))))
        ((:carx :cdrx)
         (%asm-need ins types 1)
         (%asm-require ins :cons (car types) unsafe "(chktype cons)")
         (nxt (cons :any (cdr types))))
        (:not (%asm-need ins types 1) (nxt (cons :any (cdr types))))
        (t
         (cond
           ((member k *asm-any-binary*)
            (%asm-need ins types 2)
            (nxt (cons (if (eq k :cons) :cons :any) (cddr types))))
           ((member k *asm-fixnum-binary*)
            (%asm-need ins types 2)
            (%asm-require ins :fixnum (car types) unsafe "(chkfix)") (%asm-require ins :fixnum (cadr types) unsafe "(chkfix)")
            (nxt (cons :fixnum (cddr types))))
           ((member k *asm-fixnum-compare*)
            (%asm-need ins types 2)
            (%asm-require ins :fixnum (car types) unsafe "(chkfix)") (%asm-require ins :fixnum (cadr types) unsafe "(chkfix)")
            (nxt (cons :any (cddr types))))
           ((member k *asm-fixnum-unary*)
            (%asm-need ins types 1)
            (%asm-require ins :fixnum (car types) unsafe "(chkfix)")
            (nxt (cons :fixnum (cdr types))))
           ((member k *asm-flonum-binary*)
            (%asm-need ins types 2)
            (%asm-require ins :flonum (car types) unsafe "(chktype double-float)") (%asm-require ins :flonum (cadr types) unsafe "(chktype double-float)")
            (nxt (cons :flonum (cddr types))))
           ((member k *asm-flonum-compare*)
            (%asm-need ins types 2)
            (%asm-require ins :flonum (car types) unsafe "(chktype double-float)") (%asm-require ins :flonum (cadr types) unsafe "(chktype double-float)")
            (nxt (cons :any (cddr types))))
           ((eq k :fixtofl)
            (%asm-need ins types 1)
            (%asm-require ins :fixnum (car types) unsafe "(chkfix)")
            (nxt (cons :flonum (cdr types))))
           ((member k '(:svref :schar)) (%asm-need ins types 2) (nxt (cons :any (cddr types))))
           ((eq k :svrefx)
            (%asm-need ins types 2)
            (unless unsafe (%asm-fail ins "svrefx trusts the vector, the index and the range, and cannot be proved: declare the routine (:unsafe) or use svref"))
            (nxt (cons :any (cddr types))))
           ((eq k :svset) (%asm-need ins types 3) (nxt (cons (car types) (cdddr types))))
           ((eq k :svsetx)
            (%asm-need ins types 3)
            (unless unsafe (%asm-fail ins "svsetx trusts the vector, the index and the range, and cannot be proved: declare the routine (:unsafe) or use svset"))
            (nxt (cons (car types) (cdddr types))))
           ((eq k :chkfix) (%asm-need ins types 1) (nxt (cons :fixnum (cdr types))))
           ((eq k :chkrange) (%asm-need ins types 1) (nxt (cons :fixnum (cdr types))))
           ((eq k :chktype)
            (%asm-need ins types 1)
            (nxt (cons (case (car a) (0 :fixnum) (2 :flonum) (5 :cons) (t (car types))) (cdr types))))
           (t (%asm-fail ins "Internal error: no verifier rule for ~S" k))))))))

(defun %asm-verify (recs nparams nfree unsafe param-names free-names)
  "Run the verifier over RECS.  Fills in each record's state; returns the table of (name . slot) for the routine's variables."
  (let* ((n (length recs)) (v (coerce recs 'vector)) (label-table nil) (table nil) (work nil) (ctx nil))
    (let ((i 0)) (dolist (p param-names) (setq table (cons (cons p i) table)) (setq i (+ i 1))))
    (setq ctx (list :unsafe unsafe :nfree nfree :nparams nparams
                    :table (lambda () table)
                    :free-table (%asm-free-table free-names)
                    :bind (lambda (name slot) (setq table (cons (cons name slot) table)))))
    (dotimes (i n)
      (let ((r (aref v i)))
        (when (eq (asmi-kind r) :label) (setq label-table (cons (cons (asmi-form r) i) label-table)))))
    ;; resolve jump targets
    (dotimes (i n)
      (let ((r (aref v i)))
        (when (member (asmi-kind r) '(:jmp :jnil :jnnil))
          (let ((e (assoc (car (asmi-args r)) label-table)))
            (unless e (%asm-fail r "The label ~S is not defined in this routine" (car (asmi-args r))))
            (setf (asmi-target r) (cdr e))))))
    (when (every (lambda (r) (member (asmi-kind r) '(:label :doc :directive))) recs) (%asm-fail nil "The routine has no instructions"))
    (labels ((propagate (to types spd from)
               (when (>= to n) (%asm-fail from "Execution can run off the end of the routine here: finish with ret, retv, tailcall or jmp"))
               (let ((r (aref v to)))
                 (cond ((not (asmi-seen r))
                        (setf (asmi-seen r) t (asmi-types r) types (asmi-spd r) spd)
                        (setq work (cons to work)))
                       (t
                        (unless (= (length (asmi-types r)) (length types))
                          (%asm-fail from "The stack depth does not agree where paths meet~@[ at label ~S~]: ~D value~:P on one path, ~D on another"
                                     (and (eq (asmi-kind r) :label) (asmi-form r)) (length (asmi-types r)) (length types)))
                        (unless (= (asmi-spd r) spd)
                          (%asm-fail from "The number of dynamic bindings does not agree where paths meet~@[ at label ~S~]: ~D on one path, ~D on another"
                                     (and (eq (asmi-kind r) :label) (asmi-form r)) (asmi-spd r) spd))
                        (let ((j (%asm-join (asmi-types r) types)))
                          (unless (equal j (asmi-types r))
                            (setf (asmi-types r) j)
                            (unless (member to work) (setq work (cons to work))))))))))
      (let ((init nil)) (dotimes (i nparams) (setq init (cons :any init))) (propagate 0 init 0 nil))
      (loop
        (when (null work) (return))
        (let* ((i (car (last work))) (r (aref v i)))
          (setq work (butlast work))
          (multiple-value-bind (types spd flow) (%asm-step r (asmi-types r) (asmi-spd r) ctx)
            (ecase flow
              (:next (propagate (+ i 1) types spd r))
              (:both (propagate (+ i 1) types spd r) (propagate (asmi-target r) types spd r))
              (:jump (propagate (asmi-target r) types spd r))
              (:end nil))))))
    (dotimes (i n)
      (let ((r (aref v i)))
        (when (and (not (asmi-seen r)) (not (member (asmi-kind r) '(:label :doc :directive))))
          (%asm-warn r "Unreachable instruction ~S" (asmi-form r)))))
    table))

;;; ---- emitting the code ----------------------------------------------------------------------------------------------------------

(defun %asm-emit (name recs table free-names nreq nopt rest arglist)
  (let ((pool nil) (npool 0) (addr 0) (words nil) (free-table (%asm-free-table free-names)) (by-index (coerce recs 'vector)))
    (labels ((const-index (x)
               (let ((pos (and (or (symbolp x) (numberp x)) (position x (reverse pool) :test #'eql))))
                 (or pos (progn (setq pool (cons x pool)) (setq npool (+ npool 1)) (- npool 1))))))
      (dolist (r recs)
        (setf (asmi-addr r) addr)
        (let ((k (asmi-kind r)))
          (unless (member k '(:label :doc :directive :var)) (setq addr (+ addr 1 (op-nargs k))))))
      (dolist (r recs)
        (let ((k (asmi-kind r)) (a (asmi-args r)))
          (unless (member k '(:label :doc :directive :var))
            (let ((ops
                    (case k
                      (:const (list (const-index (car a))))
                      ((:lref :lset) (list (%asm-slot-of r (car a) table :var)))
                      (:fref (list (%asm-slot-of r (car a) free-table :free)))
                      ((:gref :gset :gfun :bind) (list (const-index (car a))))
                      ((:jmp :jnil :jnnil) (list (asmi-addr (aref by-index (asmi-target r)))))
                      ((:call :tailcall :unbind :slide :ashfx :chktype) (list (car a)))
                      (:closure (list (const-index (car a)) (cadr a)))
                      (:chkrange (list (const-index (cons (car a) (cadr a)))))
                      (t nil))))
              (setq words (cons (op-code k) words))
              (dolist (w ops) (setq words (cons w words)))))))
      (%make-code (if (and (symbolp name) name (null (symbol-package name))) nil name)
                  nreq nopt rest (nreverse words) (%list-to-vector (reverse pool)) arglist))))

;;; ---- the assembler -----------------------------------------------------------------------------------------------------------------

(defun %asm-code (name lambda-list items nfree-given)
  "Assemble ITEMS into a code object; NFREE-GIVEN is the number of captured variables (a nested lambda) or NIL."
  (multiple-value-bind (nreq nopt rest params) (%asm-parse-lambda-list lambda-list)
    (multiple-value-bind (recs free-names unsafe) (%asm-parse-items items)
      (let ((nfree (or nfree-given 0)))
        (when (and (null nfree-given) free-names) (%asm-fail nil "(:free ...) is only meaningful in a (:lambda ...) given to closure"))
        (when (and nfree-given (> (length free-names) 0) (/= (length free-names) nfree))
          (%asm-fail nil "(:free ...) names ~D variable~:P but the closure instruction captures ~D" (length free-names) nfree))
        ;; nested lambdas are assembled first so that their errors point at the closure instruction
        (dolist (r recs)
          (when (eq (asmi-kind r) :closure)
            (let* ((form (car (asmi-args r))) (n (cadr (asmi-args r)))
                   (code (handler-case (%asm-code nil (cadr form) (cddr form) n)
                           (asm-error (e)
                             (error 'asm-error :message (format nil "in the nested lambda: ~A" (asm-error-message e))
                                               :index (asmi-index r) :item (asmi-form r) :routine *asm-routine*)))))
              (setf (asmi-args r) (list code n)))))
        (let ((table (%asm-verify recs (+ nreq nopt (if rest 1 0)) nfree unsafe params free-names)))
          (%asm-emit name recs table free-names nreq nopt rest lambda-list))))))

(defun asm-assemble (name lambda-list items &key positions)
  "Assemble the routine NAME with LAMBDA-LIST (required, &OPTIONAL and &REST parameters) from the assembly ITEMS: labels
(symbols), instructions (lists such as (lref 0)) and directives.  A string as the first item is the documentation.
Returns the compiled function and a list of warnings, each (item-index . message).  Does not install it: DEFASM does.
Signals ASM-ERROR, whose index says which item was at fault, when the routine is not sound.  POSITIONS is ignored here;
the editor uses it to turn the index into a place in the buffer."
  (declare (ignore positions))
  (let ((*asm-routine* name) (*asm-warnings* nil))
    (let* ((doc (and (stringp (car items)) (car items)))
           (code (%asm-code name lambda-list items nil))
           (fn (%code-closure code nil)))
      (when (and doc (symbolp name) name) (%put name '%doc-function doc))
      (values fn (reverse *asm-warnings*)))))

(defun %asm-define (name lambda-list items)
  (let ((fn (asm-assemble name lambda-list items)))
    (%defun name fn)
    name))

(defmacro defasm (name lambda-list &body items)
  "Define the function NAME in raw bytecode.  LAMBDA-LIST may use &OPTIONAL and &REST.  ITEMS are labels (symbols),
instructions such as (lref 0) (const 1) (add) (jnil done) (ret), and the directives (:unsafe), (:var name) and (:free name...).
The routine is assembled when the form is evaluated, and the assembler verifies it first.  See `:doc bytecode-manual`."
  (list '%asm-define (list 'quote name) (list 'quote lambda-list) (list 'quote items)))
