;;;; 35-statements.lisp -- the FORTRAN Tool Kit: declarations, statements, input/output and program units.
;;;;
;;;; A unit is compiled in four steps: (1) the declaration statements are applied in order (types, IMPLICIT, arrays,
;;;; COMMON, EQUIVALENCE, PARAMETER, statement functions); (2) storage is laid out; (3) the executable statements are
;;;; arranged into a tree (block IF, DO loops with labeled or ENDDO ends -- shared terminal labels included) and
;;;; compiled to a TAGBODY, where every statement label is a tag; (4) the prologue (static frame, COMMON blocks, dummy
;;;; arguments, adjustable array bounds, DATA initial values) is wrapped around the body.

(in-package :cl)

;;; ---- declarations ---------------------------------------------------------------------------------------------

(defparameter *fc-declaration-kinds*
  '(:type :dimension :common :equivalence :external :intrinsic :save :parameter :implicit :implicit-none :data :format
    :program :subroutine :function :blockdata :end))

(defun %fc-length-value (len)
  "A CHARACTER length from the parser: NIL, :STAR, (:int n) or an expression."
  (cond ((null len) 1) ((eq len :star) :star)
        (t (let ((n (%fc-const-int len "A CHARACTER length")))
             (when (< n 0) (%fc-error "A CHARACTER length cannot be negative"))
             n))))

(defun %fc-declare-var (name kind len &optional dims)
  (let ((v (or (%fc-find name) (%fc-create name :kind kind))))
    (when (and (fv-declared v) (not (eq (fv-class v) :result)) (not (eq (fv-class v) :dummy)) (not (eq (fv-class v) :procdummy))
               (not (eq (fv-class v) :external)))
      (%fc-error "The type of ~A is declared twice" name))
    (setf (fv-kind v) kind (fv-len v) (if (eq kind :character) len nil) (fv-declared v) t)
    (when dims
      (when (fv-dims v) (%fc-error "~A is dimensioned twice" name))
      (setf (fv-dims v) dims))
    v))

(defun %fc-declare (node)
  (let ((u *fc-unit*) (args (fn-args node)))
    (ecase (fn-kind node)
      ((:program :subroutine :function :blockdata :end :save) nil)
      (:implicit-none (setf (cu-implicit-none u) t))
      (:implicit
       (dolist (spec (car args))
         (let ((kind (car spec)) (len (if (eq (car spec) :character) (%fc-length-value (cadr spec)) nil)))
           (dolist (r (caddr spec))
             (do ((c (char-code (car r)) (+ c 1))) ((> c (char-code (cdr r))))
               (setf (svref (cu-implicit u) (- (char-code (char-upcase (code-char c))) 65)) (cons kind len)))))))
      (:type
       (let* ((ty (car args)) (kind (cadr ty)) (tlen (caddr ty)))
         (dolist (item (cadr args))
           (destructuring-bind (name dims ilen vals) item
             (let* ((len (if (eq kind :character) (%fc-length-value (or ilen tlen)) nil))
                    (v (%fc-declare-var name kind len dims)))
               (declare (ignorable v))
               (when vals (push (list (list (list :name name)) vals) (cu-datas u))))))))
      (:dimension
       (dolist (item (car args))
         (let ((v (or (%fc-find (car item)) (%fc-create (car item)))))
           (when (fv-dims v) (%fc-error "~A is dimensioned twice" (car item)))
           (setf (fv-dims v) (cadr item)))))
      (:common
       (dolist (blk (car args))
         (let ((vars nil))
           (dolist (item (cdr blk))
             (let ((v (or (%fc-find (car item)) (%fc-create (car item)))))
               (when (cadr item)
                 (when (fv-dims v) (%fc-error "~A is dimensioned twice" (car item)))
                 (setf (fv-dims v) (cadr item)))
               (push v vars)))
           (push (cons (car blk) (nreverse vars)) (cu-common-decls u)))))
      (:equivalence (dolist (g (car args)) (setf (cu-equivs u) (append (cu-equivs u) (list g)))))
      (:external
       (dolist (name (car args))
         (let ((v (%fc-find name)))
           (cond ((null v) (%fc-create name :class :external :kind (car (%fc-implicit name))))
                 ((member (fv-class v) '(:dummy :procdummy)) (setf (fv-class v) :procdummy))
                 (t (setf (fv-class v) :external))))))
      (:intrinsic
       (dolist (name (car args))
         (unless (%fc-intrinsic-name-p name) (%fc-error "~A is not an intrinsic function" name))
         (push name (cu-intrinsics u))))
      (:parameter
       (dolist (item (car args))
         (let* ((name (car item)) (v (or (%fc-find name) (%fc-create name))))
           (when (or (fv-dims v) (%fc-dummy-p v)) (%fc-error "~A cannot be a named constant" name))
           (multiple-value-bind (value kind) (%fc-const (cadr item))
             (when (and (eq (fv-kind v) :character) (eq (fv-len v) :star)) (setf (fv-len v) (length value)))
             (when (and (eq kind :character) (not (eq (fv-kind v) :character)) (not (fv-declared v)))
               (setf (fv-kind v) :character (fv-len v) (length value)))
             (setf (fv-value v) (%fc-convert-constant value (fv-kind v) (fv-len v)) (fv-class v) :param)))))
      (:data (dolist (g (car args)) (setf (cu-datas u) (append (cu-datas u) (list g)))))
      (:format (let ((label (fn-label node)))
                 (unless label (%fc-error "A FORMAT statement needs a statement label"))
                 (push (cons label (car args)) (cu-formats u)))))))

(defun %fc-stmtfun-node-p (node)
  "True when the ASSIGN node defines a statement function rather than assigning to an array element."
  (let ((lhs (car (fn-args node))))
    (and (eq (car lhs) :ref)
         (not (cu-exec-seen *fc-unit*))
         (let ((v (%fc-find (cadr lhs))))
           (and (or (null v) (and (null (fv-dims v)) (not (member (fv-class v) '(:dummy :common :param)))))
                (every (lambda (a) (eq (car a) :name)) (caddr lhs)))))))

(defun %fc-define-stmtfun (node)
  (let* ((lhs (car (fn-args node))) (name (cadr lhs)) (params nil))
    (dolist (a (caddr lhs))
      (let* ((pn (cadr a)) (existing (gethash pn (cu-vars *fc-unit*))) (ty (if existing (cons (fv-kind existing) (fv-len existing)) (%fc-implicit pn))))
        (when (member pn params :key #'car :test #'string=) (%fc-error "The statement function ~A uses the argument ~A twice" name pn))
        (push (list pn (car ty) (cdr ty)) params)))
    (let ((v (or (%fc-find name) (%fc-create name :class :stmtfun))))
      (setf (fv-class v) :stmtfun (fv-params v) (nreverse params) (fv-body v) (cadr (fn-args node)))
      (push v (cu-stmtfuns *fc-unit*)))))

(defun %fc-declare-all (nodes)
  "Apply the declaration statements of NODES in order; returns the executable statements."
  (let ((exec nil) (u *fc-unit*))
    (dolist (node nodes)
      (let ((*fc-node* node) (kind (fn-kind node)))
        (%fc-collecting
          (cond ((member kind *fc-declaration-kinds*) (%fc-declare node))
                ((and (eq kind :assign) (%fc-stmtfun-node-p node)) (%fc-define-stmtfun node))
                (t (unless (eq kind :continue) (setf (cu-exec-seen u) t))
                   (push node exec))))))
    (nreverse exec)))

;;; ---- the statement tree ---------------------------------------------------------------------------------------------

(defvar *fc-rest* nil "The statements not yet arranged into the tree.")
(defvar *fc-consumed* nil "The label of a DO terminal statement just consumed, for loops that share it.")

(defun %fc-parse-seq (do-label)
  "Items up to a block terminator (ELSE, ELSE IF, END IF, END DO) or the terminal statement of a labeled DO."
  (let ((items nil))
    (loop
      (when (null *fc-rest*) (return))
      (let* ((n (car *fc-rest*)) (kind (fn-kind n)))
        (cond
          ((member kind '(:else :elseif :endif :enddo)) (return))
          ((eq kind :if) (pop *fc-rest*) (push (%fc-parse-if n) items))
          ((member kind '(:do :dowhile))
           (pop *fc-rest*)
           (push (%fc-parse-do n) items)
           (when (and do-label (eql *fc-consumed* do-label)) (return)))
          (t (pop *fc-rest*)
             (push (list :stmt n) items)
             (when (and do-label (eql (fn-label n) do-label)) (setq *fc-consumed* do-label) (return))))))
    (nreverse items)))

(defun %fc-parse-if (node)
  (let ((clauses nil) (else-items nil) (else-p nil) (end-label nil) (cur node))
    (loop
      (let ((items (%fc-parse-seq nil)))
        (push (cons (car (fn-args cur)) items) clauses))
      (let ((n (car *fc-rest*)))
        (cond ((null n) (let ((*fc-node* node)) (%fc-error "The block IF has no END IF")))
              ((eq (fn-kind n) :elseif) (pop *fc-rest*) (setq cur n))
              ((eq (fn-kind n) :else)
               (pop *fc-rest*) (setq else-p t else-items (%fc-parse-seq nil))
               (let ((e (car *fc-rest*)))
                 (unless (and e (eq (fn-kind e) :endif)) (let ((*fc-node* node)) (%fc-error "The block IF has no END IF")))
                 (pop *fc-rest*) (setq end-label (fn-label e)))
               (return))
              ((eq (fn-kind n) :endif) (pop *fc-rest*) (setq end-label (fn-label n)) (return))
              (t (let ((*fc-node* n)) (%fc-error "Misplaced ~(~A~) in a block IF" (fn-kind n)))))))
    (list :if node (nreverse clauses) else-items else-p end-label)))

(defun %fc-parse-do (node)
  (let* ((label (if (eq (fn-kind node) :do) (car (fn-args node)) (car (fn-args node)))) (body nil) (end-label nil))
    (setq *fc-consumed* nil)
    (setq body (%fc-parse-seq label))
    (cond
      (label
       (cond ((eql *fc-consumed* label) nil)
             ((and *fc-rest* (eq (fn-kind (car *fc-rest*)) :enddo) (eql (fn-label (car *fc-rest*)) label))
              (pop *fc-rest*) (setq *fc-consumed* label end-label label))
             (t (let ((*fc-node* node)) (%fc-error "The terminal statement ~D of the DO loop was not found" label)))))
      (t (if (and *fc-rest* (eq (fn-kind (car *fc-rest*)) :enddo))
             (progn (let ((e (pop *fc-rest*))) (setq end-label (fn-label e))) (setq *fc-consumed* nil))
             (let ((*fc-node* node)) (%fc-error "The DO loop has no END DO")))))
    (when end-label (setq body (append body (list (list :label end-label)))))
    (list (if (eq (fn-kind node) :do) :do :dowhile) node body)))

(defun %fc-build-tree (nodes)
  (let ((*fc-rest* nodes) (*fc-consumed* nil) (items nil))
    (loop
      (let ((part (%fc-parse-seq nil)))
        (setq items (append items part)))
      (when (null *fc-rest*) (return))
      (let* ((n (pop *fc-rest*)) (*fc-node* n))
        (%fc-collecting (%fc-error "~A without a matching ~A" (case (fn-kind n) (:enddo "END DO") (:endif "END IF") (:else "ELSE") (t "ELSE IF"))
                                   (if (eq (fn-kind n) :enddo) "DO" "block IF")))))
    items))

;;; ---- variables as places ---------------------------------------------------------------------------------------------

(defun %fc-place (node)
  "(values vec-form offset-form kind len var substring) for the variable, array element or substring NODE."
  (ecase (car node)
    (:name
     (let ((v (%fc-var (cadr node))))
       (case (fv-class v)
         (:param (%fc-error "The named constant ~A cannot be assigned to" (fv-name v)))
         ((:external :procdummy) (%fc-error "The procedure ~A cannot be assigned to" (fv-name v)))
         (:stmtfun (%fc-error "The statement function ~A cannot be assigned to" (fv-name v))))
       (when (fv-dims v) (%fc-error "The array ~A is used without subscripts" (fv-name v)))
       (setf (fv-used v) t)
       (multiple-value-bind (vec off) (%fc-storage v) (values vec off (fv-kind v) (fv-len v) v nil))))
    (:ref
     (let* ((name (cadr node)) (items (caddr node)) (v (%fc-find name)))
       (cond ((and v (fv-dims v))
              (multiple-value-bind (vec off) (%fc-element-place v items) (values vec off (fv-kind v) (fv-len v) v nil)))
             ((%fc-bit-substring-var-p v items)
              (multiple-value-bind (lo hi) (%fc-bit-bounds (car items))
                (multiple-value-bind (vec off) (%fc-storage v)
                  (setf (fv-used v) t)
                  (values vec off :integer nil v (list :bits lo hi)))))
             ((and v (eq (fv-kind v) :character) (= (length items) 1) (eq (car (car items)) :slice) (not (fv-dims v)))
              (let ((slice (car items)))
                (multiple-value-bind (vec off) (%fc-storage v)
                  (setf (fv-used v) t)
                  (values vec off :character (fv-len v) v
                          (list (if (cadr slice) (%fc-int-form (cadr slice) "A substring bound") 1)
                                (and (caddr slice) (%fc-int-form (caddr slice) "A substring bound")))))))
             (t (%fc-error "~A is not an array, so ~A(...) cannot be assigned to (statement functions must come before executable statements)" name name)))))
    (:substr
     (let ((base (cadr node)))
       (unless (eq (car base) :ref) (%fc-error "Only a character variable or array element can have a substring assigned"))
       (multiple-value-bind (vec off kind len v) (%fc-place base)
         (unless (eq kind :character) (%fc-error "A substring needs a CHARACTER variable"))
         (values vec off kind len v (list (if (caddr node) (%fc-int-form (caddr node) "A substring bound") 1)
                                          (and (cadddr node) (%fc-int-form (cadddr node) "A substring bound")))))))))

(defun %fc-store-form (vec off kind len substring value vkind)
  "Form storing VALUE (of kind VKIND) into the place."
  (cond
    ((and substring (eq (car substring) :bits))
     (unless (member vkind '(:integer :logical)) (%fc-error "A bit substring is given an INTEGER value, not ~A" (%fc-kind-name vkind)))
     (let ((lo (cadr substring)) (hi (caddr substring)) (o (%fc-temp "O")) (cur (%fc-temp "C")))
       `(let* ((,o ,off) (,cur (svref ,vec ,o)))
          (setf (svref ,vec ,o) (%f7-deposit-bits ,(%fc-coerce value vkind :integer) (if (eq ,cur 'f77-undefined) 0 ,cur) ,lo ,hi)))))
    ((eq kind :character)
     (unless (eq vkind :character) (%fc-error "~@(~A~) value cannot be assigned to a CHARACTER variable" (%fc-a-kind vkind)))
     (let ((o (%fc-temp "O")) (r (%fc-temp "R")))
       (if substring
           `(let ((,o ,off) (,r ,value))
              (setf (svref ,vec ,o)
                    (let ((cur (svref ,vec ,o)))
                      (%f7-substr-set (if (stringp cur) cur (make-string ,(if (integerp len) len 1) :initial-element #\Space))
                                      ,(car substring) ,(or (cadr substring) `(length (svref ,vec ,o))) ,r))))
           `(let ((,o ,off) (,r ,value))
              (setf (svref ,vec ,o) (%f7-cfit ,r (%f7-cstore-len (svref ,vec ,o) ,r ,(if (integerp len) len nil))))))))
    (t
     (when (eq vkind :character) (%fc-error "A CHARACTER value cannot be assigned to ~A variable" (%fc-a-kind kind)))
     `(setf (svref ,vec ,off) ,(%fc-coerce value vkind kind)))))

(defun %fc-assign (lhs rhs)
  (multiple-value-bind (vec off kind len v substring) (%fc-place lhs)
    (declare (ignore v))
    (multiple-value-bind (value vkind) (%fc-expr rhs)
      (%fc-store-form vec off kind len substring value vkind))))

;;; ---- statements --------------------------------------------------------------------------------------------------------

(defun %fc-line-form (node)
  (if *f77-debug-info* `(setq *f77-line* ,(fn-line node)) nil))

(defun %fc-goto-form (label)
  (unless (member label (cu-labels *fc-unit*)) (%fc-error "The statement label ~D is not defined in this program unit" label))
  `(go ,(%fc-label-tag label)))

(defun %fc-int-scalar-place (name what)
  (let ((v (%fc-var name)))
    (unless (and (null (fv-dims v)) (eq (fv-kind v) :integer) (not (member (fv-class v) '(:param :external :stmtfun :procdummy))))
      (%fc-error "~A must be a scalar INTEGER variable (~A is ~A)" what name (%fc-kind-name (fv-kind v))))
    (setf (fv-used v) t)
    (multiple-value-bind (vec off) (%fc-storage v) (values vec off v))))

(defun %fc-return-value-form ()
  (let ((u *fc-unit*))
    (case (cu-kind u)
      (:function (let ((rv (cu-result u)))
                   (multiple-value-bind (vec off) (%fc-storage rv)
                     `(let ((r (svref ,vec ,off)))
                        (if (eq r 'f77-undefined)
                            (%f7-rerror "The function ~A returned without giving its value a result" ,(string-downcase (cu-name u)))
                            r)))))
      (:subroutine 0)
      (t nil))))

(defun %fc-call-statement (node)
  (let* ((name (car (fn-args node))) (items (cadr (fn-args node))) (v (%fc-find name)))
    (cond
      ((and (null v) (string= name "MVBITS") (= (length items) 5))
       (let ((args (mapcar (lambda (n) (multiple-value-list (%fc-expr n))) (butlast items 2))))
         (multiple-value-bind (vec off kind len vv sub) (%fc-place (car (last items 2)))
           (declare (ignore len vv sub))
           (unless (eq kind :integer) (%fc-error "The TO argument of MVBITS must be an INTEGER variable"))
           (let ((to-pos (%fc-int-form (car (last items)) "The position")))
             `(setf (svref ,vec ,off) (%f7-mvbits ,@(mapcar (lambda (a) (%fc-coerce (car a) (cadr a) :integer)) args)
                                                  (svref ,vec ,off) ,to-pos))))))
      ((and (null v) (string= name "EXIT"))
       `(%f7-exit))
      (t
       (when (and v (not (member (fv-class v) '(:external :procdummy :dummy))) (or (fv-used v) (fv-dims v) (eq (fv-class v) :param)))
         (%fc-error "~A is a variable but is used as a subroutine" name))
       (let ((v (or v (%fc-create name :class :external :kind :real))) (forms nil) (kinds nil) (alts nil))
         (cond ((member (fv-class v) '(:external :procdummy)) nil)
               ((eq (fv-class v) :dummy) (setf (fv-class v) :procdummy))
               (t (setf (fv-class v) :external)))
         (dolist (n items)
           (if (eq (car n) :altret)
               (progn (push (cadr n) alts) (push nil forms))
               (multiple-value-bind (f k) (%fc-arg-form n) (push f forms) (push k kinds))))
         (setq forms (nreverse forms) kinds (nreverse kinds) alts (nreverse alts))
         (let ((sig (%fc-known-signature name)))
           (when (and sig (eq (car sig) :function)) (%fc-error "~A is a function, not a subroutine" name))
           (%fc-check-call name kinds))
         (let ((call (%fc-call-form (%fc-routine-sym name) v forms)))
           (if alts
               `(let ((%r ,call))
                  (case %r ,@(let ((i 0)) (mapcar (lambda (l) (setq i (+ i 1)) `(,i ,(%fc-goto-form l))) alts))))
               call)))))))

(defun %fc-assigned-goto (node)
  (destructuring-bind (name labels) (fn-args node)
    (multiple-value-bind (vec off) (%fc-int-scalar-place name "The variable of an assigned GOTO")
      (let ((labels (or labels (cu-assigned-labels *fc-unit*))))
        (unless labels (%fc-error "The assigned GOTO has no label list and no ASSIGN statements"))
        `(case (%f7-rd ,vec ,off ,(string-downcase name))
           ,@(mapcar (lambda (l) `(,l ,(%fc-goto-form l))) labels)
           (t (%f7-rerror "The variable ~A does not hold one of the labels of the assigned GOTO" ,(string-downcase name))))))))

(defun %fc-simple-statement (node)
  "The form for the non-structured statement NODE (without its label)."
  (let ((args (fn-args node)))
    (ecase (fn-kind node)
      ((:continue :format) nil)
      (:assign (%fc-assign (car args) (cadr args)))
      (:goto (%fc-goto-form (car args)))
      (:cgoto
       (multiple-value-bind (f k) (%fc-expr (cadr args))
         (unless (member k '(:integer :logical)) (%fc-error "The computed GOTO needs an INTEGER expression"))
         `(case ,(%fc-coerce f k :integer)
            ,@(let ((i 0)) (mapcar (lambda (l) (setq i (+ i 1)) `(,i ,(%fc-goto-form l))) (car args))))))
      (:agoto (%fc-assigned-goto node))
      (:assign-label
       (multiple-value-bind (vec off) (%fc-int-scalar-place (cadr args) "The variable of ASSIGN")
         (unless (member (car args) (cu-labels *fc-unit*)) (%fc-error "The statement label ~D is not defined" (car args)))
         `(setf (svref ,vec ,off) ,(car args))))
      (:aif
       (multiple-value-bind (f k) (%fc-expr (car args))
         (unless (member k '(:integer :real :double)) (%fc-error "The arithmetic IF needs an INTEGER or REAL expression, not ~A" (%fc-kind-name k)))
         `(let ((%v ,f)) (cond ((< %v 0) ,(%fc-goto-form (cadr args))) ((= %v 0) ,(%fc-goto-form (caddr args))) (t ,(%fc-goto-form (cadddr args)))))))
      (:lif
       (let ((c (%fc-condition (car args) "condition of the logical IF"))
             (inner (cadr args)))
         (when (member (car inner) '(:if :elseif :else :endif :do :dowhile :enddo :lif))
           (%fc-error "The statement after a logical IF cannot be ~(~A~)" (car inner)))
         (let ((form (%fc-simple-statement (%make-fnode :kind (car inner) :args (cdr inner) :line (fn-line node) :start (fn-start node) :end (fn-end node)))))
           `(when ,c ,(or form '(progn nil))))))
      (:call (%fc-call-statement node))
      (:return
       (let ((u *fc-unit*))
         (cond ((car args)
                (unless (eq (cu-kind u) :subroutine) (%fc-error "RETURN with an expression is only for subroutines"))
                `(return-from %f77 ,(%fc-int-form (car args) "The alternate return number")))
               (t `(return-from %f77 ,(%fc-return-value-form))))))
      (:stop (if (car args)
                 (multiple-value-bind (f k) (%fc-expr (car args))
                   `(%f7-stop ,(if (member k '(:integer :character)) f (%fc-error "STOP needs an integer or a character constant"))))
                 `(%f7-stop nil)))
      (:pause (if (car args) (multiple-value-bind (f k) (%fc-expr (car args)) (declare (ignore k)) `(%f7-pause ,f)) `(%f7-pause nil)))
      ((:read :write :print) (%fc-io-statement node))
      (:open (%fc-open-statement node))
      (:close (%fc-close-statement node))
      (:inquire (%fc-inquire-statement node))
      ((:rewind :backspace :endfile) (%fc-position-statement node)))))

(defun %fc-wrap-line (node form)
  (cond ((null form) nil)
        (t (let ((line (%fc-line-form node))) (if line `(progn ,line ,form) form)))))

(defun %fc-seq (items)
  "Forms for a sequence of tree ITEMS, with a tag for each statement label (so a TAGBODY when there are labels)."
  (let ((forms nil) (tagged nil))
    (flet ((tag (label) (push (%fc-label-tag label) forms) (setq tagged t))
           (add (form) (when form (push (if (consp form) form `(progn ,form)) forms))))
      (dolist (item items)
        (%fc-collecting
          (ecase (car item)
            (:stmt
             (let* ((node (cadr item)) (*fc-node* node))
               (when (and (fn-label node) (not (eq (fn-kind node) :format))) (tag (fn-label node)))
               (add (%fc-wrap-line node (%fc-simple-statement node)))))
            (:if
             (let* ((node (cadr item)) (*fc-node* node))
               (when (fn-label node) (tag (fn-label node)))
               (add (%fc-wrap-line node (%fc-if-form item)))
               (when (nth 5 item) (tag (nth 5 item)))))
            ((:do :dowhile)
             (let* ((node (cadr item)) (*fc-node* node))
               (when (fn-label node) (tag (fn-label node)))
               (add (%fc-wrap-line node (%fc-do-form item)))))
            (:label (tag (cadr item)))))))
    (let ((forms (nreverse forms)))
      (if tagged (list `(tagbody ,@forms)) forms))))

(defun %fc-progn (forms) (cond ((null forms) '(progn nil)) ((null (cdr forms)) (car forms)) (t `(progn ,@forms))))

(defun %fc-if-form (item)
  (destructuring-bind (node clauses else-items else-p end-label) (cdr item)
    (declare (ignore node end-label))
    `(cond ,@(mapcar (lambda (c)
                       (let ((*fc-node* (cadr item)))
                         `(,(%fc-condition (car c) "condition of the IF") ,@(or (%fc-seq (cdr c)) '(nil)))))
                     clauses)
           ,@(when else-p `((t ,@(or (%fc-seq else-items) '(nil))))))))

(defun %fc-loop-var (name)
  (let ((v (%fc-var name)))
    (unless (and (null (fv-dims v)) (member (fv-kind v) '(:integer :real :double)) (not (member (fv-class v) '(:param :external :stmtfun :procdummy))))
      (%fc-error "The DO variable ~A must be a scalar INTEGER, REAL or DOUBLE PRECISION variable" name))
    (setf (fv-used v) t)
    v))

(defun %fc-counted-loop (var from to step body-forms)
  "A form running BODY-FORMS for VAR = FROM, TO, STEP (AST nodes) with FORTRAN's iteration count."
  (let* ((v (%fc-loop-var var)) (k (fv-kind v)))
    (multiple-value-bind (vec off) (%fc-storage v)
      (flet ((conv (node what)
               (multiple-value-bind (f fk) (%fc-expr node)
                 (unless (or (%fc-numeric-p fk) (eq fk :logical)) (%fc-error "The ~A of a DO loop must be a number" what))
                 (when (%fc-complex-p fk) (%fc-error "The ~A of a DO loop cannot be COMPLEX" what))
                 (%fc-coerce f fk k))))
        (let ((cv (%fc-temp "V")) (cl (%fc-temp "L")) (cs (%fc-temp "S")) (cn (%fc-temp "N")))
          `(let* ((,cv ,(conv from "start")) (,cl ,(conv to "limit")) (,cs ,(if step (conv step "step") 1)) (,cn (%f7-trips ,cv ,cl ,cs)))
             (setf (svref ,vec ,off) ,cv)
             (loop
               (when (<= ,cn 0) (return))
               (setf (svref ,vec ,off) ,cv)
               ,@body-forms
               (setq ,cv ,(if (eq k :real) `(%f7-single (+ ,cv ,cs)) `(+ ,cv ,cs)) ,cn (- ,cn 1)))
             (setf (svref ,vec ,off) ,cv)))))))

(defun %fc-do-form (item)
  (let* ((node (cadr item)) (args (fn-args node)) (body (%fc-seq (caddr item))))
    (if (eq (car item) :do)
        (destructuring-bind (label var from to step) args
          (declare (ignore label))
          (%fc-counted-loop var from to step (or body '((progn nil)))))
        (let ((c (%fc-condition (cadr args) "condition of DO WHILE")))
          `(loop (unless ,c (return)) ,@body)))))

;;; ---- input and output statements --------------------------------------------------------------------------------------------

(defun %fc-find-format (label)
  (let ((f (assoc label (cu-formats *fc-unit*))))
    (unless f (%fc-error "The FORMAT statement ~D is not defined in this program unit" label))
    (handler-case (f77-parse-format (cdr f))
      (f77-syntax-error (c) (%fc-error "~A" (f77-error-message c))))))

(defun %fc-format-form (fmt present)
  "The format argument for %F7-BEGIN: NIL (list-directed), :UNFORMATTED, or a parsed format (quoted or computed)."
  (cond ((not present) :unformatted)
        ((eq fmt :star) nil)
        ((eq (car fmt) :int) `(quote ,(%fc-find-format (cadr fmt))))
        ((eq (car fmt) :str) `(quote ,(handler-case (f77-parse-format (cadr fmt)) (f77-syntax-error (c) (%fc-error "~A" (f77-error-message c))))))
        (t (multiple-value-bind (f k) (%fc-expr fmt)
             (case k
               (:character `(%f7-runtime-format ,f))
               (:integer ;; a label set with ASSIGN
                `(case ,f ,@(mapcar (lambda (fm) `(,(car fm) (quote ,(handler-case (f77-parse-format (cdr fm)) (f77-syntax-error () nil))))) (cu-formats *fc-unit*))
                   (t (%f7-ioerror 6 "The variable does not name a FORMAT statement"))))
               (t (%fc-error "The format must be a label or a CHARACTER expression, not ~A" (%fc-kind-name k))))))))

(defun %fc-store-lambda (node)
  "A lambda of one argument storing it into the variable NODE (converted from a CHARACTER value)."
  (multiple-value-bind (vec off kind len v substring) (%fc-place node)
    (declare (ignore v))
    (let ((a (%fc-temp "X")))
      `(lambda (,a) ,(%fc-store-form vec off kind len substring a kind)))))

(defun %fc-unit-form (u mode)
  "(values form internal-p) for the unit specifier U (:STAR or an expression)."
  (cond
    ((eq u :star) (values :star nil))
    (t
     (let ((kind (multiple-value-bind (f k) (%fc-expr u) (declare (ignore f)) k)))
       (cond ((eq kind :character)
              (if (eq mode :read)
                  (multiple-value-bind (f) (%fc-expr u) (values `(cons :internal (cons ,f nil)) t))
                  (multiple-value-bind (vec off k len) (%fc-place u)
                    (declare (ignore k))
                    (values `(cons :internal (cons ,(if (integerp len) len `(length (svref ,vec ,off))) ,(%fc-store-lambda u))) t))))
             ((member kind '(:integer :logical)) (values (%fc-int-form u "The unit") nil))
             (t (%fc-error "The unit must be an INTEGER expression or a CHARACTER variable, not ~A" (%fc-kind-name kind))))))))

(defun %fc-kind-symbol (kind) (intern (symbol-name kind) "KEYWORD"))

(defun %fc-io-item (node mode st)
  "A form transferring the item NODE with the statement state ST."
  (if (eq (car node) :implied-do)
      (destructuring-bind (items var from to step) (cdr node)
        (%fc-counted-loop var from to step (mapcar (lambda (i) (%fc-io-item i mode st)) items)))
      (if (eq mode :write) (%fc-io-output node st) (%fc-io-input node st))))

(defun %fc-whole-array (node)
  "The array variable when NODE names a whole array, else NIL."
  (and (eq (car node) :name) (let ((v (%fc-find (cadr node)))) (and v (fv-dims v) v))))

(defun %fc-io-output (node st)
  (let ((arr (%fc-whole-array node)))
    (if arr
        (multiple-value-bind (vec off) (%fc-storage arr)
          (setf (fv-used arr) t)
          (let ((k (%fc-temp "K")))
            `(dotimes (,k ,(%fc-array-size arr))
               (%f7-put ,st (%f7-rd ,vec (+ ,off (* ,k ,(%fc-words (fv-kind arr) (fv-len arr)))) ,(string-downcase (fv-name arr))) ,(%fc-kind-symbol (fv-kind arr))))))
        (multiple-value-bind (f k) (%fc-expr node)
          `(%f7-put ,st ,f ,(%fc-kind-symbol k))))))

(defun %fc-io-input (node st)
  (let ((arr (%fc-whole-array node)))
    (if arr
        (multiple-value-bind (vec off) (%fc-storage arr)
          (setf (fv-used arr) t)
          (let ((k (%fc-temp "K")) (o (%fc-temp "O")) (a (%fc-temp "X")) (kind (fv-kind arr)))
            `(dotimes (,k ,(%fc-array-size arr))
               (let ((,o (+ ,off (* ,k ,(%fc-words kind (fv-len arr))))))
                 (%f7-get ,st ,(%fc-kind-symbol kind)
                          (lambda (,a) ,(%fc-store-form vec o kind (fv-len arr) nil a kind))
                          ,(if (integerp (fv-len arr)) (fv-len arr) nil))))))
        (progn
          (unless (member (car node) '(:name :ref :substr)) (%fc-error "An input item must be a variable, not an expression"))
          (multiple-value-bind (vec off kind len v substring) (%fc-place node)
            (declare (ignore v))
            (let ((a (%fc-temp "X")) (o (%fc-temp "O")))
              `(let ((,o ,off))
                 (%f7-get ,st ,(%fc-kind-symbol kind) (lambda (,a) ,(%fc-store-form vec o kind len substring a kind))
                          ,(cond (substring nil) ((integerp len) len) ((eq kind :character) `(length (svref ,vec ,o))) (t nil))))))))))

(defun %fc-io-statement (node)
  (let* ((kind (fn-kind node)) (mode (if (eq kind :read) :read :write))
         (control (car (fn-args node))) (items (cadr (fn-args node))) (st (%fc-sym "%ST")))
    (let* ((unit (let ((u (getf control :unit))) (if (null u) (%fc-error "The input/output statement has no unit") u)))
           (fmt-present (member :fmt control)))
      (multiple-value-bind (unit-form internal) (%fc-unit-form unit mode)
        (declare (ignore internal))
        (let* ((fmt-form (%fc-format-form (getf control :fmt) fmt-present))
               (rec (getf control :rec))
               (iostat (getf control :iostat)) (end (getf control :end)) (err (getf control :err))
               (body `(let ((,st (%f7-begin ,mode ,unit-form ,fmt-form ,@(when rec (list (%fc-int-form rec "REC="))))))
                        ,@(mapcar (lambda (i) (%fc-io-item i mode st)) items)
                        (%f7-end ,st))))
          (%fc-guarded body iostat end err))))))

(defun %fc-guarded (body iostat end err)
  "BODY, an input/output operation, with IOSTAT=, END= and ERR= handling."
  (if (not (or iostat end err))
      body
      `(let ((%r (%f7-guard (lambda () ,body) ,(if iostat (%fc-store-lambda iostat) nil) ,(and end t) ,(and err t))))
         ,@(when (or end err)
             `((case %r
                 ,@(when end `((:end ,(%fc-goto-form (%fc-label-number end)))))
                 ,@(when err `((:err ,(%fc-goto-form (%fc-label-number err)))))))))))

(defun %fc-label-number (node)
  (unless (eq (car node) :int) (%fc-error "END= and ERR= need a statement label"))
  (cadr node))

(defun %fc-string-arg (node what)
  (multiple-value-bind (f k) (%fc-expr node)
    (unless (eq k :character) (%fc-error "~A must be a CHARACTER expression, not ~A" what (%fc-kind-name k)))
    f))

(defun %fc-open-statement (node)
  (let* ((control (car (fn-args node))) (unit (getf control :unit)))
    (unless unit (%fc-error "OPEN needs UNIT="))
    (%fc-guarded
     `(%f7-open ,(%fc-int-form unit "The unit")
                ,@(loop for key in '(:file :status :access :form :blank :position :action)
                        when (getf control key) append (list key (%fc-string-arg (getf control key) (string key))))
                ,@(when (getf control :recl) (list :recl (%fc-int-form (getf control :recl) "RECL="))))
     (getf control :iostat) nil (getf control :err))))

(defun %fc-close-statement (node)
  (let* ((control (car (fn-args node))) (unit (getf control :unit)))
    (unless unit (%fc-error "CLOSE needs UNIT="))
    (%fc-guarded `(%f7-close ,(%fc-int-form unit "The unit") ,@(when (getf control :status) (list (%fc-string-arg (getf control :status) "STATUS="))))
                 (getf control :iostat) nil (getf control :err))))

(defun %fc-inquire-statement (node)
  (let* ((control (car (fn-args node))) (unit (getf control :unit)) (file (getf control :file)) (p (%fc-temp "P")) (sets nil))
    (unless (or unit file) (%fc-error "INQUIRE needs UNIT= or FILE="))
    (dolist (key '(:exist :opened :number :named :name :access :sequential :direct :form :formatted :unformatted :recl :nextrec :blank))
      (let ((target (getf control key)))
        (when target
          (multiple-value-bind (vec off kind len v substring) (%fc-place target)
            (declare (ignore v))
            (let ((val `(getf ,p ,key)) (a (%fc-temp "X")))
              (push (case kind
                      (:logical `(let ((,a ,val)) ,(%fc-store-form vec off kind len substring `(and ,a t) :logical)))
                      (:integer `(let ((,a ,val)) ,(%fc-store-form vec off kind len substring `(or ,a 0) :integer)))
                      (:character `(let ((,a ,val)) ,(%fc-store-form vec off kind len substring `(or ,a "") :character)))
                      (t (%fc-error "The ~A variable of INQUIRE has the wrong type" key)))
                    sets))))))
    (%fc-guarded `(let ((,p (%f7-inquire ,(and unit (%fc-int-form unit "The unit")) ,(and file (%fc-string-arg file "FILE=")))))
                    ,@(nreverse sets))
                 (getf control :iostat) nil (getf control :err))))

(defun %fc-position-statement (node)
  (let* ((control (car (fn-args node))) (unit (getf control :unit)))
    (%fc-guarded `(,(ecase (fn-kind node) (:rewind '%f7-rewind) (:backspace '%f7-backspace) (:endfile '%f7-endfile))
                   ,(%fc-int-form unit "The unit"))
                 (getf control :iostat) nil (getf control :err))))

;;; ---- DATA --------------------------------------------------------------------------------------------------------------------

(defun %fc-data-slots (target)
  "The storage slots, in order, named by a DATA target: a list of (vector-form offset kind len)."
  (ecase (car target)
    (:name
     (let ((v (%fc-var (cadr target))))
       (when (member (fv-class v) '(:dummy :procdummy :param :external)) (%fc-error "~A cannot be initialised in a DATA statement" (fv-name v)))
       (multiple-value-bind (vec off) (%fc-storage v)
         (let ((words (%fc-words (fv-kind v) (fv-len v))))
           (if (fv-dims v)
               (let ((acc nil)) (dotimes (k (%fc-const-array-size v)) (push (list vec (+ off (* k words)) (fv-kind v) (fv-len v)) acc)) (nreverse acc))
               (list (list vec off (fv-kind v) (fv-len v))))))))
    (:ref
     (multiple-value-bind (v word-off) (%fc-equiv-member target)
       (when (member (fv-class v) '(:dummy :procdummy :param)) (%fc-error "~A cannot be initialised in a DATA statement" (fv-name v)))
       (multiple-value-bind (vec off) (%fc-storage v)
         (list (list vec (+ off word-off) (fv-kind v) (fv-len v))))))
    (:implied-do
     (destructuring-bind (items var from to step) (cdr target)
       (let ((a (%fc-const-int from "A DATA loop bound")) (b (%fc-const-int to "A DATA loop bound"))
             (s (if step (%fc-const-int step "A DATA loop step") 1)) (acc nil))
         (when (zerop s) (%fc-error "The step of a DATA implied DO is zero"))
         (when (> (abs (floor (- b a) s)) 1000000) (%fc-error "The DATA implied DO runs too many times"))
         (do ((i a (+ i s))) ((if (> s 0) (> i b) (< i b)))
           (let ((*fc-const-env* (cons (cons var i) *fc-const-env*)))
             (dolist (it items) (setq acc (append acc (%fc-data-slots it))))))
         acc)))
    (:substr (%fc-error "A substring cannot be initialised in a DATA statement"))))

(defun %fc-data-statement (group)
  (let ((slots (mapcan #'%fc-data-slots (car group))) (values nil))
    (when (> (length slots) 1000000) (%fc-error "The DATA statement names too many storage units (~D)" (length slots)))
    (dolist (rv (cadr group))
      (let ((val (%fc-const (cdr rv))))
        (when (> (+ (car rv) (length values)) (+ (length slots) 1)) (%fc-error "The DATA statement gives more values than it names storage units"))
        (dotimes (i (car rv)) (push val values))))
    (setq values (nreverse values))
    (unless (= (length slots) (length values))
      (%fc-error "The DATA statement names ~D storage unit~:P but gives ~D value~:P" (length slots) (length values)))
    (do ((s slots (cdr s)) (v values (cdr v))) ((null s))
      (let ((slot (car s)))
        (push `(setf (svref ,(car slot) ,(cadr slot)) ,(%fc-literal (%fc-convert-constant (car v) (caddr slot) (cadddr slot))))
              (cu-init-forms *fc-unit*))))))

;;; ---- program units -------------------------------------------------------------------------------------------------------------

(defun %fc-collect-labels (nodes)
  (let ((labels nil) (assigned nil))
    (dolist (n nodes)
      (when (and (fn-label n) (not (eq (fn-kind n) :format)))
        (when (member (fn-label n) labels) (let ((*fc-node* n)) (%fc-collecting (%fc-error "The statement label ~D is defined twice" (fn-label n)))))
        (push (fn-label n) labels))
      (when (eq (fn-kind n) :assign-label) (push (car (fn-args n)) assigned)))
    (values labels (remove-duplicates assigned))))

(defun %fc-dummy-prologue (u)
  "Bindings for the dummy arguments: (values lambda-list bindings)."
  (let ((ll nil) (binds nil) (star 0))
    (dolist (p (cu-params u))
      (if (eq p :star)
          (progn (setq star (+ star 1)) (push (%fc-sym "%STAR~D" star) ll))
          (let* ((v (gethash p (cu-vars u))) (a (%fc-sym "~A%A" p)))
            (push a ll)
            (if (eq (fv-class v) :procdummy)
                (push (list (%fc-sym "~A%P" p) a) binds)
                (let ((r (%fc-sym "~A%R" p)))
                  (%fc-storage v)
                  (push (list r `(%f7-arg ,a)) binds)
                  (push (list (fv-vec v) `(car ,r)) binds)
                  (push (list (fv-off v) `(cdr ,r)) binds))))))
    (values (nreverse ll) (nreverse binds))))

(defun %fc-dim-bindings ()
  (let ((out nil))
    (dolist (d (reverse (cu-dim-binds *fc-unit*)))
      (if (eq (caddr d) :stride)
          (push (list (car d) (cadr d)) out)
          (push (list (car d) (%fc-int-form (cadr d) (caddr d))) out)))
    (nreverse out)))

(defun %fc-common-bindings (u)
  (let ((out nil))
    (dolist (cb (cu-commons u))
      (let ((pair (%fc-sym "%CMP-~A" (cm-sym-n cb))))
        (push (list pair `(%f7-common ,(cm-name cb) ,(cm-nn cb) ,(cm-nc cb) (quote ,(reverse (cm-zspecs cb))))) out)
        (push (list (cm-sym-n cb) `(car ,pair)) out)
        (push (list (cm-sym-c cb) `(cdr ,pair)) out)))
    (nreverse out)))

(defun %fc-parameter-kind (v)
  (cond ((eq (fv-class v) :procdummy) :procedure) (t (fv-kind v))))

(defun %fc-compile-unit (ast)
  "The Lisp forms for the program unit AST (an FAST)."
  (let* ((kind (fu-kind ast)) (name (fu-name ast))
         (u (make-cunit :kind kind :name (or name (if (eq kind :program) ".MAIN." nil)) :params (fu-params ast) :type (fu-type ast)))
         (*fc-unit* u) (*fc-temp* 0) (*fc-scope* nil) (*fc-node* (car (fu-stmts ast))))
    (setf (cu-key u) (and (member kind '(:subroutine :function)) (%fc-routine-sym name)))
    (multiple-value-bind (labels assigned) (%fc-collect-labels (fu-stmts ast))
      (setf (cu-labels u) labels (cu-assigned-labels u) assigned))
    ;; dummy arguments and the function result
    (dolist (p (cu-params u))
      (unless (eq p :star)
        (when (gethash p (cu-vars u)) (%fc-collecting (%fc-error "The argument ~A is repeated" p)))
        (%fc-create p :class :dummy :kind (car (%fc-implicit p)))))
    (when (eq kind :function)
      (let* ((ty (cu-type u)) (imp (%fc-implicit name))
             (rv (%fc-create name :class :result :kind (if ty (cadr ty) (car imp)))))
        (when ty (setf (fv-declared rv) t (fv-len rv) (if (eq (cadr ty) :character) (%fc-length-value (caddr ty)) nil)))
        (unless ty (setf (fv-len rv) (cdr imp)))
        (setf (cu-result u) rv)))
    (let* ((stmts (fu-stmts ast))
           (stmts (if (and stmts (member (fn-kind (car stmts)) '(:program :subroutine :function :blockdata))) (cdr stmts) stmts))
           (body-nodes (if (and stmts (eq (fn-kind (car (last stmts))) :end)) (butlast stmts) stmts))
           (exec (%fc-declare-all body-nodes)))
      (when (cu-implicit-none u)
        (dolist (p (cu-params u))
          (unless (eq p :star)
            (let ((v (gethash p (cu-vars u))))
              (unless (or (fv-declared v) (eq (fv-class v) :procdummy)) (%fc-collecting (%fc-error "The argument ~A is not declared (IMPLICIT NONE)" p)))))))
      ;; storage: COMMON, then EQUIVALENCE
      (dolist (d (reverse (cu-common-decls u)))
        (%fc-collecting (%fc-layout-common (car d) (cdr d))))
      (%fc-collecting (%fc-layout-equivalences))
      (dolist (g (cu-datas u)) (%fc-collecting (%fc-data-statement g)))
      (let* ((tree (%fc-build-tree exec))
             (body (let ((*fc-node* nil)) (%fc-seq tree))))
        (when (eq kind :function) (%fc-storage (cu-result u)))
        (%fc-assemble-unit u ast body)))))

(defun %fc-assemble-unit (u ast body)
  (let* ((kind (cu-kind u)) (name (cu-name u)) (init (reverse (cu-init-forms u))))
    (multiple-value-bind (ll dummy-binds) (%fc-dummy-prologue u)
      (let* ((dim-binds (%fc-dim-bindings))
             (routine-sym (%fc-routine-sym (string (or name ""))))
             (ret (%fc-return-value-form))
             (core `(let* (,@(%fc-common-bindings u))
                      ,@(if (eq kind :blockdata)
                            (when init `((progn ,@init)))
                            `((multiple-value-bind (%fr %new) (%f7-frame (quote ,routine-sym) ,(cu-nn u) ,(cu-nc u) (quote ,(reverse (cu-zspecs u))))
                                (let* ((,(%fc-sym "%NV") (car %fr)) (,(%fc-sym "%CV") (cdr %fr)) ,@dummy-binds ,@dim-binds)
                                  (when %new ,@init)
                                  (block %f77
                                    ,@(if body `((tagbody ,@body)) nil)
                                    ,ret))))))))
        ;; the signature, for call checks by units compiled later
        (when (member kind '(:subroutine :function))
          (setf (gethash (%string-concat (package-name *fc-package*) ":" name) *f77-signatures*)
                (list kind (and (eq kind :function) (fv-kind (cu-result u)))
                      (mapcar (lambda (p) (if (eq p :star) :star (let ((v (gethash p (cu-vars u)))) (cons (%fc-parameter-kind v) (and (fv-dims v) t))))) (cu-params u)))))
        (let ((wrapped (if *f77-debug-info*
                           `(let ((*f77-routine* (quote ,routine-sym)) (*f77-line* *f77-line*)) ,core)
                           core)))
          (ecase kind
            (:program
             (list `(%f77-define-program ,name ,(package-name *fc-package*) (lambda () ,wrapped) ,*fc-file*)))
            ((:subroutine :function)
             (list `(defun ,routine-sym ,ll ,wrapped)
                   `(%f77-note-routine ,name ,(package-name *fc-package*) ,kind ,*fc-file*)))
            (:blockdata
             (list `(%f77-define-blockdata ,(or name "") ,(package-name *fc-package*) (lambda () ,wrapped) ,*fc-file*)))))))))

;;; ---- the entry point -------------------------------------------------------------------------------------------------------------------

(defun f77-ensure-package (name)
  "The package NAME, created (using no other package) if it does not exist."
  (if (packagep name) name
      (or (find-package (string-upcase (string name))) (%make-package (string-upcase (string name)) nil nil))))

(defun f77-translate (source &key (package *f77-default-package*) file (directory "") only-at)
  "Translate the fixed-format FORTRAN SOURCE into Lisp: (values forms errors units).  FORMS are top-level Lisp forms to be
evaluated or compiled in order; ERRORS are F77-SYNTAX-ERRORs (if there are any, FORMS is NIL); UNITS are the parsed program
units.  With ONLY-AT (an offset), only the program unit containing that offset is translated."
  (multiple-value-bind (units errors) (f77-parse source :file file :directory directory)
    (let* ((*fc-package* (f77-ensure-package package)) (*fc-errors* nil) (*fc-file* file) (forms nil)
           (selected (if only-at
                         (remove-if-not (lambda (u) (and (<= (fu-start u) only-at) (<= only-at (fu-end u)))) units)
                         units)))
      ;; the signatures of the units being compiled again are stale until each is compiled
      (dolist (u selected)
        (when (and (fu-name u) (member (fu-kind u) '(:subroutine :function)))
          (remhash (%string-concat (package-name *fc-package*) ":" (fu-name u)) *f77-signatures*)))
      (dolist (u selected)
        (handler-case (setq forms (append forms (%fc-compile-unit u)))
          (f77-syntax-error (c) (push c *fc-errors*))))
      (let ((errs (sort (copy-list (append errors *fc-errors*)) #'< :key (lambda (c) (or (f77-error-start c) 0)))))
        (values (if errs nil forms) errs selected)))))
