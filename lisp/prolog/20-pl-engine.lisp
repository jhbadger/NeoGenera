;;;; 20-pl-engine.lisp -- the Prolog Tool Kit: the database, clause compiler and the solving machine.
;;;;
;;;; Clauses are compiled to templates: a variable becomes a PLTV (an index into a per-call binding vector), a compound
;;;; with variables a PLTC, and a ground subterm is shared as it is.  Head unification works directly against the
;;;; template (a variable's first occurrence just stores the goal's argument in the vector) and body goals are built only
;;;; when they run.  The machine is iterative: a continuation (a chain of PLFR frames) and a stack of choicepoints
;;;; (PLCP structures linked by PREV) live in a PLM, so deep recursion costs heap, not Lisp stack.  Cut is "pop the
;;;; choicepoint stack back to the entry barrier" and exceptions unwind it to the nearest active catch/3.

(in-package :cl)

;;; ---- predicates and clauses -------------------------------------------------------------------------------------------

(defstruct (plpred (:constructor %make-plpred (name arity)) (:copier nil))
  name arity (clauses nil) (tail nil) (dynamic nil) (defined nil) (library nil) (fn nil) (kind nil) (ctl nil)
  (nclauses 0) (file nil) (discontiguous nil) (spy nil))

(defstruct (plclause (:constructor %make-plclause (pred head-args body goals-rev nvars)) (:copier nil))
  pred head-args body goals-rev nvars (key1 nil) (key2 nil) (born 0) (died most-positive-fixnum) (file nil) (offset nil) (line nil))

(defstruct (pltc (:constructor %make-pltc (name args)) (:copier nil)) name args)

(defvar *pl-generation* 0 "Incremented by every change to the database (the logical update view).")
(defvar *pl-all-preds* nil "Every predicate record, newest first.")

(defun pl-find-pred (name arity)
  (cdr (assoc arity (get name '%pl-preds))))

(defun pl-ensure-pred (name arity)
  (or (pl-find-pred name arity)
      (let ((p (%make-plpred name arity)))
        (setf (get name '%pl-preds) (cons (cons arity p) (get name '%pl-preds)))
        (push p *pl-all-preds*)
        p)))

(defun %pl-register (name arity &key fn kind ctl library)
  (let ((p (pl-ensure-pred (pl-atom name) arity)))
    (setf (plpred-fn p) fn (plpred-kind p) kind (plpred-ctl p) ctl (plpred-library p) library (plpred-defined p) t)
    p))

(defun pl-builtin-p (pred) (and (or (plpred-fn pred) (plpred-ctl pred)) (not (plpred-library pred))))

;;; ---- compiling clauses ----------------------------------------------------------------------------------------------------

(defun %pl-compile-term (term map)
  "The template of TERM; MAP is (alist . count) mapping variables to PLTVs."
  (setq term (%deref term))
  (cond ((plv-p term)
         (let ((c (assq term (car map))))
           (if c (cdr c)
               (let ((tv (%make-pltv (cdr map))))
                 (setf (car map) (cons (cons term tv) (car map)) (cdr map) (+ 1 (cdr map)))
                 tv))))
        ((pls-p term)
         (let* ((args (pls-args term)) (n (length args)) (targs (make-array n)) (ground t))
           (dotimes (i n)
             (let ((tm (%pl-compile-term (svref args i) map)))
               (setf (svref targs i) tm)
               (when (or (pltv-p tm) (pltc-p tm)) (setq ground nil))))
           (if ground (%make-pls (pls-name term) targs) (%make-pltc (pls-name term) targs))))
        (t term)))

(defun %pl-build (tmpl vec)
  "Instantiate the template TMPL with the binding vector VEC."
  (let ((head nil) (parent nil))
    (loop
      (let ((new nil) (next nil) (done t))
        (cond ((pltv-p tmpl)
               (let ((i (pltv-index tmpl))) (setq new (or (svref vec i) (setf (svref vec i) (pl-new-var))))))
              ((pltc-p tmpl)
               (let* ((targs (pltc-args tmpl)) (n (length targs)) (nargs (make-array n)))
                 (dotimes (i (- n 1)) (setf (svref nargs i) (%pl-build (svref targs i) vec)))
                 (setq new (%make-pls (pltc-name tmpl) nargs) next (svref targs (- n 1)) done nil)
                 (if parent (setf (svref parent (- (length parent) 1)) new) (setq head new))
                 (setq parent nargs)))
              (t (setq new tmpl)))
        (cond (done (if parent (setf (svref parent (- (length parent) 1)) new) (setq head new))
                    (return head))
              (t (setq tmpl next)))))))

(defun %pl-unify-head (tmpl term vec)
  (loop
    (cond ((pltv-p tmpl)
           (let* ((i (pltv-index tmpl)) (cell (svref vec i)))
             (if cell (return (pl-unify cell term)) (progn (setf (svref vec i) term) (return t)))))
          ((pltc-p tmpl)
           (setq term (%deref term))
           (cond ((pls-p term)
                  (let* ((targs (pltc-args tmpl)) (args (pls-args term)) (n (length targs)))
                    (unless (and (eq (pltc-name tmpl) (pls-name term)) (= n (length args))) (return nil))
                    (dotimes (k (- n 1)) (unless (%pl-unify-head (svref targs k) (svref args k) vec) (return-from %pl-unify-head nil)))
                    (setq tmpl (svref targs (- n 1)) term (svref args (- n 1)))))
                 ((plv-p term) (pl-bind term (%pl-build tmpl vec)) (return t))
                 (t (return nil))))
          (t (return (pl-unify tmpl term))))))

(defun %pl-key-of (term)
  "The first-argument index key of TERM: (values key arity), key NIL for a variable or string (matches anything)."
  (setq term (%deref term))
  (cond ((plv-p term) (values nil nil))
        ((pls-p term) (values (pls-name term) (length (pls-args term))))
        ((stringp term) (values nil nil))
        (t (values term 0))))

(defun %pl-control-name-p (name)
  (or (eq name *pl-comma*) (eq name *pl-semi*) (eq name *pl-arrow*) (eq name *pl-softarrow*)))

(defun %pl-wrap-goals (body)
  "BODY with variable goals wrapped in call/1, so that a cut inside them is local."
  (setq body (%deref body))
  (cond ((plv-p body) (%make-pls *pl-call* (vector body)))
        ((and (pls-p body) (= (length (pls-args body)) 2) (%pl-control-name-p (pls-name body)))
         (%make-pls (pls-name body) (vector (%pl-wrap-goals (pl-arg body 0)) (%pl-wrap-goals (pl-arg body 1)))))
        ((and (pls-p body) (= (length (pls-args body)) 1) (eq (pls-name body) (pl-atom "\\+")))
         (%make-pls (pls-name body) (vector (%pl-wrap-goals (pl-arg body 0)))))
        ((or (numberp body) (stringp body)) (pl-type-error "callable" body))
        (t body)))

(defun %pl-flatten-conj (body)
  (let ((acc nil))
    (loop
      (setq body (%deref body))
      (cond ((pl-functor-p body *pl-comma* 2) (push (pl-arg body 0) acc) (setq body (pl-arg body 1)))
            (t (push body acc) (return))))
    (nreverse acc)))

(defvar *pl-empty-vector* (vector))

(defun pl-compile-clause (head body pred)
  "Compile HEAD :- BODY (terms) into a clause of PRED."
  (let* ((map (cons nil 0)) (head (%deref head)) (body (%pl-wrap-goals body))
         (hargs (if (pls-p head) (map 'simple-vector (lambda (a) (%pl-compile-term a map)) (pls-args head)) *pl-empty-vector*))
         (goals (remove-if (lambda (g) (eq (%deref g) *pl-true*)) (%pl-flatten-conj body)))
         (gtemplates (mapcar (lambda (g) (%pl-compile-term g map)) goals))
         (btemplate (%pl-compile-term body map))
         (c (%make-plclause pred hargs btemplate (reverse gtemplates) (cdr map))))
    (when (> (length hargs) 0)
      (let ((a0 (svref hargs 0)))
        (cond ((pltv-p a0) nil)
              ((pltc-p a0) (setf (plclause-key1 c) (pltc-name a0) (plclause-key2 c) (length (pltc-args a0))))
              (t (multiple-value-bind (k a) (%pl-key-of a0) (setf (plclause-key1 c) k (plclause-key2 c) a))))))
    c))

(defun pl-clause-terms (c)
  "A fresh (values head body) for the clause C."
  (let* ((vec (if (> (plclause-nvars c) 0) (make-array (plclause-nvars c) :initial-element nil) *pl-empty-vector*))
         (pred (plclause-pred c)) (hargs (plclause-head-args c)))
    (values (if (> (length hargs) 0)
                (%make-pls (plpred-name pred) (map 'simple-vector (lambda (a) (%pl-build a vec)) hargs))
                (plpred-name pred))
            (%pl-build (plclause-body c) vec))))

(defun pl-add-clause-to-pred (pred clause &optional (at-end t))
  (setq *pl-generation* (+ *pl-generation* 1))
  (setf (plclause-born clause) *pl-generation*)
  (let ((cell (list clause)))
    (cond ((null (plpred-clauses pred)) (setf (plpred-clauses pred) cell (plpred-tail pred) cell))
          (at-end (setf (cdr (plpred-tail pred)) cell (plpred-tail pred) cell))
          (t (setf (cdr cell) (plpred-clauses pred) (plpred-clauses pred) cell))))
  (setf (plpred-nclauses pred) (+ 1 (plpred-nclauses pred)) (plpred-defined pred) t)
  clause)

(defun pl-remove-clause (pred clause)
  (setq *pl-generation* (+ *pl-generation* 1))
  (when (= (plclause-died clause) most-positive-fixnum)
    (setf (plclause-died clause) *pl-generation*)
    (setf (plpred-nclauses pred) (- (plpred-nclauses pred) 1))
    (let ((was-tail (and (plpred-tail pred) (eq (car (plpred-tail pred)) clause))))
      (setf (plpred-clauses pred) (delete clause (plpred-clauses pred) :test #'eq))
      (when was-tail (setf (plpred-tail pred) (last (plpred-clauses pred)))))))

(defun pl-wipe-pred (pred)
  "Remove every clause of PRED."
  (dolist (c (copy-list (plpred-clauses pred))) (pl-remove-clause pred c)))

(defun pl-split-clause (term)
  "(values head body) of the clause TERM (Head :- Body or a fact)."
  (setq term (%deref term))
  (if (pl-functor-p term *pl-neck* 2)
      (values (%deref (pl-arg term 0)) (%deref (pl-arg term 1)))
      (values term *pl-true*)))

(defun pl-assert-clause (term &key (at-end t) file offset line library)
  "Add the clause TERM to the database (assert semantics): returns the clause."
  (multiple-value-bind (head body) (pl-split-clause term)
    (cond ((plv-p head) (pl-instantiation-error))
          ((not (pl-callable-p head)) (pl-type-error "callable" head)))
    (let* ((name (pl-name head)) (arity (pl-arity head)) (pred (pl-ensure-pred name arity)))
      (when (or (plpred-ctl pred) (and (plpred-fn pred) (not (plpred-library pred))))
        (pl-permission-error "modify" "static_procedure" (pl-indicator name arity)))
      (when (and (plpred-library pred) (not library))
        ;; the user redefines a library predicate
        (pl-wipe-pred pred)
        (setf (plpred-fn pred) nil (plpred-kind pred) nil (plpred-library pred) nil))
      (when library (setf (plpred-library pred) t))
      (let ((c (pl-compile-clause head body pred)))
        (setf (plclause-file c) file (plclause-offset c) offset (plclause-line c) line)
        (pl-add-clause-to-pred pred c at-end)))))

;;; ---- the machine -----------------------------------------------------------------------------------------------------------------

(defstruct (plm (:constructor %make-plm ()) (:copier nil)) (cont nil) (cps nil) (cur nil) (depth 0))

(defstruct (plfr (:constructor %make-plfr (goal vec cutb next)) (:copier nil)) goal vec cutb next (kind nil) (data nil))

(defstruct (plcp (:constructor %make-plcp (kind prev trail epoch)) (:copier nil))
  kind prev trail epoch (frame nil) (args nil) (clauses nil) (gen 0) (key1 nil) (key2 nil) (cont nil) (data1 nil) (data2 nil) (flag t))

(defun %pl-push-cp (m kind)
  (setq *pl-epoch* (+ *pl-epoch* 1))
  (let ((cp (%make-plcp kind (plm-cps m) *pl-tn* *pl-epoch*)))
    (setf (plm-cps m) cp *pl-hb* *pl-epoch*)
    cp))

(defun %pl-cut-to (m target)
  (setf (plm-cps m) target *pl-hb* (plcp-epoch target)))

(defun %pl-pop-cp (m cp) (%pl-cut-to m (plcp-prev cp)))

(defun %pl-special-frame (kind data next)
  (let ((f (%make-plfr nil nil nil next))) (setf (plfr-kind f) kind (plfr-data f) data) f))

(defun %pl-next-clause (cell gen k1 k2)
  (loop
    (when (null cell) (return nil))
    (let ((c (car cell)))
      (when (and (<= (plclause-born c) gen) (> (plclause-died c) gen)
                 (or (null k1) (null (plclause-key1 c)) (and (eql k1 (plclause-key1 c)) (eql k2 (plclause-key2 c)))))
        (return cell)))
    (setq cell (cdr cell))))

(defvar *pl-trace* nil "True while tracing: every user predicate call prints its Call, Exit, Redo and Fail ports.")
(defvar *pl-inferences* 0)

(defun %pl-try-clause (m clause args cont cutb)
  (let* ((n (plclause-nvars clause)) (vec (if (> n 0) (make-array n :initial-element nil) *pl-empty-vector*))
         (hargs (plclause-head-args clause)))
    (dotimes (k (length hargs)) (unless (%pl-unify-head (svref hargs k) (svref args k) vec) (return-from %pl-try-clause nil)))
    (let ((goals (plclause-goals-rev clause)))
      (when goals
        ;; Every variable of the body exists before the body's first choicepoint, so its bindings are trailed and a
        ;; slot of VEC is never left holding a variable bound on a branch that was backtracked out of.
        (dotimes (i n) (unless (svref vec i) (setf (svref vec i) (pl-new-var)))))
      (let ((frames cont))
        (dolist (g goals) (setq frames (%make-plfr g vec cutb frames)))
        (setf (plm-cont m) frames)))
    t))

(defvar *pl-empty-args* (vector))

(defun %pl-call-user (m pred args)
  (setq *pl-inferences* (+ *pl-inferences* 1))
  (when (or *pl-trace* (plpred-spy pred)) (return-from %pl-call-user (%pl-traced-call m pred args)))
  (let ((gen *pl-generation*) (k1 nil) (k2 nil))
    (when (> (length args) 0) (multiple-value-setq (k1 k2) (%pl-key-of (svref args 0))))
    (let ((cell (%pl-next-clause (plpred-clauses pred) gen k1 k2)))
      (cond ((null cell) nil)
            (t (let* ((cutb (plm-cps m)) (cont (plm-cont m)) (next (%pl-next-clause (cdr cell) gen k1 k2)))
                 (when next
                   (let ((cp (%pl-push-cp m :clauses)))
                     (setf (plcp-args cp) args (plcp-clauses cp) next (plcp-gen cp) gen (plcp-key1 cp) k1 (plcp-key2 cp) k2
                           (plcp-cont cp) cont (plcp-data1 cp) pred)))
                 (%pl-try-clause m (car cell) args cont cutb)))))))

(defun %pl-resume-clauses (m cp)
  "Backtracking into a :clauses choicepoint: try its next clause.  True if the head unified."
  (let* ((cell (plcp-clauses cp)) (cutb (or (plcp-data2 cp) (plcp-prev cp)))
         (next (%pl-next-clause (cdr cell) (plcp-gen cp) (plcp-key1 cp) (plcp-key2 cp))))
    (cond (next (setf (plcp-clauses cp) next) (setq *pl-hb* (plcp-epoch cp)))
          (t (%pl-pop-cp m cp)))
    (setq *pl-inferences* (+ *pl-inferences* 1))
    (%pl-try-clause m (car cell) (plcp-args cp) (plcp-cont cp) cutb)))

(defun %pl-backtrack (m)
  "Resume the youngest choicepoint: true if execution can continue, NIL when only the barrier is left."
  (loop
    (let ((cp (plm-cps m)))
      (%pl-undo-to (plcp-trail cp))
      (case (plcp-kind cp)
        (:barrier (return nil))
        (:alt (%pl-pop-cp m cp) (setf (plm-cont m) (plcp-frame cp)) (return t))
        (:clauses (when (%pl-resume-clauses m cp) (return t)))
        (:gen (setq *pl-hb* (plcp-epoch cp))
         (setf (plm-cont m) (plcp-cont cp))
         (let ((r (funcall (plcp-data1 cp))))
           (cond ((eq r :last) (%pl-pop-cp m cp) (return t))
                 (r (return t))
                 (t (%pl-undo-to (plcp-trail cp)) (%pl-pop-cp m cp)))))
        (:tracefail (%pl-pop-cp m cp) (%pl-trace-port :fail (plcp-data1 cp)))
        (:traceredo (%pl-pop-cp m cp) (%pl-trace-port :redo (plcp-data1 cp)))
        (t (%pl-pop-cp m cp))))))

;;; ---- calling goals ------------------------------------------------------------------------------------------------------------------

(defun %pl-add-args (goal extra)
  "GOAL (a callable term) with the list EXTRA of arguments appended."
  (setq goal (%deref goal))
  (cond ((null extra) goal)
        ((plv-p goal) (pl-instantiation-error))
        ((pls-p goal) (%make-pls (pls-name goal) (concatenate 'simple-vector (pls-args goal) extra)))
        ((and goal (symbolp goal)) (%make-pls goal (coerce extra 'simple-vector)))
        (t (pl-type-error "callable" goal))))

(defun %pl-unknown-procedure (m name arity)
  (declare (ignore m))
  (if (eq (pl-flag "unknown") (pl-atom "fail"))
      nil
      (pl-throw (pl-error-term (pl-s "existence_error" (pl-atom "procedure") (pl-indicator name arity))
                               (pl-s "context" (pl-indicator name arity) (pl-new-var))))))

(defun %pl-call-builtin (m pred args)
  (setf (plm-cur m) pred)
  (let ((fn (plpred-fn pred)) (n (length args)))
    (if (eq (plpred-kind pred) :nondet)
        (let ((gen (case n (0 (funcall fn)) (1 (funcall fn (svref args 0))) (2 (funcall fn (svref args 0) (svref args 1)))
                     (3 (funcall fn (svref args 0) (svref args 1) (svref args 2)))
                     (4 (funcall fn (svref args 0) (svref args 1) (svref args 2) (svref args 3)))
                     (t (apply fn (coerce args 'list))))))
          (and gen (%pl-run-gen m gen)))
        (case n
          (0 (funcall fn))
          (1 (funcall fn (svref args 0)))
          (2 (funcall fn (svref args 0) (svref args 1)))
          (3 (funcall fn (svref args 0) (svref args 1) (svref args 2)))
          (4 (funcall fn (svref args 0) (svref args 1) (svref args 2) (svref args 3)))
          (5 (funcall fn (svref args 0) (svref args 1) (svref args 2) (svref args 3) (svref args 4)))
          (t (apply fn (coerce args 'list)))))))

(defun %pl-run-gen (m gen)
  "Run the first alternative of the generator GEN, leaving a choicepoint when more may follow."
  (let ((cp (%pl-push-cp m :gen)))
    (setf (plcp-data1 cp) gen (plcp-cont cp) (plm-cont m))
    (let ((r (funcall gen)))
      (cond ((eq r :last) (%pl-pop-cp m cp) t)
            (r t)
            (t (%pl-undo-to (plcp-trail cp)) (%pl-pop-cp m cp) nil)))))

(defun %pl-call (m goal cutb)
  (setq goal (%deref goal))
  (let ((name nil) (args nil))
    (cond ((pls-p goal) (setq name (pls-name goal) args (pls-args goal)))
          ((plv-p goal) (pl-instantiation-error))
          ((and goal (symbolp goal)) (setq name goal args *pl-empty-args*))
          (t (pl-type-error "callable" goal)))
    (let ((pred (pl-find-pred name (length args))))
      (cond ((null pred) (%pl-unknown-procedure m name (length args)))
            ((plpred-ctl pred) (%pl-control m (plpred-ctl pred) goal args cutb))
            ((plpred-fn pred) (%pl-call-builtin m pred args))
            ((or (plpred-clauses pred) (plpred-dynamic pred) (plpred-defined pred)) (%pl-call-user m pred args))
            (t (%pl-unknown-procedure m name (length args)))))))

(defun %pl-control (m ctl goal args cutb)
  (let ((cont (plm-cont m)))
    (case ctl
      (:true t)
      (:fail nil)
      (:cut (%pl-cut-to m cutb) t)
      (:conj (setf (plm-cont m) (%make-plfr (svref args 0) nil cutb (%make-plfr (svref args 1) nil cutb cont))) t)
      (:disj
       (let ((l (%deref (svref args 0))))
         (cond ((pl-functor-p l *pl-arrow* 2)
                (let* ((before (plm-cps m)) (alt (%pl-push-cp m :alt)))
                  (setf (plcp-frame alt) (%make-plfr (svref args 1) nil cutb cont))
                  (setf (plm-cont m)
                        (%make-plfr (pl-arg l 0) nil alt (%pl-special-frame :cutto before (%make-plfr (pl-arg l 1) nil cutb cont))))
                  t))
               ((pl-functor-p l *pl-softarrow* 2)
                (let* ((alt (%pl-push-cp m :alt)))
                  (setf (plcp-frame alt) (%make-plfr (svref args 1) nil cutb cont))
                  (setf (plm-cont m)
                        (%make-plfr (pl-arg l 0) nil alt (%pl-special-frame :softcut alt (%make-plfr (pl-arg l 1) nil cutb cont))))
                  t))
               (t (let ((alt (%pl-push-cp m :alt)))
                    (setf (plcp-frame alt) (%make-plfr (svref args 1) nil cutb cont))
                    (setf (plm-cont m) (%make-plfr l nil cutb cont))
                    t)))))
      (:ite (let ((before (plm-cps m)))
              (setf (plm-cont m) (%make-plfr (svref args 0) nil before (%pl-special-frame :cutto before (%make-plfr (svref args 1) nil cutb cont))))
              t))
      (:softite (setf (plm-cont m) (%make-plfr (svref args 0) nil (plm-cps m) (%make-plfr (svref args 1) nil cutb cont))) t)
      (:not (let* ((before (plm-cps m)) (alt (%pl-push-cp m :alt)))
              (declare (ignorable before))
              (setf (plcp-frame alt) cont)
              (setf (plm-cont m) (%make-plfr (svref args 0) nil alt (%pl-special-frame :cutfail before nil)))
              t))
      (:once (let ((before (plm-cps m)))
               (setf (plm-cont m) (%make-plfr (svref args 0) nil before (%pl-special-frame :cutto before cont)))
               t))
      (:ignore (let* ((before (plm-cps m)) (alt (%pl-push-cp m :alt)))
                 (setf (plcp-frame alt) cont)
                 (setf (plm-cont m) (%make-plfr (svref args 0) nil alt (%pl-special-frame :cutto before cont)))
                 t))
      (:forall (let ((g (%make-pls (pl-atom "\\+") (vector (%make-pls *pl-comma* (vector (svref args 0) (%make-pls (pl-atom "\\+") (vector (svref args 1)))))))))
                 (setf (plm-cont m) (%make-plfr g nil cutb cont)) t))
      (:call (let ((g (%pl-add-args (svref args 0) (coerce (subseq args 1) 'list))))
               (setf (plm-cont m) (%make-plfr g nil (plm-cps m) cont)) t))
      (:colon (setf (plm-cont m) (%make-plfr (svref args 1) nil (plm-cps m) cont)) t)
      (:catch (let ((cp (%pl-push-cp m :catch)))
                (setf (plcp-data1 cp) (svref args 1) (plcp-data2 cp) (svref args 2) (plcp-cont cp) cont)
                (setf (plm-cont m) (%make-plfr (svref args 0) nil cp (%pl-special-frame :popcatch cp cont)))
                t))
      (t (error "Unknown control construct ~A" ctl)))
    ))

(defun %pl-exec-special (m fr)
  (let ((kind (plfr-kind fr)) (data (plfr-data fr)))
    (case kind
      (:cutto (%pl-cut-to m data) t)
      (:cutfail (%pl-cut-to m data) nil)
      (:softcut (setf (plcp-kind data) :dead) t)
      (:popcatch
       (if (eq (plm-cps m) data)
           (%pl-pop-cp m data)
           (progn (setf (plcp-flag data) nil)
                  (pl-trail-undo-function (lambda () (setf (plcp-flag data) t)))))
       t)
      (:traceexit (%pl-trace-exit m data) t)
      (:call-lisp (funcall data) t)
      (t (error "Unknown special frame ~A" kind)))))

(defun %pl-exec (m fr)
  (if (plfr-kind fr)
      (%pl-exec-special m fr)
      (let* ((g (plfr-goal fr)) (vec (plfr-vec fr)))
        (%pl-call m (if vec (%pl-build g vec) g) (plfr-cutb fr)))))

(defun %pl-check-memory ()
  "Raise resource_error(memory) when the live heap is above the flag stack_limit (a full collection is made first to measure
it).  The kernel does not survive exhausting its heap, so runaway recursion must be stopped before that."
  (let ((limit (let ((v (pl-flag "stack_limit"))) (if (integerp v) v 134217728))))
    (when (> (%heap-used) (+ limit 67108864))
      (when (> (gc) limit)
        (pl-resource-error "memory")))))

(defun %pl-run (m)
  "Run until the continuation is empty (:SOLUTION) or there is nothing left to backtrack into (NIL)."
  (let ((steps 0))
    (loop
      (let ((fr (plm-cont m)))
        (when (null fr) (return :solution))
        (setf (plm-cont m) (plfr-next fr))
        (setq steps (+ steps 1))
        (when (= 0 (logand steps 2047)) (%pl-check-memory))
        (unless (%pl-exec m fr)
          (unless (%pl-backtrack m) (return nil)))))))

;;; ---- exceptions -----------------------------------------------------------------------------------------------------------------------

(defun %pl-lisp-error-ball (c)
  (pl-error-term (pl-s "system_error" (let ((s (handler-case (princ-to-string c) (error () "error")))) s))))

(defun %pl-add-context (m ball)
  "BALL with the unbound context of an error(Formal, Context) filled in from the builtin that raised it."
  (setq ball (%deref ball))
  (if (and (plm-cur m) (pl-functor-p ball *pl-error* 2) (plv-p (%deref (pl-arg ball 1))))
      (let ((p (plm-cur m)))
        (%make-pls *pl-error* (vector (pl-arg ball 0) (pl-s "context" (pl-indicator (plpred-name p) (plpred-arity p)) (pl-new-var)))))
      ball))

(defun %pl-handle-throw (m ball)
  "Unwind to the nearest active catch/3 whose catcher unifies with BALL: true if one was found (execution can continue)."
  (loop
    (let ((cp (plm-cps m)))
      (%pl-undo-to (plcp-trail cp))
      (cond ((eq (plcp-kind cp) :barrier) (return nil))
            ((and (eq (plcp-kind cp) :catch) (plcp-flag cp))
             (%pl-pop-cp m cp)
             (let ((mark *pl-tn*) (b (pl-copy-term ball)))
               (if (pl-unify (plcp-data1 cp) b)
                   (progn (setf (plm-cont m) (%make-plfr (plcp-data2 cp) nil (plm-cps m) (plcp-cont cp)))
                          (return t))
                   (%pl-undo-to mark))))
            (t (%pl-pop-cp m cp))))))

(defun %pl-drive (m &optional redo)
  "Run M to its next solution (:SOLUTION) or failure (NIL); a Prolog exception unwinds to a catch/3 or, if none is
active in M, is signalled to the caller after M's bindings are undone."
  (loop
    (let ((r (handler-case (if redo (and (%pl-backtrack m) (%pl-run m)) (%pl-run m))
               (pl-halt (c) (error c))
               (pl-error (c) (list :error c (pl-error-ball c)))
               (error (c) (list :error c (%pl-lisp-error-ball c))))))
      (cond ((consp r)
             (setq redo nil)
             (let ((ball (%pl-add-context m (nth 2 r))))
               (unless (%pl-handle-throw m ball)
                 (if (typep (nth 1 r) 'pl-error)
                     (error 'pl-error :ball (pl-resolve ball))
                     (error 'pl-error :ball (pl-resolve ball))))))
            (t (return r))))))

;;; ---- solving: the Lisp interface ---------------------------------------------------------------------------------------------------

(defun %pl-new-machine (goal)
  (let ((m (%make-plm)))
    (%pl-push-cp m :barrier)
    (setf (plm-cont m) (%make-plfr goal nil (plm-cps m) nil))
    m))

(defun pl-do-solutions (goal fn)
  "Call FN once for each solution of the goal term GOAL, with the bindings in place; FN may return :STOP.  All bindings are
undone at the end.  Prolog exceptions are signalled as PL-ERROR."
  (let* ((saved-hb *pl-hb*) (m (%pl-new-machine goal)) (barrier (plm-cps m)))
    (unwind-protect
         (let ((r (%pl-drive m)))
           (loop
             (when (null r) (return))
             (when (eq (funcall fn) :stop) (return))
             (setq r (%pl-drive m t))))
      (%pl-undo-to (plcp-trail barrier))
      (setq *pl-hb* saved-hb))))

(defun pl-solve-once (goal)
  "True if the goal term GOAL has a solution; its bindings are kept (and undone if the caller backtracks)."
  (let* ((saved-hb *pl-hb*) (m (%pl-new-machine goal)) (barrier (plm-cps m)) (ok nil))
    (unwind-protect (setq ok (eq :solution (%pl-drive m)))
      (setq *pl-hb* saved-hb)
      (unless ok (%pl-undo-to (plcp-trail barrier))))
    ok))

(defun pl-prove (goal)
  "True if GOAL has a solution; no bindings are kept."
  (let ((mark *pl-tn*) (saved-hb *pl-hb*))
    (setq *pl-hb* (+ *pl-epoch* 1))
    (setq *pl-epoch* (+ *pl-epoch* 1))
    (unwind-protect (pl-solve-once goal)
      (%pl-undo-to mark)
      (setq *pl-hb* saved-hb))))

;;; ---- tracing --------------------------------------------------------------------------------------------------------------------------

(defvar *pl-trace-counter* 0)

(defun %pl-trace-port (port info)
  "INFO is (id depth goal)."
  (let ((id (first info)) (depth (second info)) (goal (third info)))
    (format *standard-output* "~&~A~A: (~D) ~A~%" (make-string (min 40 (* 1 (- depth 1))) :initial-element #\Space)
            (case port (:call "Call") (:exit "Exit") (:redo "Redo") (:fail "Fail") (t port))
            id (pl-term-to-string goal :quoted t :numbervars t :max-depth 10 :spacing t))
    t))

(defvar *pl-trace-depth* 0)

(defun %pl-traced-call (m pred args)
  (let* ((gen *pl-generation*) (k1 nil) (k2 nil)
         (goal (if (> (length args) 0) (%make-pls (plpred-name pred) args) (plpred-name pred)))
         (id (setq *pl-trace-counter* (+ *pl-trace-counter* 1)))
         (depth (+ 1 (plm-depth m)))
         (info (list id depth goal)))
    (setf (plm-depth m) depth)
    (%pl-trace-port :call info)
    (let ((cutb (plm-cps m)))
      (let ((tf (%pl-push-cp m :tracefail))) (setf (plcp-data1 tf) info))
      (when (> (length args) 0) (multiple-value-setq (k1 k2) (%pl-key-of (svref args 0))))
      (let ((cell (%pl-next-clause (plpred-clauses pred) gen k1 k2)))
        (cond ((null cell) nil)
              (t (let* ((cont (%pl-special-frame :traceexit (cons m info) (plm-cont m)))
                        (next (%pl-next-clause (cdr cell) gen k1 k2)))
                   (when next
                     (let ((cp (%pl-push-cp m :clauses)))
                       (setf (plcp-args cp) args (plcp-clauses cp) next (plcp-gen cp) gen (plcp-key1 cp) k1 (plcp-key2 cp) k2
                             (plcp-cont cp) cont (plcp-data1 cp) pred (plcp-data2 cp) cutb)))
                   ;; the clause body's cut barrier is below the trace choicepoint, so a cut also removes it
                   (%pl-try-clause m (car cell) args cont cutb))))))))

(defun %pl-trace-exit (m data)
  (let ((info (cdr data)))
    (%pl-trace-port :exit info)
    (setf (plm-depth m) (- (second info) 1))
    (let ((cp (%pl-push-cp m :traceredo))) (setf (plcp-data1 cp) info))
    t))

;;; ---- DCG translation -------------------------------------------------------------------------------------------------------------------

(defun %dcg-terminals (list s0 s)
  "S0 = [t1,...,tn|S] for the terminal list LIST."
  (multiple-value-bind (items status) (pl-list-to-lisp list)
    (unless (eq status :proper) (pl-type-error "list" list))
    (%make-pls *pl-equals* (vector s0 (lisp-to-pl-list items s)))))

(defun pl-dcg-body (b s0 s)
  "Translate the DCG body B into a goal over the difference list S0-S."
  (setq b (%deref b))
  (cond ((plv-p b) (pl-s "phrase" b s0 s))
        ((pl-functor-p b *pl-comma* 2)
         (let ((mid (pl-new-var)))
           (%make-pls *pl-comma* (vector (pl-dcg-body (pl-arg b 0) s0 mid) (pl-dcg-body (pl-arg b 1) mid s)))))
        ((or (pl-functor-p b *pl-semi* 2) (pl-functor-p b *pl-bar* 2))
         (%make-pls *pl-semi* (vector (pl-dcg-body (pl-arg b 0) s0 s) (pl-dcg-body (pl-arg b 1) s0 s))))
        ((pl-functor-p b *pl-arrow* 2)
         (let ((mid (pl-new-var)))
           (%make-pls *pl-arrow* (vector (pl-dcg-body (pl-arg b 0) s0 mid) (pl-dcg-body (pl-arg b 1) mid s)))))
        ((pl-functor-p b *pl-softarrow* 2)
         (let ((mid (pl-new-var)))
           (%make-pls *pl-softarrow* (vector (pl-dcg-body (pl-arg b 0) s0 mid) (pl-dcg-body (pl-arg b 1) mid s)))))
        ((pl-functor-p b (pl-atom "\\+") 1)
         (%make-pls *pl-comma* (vector (%make-pls (pl-atom "\\+") (vector (pl-dcg-body (pl-arg b 0) s0 (pl-new-var))))
                                       (%make-pls *pl-equals* (vector s0 s)))))
        ((eq b *pl-cut*) (%make-pls *pl-comma* (vector *pl-cut* (%make-pls *pl-equals* (vector s0 s)))))
        ((eq b *pl-nil*) (%make-pls *pl-equals* (vector s0 s)))
        ((pl-functor-p b *pl-curly* 1)
         (%make-pls *pl-comma* (vector (pl-arg b 0) (%make-pls *pl-equals* (vector s0 s)))))
        ((pl-is-cons b) (%dcg-terminals b s0 s))
        ((stringp b) (%dcg-terminals (pl-string-codes b) s0 s))
        ((and (pls-p b) (eq (pls-name b) *pl-call*))
         (%pl-add-args b (list s0 s)))
        ((pl-callable-p b) (%pl-add-args b (list s0 s)))
        (t (pl-type-error "callable" b))))

(defun pl-dcg-rule (rule)
  "Translate the rule Head --> Body into a clause term."
  (let* ((head (%deref (pl-arg rule 0))) (body (pl-arg rule 1)) (s0 (pl-new-var)) (s (pl-new-var)))
    (cond ((plv-p head) (pl-instantiation-error))
          ((pl-functor-p head *pl-comma* 2)
           (let* ((nt (%deref (pl-arg head 0))) (pushback (pl-arg head 1)) (mid (pl-new-var)))
             (unless (pl-callable-p nt) (pl-type-error "callable" nt))
             (%make-pls *pl-neck* (vector (%pl-add-args nt (list s0 s))
                                          (%make-pls *pl-comma* (vector (pl-dcg-body body s0 mid)
                                                                        (pl-dcg-body pushback s mid)))))))
          ((pl-callable-p head)
           (%make-pls *pl-neck* (vector (%pl-add-args head (list s0 s)) (pl-dcg-body body s0 s))))
          (t (pl-type-error "callable" head)))))
