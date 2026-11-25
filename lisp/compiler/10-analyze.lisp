;;;; 10-analyze.lisp -- pass 1: macroexpansion, scope analysis, AST construction.
;;;;
;;;; AST nodes:
;;;;   (:const v) (:lref cvar) (:gref sym) (:gfun sym)
;;;;   (:lset cvar node) (:gset sym node)
;;;;   (:if test then else) (:progn node...)
;;;;   (:let bindings inits body)   bindings: cvar or symbol (dynamic binding)
;;;;   (:flet cvars clams body) (:labels cvars clams body)
;;;;   (:lambda clam) (:call fn-node arg-node...)
;;;;   (:block cblock body) (:return-from cblock node)
;;;;   (:tagbody ctb item...) item: node or ctag   (:go ctb ctag)
;;;;
;;;; BLOCK/TAGBODY are first analyzed optimistically as local jumps.  If a
;;;; RETURN-FROM or GO is found inside a nested LAMBDA (a closure, or one of the
;;;; thunks generated for CATCH, UNWIND-PROTECT, ...), analysis of that form is
;;;; abandoned and redone using dynamic frames (%BLOCK / a state machine).

(in-package :cl)

(defvar *lam* nil)

(defun macroexpand-1 (form &optional env)
  (declare (ignore env))
  (if (and (consp form) (symbolp (car form)))
      (let ((mf (macro-function (car form))))
        (if mf (values (funcall mf form nil) t) (values form nil)))
      (values form nil)))

(defun macroexpand (form &optional env)
  (let ((expanded nil))
    (block nil
      (tagbody top
         (multiple-value-bind (f e) (macroexpand-1 form env)
           (when e (setq form f) (setq expanded t) (go top)))))
    (values form expanded)))

;;; ---- variables -----------------------------------------------------------

(defun note-var-use (var)
  (let ((owner (cvar-lam var)))
    (unless (eq owner *lam*)
      (setf (cvar-captured var) t)
      (let ((l *lam*))
        (block nil
          (tagbody top
             (when (or (null l) (eq l owner)) (return))
             (unless (memq var (clam-free l))
               (setf (clam-free l) (append (clam-free l) (list var))))
             (setq l (clam-parent l))
             (go top)))))))

(defun an-lref (var) (note-var-use var) (list :lref var))

(defun an-symbol (sym env)
  (let ((b (assq sym (aenv-vars env))))
    (cond (b (if (cvar-p (cdr b)) (an-lref (cdr b)) (an (cadr (cdr b)) env)))
          ((or (null sym) (eq sym t) (keywordp sym)) (list :const sym))
          ((and (%constant-p sym) (boundp sym)) (list :const (symbol-value sym)))
          (t (list :gref sym)))))

(defun special-var-p (sym env)
  (or (%special-p sym) (memq sym (aenv-specials env))))

(defun split-declarations (body)
  ;; returns (values specials-declared forms)
  (let ((specials nil))
    (block nil
      (tagbody top
         (when (and (consp body) (consp (car body)) (eq (caar body) 'declare))
           (dolist (d (cdar body))
             (when (and (consp d) (eq (car d) 'special))
               (setq specials (append specials (cdr d)))))
           (setq body (cdr body))
           (go top))))
    (values specials body)))

(defun an-body (forms env)
  (let ((nodes (mapcar (lambda (f) (an f env)) forms)))
    (cond ((null nodes) (list :const nil))
          ((null (cdr nodes)) (car nodes))
          (t (cons :progn nodes)))))

;;; ---- main dispatch ---------------------------------------------------------

(defun lookup-fun (name env) (cdr (assq name (aenv-funs env))))

(defun an (form env)
  (cond ((symbolp form) (an-symbol form env))
        ((not (consp form)) (list :const form))
        (t (let ((head (car form)))
             (cond ((and (consp head) (eq (car head) 'lambda))
                    (an (cons 'funcall (cons (list 'function head) (cdr form))) env))
                   ((not (symbolp head)) (error "Illegal function call: ~S" form))
                   (t (let ((local (lookup-fun head env)))
                        (cond ((and local (eq (car local) :macro)) (an (funcall (cadr local) form nil) env))
                              (local (list* :call (an-lref (cadr local)) (an-args (cdr form) env)))
                              (t (an-global head form env))))))))))

(defun an-args (args env) (mapcar (lambda (a) (an a env)) args))

(defun an-global (head form env)
  (case head
    (quote (list :const (cadr form)))
    (function (an-function (cadr form) env))
    (if (list :if (an (cadr form) env) (an (caddr form) env)
              (if (cdddr form) (an (cadddr form) env) (list :const nil))))
    (progn (an-body (cdr form) env))
    (locally (an-body (cdr form) env))
    (the (an (caddr form) env))
    (setq (an-setq (cdr form) env))
    (let (an-let (cadr form) (cddr form) env nil))
    (let* (an-let-star (cadr form) (cddr form) env))
    (flet (an-flet (cadr form) (cddr form) env nil))
    (labels (an-flet (cadr form) (cddr form) env t))
    (macrolet (an-macrolet (cadr form) (cddr form) env))
    (symbol-macrolet
     (let ((env2 (copy-aenv env)))
       (dolist (b (cadr form))
         (setf (aenv-vars env2) (acons (car b) (list :symmac (cadr b)) (aenv-vars env2))))
       (an-body (cddr form) env2)))
    (block (an-block (cadr form) (cddr form) env))
    (return-from (an-return-from (cadr form) (caddr form) (cdddr form) env))
    (tagbody (an-tagbody (cdr form) env))
    (go (an-go (cadr form) env))
    (lambda (an-function form env))
    (catch (an (list '%catch (cadr form) (list* 'lambda nil (cddr form))) env))
    (unwind-protect
     (an (list '%unwind-protect (list 'lambda nil (cadr form)) (list* 'lambda nil (cddr form))) env))
    (multiple-value-call
     (an (list* '%multiple-value-call (cadr form)
                (mapcar (lambda (f) (list 'lambda nil f)) (cddr form)))
         env))
    (multiple-value-prog1
     (let ((g (gensym "MV")))
       (an (list 'let (list (list g (list 'multiple-value-list (cadr form))))
                 (cons 'progn (cddr form))
                 (list 'values-list g))
           env)))
    (progv (an (list '%progv (cadr form) (caddr form) (list* 'lambda nil (cdddr form))) env))
    (eval-when (if (or (memq :execute (cadr form)) (memq :load-toplevel (cadr form)))
                   (an-body (cddr form) env)
                   (list :const nil)))
    (load-time-value (list :const (eval (copy-tree (cadr form)))))
    (declare (list :const nil))
    (funcall (if (cdr form)
                 (list* :call (an (cadr form) env) (an-args (cddr form) env))
                 (error "FUNCALL: too few arguments")))
    (%unsupplied-p (list :call (list :gfun 'eq) (an (cadr form) env) (list :const (%unsupplied))))
    (t (let ((mf (macro-function head)))
         (cond (mf (an (funcall mf form nil) env))
               (t (list* :call (list :gfun (%fn-symbol head)) (an-args (cdr form) env))))))))

;;; ---- functions ----------------------------------------------------------------

(defun an-function (spec env)
  (cond ((and (consp spec) (eq (car spec) 'lambda))
         (list :lambda (an-lambda nil (cadr spec) (cddr spec) env)))
        (t (let* ((name (%fn-symbol spec)) (local (lookup-fun name env)))
             (cond ((and local (eq (car local) :var)) (an-lref (cadr local)))
                   (local (error "~S names a local macro, not a function" name))
                   (t (list :gfun name)))))))

(defun lambda-list-parts (ll)
  ;; returns (values required optionals rest keys aux allow-other-keys)
  (let ((mode :req) (req nil) (opt nil) (rest nil) (keys nil) (aux nil) (allow nil))
    (dolist (x ll)
      (cond ((eq x '&optional) (setq mode :opt))
            ((or (eq x '&rest) (eq x '&body)) (setq mode :rest))
            ((eq x '&key) (setq mode :key))
            ((eq x '&aux) (setq mode :aux))
            ((eq x '&allow-other-keys) (setq allow t))
            ((eq mode :req) (setq req (cons x req)))
            ((eq mode :opt) (setq opt (cons x opt)))
            ((eq mode :rest) (setq rest x))
            ((eq mode :key) (setq keys (cons x keys)))
            ((eq mode :aux) (setq aux (cons x aux)))))
    (values (nreverse req) (nreverse opt) rest (nreverse keys) (nreverse aux) allow)))

(defun %check-keys (rest allowed allow-other)
  (unless allow-other
    (do-list-tails (l rest)
      (let ((k (car l)))
        (unless (or (memq k allowed) (eq k :allow-other-keys) (null (cdr l)))
          (error "Unknown &KEY argument: ~S" k))
        (when (null (cdr l)) (error "Odd number of keyword arguments")))
      (setq l (cdr l)))))

(defun expand-lambda-list (ll body)
  ;; Returns (values params nreq nopt restp wrapped-body).  Optional/keyword/aux
  ;; processing is expressed as source forms around BODY.
  (multiple-value-bind (req opt rest keys aux allow) (lambda-list-parts ll)
    (let* ((restsym (or rest (and keys (gensym "REST"))))
           (params (append req (mapcar (lambda (o) (if (consp o) (car o) o)) opt)
                           (if restsym (list restsym) nil)))
           (inner body))
      (multiple-value-bind (specials plain) (split-declarations body)
        (declare (ignore plain))
        (when (or aux keys opt)
          (setq inner (list (cons 'let (cons nil body)))))
        ;; aux (innermost)
        (when aux
          (setq inner (list (list* 'let* (mapcar (lambda (a) (if (consp a) (list (car a) (cadr a)) (list a nil))) aux)
                                  inner))))
        ;; keys
        (when keys
          (let ((bindings nil))
            (dolist (k keys)
              (let* ((spec (if (consp k) (car k) k))
                     (var (if (consp spec) (cadr spec) spec))
                     (kw (if (consp spec) (car spec) (intern (symbol-name var) "KEYWORD")))
                     (def (if (consp k) (cadr k) nil))
                     (svar (if (consp k) (caddr k) nil))
                     (probe (gensym "K")))
                (setq bindings (cons (list probe (list '%getf-key restsym (list 'quote kw))) bindings))
                (setq bindings (cons (list var (list 'if (list '%unsupplied-p probe) def probe)) bindings))
                (when svar (setq bindings (cons (list svar (list 'not (list '%unsupplied-p probe))) bindings)))))
            (setq inner (list (list* 'let* (nreverse bindings) inner))))
          (setq inner (cons (list '%check-keys restsym
                                  (list 'quote (mapcar (lambda (k)
                                                         (let ((spec (if (consp k) (car k) k)))
                                                           (if (consp spec) (car spec) (intern (symbol-name spec) "KEYWORD"))))
                                                       keys))
                                  allow)
                            inner)))
        ;; optionals, outermost-first wrapping order = reverse
        (dolist (o (reverse opt))
          (let* ((var (if (consp o) (car o) o))
                 (def (if (consp o) (cadr o) nil))
                 (svar (if (consp o) (caddr o) nil))
                 (fix (list 'when (list '%unsupplied-p var) (list 'setq var def))))
            (setq inner
                  (if svar
                      (list (list* 'let (list (list svar (list 'not (list '%unsupplied-p var)))) fix inner))
                      (list (cons 'progn (cons fix inner)))))))
        (when (and specials (null inner)) nil))
      (values params (length req) (length opt) (if restsym t nil) inner))))

(defun %interned-tree-p (x)
  "True when X contains no uninterned symbols (so it can be written to a fasl deterministically)."
  (cond ((null x) t)
        ((consp x) (and (%interned-tree-p (car x)) (%interned-tree-p (cdr x))))
        ((symbolp x) (if (symbol-package x) t nil))
        (t (or (numberp x) (stringp x) (characterp x)))))

(defun an-lambda (name ll body env)
  (let* ((lam (make-clam name *lam*)))
    (multiple-value-bind (params nreq nopt restp wrapped) (expand-lambda-list ll body)
      (let* ((*lam* lam) (env2 (copy-aenv env)) (cvars nil))
        (dolist (p params)
          (let ((v (make-cvar p lam)))
            (setq cvars (append cvars (list v)))
            (setf (aenv-vars env2) (acons p v (aenv-vars env2)))))
        (setf (clam-params lam) cvars (clam-nreq lam) nreq (clam-nopt lam) nopt (clam-rest lam) restp)
        (setf (clam-arglist lam) (and (%interned-tree-p ll) ll))
        (setf (clam-body lam) (an-body wrapped env2))
        lam))))

(defun an-flet (specs body env labels-p)
  (let* ((env2 (copy-aenv env)) (vars nil))
    (dolist (s specs)
      (let ((v (make-cvar (car s) *lam*)))
        (when labels-p (setf (cvar-mutated v) t))
        (setq vars (append vars (list v)))
        (setf (aenv-funs env2) (acons (%fn-symbol (car s)) (list :var v) (aenv-funs env2)))))
    (let ((lams (mapcar (lambda (s)
                          (multiple-value-bind (decls doc forms) (%split-body (cddr s))
                            (declare (ignore doc))
                            (an-lambda (car s) (cadr s)
                                       (append decls
                                               (list (list* 'block (if (consp (car s)) (cadr (car s)) (car s)) forms)))
                                       (if labels-p env2 env))))
                        specs)))
      (list (if labels-p :labels :flet) vars lams (an-body body env2)))))

(defun an-macrolet (specs body env)
  (let ((env2 (copy-aenv env)))
    (dolist (s specs)
      (let ((fn (eval (copy-tree (list 'lambda '(%whole %env)
                                       '(declare (ignore %env))
                                       (list* 'destructuring-bind (cadr s) '(cdr %whole) (cddr s)))))))
        (setf (aenv-funs env2) (acons (car s) (list :macro fn) (aenv-funs env2)))))
    (an-body body env2)))

;;; ---- binding forms ---------------------------------------------------------------

(defun an-let (bindings body env star-inner)
  (declare (ignore star-inner))
  (multiple-value-bind (specials forms) (split-declarations body)
    (let ((env2 (copy-aenv env)) (items nil) (inits nil))
      (setf (aenv-specials env2) (append specials (aenv-specials env2)))
      (dolist (b bindings)
        (let* ((sym (if (consp b) (car b) b))
               (init (if (and (consp b) (cdr b)) (an (cadr b) env) (list :const nil))))
          (setq inits (append inits (list init)))
          (if (special-var-p sym env2)
              (setq items (append items (list sym)))
              (let ((v (make-cvar sym *lam*)))
                (setq items (append items (list v)))
                (setf (aenv-vars env2) (acons sym v (aenv-vars env2)))))))
      ;; shadowing: later bindings of the same name hide earlier ones (alist order)
      (list :let items inits (an-body forms env2)))))

(defun an-let-star (bindings body env)
  (if (or (null bindings) (null (cdr bindings)))
      (an-let bindings body env nil)
      (an-let (list (car bindings)) (list (list* 'let* (cdr bindings) body)) env nil)))

(defun an-setq (pairs env)
  (cond ((null pairs) (list :const nil))
        ((null (cddr pairs)) (an-setq-1 (car pairs) (cadr pairs) env))
        (t (let ((nodes nil))
             (do-list-tails (p pairs)
               (setq nodes (cons (an-setq-1 (car p) (cadr p) env) nodes))
               (setq p (cdr p)))
             (cons :progn (nreverse nodes))))))

(defun an-setq-1 (sym value env)
  (let ((b (assq sym (aenv-vars env))))
    (cond ((and b (not (cvar-p (cdr b))))
           (an (list 'setf (cadr (cdr b)) value) env))
          (b (let ((var (cdr b)) (node (an value env)))
               (note-var-use var)
               (setf (cvar-mutated var) t)
               (list :lset var node)))
          (t (list :gset sym (an value env))))))

;;; ---- BLOCK / RETURN-FROM -----------------------------------------------------------

(defun an-block (name body env)
  (let* ((blk (make-cblock name *lam*))
         (r (catch blk
              (let ((env2 (copy-aenv env)))
                (setf (aenv-blocks env2) (acons name blk (aenv-blocks env2)))
                (list :ok (list :block blk (an-body body env2)))))))
    (if (and (consp r) (eq (car r) :ok))
        (cadr r)
        (let* ((tok (gensym "BLK")) (env2 (copy-aenv env)))
          (setf (aenv-blocks env2) (acons name (list :token tok) (aenv-blocks env2)))
          (an (list '%block (list 'lambda (list tok) (cons 'progn body))) env2)))))

(defun an-return-from (name value rest env)
  (declare (ignore rest))
  (let ((entry (cdr (assq name (aenv-blocks env)))))
    (cond ((null entry) (error "RETURN-FROM: no block named ~S" name))
          ((cblock-p entry)
           (if (eq (cblock-lam entry) *lam*)
               (list :return-from entry (an value env))
               (throw entry :nonlocal)))
          (t (let ((tok (cadr entry)))
               (if (and (consp value) (eq (car value) 'values))
                   (an (list '%return-from-mv tok (list 'multiple-value-list value)) env)
                   (an (list '%return-from tok value) env)))))))

;;; ---- TAGBODY / GO --------------------------------------------------------------------

(defun an-tagbody (items env)
  (let* ((tb (make-ctb *lam*))
         (r (catch tb
              (let ((env2 (copy-aenv env)) (tags nil) (nodes nil))
                (dolist (it items)
                  (unless (consp it)
                    (let ((tag (make-ctag it)))
                      (setq tags (cons (cons it tag) tags))
                      (setf (aenv-tags env2) (acons it (cons tb tag) (aenv-tags env2))))))
                (dolist (it items)
                  (if (consp it)
                      (setq nodes (cons (an it env2) nodes))
                      (setq nodes (cons (cdr (assoc it tags)) nodes))))
                (list :ok (cons :tagbody (cons tb (nreverse nodes))))))))
    (if (and (consp r) (eq (car r) :ok))
        (cadr r)
        (tagbody-state-machine items env))))

(defun tagbody-state-machine (items env)
  (let* ((state (gensym "STATE")) (restart (gensym "RESTART")) (done (gensym "DONE")) (run (gensym "RUN"))
         (env2 (copy-aenv env))
         (segments nil) (cur nil) (idx 0))
    (dolist (it items)
      (if (consp it)
          (setq cur (cons it cur))
          (progn
            (setq segments (cons (nreverse cur) segments))
            (setq cur nil)
            (setq idx (+ idx 1))
            (setf (aenv-tags env2) (acons it (list :sm state restart idx) (aenv-tags env2))))))
    (setq segments (nreverse (cons (nreverse cur) segments)))
    (let ((i -1))
      (an (list 'let (list (list state 0))
                (list 'block done
                      (list 'labels
                            (list (list run nil
                                        (cons 'block
                                              (cons restart
                                                    (append (mapcar (lambda (seg)
                                                                      (setq i (+ i 1))
                                                                      (list* 'when (list '<= state i) seg))
                                                                    segments)
                                                            (list (list 'return-from done nil)))))
                                        (list run)))
                            (list run))))
          env2))))

(defun an-go (tag env)
  (let ((entry (cdr (assoc tag (aenv-tags env)))))
    (cond ((null entry) (error "GO: no tag named ~S" tag))
          ((eq (car entry) :sm)
           (let ((state (cadr entry)) (restart (caddr entry)) (idx (cadddr entry)))
             (an (list 'progn (list 'setq state idx) (list 'return-from restart nil)) env)))
          (t (let ((tb (car entry)) (ctag (cdr entry)))
               (if (eq (ctb-lam tb) *lam*)
                   (list :go tb ctag)
                   (throw tb :nonlocal)))))))
