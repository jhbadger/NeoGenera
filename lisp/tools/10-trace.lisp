;;;; 10-trace.lisp -- TRACE, STEP and the statistical profiler.

(in-package :cl)

;;; ---- trace -----------------------------------------------------------------------------------

(defvar *traced* nil)
(defvar *trace-depth* 0)

(defun %trace-wrapper (name orig)
  (lambda (&rest args)
    (let ((depth *trace-depth*))
      (format *standard-output* "~&~A~D: (~S~{ ~S~})~%" (make-string (* 2 (min depth 20)) #\Space) depth name args)
      (let ((vals (let ((*trace-depth* (+ depth 1))) (multiple-value-list (apply orig args)))))
        (format *standard-output* "~&~A~D: ~S returned~{ ~S~}~%" (make-string (* 2 (min depth 20)) #\Space) depth name vals)
        (values-list vals)))))

(defun trace-function (name)
  (unless (member name *traced*)
    (unless (fboundp name) (error "Cannot trace ~S: it has no function definition" name))
    (let ((orig (symbol-function name)))
      (%put name '%traced orig)
      (%set-symbol-function name (%trace-wrapper name orig))
      (setq *traced* (cons name *traced*))))
  name)

(defun untrace-function (name)
  (when (member name *traced*)
    (%set-symbol-function name (get name '%traced))
    (setq *traced* (remove name *traced*)))
  name)

(defmacro trace (&rest names)
  (if names `(list ,@(mapcar (lambda (n) `(trace-function ',n)) names)) '*traced*))
(defmacro untrace (&rest names)
  (if names `(list ,@(mapcar (lambda (n) `(untrace-function ',n)) names))
      `(mapcar #'untrace-function (copy-list *traced*))))

;;; ---- the stepper ---------------------------------------------------------------------------------
;;; A small evaluator written in Lisp that pauses at each function call.  Macros are expanded
;;; transparently.  Functions defined while *RECORD-LAMBDA-EXPRESSIONS* is true (the Listener sets
;;; it) keep their source and can be stepped INTO; all other functions are called as a unit.

(defvar *step-handler* nil)     ; (lambda (kind form values)) -> :step :next :continue :quit
(defvar *stepping* t)

(defun %ll-all-vars (ll)
  (let ((acc nil))
    (dolist (x ll)
      (cond ((member x '(&optional &rest &key &aux &body &allow-other-keys)) nil)
            ((symbolp x) (setq acc (cons x acc)))
            ((consp x) (let ((v (car x)))
                         (setq acc (cons (if (consp v) (cadr v) v) acc))
                         (when (and (cddr x) (caddr x)) (setq acc (cons (caddr x) acc)))))))
    (nreverse acc)))

(defun %sv-lookup (sym env) (assq sym env))
(defun %sf-lookup (name env)
  (dolist (b env nil) (when (and (consp (car b)) (eq (caar b) :f) (eq (cdar b) name)) (return b))))

(defun %step-body (forms env)
  (let ((vals (list nil)))
    (dolist (f forms) (setq vals (multiple-value-list (%step-form f env))))
    (values-list vals)))

(defun %step-bind (syms vals thunk)
  "Call THUNK with SYMS bound dynamically to VALS."
  (%progv syms vals thunk))

(defun %step-lambda-function (ll body env)
  "A real function that runs BODY under the stepper with LL bound."
  (let ((vars (%ll-all-vars ll))
        (binder (compile nil (list 'lambda ll (cons 'list (%ll-all-vars ll))))))
    (lambda (&rest args)
      (let* ((vals (apply binder args)) (new env) (specials nil) (spvals nil))
        (do ((vs vars (cdr vs)) (xs vals (cdr xs))) ((null vs))
          (if (%special-p (car vs))
              (progn (setq specials (cons (car vs) specials)) (setq spvals (cons (car xs) spvals)))
              (setq new (cons (cons (car vs) (car xs)) new))))
        (if specials
            (%step-bind specials spvals (lambda () (%step-body body new)))
            (%step-body body new))))))

(defun %step-call (name fn args form)
  (let ((handler *step-handler*) (decision :next))
    (when (and *stepping* handler)
      (setq decision (funcall handler :call form args)))
    (when (eq decision :quit) (throw '%step-quit nil))
    (when (eq decision :continue) (setq *stepping* nil) (setq decision :next))
    (let ((vals (multiple-value-list
                 (let ((lexpr (and (symbolp name) name (get name '%lambda-expr))))
                   (cond ((and (eq decision :step) lexpr (eq fn (symbol-function name)))
                          (apply (%step-lambda-function (cadr lexpr) (cddr lexpr) nil) args))
                         ((eq decision :next) (let ((*stepping* nil)) (apply fn args)))
                         (t (apply fn args)))))))
      (when (and *stepping* handler) (funcall handler :value form vals))
      (values-list vals))))

(defun %step-form (form env)
  (cond
    ((symbolp form)
     (let ((b (%sv-lookup form env)))
       (cond (b (cdr b)) ((or (null form) (eq form t) (keywordp form)) form) (t (symbol-value form)))))
    ((atom form) form)
    (t
     (let ((head (car form)))
       (cond
         ((eq head 'quote) (cadr form))
         ((eq head 'function)
          (let ((f (cadr form)))
            (cond ((and (consp f) (eq (car f) 'lambda)) (%step-lambda-function (cadr f) (cddr f) env))
                  ((%sf-lookup f env) (cdr (%sf-lookup f env)))
                  (t (symbol-function f)))))
         ((eq head 'lambda) (%step-form (list 'function form) env))
         ((eq head 'if) (if (%step-form (cadr form) env) (%step-form (caddr form) env) (%step-form (cadddr form) env)))
         ((or (eq head 'progn) (eq head 'locally)) (%step-body (cdr form) env))
         ((or (eq head 'the)) (%step-form (caddr form) env))
         ((eq head 'declare) nil)
         ((eq head 'setq)
          (let ((r nil) (l (cdr form)))
            (loop (when (null l) (return))
                  (setq r (%step-form (cadr l) env))
                  (let ((b (%sv-lookup (car l) env))) (if b (rplacd b r) (set (car l) r)))
                  (setq l (cddr l)))
            r))
         ((or (eq head 'let) (eq head 'let*))
          (let ((new env) (specials nil) (spvals nil))
            (dolist (b (cadr form))
              (let* ((sym (if (consp b) (car b) b)) (v (if (and (consp b) (cdr b)) (%step-form (cadr b) (if (eq head 'let*) new env)) nil)))
                (if (%special-p sym)
                    (progn (setq specials (cons sym specials)) (setq spvals (cons v spvals)))
                    (setq new (cons (cons sym v) new)))))
            (if specials
                (%step-bind specials spvals (lambda () (%step-body (cddr form) new)))
                (%step-body (cddr form) new))))
         ((eq head 'block)
          (let* ((tag (list nil)) (new (cons (cons (cons :b (cadr form)) tag) env)))
            (catch tag (%step-body (cddr form) new))))
         ((eq head 'return-from)
          (let ((b (dolist (e env nil) (when (and (consp (car e)) (eq (caar e) :b) (eq (cdar e) (cadr form))) (return e)))))
            (unless b (error "RETURN-FROM: no block named ~S" (cadr form)))
            (throw (cdr b) (%step-form (caddr form) env))))
         ((eq head 'tagbody)
          (let* ((tag (list nil)) (items (cdr form)) (new env))
            (dolist (it items) (unless (consp it) (setq new (cons (cons (cons :t it) (cons tag items)) new))))
            (let ((rest items))
              (loop
                (let ((jump (catch tag
                              (dolist (it rest) (when (consp it) (%step-form it new)))
                              :done)))
                  (if (eq jump :done) (return nil) (setq rest jump)))))))
         ((eq head 'go)
          (let ((b (dolist (e env nil) (when (and (consp (car e)) (eq (caar e) :t) (eql (cdar e) (cadr form))) (return e)))))
            (unless b (error "GO: no tag named ~S" (cadr form)))
            (throw (cadr b) (member (cadr form) (cddr b)))))
         ((eq head 'catch) (let ((tag (%step-form (cadr form) env))) (catch tag (%step-body (cddr form) env))))
         ((eq head 'unwind-protect)
          (unwind-protect (%step-form (cadr form) env) (%step-body (cddr form) env)))
         ((or (eq head 'flet) (eq head 'labels))
          (let ((new env))
            (dolist (b (cadr form))
              (setq new (cons (cons (cons :f (car b)) nil) new)))
            (dolist (b (cadr form))
              (let ((fn (%step-lambda-function (cadr b) (cddr b) (if (eq head 'labels) new env))))
                (rplacd (%sf-lookup (car b) new) fn)))
            (%step-body (cddr form) new)))
         ((eq head 'multiple-value-call)
          (apply (%step-form (cadr form) env)
                 (let ((acc nil)) (dolist (f (cddr form)) (setq acc (append acc (multiple-value-list (%step-form f env))))) acc)))
         ((eq head 'funcall)
          (let ((fn (%step-form (cadr form) env)) (args (mapcar (lambda (a) (%step-form a env)) (cddr form))))
            (%step-call (if (symbolp fn) fn nil) (if (symbolp fn) (symbol-function fn) fn) args form)))
         ((and (symbolp head) (macro-function head)) (%step-form (macroexpand-1 form) env))
         ((and (symbolp head) (%sf-lookup head env))
          (%step-call nil (cdr (%sf-lookup head env)) (mapcar (lambda (a) (%step-form a env)) (cdr form)) form))
         ((symbolp head)
          (let ((args (mapcar (lambda (a) (%step-form a env)) (cdr form))))
            (%step-call head (symbol-function head) args form)))
         (t (error "Cannot step ~S" form)))))))

(defun step-form (form handler)
  "Evaluate FORM under the stepper; HANDLER decides at each call.  Returns the values of FORM."
  (let ((*step-handler* handler) (*stepping* t) (result nil) (completed nil))
    (catch '%step-quit
      (setq result (multiple-value-list (%step-form form nil)))
      (setq completed t))
    (if completed (values-list result) (values))))

(defun %interactive-step-handler (kind form data)
  (if (eq kind :call)
      (progn
        (format *standard-output* "~&Step: ~S~%" (cons (car form) data))
        (format *standard-output* "  [s]tep into  [n]ext (step over)  [c]ontinue  [q]uit > ")
        (finish-output)
        (let ((line (read-line *standard-input* nil "c")))
          (cond ((member line '("" "s") :test #'string=) :step)
                ((string= line "n") :next) ((string= line "q") :quit) (t :continue))))
      (progn (format *standard-output* "~&  => ~{~S~^, ~}~%" data) nil)))

(defun %trace-step-handler (kind form data)
  (if (eq kind :call)
      (progn (format *standard-output* "~&Step: ~S~%" (cons (car form) data)) :step)
      (progn (format *standard-output* "~&  => ~{~S~^, ~}~%" data) nil)))

(defmacro step (form) `(step-form ',form #'%interactive-step-handler))
(defun step-trace (form) (step-form form #'%trace-step-handler))

;;; ---- profiler ---------------------------------------------------------------------------------------

(defvar *profile-samples* nil)

(defun profile-start (&optional (interval-us 2000)) (%profile-start interval-us) t)
(defun profile-stop () (setq *profile-samples* (%profile-stop)) (length *profile-samples*))

(defmacro with-profiling ((&key (interval 2000)) &body body)
  `(progn (profile-start ,interval)
          (unwind-protect (progn ,@body) (profile-stop))))

(defun profile-data ()
  "Returns a list of (name self cumulative), busiest first."
  (let ((self (make-hash-table :test 'eq)) (cum (make-hash-table :test 'eq)) (names nil))
    (dolist (sample *profile-samples*)
      (let ((top (car sample)))
        (setf (gethash top self) (+ 1 (gethash top self 0)))
        (unless (member top names) (setq names (cons top names))))
      (let ((seen nil))
        (dolist (n sample)
          (unless (member n seen)
            (setq seen (cons n seen))
            (setf (gethash n cum) (+ 1 (gethash n cum 0)))
            (unless (member n names) (setq names (cons n names)))))))
    (sort (mapcar (lambda (n) (list n (gethash n self 0) (gethash n cum 0))) names)
          (lambda (a b) (or (> (cadr a) (cadr b)) (and (= (cadr a) (cadr b)) (> (caddr a) (caddr b))))))))

(defun profile-report (&optional (stream *standard-output*) (limit 15))
  (let ((total (length *profile-samples*)) (i 0))
    (format stream "~&~D samples (compiled frames only)~%    self    cum  function~%" total)
    (dolist (row (profile-data))
      (when (>= i limit) (return))
      (format stream "~7D ~6D  ~A~%" (cadr row) (caddr row) (if (car row) (prin1-to-string (car row)) "(lambda)"))
      (setq i (+ i 1)))
    (values)))
