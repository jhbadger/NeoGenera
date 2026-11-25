;;;; 54-conditions.lisp -- the condition system: conditions, handlers, restarts.
;;;;
;;;; Conditions are CLOS instances.  HANDLER-BIND handlers run at the point of
;;;; the SIGNAL (the stack is intact, so debuggers and restarts work); HANDLER-CASE
;;;; and RESTART-CASE unwind with CATCH/THROW.  Errors raised by the C kernel
;;;; reach Lisp through %SIGNAL-ERROR, which classifies the message into one of the
;;;; standard condition types.

(in-package :cl)

(defvar *handler-clusters* nil)
(defvar *restart-clusters* nil)
(defvar *debugger-hook* nil)
(defvar *signal-depth* 0)

(defclass condition () ())

(defmacro define-condition (name parents slots &rest options)
  (let ((report (cadr (assoc :report options)))
        (di (assoc :default-initargs options)))
    `(progn
       (defclass ,name ,(or parents '(condition)) ,slots ,@(if di (list di) nil))
       ,@(if report
             (list `(%put ',name '%report
                          ,(if (stringp report)
                               `(lambda (c s) (declare (ignore c)) (write-string ,report s))
                               `(function ,report))))
             nil)
       ',name)))

(defun make-condition (type &rest initargs) (apply #'make-instance type initargs))

(define-condition serious-condition (condition) ())
(define-condition error (serious-condition) ())
(define-condition warning (condition) ())
(define-condition style-warning (warning) ())
(define-condition storage-condition (serious-condition) ())
(define-condition simple-condition (condition)
  ((format-control :initarg :format-control :initform "" :reader simple-condition-format-control)
   (format-arguments :initarg :format-arguments :initform nil :reader simple-condition-format-arguments))
  (:report (lambda (c s)
             (apply #'format s (simple-condition-format-control c) (simple-condition-format-arguments c)))))
(define-condition simple-error (simple-condition error) ())
(define-condition simple-warning (simple-condition warning) ())
(define-condition type-error (error)
  ((datum :initarg :datum :initform nil :reader type-error-datum)
   (expected-type :initarg :expected-type :initform t :reader type-error-expected-type))
  (:report (lambda (c s) (format s "The value ~S is not of type ~S" (type-error-datum c) (type-error-expected-type c)))))
(define-condition simple-type-error (simple-error type-error) ())
(define-condition arithmetic-error (error)
  ((operation :initarg :operation :initform nil :reader arithmetic-error-operation)
   (operands :initarg :operands :initform nil :reader arithmetic-error-operands)))
(define-condition division-by-zero (arithmetic-error) ())
(define-condition cell-error (error) ((name :initarg :name :initform nil :reader cell-error-name)))
(define-condition unbound-variable (cell-error) ()
  (:report (lambda (c s) (format s "The variable ~S is unbound" (cell-error-name c)))))
(define-condition undefined-function (cell-error) ()
  (:report (lambda (c s) (format s "The function ~S is undefined" (cell-error-name c)))))
(define-condition unbound-slot (cell-error) ((instance :initarg :instance :initform nil :reader unbound-slot-instance)))
(define-condition program-error (error) ())
(define-condition control-error (error) ())
(define-condition package-error (error) ((package :initarg :package :initform nil :reader package-error-package)))
(define-condition stream-error (error) ((stream :initarg :stream :initform nil :reader stream-error-stream)))
(define-condition end-of-file (stream-error) ())
(define-condition file-error (error) ((pathname :initarg :pathname :initform nil :reader file-error-pathname)))
(define-condition parse-error (error) ())
(define-condition reader-error (parse-error stream-error) ())

;; classes for errors that originate in the C kernel: the kernel's message is the report
(defun %message-report (c s)
  (apply #'format s (simple-condition-format-control c) (simple-condition-format-arguments c)))
(define-condition %unbound-variable (unbound-variable simple-error) () (:report %message-report))
(define-condition %undefined-function (undefined-function simple-error) () (:report %message-report))
(define-condition %division-by-zero (division-by-zero simple-error) () (:report %message-report))
(define-condition %program-error (program-error simple-error) () (:report %message-report))
(define-condition %reader-error (reader-error simple-error) () (:report %message-report))
(define-condition %end-of-file (end-of-file simple-error) () (:report %message-report))
(define-condition %file-error (file-error simple-error) () (:report %message-report))

(defun condition-report-string (c)
  (let ((report nil))
    (dolist (cl (%cls-cpl (class-of c)))
      (let ((f (get (%cls-name cl) '%report)))
        (when f (setq report f) (return))))
    (if report
        (let ((s (make-string-output-stream))) (funcall report c s) (get-output-stream-string s))
        (format nil "Condition of type ~A" (symbol-name (%cls-name (class-of c)))))))

(defmethod print-object ((c condition) stream)
  (if *print-escape*
      (format stream "#<~A ~S>" (symbol-name (%cls-name (class-of c))) (condition-report-string c))
      (write-string (condition-report-string c) stream)))

(defun conditionp (x) (typep x 'condition))

(defun %coerce-condition (datum args default-class)
  (cond ((typep datum 'condition) datum)
        ((stringp datum) (make-condition default-class :format-control datum :format-arguments args))
        ((symbolp datum) (apply #'make-condition datum args))
        (t (error "~S is not a condition designator" datum))))

;;; ---- signalling -----------------------------------------------------------------------

(defun signal (datum &rest args)
  (let ((c (%coerce-condition datum args 'simple-condition))
        (clusters *handler-clusters*))
    (block nil
      (tagbody top
         (when (null clusters) (return))
         (let ((cluster (car clusters)) (*handler-clusters* (cdr clusters)))
           (dolist (h cluster)
             (when (typep c (car h)) (funcall (cdr h) c))))
         (setq clusters (cdr clusters))
         (go top)))
    nil))

(defun invoke-debugger (c)
  (let ((hook *debugger-hook*))
    (when hook (funcall hook c hook))
    (%error-to-top (condition-report-string c))))

(defun error (datum &rest args)
  (let ((c (%coerce-condition datum args 'simple-error)))
    (signal c)
    (invoke-debugger c)))

(defmacro handler-bind (bindings &body body)
  `(let ((*handler-clusters*
          (cons (list ,@(mapcar (lambda (b) `(cons ',(car b) ,(cadr b))) bindings)) *handler-clusters*)))
     ,@body))

(defun %handler-case (thunk clauses no-error)
  (let* ((tag (list nil)) (ok (list nil))
         (cluster (let ((i -1))
                    (mapcar (lambda (cl)
                              (setq i (+ i 1))
                              (let ((idx i)) (cons (car cl) (lambda (c) (throw tag (cons idx c))))))
                            clauses)))
         (r (catch tag
              (let ((*handler-clusters* (cons cluster *handler-clusters*)))
                (let ((vals (multiple-value-list (funcall thunk))))
                  (cons ok vals))))))
    (if (eq (car r) ok)
        (if no-error (apply no-error (cdr r)) (values-list (cdr r)))
        (funcall (cdr (nth (car r) clauses)) (cdr r)))))

(defmacro handler-case (form &rest clauses)
  (let* ((no-error (assoc :no-error clauses))
         (cls (remove-if (lambda (c) (eq (car c) :no-error)) clauses)))
    (if (null cls)
        (if no-error `(multiple-value-call (lambda ,(cadr no-error) ,@(cddr no-error)) ,form) form)
        `(%handler-case (lambda () ,form)
                        (list ,@(mapcar (lambda (c) `(cons ',(car c) (lambda ,(or (cadr c) '(%c)) ,@(cddr c)))) cls))
                        ,(if no-error `(lambda ,(cadr no-error) ,@(cddr no-error)) nil)))))

(defmacro ignore-errors (&rest forms)
  (let ((e (gensym)))
    `(handler-case (progn ,@forms) (error (,e) (values nil ,e)))))

;;; ---- restarts -------------------------------------------------------------------------

(defstruct (restart (:conc-name restart-) (:constructor %make-restart) (:copier nil))
  name fn test report)

(defun compute-restarts (&optional condition)
  (let ((acc nil))
    (dolist (cluster *restart-clusters*)
      (dolist (r cluster)
        (when (or (null (restart-test r)) (funcall (restart-test r) condition))
          (setq acc (cons r acc)))))
    (nreverse acc)))

(defun find-restart (name &optional condition)
  (if (restart-p name)
      name
      (dolist (r (compute-restarts condition) nil)
        (when (eq (restart-name r) name) (return r)))))

(defun invoke-restart (restart &rest args)
  (let ((r (if (restart-p restart)
               restart
               (or (find-restart restart) (error 'control-error :format-control "No restart named ~S is active" :format-arguments (list restart))))))
    (apply (restart-fn r) args)))

(defun restart-report-string (r)
  (if (restart-report r)
      (let ((s (make-string-output-stream)))
        (if (stringp (restart-report r)) (write-string (restart-report r) s) (funcall (restart-report r) s))
        (get-output-stream-string s))
      (symbol-name (restart-name r))))

(defmethod print-object ((r restart) stream)
  (if *print-escape*
      (format stream "#<RESTART ~A>" (symbol-name (restart-name r)))
      (write-string (restart-report-string r) stream)))

(defmacro restart-bind (bindings &body body)
  `(let ((*restart-clusters*
          (cons (list ,@(mapcar (lambda (b)
                                  `(%make-restart :name ',(car b) :fn ,(cadr b)
                                                  :report ,(getf (cddr b) :report-function)
                                                  :test ,(getf (cddr b) :test-function)))
                                bindings))
                *restart-clusters*)))
     ,@body))

(defun %restart-case (thunk specs)
  ;; SPECS: list of (name function report test)
  (let* ((tag (list nil)) (ok (list nil))
         (restarts (let ((i -1))
                     (mapcar (lambda (sp)
                               (setq i (+ i 1))
                               (let ((idx i))
                                 (%make-restart :name (car sp) :report (caddr sp) :test (cadddr sp)
                                                :fn (lambda (&rest args) (throw tag (cons idx args))))))
                             specs)))
         (r (catch tag
              (let ((*restart-clusters* (cons restarts *restart-clusters*)))
                (let ((vals (multiple-value-list (funcall thunk))))
                  (cons ok vals))))))
    (if (eq (car r) ok)
        (values-list (cdr r))
        (apply (cadr (nth (car r) specs)) (cdr r)))))

(defmacro restart-case (form &rest clauses)
  `(%restart-case
    (lambda () ,form)
    (list ,@(mapcar
             (lambda (cl)
               (let ((rest (cddr cl)) (report nil) (test nil))
                 (block nil
                   (tagbody top
                      (when (and (consp rest) (keywordp (car rest)) (cdr rest))
                        (cond ((eq (car rest) :report) (setq report (cadr rest)))
                              ((eq (car rest) :test) (setq test (cadr rest))))
                        (setq rest (cddr rest))
                        (go top))))
                 `(list ',(car cl) (lambda ,(cadr cl) ,@rest)
                        ,(if (stringp report) report (if report `(function ,report) nil))
                        ,(if test `(function ,test) nil))))
             clauses))))

(defmacro with-simple-restart ((name fmt &rest args) &body body)
  `(restart-case (progn ,@body)
     (,name () :report (lambda (s) (format s ,fmt ,@args)) (values nil t))))

(defun abort (&optional condition)
  (let ((r (find-restart 'abort condition)))
    (if r (invoke-restart r) (error 'control-error :format-control "No ABORT restart is active"))))
(defun continue (&optional condition)
  (let ((r (find-restart 'continue condition))) (if r (invoke-restart r) nil)))
(defun muffle-warning (&optional condition)
  (let ((r (find-restart 'muffle-warning condition)))
    (if r (invoke-restart r) (error 'control-error :format-control "No MUFFLE-WARNING restart is active"))))
(defun use-value (value &optional condition)
  (let ((r (find-restart 'use-value condition))) (if r (invoke-restart r value) nil)))
(defun store-value (value &optional condition)
  (let ((r (find-restart 'store-value condition))) (if r (invoke-restart r value) nil)))

(defun cerror (continue-string datum &rest args)
  (with-simple-restart (continue "~A" continue-string)
    (apply #'error datum args))
  nil)

(defun warn (datum &rest args)
  (let ((c (%coerce-condition datum args 'simple-warning)))
    (with-simple-restart (muffle-warning "Muffle this warning")
      (signal c)
      (format *error-output* "~&WARNING: ~A~%" (condition-report-string c)))
    nil))

(defun %check-type-failed (value type)
  (restart-case (error 'simple-type-error :datum value :expected-type type
                       :format-control "The value ~S is not of type ~S" :format-arguments (list value type))
    (store-value (v) :report "Supply a new value of the correct type" v)))

(defmacro check-type (place type &optional description)
  (declare (ignore description))
  `(unless (typep ,place ',type)
     (setf ,place (%check-type-failed ,place ',type))))

;;; ---- errors raised by the C kernel ----------------------------------------------------------

(defun %msg-has (msg text) (if (%string-search text msg 0) t nil))

(defun %condition-class-for (msg)
  (cond ((and (%msg-has msg "variable") (%msg-has msg "is unbound")) '%unbound-variable)
        ((and (%msg-has msg "function") (%msg-has msg "is undefined")) '%undefined-function)
        ((%msg-has msg "Division by zero") '%division-by-zero)
        ((%msg-has msg "is not of type") 'simple-type-error)
        ((or (%msg-has msg "Wrong number of arguments") (%msg-has msg "Too few arguments")
             (%msg-has msg "Too many arguments") (%msg-has msg "Unknown &KEY") (%msg-has msg "Odd number of keyword"))
         '%program-error)
        ((%msg-has msg "End of file") '%end-of-file)
        ((%msg-has msg "READ error") '%reader-error)
        ((or (%msg-has msg "Cannot open") (%msg-has msg "File not found")) '%file-error)
        (t 'simple-error)))

(defun %name-in-message (msg prefix)
  (let ((p (%string-search prefix msg 0)))
    (when p
      (let* ((start (+ p (length prefix))) (end (%string-search " is " msg start)))
        (and end (subseq msg start end))))))

(defun %signal-error (msg)
  (let ((c (make-condition (%condition-class-for msg) :format-control "~A" :format-arguments (list msg))))
    (when (typep c 'cell-error)
      (setf (slot-value c 'name) (or (%name-in-message msg "The function ") (%name-in-message msg "The variable "))))
    (let ((*in-error-signal* nil) (*signal-depth* (+ *signal-depth* 1)))
      (when (> *signal-depth* 6) (%error-to-top msg))
      (signal c)
      (invoke-debugger c))))
