;;;; 62-process.lisp -- processes, process-wait, locks (Genera-style multiprocessing).
;;;;
;;;; A PROCESS is a Lisp thread of control with its own stacks and special bindings (see src/proc.c).
;;;; Processes take turns: the running one is preempted at a safe point every quantum (20 ms) or gives up
;;;; the processor when it waits, sleeps or reads the terminal.  Global values are shared; special
;;;; bindings are private to the process that made them.
;;;;
;;;;   (process-run-function "Counter" (lambda () ...))   start a process
;;;;   (process-wait "Input" #'predicate args...)          block until the predicate is true
;;;;   (with-lock-held (lock) ...)                         mutual exclusion
;;;;   (process-kill p) (process-interrupt p fn) (process-arrest p) (process-unarrest p)
;;;;
;;;; The initial process (the Listener or the window system) is created by the kernel and named lazily.

(in-package :cl)

(defstruct (process (:constructor %make-process) (:copier nil))
  (name "Anonymous")
  function
  (args nil)
  (whostate "Run")
  (state :created)             ; :created :active :done :killed :error :aborted
  (result nil)
  (error nil)
  (backtrace nil)
  (interrupts nil)
  (output nil)
  (initial nil)
  (arrested nil)
  (created (get-universal-time))
  (finished nil))

(defmethod print-object ((p process) stream)
  (format stream "#<PROCESS ~A ~(~A~)>" (process-name p) (process-state-summary p)))

(defvar *current-process* nil
  "The process running this code.  Bound in every process; use CURRENT-PROCESS in the initial process.")
(defvar *initial-process* nil)
(defvar *recent-processes* nil "Processes that finished recently, newest first (kept for Peek).")
(defparameter *recent-process-limit* 20)
(defvar *process-error-hook* nil
  "When non-NIL, a function of (process condition) called when a process gets an error it does not handle.")

(defun current-process ()
  "The process running this code."
  (or (%process-current)
      (let ((p (%make-process :name "Initial Process" :state :active :initial t :whostate "Run")))
        (%process-set-current p)
        (setq *initial-process* p)
        p)))

(defun all-processes ()
  "The live processes, the initial process first."
  (current-process)
  (%process-list))

(defun process-active-p (process) "True while PROCESS has not finished." (%process-alive-p process))

(defun process-state-summary (p)
  "A short description: the whostate of a live process, else how it finished."
  (cond ((process-active-p p) (if (process-arrested p) "Arrested" (process-whostate p)))
        ((eq (process-state p) :created) "Created")
        (t (string-capitalize (symbol-name (process-state p))))))

(defun process-run-time (p)
  "Seconds of processor time PROCESS has used (time spent holding the interpreter), as a float."
  (let ((info (%process-info p))) (if info (/ (float (cadr info)) 1000000.0) 0.0)))

(defun process-id (p) "The number of a live PROCESS (as Peek shows it), or NIL." (car (%process-info p)))

(defun find-process (designator)
  "The live process whose name is (or starts with) the string DESIGNATOR, or whose id is the integer
DESIGNATOR (also accepted as a string of digits), or NIL."
  (let* ((n (if (integerp designator) designator (parse-integer (princ-to-string designator) :junk-allowed t)))
         (name (string-downcase (princ-to-string designator))) (procs (all-processes)))
    (or (and n (find n procs :key #'process-id))
        (find name procs :key (lambda (p) (string-downcase (process-name p))) :test #'string=)
        (find-if (lambda (p) (%starts-with (string-downcase (process-name p)) name)) procs))))

(defun show-processes (&optional (stream *standard-output*))
  "Print a table of the live processes and the recently finished ones."
  (format stream "~&    Id  ~24A ~22A ~9@A~%" "Name" "State" "Time")
  (dolist (p (append (all-processes) (remove-if #'process-active-p *recent-processes*)))
    (format stream "  ~4@A  ~24A ~22A ~9,2F~%" (or (process-id p) "") (process-name p) (process-state-summary p) (process-run-time p)))
  (values))

;;; ---- output of background processes --------------------------------------------------------------------

(defclass process-output-stream (fundamental-character-output-stream)
  ((chunks :initform nil)
   (size :initform 0)
   (column :initform 0)
   (limit :initform 20000 :initarg :limit)))
;; collects what a background process prints; PROCESS-OUTPUT-TEXT reads it back

(defun %pos-add (s string)
  (when (> (length string) 0)
    (without-interrupts
      (setf (slot-value s 'chunks) (cons string (slot-value s 'chunks)) (slot-value s 'size) (+ (slot-value s 'size) (length string)))
      (when (> (slot-value s 'size) (* 2 (slot-value s 'limit)))      ; keep about the last LIMIT characters
        (let ((text (%process-output-stream-text s)))
          (setf (slot-value s 'chunks) (list (subseq text (- (length text) (slot-value s 'limit)))) (slot-value s 'size) (slot-value s 'limit)))))
    (let ((nl (position #\Newline string :from-end t)))
      (setf (slot-value s 'column) (if nl (- (length string) nl 1) (+ (slot-value s 'column) (length string)))))))

(defmethod stream-write-char ((s process-output-stream) ch) (%pos-add s (string ch)) ch)
(defmethod stream-write-string ((s process-output-stream) string &optional start end)
  (%pos-add s (if (or start end) (subseq string (or start 0) end) string)) string)
(defmethod stream-line-column ((s process-output-stream)) (slot-value s 'column))

(defun %process-output-stream-text (s)
  (let ((text (apply #'concatenate 'string (reverse (slot-value s 'chunks)))))
    (setf (slot-value s 'chunks) (list text))
    text))

(defun process-output-text (p)
  "Everything PROCESS has printed to its own output (the most recent 20000 characters or so), or NIL."
  (let ((o (process-output p))) (and (typep o 'process-output-stream) (%process-output-stream-text o))))

;;; ---- without-interrupts -----------------------------------------------------------------------------------

(defmacro without-interrupts (&body body)
  "Run BODY without being preempted and without running interrupts (a process still gives up the
processor if BODY waits, sleeps or reads).  Keep BODY short."
  `(progn (%preempt-inhibit 1)
          (unwind-protect (progn ,@body) (%preempt-inhibit -1))))

;;; ---- starting and running processes ------------------------------------------------------------------------

(defun process-run-function (name-or-options function &rest args)
  "Start a process that applies FUNCTION to ARGS, and return it.  NAME-OR-OPTIONS is a name or a list of
options: :NAME, and :OUTPUT -- the stream the process prints to (default: a buffer of its own, shown by
Peek and PROCESS-OUTPUT-TEXT).  The process starts in the caller's package."
  (let* ((opts (if (and (consp name-or-options) (keywordp (car name-or-options))) name-or-options (list :name name-or-options)))
         (out (getf opts :output :buffer))
         (p (%make-process :name (princ-to-string (getf opts :name "Anonymous")) :function function :args args))
         (pkg *package*))
    (current-process)
    (setf (process-output p) (if (eq out :buffer) (make-instance 'process-output-stream) out))
    (setf (process-state p) :active)
    (%process-create (lambda () (%process-top p pkg)) p)
    p))

(defun %process-retire (p)
  (setf (process-finished p) (get-universal-time))
  (without-interrupts
    (setq *recent-processes* (cons p (remove p *recent-processes*)))
    (when (> (length *recent-processes*) *recent-process-limit*)
      (setq *recent-processes* (subseq *recent-processes* 0 *recent-process-limit*)))))

(defun %process-top (p pkg)
  (let* ((out (process-output p))
         (*current-process* p) (*package* pkg)
         (*standard-output* out) (*error-output* out) (*trace-output* out)
         (*standard-input* (make-string-input-stream ""))
         (*debugger-hook* nil)
         (outcome :aborted))
    (unwind-protect
         (setq outcome
               (catch '%process-kill
                 (handler-bind ((error (lambda (c)
                                         (setf (process-error p) c
                                               (process-backtrace p) (ignore-errors (%capture-backtrace)))
                                         (when *process-error-hook* (ignore-errors (funcall *process-error-hook* p c)))
                                         (throw '%process-kill :error))))
                   (setf (process-result p) (multiple-value-list (apply (process-function p) (process-args p))))
                   :done)))
      (setf (process-state p) (if (member outcome '(:done :killed :error)) outcome :aborted)
            (process-whostate p) (string-capitalize (symbol-name (process-state p))))
      (%process-retire p))))

;;; ---- waiting ------------------------------------------------------------------------------------------------

(defun process-wait (whostate function &rest args)
  "Return as soon as (APPLY FUNCTION ARGS) is true, letting other processes run meanwhile.  WHOSTATE
describes the wait (Peek shows it)."
  (unless (apply function args)
    (let* ((p (current-process)) (old (process-whostate p)))
      (setf (process-whostate p) whostate)
      (unwind-protect
           (loop (%process-wait-yield)
                 (when (apply function args) (return)))
        (%process-set-waiting nil)
        (setf (process-whostate p) old))))
  t)

(defun process-wait-with-timeout (whostate seconds function &rest args)
  "Like PROCESS-WAIT, but give up after SECONDS.  Returns true if the predicate became true, NIL on timeout."
  (let ((deadline (+ (get-internal-real-time) (round (* seconds internal-time-units-per-second)))) (ok nil))
    (process-wait whostate (lambda () (or (setq ok (apply function args)) (>= (get-internal-real-time) deadline))))
    (and ok t)))

(defun process-sleep (seconds &optional (whostate "Sleep"))
  "Sleep for SECONDS (unlike Genera's PROCESS-SLEEP, which counts 60ths), staying responsive to
PROCESS-KILL and PROCESS-INTERRUPT."
  (process-wait-with-timeout whostate seconds (constantly nil))
  nil)

(defun process-allow-schedule ()
  "Let other runnable processes run before continuing."
  (%process-yield)
  nil)
(defun process-yield () (process-allow-schedule))

(defun process-join (process &optional timeout)
  "Wait until PROCESS finishes (or TIMEOUT seconds pass); returns its values as a list, or NIL."
  (if timeout
      (process-wait-with-timeout "Join" timeout (lambda () (not (process-active-p process))))
      (process-wait "Join" (lambda () (not (process-active-p process)))))
  (process-result process))

;;; ---- interrupting, killing, arresting --------------------------------------------------------------------------

(defun %process-run-interrupts ()
  (let ((p (current-process)))
    (loop
      (let ((item (without-interrupts (let ((i (car (process-interrupts p)))) (setf (process-interrupts p) (cdr (process-interrupts p))) i))))
        (unless item (return))
        (apply (car item) (cdr item))))))

(defun process-interrupt (process function &rest args)
  "Make PROCESS call FUNCTION with ARGS at its next safe point, then carry on with what it was doing."
  (unless (process-active-p process) (error "~A is not running" process))
  (without-interrupts
    (setf (process-interrupts process) (append (process-interrupts process) (list (cons function args)))))
  (%process-interrupt process)
  process)

(defun process-kill (process)
  "Stop PROCESS: it unwinds (running its UNWIND-PROTECT cleanups) and finishes in state :KILLED."
  (cond ((process-initial process) (error "The initial process cannot be killed"))
        ((not (process-active-p process)) nil)
        (t (when (process-arrested process) (process-unarrest process))
           (process-interrupt process (lambda () (throw '%process-kill :killed)))
           t)))

(defun process-arrest (process)
  "Stop scheduling PROCESS until PROCESS-UNARREST."
  (when (process-initial process) (error "The initial process cannot be arrested"))
  (when (process-active-p process)
    (setf (process-arrested process) t)
    (%process-arrest process t))
  process)

(defun process-unarrest (process)
  (setf (process-arrested process) nil)
  (when (process-active-p process) (%process-arrest process nil))
  process)

;;; ---- locks ----------------------------------------------------------------------------------------------------------

(defstruct (lock (:constructor make-lock (&optional (name "Lock"))) (:copier nil))
  name (owner nil))

(defmethod print-object ((l lock) stream)
  (format stream "#<LOCK ~A~@[ held by ~A~]>" (lock-name l) (and (lock-owner l) (process-name (lock-owner l)))))

(defun %lock-try (lock p)
  (without-interrupts
    (cond ((null (lock-owner lock)) (setf (lock-owner lock) p) t)
          (t nil))))

(defun process-lock (lock &optional (whostate "Lock"))
  "Take LOCK, waiting while another process holds it.  It is an error to take a lock you already hold."
  (let ((p (current-process)))
    (when (eq (lock-owner lock) p) (error "~A already holds ~A" p lock))
    (loop (when (%lock-try lock p) (return lock))
          (process-wait whostate (lambda () (null (lock-owner lock)))))))

(defun process-unlock (lock)
  (unless (eq (lock-owner lock) (current-process)) (error "~A does not hold ~A" (current-process) lock))
  (setf (lock-owner lock) nil)
  lock)

(defmacro with-lock-held ((lock &optional (whostate "Lock")) &body body)
  "Run BODY holding LOCK (taken and released around it; a process that already holds LOCK just runs BODY)."
  (let ((l (gensym)) (took (gensym)))
    `(let* ((,l ,lock) (,took nil))
       (unwind-protect
            (progn (unless (eq (lock-owner ,l) (current-process)) (process-lock ,l ,whostate) (setq ,took t))
                   ,@body)
         (when ,took (process-unlock ,l))))))

;;; ---- finalization ---------------------------------------------------------------------------------------
;;; FINALIZE registers a finalizer cell: a weak object holding OBJECT weakly and the function strongly.  When a
;;; collection finds OBJECT otherwise unreachable it marks the cell fired, and at the next safe point the kernel
;;; calls %RUN-FINALIZERS, which calls the functions.

(defvar *finalizers* nil "The registered finalizers: a list of (cell . dont-save).")
(defvar *running-finalizers* nil "True while finalizers are being called (they are not called again from inside one).")

(defun finalize (object function &key dont-save)
  "Arrange for FUNCTION to be called, with no arguments, after OBJECT has been garbage collected.  FUNCTION must not
refer to OBJECT (or OBJECT is never collected).  It runs at a safe point after the collection, in whichever process
is running; an error in it is reported and ignored.  With DONT-SAVE the finalizer is dropped by SAVE-WORLD."
  (unless (functionp function) (error 'type-error :datum function :expected-type 'function))
  (let ((cell (cons (%make-weak 5 object function) dont-save)))
    (without-interrupts (setq *finalizers* (cons cell *finalizers*))))
  object)

(defun cancel-finalization (object)
  "Remove every finalizer registered for OBJECT.  True if there was one."
  (let ((found nil))
    (without-interrupts
      (setq *finalizers*
            (remove-if (lambda (c) (when (and (eql (%weak-mode (car c)) 5) (eq (%weak-key (car c)) object))
                                     (%weak-break (car c)) (setq found t)))
                       *finalizers*)))
    found))

(defun %drop-unsaved-finalizers ()
  (without-interrupts (setq *finalizers* (remove-if #'cdr *finalizers*))))

(defun %run-finalizers ()
  "Call the finalizers whose objects have been collected (the kernel calls this at a safe point)."
  (unless *running-finalizers*
    (let ((*running-finalizers* t))
      (loop while (%finalizers-pending t)
            do (let ((fired nil))
                 (without-interrupts
                   (setq *finalizers*
                         (remove-if (lambda (c)
                                      (case (%weak-mode (car c))
                                        (6 (setq fired (cons (%weak-value (car c)) fired)) (%weak-break (car c)) t)
                                        (7 t)))
                                    *finalizers*)))
                 (dolist (fn (nreverse fired))
                   (handler-case (funcall fn)
                     (error (e) (format *error-output* "~&;; Error in a finalizer: ~A~%" e)))))))))

(defun run-pending-finalizers ()
  "Call the finalizers of objects collected so far now, rather than at the next safe point."
  (%run-finalizers))
