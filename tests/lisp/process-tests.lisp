;;;; process-tests.lisp -- processes, scheduling, locks, interrupts and Peek.

(in-package :cl)

;; smaller workloads under GC stress (NG_STRESS=1), where every safe point collects
(defvar *pt-scale* (if (getenv "NG_STRESS") 1 100))

(defun %wait-until (pred &optional (seconds 5))
  "Poll PRED (letting other processes run) for up to SECONDS; returns its value."
  (process-wait-with-timeout "Test" seconds pred)
  (funcall pred))

(deftest process-basics
  (let ((me (current-process)))
    (is (process-p me))
    (is (process-initial me))
    (is (eq me (current-process)))
    (is (member me (all-processes))))
  (let ((p (process-run-function "Adder" (lambda (a b) (values (+ a b) :second)) 2 3)))
    (is-equal "Adder" (process-name p))
    (is-equal '(5 :second) (process-join p))
    (is (not (process-active-p p)))
    (is-equal :done (process-state p))
    (is (member p *recent-processes*))
    (is (not (member p (all-processes)))))
  ;; options list, explicit output stream
  (let* ((s (make-string-output-stream))
         (p (process-run-function '(:name "Writer" :output nil) (lambda (out) (format out "to ~A" "stream")) s)))
    (process-join p)
    (is-equal "to stream" (get-output-stream-string s)))
  ;; the default output is a buffer of the process's own
  (let ((p (process-run-function "Talker" (lambda () (format t "line ~D~%" 1) (princ "last")))))
    (process-join p)
    (is-equal (format nil "line 1~%last") (process-output-text p)))
  ;; a process starts in its creator's package and reads from an empty stream
  (let ((p (let ((*package* (find-package "COMMON-LISP-USER")))
             (process-run-function "Pkg" (lambda () (list (package-name *package*) (read-char *standard-input* nil :eof)))))))
    (is-equal '(("COMMON-LISP-USER" :eof)) (process-join p)))
  (is (find-process "Initial"))
  (is (eq (current-process) (find-process (process-id (current-process))))))

(defvar *pt-a* 0)
(defvar *pt-b* 0)

(deftest process-preemption
  ;; two processes in endless loops both make progress, and the initial process gets the processor back
  (setq *pt-a* 0 *pt-b* 0)
  (let ((pa (process-run-function "Loop A" (lambda () (loop (setq *pt-a* (+ *pt-a* 1))))))
        (pb (process-run-function "Loop B" (lambda () (loop (setq *pt-b* (+ *pt-b* 1)))))))
    (sleep 0.2)
    (is (> *pt-a* 1000))
    (is (> *pt-b* 1000))
    (is (> (process-run-time pa) 0.0))
    (process-kill pa) (process-kill pb)
    (is (%wait-until (lambda () (not (or (process-active-p pa) (process-active-p pb))))))
    (is-equal :killed (process-state pa))
    (is-equal :killed (process-state pb))))

(defvar *pt-special* :global)

(deftest process-special-bindings
  ;; bindings are private to a process; global values are shared
  (let ((p (let ((*pt-special* :creator))
             (process-run-function "Binder"
                                   (lambda () (list *pt-special*
                                                    (let ((*pt-special* :inner)) (process-sleep 0.05) *pt-special*)
                                                    *pt-special*))))))
    (let ((*pt-special* :mine))
      (process-allow-schedule)
      (is-equal '((:global :inner :global)) (process-join p))
      (is-equal :mine *pt-special*)))
  (let ((p (process-run-function "Setter" (lambda () (setq *pt-special* :changed)))))
    (process-join p)
    (is-equal :changed *pt-special*)
    (setq *pt-special* :global)))

(deftest process-errors
  ;; the hook is a global value: a binding in this process would not be seen by the new one
  (let* ((seen nil) (old *process-error-hook*)
         (p (progn (setq *process-error-hook* (lambda (p c) (setq seen (list p c))))
                   (process-run-function "Bad" (lambda () (car 5))))))
    (process-join p)
    (setq *process-error-hook* old)
    (is-equal :error (process-state p))
    (is (typep (process-error p) 'type-error))
    (is (consp (process-backtrace p)))
    (is (eq p (car seen))))
  ;; an error handled inside the process does not end it
  (let ((p (process-run-function "Handled" (lambda () (handler-case (error "oops") (error () :recovered))))))
    (is-equal '(:recovered) (process-join p))))

(defvar *pt-lock* nil)
(defvar *pt-count* 0)

(deftest process-locks
  (setq *pt-lock* (make-lock "Counter") *pt-count* 0)
  (is (lock-p *pt-lock*))
  (is-equal "Counter" (lock-name *pt-lock*))
  ;; a read-modify-write that yields in the middle stays correct under the lock
  (let ((ps (loop for i below 4
                  collect (process-run-function (format nil "Incr ~D" i)
                                                (lambda () (dotimes (k (if (getenv "NG_STRESS") 5 50))
                                                             (with-lock-held (*pt-lock*)
                                                               (let ((v *pt-count*)) (process-allow-schedule) (setq *pt-count* (+ v 1))))))))))
    (dolist (p ps) (process-join p))
    (is-equal (* 4 (if (getenv "NG_STRESS") 5 50)) *pt-count*))
  (is (null (lock-owner *pt-lock*)))
  ;; nested WITH-LOCK-HELD in the same process is allowed; PROCESS-LOCK twice is an error
  (with-lock-held (*pt-lock*)
    (with-lock-held (*pt-lock*) (is (eq (current-process) (lock-owner *pt-lock*))))
    (is (eq (current-process) (lock-owner *pt-lock*)))
    (is (null (ignore-errors (process-lock *pt-lock*)))))
  (is (null (lock-owner *pt-lock*)))
  ;; a waiter shows the lock whostate
  (process-lock *pt-lock*)
  (let ((p (process-run-function "Blocked" (lambda () (with-lock-held (*pt-lock* "Counter lock") :got-it)))))
    (is (%wait-until (lambda () (equal (process-whostate p) "Counter lock"))))
    (process-unlock *pt-lock*)
    (is-equal '(:got-it) (process-join p))))

(defvar *pt-flag* nil)

(deftest process-control
  ;; process-wait, interrupt, arrest, kill
  (setq *pt-flag* nil)
  (let ((p (process-run-function "Waiter" (lambda () (process-wait "Flag" (lambda () *pt-flag*)) :woke))))
    (is (%wait-until (lambda () (equal (process-whostate p) "Flag"))))
    (is-equal "Flag" (process-state-summary p))
    (let ((got nil))
      (process-interrupt p (lambda () (setq got (process-whostate (current-process)))))
      (is (%wait-until (lambda () got)))
      (is-equal "Flag" got))
    (setq *pt-flag* t)
    (is-equal '(:woke) (process-join p 5)))
  (let* ((n 0) (p (process-run-function "Busy" (lambda () (loop (setq n (+ n 1)) (process-allow-schedule))))))
    (is (%wait-until (lambda () (> n 0))))
    (process-arrest p)
    (process-sleep 0.05)
    (is-equal "Arrested" (process-state-summary p))
    (let ((before n)) (process-sleep 0.05) (is-equal before n))   ; an arrested process does not run
    (process-unarrest p)
    (let ((before n)) (is (%wait-until (lambda () (> n before)))))
    ;; kill runs unwind-protect cleanups
    (process-kill p)
    (is (%wait-until (lambda () (not (process-active-p p)))))
    (is-equal :killed (process-state p)))
  (let* ((cleaned nil)
         (p (process-run-function "Cleanup" (lambda () (unwind-protect (process-sleep 100) (setq cleaned t))))))
    (process-sleep 0.02)
    (process-kill p)
    (is (%wait-until (lambda () cleaned))))
  (is-equal nil (process-wait-with-timeout "Never" 0.05 (constantly nil)))
  (is (null (ignore-errors (process-kill (current-process)))))
  (is (null (ignore-errors (process-arrest (current-process))))))

(deftest process-consing
  ;; several processes allocating at once (collections happen while the others are switched out)
  (let ((ps (loop for i below 3
                  collect (process-run-function "Consing"
                                                (lambda (seed)
                                                  (let ((l nil))
                                                    (dotimes (k (* 300 *pt-scale*)) (push (list k (format nil "~D-~D" seed k)) l))
                                                    (list (length l) (cadr (car l)))))
                                                i))))
    (let ((n (* 300 *pt-scale*)))
      (is-equal (loop for i below 3 collect (list (list n (format nil "~D-~D" i (- n 1))))) (mapcar #'process-join ps)))))

(deftest process-blocking-calls
  ;; SLEEP and RUN-PROGRAM let other processes run
  (setq *pt-a* 0)
  (let ((p (process-run-function "Ticker" (lambda () (loop (setq *pt-a* (+ *pt-a* 1)) (process-allow-schedule))))))
    (sleep 0.1)
    (is (> *pt-a* 10))
    (let ((before *pt-a*))
      (multiple-value-bind (out status) (run-program "sh" (list "-c" "sleep 0.1; echo done"))
        (is-equal (format nil "done~%") out)
        (is-equal 0 status))
      (is (> *pt-a* before)))
    (process-kill p)
    (is (%wait-until (lambda () (not (process-active-p p)))))))

;;; ---- Peek ------------------------------------------------------------------------------------------------

(defun find-peek (wm) (find-if (lambda (w) (typep w 'peek-window)) (wm-windows wm)))

;; Skipped under GC stress: it redraws a large window many times and every call collects, which takes
;; hours; the other process tests exercise the roots of switched-out processes.
(deftest peek-window
  (unless (getenv "NG_STRESS")
  (multiple-value-bind (wm lw) (make-test-wm 30 110)
    (let ((p (process-run-function "Peeked" (lambda () (format t "hello from peek~%") (process-wait "Waiting for Godot" (constantly nil))))))
      (unwind-protect
           (progn
             (%wait-until (lambda () (equal (process-whostate p) "Waiting for Godot")))
             (type-line wm ":peek")
             (let ((w (find-peek wm)))
               (is w)
               (wm-draw wm)
               (is (screen-has wm "Peek: Processes"))
               (is (screen-has wm "Initial Process"))
               (is (screen-has wm "Peeked"))
               (is (screen-has wm "Waiting for Godot"))
               (is (screen-has wm "hello from peek"))              ; last line of the process's output
               (is (screen-has wm "Processes: 2"))                 ; the who-line counts processes
               ;; right-click a process: its commands
               (let ((pos (screen-has wm "Peeked")))
                 (wm-feed wm (mouse-bytes :press (+ (cdr pos) 1) (car pos) 2))
                 (wm-feed wm (mouse-bytes :release (+ (cdr pos) 1) (car pos) 2))
                 (is (screen-has wm "Kill"))
                 (is (screen-has wm "Show output"))
                 (is (screen-has wm "Arrest"))
                 (wm-close-popups wm))
               ;; it redraws itself on the clock: a new process appears
               (let ((q (process-run-function "Latecomer" (lambda () (process-sleep 100)))))
                 (dotimes (i 4) (wm-handle-event wm (list :tick)))
                 (wm-draw wm)
                 (is (screen-has wm "Latecomer"))
                 (process-kill q))
               ;; modes
               (wm-feed wm "m")
               (is (screen-has wm "Peek: Memory"))
               (is (screen-has wm "Collections"))
               (wm-feed wm "w")
               (is (screen-has wm "Peek: Windows"))
               (is (screen-has wm "listener-window"))
               (wm-feed wm "p")
               (is (screen-has wm "Peek: Processes"))
               ;; killing from the Listener; the finished process is listed as such
               (wm-raise wm lw)
               (type-line wm ":kill-process Peeked")
               (is (screen-has wm "Killed Peeked"))
               (is (%wait-until (lambda () (not (process-active-p p)))))
               (wm-raise wm w)
               (peek-refresh w)
               (wm-draw wm)
               (is (screen-has wm "Recently finished"))
               ;; :peek again reuses the window
               (wm-raise wm lw)
               (type-line wm ":peek memory")
               (is-equal 1 (length (remove-if-not (lambda (x) (typep x 'peek-window)) (wm-windows wm))))
               (is (screen-has wm "Peek: Memory"))))
        (when (process-active-p p) (process-kill p))))
    ;; an error in a background process is announced
    (let* ((*wm* wm) (p (process-run-function "Doomed" (lambda () (error "Doom ~D" 42)))))
      (process-join p)
      (wm-draw wm)
      (is (screen-has wm "Process Doomed got an error: Doom 42"))))))

(deftest show-processes-listing
  (let ((out (with-output-to-string (s) (show-processes s))))
    (is (%string-search "Initial Process" out 0))
    (is (%string-search "Name" out 0))))
