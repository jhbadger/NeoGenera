;;;; 42-debugger.lisp -- the debugger in the Listener window.
;;;;
;;;; An error in a Listener evaluation does not unwind.  The handler (%WINDOW-DEBUGGER) runs where the error
;;;; was signalled, prints the condition, the restarts and the top of the backtrace as presentations, and runs
;;;; a nested event loop for the whole window system.  The Listener prompt becomes "debug N>"; forms typed
;;;; there are evaluated in the dynamic environment of the error, and an error among them enters level N+1.
;;;; The level ends only by a non-local exit: a restart (typed by number, clicked, :abort, :continue, or
;;;; invoked by code), quitting the window system, or, when running headless, the end of the scripted input.
;;;; Other windows keep working meanwhile, as they all run in the nested loop.

(in-package :cl)

(defstruct (dbg (:constructor %make-dbg) (:copier nil)) level condition restarts frames)

(defparameter *window-debug-limit* 5 "Debugger levels per Listener; an error deeper than this is only reported.")
(defvar *debug-resumer* nil "The window that last chose a restart, so other windows unwound by it can say so.")

(defun %window-restarts (w c)
  "The restarts for condition C as listener W sees them: the innermost ones up to W's outermost ABORT restart
(beyond that are other windows' evaluations and the line-mode Listener)."
  (let* ((all (compute-restarts c)) (mine (mapcar #'car (remove-if-not (lambda (e) (eq (cdr e) w)) *listener-aborts*)))
         (last-index nil) (i 0))
    (dolist (r all) (when (member r mine) (setq last-index i)) (setq i (+ i 1)))
    (if last-index (subseq all 0 (+ last-index 1)) all)))

(defun %restart-needs-value-p (r)
  (let ((how (restart-arguments r)))
    (or (eq how :required) (and (eq how :unknown) (member (restart-name r) '(use-value store-value))))))

(defun %restart-takes-value-p (r) (not (eq (restart-arguments r) :none)))

(defun %debugging-p (w) (and w (typep w 'listener-window) (lw-debug w) t))

(defun %print-debugger-entry (w d)
  (let ((out (lw-stream w)) (c (dbg-condition d)))
    (%fresh-line w)
    (with-style (out :error)
      (with-output-as-presentation (out c 'condition)
        (format out "~A: ~A" (if (typep c 'serious-condition) "Error" "Break") (condition-report-string c)))
      (terpri out))
    (with-style (out :dim)
      (format out "  [~A]  debug level ~D~%" (string-downcase (symbol-name (%cls-name (class-of c)))) (dbg-level d)))
    (let ((i 0))
      (dolist (r (dbg-restarts d))
        (write-string "  " out)
        (with-output-as-presentation (out r 'restart)
          (with-style (out :link) (format out "~D: ~A" i (restart-report-string r))))
        (with-style (out :dim) (format out "  [~A]~%" (symbol-name (restart-name r))))
        (setq i (+ i 1))))
    (with-style (out :dim)
      (let ((i 0))
        (dolist (f (dbg-frames d))
          (when (>= i 4) (format out "  ... ~D more (:bt)~%" (- (length (dbg-frames d)) 4)) (return))
          (with-output-as-presentation (out f 'frame)
            (format out "  ~D: (~A~{ ~A~})" i (%frame-name f) (mapcar #'%abbrev (caddr f))))
          (terpri out)
          (setq i (+ i 1))))
      (format out "  Type a restart number or click one; forms are evaluated where the error happened.  :help~%"))))

(defun %debug-loop (w)
  "Serve the window system until a non-local exit leaves this debugger level."
  (let* ((wm (window-wm w)) (o *outside-listener*)
         (*standard-output* (first o)) (*error-output* (second o)) (*standard-input* (third o))
         (*package* (fourth o)) (*debugger-hook* (fifth o)) (*current-listener* (sixth o)))
    (setf (wm-busy wm) nil (wm-dirty wm) t)
    (loop
      (when (wm-quit wm) (setq *debug-resumer* w) (throw 'listener-abort nil))
      (if *wm-script*
          (let ((ev (wm-next-scripted-event)))
            (unless ev (setq *debug-resumer* w) (throw 'listener-abort nil))   ; headless: no more input
            (wm-handle-event wm ev))
          (wm-serve wm)))))

(defun %window-debugger (w c)
  "Enter the debugger in listener W for condition C, at the point where C was signalled."
  (when (or (not *debugger-enabled*) (null (window-wm w)) (not (or *wm-script* *wm-live*))
            (>= (length (lw-debug w)) *window-debug-limit*))
    (%post-mortem w c))
  (let* ((frames (%capture-error-frames))
         (d (%make-dbg :level (+ 1 (length (lw-debug w))) :condition c :restarts (%window-restarts w c) :frames frames))
         (*debug-condition* c) (*debug-frames* frames) (*debug-frame-index* 0) (*frame-args* nil)
         (*signal-depth* 0))
    (setq *debug-resumer* nil)
    (setf (lw-last-error w) (cons c frames))
    (%print-debugger-entry w d)
    (setf (lw-debug w) (cons d (lw-debug w)))
    (unwind-protect (%debug-loop w)
      (setf (lw-debug w) (remove d (lw-debug w)))
      (let ((outer (car (lw-debug w))))
        (setf (lw-last-error w) (if outer (cons (dbg-condition outer) (dbg-frames outer)) (lw-last-error w))))
      (when (and *debug-resumer* (not (eq *debug-resumer* w)))
        (%fresh-line w)
        (with-style ((lw-stream w) :dim)
          (format (lw-stream w) "; Debug level ~D left: a restart was chosen in another window~%" (dbg-level d))))
      (setf (wm-dirty (window-wm w)) t))))

;;; ---- choosing a restart -----------------------------------------------------------------------------------

(defun %invoke-window-restart (w r &optional args)
  (cond ((not (member r (compute-restarts)))
         (with-style ((lw-stream w) :dim) (format (lw-stream w) "~&; That restart is no longer active~%")))
        (t (setq *debug-resumer* w)
           (unless (eq (restart-name r) 'abort)
             (with-style ((lw-stream w) :dim) (format (lw-stream w) "~&; ~A~%" (restart-report-string r))))
           (apply #'invoke-restart r args))))

(defun %restart-number-line (text)
  "If TEXT is a restart number, optionally followed by a form for its value, return (values number rest)."
  (let ((i 0) (n (length text)))
    (loop (when (or (>= i n) (not (digit-char-p (char text i)))) (return)) (setq i (+ i 1)))
    (when (and (> i 0) (or (= i n) (member (char text i) '(#\Space #\Tab))))
      (values (parse-integer (subseq text 0 i)) (string-trim " " (subseq text i))))))

(defun %debug-restart-line (w text)
  (multiple-value-bind (n rest) (%restart-number-line text)
    (let* ((d (car (lw-debug w))) (r (nth n (dbg-restarts d))) (out (lw-stream w)))
      (cond ((null r) (with-style (out :dim) (format out "; There is no restart ~D (:restarts lists them)~%" n)))
            ((and (> (length rest) 0) (not (%restart-takes-value-p r)))
             (with-style (out :dim) (format out "; Restart ~D takes no value: type just ~D~%" n n)))
            ((> (length rest) 0)
             (multiple-value-bind (vals ok) (%in-listener w (lambda () (eval (read-from-string rest))))
               (when ok (%invoke-window-restart w r (list (car vals))))))
            ((%restart-needs-value-p r)
             (with-style (out :dim) (format out "; Restart ~D needs a value: type ~D and a form, as in ~D 42~%" n n n)))
            (t (%invoke-window-restart w r))))))

(defun %current-dbg () (and (%debugging-p *current-listener*) (car (lw-debug *current-listener*))))

(defun %debug-named-restart (name)
  (let* ((d (%current-dbg)) (r (and d (find name (dbg-restarts d) :key #'restart-name))))
    (cond ((null d) (format t "~&Not in the debugger~%"))
          ((null r) (format t "~&There is no ~A restart here~%" (symbol-name name)))
          (t (%invoke-window-restart *current-listener* r)))))

(define-presentation-command (choose-restart restart :documentation "Invoke this restart" :default t) (r)
  (let* ((w *current-listener*) (d (and (%debugging-p w) (find-if (lambda (d) (member r (dbg-restarts d))) (lw-debug w)))))
    (cond ((null d) (format t "~&; That restart is no longer active~%"))
          ((%restart-needs-value-p r)
           (let ((n (position r (dbg-restarts d))))
             (%insert-text w (format nil "~D " n))
             (format t "~&; Type a form for the value of restart ~D and press Return~%" n)))
          (t (%invoke-window-restart w r)))))

;;; ---- commands --------------------------------------------------------------------------------------------

(define-command (restarts :documentation "List the restarts of the error being debugged") ()
  (let ((d (%current-dbg)) (out *standard-output*))
    (if (null d)
        (format t "~&Not in the debugger~%")
        (let ((i 0))
          (dolist (r (dbg-restarts d))
            (write-string "  " out)
            (with-output-as-presentation (out r 'restart) (format out "~D: ~A" i (restart-report-string r)))
            (format out "  [~A]~%" (symbol-name (restart-name r)))
            (setq i (+ i 1)))))))

(define-command (abort :documentation "Leave this debugger level (its ABORT restart)") ()
  (%debug-named-restart 'abort))

(define-command (continue :documentation "Continue from the error (its CONTINUE restart)") ()
  (%debug-named-restart 'continue))

(define-command (frame :documentation "Show a frame of the last backtrace; in the debugger, *FRAME-ARGS* gets its arguments")
    ((n integer :prompt "Frame number"))
  (let* ((e (and *current-listener* (lw-last-error *current-listener*))) (f (and e (integerp n) (>= n 0) (nth n (cdr e)))))
    (cond ((null e) (format t "~&No error to show~%"))
          ((null f) (format t "~&No frame ~A~%" n))
          (t (with-output-as-presentation (*standard-output* f 'frame)
               (format t "~3D: (~A~{ ~A~})" n (%frame-name f) (mapcar #'%abbrev (caddr f))))
             (terpri)
             (let ((i 0)) (dolist (a (caddr f)) (format t "       arg ~D = ~A~%" i (%abbrev a 100)) (setq i (+ i 1))))
             (when (%current-dbg)
               (setq *frame-args* (caddr f) *debug-frame-index* n)
               (format t "~&  (the arguments are in *FRAME-ARGS*)~%"))))))
