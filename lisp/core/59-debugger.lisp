;;;; 59-debugger.lisp -- the interactive debugger.
;;;;
;;;; INVOKE-DEBUGGER calls *DEBUGGER-HOOK* at the point of the error, with the stack
;;;; intact.  The Listener's hook (%LISTENER-DEBUGGER) prints the condition and
;;;; the available restarts, captures a backtrace of the active compiled frames
;;;; (function and argument values), and runs a nested read-eval-print loop.
;;;; Commands: N (invoke restart N), :bt, :frame N, :restarts, :abort, :continue,
;;;; :edit N (jump to the frame function's definition, if an editor is loaded),
;;;; :help.  Not implemented: returning a value from a frame, local variables.

(in-package :cl)

(defvar *debug-level* 0)
(defvar *debug-frames* nil)
(defvar *debug-frame-index* 0)
(defvar *debug-condition* nil)
(defvar *frame-args* nil)
(defvar *debugger-enabled* t)
(defvar *edit-hook* nil)

(defparameter *internal-frames*
  '(%capture-backtrace invoke-debugger %listener-debugger error signal %signal-error warn cerror
    %handler-case %restart-case %debugger-repl %call-gf))

(defun %capture-backtrace ()
  (let ((frames (%backtrace 400)))
    (loop while (and frames (member (cadr (car frames)) *internal-frames*)) do (setq frames (cdr frames)))
    frames))

(defun %abbrev (x &optional (n 60))
  (let ((s (if (%unsupplied-p x) "#<unsupplied>" (prin1-to-string x))))
    (if (> (length s) n) (concatenate 'string (subseq s 0 (- n 3)) "...") s)))

(defun %frame-name (frame)
  (let ((name (cadr frame))) (if name (prin1-to-string name) "(LAMBDA)")))

(defun %print-frame (i frame stream)
  (format stream "~&~3D: (~A~{ ~A~})~%" i (%frame-name frame) (mapcar #'%abbrev (caddr frame))))

(defun print-backtrace (&optional (frames *debug-frames*) (stream *standard-output*) (limit 25))
  (let ((i 0))
    (dolist (f frames)
      (when (>= i limit) (format stream "~&     ... ~D more frames~%" (- (length frames) limit)) (return))
      (%print-frame i f stream)
      (setq i (+ i 1)))))

(defun %show-restarts (restarts &optional (stream *standard-output*))
  (format stream "~&Restarts:~%")
  (let ((i 0))
    (dolist (r restarts)
      (format stream "  ~D: [~A] ~A~%" i (symbol-name (restart-name r)) (restart-report-string r))
      (setq i (+ i 1)))))

(defun %select-frame (n)
  (if (and (integerp n) (>= n 0) (< n (length *debug-frames*)))
      (let ((f (nth n *debug-frames*)))
        (setq *debug-frame-index* n)
        (setq *frame-args* (caddr f))
        (%print-frame n f *standard-output*)
        (let ((i 0))
          (dolist (a (caddr f)) (format t "       arg ~D = ~A~%" i (%abbrev a 100)) (setq i (+ i 1))))
        (format t "~&  (arguments are available as *FRAME-ARGS*)~%"))
      (format t "~&No such frame: ~S~%" n)))

(defun %debugger-help ()
  (format t "~&Debugger commands:
  N              invoke restart number N
  :restarts      list restarts          :bt [N]    backtrace (N frames)
  :frame N       select frame N         :edit [N]  edit the function of frame N
  :abort         return to previous level   :continue  invoke CONTINUE
  :help          this message           any Lisp form is evaluated~%"))

(defun %debugger-command (form restarts)
  ;; returns T if FORM was a debugger command
  (cond ((integerp form)
         (if (and (>= form 0) (< form (length restarts)))
             (invoke-restart (nth form restarts))
             (format t "~&No restart ~D~%" form))
         t)
        ((member form '(:restarts :r)) (%show-restarts restarts) t)
        ((member form '(:bt :backtrace :b)) (print-backtrace) t)
        ((member form '(:help :h :?)) (%debugger-help) t)
        ((member form '(:abort :a)) (abort) t)
        ((member form '(:continue :c)) (continue) (format t "~&No CONTINUE restart~%") t)
        ((member form '(:quit :q)) (exit 1) t)
        ((member form '(:edit :e))
         (let ((f (nth *debug-frame-index* *debug-frames*)))
           (if (and *edit-hook* f (cadr f)) (funcall *edit-hook* (cadr f)) (format t "~&No editor available~%")))
         t)
        ((and (consp form) (member (car form) '(:bt :backtrace)))
         (print-backtrace *debug-frames* *standard-output* (cadr form)) t)
        ((and (consp form) (member (car form) '(:frame :f))) (%select-frame (cadr form)) t)
        ((and (consp form) (member (car form) '(:edit :e)))
         (let ((f (nth (or (cadr form) *debug-frame-index*) *debug-frames*)))
           (if (and *edit-hook* f (cadr f)) (funcall *edit-hook* (cadr f)) (format t "~&No editor available~%")))
         t)
        (t nil)))

(defun %debugger-repl (restarts)
  (let ((level *debug-level*))
    (loop
      (format t "~&debug ~D> " level)
      (finish-output)
      (let ((form (handler-case (read *standard-input* nil :eof)
                    (error (e) (format t "~&Read error: ~A~%" e) :retry))))
        (when (member form '(:frame :f :bt :backtrace :b :edit :e))
          (let ((arg (parse-integer (read-line *standard-input* nil "") :junk-allowed t)))
            (when arg (setq form (list form arg)))))
        (cond ((eq form :eof) (terpri) (abort))
              ((eq form :retry) nil)
              ((%debugger-command form restarts) nil)
              (t (restart-case
                     (let ((vals (multiple-value-list (eval form))))
                       (dolist (v vals) (format t "~&~S~%" v)))
                   (abort () :report (lambda (s) (format s "Return to debug level ~D" level))
                     (format t "~&; Back at debug level ~D~%" level)))))))))

(defun %listener-debugger (condition hook)
  (declare (ignore hook))
  (when (or (not *debugger-enabled*) (> *debug-level* 8))
    (format t "~&Error: ~A~%" (condition-report-string condition))
    (abort))
  (let* ((*debug-level* (+ *debug-level* 1))
         (*debug-condition* condition)
         (*debug-frames* (%capture-backtrace))
         (*debug-frame-index* 0)
         (*frame-args* nil)
         (restarts (compute-restarts condition)))
    (format t "~&~%Error: ~A~%" (condition-report-string condition))
    (format t "  [Condition of type ~A]~%" (symbol-name (%cls-name (class-of condition))))
    (%show-restarts restarts)
    (format t "~&Backtrace (innermost first; :BT for more, :HELP for commands):~%")
    (print-backtrace *debug-frames* *standard-output* 5)
    (%debugger-repl restarts)))
