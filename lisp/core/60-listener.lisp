;;;; 60-listener.lisp -- the Lisp-written Listener (REPL), line oriented.
;;;;
;;;; The text-mode precursor of the windowed Listener (lisp/ui).  Unhandled errors
;;;; enter the interactive debugger (59-debugger.lisp); :ABORT returns here.

(in-package :cl)

(defvar *ng-version* "1.0.0")
(defvar * nil)
(defvar ** nil)
(defvar *** nil)
(defvar + nil)
(defvar ++ nil)
(defvar +++ nil)
(defvar - nil)
(defvar / nil)
(defvar *listener-running* nil)

(defun %package-prompt ()
  (let ((n (package-name *package*)))
    (cond ((string= n "COMMON-LISP-USER") "CL-USER")
          ((string= n "COMMON-LISP") "CL")
          (t n))))

(defun %print-values (vals)
  (if (null vals)
      (format t "~&; No values~%")
      (dolist (v vals) (format t "~&~S~%" v))))

(defun %listener-command (cmd)
  (case cmd
    ((:quit :q :exit) (setq *listener-running* nil) t)
    (:help (format t "~&Commands: :help  :quit  :package NAME  :apropos STRING  :time FORM~%")
           (format t "Documentation and tutorial (window system): :tutorial  :doc [TOPIC]  :search-doc WORDS~%")
           t)
    (t nil)))

(defun %listener-eval-and-print (form)
  (setq - form)
  (let ((vals (multiple-value-list (eval form))))
    (setq +++ ++ ++ + + form)
    (setq *** ** ** * * (car vals))
    (setq / vals)
    (%print-values vals)))

(defun apropos-list (string &optional package)
  (let ((acc nil) (s (string-upcase (string string))))
    (dolist (p (if package (list (find-package package)) (list-all-packages)))
      (dolist (sym (%package-symbols p nil))
        (when (%string-search s (symbol-name sym) 0) (setq acc (cons sym acc)))))
    (sort (remove-duplicates acc) (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(defun apropos (string &optional package)
  (dolist (s (apropos-list string package))
    (format t "~&~S~@[ [function]~]~@[ [variable]~]~%" s (and (fboundp s) t) (and (boundp s) t)))
  (values))

(defun %listener-step (form)
  (cond ((and (keywordp form) (%listener-command form)) nil)
        ((and (consp form) (eq (car form) :package))
         (setq *package* (find-package (cadr form))))
        ((and (consp form) (eq (car form) :apropos)) (apropos (cadr form)))
        ((and (consp form) (eq (car form) :time))
         (%time-it (lambda () (%listener-eval-and-print (cadr form)))))
        (t (%listener-eval-and-print form))))

(defun ng-toplevel ()
  (setq *listener-running* t)
  (when (getenv "NG_NODEBUG") (setq *debugger-enabled* nil))
  (format t "~&NeoGenera ~A -- type :help for help, :quit to leave.~%" *ng-version*)
  (when (and (boundp '*world-saved*) (symbol-value '*world-saved*))
    (let ((w (symbol-value '*world-saved*)))
      (format t "~&; A saved world: ~A~@[ (~A)~]~%" (namestring (car w)) (caddr w))))
  (let ((*debugger-hook* (function %listener-debugger)))
    (loop
      (when (not *listener-running*) (return))
      (format t "~&~A> " (%package-prompt))
      (finish-output)
      (%stdin-wait *standard-input*)              ; let other processes run while waiting for input
      (let ((form (handler-case (read *standard-input* nil '%eof)
                    (error (e) (format t "~&Read error: ~A~%" e) '%retry))))
        (cond ((eq form '%eof) (terpri) (return))
              ((eq form '%retry) nil)
              (t (when (fboundp '%dribble-input) (funcall '%dribble-input (prin1-to-string form)))
                 (restart-case (%listener-step form)
                   (abort () :report "Return to the Listener top level"
                     (format t "~&; Aborted~%"))))))))
  nil)
