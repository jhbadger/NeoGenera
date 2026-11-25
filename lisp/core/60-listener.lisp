;;;; 60-listener.lisp -- the Lisp-written Listener (REPL).
;;;;
;;;; This is the text-mode precursor of the Genera-style Listener planned for
;;;; Phase 5.  It supports history variables, error recovery and a few
;;;; colon-commands.

(in-package :cl)

(defvar *ng-version* "0.3.0")
(defvar * nil)
(defvar ** nil)
(defvar *** nil)
(defvar + nil)
(defvar ++ nil)
(defvar +++ nil)
(defvar - nil)
(defvar / nil)
(defvar *listener-prompt* nil)
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
     t)
    (t nil)))

(defun %listener-eval-and-print (form)
  (setq - form)
  (let ((vals (multiple-value-list (eval form))))
    (setq +++ ++ ++ + + form)
    (setq *** ** ** * * (car vals))
    (setq / vals)
    (%print-values vals)))

(defun apropos (string &optional package)
  (let ((acc nil) (s (string-upcase (string string))))
    (dolist (p (if package (list (find-package package)) (list-all-packages)))
      (dolist (sym (%package-symbols p nil))
        (when (%string-search s (symbol-name sym) 0) (setq acc (cons sym acc)))))
    (sort (remove-duplicates acc) (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(defun ng-toplevel ()
  (setq *listener-running* t)
  (format t "~&NeoGenera ~A -- type :help for help, :quit to leave.~%" *ng-version*)
  (block nil
    (tagbody next
       (when (not *listener-running*) (return))
       (format t "~&~A> " (%package-prompt))
       (finish-output)
       (let ((form (handler-case (read *standard-input* nil '%eof)
                     (error (e) (format t "~&Read error: ~A~%" e) '%retry))))
         (cond ((eq form '%eof) (terpri) (return))
               ((eq form '%retry) nil)
               ((and (keywordp form) (%listener-command form)) nil)
               ((and (consp form) (eq (car form) :package))
                (setq *package* (find-package (cadr form))))
               ((and (consp form) (eq (car form) :apropos))
                (format t "~&~{~S~%~}" (apropos (cadr form))))
               ((and (consp form) (eq (car form) :time))
                (handler-case (%time-it (lambda () (%listener-eval-and-print (cadr form))))
                  (error (e) (format t "~&Error: ~A~%" e))))
               (t (handler-case (%listener-eval-and-print form)
                    (error (e) (format t "~&Error: ~A~%" e))))))
       (go next)))
  nil)
