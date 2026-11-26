;;;; 15-advise.lisp -- ADVISE (wrapping code around existing functions) and DRIBBLE (recording a session).
;;;;
;;;; Advice, as in Genera: (ADVISE fn :BEFORE name nil forms...) runs FORMS before FN with ARGLIST bound to the
;;;; argument list (which they may change); :AFTER runs them afterwards with VALUES bound to the list of values
;;;; (which they may change); :AROUND wraps them around the call, which happens where :DO-IT appears.  A function
;;;; is advised by one wrapper that reads its advice from the symbol at each call, so advice can be added and removed
;;;; in any order, and tracing still works on top of it.

(in-package :cl)

(defvar *advised-functions* nil "The names of the functions that have advice.")

(defun %replace-do-it (form replacement)
  (cond ((eq form :do-it) replacement)
        ((and (consp form) (eq (car form) 'quote)) form)
        ((consp form) (cons (%replace-do-it (car form) replacement) (%replace-do-it (cdr form) replacement)))
        (t form)))

(defmacro advise (function class name position &body forms)
  "Add advice to the function named FUNCTION.  CLASS is :BEFORE, :AFTER or :AROUND; NAME identifies the advice (advice
with the same class and name replaces it); POSITION is NIL (last) or an index among the advice of that class.  In the
FORMS, ARGLIST is the list of arguments; in :AFTER advice VALUES is the list of values; in :AROUND advice :DO-IT calls
the function (and the inner advice) and returns its values."
  (let ((fn (ecase class
              (:before `(lambda (arglist) (declare (ignorable arglist)) ,@forms arglist))
              (:after `(lambda (arglist values) (declare (ignorable arglist values)) ,@forms values))
              (:around `(lambda (arglist %continue) (declare (ignorable arglist))
                          ,@(%replace-do-it forms '(funcall %continue arglist)))))))
    `(%add-advice ',function ,class ',name ,position ,fn)))

(defun %advice-list (fn) (get fn '%advice))

(defun %add-advice (fn class name position advice)
  (unless (fboundp fn) (error "Cannot advise ~S: it has no function definition" fn))
  (unless (get fn '%advised-original)
    (let ((orig (symbol-function fn)))
      (%put fn '%advised-original orig)
      (%set-symbol-function fn (%advice-wrapper fn orig))
      (pushnew fn *advised-functions*)))
  (let* ((all (remove-if (lambda (a) (and (eq (car a) class) (equal (cadr a) name))) (%advice-list fn)))
         (same (remove-if-not (lambda (a) (eq (car a) class)) all))
         (others (remove-if (lambda (a) (eq (car a) class)) all))
         (entry (list class name advice))
         (k (if (and (integerp position) (<= 0 position (length same))) position (length same))))
    (%put fn '%advice (append others (append (subseq same 0 k) (list entry) (nthcdr k same))))
    fn))

(defun %advice-wrapper (fn orig)
  (lambda (&rest args)
    (let ((advice (%advice-list fn)) (arglist args))
      (dolist (a advice) (when (eq (car a) :before) (setq arglist (funcall (caddr a) arglist))))
      (let* ((arounds (remove-if-not (lambda (a) (eq (car a) :around)) advice))
             (values (labels ((call (rest arglist)
                                (if (null rest)
                                    (multiple-value-list (apply orig arglist))
                                    (multiple-value-list (funcall (caddr (car rest)) arglist
                                                                  (lambda (al) (values-list (call (cdr rest) al))))))))
                       (call arounds arglist))))
        (dolist (a advice) (when (eq (car a) :after) (setq values (funcall (caddr a) arglist values))))
        (values-list values)))))

(defun unadvise-function (fn &optional class name)
  "Remove the advice of FN of CLASS (all classes if NIL) and NAME (all names if not given)."
  (%put fn '%advice (remove-if (lambda (a) (and (or (null class) (eq (car a) class)) (or (eq name :all) (equal (cadr a) name))))
                               (%advice-list fn)))
  (when (null (%advice-list fn))
    (let ((orig (get fn '%advised-original)))
      (when orig (%set-symbol-function fn orig) (%put fn '%advised-original nil)))
    (setq *advised-functions* (remove fn *advised-functions*)))
  fn)

(defmacro unadvise (&optional function class (name :all))
  "Remove advice: of FUNCTION (or of all functions), of CLASS (or all classes), named NAME (or all)."
  (if function
      `(unadvise-function ',function ,class ',name)
      `(mapcar (lambda (f) (unadvise-function f ,class ',name)) (copy-list *advised-functions*))))

(defun describe-advice (fn &optional (stream *standard-output*))
  "Print the advice of the function FN."
  (let ((advice (%advice-list fn)))
    (if (null advice)
        (format stream "~&~S has no advice~%" fn)
        (dolist (a advice) (format stream "~&~S ~S advice ~@[~S~]~%" fn (car a) (cadr a))))
    (values)))

;;; ---- dribble -------------------------------------------------------------------------------------------------

(defvar *dribble-saved-output* nil)

(defun dribble (&optional path)
  "Record the session to the file PATH: what is typed and what is printed.  (DRIBBLE) stops recording."
  (when *dribble-stream*
    (format *dribble-stream* "~&;;; Dribble ended ~A~%" (%date-string (get-universal-time)))
    (close *dribble-stream*)
    (when *dribble-saved-output* (setq *standard-output* *dribble-saved-output*))
    (setq *dribble-stream* nil *dribble-pane* nil *dribble-saved-output* nil))
  (when path
    (let ((s (open (%native-namestring path) :direction :output :if-exists :append)))
      (format s ";;; Dribble of NeoGenera ~A started ~A~%" *ng-version* (%date-string (get-universal-time)))
      (setq *dribble-stream* s)
      (if (and (boundp '*current-listener*) *current-listener*)
          (setq *dribble-pane* (funcall 'lw-obuf *current-listener*))     ; the window Listener copies its pane
          (setq *dribble-saved-output* *standard-output*
                *standard-output* (make-broadcast-stream *standard-output* s)))))
  (and path t))

(defun %dribble-input (text)
  "Called by the Listeners with the text of each form or command typed."
  (when (and *dribble-stream* (null *dribble-pane*))
    (write-string text *dribble-stream*) (terpri *dribble-stream*) (finish-output *dribble-stream*)))
