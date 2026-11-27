;;;; 57-reader.lisp -- readtables and reader macros.
;;;;
;;;; The reader itself is C (src/reader.c).  *READTABLE* holds a READTABLE structure that it consults: macro
;;;; characters call Lisp functions (STREAM CHAR), dispatch characters such as # call a function for their
;;;; sub-character (STREAM SUB-CHAR ARG), and the readtable case controls how symbol names are converted.  Only
;;;; characters below 128 can be macro characters or have their syntax changed.  The slot order of READTABLE is known to
;;;; the C reader.  The SYNTAX slot holds, per character, NIL (the standard syntax: whitespace for space, tab, newline,
;;;; return, page and VT, macro syntax for the characters ( ) ' ` , " ; #, a constituent otherwise), T (whitespace) or 0
;;;; (neither: a constituent, or a macro character whose function is in MACROS).

(in-package :cl)

(defstruct (readtable (:constructor %make-readtable) (:conc-name %rt-) (:copier nil) (:predicate readtablep))
  (case 0)                                             ; 0 :upcase, 1 :downcase, 2 :preserve, 3 :invert
  (macros (make-array 128 :initial-element nil))       ; function per character, or NIL
  (terminating (make-array 128 :initial-element nil))  ; T for a terminating macro character
  (dispatch (make-array 128 :initial-element nil))     ; for a dispatch character: a vector of 128 functions
  (syntax (make-array 128 :initial-element nil)))      ; per character: NIL standard syntax, T whitespace, 0 no standard syntax

(defmethod print-object ((rt readtable) stream) (write-string "#<READTABLE>" stream))

(defparameter %readtable-cases '(:upcase :downcase :preserve :invert))

(defun readtable-case (readtable)
  "How READTABLE converts the case of symbol names: :UPCASE, :DOWNCASE, :PRESERVE or :INVERT."
  (nth (%rt-case readtable) %readtable-cases))

(defun (setf readtable-case) (mode readtable)
  "Set how READTABLE converts symbol names (:UPCASE, :DOWNCASE, :PRESERVE or :INVERT)."
  (let ((n (position mode %readtable-cases)))
    (unless n (error "~S is not a readtable case (one of ~{~S~^ ~})" mode %readtable-cases))
    (setf (%rt-case readtable) n)
    mode))

(defun %rt-code (char)
  (let ((code (char-code char)))
    (unless (< code 128) (error "Only characters below 128 can be macro characters, not ~S" char))
    code))

(defun %designated-function (f) (if (and (symbolp f) f) (symbol-function f) f))

(defvar %standard-readtable nil "The standard readtable: never modified once made.")

(defun copy-readtable (&optional (from *readtable*) to)
  "A copy of the readtable FROM (NIL: the standard readtable), into TO if it is given."
  (let* ((src (or from %standard-readtable)) (dst (or to (%make-readtable))))
    (setf (%rt-case dst) (%rt-case src)
          (%rt-macros dst) (copy-seq (%rt-macros src))
          (%rt-terminating dst) (copy-seq (%rt-terminating src))
          (%rt-dispatch dst) (map 'vector (lambda (d) (and d (copy-seq d))) (%rt-dispatch src))
          (%rt-syntax dst) (copy-seq (%rt-syntax src)))
    dst))

(defun set-macro-character (char function &optional non-terminating-p (readtable *readtable*))
  "Make CHAR a macro character: reading it calls FUNCTION with the stream and CHAR, which returns the object
read (or no values, for something like a comment).  Unless NON-TERMINATING-P, CHAR also ends tokens."
  (let ((code (%rt-code char)))
    (setf (svref (%rt-macros readtable) code) (%designated-function function)
          (svref (%rt-terminating readtable) code) (not non-terminating-p)
          (svref (%rt-dispatch readtable) code) nil
          (svref (%rt-syntax readtable) code) 0)
    t))

;;; The built-in syntax, as functions (what GET-MACRO-CHARACTER returns for a standard character)
(defun %standard-macro-function (char)
  (case char
    (#\; (lambda (stream c) (declare (ignore c)) (read-line stream nil) (values)))
    ((#\( #\) #\' #\` #\, #\" #\#)                      ; the function reads as CHAR whatever character it is copied to
     (lambda (stream c) (declare (ignore c)) (%read-standard-macro stream char)))
    (t nil)))

(defun get-macro-character (char &optional (readtable *readtable*))
  "The function of the macro character CHAR (or NIL), and whether it is non-terminating."
  (let* ((rt (or readtable %standard-readtable)) (code (char-code char))
         (fn (and (< code 128) (svref (%rt-macros rt) code))))
    (cond (fn (values fn (not (svref (%rt-terminating rt) code))))
          ((and (< code 128) (svref (%rt-syntax rt) code)) (values nil nil))   ; made whitespace or a constituent
          ((%standard-macro-function char) (values (%standard-macro-function char) (char= char #\#)))
          (t (values nil nil)))))

(defun %whitespace-char-p (char readtable)
  "True when CHAR is whitespace[2] in READTABLE."
  (let* ((code (char-code char)) (syn (and (< code 128) (svref (%rt-syntax readtable) code))))
    (cond ((< code 128) (cond ((eq syn t) t) (syn nil) (t (and (member code '(9 10 11 12 13 32)) t))))
          (t nil))))

(defun %read-dispatch-arg (stream)
  (let ((n nil))
    (loop (let ((c (read-char stream)))
            (if (digit-char-p c) (setq n (+ (* 10 (or n 0)) (digit-char-p c))) (return (values c n)))))))

(defun make-dispatch-macro-character (char &optional non-terminating-p (readtable *readtable*))
  "Make CHAR a dispatching macro character: it reads an optional decimal argument and a sub-character and
calls the function set with SET-DISPATCH-MACRO-CHARACTER for it."
  (let ((code (%rt-code char)))
    (set-macro-character char
                         (lambda (stream c)
                           (multiple-value-bind (sub arg) (%read-dispatch-arg stream)
                             (let ((fn (get-dispatch-macro-character c sub)))
                               (unless fn (error "READ error: no ~C~C syntax" c sub))
                               (funcall fn stream sub arg))))
                         non-terminating-p readtable)
    (setf (svref (%rt-dispatch readtable) code) (make-array 128 :initial-element nil))
    t))

(defun set-dispatch-macro-character (disp-char sub-char function &optional (readtable *readtable*))
  "Make DISP-CHAR followed by SUB-CHAR call FUNCTION with the stream, SUB-CHAR and the numeric argument (or NIL)."
  (let ((table (svref (%rt-dispatch readtable) (%rt-code disp-char))))
    (unless table (error "~S is not a dispatching macro character" disp-char))
    (setf (svref table (char-code (char-upcase sub-char))) (%designated-function function))
    t))

(defparameter %standard-sharp-chars "'(\\:|+-XBOC.")

(defun %read-sharp-a (contents rank)
  "#rankA(contents): an array of RANK dimensions with the nested CONTENTS."
  (unless rank (error "READ error: #A needs a rank, as in #2A((1 2) (3 4))"))
  (let ((dims nil) (x contents))
    (dotimes (i rank) (setq dims (cons (length x) dims) x (if (> (length x) 0) (elt x 0) nil)))
    (if (= rank 1) (coerce contents 'vector) (make-array (reverse dims) :initial-contents contents))))

(defun get-dispatch-macro-character (disp-char sub-char &optional (readtable *readtable*))
  "The function for DISP-CHAR SUB-CHAR, or NIL."
  (let* ((rt (or readtable %standard-readtable)) (table (svref (%rt-dispatch rt) (%rt-code disp-char)))
         (fn (and table (< (char-code sub-char) 128) (svref table (char-code (char-upcase sub-char))))))
    (cond (fn fn)
          ((and (char= disp-char #\#) (find (char-upcase sub-char) %standard-sharp-chars))
           (lambda (stream c n) (declare (ignore n)) (%read-standard-dispatch stream c n)))
          ((and (null table) (not (char= disp-char #\#))) (error "~S is not a dispatching macro character" disp-char))
          (t nil))))

(defun set-syntax-from-char (to-char from-char &optional (to-readtable *readtable*) (from-readtable nil))
  "Give TO-CHAR in TO-READTABLE the syntax FROM-CHAR has in FROM-READTABLE (the standard readtable if NIL): whitespace,
a macro character (with its function, terminating-ness and dispatch table), or a constituent.  Setting a character
from a whitespace character makes it separate tokens; setting a standard whitespace character from a letter makes it a
constituent.  TO-CHAR must be below 128."
  (let* ((from (char-code from-char)) (src (or from-readtable %standard-readtable)) (dst to-readtable)
         (to (if (< (char-code to-char) 128) (char-code to-char) -1)))
    (when (< to 0)                       ; characters above 127 are always constituents
      (when (or (get-macro-character from-char src) (%whitespace-char-p from-char src))
        (error "Only characters below 128 can be given macro or whitespace syntax, not ~S" to-char))
      (return-from set-syntax-from-char t))
    (multiple-value-bind (fn non-term) (get-macro-character from-char src)
      (cond (fn
             (setf (svref (%rt-macros dst) to) fn
                   (svref (%rt-terminating dst) to) (not non-term)
                   (svref (%rt-dispatch dst) to) (let ((d (and (< from 128) (svref (%rt-dispatch src) from)))) (and d (copy-seq d)))
                   (svref (%rt-syntax dst) to) 0))
            (t
             (setf (svref (%rt-macros dst) to) nil
                   (svref (%rt-terminating dst) to) nil
                   (svref (%rt-dispatch dst) to) nil
                   (svref (%rt-syntax dst) to) (if (%whitespace-char-p from-char src) t 0)))))
    t))

(defun read-delimited-list (char &optional stream recursive-p)
  "Read objects up to CHAR (which should be a terminating macro character) and return them as a list."
  (declare (ignore recursive-p))
  (%read-delimited-list char (%in stream)))

(defmacro with-standard-io-syntax (&body body)
  "Run BODY with the standard reader and printer settings."
  `(let ((*readtable* %standard-readtable) (*read-eval* t) (*package* (find-package "COMMON-LISP-USER"))
         (*print-pretty* nil) (*print-length* nil) (*print-level* nil) (*print-circle* nil) (*print-case* :upcase)
         (*print-base* 10) (*print-radix* nil))
     ,@body))

;;; The standard readtable: the built-in syntax, with # as a dispatching character whose user-visible table holds
;;; the extensions written in Lisp (#P, #C, #A).
(setq %standard-readtable (%make-readtable))
(setf (svref (%rt-dispatch %standard-readtable) (char-code #\#)) (make-array 128 :initial-element nil))
(set-dispatch-macro-character #\# #\A (lambda (stream c n) (declare (ignore c)) (%read-sharp-a (read stream t nil t) n)) %standard-readtable)
;;; #*0101: a bit vector (#n* repeats the last bit, or pads with 0)
(defun %read-sharp-star (stream c n)
  (declare (ignore c))
  (let ((bits nil))
    (loop (let ((ch (peek-char nil stream nil nil)))
            (if (and ch (or (char= ch #\0) (char= ch #\1)))
                (progn (read-char stream) (setq bits (cons (if (char= ch #\1) 1 0) bits)))
                (return))))
    (setq bits (nreverse bits))
    (let* ((len (or n (length bits))) (v (make-array len :element-type 'bit)) (last 0) (i 0))
      (when (> (length bits) len) (error "READ error: #*~D has ~D bits" len (length bits)))
      (dolist (b bits) (setq last b) (%aset v i b) (setq i (+ i 1)))
      (loop while (< i len) do (%aset v i last) (setq i (+ i 1)))
      v)))
(set-dispatch-macro-character #\# #\* #'%read-sharp-star %standard-readtable)
(setq *readtable* (copy-readtable %standard-readtable))
