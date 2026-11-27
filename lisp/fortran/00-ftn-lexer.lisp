;;;; 00-lexer.lisp -- the FORTRAN Tool Kit: fixed-format source and tokens.
;;;;
;;;; The FORTRAN Tool Kit compiles ANSI FORTRAN 77 (X3.9-1978) with the Symbolics extensions -- arbitrary-size
;;;; integers, uninitialized-value detection, DO WHILE and ENDDO, INCLUDE, IMPLICIT NONE, the Department of
;;;; Defense bit operations, binary patterns -- into Lisp, which the system compiler then compiles to bytecode,
;;;; as the Symbolics FORTRAN product did on Genera (1982).  This file reads fixed-format source: columns 1-5
;;;; hold the statement label, column 6 a continuation mark, columns 7-72 the statement, and everything after
;;;; column 72 (the sequence field) is ignored.  A "card" is one line; a logical statement is a card with its
;;;; continuation cards.  Source is case-insensitive except in character constants and Hollerith data.
;;;; Every statement keeps the byte offsets of its cards so the editor can map errors and routines back to a buffer.

(in-package :cl)

(defvar *f77-default-package* "FTN-USER" "The package FORTRAN source is compiled into when its attribute list names none.")
(defvar *f77-include-path* nil "Extra directories (strings ending in /) searched for INCLUDE files, after the source's own.")

(define-condition f77-syntax-error (error)
  ((message :initarg :message :reader f77-error-message)
   (start :initarg :start :initform nil :reader f77-error-start)
   (end :initarg :end :initform nil :reader f77-error-end)
   (line :initarg :line :initform nil :reader f77-error-line)
   (col :initarg :col :initform nil :reader f77-error-col)
   (file :initarg :file :initform nil :reader f77-error-file))
  (:report (lambda (c s)
             (if (f77-error-line c)
                 (format s "~@[~A: ~]line ~D, column ~D: ~A" (f77-error-file c) (f77-error-line c) (f77-error-col c)
                         (f77-error-message c))
                 (write-string (f77-error-message c) s)))))

;;; ---- logical statements -------------------------------------------------------------------------------

(defstruct (fstmt (:conc-name fs-) (:constructor %make-fstmt) (:copier nil))
  label text start end line col file (trunc nil))

(defmethod print-object ((s fstmt) stream)
  (format stream "#<Fortran statement line ~D ~A>" (fs-line s) (fs-text s)))

(defun %f7-lex-error (msg start line col file)
  (error 'f77-syntax-error :message msg :start start :end (+ start 1) :line line :col col :file file))

(defun %f7-blank-p (c) (member c '(#\Space #\Tab #\Return #\Page)))

(defun %f7-strip-comment (text)
  "TEXT without a trailing ! comment (a ! outside character constants)."
  (let ((q nil) (n (length text)))
    (dotimes (i n text)
      (let ((c (char text i)))
        (cond (q (when (char= c q) (setq q nil)))
              ((or (char= c #\') (char= c #\")) (setq q c))
              ((char= c #\!) (return (subseq text 0 i))))))))

(defun %f7-split-lines (source)
  "A list of (text . offset) for each line of SOURCE."
  (let ((acc nil) (i 0) (n (length source)))
    (loop
      (when (>= i n) (return))
      (let* ((e (or (%string-position-char source #\Newline i) n))
             (text (subseq source i e)))
        (when (and (> (length text) 0) (char= (char text (- (length text) 1)) #\Return))
          (setq text (subseq text 0 (- (length text) 1))))
        (push (cons text i) acc)
        (setq i (+ e 1))))
    (nreverse acc)))

(defun %f7-truncated-p (text)
  "True when TEXT has something other than blanks and digits (a sequence number) beyond column 72."
  (and (> (length text) 72)
       (some (lambda (c) (not (or (%f7-blank-p c) (digit-char-p c)))) (subseq text 72))))

(defun %f7-card-fields (text)
  "Split one card into (values label-string continuation-p statement-text), or NIL for a comment or blank card.
Columns 1-5 are the label, column 6 the continuation mark, columns 7-72 the statement.  A tab in the first six
columns ends the label field; if a digit 1-9 follows, the card is a continuation (the DEC convention)."
  (let ((n (length text)))
    (cond
      ((= n 0) nil)
      ((member (char text 0) '(#\C #\c #\* #\!)) nil)
      ((every #'%f7-blank-p text) nil)
      (t
       (let ((tab (position #\Tab text)))
         (if (and tab (< tab 6))
             (let* ((rest (subseq text (+ tab 1))) (cont (and (> (length rest) 0) (digit-char-p (char rest 0)) (char/= (char rest 0) #\0))))
               (values (string-trim " " (subseq text 0 tab)) (and cont t)
                       (if cont (subseq rest 1 (min (length rest) 66)) (subseq rest 0 (min (length rest) 66)))))
             (let* ((padded (if (< n 6) (%string-concat text (make-string (- 6 n) :initial-element #\Space)) text))
                    (label (string-trim " " (subseq padded 0 5)))
                    (cont (let ((c (char padded 5))) (and (not (%f7-blank-p c)) (char/= c #\0))))
                    (body (if (> (length padded) 6) (subseq padded 6 (min (length padded) 72)) "")))
               (values label cont body))))))))

(defun %f7-include-path (name directory)
  (dolist (dir (cons directory *f77-include-path*) nil)
    (dolist (ext '("" ".f" ".for" ".ftn" ".f77" ".fortran" ".inc" ".i") nil)
      (let* ((cand (%string-concat name ext))
             (p (if (and (> (length cand) 0) (char= (char cand 0) #\/)) cand (%string-concat (or dir "") cand))))
        (when (probe-file p) (return-from %f7-include-path p))))))

(defun %f7-include-name (squeezed)
  "The file named by the INCLUDE statement whose squeezed text is SQUEEZED, or NIL when it is not one."
  (when (and (> (length squeezed) 8) (string= (subseq squeezed 0 7) "INCLUDE") (char= (char squeezed 7) #\'))
    (let ((e (position #\' squeezed :start 8)))
      (when e (subseq squeezed 8 e)))))

(defun f77-read-statements (source &key file (directory "") (include-depth 0))
  "The logical statements of the fixed-format SOURCE: (values statements errors).  Comment and blank cards are
dropped, continuation cards joined, and INCLUDE files spliced in (their statements take the position of the
INCLUDE).  ERRORS are F77-SYNTAX-ERRORs for malformed label fields and orphan continuations."
  (let ((stmts nil) (errors nil) (cur nil) (line-no 0))
    (labels ((finish ()
               (when cur
                 (let* ((squeezed (handler-case (%f7-squeeze (fs-text cur)) (f77-syntax-error () nil)))
                        (inc (and squeezed (%f7-include-name squeezed))))
                   (if inc
                       (let ((path (%f7-include-path inc directory)))
                         (cond ((> include-depth 8)
                                (push (make-condition 'f77-syntax-error :message "INCLUDE nested too deeply" :start (fs-start cur)
                                                      :end (fs-end cur) :line (fs-line cur) :col (fs-col cur) :file file) errors))
                               ((null path)
                                (push (make-condition 'f77-syntax-error :message (format nil "INCLUDE: no file ~A" inc) :start (fs-start cur)
                                                      :end (fs-end cur) :line (fs-line cur) :col (fs-col cur) :file file) errors))
                               (t (multiple-value-bind (ss es)
                                      (f77-read-statements (%read-file-string path) :file (namestring path)
                                                           :directory (directory-namestring path) :include-depth (+ include-depth 1))
                                    (dolist (s ss)
                                      (setf (fs-start s) (fs-start cur) (fs-end s) (fs-end cur) (fs-line s) (fs-line cur) (fs-col s) (fs-col cur))
                                      (push s stmts))
                                    (dolist (e es) (push e errors))))))
                       (push cur stmts)))
                 (setq cur nil))))
      (dolist (ln (%f7-split-lines source))
        (setq line-no (+ line-no 1))
        (let ((text (car ln)) (off (cdr ln)))
          (multiple-value-bind (label cont body) (%f7-card-fields text)
            (when (or label cont body)
              (let ((body (%f7-strip-comment body)) (end (+ off (length text))))
                (cond
                  (cont
                   (if cur
                       (let ((prev (fs-text cur)))
                         (setf (fs-text cur) (%string-concat (if (< (length prev) 66) (%string-concat prev (make-string (- 66 (length prev)) :initial-element #\Space)) prev) body)
                               (fs-end cur) end (fs-trunc cur) (or (fs-trunc cur) (%f7-truncated-p text))))
                       (push (make-condition 'f77-syntax-error :message "Continuation card with no statement to continue"
                                             :start off :end end :line line-no :col 6 :file file) errors)))
                  (t
                   (finish)
                   (cond ((every #'%f7-blank-p body)
                          (unless (string= label "")
                            (setq cur (%make-fstmt :label nil :text "CONTINUE" :start off :end end :line line-no :col 1 :file file))))
                         (t
                          (let ((lab (cond ((string= label "") nil)
                                           ((every #'digit-char-p label) (parse-integer label))
                                           (t (push (make-condition 'f77-syntax-error :message (format nil "Invalid label field ~S" label)
                                                                    :start off :end end :line line-no :col 1 :file file) errors)
                                              nil))))
                            (setq cur (%make-fstmt :label lab :text body :start off :end end :line line-no
                                                   :col (+ 1 (or (position-if-not #'%f7-blank-p body) 0) 6) :file file :trunc (%f7-truncated-p text)))))))))))))
      (finish))
    (values (nreverse stmts) (nreverse errors))))

;;; ---- squeezing a statement: blanks out, upper case, Hollerith to strings -----------------------------

(defun %f7-quote (s)
  "S as a quoted character constant ('' for an embedded quote)."
  (let ((out (make-string-output-stream)))
    (write-char #\' out)
    (dotimes (i (length s)) (let ((c (char s i))) (when (char= c #\') (write-char #\' out)) (write-char c out)))
    (write-char #\' out)
    (get-output-stream-string out)))

(defun %f7-squeeze (text)
  "TEXT without blanks outside character constants, in upper case outside them; \"abc\" constants become 'abc' and
Hollerith constants (nHtext) become character constants.  Signals F77-SYNTAX-ERROR for an unterminated constant."
  (let ((out (make-string-output-stream)) (i 0) (n (length text)) (prev nil))
    (loop
      (when (>= i n) (return))
      (let ((c (char text i)))
        (cond
          ((%f7-blank-p c) (setq i (+ i 1)))
          ((or (char= c #\') (char= c #\"))
           (let ((j (+ i 1)) (buf (make-string-output-stream)))
             (loop
               (when (>= j n) (error 'f77-syntax-error :message "Unterminated character constant" :start i :end (+ i 1)))
               (let ((d (char text j)))
                 (cond ((and (char= d c) (< (+ j 1) n) (char= (char text (+ j 1)) c)) (write-char c buf) (setq j (+ j 2)))
                       ((char= d c) (setq j (+ j 1)) (return))
                       (t (write-char d buf) (setq j (+ j 1))))))
             (write-string (%f7-quote (get-output-stream-string buf)) out)
             (setq prev #\' i j)))
          ((and (digit-char-p c) (or (null prev) (member prev '(#\( #\, #\/ #\=))))
           ;; possibly a Hollerith constant nH...
           (let ((j i))
             (loop (when (or (>= j n) (not (digit-char-p (char text j)))) (return)) (setq j (+ j 1)))
             (if (and (< j n) (char-equal (char text j) #\H) (<= 1 (parse-integer text :start i :end j) 100000))
                 (let* ((len (parse-integer text :start i :end j)) (e (min n (+ j 1 len))))
                   (write-string (%f7-quote (let ((s (subseq text (+ j 1) e)))
                                              (if (and (< (length s) len) (<= (- len (length s)) 80))
                                                  (%string-concat s (make-string (- len (length s)) :initial-element #\Space))
                                                  s)))
                                 out)
                   (setq prev #\' i e))
                 (progn (write-string (subseq text i j) out) (setq prev (char text (- j 1)) i j)))))
          (t (write-char (char-upcase c) out) (setq prev (char-upcase c) i (+ i 1))))))
    (get-output-stream-string out)))

;;; ---- tokens -------------------------------------------------------------------------------------------
;;; A token is (KIND . VALUE): :INT, :REAL (a double), :DREAL (a double written with D), :STR, :NAME, :LOGICAL,
;;; :DOT (a relational or logical operator keyword) and :OP (an operator or punctuation string).

(defparameter *f77-dot-operators*
  '(("EQ" . :eq) ("NE" . :ne) ("LT" . :lt) ("LE" . :le) ("GT" . :gt) ("GE" . :ge)
    ("AND" . :and) ("OR" . :or) ("NOT" . :not) ("EQV" . :eqv) ("NEQV" . :neqv) ("XOR" . :xor)))

(defun %f7-name-char-p (c) (or (alpha-char-p c) (digit-char-p c) (char= c #\_) (char= c #\$)))

(defun %f7-rational-double (r)
  "The double float nearest to the rational R, rounding half to even (FLOAT of a ratio is not always correctly rounded)."
  (cond ((zerop r) 0d0)
        ((< r 0) (- (%f7-rational-double (- r))))
        (t
         (let ((e (- (integer-length (numerator r)) (integer-length (denominator r)) 53)))
           (flet ((scaled () (if (>= e 0) (/ r (expt 2 e)) (* r (expt 2 (- e))))))
             (loop
               (let ((q (scaled)))
                 (cond ((>= q (expt 2 53)) (setq e (+ e 1)))
                       ((< q (expt 2 52)) (setq e (- e 1)))
                       (t (return)))))
             (let ((m (round (scaled))))
               (* (float m 1d0) (expt 2d0 e))))))))

(defun %f7-parse-real (text)
  "The double float written as TEXT (digits, optional fraction, optional exponent with E or D)."
  (let* ((e (position-if (lambda (c) (member c '(#\E #\D #\e #\d))) text))
         (mant (subseq text 0 (or e (length text))))
         (ex (if e (parse-integer text :start (+ e 1)) 0))
         (dot (position #\. mant))
         (digits (if dot (%string-concat (subseq mant 0 dot) (subseq mant (+ dot 1))) mant))
         (scale (if dot (- (length mant) dot 1) 0)))
    (if (every (lambda (c) (char= c #\0)) digits)
        0d0
        (%f7-rational-double (* (parse-integer (if (string= digits "") "0" digits)) (expt 10 (- ex scale)))))))

(defun %f7-radix-constant (name str)
  "The integer for a binary pattern B'101', O'17', Z'FF' or X'FF'; NIL if NAME is not one of those letters or STR is bad."
  (let ((radix (cond ((string= name "B") 2) ((string= name "O") 8) ((or (string= name "Z") (string= name "X")) 16) (t nil))))
    (and radix (> (length str) 0) (every (lambda (c) (digit-char-p c radix)) str) (parse-integer str :radix radix))))

(defun f77-tokenize (s &optional (start 0) (end (length s)))
  "The tokens of the squeezed statement text S between START and END, as a list of (KIND . VALUE)."
  (let ((i start) (toks nil))
    (labels ((peek (k) (if (< (+ i k) end) (char s (+ i k)) nil))
             (emit (kind value) (push (cons kind value) toks))
             (scan-digits () (loop (when (or (>= i end) (not (digit-char-p (char s i)))) (return)) (setq i (+ i 1))))
             (dot-operator-at (p)
               ;; p indexes a ".": the length of ".WORD." if that is a dot operator or logical constant
               (let ((q (+ p 1)))
                 (loop (when (or (>= q end) (not (alpha-char-p (char s q)))) (return)) (setq q (+ q 1)))
                 (if (and (< q end) (char= (char s q) #\.) (> q (+ p 1))) (- (+ q 1) p) nil))))
      (loop
        (when (>= i end) (return))
        (let ((c (char s i)) (st i))
          (cond
            ((char= c #\')
             (let ((j (+ i 1)) (buf (make-string-output-stream)))
               (loop
                 (when (>= j end) (error 'f77-syntax-error :message "Unterminated character constant"))
                 (let ((d (char s j)))
                   (cond ((and (char= d #\') (< (+ j 1) end) (char= (char s (+ j 1)) #\')) (write-char #\' buf) (setq j (+ j 2)))
                         ((char= d #\') (setq j (+ j 1)) (return))
                         (t (write-char d buf) (setq j (+ j 1))))))
               (setq i j)
               (let ((str (get-output-stream-string buf)))
                 ;; 'FF'X and friends
                 (let ((p (and (< i end) (member (char s i) '(#\X #\Z #\O #\B))
                               (or (>= (+ i 1) end) (not (%f7-name-char-p (char s (+ i 1)))))
                               (%f7-radix-constant (string (char s i)) str))))
                   (if p (progn (setq i (+ i 1)) (emit :int p)) (emit :str str))))))
            ((or (alpha-char-p c) (char= c #\_) (char= c #\$))
             (loop (when (or (>= i end) (not (%f7-name-char-p (char s i)))) (return)) (setq i (+ i 1)))
             (let ((name (subseq s st i)))
               ;; B'101' O'17' Z'FF' X'FF'
               (if (and (< i end) (char= (char s i) #\') (= (length name) 1) (member (char name 0) '(#\B #\O #\Z #\X)))
                   (let ((e (position #\' s :start (+ i 1) :end end)))
                     (let ((p (and e (%f7-radix-constant name (subseq s (+ i 1) e)))))
                       (if p (progn (setq i (+ e 1)) (emit :int p)) (emit :name name))))
                   (emit :name name))))
            ((or (digit-char-p c) (and (char= c #\.) (peek 1) (digit-char-p (peek 1))))
             (scan-digits)
             (let ((realp nil) (dbl nil))
               (when (and (< i end) (char= (char s i) #\.) (not (dot-operator-at i)))
                 (setq realp t i (+ i 1))
                 (scan-digits))
               (when (and (< i end) (member (char s i) '(#\E #\D))
                          (let ((j (+ i 1)))
                            (when (and (< j end) (member (char s j) '(#\+ #\-))) (setq j (+ j 1)))
                            (and (< j end) (digit-char-p (char s j)))))
                 (setq realp t dbl (char= (char s i) #\D) i (+ i 1))
                 (when (member (char s i) '(#\+ #\-)) (setq i (+ i 1)))
                 (scan-digits))
               (let ((text (subseq s st i)))
                 (cond ((not realp) (emit :int (parse-integer text)))
                       (t (emit (if dbl :dreal :real) (%f7-parse-real text)))))))
            ((char= c #\.)
             (let ((len (dot-operator-at i)))
               (unless len (error 'f77-syntax-error :message "Stray \".\""))
               (let ((word (subseq s (+ i 1) (+ i len -1))))
                 (setq i (+ i len))
                 (cond ((string= word "TRUE") (emit :logical t))
                       ((string= word "FALSE") (emit :logical nil))
                       ((assoc word *f77-dot-operators* :test #'string=) (emit :dot (cdr (assoc word *f77-dot-operators* :test #'string=))))
                       (t (error 'f77-syntax-error :message (format nil "Unknown operator .~A." word)))))))
            ((char= c #\*) (if (eql (peek 1) #\*) (progn (setq i (+ i 2)) (emit :op "**")) (progn (setq i (+ i 1)) (emit :op "*"))))
            ((char= c #\/) (if (eql (peek 1) #\/) (progn (setq i (+ i 2)) (emit :op "//")) (progn (setq i (+ i 1)) (emit :op "/"))))
            ((member c '(#\+ #\- #\( #\) #\, #\= #\: #\& #\<))
             (setq i (+ i 1)) (emit :op (string c)))
            (t (error 'f77-syntax-error :message (format nil "Unexpected character ~S" c))))))
      (nreverse toks))))
