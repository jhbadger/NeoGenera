;;;; 00-lexer.lisp -- the Pascal Tool Kit: tokens.
;;;;
;;;; The Pascal Tool Kit compiles ISO Pascal and Pascal/VS into Lisp, which the system compiler then compiles
;;;; to bytecode, as the Symbolics Pascal Tool Kit did on Genera.  This file turns source text into tokens.
;;;; Identifiers are case-insensitive (a token's value is the upper-case name); every token keeps the byte
;;;; offsets of its text so the editor can map errors and language units back to the buffer.  Template items
;;;; of the editor (text between U+2282 and U+2283, "⊂statement⊃") are tokens of their own.

(in-package :cl)

(defvar *pascal-default-dialect* :iso
  "The dialect of Pascal used when a file or buffer does not name one in its attribute list: :ISO or :VS.")

(defstruct (ptoken (:conc-name tok-) (:constructor %make-ptoken) (:copier nil))
  kind value text start end line col file)

(define-condition pascal-syntax-error (error)
  ((message :initarg :message :reader pascal-error-message)
   (start :initarg :start :initform nil :reader pascal-error-start)
   (end :initarg :end :initform nil :reader pascal-error-end)
   (line :initarg :line :initform nil :reader pascal-error-line)
   (col :initarg :col :initform nil :reader pascal-error-col)
   (file :initarg :file :initform nil :reader pascal-error-file))
  (:report (lambda (c s)
             (if (pascal-error-line c)
                 (format s "~@[~A: ~]line ~D, column ~D: ~A" (pascal-error-file c) (pascal-error-line c) (pascal-error-col c)
                         (pascal-error-message c))
                 (write-string (pascal-error-message c) s)))))

(defparameter *pascal-iso-reserved*
  '("AND" "ARRAY" "BEGIN" "CASE" "CONST" "DIV" "DO" "DOWNTO" "ELSE" "END" "FILE" "FOR" "FUNCTION" "GOTO" "IF" "IN"
    "LABEL" "MOD" "NIL" "NOT" "OF" "OR" "PACKED" "PROCEDURE" "PROGRAM" "RECORD" "REPEAT" "SET" "THEN" "TO" "TYPE"
    "UNTIL" "VAR" "WHILE" "WITH"
    ;; Tool Kit extensions to both dialects
    "OTHERWISE" "UNIT_SPECIFICATION" "UNIT_IMPLEMENTATION" "USING_UNITS")
  "The reserved words of ISO Pascal, with the Tool Kit's extensions.")

(defparameter *pascal-vs-reserved* '("LEAVE" "CONTINUE" "RETURN" "XOR")
  "Words reserved only in the Pascal/VS dialect.")

(defun pascal-reserved-word-p (name &optional (dialect *pascal-default-dialect*))
  "True when NAME (any case) is a reserved word of DIALECT."
  (let ((u (string-upcase name)))
    (or (member u *pascal-iso-reserved* :test #'string=)
        (and (eq dialect :vs) (member u *pascal-vs-reserved* :test #'string=)))))

(defparameter +template-open-string+ "⊂" "The text (UTF-8) opening an editor template item.")
(defparameter +template-close-string+ "⊃" "The text (UTF-8) closing an editor template item.")
(defparameter +template-mark-length+ (length +template-open-string+) "The length of the template marks (characters).")

(defun %pas-ident-start-p (c) (or (alpha-char-p c) (char= c #\_)))
(defun %pas-ident-char-p (c) (or (alpha-char-p c) (digit-char-p c) (char= c #\_)))

(defun %pas-lex-error (msg start line col file)
  (error 'pascal-syntax-error :message msg :start start :end (+ start 1) :line line :col col :file file))

(defun pascal-tokenize (source &key (dialect *pascal-default-dialect*) file (directory "") (include-depth 0))
  "The tokens of the Pascal SOURCE string, as a list ending with an :EOF token.  Signals PASCAL-SYNTAX-ERROR for
an unterminated comment or string.  %INCLUDE directives splice in the tokens of the named file (looked up in
DIRECTORY); those tokens keep the position of the directive."
  (let ((i 0) (n (length source)) (line 1) (line-start 0) (toks nil))
    (labels ((peek (k) (if (< (+ i k) n) (char source (+ i k)) nil))
             (emit (kind value start)
               (push (%make-ptoken :kind kind :value value :text (subseq source start i) :start start :end i
                                   :line line :col (+ 1 (- start line-start)) :file file)
                     toks))
             (newline-at (k) (setq line (+ line 1) line-start (+ k 1)))
             (skip-comment (close-char close2 start)
               ;; skip to the closing } or *), counting lines
               (let ((l0 line) (c0 (+ 1 (- start line-start))))
                 (loop
                   (when (>= i n) (%pas-lex-error "Unterminated comment" start l0 c0 file))
                   (let ((c (char source i)))
                     (cond ((char= c #\Newline) (newline-at i) (setq i (+ i 1)))
                           ((and (null close2) (char= c close-char)) (setq i (+ i 1)) (return))
                           ((and close2 (char= c close-char) (< (+ i 1) n) (char= (char source (+ i 1)) close2))
                            (setq i (+ i 2)) (return))
                           (t (setq i (+ i 1))))))))
             (sym (value len start) (setq i (+ i len)) (emit :sym value start)))
      (loop
        (when (>= i n) (return))
        (let ((c (char source i)) (start i))
          (cond
            ((char= c #\Newline) (newline-at i) (setq i (+ i 1)))
            ((member c '(#\Space #\Tab #\Return #\Page)) (setq i (+ i 1)))
            ((char= c #\{) (setq i (+ i 1)) (skip-comment #\} nil start))
            ((and (char= c #\() (eql (peek 1) #\*)) (setq i (+ i 2)) (skip-comment #\* #\) start))
            ;; template item ⊂...⊃
            ((and (char= c (char +template-open-string+ 0)) (<= (+ i +template-mark-length+) n)
                  (string= (subseq source i (+ i +template-mark-length+)) +template-open-string+))
             (let ((e (search +template-close-string+ source :start2 (+ i +template-mark-length+))))
               (setq i (if e (+ e +template-mark-length+) n))
               (emit :template (subseq source start i) start)))
            ((%pas-ident-start-p c)
             (loop (when (or (>= i n) (not (%pas-ident-char-p (char source i)))) (return)) (setq i (+ i 1)))
             (let ((name (string-upcase (subseq source start i))))
               (if (pascal-reserved-word-p name dialect)
                   (emit :kw (intern name "KEYWORD") start)
                   (emit :id name start))))
            ((digit-char-p c)
             (loop (when (or (>= i n) (not (digit-char-p (char source i)))) (return)) (setq i (+ i 1)))
             (let ((realp nil))
               (when (and (< (+ i 1) n) (char= (char source i) #\.) (digit-char-p (char source (+ i 1))))
                 (setq realp t i (+ i 1))
                 (loop (when (or (>= i n) (not (digit-char-p (char source i)))) (return)) (setq i (+ i 1))))
               (when (and (< i n) (char-equal (char source i) #\E)
                          (let ((j (+ i 1)))
                            (when (and (< j n) (member (char source j) '(#\+ #\-))) (setq j (+ j 1)))
                            (and (< j n) (digit-char-p (char source j)))))
                 (setq realp t i (+ i 1))
                 (when (member (char source i) '(#\+ #\-)) (setq i (+ i 1)))
                 (loop (when (or (>= i n) (not (digit-char-p (char source i)))) (return)) (setq i (+ i 1))))
               (let ((text (subseq source start i)))
                 (if realp
                     (emit :real (%pas-parse-real text) start)
                     (emit :int (parse-integer text) start)))))
            ((char= c #\')
             (let ((out (make-string-output-stream)) (l0 line) (c0 (+ 1 (- start line-start))))
               (setq i (+ i 1))
               (loop
                 (when (or (>= i n) (char= (char source i) #\Newline)) (%pas-lex-error "Unterminated string" start l0 c0 file))
                 (let ((ch (char source i)))
                   (cond ((and (char= ch #\') (eql (peek 1) #\')) (write-char #\' out) (setq i (+ i 2)))
                         ((char= ch #\') (setq i (+ i 1)) (return))
                         (t (write-char ch out) (setq i (+ i 1))))))
               (emit :str (get-output-stream-string out) start)))
            ((char= c #\%)
             ;; compiler directive: %include
             (let ((j (+ i 1)))
               (loop (when (or (>= j n) (not (%pas-ident-char-p (char source j)))) (return)) (setq j (+ j 1)))
               (let ((word (string-upcase (subseq source (+ i 1) j))) (eol (or (%string-position-char source #\Newline i) n)))
                 (cond ((string= word "INCLUDE")
                        (let* ((arg (string-trim '(#\Space #\Tab) (subseq source j eol)))
                               (name (cond ((and (> (length arg) 0) (char= (char arg 0) #\'))
                                            (subseq arg 1 (or (%string-position-char arg #\' 1) (length arg))))
                                           (t (let ((k 0)) (loop (when (or (>= k (length arg)) (not (%pas-ident-char-p (char arg k)))) (return)) (setq k (+ k 1)))
                                                (subseq arg 0 k)))))
                               (l0 line) (c0 (+ 1 (- start line-start))))
                          (setq i eol)
                          (when (> include-depth 8) (%pas-lex-error "%INCLUDE nested too deeply" start l0 c0 file))
                          (let ((path (%pascal-include-path name directory)))
                            (unless path (%pas-lex-error (format nil "%INCLUDE: no file ~A" name) start l0 c0 file))
                            (dolist (tk (butlast (pascal-tokenize (%read-file-string path) :dialect dialect :file (namestring path)
                                                                  :directory (directory-namestring path)
                                                                  :include-depth (+ include-depth 1))))
                              (setf (tok-start tk) start (tok-end tk) eol (tok-line tk) l0 (tok-col tk) c0)
                              (push tk toks)))))
                       (t (%pas-lex-error (format nil "Unknown directive %~A" (string-downcase word)) start line (+ 1 (- start line-start)) file))))))
            (t
             (let ((c1 (peek 1)))
               (case c
                 (#\: (if (eql c1 #\=) (sym :assign 2 start) (sym :colon 1 start)))
                 (#\. (cond ((eql c1 #\.) (sym :dotdot 2 start)) ((eql c1 #\)) (sym :rbrack 2 start)) (t (sym :dot 1 start))))
                 (#\; (sym :semi 1 start))
                 (#\, (sym :comma 1 start))
                 (#\( (if (eql c1 #\.) (sym :lbrack 2 start) (sym :lparen 1 start)))
                 (#\) (sym :rparen 1 start))
                 (#\[ (sym :lbrack 1 start))
                 (#\] (sym :rbrack 1 start))
                 ((#\^ #\@) (sym :caret 1 start))
                 (#\+ (sym :plus 1 start))
                 (#\- (if (eql c1 #\>) (sym :caret 2 start) (sym :minus 1 start)))
                 (#\* (sym :star 1 start))
                 (#\/ (sym :slash 1 start))
                 (#\= (sym :eq 1 start))
                 (#\< (cond ((eql c1 #\>) (sym :ne 2 start)) ((eql c1 #\=) (sym :le 2 start)) (t (sym :lt 1 start))))
                 (#\> (if (eql c1 #\=) (sym :ge 2 start) (sym :gt 1 start)))
                 (#\| (if (eql c1 #\|) (sym :concat 2 start) (sym :or 1 start)))
                 (#\& (sym :and 1 start))
                 (#\~ (sym :not 1 start))
                 (t (%pas-lex-error (format nil "Unexpected character ~S" c) start line (+ 1 (- start line-start)) file)))))))))
    (push (%make-ptoken :kind :eof :value nil :text "" :start n :end n :line line :col (+ 1 (- n line-start)) :file file) toks)
    (nreverse toks)))

(defun %pas-parse-real (text)
  "The double float written as TEXT (digits, optional fraction, optional exponent)."
  (let* ((e (position-if (lambda (c) (char-equal c #\E)) text))
         (mant (subseq text 0 (or e (length text))))
         (ex (if e (parse-integer text :start (+ e 1)) 0))
         (dot (%string-position-char mant #\. 0))
         (digits (if dot (%string-concat (subseq mant 0 dot) (subseq mant (+ dot 1))) mant))
         (scale (if dot (- (length mant) dot 1) 0)))
    (float (* (parse-integer digits) (expt 10 (- ex scale))) 1d0)))

(defun %pascal-include-path (name directory)
  (dolist (cand (list name (%string-concat name ".pas") (%string-concat name ".pascal")) nil)
    (let ((p (if (and (> (length cand) 0) (char= (char cand 0) #\/)) cand (%string-concat (or directory "") cand))))
      (when (probe-file p) (return p)))))

(defun %read-file-string (path)
  (with-open-file (s path)
    (let ((out (make-string-output-stream)))
      (loop (let ((line (read-line s nil :eof)))
              (when (eq line :eof) (return))
              (write-string line out) (write-char #\Newline out)))
      (get-output-stream-string out))))
