;;;; 50-editor.lisp -- the FORTRAN Tool Kit: FORTRAN mode in the editor.
;;;;
;;;; FORTRAN mode is deliberately unlike the syntax-directed Pascal and C modes: it knows the fixed-format source
;;;; fields (label in columns 1-5, continuation mark in column 6, statement from column 7), continuation cards, sequence
;;;; numbers and the column-72 limit.  Electric FORTRAN mode (a minor mode) wraps text typed past the wraparound column
;;;; and starts a continuation card for the rest.  Routines and statements come from the parser, so motion and marking
;;;; work on any text that parses; compiling is incremental per routine, as in the Pascal mode.
;;;;
;;;; Keys (Genera's in brackets):
;;;;   TAB     in columns 0-6 pad to the statement field, elsewhere indent the line      [TAB]
;;;;   C-j     end the line and open an indented statement line                         [LINE]
;;;;   C-M-j   open a continuation card                                                 [c-LINE]
;;;;   C-M-a C-M-e  routine start/end    C-M-f C-M-b  next/previous statement    C-M-h  mark routine
;;;;   C-c C-a show the compiled routine's name and argument types                       [c-sh-A]
;;;;   C-c C-c compile the region or routine at point    C-c C-b  compile buffer    C-c C-e  compile and execute
;;;;   C-c C-x execute the program at point              C-c C-w  edit compiler warnings
;;;;   C-^     merge this line with the previous one                                     [c-^]
;;;;   M-.     edit the definition of the routine named at point

(in-package :cl)

(defvar *f77-mode-map* (make-keymap "fortran"))
(defvar *f77-indent* 3 "Columns per level of nesting (block IF and DO) in FORTRAN mode.")
(defvar *f77-breakpoints* nil "The language breakpoints set in FORTRAN buffers: (buffer-name . line).  Recorded and listed; the run time does not stop at them.")

(setf (get 'fortran-mode 'mode-keymap) *f77-mode-map*
      (get 'fortran-mode 'mode-name) "FORTRAN")

(dolist (suffix '(".f" ".for" ".ftn" ".f77" ".fortran"))
  (unless (assoc suffix *auto-mode-alist* :test #'string=) (push (cons suffix 'fortran-mode) *auto-mode-alist*)))

(defun %f77-buffer-p (b) (eq (buffer-mode b) 'fortran-mode))

(defun %f77-wraparound-column ()
  "The column electric wrapping happens at: F77:WRAPAROUND-COLUMN, but never beyond the source limit 72."
  (let ((v (let ((s (find-symbol "WRAPAROUND-COLUMN" "F77"))) (and s (boundp s) (symbol-value s)))))
    (min 72 (if (and (integerp v) (> v 7)) v 72))))

(defun %f77-continuation-char ()
  (let* ((s (find-symbol "CONTINUATION-CHARACTER" "F77")) (v (and s (boundp s) (symbol-value s))))
    (cond ((and (stringp v) (> (length v) 0)) (char v 0)) ((characterp v) v) (t #\$))))

;;; ---- the buffer's attributes -----------------------------------------------------------------------------

(defun f77-buffer-attributes (b)
  "The attribute list of buffer B: (:mode :package)."
  (f77-file-attributes (if (> (buf-nlines b) 0) (buf-line b 0) "")))

(defun %f77-buffer-package (b)
  (let ((f (buffer-file b))) (%f77-file-package (or f (buffer-name b)) (f77-buffer-attributes b))))

(defun %f77-attribute-line (mode package)
  (format nil "C -*- Mode: ~A; Package: ~A -*-" mode package))

(defun %f77-set-attributes (b &key (package (%f77-buffer-package b)))
  "Write (or rewrite) the attribute list, a comment, on the first line of B."
  (let ((line (%f77-attribute-line "FORTRAN" package)) (l (point-line b)) (c (point-col b)))
    (if (and (> (buf-nlines b) 0) (search "-*-" (buf-line b 0)))
        (progn (buf-delete b 0 0 0 (length (buf-line b 0))) (buf-goto b 0 0) (buf-insert b line))
        (progn (buf-goto b 0 0) (buf-insert b (format nil "~A~%" line)) (setq l (+ l 1))))
    (buf-goto b (min l (- (buf-nlines b) 1)) (min c (length (buf-line b (min l (- (buf-nlines b) 1))))))
    line))

;;; ---- cards: comments, labels, continuations ------------------------------------------------------------------

(defun %f77-comment-line-p (text)
  (and (> (length text) 0) (member (char text 0) '(#\C #\c #\* #\!))))

(defun %f77-continuation-line-p (text)
  (and (not (%f77-comment-line-p text)) (> (length text) 5)
       (let ((c (char text 5))) (and (not (%f7-blank-p c)) (char/= c #\0)))
       (every #'%f7-blank-p (subseq text 0 5))))

(defun %f77-statement-line-p (text)
  (and (not (%f77-comment-line-p text)) (not (%f77-continuation-line-p text))
       (notevery #'%f7-blank-p text)))

;;; ---- colouring ----------------------------------------------------------------------------------------------------

(defvar *f77-keywords*
  '("PROGRAM" "SUBROUTINE" "FUNCTION" "BLOCK" "DATA" "END" "ENDIF" "ENDDO" "IF" "THEN" "ELSE" "ELSEIF" "DO" "WHILE" "CONTINUE"
    "GOTO" "GO" "TO" "CALL" "RETURN" "STOP" "PAUSE" "READ" "WRITE" "PRINT" "OPEN" "CLOSE" "INQUIRE" "REWIND" "BACKSPACE"
    "ENDFILE" "FORMAT" "INTEGER" "REAL" "DOUBLE" "PRECISION" "COMPLEX" "LOGICAL" "CHARACTER" "IMPLICIT" "NONE" "PARAMETER"
    "DIMENSION" "COMMON" "EQUIVALENCE" "EXTERNAL" "INTRINSIC" "SAVE" "INCLUDE" "ASSIGN" "ENTRY")
  "Words colored as statement keywords when they begin a statement (or follow IF/THEN).")

(defvar *f77-keyword-table* (let ((h (make-hash-table :test 'equal))) (dolist (w *f77-keywords* h) (setf (gethash w h) t))))

(defun f77-line-syntax (line state)
  "Syntax spans of one card: (values spans end-state).  A span is (start end class), CLASS :COMMENT :STRING :BUILTIN
(statement keywords and the continuation mark) :KEYWORD (intrinsic functions) :DEFNAME (routine names) :CONSTANT (labels
and operators).  FORTRAN has no multi-line state."
  (declare (ignore state))
  (let ((n (length line)) (spans nil))
    (cond
      ((%f77-comment-line-p line) (when (> n 0) (push (list 0 n :comment) spans)))
      (t
       (let ((i 0) (first-word t) (expect-name nil))
         ;; label field
         (let ((e (min n 5)))
           (when (and (> e 0) (some #'digit-char-p (subseq line 0 e))) (push (list 0 e :constant) spans)))
         (when (and (> n 5) (not (%f7-blank-p (char line 5)))) (push (list 5 6 :builtin) spans) (setq first-word nil))
         (setq i (min n 6))
         (loop
           (when (>= i n) (return))
           (let ((c (char line i)))
             (cond
               ((char= c #\!) (push (list i n :comment) spans) (return))
               ((or (char= c #\') (char= c #\"))
                (let ((j (+ i 1)))
                  (loop (cond ((>= j n) (return))
                              ((and (char= (char line j) c) (< (+ j 1) n) (char= (char line (+ j 1)) c)) (setq j (+ j 2)))
                              ((char= (char line j) c) (setq j (+ j 1)) (return))
                              (t (setq j (+ j 1)))))
                  (push (list i j :string) spans) (setq i j first-word nil)))
               ((char= c #\.)
                (let ((j (+ i 1)))
                  (loop (when (or (>= j n) (not (alpha-char-p (char line j)))) (return)) (setq j (+ j 1)))
                  (if (and (< j n) (char= (char line j) #\.) (> j (+ i 1)))
                      (progn (push (list i (+ j 1) :constant) spans) (setq i (+ j 1)))
                      (setq i (+ i 1)))))
               ((alpha-char-p c)
                (let ((j i))
                  (loop (when (or (>= j n) (not (%f7-name-char-p (char line j)))) (return)) (setq j (+ j 1)))
                  (let* ((w (string-upcase (subseq line i j)))
                         (class (cond ((and (or first-word (member w '("THEN" "ELSE" "WHILE" "TO" "PRECISION" "NONE" "BLOCK" "DATA") :test #'string=))
                                            (gethash w *f77-keyword-table*))
                                       (when (member w '("PROGRAM" "SUBROUTINE" "FUNCTION") :test #'string=) (setq expect-name t))
                                       :builtin)
                                      (expect-name (setq expect-name nil) :defname)
                                      ((gethash w *fc-intrinsics*) :keyword)
                                      (t nil))))
                    (when class (push (list i j class) spans))
                    (setq first-word nil i j))))
               ((%f7-blank-p c) (setq i (+ i 1)))
               (t (setq first-word nil i (+ i 1)))))))))
    (values (nreverse spans) nil)))

(setf (get 'fortran-mode 'line-syntax) #'f77-line-syntax)

;;; ---- language units from the parser ---------------------------------------------------------------------------

(defun f77-buffer-parse (b)
  "Parse buffer B (cached until it changes): (values units errors)."
  (let ((text (buffer-string b)) (cache (buf-local b 'f77-parse)))
    (if (and cache (string= (car cache) text))
        (values (cadr cache) (caddr cache))
        (multiple-value-bind (units errors)
            (handler-case (f77-parse text :file (buffer-file b) :directory (if (buffer-file b) (path-directory (buffer-file b)) ""))
              (error (e) (values nil (list e))))
          (setf (buf-local b 'f77-parse) (list text units errors))
          (values units errors)))))

(defun %f77-unit-label (u)
  (format nil "~(~A~) ~A" (fu-kind u) (string-downcase (or (fu-name u) (if (eq (fu-kind u) :program) "(unnamed main program)" "")))))

(defun %f77-unit-at (b off)
  "The program unit around OFFSET, else the nearest one before it, else the first."
  (let ((units (f77-buffer-parse b)))
    (or (find-if (lambda (u) (and (<= (fu-start u) off) (<= off (fu-end u)))) units)
        (let ((best nil)) (dolist (u units best) (when (and (<= (fu-end u) off) (or (null best) (> (fu-end u) (fu-end best)))) (setq best u))))
        (car units))))

(defun %f77-statements (b)
  "Every statement of B as (start end fnode), in order."
  (let ((acc nil))
    (dolist (u (f77-buffer-parse b) (nreverse acc))
      (dolist (n (fu-stmts u)) (push (list (fn-start n) (fn-end n) n) acc)))))

(defun %f77-goto (b off) (goto-offset b (max 0 (min off (length (buffer-string b))))))

;;; ---- the mode and its minor mode -----------------------------------------------------------------------------

(defcmd fortran-mode (ed arg)
  (with-b (b ed)
    (set-buffer-mode b 'fortran-mode)
    (when arg (%f77-set-attributes b))
    (ed-message* ed "FORTRAN mode~:[ (with a numeric argument, the attribute list is written too)~;~]" arg)))

(defcmd electric-fortran-mode (ed arg)
  (with-b (b ed)
    (unless (%f77-buffer-p b) (set-buffer-mode b 'fortran-mode))
    (if (member 'electric (buffer-minor-modes b))
        (progn (setf (buffer-minor-modes b) (remove 'electric (buffer-minor-modes b))) (ed-message* ed "Electric FORTRAN mode off"))
        (progn (push 'electric (buffer-minor-modes b))
               (ed-message* ed "Electric FORTRAN mode on: text wraps at column ~D with continuation character ~A" (%f77-wraparound-column) (%f77-continuation-char))))))

;;; ---- electric wrapping ----------------------------------------------------------------------------------------------

(defun %f77-in-string-at-p (text pos)
  "True when position POS of the card TEXT lies inside a character constant."
  (let ((q nil))
    (dotimes (i (min pos (length text)))
      (let ((c (char text i)))
        (cond (q (when (char= c q) (setq q nil)))
              ((and (>= i 6) (or (char= c #\') (char= c #\"))) (setq q c)))))
    (and q t)))

(defun %f77-break-point (text limit)
  "Where to break the card TEXT longer than LIMIT: an index after a blank, comma or operator outside a character
constant, else (inside a constant) exactly column 72 when the card is longer than that, else NIL."
  (let ((p limit))
    (loop
      (when (<= p 7) (return))
      (when (and (member (char text (- p 1)) '(#\Space #\, #\+ #\- #\* #\/ #\( #\)))
                 (not (%f77-in-string-at-p text (- p 1))))
        (return-from %f77-break-point p))
      (setq p (- p 1)))
    (cond ((> (length text) 72) 72)
          ((not (%f77-in-string-at-p text limit)) limit)
          (t nil))))

(defun %f77-after-insert (ed c)
  "Electric FORTRAN mode: wrap the card when it grows past the wraparound column."
  (declare (ignore c))
  (let ((b (ed-buffer ed)))
    (when (member 'electric (buffer-minor-modes b))
      (let* ((line (point-line b)) (text (buf-line b line)) (limit (%f77-wraparound-column)))
        (when (and (> (length text) limit) (not (%f77-comment-line-p text)))
          (let ((p (%f77-break-point text limit)))
            (when (and p (< p (length text)))
              (let* ((col (point-col b)) (rest (string-left-trim " " (subseq text p))) (lead (- (- (length text) p) (length rest))))
                (buf-delete b line p line (length text))
                (buf-goto b line p)
                (let ((tail (string-right-trim " " (buf-line b line))))
                  (unless (= (length tail) p) (buf-delete b line (length tail) line p)))
                (buf-goto b line (length (buf-line b line)))
                (buf-insert b (format nil "~%     ~A~A" (%f77-continuation-char) rest))
                (let ((nl (+ line 1)))
                  (buf-goto b nl (if (> col p) (min (length (buf-line b nl)) (max 6 (+ 6 (- col p lead)))) (length (buf-line b nl)))))))))))))

(setf (get 'fortran-mode 'after-self-insert) #'%f77-after-insert)

;;; ---- indentation ------------------------------------------------------------------------------------------------------

(defun %f77-card-statement (b line)
  "The squeezed text of the statement starting on LINE of B (with up to nine continuation cards)."
  (let ((text (nth-value 1 (%f77-split-card (buf-line b line)))))
    (do ((l (+ line 1) (+ l 1)) (k 0 (+ k 1))) ((or (>= l (buf-nlines b)) (>= k 9) (not (%f77-continuation-line-p (buf-line b l)))))
      (let ((s (buf-line b l))) (setq text (%string-concat text (subseq s 6 (min 72 (length s)))))))
    (handler-case (%f7-squeeze (%f7-strip-comment text)) (error () ""))))

(defun %f77-line-label (b line)
  (let ((label (%f77-split-card (buf-line b line)))) (and (%f7-digits-p label) (parse-integer label))))

(defun %f77-block-effect (s)
  "How the statement S (squeezed) changes the nesting: (values opens-p closes-n do-label)."
  (cond ((= (length s) 0) nil)
        ((or (%f7-starts s "ENDIF") (%f7-starts s "ENDDO")) :close)
        ((and (%f7-starts s "END") (member (subseq s 3) '("IF" "DO") :test #'string=)) :close)
        ((or (%f7-starts s "ELSEIF") (string= s "ELSE")) :else)
        ((multiple-value-bind (k label) (%f7-do-parts s) (and k (list :do label))))
        ((and (%f7-starts s "IF") (> (length s) 2) (char= (char s 2) #\()
              (let ((m (%f7-match-paren s 2))) (and m (string= (subseq s (+ m 1)) "THEN"))))
         :open)
        (t nil)))

(defun f77-indent-depth (b line)
  "The nesting depth of the statement on LINE of B, and whether this statement closes or continues a block."
  (let ((depth 0) (labels nil))
    (do ((l 0 (+ l 1))) ((>= l line))
      (let ((text (buf-line b l)))
        (when (%f77-statement-line-p text)
          (let* ((s (%f77-card-statement b l)) (eff (%f77-block-effect s)) (lab (%f77-line-label b l)))
            (cond ((or (%f7-starts s "PROGRAM") (%f7-starts s "SUBROUTINE") (%f7-starts s "BLOCKDATA") (%f7-starts s "FUNCTION") (string= s "END"))
                   (setq depth 0 labels nil))
                  (t
                   (when (and lab labels)
                     (loop (if (and labels (eql (car labels) lab)) (progn (pop labels) (setq depth (max 0 (- depth 1)))) (return))))
                   (cond ((eq eff :open) (setq depth (+ depth 1)))
                         ((eq eff :close) (setq depth (max 0 (- depth 1))))
                         ((and (consp eff) (eq (car eff) :do)) (setq depth (+ depth 1)) (push (cadr eff) labels)))))))))
    ;; this line itself
    (let* ((text (buf-line b line)) (s (if (%f77-statement-line-p text) (%f77-card-statement b line) "")) (eff (%f77-block-effect s))
           (lab (%f77-line-label b line)) (d depth))
      (when (or (eq eff :close) (eq eff :else)) (setq d (max 0 (- d 1))))
      (when (and lab labels) (let ((ls labels)) (loop (if (and ls (eql (car ls) lab)) (progn (pop ls) (setq d (max 0 (- d 1)))) (return)))))
      d)))

(defun %f77-split-card (text)
  "Split the statement card TEXT into (values label-string body-string), where a label typed in the statement field
(a statement never starts with a digit) is still taken as the label."
  (multiple-value-bind (label body) (%f77-split-card-1 text)
    (if (and (string= label "") (> (length body) 0) (digit-char-p (char body 0)))
        (let ((d (or (position-if-not #'digit-char-p body) (length body))))
          (if (and (<= d 5) (< d (length body)) (%f7-blank-p (char body d)))
              (values (subseq body 0 d) (string-trim " " (subseq body d)))
              (values label body)))
        (values label body))))

(defun %f77-split-card-1 (text)
  "Split the statement card TEXT into (values label-string body-string): the fixed label field when columns 1-5 hold only a
label, a label typed at column 1 and followed by a blank, else no label and the whole text as the statement."
  (let* ((n (length text)) (field (subseq text 0 (min n 5))))
    (cond ((and (every (lambda (c) (or (%f7-blank-p c) (digit-char-p c))) field) (or (<= n 5) (%f7-blank-p (char text 5))))
           (values (string-trim " " field) (string-trim " " (if (> n 6) (subseq text 6 (min n 72)) ""))))
          (t
           (let ((d (or (position-if-not #'digit-char-p text) n)))
             (if (and (> d 0) (<= d 5) (< d n) (%f7-blank-p (char text d)))
                 (values (subseq text 0 d) (string-trim " " (subseq text d)))
                 (values "" (string-trim " " text))))))))

(defun f77-indent-line (b line)
  "Indent the statement card on LINE of B by its nesting; label and continuation fields are kept."
  (let ((text (buf-line b line)))
    (when (%f77-statement-line-p text)
      (multiple-value-bind (label body) (%f77-split-card text)
        (let* ((col (+ 6 (* *f77-indent* (f77-indent-depth b line))))
               (new (%string-concat label (make-string (- 6 (length label)) :initial-element #\Space)
                                    (make-string (- col 6) :initial-element #\Space) body)))
          (unless (string= new text)
            (buf-delete b line 0 line (length text))
            (buf-goto b line 0)
            (buf-insert b new)))))))

(defcmd fortran-tab (ed arg)
  (with-b (b ed)
    (let* ((line (point-line b)) (col (point-col b)) (text (buf-current-line b)))
      (cond
        ((%f77-comment-line-p text) (buf-insert b (string #\Tab)))
        ((< col 6)
         (when (< (length text) 6) (buf-goto b line (length text)) (buf-insert b (make-string (- 6 (length text)) :initial-element #\Space)))
         (buf-goto b line 6))
        (t
         (f77-indent-line b line)
         (let ((ws (+ 6 (or (position-if-not #'%f7-blank-p (subseq (buf-current-line b) (min 6 (length (buf-current-line b))))) 0))))
           (when (< (point-col b) ws) (buf-goto b line ws))))))))

(defcmd fortran-newline-and-indent (ed arg)
  ;; LINE: end this line and open an indented statement line
  (with-b (b ed)
    (let ((ended (point-line b)))
      (f77-indent-line b ended)
      (buf-goto b (point-line b) (length (buf-current-line b)))
      (buf-insert b (string #\Newline))
      (let ((col (+ 6 (* *f77-indent* (f77-indent-depth b (point-line b))))))
        (buf-insert b (make-string col :initial-element #\Space))))))

(defcmd fortran-continuation-line (ed arg)
  ;; Control-LINE: open a continuation card
  (with-b (b ed)
    (buf-goto b (point-line b) (length (buf-current-line b)))
    (buf-insert b (format nil "~%     ~A" (%f77-continuation-char)))))

(defcmd fortran-merge-lines (ed arg)
  ;; Control-^: merge this line into the previous one, dropping a continuation card's fields
  (with-b (b ed)
    (let ((line (point-line b)))
      (when (= line 0) (error "There is no previous line"))
      (let* ((text (buf-current-line b)) (prev (buf-line b (- line 1)))
             (tail (if (%f77-continuation-line-p text) (string-left-trim " " (subseq text 6)) (string-left-trim " " text)))
             (joiner (if (or (%f77-continuation-line-p text) (= (length prev) 0)) "" " ")))
        (buf-delete b (- line 1) (length prev) line (length text))
        (buf-goto b (- line 1) (length prev))
        (buf-insert b (%string-concat joiner tail))
        (buf-goto b (- line 1) (length prev))))))

;;; ---- routines and statements ------------------------------------------------------------------------------------------

(defcmd fortran-beginning-of-routine (ed arg)
  (with-b (b ed)
    (let* ((off (point-offset b)) (best nil))
      (dolist (u (f77-buffer-parse b)) (when (and (< (fu-start u) off) (or (null best) (> (fu-start u) (fu-start best)))) (setq best u)))
      (if best (progn (push-mark ed) (%f77-goto b (fu-start best))) (ed-message* ed "No routine before point")))))

(defcmd fortran-end-of-routine (ed arg)
  (with-b (b ed)
    (let* ((off (point-offset b)) (best nil))
      (dolist (u (f77-buffer-parse b)) (when (and (> (fu-end u) off) (or (null best) (< (fu-end u) (fu-end best)))) (setq best u)))
      (if best (progn (push-mark ed) (%f77-goto b (fu-end best))) (ed-message* ed "No routine after point")))))

(defcmd fortran-mark-routine (ed arg)
  (with-b (b ed)
    (let ((u (%f77-unit-at b (point-offset b))))
      (unless u (error "No routine here"))
      (%f77-goto b (fu-end u))
      (let ((p (buf-pos-of-offset b (fu-end u)))) (setf (buffer-mark b) (cons (car p) (cdr p))))
      (%f77-goto b (fu-start u))
      (ed-message* ed "Marked ~A" (%f77-unit-label u)))))

(defcmd fortran-forward-statement (ed arg)
  (with-b (b ed)
    (dotimes (i (%n arg))
      (let ((off (point-offset b)) (best nil))
        (dolist (s (%f77-statements b)) (when (and (> (car s) off) (or (null best) (< (car s) (car best)))) (setq best s)))
        (if best (%f77-goto b (car best)) (error "No more statements"))))))

(defcmd fortran-backward-statement (ed arg)
  (with-b (b ed)
    (dotimes (i (%n arg))
      (let ((off (point-offset b)) (best nil))
        (dolist (s (%f77-statements b)) (when (and (< (car s) off) (or (null best) (> (car s) (car best)))) (setq best s)))
        (if best (%f77-goto b (car best)) (error "No previous statement"))))))

;;; ---- the compiled routine's arguments -----------------------------------------------------------------------------------

(defun %f77-describe-signature (u package)
  (let* ((name (or (fu-name u) ".MAIN.")) (sig (gethash (%string-concat package ":" name) *f77-signatures*)))
    (cond (sig (format nil "~(~A~) ~A(~{~A~^, ~})~@[ returns ~A~]" (car sig) (string-downcase name)
                       (mapcar (lambda (p) (if (eq p :star) "*" (format nil "~A~:[~; array~]" (%fc-kind-name (car p)) (cdr p)))) (caddr sig))
                       (and (cadr sig) (%fc-kind-name (cadr sig)))))
          ((eq (fu-kind u) :program) (format nil "program ~(~A~)" name))
          (t (format nil "~(~A~) ~A(~{~A~^, ~}) is not compiled" (fu-kind u) (string-downcase name)
                     (mapcar (lambda (p) (if (eq p :star) "*" (string-downcase p))) (fu-params u)))))))

(defcmd fortran-show-arglist (ed arg)
  (with-b (b ed)
    (let ((u (%f77-unit-at b (point-offset b))))
      (if u (ed-message* ed "~A" (%f77-describe-signature u (%f77-buffer-package b))) (ed-message* ed "No routine here")))))

;;; ---- compiling ------------------------------------------------------------------------------------------------------------

(defun %f77-set-warning-possibilities (ed)
  (let ((ws (f77-compiler-warnings)))
    (setf (ed-possibilities ed)
          (make-possibilities "compiler warnings"
                              (mapcar (lambda (w)
                                        (make-possibility (format nil "~A:~D: ~A" (path-filename (princ-to-string (fwarning-source w))) (fwarning-line w) (fwarning-message w))
                                                          (lambda (e) (%f77-visit-warning e w))))
                                      ws)))
    ws))

(defun %f77-visit-warning (ed w)
  (let* ((src (fwarning-source w))
         (b (or (find src (ed-buffers ed) :key #'buffer-file :test #'equal)
                (find src (ed-buffers ed) :key #'buffer-name :test #'equal)
                (and (stringp src) (probe-file src) (ed-visit-file ed src)))))
    (unless b (error "The source ~A of the warning is not available" src))
    (ed-switch-to-buffer ed b)
    (when (fwarning-start w) (%f77-goto b (fwarning-start w)))))

(defun %f77-compile-in-editor (ed only-ats)
  "Compile the program units of the current buffer at the offsets ONLY-ATS (NIL: the whole buffer)."
  (let* ((b (ed-buffer ed)) (text (buffer-string b)) (file (buffer-file b)) (names nil) (errors nil))
    (dolist (at (or only-ats (list nil)))
      (multiple-value-bind (n e)
          (f77-compile-string text :package (%f77-buffer-package b) :file file :directory (if file (path-directory file) "")
                                   :only-at at :warnings-key (or file (buffer-name b)))
        (setq names (append names n) errors (append errors e))))
    (%f77-record-warnings (or file (buffer-name b)) errors)
    (cond (errors
           (%f77-set-warning-possibilities ed)
           (ed-message* ed "~D compiler warning~:P; line ~D: ~A  (C-. visits them)" (length errors) (f77-error-line (car errors)) (f77-error-message (car errors))))
          ((null names) (ed-message* ed "Nothing to compile"))
          (t (f77-note-compiled b (mapcar (lambda (n) (string-downcase (symbol-name n))) names))
             (ed-message* ed "~{~A~^, ~} compiled" (mapcar (lambda (n) (string-downcase (symbol-name n))) names))))
    (values names errors)))

(defmacro %f77-chain (name (ed arg) fortran-form)
  "Define the editor command NAME to run FORTRAN-FORM in FORTRAN buffers and whatever it did before elsewhere."
  `(let ((%previous (get ',name '%ecommand)))
     (defcmd ,name (,ed ,arg)
       (if (%f77-buffer-p (ed-buffer ,ed))
           ,fortran-form
           (if %previous (funcall %previous ,ed ,arg) (ed-message* ,ed "~A is not available in this buffer" ',name))))))

(%f77-chain compile-region (ed arg)
  ;; c-sh-C: the units in the region, or else the routine at point
  (with-b (b ed)
    (multiple-value-bind (l1 c1 l2 c2) (region-bounds b)
      (cond (l1
             (let* ((s (buf-offset b l1 c1)) (e (buf-offset b l2 c2))
                    (ats (mapcar #'fu-start (remove-if-not (lambda (u) (and (<= s (fu-start u)) (< (fu-start u) e))) (f77-buffer-parse b)))))
               (setf (buffer-mark b) nil)
               (%f77-compile-in-editor ed (or ats (list (point-offset b))))))
            (t (%f77-compile-in-editor ed (list (point-offset b))))))))

(%f77-chain compile-buffer (ed arg) (%f77-compile-in-editor ed nil))

(%f77-chain compile-file (ed arg)
  (with-b (b ed)
    (unless (buffer-file b) (error "The buffer is not visiting a file"))
    (when (buffer-modified b) (ed-save-buffer ed b))
    (handler-case (ed-message* ed "Wrote ~A" (f77-compile-file (buffer-file b)))
      (error (e) (%f77-set-warning-possibilities ed) (ed-message* ed "~A" (car (%split-string (princ-to-string e) #\Newline)))))))

(%f77-chain compiler-warnings (ed arg)
  (let ((ws (f77-compiler-warnings)))
    (%show-text-buffer ed "*Compiler-Warnings-1*"
                       (if ws (format nil "~{~A~%~}" (mapcar (lambda (w) (format nil "~A: line ~D, column ~D: ~A" (fwarning-source w) (fwarning-line w) (fwarning-col w) (fwarning-message w))) ws))
                           "No compiler warnings."))))

(defcmd load-compiler-warnings (ed arg)
  ;; reload the saved diagnostics: recompile the buffer's file quietly and keep its warnings
  (with-b (b ed)
    (let ((ws (f77-compiler-warnings (or (buffer-file b) (buffer-name b)))))
      (%f77-set-warning-possibilities ed)
      (ed-message* ed "~D compiler warning~:P loaded" (length ws)))))

(%f77-chain edit-compiler-warnings (ed arg)
  (let ((ws (%f77-set-warning-possibilities ed)))
    (if ws (ed-goto-possibility ed 0) (ed-message* ed "No compiler warnings"))))

;;; ---- running ------------------------------------------------------------------------------------------------------------------

(defun %f77-program-at (b)
  "The name of the main program around point, or the nearest one before it, or the first in the buffer."
  (let* ((off (point-offset b)) (progs (remove-if-not (lambda (u) (eq (fu-kind u) :program)) (f77-buffer-parse b))))
    (when (null progs) (error "There is no main program in this buffer"))
    (or (fu-name (or (find-if (lambda (u) (and (<= (fu-start u) off) (<= off (fu-end u)))) progs)
                     (car (last (remove-if-not (lambda (u) (<= (fu-end u) off)) progs)))
                     (car progs)))
        ".MAIN.")))

(defun %f77-find-listener (ed)
  (let ((wm (window-wm ed)))
    (and wm (find-if (lambda (w) (and (typep w 'listener-window) (not (typep w 'report-window)))) (wm-windows wm)))))

(defun f77-run-from-editor (ed name package)
  "Run the program NAME in the Listener (where it can read input), or with output to a buffer when there is none."
  (let ((sym (intern name package)) (lw (%f77-find-listener ed)))
    (if lw
        (progn
          (wm-raise (window-wm ed) lw)
          (%in-listener lw (lambda ()
                             (format t "~&; Executing FORTRAN program ~(~A~)~%" name)
                             (f77-execute sym)))
          (setf (wm-dirty (window-wm ed)) t))
        (let ((out (make-string-output-stream)))
          (let ((*standard-output* out)) (f77-execute sym :input (make-string-input-stream "")))
          (%show-text-buffer ed "*FORTRAN Output*" (get-output-stream-string out))))))

(defcmd execute-fortran-program (ed arg)
  (with-b (b ed)
    (let ((name (%f77-program-at b)) (pkg (%f77-buffer-package b)))
      (unless (gethash (%string-concat pkg ":" name) *f77-programs*)
        (error "The program ~(~A~) has not been compiled (C-c C-e compiles and executes it)" name))
      (f77-run-from-editor ed name pkg))))

(defcmd compile-and-execute-fortran-program (ed arg)
  (with-b (b ed)
    (let* ((name (%f77-program-at b)) (pkg (%f77-buffer-package b))
           (unit (find-if (lambda (u) (and (eq (fu-kind u) :program) (equal (or (fu-name u) ".MAIN.") name))) (f77-buffer-parse b))))
      (multiple-value-bind (names errors) (%f77-compile-in-editor ed (list (fu-start unit)))
        (declare (ignore names))
        (unless errors (f77-run-from-editor ed name pkg))))))

;;; ---- attribute list commands ----------------------------------------------------------------------------------------------------

(%f77-chain set-package (ed arg)
  (with-b (b ed)
    (ed-read-string ed (format nil "Package [~A]: " (%f77-buffer-package b))
                    (lambda (s)
                      (let ((name (string-upcase (string-trim " " s))))
                        (when (string= name "") (setq name (%f77-buffer-package b)))
                        (unless (find-package name) (package-declare-with-no-superiors name))
                        (%f77-set-attributes b :package name)
                        (ed-message* ed "Package ~A" name))))))

(%f77-chain update-attribute-list (ed arg) (ed-message* ed "~A" (%f77-set-attributes (ed-buffer ed))))

(%f77-chain reparse-attribute-list (ed arg)
  (with-b (b ed)
    (setf (buf-local b 'f77-parse) nil)
    (ed-message* ed "Mode ~A, package ~A" (mode-display-name (buffer-mode b)) (%f77-buffer-package b))))

;;; ---- changed definitions ----------------------------------------------------------------------------------------------------------

(defun %f77-unit-key (u idx) (list (fu-kind u) (fu-name u) idx))

(defun %f77-unit-texts (b)
  "Hash of unit key -> text for the program units of B."
  (let ((h (make-hash-table :test 'equal)) (counts nil) (text (buffer-string b)))
    (dolist (u (f77-buffer-parse b) h)
      (let* ((k (list (fu-kind u) (fu-name u))) (cell (assoc k counts :test #'equal)) (idx (if cell (cdr cell) 0)))
        (if cell (rplacd cell (+ idx 1)) (setq counts (cons (cons k 1) counts)))
        (setf (gethash (%f77-unit-key u idx) h) (subseq text (fu-start u) (min (length text) (fu-end u))))))))

(defun f77-reset-baselines (b &optional (which '(:read :saved :compiled)))
  "Record the current text of every program unit of B as the baseline for each of WHICH."
  (dolist (w which)
    (setf (buf-local b (intern (format nil "F77-BASELINE-~A" w) "KEYWORD")) (%f77-unit-texts b))))

(defun %f77-baseline (b which)
  (let ((key (intern (format nil "F77-BASELINE-~A" which) "KEYWORD")))
    (or (buf-local b key)
        (progn (f77-reset-baselines b (list which)) (buf-local b key)))))

(defun f77-note-compiled (b names)
  "Mark the units of B named NAMES (strings) as compiled with their current text."
  (let ((h (%f77-baseline b :compiled)) (now (%f77-unit-texts b)))
    (maphash (lambda (k v) (when (member (and (cadr k) (string-downcase (cadr k))) names :test #'equal) (setf (gethash k h) v))) now)
    (when (member ".main." names :test #'string=) (maphash (lambda (k v) (when (null (cadr k)) (setf (gethash k h) v))) now))))

(defun f77-changed-units (b which)
  "The program units of B whose text differs from the WHICH baseline, as (key . FAST)."
  (let ((h (%f77-baseline b which)) (counts nil) (acc nil) (text (buffer-string b)))
    (dolist (u (f77-buffer-parse b))
      (let* ((k (list (fu-kind u) (fu-name u))) (cell (assoc k counts :test #'equal)) (idx (if cell (cdr cell) 0)) (key (%f77-unit-key u idx)))
        (if cell (rplacd cell (+ idx 1)) (setq counts (cons (cons k 1) counts)))
        (unless (equal (gethash key h) (subseq text (fu-start u) (min (length text) (fu-end u))))
          (push (cons key u) acc))))
    (nreverse acc)))

(defun %f77-changed-which (arg)
  "No argument: since last compiled; 1: since the file was read; 2: since last saved; 3: since last compiled."
  (cond ((eql arg 1) :read) ((eql arg 2) :saved) (t :compiled)))

(defun %f77-unit-possibility (b u)
  (make-possibility (format nil "~A (~A)" (%f77-unit-label u) (buffer-name b))
                    (lambda (e) (ed-switch-to-buffer e b) (%f77-goto b (fu-start u)))))

(%f77-chain edit-changed-definitions-of-buffer (ed arg)
  (let* ((b (ed-buffer ed)) (which (%f77-changed-which arg)))
    (ed-set-possibilities ed (format nil "definitions changed ~(~A~)" which)
                          (mapcar (lambda (c) (%f77-unit-possibility b (cdr c))) (f77-changed-units b which)) :go t)))

(%f77-chain list-changed-definitions (ed arg)
  (let* ((b (ed-buffer ed)) (which (%f77-changed-which arg)))
    (ed-set-possibilities ed (format nil "definitions changed ~(~A~)" which)
                          (mapcar (lambda (c) (%f77-unit-possibility b (cdr c))) (f77-changed-units b which)) :go nil)))

(%f77-chain compile-changed-definitions-of-buffer (ed arg)
  (let* ((b (ed-buffer ed)) (changed (f77-changed-units b (%f77-changed-which arg))))
    (if (null changed)
        (ed-message* ed "No definitions changed")
        (%f77-compile-in-editor ed (mapcar (lambda (c) (fu-start (cdr c))) changed)))))

(defcmd list-definitions (ed arg)
  (with-b (b ed)
    (unless (%f77-buffer-p b) (error "List Definitions lists the routines of a FORTRAN buffer"))
    (%show-text-buffer ed "*Definitions*"
                       (format nil "~{~A~%~}" (mapcar (lambda (u) (format nil "~A  (line ~D)" (%f77-unit-label u) (fu-line u))) (f77-buffer-parse b))))))

;;; ---- edit definition ------------------------------------------------------------------------------------------------------------------

(defun %f77-word-at (b)
  (let* ((text (buf-current-line b)) (col (min (point-col b) (length text))) (s col) (e col))
    (loop (when (or (<= s 0) (not (%f7-name-char-p (char text (- s 1))))) (return)) (setq s (- s 1)))
    (loop (when (or (>= e (length text)) (not (%f7-name-char-p (char text e)))) (return)) (setq e (+ e 1)))
    (and (> e s) (string-upcase (subseq text s e)))))

(defun %f77-edit-definition (ed)
  (with-b (b ed)
    (let ((word (%f77-word-at b)))
      (unless word (error "Point is not on a name"))
      (let ((u (find-if (lambda (u) (and (member (fu-kind u) '(:subroutine :function :program :blockdata)) (equal (fu-name u) word))) (f77-buffer-parse b))))
        (if u
            (progn (push-mark ed) (%f77-goto b (fu-start u)) (ed-message* ed "~A" (%f77-unit-label u)))
            (let* ((pkg (%f77-buffer-package b)) (info (gethash (%string-concat pkg ":" word) *f77-routine-info*))
                   (file (cadr info)))
              (cond ((and file (probe-file file))
                     (let ((nb (ed-visit-file ed file)))
                       (let ((target (find-if (lambda (x) (equal (fu-name x) word)) (f77-buffer-parse nb))))
                         (when target (%f77-goto nb (fu-start target))))
                       (ed-message* ed "~A" file)))
                    (t (ed-message* ed "No definition of ~(~A~) is known" word)))))))))

(defcmd edit-definition (ed arg)
  (if (%f77-buffer-p (ed-buffer ed)) (%f77-edit-definition ed) (run-editor-command ed 'find-definition arg)))

(defcmd fortran-edit-definition (ed arg) (%f77-edit-definition ed))

;;; ---- sequence numbers, tabs, breakpoints -------------------------------------------------------------------------------------------

(defcmd find-next-fortran-sequence-number (ed arg)
  (with-b (b ed)
    (let ((start (+ 1 (point-line b))))
      (do ((l start (+ l 1))) ((>= l (buf-nlines b)) (ed-message* ed "No more sequence numbers"))
        (let ((text (buf-line b l)))
          (when (and (> (length text) 72) (notevery #'%f7-blank-p (subseq text 72)))
            (buf-goto b l 72) (return)))))))

(defcmd remove-sequence-numbers (ed arg)
  (with-b (b ed)
    (let ((n 0))
      (multiple-value-bind (l1 c1 l2 c2) (region-bounds b)
        (declare (ignore c1 c2))
        (do ((l (or l1 0) (+ l 1))) ((> l (or l2 (- (buf-nlines b) 1))))
          (let ((text (buf-line b l)))
            (when (and (> (length text) 72) (not (%f77-comment-line-p text)))
              (let ((keep (string-right-trim " " (subseq text 0 72))))
                (buf-delete b l (length keep) l (length text))
                (setq n (+ n 1)))))))
      (ed-message* ed "Removed ~D sequence field~:P" n))))

(defun %f77-replace-tabs-in-line (text)
  "TEXT with each tab replaced by spaces to a fixed-format stop: a tab in the first six columns ends the label field."
  (let ((out (make-string-output-stream)) (col 0) (n (length text)))
    (dotimes (i n)
      (let ((c (char text i)))
        (cond ((char/= c #\Tab) (write-char c out) (setq col (+ col 1)))
              ((< col 6)
               (let ((next (and (< (+ i 1) n) (char text (+ i 1)))))
                 (if (and next (digit-char-p next) (char/= next #\0) (= col col))
                     (progn (dotimes (k (- 5 col)) (write-char #\Space out)) (setq col 5))
                     (progn (dotimes (k (- 6 col)) (write-char #\Space out)) (setq col 6)))))
              (t (let ((stop (* 8 (+ 1 (floor col 8))))) (dotimes (k (- stop col)) (write-char #\Space out)) (setq col stop))))))
    (get-output-stream-string out)))

(defcmd replace-tabs (ed arg)
  (with-b (b ed)
    (let ((n 0))
      (multiple-value-bind (l1 c1 l2 c2) (region-bounds b)
        (declare (ignore c1 c2))
        (do ((l (or l1 0) (+ l 1))) ((> l (or l2 (- (buf-nlines b) 1))))
          (let ((text (buf-line b l)))
            (when (find #\Tab text)
              (let ((new (%f77-replace-tabs-in-line text)))
                (buf-delete b l 0 l (length text)) (buf-goto b l 0) (buf-insert b new) (setq n (+ n 1)))))))
      (ed-message* ed "Replaced tabs on ~D line~:P" n))))

(defcmd set-breakpoint (ed arg)
  (with-b (b ed)
    (let ((entry (cons (buffer-name b) (+ 1 (point-line b)))))
      (if (member entry *f77-breakpoints* :test #'equal)
          (progn (setq *f77-breakpoints* (remove entry *f77-breakpoints* :test #'equal)) (ed-message* ed "Breakpoint cleared at line ~D" (cdr entry)))
          (progn (push entry *f77-breakpoints*) (ed-message* ed "Breakpoint set at line ~D (recorded; the run time does not stop at breakpoints)" (cdr entry)))))))

(defcmd list-breakpoints (ed arg)
  (%show-text-buffer ed "*Breakpoints*"
                     (if *f77-breakpoints*
                         (format nil "~{~A~%~}" (mapcar (lambda (e) (format nil "~A line ~D" (car e) (cdr e))) (reverse *f77-breakpoints*)))
                         "No breakpoints.")))

(defcmd clear-all-breakpoints (ed arg)
  (let ((n (length *f77-breakpoints*)))
    (setq *f77-breakpoints* nil)
    (ed-message* ed "Cleared ~D breakpoint~:P" n)))

;;; ---- keys ------------------------------------------------------------------------------------------------------------------------------

(dolist (k '(("<tab>" fortran-tab) ("C-j" fortran-newline-and-indent) ("C-M-j" fortran-continuation-line)
             ("C-^" fortran-merge-lines)
             ("C-M-a" fortran-beginning-of-routine) ("C-M-e" fortran-end-of-routine) ("C-M-h" fortran-mark-routine)
             ("C-M-f" fortran-forward-statement) ("C-M-b" fortran-backward-statement)
             ("C-c C-a" fortran-show-arglist) ("C-c C-c" compile-region) ("C-c C-b" compile-buffer)
             ("C-c C-e" compile-and-execute-fortran-program) ("C-c C-x" execute-fortran-program)
             ("C-c C-w" edit-compiler-warnings) ("M-." fortran-edit-definition)))
  (define-key *f77-mode-map* (car k) (cadr k)))
