;;;; 50-editor.lisp -- the Pascal Tool Kit: Pascal mode in the editor.
;;;;
;;;; Pascal mode is a syntax-directed editing mode, after the Pascal editor mode of the Genera Tool Kit: it colours
;;;; reserved words, comments and strings; moves over and deletes Pascal language units (statements, routine
;;;; definitions, expressions, tokens) using the parser; finds syntax errors; inserts templates of language
;;;; constructs with template items ("⊂statement⊃") and refines them; completes reserved words; indents; and
;;;; compiles the routine at point, the region or the buffer into the running world.  Electric Pascal mode (a minor
;;;; mode) puts reserved words and identifiers in the case chosen with Adjust Face And Case as they are typed.
;;;;
;;;; Keys (Genera's in brackets):
;;;;   C-c C-c  compile the routine at point, or the region      [c-sh-C]
;;;;   C-c C-b  compile the buffer        C-c C-e  compile and execute the program at point
;;;;   C-c C-x  execute the program at point                     C-c C-w  edit compiler warnings (then C-.)
;;;;   C-M-a C-M-e  routine start/end     C-M-f C-M-b  statement end/start     C-M-h  mark routine
;;;;   C-M-k  kill statement forward      C-M-<backspace>  kill statement backward   C-c k  kill statement around
;;;;   C-c f C-c b  expression forward/back   C-c x  kill expression   C-c t C-c T  kill token forward/back
;;;;   C-c C-n C-c C-p  next/previous syntax error [c-sh-N c-sh-P]   C-j  newline, indent, check the line [LINE]
;;;;   C-c C-t  template for the keyword before point [END]   C-c ?  constructs valid here [c-HELP]
;;;;   M-<tab>  complete a reserved word, or refine the template item at point [COMPLETE]
;;;;   C-c /  predeclared identifiers completing the word [c-?]   C-c ]  close the open construct [c-END]
;;;;   C-M-n C-M-p  next/previous template item     SPC at a template item deletes it

(in-package :cl)

(defvar *pascal-mode-map* (make-keymap "pascal"))
(defvar *pascal-indent* 2 "Columns per level of indentation in Pascal mode.")
(defvar *pascal-reserved-word-case* :upper
  "How Electric Pascal mode writes reserved words: :UPPER, :LOWER, :CAPITALIZE, :CAPITALIZE-WORDS or :LEAVE.")
(defvar *pascal-identifier-case* :lower "How Electric Pascal mode writes other identifiers (the same choices).")
(defvar *pascal-blink-matching* nil "When true, the construct an END or UNTIL at point closes is highlighted.")

(setf (get 'pascal-mode 'mode-keymap) *pascal-mode-map*
      (get 'pascal-mode 'mode-name) "Pascal")

(dolist (suffix '(".pas" ".pascal" ".p"))
  (unless (assoc suffix *auto-mode-alist* :test #'string=) (push (cons suffix 'pascal-mode) *auto-mode-alist*)))

(defun %pascal-buffer-p (b) (eq (buffer-mode b) 'pascal-mode))

;;; ---- the buffer's attributes ------------------------------------------------------------------------------

(defun pascal-buffer-attributes (b)
  "The attribute list of buffer B: (:mode :dialect :package)."
  (pascal-file-attributes (if (> (buf-nlines b) 0) (buf-line b 0) "")))

(defun %pascal-buffer-dialect (b) (%pascal-dialect-of (pascal-buffer-attributes b)))
(defun %pascal-buffer-package (b)
  (let ((f (buffer-file b))) (%pascal-file-package (or f (buffer-name b)) (pascal-buffer-attributes b))))

(defun %pascal-attribute-line (mode dialect package)
  (format nil "{-*- Mode: ~A; Dialect: ~A; Package: ~A -*- }" mode (if (eq dialect :vs) "VS" "ISO") package))

(defun %pascal-set-attributes (b &key (dialect (%pascal-buffer-dialect b)) (package (%pascal-buffer-package b)))
  "Write (or rewrite) the attribute list on the first line of B."
  (let ((line (%pascal-attribute-line "PASCAL" dialect package)) (l (point-line b)) (c (point-col b)))
    (if (and (> (buf-nlines b) 0) (search "-*-" (buf-line b 0)))
        (progn (buf-delete b 0 0 0 (length (buf-line b 0))) (buf-goto b 0 0) (buf-insert b line))
        (progn (buf-goto b 0 0) (buf-insert b (format nil "~A~%" line)) (setq l (+ l 1))))
    (buf-goto b (min l (- (buf-nlines b) 1)) (min c (length (buf-line b (min l (- (buf-nlines b) 1))))))
    line))

;;; ---- colouring ---------------------------------------------------------------------------------------------

(defvar *pascal-reserved-table* (make-hash-table :test 'equal))
(dolist (w (append *pascal-iso-reserved* *pascal-vs-reserved*)) (setf (gethash w *pascal-reserved-table*) t))
(defvar *pascal-predeclared-table* (make-hash-table :test 'equal))
(dolist (w (pascal-predeclared-identifiers :vs)) (setf (gethash w *pascal-predeclared-table*) t))

(defun %pascal-template-at (line i)
  "The end (after ⊃) of the template item starting at byte I of LINE, or NIL."
  (and (<= (+ i +template-mark-length+) (length line)) (string= (subseq line i (+ i +template-mark-length+)) +template-open-string+)
       (let ((e (search +template-close-string+ line :start2 (+ i +template-mark-length+)))) (and e (+ e +template-mark-length+)))))

(defun pascal-line-syntax (line state)
  "Syntax spans of one line of Pascal: (values spans end-state).  A span is (start end class), CLASS one of :COMMENT
:STRING :BUILTIN (reserved words) :KEYWORD (predeclared identifiers) :DEFNAME (the names of programs and routines
being declared) :CONSTANT (true, false, nil and template items).  STATE is NIL, :BRACE (inside { }) or :PAREN (inside (* *))."
  (let ((i 0) (n (length line)) (spans nil) (expect-name nil) (start 0))
    (loop
      (when (>= i n) (return))
      (cond
        ((eq state :brace)
         (let ((e (%string-position-char line #\} i)))
           (setq spans (cons (list start (if e (+ e 1) n) :comment) spans) i (if e (+ e 1) n) state (if e nil :brace))))
        ((eq state :paren)
         (let ((e (search "*)" line :start2 i)))
           (setq spans (cons (list start (if e (+ e 2) n) :comment) spans) i (if e (+ e 2) n) state (if e nil :paren))))
        (t
         (let ((c (char line i)))
           (cond ((char= c #\{) (setq state :brace start i i (+ i 1)))
                 ((and (char= c #\() (< (+ i 1) n) (char= (char line (+ i 1)) #\*)) (setq state :paren start i i (+ i 2)))
                 ((char= c #\')
                  (let ((j (+ i 1)))
                    (loop (cond ((>= j n) (return))
                                ((and (char= (char line j) #\') (< (+ j 1) n) (char= (char line (+ j 1)) #\')) (setq j (+ j 2)))
                                ((char= (char line j) #\') (setq j (+ j 1)) (return))
                                (t (setq j (+ j 1)))))
                    (setq spans (cons (list i j :string) spans) i j)))
                 ((%pascal-template-at line i)
                  (let ((e (%pascal-template-at line i))) (setq spans (cons (list i e :constant) spans) i e)))
                 ((%pas-ident-start-p c)
                  (let ((j i))
                    (loop (when (or (>= j n) (not (%pas-ident-char-p (char line j)))) (return)) (setq j (+ j 1)))
                    (let* ((w (string-upcase (subseq line i j)))
                           (class (cond ((member w '("TRUE" "FALSE" "NIL") :test #'string=) :constant)
                                        ((gethash w *pascal-reserved-table*)
                                         (when (member w '("PROGRAM" "PROCEDURE" "FUNCTION" "UNIT_SPECIFICATION" "UNIT_IMPLEMENTATION") :test #'string=)
                                           (setq expect-name t))
                                         :builtin)
                                        (expect-name (setq expect-name nil) :defname)
                                        ((gethash w *pascal-predeclared-table*) :keyword)
                                        (t nil))))
                      (when class (setq spans (cons (list i j class) spans)))
                      (setq i j))))
                 (t (setq i (+ i 1))))))))
    (values (nreverse spans) state)))

(setf (get 'pascal-mode 'line-syntax) #'pascal-line-syntax)

;;; ---- code tokens of the buffer (outside comments and strings) --------------------------------------------------

(defun %pascal-line-tokens (b line)
  "The code tokens of LINE of B: a list of (text col), identifiers upper-cased; strings and comments are skipped."
  (let* ((text (buf-line b line)) (n (length text)) (spans (pascal-line-syntax text (%syntax-state-at b line #'pascal-line-syntax)))
         (acc nil) (i 0))
    (loop
      (when (>= i n) (return))
      (let ((sp (find-if (lambda (s) (and (<= (car s) i) (< i (cadr s)) (member (caddr s) '(:comment :string)))) spans)))
        (if sp
            (progn (when (eq (caddr sp) :string) (push (list "'" i) acc)) (setq i (cadr sp)))
            (let ((c (char text i)))
              (cond ((%pas-ident-start-p c)
                     (let ((j i)) (loop (when (or (>= j n) (not (%pas-ident-char-p (char text j)))) (return)) (setq j (+ j 1)))
                       (push (list (string-upcase (subseq text i j)) i) acc) (setq i j)))
                    ((member c '(#\Space #\Tab)) (setq i (+ i 1)))
                    ((and (char= c #\:) (< (+ i 1) n) (char= (char text (+ i 1)) #\=)) (push (list ":=" i) acc) (setq i (+ i 2)))
                    ((%pascal-template-at text i) (push (list "TEMPLATE" i) acc) (setq i (%pascal-template-at text i)))
                    (t (push (list (string c) i) acc) (setq i (+ i 1))))))))
    (nreverse acc)))

(defun %pascal-line-words (b line) (mapcar #'car (%pascal-line-tokens b line)))

(defun %line-indentation (text)
  (let ((i 0)) (loop (when (or (>= i (length text)) (not (member (char text i) '(#\Space #\Tab)))) (return i)) (setq i (+ i 1)))))

(defparameter *pascal-openers* '("BEGIN" "CASE" "RECORD" "REPEAT"))
(defparameter *pascal-closers* '("END" "UNTIL"))

;;; ---- indentation ---------------------------------------------------------------------------------------------

(defun %pascal-prev-code-line (b line)
  (let ((l (- line 1)))
    (loop (when (< l 0) (return nil))
          (when (%pascal-line-words b l) (return l))
          (setq l (- l 1)))))

(defun %pascal-matching-opener-line (b line)
  "The line of the BEGIN/CASE/RECORD/REPEAT that the END or UNTIL starting LINE closes."
  (let ((depth 1) (l (- line 1)))
    (loop
      (when (< l 0) (return nil))
      (dolist (w (reverse (%pascal-line-words b l)))
        (cond ((member w *pascal-closers* :test #'string=) (setq depth (+ depth 1)))
              ((member w *pascal-openers* :test #'string=)
               (setq depth (- depth 1))
               (when (= depth 0) (return-from %pascal-matching-opener-line l)))))
      (setq l (- l 1)))))

(defun %pascal-header-line (b line)
  "The line of the innermost routine (or program) heading whose body is still open at LINE, or NIL."
  (let ((depth 0) (closed nil) (l (- line 1)))
    (loop
      (when (< l 0) (return nil))
      (let ((words (%pascal-line-words b l)))
        (dolist (w (reverse words))
          (cond ((member w *pascal-closers* :test #'string=) (setq depth (+ depth 1)))
                ((member w *pascal-openers* :test #'string=)
                 (setq depth (- depth 1))
                 (when (and (<= depth 0) (string= w "BEGIN")) (setq closed t depth 0)))))
        (when (and (= depth 0) words (member (car words) '("PROCEDURE" "FUNCTION" "PROGRAM" "UNIT_SPECIFICATION" "UNIT_IMPLEMENTATION") :test #'string=))
          (if closed (setq closed nil) (return l))))
      (setq l (- l 1)))))

(defun pascal-indent-for-line (b line)
  "The column LINE of buffer B should be indented to."
  (let* ((words (%pascal-line-words b line)) (first (car words)) (prev (%pascal-prev-code-line b line)))
    (if (null prev) 0
        (let* ((pind (%line-indentation (buf-line b prev))) (pw (%pascal-line-words b prev)) (plast (car (last (remove ";" pw :test #'string=))))
               (pp (%pascal-prev-code-line b prev)) (ppw (and pp (%pascal-line-words b pp)))
               (pplast (car (last ppw))))
          (flet ((is (w set) (and w (member w set :test #'string=))))
            (cond
              ((is first '("END" "UNTIL"))
               (let ((o (%pascal-matching-opener-line b line))) (if o (%line-indentation (buf-line b o)) (max 0 (- pind *pascal-indent*)))))
              ((is first '("ELSE"))
               (let ((l prev))
                 (loop (when (or (null l) (is (car (%pascal-line-words b l)) '("IF"))) (return)) (setq l (%pascal-prev-code-line b l)))
                 (if l (%line-indentation (buf-line b l)) pind)))
              ((and (is first '("BEGIN")) (is plast '("THEN" "DO" "ELSE"))) pind)
              ((is first '("PROCEDURE" "FUNCTION"))
               (let ((h (%pascal-header-line b line))) (if h (+ (%line-indentation (buf-line b h)) *pascal-indent*) 0)))
              ((is first '("VAR" "CONST" "TYPE" "LABEL" "BEGIN"))
               (let ((h (%pascal-header-line b line))) (if h (%line-indentation (buf-line b h)) pind)))
              ((is first '("OTHERWISE"))
               (let ((o (%pascal-matching-opener-line b line)))
                 (if o (%line-indentation (buf-line b o)) pind)))
              ((is plast '("BEGIN" "REPEAT" "RECORD" "OF" "THEN" "DO" "ELSE" "VAR" "CONST" "TYPE" "LABEL" "OTHERWISE"))
               (+ pind *pascal-indent*))
              ((and (is (car pw) '("VAR" "CONST" "TYPE" "LABEL")) (cdr pw)) (+ pind *pascal-indent*))
              ((and (is pplast '("THEN" "DO" "ELSE")) (not (is (car pw) '("BEGIN"))))
               (%line-indentation (buf-line b pp)))
              (t pind)))))))

(defun pascal-indent-line (b line)
  "Indent LINE of B; returns T if it changed.  Point keeps its place in the text."
  (let* ((text (buf-line b line)) (have (%line-indentation text)) (want (pascal-indent-for-line b line)))
    (unless (= have want)
      (let ((pl (point-line b)) (pc (point-col b)))
        (buf-delete b line 0 line have)
        (buf-goto b line 0)
        (buf-insert b (make-string want :initial-element #\Space))
        (if (= pl line) (buf-goto b line (max want (+ pc (- want have)))) (buf-goto b pl pc))
        t))))

;;; ---- language units from the parser ---------------------------------------------------------------------------

(defparameter *pascal-statement-kinds* '(:assign :pcall :compound :if :while :repeat :for :case :with :goto :labeled :leave :continue :return :template))
(defparameter *pascal-definition-kinds* '(:program :unit-spec :unit-impl :routine))

(defun pascal-buffer-parse (b)
  "Parse buffer B (cached until it changes): (values units errors)."
  (let ((text (buffer-string b)) (cache (buf-local b 'pascal-parse)))
    (if (and cache (string= (car cache) text))
        (values (cadr cache) (caddr cache))
        (multiple-value-bind (units errors)
            (handler-case (pascal-parse text :dialect (%pascal-buffer-dialect b) :file (buffer-file b)
                                             :directory (if (buffer-file b) (path-directory (buffer-file b)) ""))
              (error (e) (values nil (list e))))
          (setf (buf-local b 'pascal-parse) (list text units errors))
          (values units errors)))))

(defun %pascal-nodes (units kinds)
  "Every node of UNITS whose kind is in KINDS, as (start end kind node)."
  (let ((acc nil))
    (labels ((walk (x)
               (when (consp x)
                 (when (and (keywordp (car x)) (consp (cdr x)) (integerp (cadr x)) (consp (cddr x)) (integerp (caddr x)) (member (car x) kinds))
                   (push (list (cadr x) (caddr x) (car x) x) acc))
                 (dolist (y (if (keywordp (car x)) (cdr x) x)) (walk y)))))
      (dolist (u units) (walk u)))
    acc))

(defun pascal-statements (b) (%pascal-nodes (pascal-buffer-parse b) *pascal-statement-kinds*))
(defun pascal-definitions (b) (%pascal-nodes (pascal-buffer-parse b) *pascal-definition-kinds*))

(defun %pascal-innermost (nodes off)
  (let ((best nil))
    (dolist (n nodes best)
      (when (and (<= (car n) off) (< off (cadr n)) (or (null best) (> (car n) (car best)) (and (= (car n) (car best)) (< (cadr n) (cadr best)))))
        (setq best n)))))

(defun %pascal-definition-at (b off)
  "The innermost routine or compilation unit around OFFSET, else the nearest one before it."
  (let ((defs (pascal-definitions b)))
    (or (%pascal-innermost defs off)
        (let ((best nil)) (dolist (d defs best) (when (and (<= (cadr d) off) (or (null best) (> (cadr d) (cadr best)))) (setq best d)))))))

;;; ---- the mode and its minor mode ------------------------------------------------------------------------------

(defcmd pascal-mode (ed arg)
  (with-b (b ed)
    (set-buffer-mode b 'pascal-mode)
    (when arg (%pascal-set-attributes b))
    (ed-message* ed "Pascal mode~:[ (with a numeric argument, the attribute list is written too)~;~]" arg)))

(defcmd electric-pascal-mode (ed arg)
  (with-b (b ed)
    (unless (%pascal-buffer-p b) (error "Electric Pascal mode is available only in Pascal buffers"))
    (if (member 'electric (buffer-minor-modes b))
        (progn (setf (buffer-minor-modes b) (remove 'electric (buffer-minor-modes b))) (ed-message* ed "Electric Pascal mode off"))
        (progn (push 'electric (buffer-minor-modes b)) (ed-message* ed "Electric Pascal mode on")))))

(defun %pascal-case (word style)
  (case style
    (:upper (string-upcase word))
    (:lower (string-downcase word))
    (:capitalize (if (> (length word) 0) (%string-concat (string-upcase (subseq word 0 1)) (string-downcase (subseq word 1))) word))
    (:capitalize-words (%join-strings (mapcar (lambda (p) (%pascal-case p :capitalize)) (%split-string word #\_)) "_"))
    (t word)))

(defun %pascal-reserved-case (b) (buf-local b 'pascal-reserved-case *pascal-reserved-word-case*))
(defun %pascal-identifier-case (b) (buf-local b 'pascal-identifier-case *pascal-identifier-case*))

(defun %pascal-class-at (b line col)
  (let ((sp (find-if (lambda (s) (and (<= (car s) col) (< col (cadr s))))
                     (pascal-line-syntax (buf-line b line) (%syntax-state-at b line #'pascal-line-syntax)))))
    (and sp (caddr sp))))

(defun %pascal-recase-word (b line start end)
  "Put the identifier at START..END of LINE in the case Electric Pascal mode gives its kind."
  (let* ((text (buf-line b line)) (word (subseq text start end))
         (style (if (gethash (string-upcase word) *pascal-reserved-table*) (%pascal-reserved-case b) (%pascal-identifier-case b)))
         (new (%pascal-case word style)))
    (unless (string= new word)
      (let ((pl (point-line b)) (pc (point-col b)))
        (buf-delete b line start line end)
        (buf-goto b line start)
        (buf-insert b new)
        (buf-goto b pl pc)))))

(defun %pascal-after-insert (ed c)
  "Electric Pascal mode: when a character that ends a word is typed, give the word its case."
  (let ((b (ed-buffer ed)))
    (when (and (member 'electric (buffer-minor-modes b)) (< c 128) (not (%pas-ident-char-p (code-char c))))
      (let* ((line (point-line b)) (text (buf-line b line)) (end (- (point-col b) 1)) (start end))
        (loop (when (or (<= start 0) (not (%pas-ident-char-p (char text (- start 1))))) (return)) (setq start (- start 1)))
        (when (and (> end start) (%pas-ident-start-p (char text start)) (not (member (%pascal-class-at b line start) '(:comment :string))))
          (%pascal-recase-word b line start end))))))

(defun %pascal-electric-word-before (ed)
  "In Electric Pascal mode, give the word just before point its case (when a line is ended)."
  (let ((b (ed-buffer ed)))
    (when (and (%pascal-buffer-p b) (member 'electric (buffer-minor-modes b)) (> (point-col b) 0))
      (let* ((line (point-line b)) (text (buf-line b line)) (end (point-col b)) (start end))
        (loop (when (or (<= start 0) (not (%pas-ident-char-p (char text (- start 1))))) (return)) (setq start (- start 1)))
        (when (and (> end start) (%pas-ident-start-p (char text start)) (not (member (%pascal-class-at b line start) '(:comment :string))))
          (%pascal-recase-word b line start end))))))

(defcmd pascal-newline (ed arg)
  (%pascal-electric-word-before ed)
  (with-b (b ed) (dotimes (i (%n arg)) (buf-insert b (string #\Newline)))))

(defun %pascal-before-insert (ed c)
  "Typing at the start of a template item replaces it; a space just deletes it."
  (let* ((b (ed-buffer ed)) (text (buf-current-line b)) (col (point-col b)) (e (%pascal-template-at text col)))
    (when e
      (buf-delete b (point-line b) col (point-line b) e)
      (= c 32))))

(setf (get 'pascal-mode 'after-self-insert) #'%pascal-after-insert
      (get 'pascal-mode 'before-self-insert) #'%pascal-before-insert)

(defcmd adjust-face-and-case (ed arg)
  (let ((b (ed-buffer ed)) (choices "upper, lower, capitalize, capitalize-words, leave"))
    (flet ((parse (s) (let ((k (find (string-upcase (string-trim " " s)) '(:upper :lower :capitalize :capitalize-words :leave) :key #'symbol-name :test #'string=)))
                        (or k (error "Choose one of ~A" choices)))))
      (ed-read-string ed (format nil "Case of reserved words (~A) [~(~A~)]: " choices (%pascal-reserved-case b))
                      (lambda (s1)
                        (unless (string= (string-trim " " s1) "") (setf (buf-local b 'pascal-reserved-case) (parse s1)))
                        (ed-read-string ed (format nil "Case of identifiers (~A) [~(~A~)]: " choices (%pascal-identifier-case b))
                                        (lambda (s2)
                                          (unless (string= (string-trim " " s2) "") (setf (buf-local b 'pascal-identifier-case) (parse s2)))
                                          (ed-message* ed "Reserved words ~(~A~), identifiers ~(~A~); Format Language Region applies them"
                                                       (%pascal-reserved-case b) (%pascal-identifier-case b)))))))))

(defun %pascal-region-or-routine (ed)
  "(values start-offset end-offset) of the region, or else of the routine at point."
  (let ((b (ed-buffer ed)))
    (multiple-value-bind (l1 c1 l2 c2) (region-bounds b)
      (if l1 (values (buf-offset b l1 c1) (buf-offset b l2 c2))
          (let ((d (%pascal-definition-at b (point-offset b))))
            (if d (values (car d) (cadr d)) (values 0 (length (buffer-string b)))))))))

(defcmd format-language-region (ed arg)
  (with-b (b ed)
    (multiple-value-bind (s e) (%pascal-region-or-routine ed)
      (let ((p1 (buf-pos-of-offset b s)) (p2 (buf-pos-of-offset b e)) (n 0))
        (do ((l (car p1) (+ l 1))) ((> l (car p2)))
          (dolist (tk (%pascal-line-tokens b l))
            (let ((col (cadr tk)) (w (car tk)))
              (when (and (%pas-ident-start-p (char w 0)) (or (> l (car p1)) (>= col (cdr p1))) (or (< l (car p2)) (< col (cdr p2))))
                (if arg
                    (when (gethash w *pascal-reserved-table*)
                      (buf-delete b l col l (+ col (length w))) (buf-goto b l col) (buf-insert b (string-downcase w)))
                    (%pascal-recase-word b l col (+ col (length w))))
                (setq n (+ n 1))))))
        (ed-message* ed "~:[Formatted~;Removed the formatting of~] ~D word~:P" arg n)))))

;;; ---- motion and deletion by language unit ---------------------------------------------------------------------

(defun %pascal-goto (b off) (goto-offset b (max 0 (min off (length (buffer-string b))))))

(defcmd pascal-beginning-of-definition (ed arg)
  (with-b (b ed)
    (let* ((off (point-offset b)) (defs (pascal-definitions b)) (best nil))
      (dolist (d defs) (when (and (< (car d) off) (or (null best) (> (car d) (car best)))) (setq best d)))
      (if best (progn (push-mark ed) (%pascal-goto b (car best))) (ed-message* ed "No definition before point")))))

(defcmd pascal-end-of-definition (ed arg)
  (with-b (b ed)
    (dotimes (i (%n arg))
      (let* ((off (point-offset b)) (defs (pascal-definitions b))
             (cur (%pascal-innermost (remove :program (remove :unit-spec (remove :unit-impl defs :key #'caddr) :key #'caddr) :key #'caddr) off))
             (target (or cur (let ((best nil)) (dolist (d defs best) (when (and (> (cadr d) off) (or (null best) (< (cadr d) (cadr best)))) (setq best d)))))))
        (if target
            (progn (push-mark ed) (%pascal-goto b (cadr target))
                   ;; past the ";" that ends a routine
                   (let ((line (buf-current-line b)) (c (point-col b))) (when (and (< c (length line)) (char= (char line c) #\;)) (buf-goto b (point-line b) (+ c 1)))))
            (ed-message* ed "No definition after point"))))))

(defcmd pascal-mark-definition (ed arg)
  (with-b (b ed)
    (let ((d (%pascal-definition-at b (point-offset b))))
      (unless d (error "No definition here"))
      (%pascal-goto b (cadr d))
      (let ((p (buf-pos-of-offset b (cadr d)))) (setf (buffer-mark b) (cons (car p) (cdr p))))
      (%pascal-goto b (car d))
      (ed-message* ed "Marked ~(~A~)" (caddr d)))))

(defun %pascal-statement-forward-end (b off)
  (let* ((stmts (pascal-statements b)) (cur (%pascal-innermost (remove :compound stmts :key #'caddr) off)))
    (if cur (cadr cur)
        (let ((best nil)) (dolist (s stmts) (when (and (> (car s) off) (or (null best) (< (car s) (car best)) (and (= (car s) (car best)) (> (cadr s) (cadr best))))) (setq best s)))
          (and best (cadr best))))))

(defun %pascal-statement-backward-start (b off)
  (let ((best nil))
    (dolist (s (pascal-statements b) (and best (car best)))
      (when (and (< (car s) off) (not (eq (caddr s) :compound)) (or (null best) (> (car s) (car best)) (and (= (car s) (car best)) (> (cadr s) (cadr best)))))
        (setq best s)))))

(defcmd pascal-forward-statement (ed arg)
  (with-b (b ed) (dotimes (i (%n arg)) (let ((e (%pascal-statement-forward-end b (point-offset b)))) (if e (%pascal-goto b e) (error "No more statements"))))))
(defcmd pascal-backward-statement (ed arg)
  (with-b (b ed) (dotimes (i (%n arg)) (let ((s (%pascal-statement-backward-start b (point-offset b)))) (if s (%pascal-goto b s) (error "No previous statement"))))))

(defun %pascal-kill-range (ed b from to)
  (let ((p1 (buf-pos-of-offset b (min from to))) (p2 (buf-pos-of-offset b (max from to))))
    (do-kill ed (buf-delete b (car p1) (cdr p1) (car p2) (cdr p2)))))

(defun %pascal-check-balanced (b from to)
  "Refuse to delete a construct containing a syntax error."
  (multiple-value-bind (units errors) (pascal-buffer-parse b)
    (declare (ignore units))
    (dolist (e errors)
      (when (and (pascal-error-start e) (<= (min from to) (pascal-error-start e)) (< (pascal-error-start e) (max from to)))
        (error "No balanced language construct to delete: ~A" (pascal-error-message e))))))

(defcmd pascal-kill-statement (ed arg)
  (with-b (b ed)
    (let* ((off (point-offset b)) (e (%pascal-statement-forward-end b off)))
      (unless e (error "No balanced language construct to delete"))
      (%pascal-check-balanced b off e)
      (%pascal-kill-range ed b off e))))

(defcmd pascal-backward-kill-statement (ed arg)
  (with-b (b ed)
    (let* ((off (point-offset b)) (s (%pascal-statement-backward-start b off)))
      (unless s (error "No balanced language construct to delete"))
      (%pascal-check-balanced b s off)
      (%pascal-kill-range ed b s off))))

(defcmd pascal-kill-statement-around (ed arg)
  (with-b (b ed)
    (let* ((off (point-offset b)) (line-start (buf-offset b (point-line b) 0))
           (stmts (remove :compound (pascal-statements b) :key #'caddr))
           ;; the largest statement starting on this line at or after its indentation, else the innermost around point
           (s (or (let ((best nil)) (dolist (x stmts best) (when (and (<= line-start (car x)) (<= (car x) (max off (+ line-start (%line-indentation (buf-current-line b)))))
                                                                       (or (null best) (> (cadr x) (cadr best))))
                                                              (setq best x))))
                  (%pascal-innermost stmts off))))
      (unless s (error "No statement here"))
      (%pascal-check-balanced b (car s) (cadr s))
      (%pascal-kill-range ed b (car s) (cadr s)))))

;;; expressions and tokens: character scanning
(defun %pascal-skip-blanks (s i) (loop (when (or (>= i (length s)) (not (member (char s i) '(#\Space #\Tab #\Newline)))) (return i)) (setq i (+ i 1))))

(defun %pascal-token-end (s i)
  "The end of the token starting at I in S (after blanks): an identifier, number, string, template item or symbol."
  (let ((i (%pascal-skip-blanks s i)) (n (length s)))
    (cond ((>= i n) nil)
          ((%pascal-template-at s i))
          ((%pas-ident-char-p (char s i)) (loop (when (or (>= i n) (not (or (%pas-ident-char-p (char s i)) (char= (char s i) #\.)))) (return i)) (setq i (+ i 1))))
          ((char= (char s i) #\') (let ((j (+ i 1))) (loop (cond ((>= j n) (return n)) ((char= (char s j) #\') (return (+ j 1))) (t (setq j (+ j 1)))))))
          ((and (< (+ i 1) n) (member (subseq s i (+ i 2)) '(":=" "<=" ">=" "<>" ".." "||") :test #'string=)) (+ i 2))
          (t (+ i 1)))))

(defun %pascal-expression-end (s i)
  (let* ((j (%pascal-skip-blanks s i)) (n (length s)))
    (cond ((>= j n) nil)
          ((member (char s j) '(#\( #\[))
           (let ((depth 0) (k j))
             (loop (when (>= k n) (return nil))
                   (let ((c (char s k)))
                     (cond ((member c '(#\( #\[)) (setq depth (+ depth 1)))
                           ((member c '(#\) #\])) (setq depth (- depth 1)) (when (= depth 0) (return (+ k 1))))
                           ((char= c #\') (setq k (- (or (%pascal-token-end s k) n) 1)))))
                   (setq k (+ k 1)))))
          ((member (char s j) '(#\) #\])) nil)
          (t (let ((e (%pascal-token-end s j)))
               ;; a name followed by an argument list or subscripts goes with them
               (if (and e (%pas-ident-start-p (char s j)) (< e n) (member (char s e) '(#\( #\[))) (%pascal-expression-end s e) e))))))

(defun %pascal-expression-start (s i)
  (let ((j (- i 1)))
    (loop (when (or (< j 0) (not (member (char s j) '(#\Space #\Tab #\Newline)))) (return)) (setq j (- j 1)))
    (cond ((< j 0) nil)
          ((member (char s j) '(#\) #\]))
           (let ((depth 0) (k j))
             (loop (when (< k 0) (return nil))
                   (let ((c (char s k)))
                     (cond ((member c '(#\) #\])) (setq depth (+ depth 1)))
                           ((member c '(#\( #\[)) (setq depth (- depth 1))
                            (when (= depth 0)
                              (let ((m k)) (loop (when (or (<= m 0) (not (%pas-ident-char-p (char s (- m 1))))) (return)) (setq m (- m 1)))
                                (return m))))))
                   (setq k (- k 1)))))
          ((member (char s j) '(#\( #\[)) nil)
          ((%pas-ident-char-p (char s j)) (loop (when (or (<= j 0) (not (%pas-ident-char-p (char s (- j 1))))) (return j)) (setq j (- j 1))))
          (t j))))

(defcmd pascal-forward-expression (ed arg)
  (with-b (b ed) (dotimes (i (%n arg)) (let ((e (%pascal-expression-end (buffer-string b) (point-offset b)))) (if e (%pascal-goto b e) (error "No balanced expression after point"))))))
(defcmd pascal-backward-expression (ed arg)
  (with-b (b ed) (dotimes (i (%n arg)) (let ((s (%pascal-expression-start (buffer-string b) (point-offset b)))) (if s (%pascal-goto b s) (error "No balanced expression before point"))))))
(defcmd pascal-kill-expression (ed arg)
  (with-b (b ed) (let* ((off (point-offset b)) (e (%pascal-expression-end (buffer-string b) off)))
                   (unless e (error "No balanced language construct to delete"))
                   (%pascal-kill-range ed b off e))))
(defcmd pascal-backward-kill-expression (ed arg)
  (with-b (b ed) (let* ((off (point-offset b)) (s (%pascal-expression-start (buffer-string b) off)))
                   (unless s (error "No balanced language construct to delete"))
                   (%pascal-kill-range ed b s off))))
(defcmd pascal-kill-token (ed arg)
  (with-b (b ed) (let* ((off (point-offset b)) (e (%pascal-token-end (buffer-string b) off)))
                   (unless e (error "No token after point"))
                   (%pascal-kill-range ed b off e))))
(defcmd pascal-backward-kill-token (ed arg)
  (with-b (b ed) (let* ((s (buffer-string b)) (off (point-offset b)) (j (- off 1)))
                   (loop (when (or (< j 0) (not (member (char s j) '(#\Space #\Tab #\Newline)))) (return)) (setq j (- j 1)))
                   (when (< j 0) (error "No token before point"))
                   (let ((start (if (%pas-ident-char-p (char s j))
                                    (loop (when (or (<= j 0) (not (%pas-ident-char-p (char s (- j 1))))) (return j)) (setq j (- j 1)))
                                    j)))
                     (%pascal-kill-range ed b start off)))))

;;; ---- syntax errors -------------------------------------------------------------------------------------------

(defun %pascal-show-error (ed b c)
  (when (pascal-error-start c) (%pascal-goto b (pascal-error-start c)))
  (ed-message* ed "Syntax error: ~A" (pascal-error-message c)))

(defcmd pascal-next-syntax-error (ed arg)
  (with-b (b ed)
    (multiple-value-bind (units errors) (pascal-buffer-parse b)
      (declare (ignore units))
      (let* ((off (point-offset b)) (errs (remove-if-not #'pascal-error-start errors))
             (e (if arg (car (last errs)) (find-if (lambda (c) (> (pascal-error-start c) off)) errs))))
        (cond (e (%pascal-show-error ed b e))
              (errs (ed-message* ed "No more syntax errors after point (~D before it)" (length errs)))
              (t (ed-message* ed "No syntax errors")))))))

(defcmd pascal-previous-syntax-error (ed arg)
  (with-b (b ed)
    (multiple-value-bind (units errors) (pascal-buffer-parse b)
      (declare (ignore units))
      (let* ((off (point-offset b)) (errs (remove-if-not #'pascal-error-start errors))
             (e (if arg (car errs) (find-if (lambda (c) (< (pascal-error-start c) off)) errs :from-end t))))
        (cond (e (%pascal-show-error ed b e))
              (errs (ed-message* ed "No syntax errors before point"))
              (t (ed-message* ed "No syntax errors")))))))

(defcmd pascal-indent-command (ed arg)
  (%pascal-electric-word-before ed)
  (with-b (b ed)
    (multiple-value-bind (l1 c1 l2 c2) (region-bounds b)
      (declare (ignore c1 c2))
      (if (and l1 (/= l1 l2))
          (do ((l l1 (+ l 1))) ((> l l2)) (pascal-indent-line b l))
          (progn (pascal-indent-line b (point-line b))
                 (let ((ws (%line-indentation (buf-current-line b)))) (when (< (point-col b) ws) (buf-goto b (point-line b) ws))))))))

(defcmd pascal-newline-and-indent (ed arg)
  ;; LINE: end the line, indent the next, and report a syntax error in the line just ended
  (%pascal-electric-word-before ed)
  (with-b (b ed)
    (let ((ended (point-line b)))
      (pascal-indent-line b ended)
      (buf-insert b (string #\Newline))
      (pascal-indent-line b (point-line b))
      (buf-goto b (point-line b) (%line-indentation (buf-current-line b)))
      (multiple-value-bind (units errors) (pascal-buffer-parse b)
        (declare (ignore units))
        (let ((e (find-if (lambda (c) (and (pascal-error-start c) (= (car (buf-pos-of-offset b (pascal-error-start c))) ended))) errors)))
          (when e
            (let ((col (cdr (buf-pos-of-offset b (pascal-error-start e)))))
              (ed-message* ed "~A  <-- ~A (column ~D)" (string-trim " " (subseq (buf-line b ended) 0 (min col (length (buf-line b ended)))))
                           (pascal-error-message e) (+ col 1)))))))))

;;; ---- templates -----------------------------------------------------------------------------------------------

(defun %tpl (s) (%pascal-template-expand s))
(defun %pascal-template-expand (s)
  "S with <x> written as the template item ⊂x⊃ and ~% as a newline."
  (let ((out (make-string-output-stream)) (i 0) (n (length s)))
    (loop (when (>= i n) (return))
          (let ((c (char s i)))
            (cond ((char= c #\<) (write-string +template-open-string+ out) (setq i (+ i 1)))
                  ((char= c #\>) (write-string +template-close-string+ out) (setq i (+ i 1)))
                  ((and (char= c #\~) (< (+ i 1) n) (char= (char s (+ i 1)) #\%)) (write-char #\Newline out) (setq i (+ i 2)))
                  (t (write-char c out) (setq i (+ i 1))))))
    (get-output-stream-string out)))

(defparameter *pascal-templates*
  ;; (keyword label context template), <item> marks template items
  '(("PROGRAM" "Program" :unit "PROGRAM <identifier>;~%<{ declaration }>~%BEGIN~%  <statement> <{; statement}>~%END.")
    ("UNIT_SPECIFICATION" "Unit specification" :unit "UNIT_SPECIFICATION <identifier>;~%<{ declaration }>~%<{ routine heading }>~%END.")
    ("UNIT_IMPLEMENTATION" "Unit implementation" :unit "UNIT_IMPLEMENTATION <identifier>;~%<{ declaration }>~%<{ routine }>~%END.")
    ("PROCEDURE" "Procedure declaration" :declaration "PROCEDURE <identifier><[ formal part ]>;~%<{ declaration }>~%BEGIN~%  <statement> <{; statement}>~%END;")
    ("FUNCTION" "Function declaration" :declaration "FUNCTION <identifier><[ formal part ]> : <type identifier>;~%<{ declaration }>~%BEGIN~%  <statement> <{; statement}>~%END;")
    ("VAR" "Variable declaration" :declaration "VAR <identifier> <{, identifier}> : <type>;")
    ("CONST" "Constant declaration" :declaration "CONST <identifier> = <constant>;")
    ("TYPE" "Type declaration" :declaration "TYPE <identifier> = <type>;")
    ("LABEL" "Label declaration" :declaration "LABEL <label> <{, label}>;")
    ("BEGIN" "Compound statement" :statement "BEGIN~%  <statement> <{; statement}>~%END")
    ("ASSIGNMENT" "Assignment statement" :statement "<variable> := <expression>")
    ("CALL" "Procedure call" :statement "<procedure identifier><[ actual parameter list ]>")
    ("IF" "If statement" :statement "IF <expression> THEN~%  <statement>~%<[ ELSE statement ]>")
    ("CASE" "Case statement" :statement "CASE <expression> OF~%  <case label list> : <statement> <{; case label list : statement}>~%<[ OTHERWISE statement ]>~%END")
    ("WHILE" "While statement" :statement "WHILE <expression> DO~%  <statement>")
    ("REPEAT" "Repeat statement" :statement "REPEAT~%  <statement> <{; statement}>~%UNTIL <expression>")
    ("FOR" "For statement" :statement "FOR <control variable> := <initial value> <to or downto> <final value> DO~%  <statement>")
    ("WITH" "With statement" :statement "WITH <record variable> <{, record variable}> DO~%  <statement>")
    ("GOTO" "Goto statement" :statement "GOTO <label>")
    ("ARRAY" "Array type" :type "ARRAY [<index type> <{, index type}>] OF <component type>")
    ("RECORD" "Record type" :type "RECORD~%  <field list>~%END")
    ("SET" "Set type" :type "SET OF <base type>")
    ("FILE" "File type" :type "FILE OF <component type>")))

(defparameter *pascal-template-refinements*
  ;; item (without its horseshoes) -> replacements; :statement :declaration :type stand for menus of templates
  '(("initial value" "<expression>") ("final value" "<expression>") ("to or downto" "TO" "DOWNTO")
    ("statement" :statement) ("{; statement}" "; <statement> <{; statement}>")
    ("{ declaration }" :declaration) ("{ routine }" "PROCEDURE" "FUNCTION") ("{ routine heading }" "PROCEDURE <identifier><[ formal part ]>;" "FUNCTION <identifier><[ formal part ]> : <type identifier>;")
    ("type" :type "integer" "real" "boolean" "char" "<type identifier>" "<low> .. <high>" "(<identifier> <{, identifier}>)")
    ("component type" :type "integer" "real" "boolean" "char") ("base type" "<ordinal type>") ("type identifier" "integer" "real" "boolean" "char")
    ("[ ELSE statement ]" "ELSE~%  <statement>") ("[ OTHERWISE statement ]" "OTHERWISE <statement>")
    ("{, identifier}" ", <identifier> <{, identifier}>") ("{, label}" ", <label> <{, label}>")
    ("{, record variable}" ", <record variable> <{, record variable}>") ("{, index type}" ", <index type> <{, index type}>")
    ("[ formal part ]" "(<formal parameter section> <{; formal parameter section}>)")
    ("formal parameter section" "<identifier> <{, identifier}> : <type identifier>" "VAR <identifier> <{, identifier}> : <type identifier>"
     "PROCEDURE <identifier><[ formal part ]>" "FUNCTION <identifier><[ formal part ]> : <type identifier>")
    ("{; formal parameter section}" "; <formal parameter section> <{; formal parameter section}>")
    ("[ actual parameter list ]" "(<expression> <{, expression}>)") ("{, expression}" ", <expression> <{, expression}>")
    ("{; case label list : statement}" ";~%  <case label list> : <statement> <{; case label list : statement}>")
    ("case label list" "<constant> <{, constant}>") ("{, constant}" ", <constant> <{, constant}>")
    ("index type" "<low> .. <high>" "<ordinal type identifier>") ("field list" "<identifier> <{, identifier}> : <type>; <field list>")))

(defun %pascal-recase-template (b text)
  "TEXT with its reserved words in the case chosen for the buffer."
  (let ((out (make-string-output-stream)) (i 0) (n (length text)) (style (%pascal-reserved-case b)))
    (loop (when (>= i n) (return))
          (let ((e (%pascal-template-at text i)))
            (cond (e (write-string (subseq text i e) out) (setq i e))
                  ((%pas-ident-start-p (char text i))
                   (let ((j i)) (loop (when (or (>= j n) (not (%pas-ident-char-p (char text j)))) (return)) (setq j (+ j 1)))
                     (let ((w (subseq text i j))) (write-string (if (gethash (string-upcase w) *pascal-reserved-table*) (%pascal-case w style) w) out))
                     (setq i j)))
                  (t (write-char (char text i) out) (setq i (+ i 1))))))
    (get-output-stream-string out)))

(defun %pascal-insert-template (b text)
  "Insert the template TEXT at point, indenting its lines to point's column; point goes to its first item."
  (let* ((col (display-col (buf-current-line b) (point-col b)))
         (lines (%split-string (%pascal-recase-template b (%tpl text)) #\Newline))
         (start (point-offset b))
         (body (%join-strings (cons (car lines) (mapcar (lambda (l) (%string-concat (make-string col :initial-element #\Space) l)) (cdr lines)))
                              (string #\Newline))))
    (buf-insert b body)
    (let ((item (search +template-open-string+ (buffer-string b) :start2 start)))
      (when (and item (< item (+ start (length body)))) (%pascal-goto b item)))))

(defun %pascal-template-menu (ed title templates)
  "Offer TEMPLATES ((label . text) ...) as possibilities; choosing one inserts it where point was."
  (let* ((b (ed-buffer ed)) (off (point-offset b)))
    (ed-set-possibilities ed title
                          (mapcar (lambda (tp)
                                    (make-possibility (car tp)
                                                      (lambda (e) (ed-switch-to-buffer e b) (%pascal-goto b off)
                                                        (let ((it (%pascal-template-at (buf-current-line b) (point-col b))))
                                                          (when it (buf-delete b (point-line b) (point-col b) (point-line b) it)))
                                                        (%pascal-insert-template b (cdr tp)))))
                                  templates)
                          :go nil)))

(defun %pascal-templates-for (context)
  (mapcar (lambda (tp) (cons (cadr tp) (cadddr tp))) (remove-if-not (lambda (tp) (eq (caddr tp) context)) *pascal-templates*)))

(defun %pascal-context (b off)
  "Whether OFF is among statements (:statement) or declarations (:declaration)."
  (multiple-value-bind (units) (pascal-buffer-parse b)
    (if (%pascal-innermost (%pascal-nodes units '(:compound :repeat :case)) off) :statement
        (if units :declaration :unit))))

(defun %pascal-word-before (b)
  "(values word start-col) of the identifier ending at (or just before) point on its line."
  (let* ((text (buf-current-line b)) (c (point-col b)) (e c))
    (loop (when (or (<= e 0) (not (member (char text (- e 1)) '(#\Space #\Tab)))) (return)) (setq e (- e 1)))
    (let ((s e)) (loop (when (or (<= s 0) (not (%pas-ident-char-p (char text (- s 1))))) (return)) (setq s (- s 1)))
      (if (> e s) (values (subseq text s e) s) (values nil nil)))))

(defcmd pascal-insert-template (ed arg)
  ;; END: the template of the keyword before point; or a menu of the constructs valid here
  (with-b (b ed)
    (multiple-value-bind (word col) (%pascal-word-before b)
      (let ((tp (and word (find (string-upcase word) *pascal-templates* :key #'car :test #'string=))))
        (if tp
            (progn (buf-delete b (point-line b) col (point-line b) (point-col b))
                   (buf-goto b (point-line b) col)
                   (%pascal-insert-template b (cadddr tp)))
            (run-editor-command ed 'pascal-construct-help arg))))))

(defcmd pascal-construct-help (ed arg)
  ;; c-HELP: the language constructs valid at point
  (with-b (b ed)
    (let ((ctx (%pascal-context b (point-offset b))))
      (%pascal-template-menu ed (format nil "~(~A~) templates (RET inserts)" ctx)
                             (if (eq ctx :unit) (%pascal-templates-for :unit) (%pascal-templates-for ctx))))))

(defun %pascal-item-at-point (b)
  "(values start end item-text) of the template item at or around point on its line."
  (let* ((text (buf-current-line b)) (c (point-col b)) (open (search +template-open-string+ (subseq text 0 (min (length text) (+ c +template-mark-length+))) :from-end t)))
    (when open
      (let ((e (%pascal-template-at text open)))
        (when (and e (< c e)) (values open e (subseq text (+ open +template-mark-length+) (- e +template-mark-length+))))))))

(defcmd pascal-complete (ed arg)
  ;; COMPLETE: refine the template item at point, or complete the reserved word or predeclared name before point
  (with-b (b ed)
    (multiple-value-bind (s e item) (%pascal-item-at-point b)
      (if s
          (let ((ref (cdr (assoc (string-trim " " item) *pascal-template-refinements* :test #'string=))))
            (cond ((null ref) (ed-message* ed "No completion for ~A~A~A" +template-open-string+ item +template-close-string+))
                  ((and (null (cdr ref)) (stringp (car ref)))
                   (buf-delete b (point-line b) s (point-line b) e) (buf-goto b (point-line b) s)
                   (%pascal-insert-template b (car ref)))
                  (t (buf-goto b (point-line b) s)
                     (%pascal-template-menu ed (format nil "completions of ~A" item)
                                            (apply #'append (mapcar (lambda (r) (if (keywordp r) (%pascal-templates-for r) (list (cons (%tpl r) r)))) ref))))))
          (multiple-value-bind (word col) (%pascal-word-before b)
            (unless (and word (= (+ col (length word)) (point-col b))) (error "Nothing to complete here"))
            (let* ((u (string-upcase word))
                   (cands (remove-if-not (lambda (w) (%word-prefix-p u w))
                                         (append (remove-if-not (lambda (w) (pascal-reserved-word-p w (%pascal-buffer-dialect b))) (append *pascal-iso-reserved* *pascal-vs-reserved*))
                                                 (pascal-predeclared-identifiers (%pascal-buffer-dialect b))))))
              (cond ((null cands) (ed-message* ed "No completions of ~A" word))
                    ((null (cdr cands))
                     (let* ((w (car cands)) (style (if (gethash w *pascal-reserved-table*) (%pascal-reserved-case b) (%pascal-identifier-case b)))
                            (full (%pascal-case w (if (eq style :leave) (if (string= word (string-upcase word)) :upper :lower) style))))
                       (buf-delete b (point-line b) col (point-line b) (point-col b))
                       (buf-goto b (point-line b) col)
                       (buf-insert b full)))
                    (t (let ((common (reduce (lambda (a c) (subseq a 0 (or (mismatch a c) (length a)))) cands)))
                         (when (> (length common) (length u))
                           (buf-insert b (%pascal-case (subseq common (length u)) (if (string= word (string-upcase word)) :upper :lower))))
                         (ed-message* ed "Possible completions: ~{~(~A~)~^ ~}" cands))))))))))

(defcmd pascal-list-predeclared-completions (ed arg)
  ;; c-?: the predeclared identifiers that complete the name before point
  (with-b (b ed)
    (let* ((word (or (%pascal-word-before b) "")) (u (string-upcase word))
           (cands (remove-if-not (lambda (w) (%word-prefix-p u w)) (pascal-predeclared-identifiers (%pascal-buffer-dialect b)))))
      (if cands (ed-message* ed "~{~(~A~)~^ ~}" cands) (ed-message* ed "No predeclared identifier starts with ~A" word)))))

(defcmd pascal-next-template-item (ed arg)
  (with-b (b ed)
    (let* ((s (buffer-string b)) (off (point-offset b))
           (p (search +template-open-string+ s :start2 (min (length s) (if (%pascal-template-at s off) (+ off +template-mark-length+) off)))))
      (if p (%pascal-goto b p) (ed-message* ed "No more template items")))))

(defcmd pascal-previous-template-item (ed arg)
  (with-b (b ed)
    (let* ((s (buffer-string b)) (off (point-offset b)) (p (and (> off 0) (search +template-open-string+ (subseq s 0 off) :from-end t))))
      (if p (%pascal-goto b p) (ed-message* ed "No previous template items")))))

(defcmd remove-template-item (ed arg)
  (with-b (b ed)
    (let* ((s (buffer-string b)) (off (point-offset b)) (p (search +template-open-string+ s :start2 off)))
      (unless p (error "No template item after point"))
      (let ((e (%pascal-template-at s p))) (%pascal-kill-range ed b p e)))))

(defcmd pascal-close-construct (ed arg)
  ;; c-END: insert what closes the innermost construct open before point
  (with-b (b ed)
    (let ((stack nil) (pl (point-line b)) (pc (point-col b)))
      (dotimes (l (+ pl 1))
        (dolist (tk (%pascal-line-tokens b l))
          (when (or (< l pl) (< (cadr tk) pc))
            (let ((w (car tk)))
              (cond ((member w '("BEGIN" "CASE" "RECORD") :test #'string=) (push "END" stack))
                    ((string= w "REPEAT") (push "UNTIL" stack))
                    ((string= w "(") (push ")" stack)) ((string= w "[") (push "]" stack))
                    ((member w '("END" "UNTIL" ")" "]") :test #'string=) (pop stack)))))))
      (if (null stack) (ed-message* ed "No open construct before point")
          (let ((closer (car stack)))
            (if (member closer '(")" "]") :test #'string=)
                (buf-insert b closer)
                (progn (unless (= (%line-indentation (buf-current-line b)) (length (buf-current-line b)))
                         (buf-insert b (string #\Newline)))
                       (buf-insert b (%pascal-case closer (%pascal-reserved-case b)))
                       (pascal-indent-line b (point-line b))
                       (when (string= closer "UNTIL") (buf-insert b (%string-concat " " (%tpl "<expression>")))))))))))

;;; ---- blinking the matching construct ---------------------------------------------------------------------------

(defun pascal-match-offsets (b)
  "With Blink Matching Construct on: the offsets of the END or UNTIL at point and of the reserved word it closes."
  (when *pascal-blink-matching*
    (let* ((line (point-line b)) (col (point-col b))
           (tk (find-if (lambda (tk) (and (member (car tk) *pascal-closers* :test #'string=) (< (cadr tk) col) (<= col (+ (cadr tk) (length (car tk))))))
                        (%pascal-line-tokens b line))))
      (when tk
        (let ((depth 0) (l line))
          (loop
            (when (< l 0) (return nil))
            (dolist (o (reverse (%pascal-line-tokens b l)))
              (when (or (< l line) (< (cadr o) (cadr tk)))
                (cond ((member (car o) *pascal-closers* :test #'string=) (setq depth (+ depth 1)))
                      ((member (car o) *pascal-openers* :test #'string=)
                       (if (= depth 0)
                           (let ((acc nil))
                             (dotimes (i (length (car o))) (push (+ (buf-offset b l (cadr o)) i) acc))
                             (dotimes (i (length (car tk))) (push (+ (buf-offset b line (cadr tk)) i) acc))
                             (return-from pascal-match-offsets acc))
                           (setq depth (- depth 1)))))))
            (setq l (- l 1))))))))

(setf (get 'pascal-mode 'match-offsets) #'pascal-match-offsets)

(defcmd blink-matching-construct (ed arg)
  (setq *pascal-blink-matching* (not *pascal-blink-matching*))
  (ed-message* ed "Blink Matching Construct ~:[off~;on~]" *pascal-blink-matching*))

;;; ---- compiling -------------------------------------------------------------------------------------------------

(defun %pascal-set-warning-possibilities (ed)
  (let ((ws (pascal-compiler-warnings)))
    (setf (ed-possibilities ed)
          (make-possibilities "compiler warnings"
                              (mapcar (lambda (w)
                                        (make-possibility (format nil "~A:~D: ~A" (path-filename (princ-to-string (pwarning-source w))) (pwarning-line w) (pwarning-message w))
                                                          (lambda (e) (%pascal-visit-warning e w))))
                                      ws)))
    ws))

(defun %pascal-visit-warning (ed w)
  (let* ((src (pwarning-source w))
         (b (or (find src (ed-buffers ed) :key #'buffer-file :test #'equal)
                (find src (ed-buffers ed) :key #'buffer-name :test #'equal)
                (and (stringp src) (probe-file src) (ed-visit-file ed src)))))
    (unless b (error "The source ~A of the warning is not available" src))
    (ed-switch-to-buffer ed b)
    (when (pwarning-start w) (%pascal-goto b (pwarning-start w)))))

(defun %pascal-compile-in-editor (ed only-ats)
  "Compile the units or routines of the current buffer at the offsets ONLY-ATS (NIL: the whole buffer)."
  (let* ((b (ed-buffer ed)) (text (buffer-string b)) (file (buffer-file b)) (names nil) (errors nil))
    (dolist (at (or only-ats (list nil)))
      (multiple-value-bind (n e)
          (pascal-compile-string text :package (%pascal-buffer-package b) :dialect (%pascal-buffer-dialect b) :file file
                                      :directory (if file (path-directory file) "") :only-at at :warnings-key (or file (buffer-name b)))
        (setq names (append names n) errors (append errors e))))
    (%pascal-record-warnings (or file (buffer-name b)) errors)
    (cond (errors
           (%pascal-set-warning-possibilities ed)
           (ed-message* ed "~D compiler warning~:P; line ~D: ~A  (C-. visits them)" (length errors) (pascal-error-line (car errors)) (pascal-error-message (car errors))))
          ((null names) (ed-message* ed "Nothing to compile"))
          (t (ed-message* ed "~{~A~^, ~} compiled" (mapcar #'%pascal-describe-name names))))
    (values names errors)))

(defun %pascal-describe-name (sym)
  (let* ((n (symbol-name sym)) (p (search "<-" n)) (pkg (package-name (symbol-package sym))))
    (cond (p (format nil "~(~A~) (in ~(~A~))" (subseq n 0 p) (subseq n (+ p 2))))
          ((gethash (%string-concat pkg ":" n) *pascal-programs*) (format nil "program ~(~A~)" n))
          (t (format nil "unit ~(~A~)" n)))))

(defun %pascal-top-items (b)
  "The compilation units of B and the top-level routines of programs and unit implementations, as (start end kind)."
  (let ((acc nil))
    (dolist (u (pascal-buffer-parse b) (nreverse acc))
      (push (list (pnode-start u) (pnode-end u) (pnode-kind u)) acc)
      (let ((decls (case (pnode-kind u) (:program (car (pnode-args (cadddr (pnode-args u))))) (:unit-impl (caddr (pnode-args u))) (t nil))))
        (dolist (r (getf decls :routines)) (push (list (pnode-start r) (pnode-end r) :routine) acc))))))

(defcmd compile-region (ed arg)
  ;; c-sh-C: the region's units and routines, or else the routine (or unit) at point
  (with-b (b ed)
    (multiple-value-bind (l1 c1 l2 c2) (region-bounds b)
      (cond
        ((not (%pascal-buffer-p b)) (run-editor-command ed (if l1 'eval-region 'eval-defun) arg))
        (l1
          (let* ((s (buf-offset b l1 c1)) (e (buf-offset b l2 c2)) (items (%pascal-top-items b))
                 (units (remove-if-not (lambda (i) (and (not (eq (caddr i) :routine)) (<= s (car i)) (< (car i) e))) items))
                 (routines (remove-if-not (lambda (i) (and (eq (caddr i) :routine) (<= s (car i)) (< (car i) e)
                                                           (notany (lambda (u) (and (<= (car u) (car i)) (<= (cadr i) (cadr u)))) units)))
                                          items)))
            (setf (buffer-mark b) nil)
            (%pascal-compile-in-editor ed (or (mapcar #'car (append units routines)) (list (point-offset b))))))
        (t (%pascal-compile-in-editor ed (list (point-offset b))))))))

(defcmd compile-buffer (ed arg)
  (with-b (b ed)
    (if (%pascal-buffer-p b)
        (if arg
            (%pascal-compile-in-editor ed (mapcar #'car (remove-if (lambda (i) (or (eq (caddr i) :routine) (< (car i) (point-offset b)))) (%pascal-top-items b))))
            (%pascal-compile-in-editor ed nil))
        (run-editor-command ed 'eval-buffer arg))))

(defcmd compile-file (ed arg)
  (with-b (b ed)
    (unless (buffer-file b) (error "The buffer is not visiting a file"))
    (when (buffer-modified b) (ed-save-buffer ed b))
    (if (%pascal-buffer-p b)
        (handler-case (ed-message* ed "Wrote ~A" (pascal-compile-file (buffer-file b)))
          (error (e) (%pascal-set-warning-possibilities ed) (ed-message* ed "~A" (car (%split-string (princ-to-string e) #\Newline)))))
        (ed-message* ed "Wrote ~A" (compile-file (buffer-file b))))))

(defcmd compiler-warnings (ed arg)
  (let ((ws (pascal-compiler-warnings)))
    (%show-text-buffer ed "*Compiler-Warnings-1*"
                       (if ws (format nil "~{~A~%~}" (mapcar (lambda (w) (format nil "~A: line ~D, column ~D: ~A" (pwarning-source w) (pwarning-line w) (pwarning-col w) (pwarning-message w))) ws))
                           "No compiler warnings."))))

(defcmd edit-compiler-warnings (ed arg)
  (let ((ws (%pascal-set-warning-possibilities ed)))
    (if ws (ed-goto-possibility ed 0) (ed-message* ed "No compiler warnings"))))

(defun %pascal-program-at (b)
  "The name of the main program around point, or of the nearest one before it, or of the first in the buffer."
  (let* ((off (point-offset b)) (progs (remove-if-not (lambda (u) (eq (pnode-kind u) :program)) (pascal-buffer-parse b))))
    (when (null progs) (error "There is no main program in this buffer"))
    (car (pnode-args (or (find-if (lambda (u) (and (<= (pnode-start u) off) (<= off (pnode-end u)))) progs)
                         (car (last (remove-if-not (lambda (u) (<= (pnode-end u) off)) progs)))
                         (car progs))))))

(defun %pascal-find-listener (ed)
  (let ((wm (window-wm ed)))
    (and wm (find-if (lambda (w) (and (typep w 'listener-window) (not (typep w 'report-window)))) (wm-windows wm)))))

(defun pascal-run-from-editor (ed name package)
  "Run the program NAME in the Listener (where it can read input), or with output to a buffer when there is none."
  (let ((sym (intern name package)) (lw (%pascal-find-listener ed)))
    (if lw
        (progn
          (wm-raise (window-wm ed) lw)
          (%in-listener lw (lambda ()
                             (format t "~&; Executing Pascal program ~(~A~)~%" name)
                             (pascal-execute sym)))
          (setf (wm-dirty (window-wm ed)) t))
        (let ((out (make-string-output-stream)))
          (let ((*standard-output* out)) (pascal-execute sym :input (make-string-input-stream "")))
          (%show-text-buffer ed "*Pascal Output*" (get-output-stream-string out))))))

(defcmd execute-pascal-program (ed arg)
  (with-b (b ed)
    (let ((name (%pascal-program-at b)) (pkg (%pascal-buffer-package b)))
      (unless (gethash (%string-concat pkg ":" name) *pascal-programs*)
        (error "The program ~(~A~) has not been compiled (C-c C-e compiles and executes it)" name))
      (pascal-run-from-editor ed name pkg))))

(defcmd compile-and-execute-pascal-program (ed arg)
  (with-b (b ed)
    (let* ((name (%pascal-program-at b)) (pkg (%pascal-buffer-package b))
           (unit (find-if (lambda (u) (and (eq (pnode-kind u) :program) (string= (car (pnode-args u)) name))) (pascal-buffer-parse b))))
      (multiple-value-bind (names errors) (%pascal-compile-in-editor ed (list (pnode-start unit)))
        (declare (ignore names))
        (unless errors (pascal-run-from-editor ed name pkg))))))

;;; ---- attribute list commands -----------------------------------------------------------------------------------

(defcmd set-dialect (ed arg)
  (with-b (b ed)
    (ed-read-string ed (format nil "Dialect (iso or vs) [~(~A~)]: " (%pascal-buffer-dialect b))
                    (lambda (s)
                      (let ((d (cond ((string-equal (string-trim " " s) "vs") :vs) ((string-equal (string-trim " " s) "iso") :iso)
                                     ((string= (string-trim " " s) "") (%pascal-buffer-dialect b))
                                     (t (error "The dialect is ISO or VS")))))
                        (%pascal-set-attributes b :dialect d)
                        (ed-message* ed "Dialect ~A" (if (eq d :vs) "Pascal/VS" "ISO Pascal")))))))

(defcmd set-package (ed arg)
  (with-b (b ed)
    (unless (%pascal-buffer-p b) (error "Set Package sets the package of a Pascal buffer"))
    (ed-read-string ed (format nil "Package [~A]: " (%pascal-buffer-package b))
                    (lambda (s)
                      (let ((name (string-upcase (string-trim " " s))))
                        (when (string= name "") (setq name (%pascal-buffer-package b)))
                        (unless (find-package name) (package-declare-with-no-superiors name))
                        (%pascal-set-attributes b :package name)
                        (ed-message* ed "Package ~A" name))))))

(defcmd update-attribute-list (ed arg)
  (with-b (b ed)
    (if (%pascal-buffer-p b)
        (ed-message* ed "~A" (%pascal-set-attributes b))
        (error "Update Attribute List works in Pascal buffers"))))

(defcmd reparse-attribute-list (ed arg)
  (with-b (b ed)
    (setf (buf-local b 'pascal-parse) nil)
    (ed-message* ed "Mode ~A, dialect ~A, package ~A" (mode-display-name (buffer-mode b))
                 (if (eq (%pascal-buffer-dialect b) :vs) "Pascal/VS" "ISO Pascal") (%pascal-buffer-package b))))

;;; ---- units ------------------------------------------------------------------------------------------------------

(defun pascal-units-depended-on (name &optional (package "PASCAL-USER"))
  "The units the program or unit NAME uses, directly or not, as upper-case names."
  (let ((seen nil))
    (labels ((uses-of (n)
               (let ((p (gethash (%string-concat package ":" n) *pascal-programs*)) (u (gethash (%string-concat package ":" n) *pascal-unit-info*)))
                 (cond (p (pprogram-uses p)) (u (getf u :uses)) (t nil))))
             (walk (n) (dolist (d (uses-of n)) (unless (member d seen :test #'string=) (push d seen) (walk d)))))
      (walk (string-upcase name)))
    (nreverse seen)))

(defun pascal-dependent-units (name &optional (package "PASCAL-USER"))
  "The programs and units that use the unit NAME, directly or not."
  (let ((all nil) (target (string-upcase name)))
    (maphash (lambda (k p) (declare (ignore k)) (when (string= (pprogram-package p) package) (push (pprogram-name p) all))) *pascal-programs*)
    (maphash (lambda (k u) (declare (ignore u)) (let ((c (%string-position-char k #\: 0))) (when (string= (subseq k 0 c) package) (push (subseq k (+ c 1)) all)))) *pascal-unit-info*)
    (remove-if-not (lambda (n) (member target (pascal-units-depended-on n package) :test #'string=)) (remove-duplicates all :test #'string=))))

(defcmd show-units-depended-on (ed arg)
  (let ((pkg (%pascal-buffer-package (ed-buffer ed))))
    (ed-read-string ed "Units depended on by program or unit: "
                    (lambda (s) (ed-message* ed "~A uses: ~{~(~A~)~^, ~}" s (or (pascal-units-depended-on (string-trim " " s) pkg) '("no units")))))))

(defcmd show-dependent-units (ed arg)
  (let ((pkg (%pascal-buffer-package (ed-buffer ed))))
    (ed-read-string ed "Units and programs depending on unit: "
                    (lambda (s) (ed-message* ed "Using ~A: ~{~(~A~)~^, ~}" s (or (pascal-dependent-units (string-trim " " s) pkg) '("nothing")))))))

;;; ---- keys ------------------------------------------------------------------------------------------------------

(dolist (k '(("<tab>" pascal-indent-command) ("C-j" pascal-newline-and-indent) ("<return>" pascal-newline)
             ("C-c C-c" compile-region) ("C-c C-b" compile-buffer) ("C-c C-e" compile-and-execute-pascal-program)
             ("C-c C-x" execute-pascal-program) ("C-c C-w" edit-compiler-warnings)
             ("C-M-a" pascal-beginning-of-definition) ("C-M-e" pascal-end-of-definition) ("C-M-h" pascal-mark-definition)
             ("C-M-f" pascal-forward-statement) ("C-M-b" pascal-backward-statement)
             ("C-M-k" pascal-kill-statement) ("C-M-<backspace>" pascal-backward-kill-statement) ("C-c k" pascal-kill-statement-around)
             ("C-c f" pascal-forward-expression) ("C-c b" pascal-backward-expression)
             ("C-c x" pascal-kill-expression) ("C-c X" pascal-backward-kill-expression)
             ("C-c t" pascal-kill-token) ("C-c T" pascal-backward-kill-token)
             ("C-c C-n" pascal-next-syntax-error) ("C-c C-p" pascal-previous-syntax-error)
             ("C-c C-t" pascal-insert-template) ("C-c ?" pascal-construct-help) ("M-<tab>" pascal-complete)
             ("C-c /" pascal-list-predeclared-completions) ("C-c ]" pascal-close-construct)
             ("C-M-n" pascal-next-template-item) ("C-M-p" pascal-previous-template-item)))
  (define-key *pascal-mode-map* (car k) (cadr k)))
