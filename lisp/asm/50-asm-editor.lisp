;;;; 50-asm-editor.lisp -- ASM mode: the editor mode for DEFASM bytecode assembly source.
;;;;
;;;; ASM mode is a line-oriented mode in the spirit of an assembler's: labels stand in the label field, instructions
;;;; are indented under them, and trailing comments line up in a column.  Inside a DEFASM form
;;;;     (defasm fact (n)
;;;;       "Factorial."                  directives, documentation, labels: column 2
;;;;       (:var acc)                    instructions: column 4
;;;;       top
;;;;         (lref n) ...                comments line up at *ASM-COMMENT-COLUMN*
;;;; Compiling assembles the routine under the cursor (or the region, or the buffer) into the running world; the
;;;; assembler's diagnostics point at the offending instruction.  Files ending in .ngasm open in this mode.
;;;;
;;;; Keys:
;;;;   TAB      format the line: indent it by its role, tidy its spacing, align its comment
;;;;   RET C-j  end the line and open a correctly indented one
;;;;   M-;      align the line's comment, or start one      C-c C-f  format the region (the buffer without a mark)
;;;;   C-M-q    format the routine at point                 M-<tab>  complete an instruction name
;;;;   C-c ?    describe the instruction at point (operands, stack effect)
;;;;   C-c C-c  compile the region or routine at point      C-c C-b  compile the buffer        C-M-x  compile the routine
;;;;   C-c C-w  edit the assembler's diagnostics (then C-.) C-c C-a  show the assembly listing of the compiled routine
;;;;   C-c C-x  call the routine at point, asking for the arguments
;;;;   M-.      go to the routine or label named at point   C-M-a C-M-e  routine start/end; C-M-f C-M-b etc. move over forms

(in-package :cl)

(defvar *asm-mode-map* (make-keymap "asm"))

(setf (get 'asm-mode 'mode-keymap) *asm-mode-map*
      (get 'asm-mode 'mode-name) "Assembly")

(dolist (suffix '(".ngasm"))
  (unless (assoc suffix *auto-mode-alist* :test #'string=) (push (cons suffix 'asm-mode) *auto-mode-alist*)))

(defun %asm-buffer-p (b) (eq (buffer-mode b) 'asm-mode))

;;; ---- colouring ---------------------------------------------------------------------------------------------------------

(defun %asm-digit-p (c) (and (char<= #\0 c) (char<= c #\9)))

(defun %asm-number-token-p (tok)
  (let ((n (length tok)))
    (and (> n 0)
         (or (%asm-digit-p (char tok 0))
             (and (> n 1) (member (char tok 0) '(#\+ #\-)) (%asm-digit-p (char tok 1)))))))

(defun asm-line-syntax (line state)
  "Syntax spans of one line of assembly source: (values spans end-state).  A span is (start end class): :COMMENT, :STRING,
:BUILTIN (instructions and DEFASM), :KEYWORD (directives), :DEFNAME (routine names and labels), :CONSTANT (numbers, T, NIL).
STATE is NIL or :STRING (inside a string that began on an earlier line)."
  (let ((i 0) (n (length line)) (spans nil) (start 0) (after-open nil) (expect-name nil) (first-token t))
    (loop
      (when (>= i n) (return))
      (cond
        ((eq state :string)
         (let ((j i))
           (loop (cond ((>= j n) (return))
                       ((char= (char line j) #\\) (setq j (+ j 2)))
                       ((char= (char line j) #\") (setq j (+ j 1) state nil) (return))
                       (t (setq j (+ j 1)))))
           (setq j (min j n))
           (setq spans (cons (list start j :string) spans) i j first-token nil)))
        (t
         (let ((c (char line i)))
           (cond ((char= c #\;) (setq spans (cons (list i n :comment) spans) i n))
                 ((char= c #\") (setq state :string start i i (+ i 1) first-token nil after-open nil))
                 ((and (char= c #\#) (< (+ i 2) n) (char= (char line (+ i 1)) #\\))
                  (setq spans (cons (list i (+ i 3) :constant) spans) i (+ i 3) first-token nil after-open nil))
                 ((char= c #\() (setq after-open t first-token nil i (+ i 1)))
                 ((%syntax-delimiter-p c) (when (char= c #\)) (setq after-open nil expect-name nil)) (setq i (+ i 1)))
                 (t (let ((j i))
                      (loop (when (or (>= j n) (%syntax-delimiter-p (char line j))) (return)) (setq j (+ j 1)))
                      (let* ((tok (subseq line i j)) (up (string-upcase tok))
                             (class (cond ((char= (char tok 0) #\:) :keyword)
                                          ((char= (char tok 0) #\&) :keyword)
                                          ((or (string= up "T") (string= up "NIL")) :constant)
                                          ((%asm-number-token-p tok) :constant)
                                          (expect-name (setq expect-name nil) :defname)
                                          ((and after-open (string= up "DEFASM")) (setq expect-name t) :builtin)
                                          ((and after-open (gethash up *asm-mnemonics*)) :builtin)
                                          ((and first-token (not after-open)) :defname)
                                          (t nil))))
                        (when class (setq spans (cons (list i j class) spans)))
                        (setq i j after-open nil first-token nil)))))))))
    (values (nreverse spans) state)))

(setf (get 'asm-mode 'line-syntax) #'asm-line-syntax)

;;; ---- structure of the buffer ----------------------------------------------------------------------------------------------

(defun %asm-line-depths (b)
  "A vector with, for each line of B, (paren-depth . string-state) at the start of the line."
  (let* ((n (buf-nlines b)) (v (make-array n)) (depth 0) (state nil))
    (dotimes (l n)
      (setf (aref v l) (cons depth state))
      (let ((text (buf-line b l)))
        (multiple-value-bind (spans st) (asm-line-syntax text state)
          (let ((i 0) (len (length text)))
            (loop
              (when (>= i len) (return))
              (let ((sp (find-if (lambda (s) (and (<= (car s) i) (< i (cadr s)) (member (caddr s) '(:comment :string)))) spans))
                    (c (char text i)))
                (cond (sp (setq i (cadr sp)))
                      ((and (char= c #\#) (< (+ i 1) len) (char= (char text (+ i 1)) #\\)) (setq i (+ i 3)))
                      ((char= c #\() (setq depth (+ depth 1) i (+ i 1)))
                      ((char= c #\)) (setq depth (max 0 (- depth 1)) i (+ i 1)))
                      (t (setq i (+ i 1)))))))
          (setq state st))))
    v))

(defun %asm-first-nonblank (text)
  (let ((i 0) (n (length text))) (loop (when (or (>= i n) (not (member (char text i) '(#\Space #\Tab)))) (return i)) (setq i (+ i 1)))))

(defun %asm-target-column (b line depths)
  "The column LINE of B should start at, or NIL to leave it alone (inside a string)."
  (let* ((text (buf-line b line)) (ws (%asm-first-nonblank text)) (info (aref depths line)) (depth (car info)))
    (cond ((eq (cdr info) :string) nil)
          ((>= ws (length text)) (cond ((= depth 0) 0) ((= depth 1) *asm-instruction-indent*) (t (lisp-indent-for-line b line))))
          (t (let ((c (char text ws)))
               (cond ((char= c #\;)
                      (cond ((and (>= (length text) (+ ws 3)) (string= (subseq text ws (+ ws 3)) ";;;")) 0)
                            ((= depth 0) 0)
                            ((= depth 1) *asm-instruction-indent*)
                            (t (lisp-indent-for-line b line))))
                     ((= depth 0) 0)
                     ((= depth 1)
                      (cond ((char= c #\() (if (and (< (+ ws 1) (length text)) (char= (char text (+ ws 1)) #\:)) *asm-label-indent* *asm-instruction-indent*))
                            (t *asm-label-indent*)))
                     (t (lisp-indent-for-line b line))))))))

(defun %asm-normalize-code (code)
  "CODE with runs of blanks outside strings collapsed to one, and no blank after ( or before )."
  (let ((out (make-string-output-stream)) (in-string nil) (prev nil) (n (length code)) (i 0))
    (loop
      (when (>= i n) (return))
      (let ((c (char code i)))
        (cond (in-string
               (write-char c out)
               (cond ((char= c #\\) (when (< (+ i 1) n) (write-char (char code (+ i 1)) out)) (setq i (+ i 1)))
                     ((char= c #\") (setq in-string nil)))
               (setq prev c))
              ((char= c #\") (setq in-string t prev c) (write-char c out))
              ((member c '(#\Space #\Tab))
               (let ((j i)) (loop (when (or (>= j n) (not (member (char code j) '(#\Space #\Tab)))) (return)) (setq j (+ j 1)))
                 (unless (or (null prev) (char= prev #\() (and (< j n) (char= (char code j) #\))))
                   (write-char #\Space out) (setq prev #\Space))
                 (setq i (- j 1))))
              (t (write-char c out) (setq prev c))))
      (setq i (+ i 1)))
    (get-output-stream-string out)))

(defun %asm-split-comment (text state)
  "(values code comment) of TEXT: the code before a ; comment, trimmed on the right, and the comment (or NIL)."
  (let ((cs (find-if (lambda (s) (eq (caddr s) :comment)) (asm-line-syntax text state))))
    (if cs
        (values (string-right-trim '(#\Space #\Tab) (subseq text 0 (car cs))) (subseq text (car cs)))
        (values text nil))))

(defun %asm-set-line (b line new)
  (let ((old (buf-line b line)))
    (unless (string= old new)
      (buf-delete b line 0 line (length old))
      (buf-goto b line 0)
      (buf-insert b new))))

(defun %asm-format-line (b line depths)
  "Indent, tidy and comment-align LINE of B.  Returns the amount the text moved right at its start."
  (let* ((text (buf-line b line)) (state (cdr (aref depths line))) (col (%asm-target-column b line depths)))
    (if (null col)
        0
        (multiple-value-bind (code comment) (%asm-split-comment text state)
          (let* ((code (%asm-normalize-code (string-trim '(#\Space #\Tab) code)))
                 (head (make-string col :initial-element #\Space))
                 (new (cond ((and (string= code "") comment) (concatenate 'string head comment))
                            ((string= code "") "")
                            ((null comment) (concatenate 'string head code))
                            (t (let* ((end (+ col (length code)))
                                      (pad (if (< (+ end 1) *asm-comment-column*) (- *asm-comment-column* end) 2)))
                                 (concatenate 'string head code (make-string pad :initial-element #\Space) comment))))))
            (%asm-set-line b line new)
            (- col (%asm-first-nonblank text)))))))

(defun %asm-format-lines (b first last)
  "Format lines FIRST to LAST of B, keeping point where it was relative to the text."
  (let* ((pl (point-line b)) (pc (point-col b)) (depths (%asm-line-depths b)))
    (do ((l first (+ l 1))) ((> l last))
      ;; the depth vector goes stale when a line's length changes only its own text, never the parens, so it stays valid
      (let ((delta (%asm-format-line b l depths)))
        (when (= l pl) (setq pc (max 0 (+ pc delta))))))
    (buf-goto b pl pc)))

;;; ---- the mode and its editing commands --------------------------------------------------------------------------------

(defcmd asm-mode (ed arg)
  (set-buffer-mode (ed-buffer ed) 'asm-mode)
  (ed-message* ed "Assembly mode: C-c C-c compiles the routine, C-c ? describes an instruction"))

(defcmd asm-tab (ed arg)
  (with-b (b ed)
    (let* ((line (point-line b)) (old-ws (%asm-first-nonblank (buf-line b line))) (old-col (point-col b)))
      (%asm-format-lines b line line)
      (let ((ws (%asm-first-nonblank (buf-line b line))))
        (when (or (<= old-col old-ws) (< (point-col b) ws)) (buf-goto b line ws))))))

(defcmd asm-newline-and-indent (ed arg)
  (with-b (b ed)
    (let ((line (point-line b)))
      (%asm-format-lines b line line)
      (buf-goto b (point-line b) (length (buf-current-line b)))
      (buf-insert b (string #\Newline))
      (let* ((l (point-line b)) (depths (%asm-line-depths b)) (col (%asm-target-column b l depths)))
        (when (and col (> col 0)) (buf-insert b (make-string col :initial-element #\Space)))))))

(defcmd asm-align-comment (ed arg)
  (with-b (b ed)
    (let* ((line (point-line b)) (depths (%asm-line-depths b)) (text (buf-line b line)))
      (multiple-value-bind (code comment) (%asm-split-comment text (cdr (aref depths line)))
        (cond (comment (%asm-format-lines b line line)
                       (let ((new (buf-line b line))) (buf-goto b line (min (length new) (+ (or (position #\; new :from-end nil) 0) 2)))))
              (t (let* ((code (string-right-trim '(#\Space #\Tab) text))
                        (pad (max 2 (- *asm-comment-column* (length code)))))
                   (%asm-set-line b line (concatenate 'string code (make-string pad :initial-element #\Space) "; "))
                   (buf-goto b line (length (buf-line b line))))))
        code))))

(defcmd asm-format-region (ed arg)
  (with-b (b ed)
    (multiple-value-bind (l1 c1 l2 c2) (region-bounds b)
      (declare (ignore c1 c2))
      (if l1
          (progn (%asm-format-lines b l1 l2) (ed-message* ed "Formatted lines ~D to ~D" (+ l1 1) (+ l2 1)))
          (progn (%asm-format-lines b 0 (- (buf-nlines b) 1)) (ed-message* ed "Formatted the buffer"))))))

(defun %asm-routine-start (b)
  "The offset of the top-level form around point: the nearest line at or before point that starts a form at depth 0."
  (let ((depths (%asm-line-depths b)) (l (point-line b)))
    (loop
      (when (< l 0) (return 0))
      (let ((text (buf-line b l)))
        (when (and (= 0 (car (aref depths l))) (null (cdr (aref depths l))) (> (length text) 0) (char= (char text 0) #\())
          (return (buf-offset b l 0))))
      (setq l (- l 1)))))

(defcmd asm-beginning-of-routine (ed arg)
  (with-b (b ed) (push-mark ed) (goto-offset b (%asm-routine-start b))))

(defcmd asm-end-of-routine (ed arg)
  (with-b (b ed)
    (let ((e (sexp-end (buffer-string b) (%asm-routine-start b)))) (when e (goto-offset b e)))))

(defcmd asm-format-routine (ed arg)
  (with-b (b ed)
    (let* ((s (buffer-string b)) (st (%asm-routine-start b)) (e (sexp-end s st)))
      (if e
          (let ((l1 (car (buf-pos-of-offset b st))) (l2 (car (buf-pos-of-offset b e))))
            (%asm-format-lines b l1 l2)
            (ed-message* ed "Formatted the routine"))
          (ed-message* ed "No routine at point")))))

(defun %asm-word-at (b)
  (let* ((text (buf-current-line b)) (col (min (point-col b) (length text))) (s col) (e col))
    (loop (when (or (<= s 0) (%syntax-delimiter-p (char text (- s 1)))) (return)) (setq s (- s 1)))
    (loop (when (or (>= e (length text)) (%syntax-delimiter-p (char text e))) (return)) (setq e (+ e 1)))
    (and (> e s) (subseq text s e))))

(defcmd asm-describe-instruction (ed arg)
  (with-b (b ed)
    (let* ((w (%asm-word-at b)) (help (and w (asm-instruction-help w))))
      (if help
          (ed-message* ed "~A" help)
          (ed-read-string ed "Describe instruction: "
                          (lambda (s)
                            (let ((h (asm-instruction-help (string-trim " " s))))
                              (ed-message* ed "~A" (or h (format nil "~A is not an instruction" s)))))
                          :completer (lambda (s) (remove-if-not (lambda (m) (and (<= (length s) (length m)) (string-equal s (subseq m 0 (length s))))) (asm-mnemonics))))))))

(defcmd asm-complete-mnemonic (ed arg)
  (with-b (b ed)
    (let* ((text (buf-current-line b)) (col (min (point-col b) (length text))) (s col))
      (loop (when (or (<= s 0) (%syntax-delimiter-p (char text (- s 1)))) (return)) (setq s (- s 1)))
      (let* ((prefix (subseq text s col))
             (cands (remove-if-not (lambda (m) (and (<= (length prefix) (length m)) (string-equal prefix (subseq m 0 (length prefix))))) (asm-mnemonics))))
        (cond ((string= prefix "") (ed-message* ed "Type the start of an instruction name first"))
              ((null cands) (ed-message* ed "No instruction starts with ~A" prefix))
              (t (let ((common (car cands)))
                   (dolist (c (cdr cands))
                     (let ((k 0)) (loop (when (or (>= k (length common)) (>= k (length c)) (char/= (char common k) (char c k))) (return)) (setq k (+ k 1)))
                       (setq common (subseq common 0 k))))
                   (when (> (length common) (length prefix))
                     (buf-delete b (point-line b) s (point-line b) col)
                     (buf-goto b (point-line b) s)
                     (buf-insert b common))
                   (if (cdr cands)
                       (ed-message* ed "~{~A~^ ~}" (subseq cands 0 (min 24 (length cands))))
                       (ed-message* ed "~A" (asm-instruction-help (car cands)))))))))))

;;; ---- compiling ------------------------------------------------------------------------------------------------------------

(defvar *asm-diagnostics* nil "The assembler diagnostics from the editor: (source offset line kind message), kind :ERROR or :WARNING.")
(defvar *asm-error-offset* nil "While a routine is read from a buffer: the offset of the text being read.")

(defun %asm-form-ranges (s)
  "The top-level forms of the text S: a list of (start . end) offsets."
  (let ((i 0) (acc nil) (n (length s)))
    (loop
      (let ((st (%skip-ws s i)))
        (when (>= st n) (return))
        (let ((e (sexp-end s st)))
          (when (null e) (return))
          (setq acc (cons (cons st e) acc) i e))))
    (nreverse acc)))

(defun %asm-defasm-form-p (s start)
  (let ((n (length s)))
    (and (< (+ start 7) n) (char= (char s start) #\() (string-equal (subseq s (+ start 1) (+ start 7)) "defasm")
         (member (char s (+ start 7)) '(#\Space #\Tab #\Newline)))))

(defun %asm-read-defasm (s start end package)
  "Read the DEFASM form at START in S: (values name lambda-list items offsets), OFFSETS giving where each item begins."
  (let ((i (+ start 7)) (name nil) (ll nil) (items nil) (offsets nil))
    (flet ((next (what)
             (setq i (%skip-ws s i))
             (setq *asm-error-offset* i)
             (when (>= i end) (error "The defasm form ends before ~A" what))
             (multiple-value-bind (obj p ok) (%read-at s i package)
               (unless ok (error "Cannot read ~A here" what))
               (setq i p)
               obj)))
      (setq name (next "the routine name"))
      (setq ll (next "the lambda list"))
      (loop
        (setq i (%skip-ws s i))
        (when (or (>= i end) (char= (char s i) #\))) (return))
        (let ((at i))
          (let ((obj (next "an item")))
            (setq offsets (cons at offsets) items (cons obj items))))))
    (values name ll (nreverse items) (nreverse offsets))))

(defun %asm-note (b offset kind message)
  (let ((src (or (buffer-file b) (buffer-name b))))
    (setq *asm-diagnostics*
          (append *asm-diagnostics* (list (list src offset (+ 1 (car (buf-pos-of-offset b offset))) kind message (buffer-name b)))))))

(defun %asm-compile-ranges (ed ranges)
  "Assemble (and evaluate the other forms of) the RANGES of the current buffer.  Returns (values names error-count)."
  (let* ((b (ed-buffer ed)) (s (buffer-string b)) (src (or (buffer-file b) (buffer-name b))) (names nil) (errors 0) (pkg (buffer-package-at b (point-offset b))))
    (setq *asm-diagnostics* (remove src *asm-diagnostics* :key #'car :test #'equal))
    (dolist (r ranges)
      (let ((start (car r)) (end (cdr r)) (*asm-error-offset* nil))
        (handler-case
            (if (%asm-defasm-form-p s start)
                (multiple-value-bind (name ll items offsets) (%asm-read-defasm s start end pkg)
                  (handler-case
                      (multiple-value-bind (fn warns) (asm-assemble name ll items)
                        (%defun name fn)
                        (setq names (append names (list name)))
                        (dolist (w warns)
                          (%asm-note b (or (and (car w) (nth (car w) offsets)) start) :warning (cdr w)))
                        (when (buffer-file b) (ignore-errors (%note-definition name :function (buffer-file b) start))))
                    (asm-error (e)
                      (setq errors (+ errors 1))
                      (%asm-note b (or (and (asm-error-index e) (nth (asm-error-index e) offsets)) start) :error (asm-error-message e)))))
                (let ((*package* pkg)) (multiple-value-bind (form p ok) (%read-at s start pkg)
                                         (declare (ignore p))
                                         (when ok (eval form)))))
          (error (e)
            (setq errors (+ errors 1))
            (%asm-note b (or *asm-error-offset* start) :error (princ-to-string e))))))
    (values names errors)))

(defun %asm-visit-diagnostic (ed d)
  (let* ((name (nth 5 d)) (b (or (find (car d) (ed-buffers ed) :key #'buffer-file :test #'equal)
                                (find name (ed-buffers ed) :key #'buffer-name :test #'equal))))
    (unless b (error "The buffer of this diagnostic is no longer open"))
    (ed-switch-to-buffer ed b)
    (goto-offset b (min (cadr d) (length (buffer-string b))))))

(defun %asm-set-diagnostic-possibilities (ed)
  (let ((ds *asm-diagnostics*))
    (setf (ed-possibilities ed)
          (make-possibilities "assembler diagnostics"
                              (mapcar (lambda (d)
                                        (make-possibility (format nil "~A:~D: ~(~A~): ~A" (nth 5 d) (nth 2 d) (nth 3 d) (nth 4 d))
                                                          (lambda (e) (%asm-visit-diagnostic e d))))
                                      ds)))
    ds))

(defun %asm-compile-in-editor (ed ranges)
  (multiple-value-bind (names errors) (%asm-compile-ranges ed ranges)
    (let ((ds *asm-diagnostics*))
      (when ds (%asm-set-diagnostic-possibilities ed))
      (cond ((> errors 0)
             (let ((first (find :error ds :key #'cadddr)))
               (ed-message* ed "~D error~:P; line ~D: ~A  (C-. visits the diagnostics)" errors (if first (nth 2 first) 0) (if first (nth 4 first) "?"))))
            (names (ed-message* ed "~{~(~A~)~^, ~} assembled~:[~; (~D warning~:P; C-c C-w lists them)~]" names (and ds t) (length ds)))
            (t (ed-message* ed "Nothing to compile"))))
    (values names errors)))

(defmacro %asm-chain (name (ed arg) asm-form)
  "Define the editor command NAME to run ASM-FORM in ASM buffers and whatever it did before elsewhere."
  `(let ((%previous (get ',name '%ecommand)))
     (defcmd ,name (,ed ,arg)
       (if (%asm-buffer-p (ed-buffer ,ed))
           ,asm-form
           (if %previous (funcall %previous ,ed ,arg) (ed-message* ,ed "~A is not available in this buffer" ',name))))))

(defun %asm-ranges-at (b offset)
  "The range of the top-level form containing OFFSET, or the nearest one before it."
  (let* ((ranges (%asm-form-ranges (buffer-string b)))
         (r (or (find-if (lambda (r) (and (<= (car r) offset) (<= offset (cdr r)))) ranges)
                (car (last (remove-if-not (lambda (r) (<= (cdr r) offset)) ranges)))
                (car ranges))))
    (and r (list r))))

(%asm-chain compile-region (ed arg)
  (with-b (b ed)
    (multiple-value-bind (l1 c1 l2 c2) (region-bounds b)
      (if l1
          (let* ((s (buf-offset b l1 c1)) (e (buf-offset b l2 c2))
                 (ranges (remove-if-not (lambda (r) (and (<= s (cdr r)) (<= (car r) e))) (%asm-form-ranges (buffer-string b)))))
            (setf (buffer-mark b) nil)
            (%asm-compile-in-editor ed ranges))
          (%asm-compile-in-editor ed (%asm-ranges-at b (point-offset b)))))))

(%asm-chain compile-buffer (ed arg)
  (with-b (b ed) (%asm-compile-in-editor ed (%asm-form-ranges (buffer-string b)))))

(%asm-chain edit-compiler-warnings (ed arg)
  (let ((ds (%asm-set-diagnostic-possibilities ed)))
    (if ds (ed-goto-possibility ed 0) (ed-message* ed "No assembler diagnostics"))))

(defcmd asm-compile-routine (ed arg)
  (with-b (b ed) (%asm-compile-in-editor ed (%asm-ranges-at b (point-offset b)))))

;;; ---- the routine at point: listing, running, finding ---------------------------------------------------------------

(defun %asm-routine-name-at (b)
  "The name of the DEFASM routine around point (or the nearest one before it) as a symbol, or NIL."
  (let* ((s (buffer-string b)) (r (car (%asm-ranges-at b (point-offset b)))))
    (when (and r (%asm-defasm-form-p s (car r)))
      (multiple-value-bind (obj p ok) (%read-at s (%skip-ws s (+ (car r) 7)) (buffer-package-at b (car r)))
        (declare (ignore p))
        (and ok (symbolp obj) obj)))))

(defcmd asm-disassemble-routine (ed arg)
  (with-b (b ed)
    (let ((name (%asm-routine-name-at b)))
      (cond ((null name) (ed-message* ed "No routine here"))
            ((not (and (fboundp name) (ignore-errors (%code-parts (symbol-function name)))))
             (ed-message* ed "~(~A~) is not assembled yet: C-c C-c assembles it" name))
            (t (%show-text-buffer ed "*Assembly Listing*" (let ((*package* (or (symbol-package name) *package*))) (asm-listing name)) 'asm-mode))))))

(defcmd asm-run-routine (ed arg)
  (with-b (b ed)
    (let ((name (%asm-routine-name-at b)) (pkg (buffer-package-at b (point-offset b))))
      (cond ((null name) (ed-message* ed "No routine here"))
            ((not (fboundp name)) (ed-message* ed "~(~A~) is not assembled yet: C-c C-c assembles it" name))
            (t (ed-read-string ed (format nil "Arguments for ~(~A~) (Lisp forms): " name)
                               (lambda (s)
                                 (let ((*ed-package* pkg) (*package* pkg))
                                   (eval-string-in-editor ed (format nil "(~S ~A)" name s))))))))))

(defun %asm-find-label (b name)
  "The offset of the line defining the label NAME in the routine around point, or NIL."
  (let* ((s (buffer-string b)) (r (car (%asm-ranges-at b (point-offset b)))))
    (when r
      (let ((l1 (car (buf-pos-of-offset b (car r)))) (l2 (car (buf-pos-of-offset b (cdr r)))))
        (do ((l l1 (+ l 1))) ((> l l2) nil)
          (let* ((text (buf-line b l)) (ws (%asm-first-nonblank text)))
            (when (and (< ws (length text)) (not (member (char text ws) '(#\( #\; #\")))
                       (let ((e ws)) (loop (when (or (>= e (length text)) (%syntax-delimiter-p (char text e))) (return)) (setq e (+ e 1)))
                         (string-equal (subseq text ws e) name)))
              (return (buf-offset b l ws)))))))
    ))

(defcmd asm-edit-definition (ed arg)
  (with-b (b ed)
    (let ((word (%asm-word-at b)))
      (cond ((null word) (ed-message* ed "Point is not on a name"))
            ((%asm-find-label b word)
             (push-mark ed) (goto-offset b (%asm-find-label b word)) (ed-message* ed "Label ~A" word))
            (t (let* ((s (buffer-string b))
                      (hit (find-if (lambda (r)
                                      (and (%asm-defasm-form-p s (car r))
                                           (multiple-value-bind (obj p ok) (%read-at s (%skip-ws s (+ (car r) 7)) (buffer-package-at b (car r)))
                                             (declare (ignore p))
                                             (and ok (symbolp obj) (string-equal (symbol-name obj) word)))))
                                    (%asm-form-ranges s))))
                 (if hit
                     (progn (push-mark ed) (goto-offset b (car hit)) (ed-message* ed "~A" word))
                     (run-editor-command ed 'find-definition arg))))))))

;;; ---- keys ---------------------------------------------------------------------------------------------------------------------

(dolist (k '(("<tab>" asm-tab) ("<return>" asm-newline-and-indent) ("C-j" asm-newline-and-indent)
             ("M-;" asm-align-comment) ("C-c C-f" asm-format-region) ("C-M-q" asm-format-routine)
             ("M-<tab>" asm-complete-mnemonic) ("C-c ?" asm-describe-instruction)
             ("C-c C-c" compile-region) ("C-c C-b" compile-buffer) ("C-M-x" asm-compile-routine)
             ("C-c C-w" edit-compiler-warnings) ("C-c C-a" asm-disassemble-routine) ("C-c C-x" asm-run-routine)
             ("M-." asm-edit-definition)
             ("C-M-f" forward-sexp) ("C-M-b" backward-sexp) ("C-M-u" backward-up-list) ("C-M-d" down-list)
             ("C-M-k" kill-sexp) ("C-M-a" asm-beginning-of-routine) ("C-M-e" asm-end-of-routine) ("C-M-n" up-list)))
  (define-key *asm-mode-map* (car k) (cadr k)))
