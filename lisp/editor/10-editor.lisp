;;;; 10-editor.lisp -- the editor window: keymaps, minibuffer, isearch, commands, display.
;;;;
;;;; A Zmacs/Emacs-style editor.  Keys are looked up in the mode keymap and then the global
;;;; keymap; commands are ordinary Lisp functions registered with DEFCMD and callable with M-x.
;;;; An editor window shows one or more VIEWs (C-x 2, C-x 3), each on a buffer with its own point.

(in-package :cl)

;;; ---- keymaps ------------------------------------------------------------------------------------

(defstruct (keymap (:constructor make-keymap (&optional name)) (:copier nil)) name (table nil))
(defvar *global-keymap* (make-keymap "global"))
(defvar *lisp-mode-map* (make-keymap "lisp"))
(defvar *text-mode-map* (make-keymap "text"))
(defvar *dired-mode-map* (make-keymap "dired"))
(defvar *possibilities-mode-map* (make-keymap "possibilities"))

(defun %binding (map key) (cdr (assoc key (keymap-table map) :test #'equal)))
(defun %set-binding (map key value)
  (let ((cell (assoc key (keymap-table map) :test #'equal)))
    (if cell (rplacd cell value) (setf (keymap-table map) (cons (cons key value) (keymap-table map))))))

(defun define-key (map seq command)
  (let ((keys (parse-key-sequence seq)) (m map))
    (loop
      (let ((k (car keys)))
        (if (null (cdr keys))
            (progn (%set-binding m k command) (return command))
            (let ((b (%binding m k)))
              (unless (keymap-p b) (setq b (make-keymap)) (%set-binding m k b))
              (setq m b)
              (setq keys (cdr keys))))))))

(defun lookup-key (maps keys)
  "Returns a command symbol, :PARTIAL (a prefix) or NIL."
  (let ((partial nil))
    (dolist (m maps)
      (let ((cur m) (ks keys) (res nil))
        (loop
          (let ((b (%binding cur (car ks))))
            (cond ((null b) (return))
                  ((keymap-p b) (if (cdr ks) (setq cur b ks (cdr ks)) (progn (setq res :partial) (return))))
                  (t (if (cdr ks) (return) (progn (setq res b) (return)))))))
        (cond ((eq res :partial) (setq partial t))
              (res (return-from lookup-key res)))))
    (if partial :partial nil)))

;;; ---- commands -------------------------------------------------------------------------------------

(defvar *editor-commands* nil)
(defmacro defcmd (name (ed arg) &body body)
  `(progn (%put ',name '%ecommand (lambda (,ed ,arg) (declare (ignorable ,ed ,arg)) ,@body))
          (unless (member ',name *editor-commands*) (setq *editor-commands* (cons ',name *editor-commands*)))
          ',name))
(defun bind (seq command) (define-key *global-keymap* seq command))
(defun bind-lisp (seq command) (define-key *lisp-mode-map* seq command))

;;; ---- the window -------------------------------------------------------------------------------------

(defstruct (minibuf (:conc-name mb-) (:constructor make-minibuf) (:copier nil)) prompt (input "") (pos 0) callback completer)
(defstruct (isearch (:conc-name is-) (:constructor make-isearch) (:copier nil))
  forward (string "") start-line start-col anchor-line anchor-col (failed nil) match regexp (error nil))

;;; A view shows a buffer.  The selected view uses its buffer's point; the others remember their own (LINE, COL).
(defstruct (view (:constructor %make-view) (:copier nil))
  buffer (top 0) (hscroll 0) (height 10) (line 0) (col 0) (rx 0) (ry 0) (rw 0) (rh 0))

(defclass editor-window (window)
  ((buffers :initform nil :accessor ed-buffers)
   (layout :initform nil :accessor ed-layout)        ; a view, or (:below|:right first second)
   (selected :initform nil :accessor ed-selected)
   (pending :initform nil :accessor ed-pending)
   (prefix :initform nil :accessor ed-prefix)
   (last-command :initform nil :accessor ed-last-command)
   (message :initform nil :accessor ed-message)
   (minibuf :initform nil :accessor ed-minibuf)
   (isearch :initform nil :accessor ed-isearch)
   (mark-ring :initform nil :accessor ed-mark-ring)
   (insert-count :initform 0 :accessor ed-insert-count)
   (last-search :initform "" :accessor ed-last-search)
   (possibilities :initform nil :accessor ed-possibilities)
   (hint :initform nil :accessor ed-hint)            ; (text start end): the arglist shown while typing in Lisp mode
   (query :initform nil :accessor ed-query)          ; query-replace in progress
   (recording :initform nil :accessor ed-recording)  ; :NONE or the keys of the keyboard macro being defined, newest first
   (macro :initform nil :accessor ed-macro)          ; the last keyboard macro, a list of keys
   (macro-repeat :initform nil :accessor ed-macro-repeat)
   (command-keys :initform nil :accessor ed-command-keys)))

(defmethod initialize-instance :after ((w editor-window) &rest args)
  (declare (ignore args))
  (let ((v (%make-view)))
    (setf (ed-layout w) v (ed-selected w) v))
  (ed-switch-to-buffer w (ed-get-buffer w "*scratch*" t)))

(defun ed-buffer (w) (let ((v (ed-selected w))) (and v (view-buffer v))))
(defun (setf ed-buffer) (b w) (setf (view-buffer (ed-selected w)) b))
(defun ed-top (w) (view-top (ed-selected w)))
(defun (setf ed-top) (n w) (setf (view-top (ed-selected w)) n))
(defun ed-hscroll (w) (view-hscroll (ed-selected w)))
(defun (setf ed-hscroll) (n w) (setf (view-hscroll (ed-selected w)) n))
(defun ed-view-h (w) (view-height (ed-selected w)))

(defun %layout-views (node) (if (view-p node) (list node) (append (%layout-views (cadr node)) (%layout-views (caddr node)))))
(defun ed-views (w) "The views of editor W, top to bottom and left to right." (%layout-views (ed-layout w)))

(defun ed-select-view (w v)
  "Make V the selected view of W: the current view remembers its point, and V's point becomes its buffer's."
  (let ((old (ed-selected w)))
    (unless (eq old v)
      (let ((b (view-buffer old))) (setf (view-line old) (point-line b) (view-col old) (point-col b)))
      (setf (ed-selected w) v)
      (buf-goto (view-buffer v) (view-line v) (view-col v)))
    v))

(defun %replace-node (tree old new)
  (cond ((eq tree old) new)
        ((view-p tree) tree)
        (t (list (car tree) (%replace-node (cadr tree) old new) (%replace-node (caddr tree) old new)))))

(defun %parent-of (tree v)
  (and (consp tree) (if (or (eq (cadr tree) v) (eq (caddr tree) v)) tree (or (%parent-of (cadr tree) v) (%parent-of (caddr tree) v)))))

(defun ed-split-view (w dir)
  "Split the selected view in two (DIR :BELOW or :RIGHT); the new view shows the same buffer.  Returns it."
  (let* ((v (ed-selected w)) (b (view-buffer v))
         (new (%make-view :buffer b :top (view-top v) :line (point-line b) :col (point-col b))))
    (setf (ed-layout w) (%replace-node (ed-layout w) v (list dir v new)))
    new))

(defun ed-delete-view (w v)
  "Remove view V from W (unless it is the only one); returns T if it was removed."
  (let ((parent (%parent-of (ed-layout w) v)))
    (when parent
      (let ((sibling (if (eq (cadr parent) v) (caddr parent) (cadr parent))))
        (when (eq (ed-selected w) v) (ed-select-view w (car (%layout-views sibling))))
        (setf (ed-layout w) (%replace-node (ed-layout w) parent sibling))
        t))))

(defvar *ed-package* nil)
(defvar *typed-char* 32)
(defun ed-message* (w fmt &rest args) (setf (ed-message w) (apply #'format nil fmt args)))

(defun ed-get-buffer (w name &optional create)
  (or (find name (ed-buffers w) :key #'buffer-name :test #'string=)
      (and create
           (let ((b (make-buffer name)))
             (setf (ed-buffers w) (append (ed-buffers w) (list b)))
             (when (string= name "*scratch*")
               (setf (buffer-mode b) 'lisp-mode)
               (buffer-set-text b ";; This buffer is for Lisp evaluation (C-x C-e, C-M-x).")
               (buf-goto b 0 (length (buf-line b 0))) (buf-insert b (format nil "~%~%")))
             b))))

(defun ed-switch-to-buffer (w b)
  (unless (member b (ed-buffers w)) (setf (ed-buffers w) (append (ed-buffers w) (list b))))
  (setf (ed-buffer w) b (ed-top w) 0 (ed-hscroll w) 0 (ed-pending w) nil)
  b)

(defun ed-forget-buffer (w b replacement)
  "Show REPLACEMENT in every view of W that shows B."
  (dolist (v (ed-views w))
    (when (eq (view-buffer v) b)
      (setf (view-buffer v) replacement (view-top v) 0 (view-line v) (point-line replacement) (view-col v) (point-col replacement)))))

(defun ed-buffer-names (w) (mapcar #'buffer-name (ed-buffers w)))

;;; Other major modes (the Pascal Tool Kit's, for one) describe themselves on their symbol's property list:
;;; MODE-KEYMAP, MODE-NAME, LINE-SYNTAX (a function like LISP-LINE-SYNTAX), MATCH-OFFSETS (offsets to highlight,
;;; like %PAREN-MATCH-OFFSETS), BEFORE-SELF-INSERT (called with the editor and the character code; true means the
;;; character was handled) and AFTER-SELF-INSERT.
(defun mode-keymap (mode)
  (case mode (lisp-mode *lisp-mode-map*) (text-mode *text-mode-map*) (dired-mode *dired-mode-map*) (possibilities-mode *possibilities-mode-map*)
    (t (and mode (symbolp mode) (get mode 'mode-keymap)))))
(defun mode-display-name (mode)
  (case mode (lisp-mode "Lisp") (text-mode "Text") (dired-mode "Dired") (diff-mode "Diff") (possibilities-mode "Possibilities")
    (t (or (and mode (symbolp mode) (get mode 'mode-name)) "Fundamental"))))

;;; ---- display ----------------------------------------------------------------------------------------------

(defun %line-cells (line)
  "The screen cells of LINE: (values codes bytes).  CODES holds a code per column: UTF-8 decoded, tabs
expanded, control characters as ^X, wide characters followed by +WIDE-CONT+, combining marks merged into the
character before them.  BYTES holds the byte offset in LINE of the character shown in each column."
  (let ((cells (make-array (+ 8 (length line)) :adjustable t :fill-pointer 0))
        (bytes (make-array (+ 8 (length line)) :adjustable t :fill-pointer 0))
        (i 0) (n (length line)))
    (flet ((put (code at) (vector-push-extend code cells) (vector-push-extend at bytes)))
      (loop
        (when (>= i n) (return))
        (let ((b (char-code (char line i))))
          (cond ((= b 9) (dotimes (k (- 8 (mod (length cells) 8))) (put 32 i)) (setq i (+ i 1)))
                ((< b 32) (put 94 i) (put (+ b 64) i) (setq i (+ i 1)))
                ((< b 128) (put b i) (setq i (+ i 1)))
                (t (multiple-value-bind (code next) (%utf8-decode line i)
                     (let ((w (char-display-width code)) (k (fill-pointer cells)))
                       (cond ((and (= w 0) (> k 0))
                              (let ((j (if (= (aref cells (- k 1)) +wide-cont+) (- k 2) (- k 1))))
                                (setf (aref cells j) (screen-cluster (%string-concat (%cell-string (aref cells j)) (utf8-from-codes (list code)))))))
                             ((= w 2) (put code i) (put +wide-cont+ i))
                             (t (put code i))))
                     (setq i next)))))))
    (values cells bytes)))

(defun ed-ensure-visible (v th line nlines)
  (cond ((< line (view-top v)) (setf (view-top v) (if (> (- (view-top v) line) th) (max 0 (- line (floor th 2))) line)))
        ((>= line (+ (view-top v) th)) (setf (view-top v) (if (> (- line (+ (view-top v) th)) th) (max 0 (- line (floor th 2))) (+ (- line th) 1)))))
  (setf (view-top v) (max 0 (min (view-top v) (max 0 (- nlines 1))))))

(defun %paren-match-offsets (b)
  "Offsets of the two parentheses to highlight (or NIL)."
  (let ((before (buf-char-before b)) (after (buf-char-after b)))
    (cond ((and before (char= before #\)))
           (let* ((s (buffer-string b)) (off (point-offset b)) (st (sexp-start-before s off)))
             (and st (char= (char s st) #\() (list st (- off 1)))))
          ((and after (char= after #\())
           (let* ((s (buffer-string b)) (off (point-offset b)) (e (sexp-end s off)))
             (and e (list off (- e 1)))))
          (t nil))))

(defun %pos-in-region-p (b l c)
  (multiple-value-bind (l1 c1 l2 c2) (region-bounds b)
    (and l1 (not (%pos< l c l1 c1)) (%pos< l c l2 c2))))

;;; ---- syntax colouring -------------------------------------------------------------------------------------
;;; Lisp mode colours comments, strings, character literals, keywords and constants, the heads of special forms
;;; and macros, and the names being defined.  The state at the start of each line (inside a string or a #| |#
;;; comment) is cached per buffer and recomputed from the first line changed (see %NOTE-CHANGE).

(defvar *lisp-builtins* (make-hash-table :test 'equal))
(dolist (n '("lambda" "let" "let*" "flet" "labels" "macrolet" "symbol-macrolet" "if" "when" "unless" "cond" "case"
             "ecase" "ccase" "typecase" "etypecase" "and" "or" "progn" "prog1" "prog2" "block" "return" "return-from"
             "tagbody" "go" "catch" "throw" "unwind-protect" "loop" "do" "do*" "dolist" "dotimes" "handler-case"
             "handler-bind" "restart-case" "restart-bind" "ignore-errors" "multiple-value-bind" "multiple-value-prog1"
             "multiple-value-call" "destructuring-bind" "setq" "setf" "psetq" "psetf" "incf" "decf" "push" "pop"
             "function" "quote" "declare" "eval-when" "in-package" "the" "locally" "load-time-value" "assert"
             "check-type" "error" "cerror" "warn" "signal" "with-slots" "with-accessors" "with-open-file"
             "with-output-to-string" "with-input-from-string" "with-simple-restart" "with-lock-held" "without-interrupts"))
  (setf (gethash n *lisp-builtins*) t))

(defun %definer-name-p (tok)
  (and (> (length tok) 3) (or (string= (subseq tok 0 3) "def") (and (> (length tok) 7) (string= (subseq tok 0 7) "define-")))))

(defun %syntax-delimiter-p (c) (member c '(#\Space #\Tab #\( #\) #\" #\; #\' #\` #\,)))

(defun lisp-line-syntax (line state)
  "Syntax spans of one line of Lisp: (values spans end-state).  A span is (start end class), CLASS one of
:COMMENT :STRING :CONSTANT :KEYWORD :BUILTIN :DEFNAME.  STATE is the state at the start of the line: NIL,
:STRING, or (:BLOCK . depth) inside #| |#."
  (let ((i 0) (n (length line)) (spans nil) (expect-name nil) (after-open nil) (start 0))
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
           (setq spans (cons (list start j :string) spans) i j start j)))
        ((consp state)
         (let ((j i) (depth (cdr state)))
           (loop (cond ((>= j n) (return))
                       ((and (char= (char line j) #\|) (< (+ j 1) n) (char= (char line (+ j 1)) #\#))
                        (setq j (+ j 2) depth (- depth 1)) (when (= depth 0) (return)))
                       ((and (char= (char line j) #\#) (< (+ j 1) n) (char= (char line (+ j 1)) #\|))
                        (setq j (+ j 2) depth (+ depth 1)))
                       (t (setq j (+ j 1)))))
           (setq state (if (= depth 0) nil (cons :block depth)))
           (setq spans (cons (list start j :comment) spans) i j start j)))
        (t
         (let ((c (char line i)))
           (cond ((char= c #\;) (setq spans (cons (list i n :comment) spans) i n))
                 ((char= c #\") (setq state :string start i i (+ i 1)))
                 ((and (char= c #\#) (< (+ i 1) n) (char= (char line (+ i 1)) #\|)) (setq state (cons :block 1) start i i (+ i 2)))
                 ((and (char= c #\#) (< (+ i 2) n) (char= (char line (+ i 1)) #\\))
                  (let ((j (+ i 3))) (loop (when (or (>= j n) (%syntax-delimiter-p (char line j))) (return)) (setq j (+ j 1)))
                    (setq spans (cons (list i j :constant) spans) i j after-open nil)))
                 ((char= c #\() (setq after-open t i (+ i 1)))
                 ((%syntax-delimiter-p c) (when (char= c #\)) (setq after-open nil expect-name nil)) (setq i (+ i 1)))
                 ((and (char= c #\#) (< (+ i 1) n) (char= (char line (+ i 1)) #\')) (setq i (+ i 2) after-open nil))
                 (t (let ((j i))
                      (loop (when (or (>= j n) (%syntax-delimiter-p (char line j))) (return)) (setq j (+ j 1)))
                      (let* ((tok (string-downcase (subseq line i j)))
                             (class (cond ((char= (char tok 0) #\:) :keyword)
                                          ((char= (char tok 0) #\&) :keyword)
                                          ((or (string= tok "t") (string= tok "nil")) :constant)
                                          (expect-name (setq expect-name nil) :defname)
                                          ((and after-open (%definer-name-p tok)) (setq expect-name t) :builtin)
                                          ((and after-open (gethash tok *lisp-builtins*)) :builtin)
                                          (t nil))))
                        (when class (setq spans (cons (list i j class) spans)))
                        (setq i j after-open nil)))))))))
    (values (nreverse spans) state)))

(defun %syntax-state-at (b line &optional (fn #'lisp-line-syntax))
  "The syntax state (for the line syntax function FN, by default Lisp's) at the start of LINE of buffer B (cached)."
  (let ((v (buf-local b 'syntax-states)))
    (unless (eq (buf-local b 'syntax-fn) fn) (setq v nil) (setf (buf-local b 'syntax-fn) fn))
    (unless v (setq v (make-array 64 :adjustable t :fill-pointer 1 :initial-element nil)) (setf (buf-local b 'syntax-states) v))
    (loop (when (> (fill-pointer v) line) (return))
          (let ((k (- (fill-pointer v) 1)))
            (vector-push-extend (nth-value 1 (funcall fn (buf-line b k) (aref v k))) v)))
    (aref v line)))

(defun line-syntax-spans (b line)
  "Syntax spans for LINE of buffer B in its mode (Lisp source, or a diff), or NIL."
  (case (buffer-mode b)
    (lisp-mode (values (lisp-line-syntax (buf-line b line) (%syntax-state-at b line))))
    (diff-mode (let ((text (buf-line b line)))
                 (cond ((= (length text) 0) nil)
                       ((and (> (length text) 2) (string= (subseq text 0 3) "+++")) (list (list 0 (length text) :builtin)))
                       ((and (> (length text) 2) (string= (subseq text 0 3) "---")) (list (list 0 (length text) :builtin)))
                       ((char= (char text 0) #\+) (list (list 0 (length text) :added)))
                       ((char= (char text 0) #\-) (list (list 0 (length text) :removed)))
                       ((and (> (length text) 1) (string= (subseq text 0 2) "@@")) (list (list 0 (length text) :keyword)))
                       (t nil))))
    (t (let ((fn (and (buffer-mode b) (symbolp (buffer-mode b)) (get (buffer-mode b) 'line-syntax))))
         (and fn (values (funcall fn (buf-line b line) (%syntax-state-at b line fn))))))))

(defun %theme-color (key fallback)
  (let ((v (getf (theme-colors *theme*) key :none))) (if (eq v :none) (tc fallback) v)))

(defun syntax-face (class)
  "(values fg attr) for a syntax CLASS in the current theme."
  (let ((mono (= -1 (tc :fg))))
    (case class
      (:comment (values (%theme-color :syn-comment :dim) (if mono +dim+ 0)))
      (:string (values (%theme-color :syn-string :value) 0))
      ((:keyword :constant) (values (%theme-color :syn-keyword :link) 0))
      (:builtin (values (%theme-color :syn-builtin :prompt) +bold+))
      (:defname (values (%theme-color :syn-name :link) (if mono +underline+ +bold+)))
      (:added (values (%theme-color :syn-string :value) 0))
      (:removed (values (%theme-color :syn-removed :error) 0))
      (t (values (tc :fg) 0)))))

(defun %span-class (spans bc)
  (dolist (sp spans nil) (when (and (>= bc (car sp)) (< bc (cadr sp))) (return (caddr sp)))))

;;; ---- search highlighting ----------------------------------------------------------------------------------

(defun %line-matches (re line)
  "Byte ranges (start . end) of the non-empty matches of RE in LINE."
  (let ((acc nil) (pos 0) (n (length line)))
    (loop
      (when (> pos n) (return))
      (multiple-value-bind (ms me) (regex-find re line pos)
        (unless ms (return))
        (when (> me ms) (setq acc (cons (cons ms me) acc)))
        (setq pos (if (> me ms) me (if (< ms n) (nth-value 1 (utf8-char-at line ms)) (+ n 1))))))
    (nreverse acc)))

(defun %active-search-regex (w)
  "The regex of the isearch or query-replace in progress in W, for highlighting its other matches."
  (let ((is (ed-isearch w)) (q (ed-query w)))
    (cond (q (qr-regex q))
          ((and is (> (length (is-string is)) 0) (not (is-error is)))
           (ignore-errors (search-regex (is-string is) (is-regexp is)))))))

;;; ---- drawing ----------------------------------------------------------------------------------------------

(defun %view-point (w v)
  (let ((b (view-buffer v)))
    (if (eq v (ed-selected w))
        (values (point-line b) (point-col b))
        (let ((l (max 0 (min (view-line v) (- (buf-nlines b) 1))))) (values l (min (view-col v) (length (buf-line b l))))))))

(defun %mode-line-text (w v pline pdcol th)
  (let* ((b (view-buffer v)) (n (buf-nlines b)) (top (view-top v))
         (pos (cond ((and (= top 0) (<= n th)) "All") ((= top 0) "Top")
                    ((>= (+ top th) n) "Bot") (t (format nil "~D%" (floor (* 100 top) (max 1 (- n th))))))))
    (format nil " ~A  ~A   (~A~{ ~A~}~A)   L~D C~D   ~A"
            (if (buffer-modified b) "**" "--") (buffer-name b) (mode-display-name (buffer-mode b))
            (mapcar #'symbol-name (buffer-minor-modes b)) (if (ed-recording w) " Def" "") (+ pline 1) (+ 1 pdcol) pos)))

(defun draw-view (w v screen x y wd ht ox oy focused)
  "Draw view V of editor W in the rectangle X Y WD HT: its text and its mode line."
  (let* ((b (view-buffer v)) (th (max 1 (- ht 1))) (selected (eq v (ed-selected w))) (lispy (eq (buffer-mode b) 'lisp-mode)))
    (setf (view-height v) th (view-rx v) (- x ox) (view-ry v) (- y oy) (view-rw v) wd (view-rh v) ht)
    (multiple-value-bind (pline pcol) (%view-point w v)
      (ed-ensure-visible v th pline (buf-nlines b))
      (let* ((pdcol (display-col (buf-line b pline) pcol)) (hs (view-hscroll v)))
        (when (< pdcol hs) (setq hs pdcol))
        (when (>= pdcol (+ hs wd)) (setq hs (+ (- pdcol wd) 1)))
        (setf (view-hscroll v) hs)
        (let* ((parens (and selected (not (ed-minibuf w))
                            (ignore-errors (if lispy (%paren-match-offsets b)
                                               (let ((fn (and (symbolp (buffer-mode b)) (get (buffer-mode b) 'match-offsets))))
                                                 (and fn (funcall fn b)))))))
               (ppos (mapcar (lambda (o) (buf-pos-of-offset b o)) parens))
               (is (and selected (ed-isearch w))) (q (and selected (ed-query w)))
               (match (cond (q (qr-match q)) (is (is-match is))))
               (re (and selected (%active-search-regex w)))
               (fg0 (tc :fg)) (bg0 (tc :bg)))
          (dotimes (i th)
            (let ((ln (+ (view-top v) i)))
              (when (< ln (buf-nlines b))
                (multiple-value-bind (cells bytes) (%line-cells (buf-line b ln))
                  (let ((spans (line-syntax-spans b ln)) (others (and re (%line-matches re (buf-line b ln)))))
                    (dotimes (k wd)
                      (let ((dc (+ hs k)))
                        (when (< dc (length cells))
                          (let ((bc (aref bytes dc)) (fg fg0) (bg bg0) (attr 0) (class nil))
                            (when spans
                              (setq class (%span-class spans bc))
                              (when class (multiple-value-bind (f a) (syntax-face class) (setq fg f attr a))))
                            (when (and others (find-if (lambda (r) (and (>= bc (car r)) (< bc (cdr r)))) others))
                              (setq fg (tc :link) attr (+ +underline+ (logand attr +bold+))))
                            (when (and selected (%pos-in-region-p b ln bc))
                              (setq fg (tc :hi-fg) bg (tc :hi-bg) attr (if (= -1 (tc :hi-bg)) +reverse+ 0)))
                            (when (and match (= ln (car match)) (>= bc (cadr match)) (< bc (caddr match)))
                              (setq fg (tc :sel-fg) bg (tc :sel-bg) attr (if (= -1 (tc :sel-bg)) +reverse+ 0)))
                            (dolist (pp ppos) (when (and (= ln (car pp)) (= bc (cdr pp))) (setq fg (tc :sel-fg) bg (tc :link) attr (+ +bold+ (if (= -1 (tc :link)) +reverse+ 0)))))
                            (screen-put screen (+ y i) (+ x k) (aref cells dc) fg bg attr))))))))))
          ;; mode line: the selected view stands out when there are several
          (let* ((my (+ y th)) (text (%mode-line-text w v pline pdcol th))
                 (single (view-p (ed-layout w)))
                 (mfg (if (or selected single) (tc :wholine-fg) (tc :inactive-fg)))
                 (mbg (if (or selected single) (tc :wholine-bg) (tc :inactive-bg)))
                 (attr (if (= -1 mbg) (if (or selected single) +reverse+ +dim+) 0)))
            (screen-fill screen my x 1 wd 32 mfg mbg attr)
            (screen-draw-string screen my x text mfg mbg attr 0 (utf8-prefix-end text 0 wd)))
          (when (and selected focused (not (ed-minibuf w)))
            (setf (window-cursor w) (cons (+ y (- pline (view-top v))) (+ x (min (- wd 1) (max 0 (- pdcol hs))))))))))))

(defun draw-layout (w node screen x y wd ht ox oy focused)
  (cond ((view-p node) (when (and (>= ht 2) (>= wd 1)) (draw-view w node screen x y wd ht ox oy focused)))
        ((eq (car node) :below)
         (let ((h1 (max 2 (floor ht 2))))
           (draw-layout w (cadr node) screen x y wd h1 ox oy focused)
           (draw-layout w (caddr node) screen x (+ y h1) wd (- ht h1) ox oy focused)))
        (t
         (let ((w1 (max 1 (floor (- wd 1) 2))))
           (draw-layout w (cadr node) screen x y w1 ht ox oy focused)
           (dotimes (i ht) (screen-put screen (+ y i) (+ x w1) #x2502 (tc :border) (tc :bg) 0))
           (draw-layout w (caddr node) screen (+ x w1 1) y (- wd w1 1) ht ox oy focused)))))

(defun %echo-text (w)
  "The echo area: (values text highlight-start highlight-end)."
  (let ((mb (ed-minibuf w)) (is (ed-isearch w)) (q (ed-query w)) (hint (ed-hint w)))
    (cond (mb (%string-concat (mb-prompt mb) (mb-input mb)))
          (is (format nil "~A~A~A: ~A~A" (if (is-failed is) "Failing " "") (if (is-regexp is) "Regexp " "")
                      (if (is-forward is) "I-search" "I-search backward") (is-string is)
                      (if (is-error is) (format nil "  [~A]" (is-error is)) "")))
          (q (format nil "Query replacing ~A with ~A: (y, n, !, ., q, ^)" (qr-from q) (qr-to q)))
          ((ed-pending w) (format nil "~{~A~^ ~} -" (mapcar #'key-string (ed-pending w))))
          ((ed-message w) (ed-message w))
          (hint (values (car hint) (cadr hint) (caddr hint)))
          (t ""))))

(defmethod window-draw-content ((w editor-window) screen ix iy iw ih focused)
  (when (and (> iw 0) (> ih 2))
    (setf (window-title w) (format nil "Editor: ~A" (buffer-name (ed-buffer w))))
    (draw-layout w (ed-layout w) screen ix iy iw (- ih 1) ix iy focused)
    ;; echo area / minibuffer
    (let ((y (+ iy ih -1)) (mb (ed-minibuf w)))
      (multiple-value-bind (text hs he) (%echo-text w)
        (let ((end (utf8-prefix-end text 0 iw)))
          (screen-draw-string screen y ix text (tc :fg) (tc :bg) 0 0 end)
          (when (and hs (< hs end))
            (screen-draw-string screen y (+ ix (utf8-length (subseq text 0 hs))) text (tc :link) (tc :bg) +bold+ hs (min he end)))))
      (when (and focused mb)
        (setf (window-cursor w) (cons y (+ ix (min (- iw 1) (utf8-length (%string-concat (mb-prompt mb) (subseq (mb-input mb) 0 (mb-pos mb)))))))))))
  (setf (window-content-height w) (buf-nlines (ed-buffer w)) (window-scroll w) (ed-top w)))

(defmethod window-scroll-by ((w editor-window) n)
  (setf (ed-top w) (max 0 (min (- (buf-nlines (ed-buffer w)) 1) (- (ed-top w) n)))))

(defmethod window-package ((w editor-window)) (or *ed-package* *package*))

(defun %view-at (w lx ly)
  (dolist (v (ed-views w) nil)
    (when (and (>= lx (view-rx v)) (< lx (+ (view-rx v) (view-rw v))) (>= ly (view-ry v)) (< ly (+ (view-ry v) (view-rh v))))
      (return v))))

(defmethod window-mouse ((w editor-window) kind lx ly button mods)
  (declare (ignore mods button))
  (let ((v (%view-at w lx ly)))
    (when v
      (when (and (eq kind :press) (not (eq v (ed-selected w)))) (ed-select-view w v))
      (when (eq v (ed-selected w))
        (let* ((b (view-buffer v)) (ry (- ly (view-ry v))))
          (when (and (>= ry 0) (< ry (view-height v)))
            (let* ((ln (min (- (buf-nlines b) 1) (+ (view-top v) ry)))
                   (bc (byte-col-for-display (buf-line b ln) (+ (- lx (view-rx v)) (view-hscroll v)))))
              (cond ((eq kind :press) (buf-goto b ln bc) (setf (buffer-mark b) nil))
                    ((eq kind :drag)
                     (unless (buffer-mark b) (setf (buffer-mark b) (cons (point-line b) (point-col b))))
                     (buf-goto b ln bc)))))))))
  t)

;;; ---- minibuffer ----------------------------------------------------------------------------------------------

(defun ed-read-string (w prompt callback &key completer (initial ""))
  (setf (ed-minibuf w) (make-minibuf :prompt prompt :input initial :pos (length initial) :callback callback :completer completer)))

(defun %mb-set (mb s) (setf (mb-input mb) s (mb-pos mb) (length s)))

(defun minibuf-complete (w mb)
  (when (mb-completer mb)
    (let* ((cands (funcall (mb-completer mb) (mb-input mb))))
      (cond ((null cands) (ed-message* w "[No match]"))
            ((null (cdr cands)) (%mb-set mb (car cands)))
            (t (let ((cp (common-prefix cands)))
                 (when (> (length cp) (length (mb-input mb))) (%mb-set mb cp))
                 (ed-message* w "~{~A~^  ~}~A" (subseq cands 0 (min 10 (length cands))) (if (> (length cands) 10) " ..." ""))))))))

(defun minibuf-key (w key)
  (let* ((mb (ed-minibuf w)) (c (key-code key)) (m (key-mods key)) (in (mb-input mb)) (pos (mb-pos mb))
         (ctrl (/= 0 (logand m +ctrl+))))
    (setf (ed-message w) nil)
    (cond ((eq c :return)
           (setf (ed-minibuf w) nil)
           (handler-case (funcall (mb-callback mb) in) (error (e) (ed-message* w "~A" e))))
          ((or (eq c :escape) (and ctrl (eql c 103))) (setf (ed-minibuf w) nil) (ed-message* w "Quit"))
          ((eq c :tab) (minibuf-complete w mb))
          ((eq c :backspace) (when (> pos 0) (setf (mb-input mb) (%string-concat (subseq in 0 (- pos 1)) (subseq in pos)) (mb-pos mb) (- pos 1))))
          ((or (eq c :delete) (and ctrl (eql c 100))) (when (< pos (length in)) (setf (mb-input mb) (%string-concat (subseq in 0 pos) (subseq in (+ pos 1))))))
          ((or (eq c :left) (and ctrl (eql c 98))) (setf (mb-pos mb) (max 0 (- pos 1))))
          ((or (eq c :right) (and ctrl (eql c 102))) (setf (mb-pos mb) (min (length in) (+ pos 1))))
          ((or (eq c :home) (and ctrl (eql c 97))) (setf (mb-pos mb) 0))
          ((or (eq c :end) (and ctrl (eql c 101))) (setf (mb-pos mb) (length in)))
          ((and ctrl (eql c 107)) (setf (mb-input mb) (subseq in 0 pos)))
          ((and ctrl (eql c 117)) (%mb-set mb ""))
          ((and (integerp c) (not ctrl) (/= 0 m) (= m +meta+) nil) nil)
          ((and (integerp c) (not ctrl) (zerop (logand m +meta+)) (>= c 32))
           (let ((s (if (< c 128) (string (code-char c)) (%utf8-encode c))))
             (setf (mb-input mb) (%string-concat (subseq in 0 pos) s (subseq in pos)) (mb-pos mb) (+ pos (length s))))))))

;;; ---- searching ------------------------------------------------------------------------------------------------
;;; Every search is a regular expression (a literal search quotes its string).  Searches fold case unless the
;;; string has an upper-case letter.  Matches lie within one line.

(defun quote-regex (string)
  "A regular expression that matches STRING literally."
  (let ((out (make-string-output-stream)))
    (dotimes (i (length string))
      (let ((c (char string i)))
        (when (find c ".*+?()[]{}|^$\\") (write-char #\\ out))
        (write-char c out)))
    (get-output-stream-string out)))

(defun search-case-fold-p (string &optional regexp)
  "True when a search for STRING should ignore case: it has no upper-case letter (escapes aside, for a regexp)."
  (let ((codes (utf8-codes string)) (fold t))
    (loop (when (null codes) (return fold))
          (let ((c (pop codes)))
            (cond ((and regexp (= c 92)) (pop codes))
                  ((unicode-upper-case-p c) (setq fold nil)))))))

(defvar *search-regex-cache* nil)

(defun search-regex (string regexp)
  "The compiled regex for searching for STRING (a regexp if REGEXP, else literal text)."
  (let ((key (list string regexp)))
    (if (equal key (car *search-regex-cache*))
        (cdr *search-regex-cache*)
        (let ((re (regex-compile (if regexp string (quote-regex string)) :case-fold (search-case-fold-p string regexp))))
          (setq *search-regex-cache* (cons key re))
          re))))

(defun buf-find (b re from-line from-col &optional backward)
  "The next match of RE in buffer B from (FROM-LINE . FROM-COL): forward, the first starting there or later;
BACKWARD, the last ending there or earlier.  Returns (line start end groups), or NIL."
  (let ((n (buf-nlines b)))
    (if backward
        (let ((l (min from-line (- n 1))) (limit from-col))
          (loop (when (< l 0) (return nil))
                (multiple-value-bind (ms me groups) (regex-find-backward re (buf-line b l) (min limit (length (buf-line b l))))
                  (when ms (return (list l ms me groups))))
                (setq l (- l 1))
                (when (>= l 0) (setq limit (length (buf-line b l))))))
        (let ((l from-line) (c from-col))
          (loop (when (>= l n) (return nil))
                (when (<= c (length (buf-line b l)))
                  (multiple-value-bind (ms me groups) (regex-find re (buf-line b l) c)
                    (when ms (return (list l ms me groups)))))
                (setq l (+ l 1) c 0))))))

(defun %after-char (b line col)
  "The column after the character at COL of LINE (COL + 1 at the end of the line)."
  (let ((text (buf-line b line))) (if (< col (length text)) (char-end text col) (+ col 1))))

;;; ---- incremental search ------------------------------------------------------------------------------------------

(defvar *ed-executing-macro* nil)
(defvar *ed-command-failed* nil)

(defun isearch-start (w forward &optional regexp)
  (let ((b (ed-buffer w)))
    (setf (ed-isearch w) (make-isearch :forward forward :regexp regexp :start-line (point-line b) :start-col (point-col b)
                                       :anchor-line (point-line b) :anchor-col (point-col b)))))

(defun %isearch-regex (is)
  (handler-case (progn (setf (is-error is) nil) (search-regex (is-string is) (is-regexp is)))
    (regex-syntax-error () (setf (is-error is) "incomplete input") nil)))

(defun %isearch-found (w p)
  (let ((is (ed-isearch w)) (b (ed-buffer w)))
    (setf (is-failed is) nil (is-match is) (list (car p) (cadr p) (caddr p)))
    (setf (is-anchor-line is) (car p) (is-anchor-col is) (cadr p))
    (if (is-forward is) (buf-goto b (car p) (caddr p)) (buf-goto b (car p) (cadr p)))))

(defun isearch-run (w from-line from-col &optional (at-anchor nil))
  "Search from FROM in the isearch direction; with AT-ANCHOR, first try a match starting at the anchor."
  (let* ((is (ed-isearch w)) (b (ed-buffer w)))
    (if (= (length (is-string is)) 0)
        (setf (is-match is) nil (is-failed is) nil)
        (let ((re (%isearch-regex is)))
          (when re
            (let ((here (and at-anchor
                             (let ((line (buf-line b (is-anchor-line is))))
                               (and (<= (is-anchor-col is) (length line))
                                    (let ((e (regex-match re line (is-anchor-col is)))) (and e (list (is-anchor-line is) (is-anchor-col is) e))))))))
              (let ((p (or here (buf-find b re from-line from-col (not (is-forward is))))))
                (if p
                    (%isearch-found w p)
                    (progn (setf (is-failed is) t) (when *ed-executing-macro* (setq *ed-command-failed* t)))))))))))

(defun isearch-key (w key)
  (let* ((is (ed-isearch w)) (b (ed-buffer w)) (c (key-code key)) (m (key-mods key))
         (ctrl (/= 0 (logand m +ctrl+))) (meta (/= 0 (logand m +meta+))))
    (cond ((and ctrl (member c '(115 114)))                     ; C-s / C-r (also C-M-s / C-M-r): next match
           (let ((fwd (eql c 115)))
             (when (and (= (length (is-string is)) 0) (> (length (ed-last-search w)) 0)) (setf (is-string is) (ed-last-search w)))
             (setf (is-forward is) fwd)
             (let ((from-l (is-anchor-line is)) (from-c (is-anchor-col is)))
               (when (is-failed is)                           ; wrap around
                 (if fwd (setq from-l 0 from-c 0) (setq from-l (- (buf-nlines b) 1) from-c (length (buf-line b (- (buf-nlines b) 1))))))
               (if fwd
                   (isearch-run w from-l (cond ((or (is-failed is) (null (is-match is))) from-c)
                                               ((> (caddr (is-match is)) (cadr (is-match is))) (caddr (is-match is)))   ; after the match
                                               (t (%after-char b from-l from-c))))
                   (isearch-run w from-l from-c)))))
          ((and ctrl (eql c 103)) (buf-goto b (is-start-line is) (is-start-col is)) (setf (ed-isearch w) nil) (ed-message* w "Quit"))
          ((eq c :return) (setf (ed-last-search w) (is-string is)) (setf (ed-isearch w) nil)
           (unless (and (= (point-line b) (is-start-line is)) (= (point-col b) (is-start-col is)))
             (setf (ed-mark-ring w) (cons (list b (is-start-line is) (is-start-col is)) (ed-mark-ring w)))))
          ((and meta (not ctrl) (eql c 114))                    ; M-r: toggle regular expression search
           (setf (is-regexp is) (not (is-regexp is)))
           (isearch-run w (is-start-line is) (is-start-col is)))
          ((and ctrl (eql c 119))                               ; C-w: add the rest of the word at point
           (let* ((l (point-line b)) (col (point-col b)) (text (buf-line b l)) (e col))
             (loop (when (or (>= e (length text)) (not (word-char-p (code-at text e)))) (return)) (setq e (char-end text e)))
             (when (= e col) (setq e (min (length text) (char-end text col))))
             (when (> e col)
               (setf (is-string is) (%string-concat (is-string is) (if (is-regexp is) (quote-regex (subseq text col e)) (subseq text col e))))
               (isearch-run w (is-anchor-line is) (is-anchor-col is) t))))
          ((eq c :backspace)
           (when (> (length (is-string is)) 0)
             (setf (is-string is) (subseq (is-string is) 0 (char-start (is-string is) (length (is-string is)))))
             (isearch-run w (is-start-line is) (is-start-col is))
             (when (= (length (is-string is)) 0) (buf-goto b (is-start-line is) (is-start-col is)))))
          ((and (integerp c) (not ctrl) (not meta) (>= c 32))
           (setf (is-string is) (%string-concat (is-string is) (if (< c 128) (string (code-char c)) (%utf8-encode c))))
           ;; a longer string: try at the same place first; backward, a match may end where the last one did
           (isearch-run w (is-anchor-line is)
                        (if (and (not (is-forward is)) (is-match is)) (max (is-anchor-col is) (caddr (is-match is))) (is-anchor-col is))
                        t))
          (t (setf (ed-last-search w) (is-string is)) (setf (ed-isearch w) nil)
             (editor-dispatch-key w key)))))

;;; ---- query replace -------------------------------------------------------------------------------------------------

(defstruct (qreplace (:conc-name qr-) (:constructor make-qreplace) (:copier nil))
  from to regex regexp fold match groups (count 0))

(defun %preserve-case (matched replacement)
  "REPLACEMENT in the case pattern of MATCHED (all capitals, or capitalized), as Emacs does when folding."
  (let ((letters (remove-if-not (lambda (c) (or (unicode-upper-case-p c) (unicode-lower-case-p c))) (utf8-codes matched))))
    (cond ((null letters) replacement)
          ((and (cdr letters) (every #'unicode-upper-case-p letters)) (utf8-upcase replacement))
          ((and (unicode-upper-case-p (car letters)) (every #'unicode-lower-case-p (cdr letters))) (utf8-capitalize replacement))
          ((and (null (cdr letters)) (unicode-upper-case-p (car letters))) (utf8-capitalize replacement))
          (t replacement))))

(defun %qr-find (w from-line from-col)
  "Find the next match for the query replace from FROM; returns T if there is one (point goes to its end)."
  (let* ((q (ed-query w)) (b (ed-buffer w)) (p (buf-find b (qr-regex q) from-line from-col)))
    (if p
        (progn (setf (qr-match q) (list (car p) (cadr p) (caddr p)) (qr-groups q) (cadddr p))
               (buf-goto b (car p) (caddr p))
               t)
        (progn (setf (qr-match q) nil) nil))))

(defun %qr-replace (w)
  "Replace the current match; returns the position (line . col) to search from next."
  (let* ((q (ed-query w)) (b (ed-buffer w)) (m (qr-match q)) (l (car m)) (ms (cadr m)) (me (caddr m))
         (line (buf-line b l)) (matched (subseq line ms me))
         (rep (if (qr-regexp q) (regex-expand (qr-to q) line (qr-groups q)) (qr-to q))))
    (when (and (qr-fold q) (search-case-fold-p (qr-to q) (qr-regexp q))) (setq rep (%preserve-case matched rep)))
    (buf-undo-boundary b)
    (buf-delete b l ms l me)
    (buf-goto b l ms)
    (buf-insert b rep)
    (setf (qr-count q) (+ 1 (qr-count q)))
    (if (= ms me) (cons (point-line b) (%after-char b (point-line b) (point-col b))) (cons (point-line b) (point-col b)))))

(defun %qr-skip-position (w)
  (let* ((q (ed-query w)) (m (qr-match q)))
    (if (= (cadr m) (caddr m)) (cons (car m) (%after-char (ed-buffer w) (car m) (caddr m))) (cons (car m) (caddr m)))))

(defun %qr-finish (w &optional quit)
  (let ((q (ed-query w)))
    (setf (ed-query w) nil)
    (ed-message* w "~:[Replaced~;Quit: replaced~] ~D occurrence~:P" quit (qr-count q))))

(defun start-query-replace (w from to regexp &optional all)
  "Replace matches of FROM (a regexp if REGEXP) with TO after point, asking about each one unless ALL."
  (let ((re (handler-case (search-regex from regexp) (regex-syntax-error (e) (ed-message* w "Invalid regexp: ~A" e) nil))))
    (when re
      (let ((b (ed-buffer w)))
        (push-mark w)
        (setf (ed-query w) (make-qreplace :from from :to to :regex re :regexp regexp :fold (search-case-fold-p from regexp)))
        (cond ((not (%qr-find w (point-line b) (point-col b))) (%qr-finish w))
              (all (%qr-replace-rest w)))))))

(defun %qr-replace-rest (w)
  (loop (let ((next (%qr-replace w))) (unless (%qr-find w (car next) (cdr next)) (return))))
  (%qr-finish w))

(defun query-replace-key (w key)
  (let* ((c (key-code key)) (m (key-mods key)) (ctrl (/= 0 (logand m +ctrl+))))
    (cond ((and (integerp c) (not ctrl) (member c '(121 32)))           ; y, SPC: replace, go on
           (let ((next (%qr-replace w))) (unless (%qr-find w (car next) (cdr next)) (%qr-finish w))))
          ((or (eq c :backspace) (and (integerp c) (not ctrl) (eql c 110))) ; n, DEL: skip
           (let ((next (%qr-skip-position w))) (unless (%qr-find w (car next) (cdr next)) (%qr-finish w))))
          ((and (integerp c) (not ctrl) (eql c 33)) (%qr-replace-rest w))  ; !: all the rest
          ((and (integerp c) (not ctrl) (eql c 46)) (%qr-replace w) (%qr-finish w))  ; .: this one, then stop
          ((or (eq c :return) (eq c :escape) (and (integerp c) (not ctrl) (eql c 113))) (%qr-finish w))
          ((and ctrl (eql c 103)) (%qr-finish w t))
          ((and (integerp c) (not ctrl) (eql c 94)) nil)                  ; ^: stay (no going back)
          (t (%qr-finish w) (editor-dispatch-key w key)))))

(defun %read-replacement (ed what regexp all)
  (ed-read-string ed (format nil "~A: " what)
                  (lambda (from)
                    (if (= (length from) 0)
                        (ed-message* ed "Nothing to replace")
                        (ed-read-string ed (format nil "~A ~A with: " what from)
                                        (lambda (to) (start-query-replace ed from to regexp all)))))))

;;; ---- key dispatch ---------------------------------------------------------------------------------------------------

(defun %kill-command-p (name) (member name '(kill-line kill-word backward-kill-word kill-region kill-sexp)))

(defun run-editor-command (w name arg)
  (let ((fn (get name '%ecommand)) (b (ed-buffer w)))
    (if (null fn)
        (ed-message* w "~A is not a command" name)
        (progn
          (cond ((eq name 'self-insert-command)
                 (if (and (eq (ed-last-command w) 'self-insert-command) (< (ed-insert-count w) 20))
                     (setf (ed-insert-count w) (+ 1 (ed-insert-count w)))
                     (progn (buf-undo-boundary b) (setf (ed-insert-count w) 0))))
                ((member name '(undo redo)) nil)
                (t (buf-undo-boundary b)))
          (unless (%kill-command-p name) (setq *last-kill-command* nil))
          (handler-case (funcall fn w arg)
            (error (e) (setq *ed-command-failed* t) (ed-message* w "~A" e)))
          (setf (ed-last-command w) name)))))

(defun editor-dispatch-key (w key)
  (let* ((c (key-code key)) (m (key-mods key)) (b (ed-buffer w)))
    (cond
      ;; universal argument digits after C-u
      ((and (ed-prefix w) (null (ed-pending w)) (integerp c) (<= 48 c 57) (or (zerop m) (= m +meta+)))
       (setf (ed-prefix w) (if (eq (ed-last-command w) :prefix-digit) (+ (* 10 (ed-prefix w)) (- c 48)) (- c 48)))
       (setf (ed-last-command w) :prefix-digit) (ed-message* w "Arg: ~D" (ed-prefix w)))
      (t
       (let* ((pend (ed-pending w))
              (esc-prefix (let ((lk (car (last pend)))) (and lk (eq (key-code lk) :escape) (zerop (key-mods lk)))))
              (keys (cond ((not esc-prefix) (append pend (list key)))
                          ;; ESC C-g cancels the Meta prefix, like plain C-g
                          ((and (eql c 103) (= m +ctrl+)) (append (butlast pend) (list key)))
                          ;; ESC <key> is Meta-<key>, for terminals that cannot send Meta directly
                          (t (append (butlast pend) (list (make-key c (logior m +meta+)))))))
              (maps (append (let ((mm (mode-keymap (buffer-mode b)))) (and mm (list mm))) (list *global-keymap*)))
              (res (lookup-key maps keys)))
         (cond ((eq res :partial) (setf (ed-pending w) keys))
               ((and (null res) (eq c :escape) (zerop m))
                (setf (ed-pending w) keys) (ed-message* w "~{~A~^ ~}-" (mapcar #'key-string keys)))
               (res (setf (ed-pending w) nil (ed-command-keys w) keys)
                    (let ((arg (ed-prefix w)))
                      (setf (ed-prefix w) nil)
                      (run-editor-command w res arg)))
               ((and (null (cdr keys)) (not esc-prefix) (integerp c) (zerop (logand m +ctrl+)) (zerop (logand m +meta+)) (>= c 32))
                (let ((arg (ed-prefix w))) (setf (ed-prefix w) nil)
                  (setq *typed-char* c)
                  (run-editor-command w 'self-insert-command arg)))
               (t (setf (ed-pending w) nil (ed-prefix w) nil)
                  (setq *ed-command-failed* t)
                  (ed-message* w "~{~A~^ ~} is undefined" (mapcar #'key-string keys)))))))))


(defmethod window-key ((w editor-window) key)
  (when (and (ed-recording w) (not *ed-executing-macro*)) (setf (cdr (ed-recording w)) (cons key (cdr (ed-recording w)))))
  (setf (ed-message w) (if (ed-pending w) (ed-message w) nil))
  (cond ((and (ed-macro-repeat w) (eql (key-code key) 101) (zerop (key-mods key)) (not (ed-minibuf w)) (not (ed-isearch w)))
         (run-editor-command w 'call-last-kbd-macro nil))       ; e right after C-x e repeats the macro
        (t (setf (ed-macro-repeat w) nil)
           (cond ((ed-minibuf w) (minibuf-key w key))
                 ((ed-isearch w) (isearch-key w key))
                 ((ed-query w) (query-replace-key w key))
                 (t (editor-dispatch-key w key)))))
  (ed-update-hint w)
  t)

;;; ---- commands: motion --------------------------------------------------------------------------------------------------

(defun %n (arg) (if (integerp arg) arg 1))
(defmacro with-b ((b ed) &body body) `(let ((,b (ed-buffer ,ed))) ,@body))

(defcmd forward-char (ed arg) (with-b (b ed) (buf-forward-char b (%n arg))))
(defcmd backward-char (ed arg) (with-b (b ed) (buf-backward-char b (%n arg))))
(defcmd next-line (ed arg) (with-b (b ed) (buf-move-vertically b (%n arg))))
(defcmd previous-line (ed arg) (with-b (b ed) (buf-move-vertically b (- (%n arg)))))
(defcmd beginning-of-line (ed arg) (with-b (b ed) (buf-goto b (point-line b) 0)))
(defcmd end-of-line (ed arg) (with-b (b ed) (buf-goto b (point-line b) (length (buf-current-line b)))))
(defcmd forward-word (ed arg) (with-b (b ed) (dotimes (i (%n arg)) (buf-forward-word b))))
(defcmd backward-word (ed arg) (with-b (b ed) (dotimes (i (%n arg)) (buf-backward-word b))))
(defcmd beginning-of-buffer (ed arg)
  (with-b (b ed) (push-mark ed) (buf-goto b 0 0)))
(defcmd end-of-buffer (ed arg)
  (with-b (b ed) (push-mark ed) (buf-goto b (- (buf-nlines b) 1) (length (buf-line b (- (buf-nlines b) 1))))))
(defcmd scroll-up-command (ed arg)
  (with-b (b ed) (buf-move-vertically b (max 1 (- (ed-view-h ed) 2)))))
(defcmd scroll-down-command (ed arg)
  (with-b (b ed) (buf-move-vertically b (- (max 1 (- (ed-view-h ed) 2))))))
(defcmd goto-line (ed arg)
  (if (integerp arg)
      (with-b (b ed) (buf-goto b (- arg 1) 0))
      (ed-read-string ed "Goto line: " (lambda (s) (let ((n (parse-integer s :junk-allowed t))) (when n (buf-goto (ed-buffer ed) (- n 1) 0)))))))
(defcmd recenter (ed arg) (with-b (b ed) (setf (ed-top ed) (max 0 (- (point-line b) (floor (ed-view-h ed) 2))))))

(defun push-mark (ed)
  (with-b (b ed) (setf (ed-mark-ring ed) (cons (list b (point-line b) (point-col b)) (ed-mark-ring ed)))))

(defcmd pop-mark (ed arg)
  (let ((m (car (ed-mark-ring ed))))
    (if m
        (progn (setf (ed-mark-ring ed) (cdr (ed-mark-ring ed)))
               (ed-switch-to-buffer ed (car m)) (buf-goto (car m) (cadr m) (caddr m)))
        (ed-message* ed "Mark ring is empty"))))

;;; ---- commands: editing --------------------------------------------------------------------------------------------------

(defun utf8-string (code) (if (< code 128) (string (code-char code)) (%utf8-encode code)))

(defcmd self-insert-command (ed arg)
  (with-b (b ed)
    (let* ((c *typed-char*) (mode (buffer-mode b)) (before (and mode (symbolp mode) (get mode 'before-self-insert))))
      (unless (and before (funcall before ed c))
        (dotimes (i (%n arg)) (buf-insert b (utf8-string c)))
        (when (and (= c 32) (member 'auto-fill (buffer-minor-modes b))) (auto-fill-maybe ed))
        (let ((after (and mode (symbolp mode) (get mode 'after-self-insert)))) (when after (funcall after ed c)))))))

(defun auto-fill-maybe (ed)
  (with-b (b ed)
    (let ((line (buf-current-line b)))
      (when (> (display-col line (point-col b)) 70)
        (let ((sp (position #\Space (subseq line 0 (point-col b)) :from-end t :end 70)))
          (when (and sp (> sp 0))
            (let ((save (- (point-col b) sp 1)))
              (buf-delete b (point-line b) sp (point-line b) (+ sp 1))
              (buf-insert b (string #\Newline))
              (buf-goto b (point-line b) (min (length (buf-current-line b)) save)))))))))

(defcmd newline (ed arg) (with-b (b ed) (dotimes (i (%n arg)) (buf-insert b (string #\Newline)))))
(defcmd open-line (ed arg) (with-b (b ed) (let ((l (point-line b)) (c (point-col b))) (buf-insert b (string #\Newline)) (buf-goto b l c))))
(defcmd delete-char (ed arg) (with-b (b ed) (let ((l (point-line b)) (c (point-col b))) (buf-forward-char b (%n arg)) (buf-delete b l c (point-line b) (point-col b)))))
(defcmd delete-backward-char (ed arg)
  (with-b (b ed) (let ((l (point-line b)) (c (point-col b))) (buf-backward-char b (%n arg)) (buf-delete b (point-line b) (point-col b) l c))))

(defun do-kill (ed text &optional prepend)
  (kill-new text *last-kill-command* prepend) (setq *last-kill-command* t) text)

(defcmd kill-line (ed arg)
  (with-b (b ed)
    (let* ((l (point-line b)) (c (point-col b)) (line (buf-current-line b)))
      (if (and (>= c (length line)) (< l (- (buf-nlines b) 1)))
          (do-kill ed (buf-delete b l c (+ l 1) 0))
          (do-kill ed (buf-delete b l c l (length line)))))))
(defcmd kill-word (ed arg)
  (with-b (b ed) (let ((l (point-line b)) (c (point-col b))) (dotimes (i (%n arg)) (buf-forward-word b)) (do-kill ed (buf-delete b l c (point-line b) (point-col b))))))
(defcmd backward-kill-word (ed arg)
  (with-b (b ed) (let ((l (point-line b)) (c (point-col b))) (dotimes (i (%n arg)) (buf-backward-word b)) (do-kill ed (buf-delete b (point-line b) (point-col b) l c) t))))
(defcmd kill-region (ed arg)
  (with-b (b ed)
    (multiple-value-bind (l1 c1 l2 c2) (region-bounds b)
      (unless l1 (error "The mark is not set now, so there is no region"))
      (do-kill ed (buf-delete b l1 c1 l2 c2)))))
(defcmd copy-region-as-kill (ed arg)
  (with-b (b ed)
    (multiple-value-bind (l1 c1 l2 c2) (region-bounds b)
      (unless l1 (error "The mark is not set now, so there is no region"))
      (kill-new (%raw-text b l1 c1 l2 c2)) (setq *last-kill-command* nil) (setf (buffer-mark b) nil) (ed-message* ed "Region copied"))))
(defcmd yank (ed arg)
  (with-b (b ed)
    (let ((text (current-kill 0)))
      (unless text (error "Kill ring is empty"))
      (setf (buf-local b 'yank-start) (cons (point-line b) (point-col b)) (buf-local b 'yank-index) 0)
      (buf-insert b text))))
(defcmd yank-pop (ed arg)
  (with-b (b ed)
    (unless (member (ed-last-command ed) '(yank yank-pop)) (error "Previous command was not a yank"))
    (let* ((start (buf-local b 'yank-start)) (i (+ 1 (buf-local b 'yank-index 0))))
      (buf-delete b (car start) (cdr start) (point-line b) (point-col b))
      (setf (buf-local b 'yank-index) i)
      (buf-insert b (current-kill i)))))
(defcmd set-mark-command (ed arg) (with-b (b ed) (setf (buffer-mark b) (cons (point-line b) (point-col b))) (ed-message* ed "Mark set")))
(defcmd exchange-point-and-mark (ed arg)
  (with-b (b ed) (let ((m (buffer-mark b))) (when m (let ((l (point-line b)) (c (point-col b))) (buf-goto b (car m) (cdr m)) (setf (buffer-mark b) (cons l c)))))))
(defcmd mark-whole-buffer (ed arg)
  (with-b (b ed) (buf-goto b (- (buf-nlines b) 1) (length (buf-line b (- (buf-nlines b) 1)))) (setf (buffer-mark b) (cons 0 0))))
(defcmd transpose-chars (ed arg)
  (with-b (b ed)
    (let* ((line (buf-current-line b)) (n (length line))
           (c (if (and (>= (point-col b) n) (> n 0)) (char-start line n) (point-col b))))   ; at end of line, the last two
      (when (and (> c 0) (< c n))
        (let* ((s (char-start line c)) (e (char-end line c)))
          (buf-delete b (point-line b) s (point-line b) e)
          (buf-insert b (%string-concat (subseq line c e) (subseq line s c))))))))

(defun %replace-word (ed fn)
  (with-b (b ed)
    (let ((l (point-line b)) (c (point-col b)))
      (buf-forward-word b)
      (let ((text (buf-delete b l c (point-line b) (point-col b))))
        (buf-insert b (funcall fn text))))))
(defcmd upcase-word (ed arg) (%replace-word ed #'utf8-upcase))
(defcmd downcase-word (ed arg) (%replace-word ed #'utf8-downcase))
(defcmd capitalize-word (ed arg) (%replace-word ed #'utf8-capitalize))

(defcmd undo (ed arg)
  (with-b (b ed) (if (buf-undo b) (ed-message* ed "Undo") (ed-message* ed "No further undo information"))))
(defcmd redo (ed arg)
  (with-b (b ed) (if (buf-redo b) (ed-message* ed "Redo") (ed-message* ed "Nothing to redo"))))
(defcmd undo-tree-select-branch (ed arg)
  (with-b (b ed) (if (undo-tree-select b (%n arg)) (ed-message* ed "Branch ~D selected; use redo" (%n arg)) (ed-message* ed "No such branch"))))

(defcmd keyboard-quit (ed arg)
  (setf (ed-pending ed) nil (ed-prefix ed) nil (buffer-mark (ed-buffer ed)) nil) (ed-message* ed "Quit"))
(defcmd universal-argument (ed arg) (setf (ed-prefix ed) (if (integerp (ed-prefix ed)) (* 4 (ed-prefix ed)) 4)) (ed-message* ed "C-u ~D" (ed-prefix ed)))

;;; rectangles
(defcmd kill-rectangle (ed arg) (buf-kill-rectangle (ed-buffer ed)))
(defcmd yank-rectangle (ed arg) (buf-yank-rectangle (ed-buffer ed)))
(defcmd open-rectangle (ed arg) (buf-open-rectangle (ed-buffer ed)))
(defcmd clear-rectangle (ed arg) (buf-clear-rectangle (ed-buffer ed)))
(defcmd string-rectangle (ed arg)
  (ed-read-string ed "String rectangle: " (lambda (s) (buf-string-rectangle (ed-buffer ed) s))))

;;; search
(defcmd isearch-forward (ed arg) (isearch-start ed t))
(defcmd isearch-backward (ed arg) (isearch-start ed nil))
(defcmd replace-string (ed arg) (%read-replacement ed "Replace string" nil t))

;;; modes
(defun set-buffer-mode (b mode) (setf (buffer-mode b) mode))
(defcmd lisp-mode (ed arg) (set-buffer-mode (ed-buffer ed) 'lisp-mode))
(defcmd text-mode (ed arg) (set-buffer-mode (ed-buffer ed) 'text-mode))
(defcmd fundamental-mode (ed arg) (set-buffer-mode (ed-buffer ed) 'fundamental-mode))
(defcmd auto-fill-mode (ed arg)
  (with-b (b ed) (if (member 'auto-fill (buffer-minor-modes b))
                     (setf (buffer-minor-modes b) (remove 'auto-fill (buffer-minor-modes b)))
                     (setf (buffer-minor-modes b) (cons 'auto-fill (buffer-minor-modes b))))))

(defun editor-command-names (prefix)
  (sort (mapcar (lambda (n) (string-downcase (symbol-name n)))
                (remove-if-not (lambda (n) (%word-prefix-p (string-upcase (substitute #\- #\Space prefix)) (symbol-name n))) *editor-commands*))
        #'string<))

(defcmd execute-extended-command (ed arg)
  (ed-read-string ed "M-x "
                  (lambda (name)
                    (let ((sym (find (string-upcase (substitute #\- #\Space name)) *editor-commands* :key #'symbol-name :test #'string=)))
                      (if sym (run-editor-command ed sym arg) (ed-message* ed "[No command named ~A]" name))))
                  :completer #'editor-command-names))

;;; ---- commands: Lisp mode ---------------------------------------------------------------------------------------------------------

(defcmd forward-sexp (ed arg)
  (with-b (b ed) (dotimes (i (%n arg))
                   (let ((e (sexp-end (buffer-string b) (point-offset b)))) (if e (goto-offset b e) (error "No more sexps"))))))
(defcmd backward-sexp (ed arg)
  (with-b (b ed) (dotimes (i (%n arg))
                   (let ((st (sexp-start-before (buffer-string b) (point-offset b)))) (if st (goto-offset b st) (error "No previous sexp"))))))
(defcmd up-list (ed arg)
  (with-b (b ed) (let* ((s (buffer-string b)) (stack (sexp-context s (point-offset b))))
                   (if stack (let ((e (list-end s (+ (car stack) 1)))) (if e (goto-offset b e) (error "Unbalanced parentheses"))) (error "At top level")))))
(defcmd backward-up-list (ed arg)
  (with-b (b ed) (let ((stack (sexp-context (buffer-string b) (point-offset b))))
                   (if stack (goto-offset b (car stack)) (error "At top level")))))
(defcmd down-list (ed arg)
  (with-b (b ed) (let* ((s (buffer-string b)) (i (point-offset b)) (p (%string-position-char s #\( i)))
                   (if p (goto-offset b (+ p 1)) (error "No inner list")))))
(defcmd kill-sexp (ed arg)
  (with-b (b ed) (let* ((s (buffer-string b)) (off (point-offset b)) (e (sexp-end s off)))
                   (unless e (error "No sexp to kill"))
                   (let ((p (buf-pos-of-offset b e)) (l (point-line b)) (c (point-col b)))
                     (do-kill ed (buf-delete b l c (car p) (cdr p)))))))
(defun %defun-start (b)
  "Offset of the '(' that starts the top-level form containing or preceding point."
  (let* ((s (buffer-string b)) (i (point-offset b)))
    (loop (cond ((< i 0) (return 0))
                ((and (char= (char s (min i (- (length s) 1))) #\() (or (= i 0) (char= (char s (- i 1)) #\Newline))) (return i))
                (t (setq i (- i 1)))))))
(defcmd beginning-of-defun (ed arg) (with-b (b ed) (push-mark ed) (goto-offset b (%defun-start b))))
(defcmd end-of-defun (ed arg)
  (with-b (b ed) (let* ((s (buffer-string b)) (e (sexp-end s (%defun-start b)))) (when e (goto-offset b e)))))

(defcmd indent-line (ed arg)
  (with-b (b ed)
    (multiple-value-bind (l1 c1 l2 c2) (region-bounds b)
      (declare (ignore c1 c2))
      (if (and l1 (/= l1 l2))
          (do ((l l1 (+ l 1))) ((> l l2)) (buf-indent-line b l))
          (let* ((line (point-line b)) (before (buf-line b line)) (col (point-col b)))
            (if (and (null (buf-indent-line b line)) (eq (buffer-mode b) 'lisp-mode) (> col 0)
                       (string= before (buf-line b line)) (%symbol-constituent-p (char before (- col 1)))
                       (or (>= col (length before)) (not (%symbol-constituent-p (char before col))))
                       (> (- col (%token-start-before before col #'%symbol-constituent-p)) 0)
                       (let ((ws (let ((i 0)) (loop (when (or (>= i (length before)) (not (member (char before i) '(#\Space #\Tab)))) (return i)) (setq i (+ i 1))))))
                         (> col ws)))
              ;; TAB on a line that is already indented, after a symbol: complete it
              (funcall (get 'complete-symbol '%ecommand) ed arg)
            (let* ((text (buf-line b line)) (ws (let ((i 0)) (loop (when (or (>= i (length text)) (not (member (char text i) '(#\Space #\Tab)))) (return i)) (setq i (+ i 1))))))
              (when (< (point-col b) ws) (buf-goto b line ws)))))))))
(defcmd newline-and-indent (ed arg)
  (with-b (b ed) (buf-insert b (string #\Newline)) (buf-indent-line b (point-line b))
    (let* ((line (point-line b)) (text (buf-line b line)))
      (buf-goto b line (let ((i 0)) (loop (when (or (>= i (length text)) (not (member (char text i) '(#\Space #\Tab)))) (return i)) (setq i (+ i 1))))))))
(defcmd indent-sexp (ed arg)
  (with-b (b ed) (let* ((s (buffer-string b)) (st (%defun-start b)) (e (sexp-end s st)))
                   (when e (let ((l1 (car (buf-pos-of-offset b st))) (l2 (car (buf-pos-of-offset b e))) (save (point-line b)))
                             (do ((l (+ l1 1) (+ l 1))) ((> l l2)) (buf-indent-line b l))
                             (buf-goto b (min save (- (buf-nlines b) 1)) 0))))))

(defun symbol-at-point (b)
  "The symbol-ish token around point as a string, or NIL."
  (let* ((line (buf-current-line b)) (c (point-col b)) (s c) (e c))
    (loop (when (or (<= s 0) (%delimiter-p (char line (- s 1)))) (return)) (setq s (- s 1)))
    (loop (when (or (>= e (length line)) (%delimiter-p (char line e))) (return)) (setq e (+ e 1)))
    (and (> e s) (subseq line s e))))

(defun %find-symbol-named (token)
  (let* ((tok (string-upcase token)) (colon (%string-position-char tok #\: 0)))
    (when (and colon (> colon 0)) (setq tok (subseq tok (+ colon (if (and (< (+ colon 1) (length tok)) (char= (char tok (+ colon 1)) #\:)) 2 1)))))
    (or (and (> (length tok) 0) (char= (char tok 0) #\:) (find-symbol (subseq tok 1) (find-package "KEYWORD")))
        (find-symbol tok (or *ed-package* *package*)) (find-symbol tok (find-package "COMMON-LISP")) (find-symbol tok (find-package "COMMON-LISP-USER")))))

(defun show-arglist-at-point (ed)
  "After a space typed following '(name', show the lambda list of NAME in the echo area."
  (with-b (b ed)
    (let* ((line (buf-current-line b)) (c (- (point-col b) 1)) (e c) (s c))
      (loop (when (or (<= s 0) (%delimiter-p (char line (- s 1)))) (return)) (setq s (- s 1)))
      (when (and (> s 0) (char= (char line (- s 1)) #\() (> e s))
        (let ((sym (%find-symbol-named (subseq line s e))))
          (when sym (let ((str (arglist-string sym))) (when str (ed-message* ed "~A" str)))))))))

(defun eval-string-in-editor (ed text)
  "Evaluate every form in TEXT; the message shows the last value (output is shown first)."
  (let ((out (make-string-output-stream)) (last nil) (pos 0) (*package* (or *ed-package* *package*)))
    (handler-case
        (let ((*standard-output* out))
          (loop (let ((rest (string-left-trim '(#\Space #\Newline #\Tab) (subseq text pos))))
                  (when (= (length rest) 0) (return))
                  (setq pos (- (length text) (length rest)))
                  (multiple-value-bind (form p) (%read-from-string text pos)
                    (when (eq form (%eof-object)) (return))
                    (setq pos p)
                    (setq last (multiple-value-list (eval form))))))
          (let ((o (get-output-stream-string out)))
            (ed-message* ed "~A~{~S~^, ~}" (if (> (length o) 0) (%string-concat (substitute #\Space #\Newline o) " => ") "=> ") last)))
      (error (e) (ed-message* ed "Error: ~A" e)))
    (when (not (eq *package* (or *ed-package* *package*))) nil)
    last))

(defcmd eval-last-sexp (ed arg)
  (with-b (b ed) (let* ((s (buffer-string b)) (off (point-offset b)) (st (sexp-start-before s off)))
                   (if st (eval-string-in-editor ed (subseq s st off)) (ed-message* ed "No sexp before point")))))
(defcmd eval-defun (ed arg)
  (with-b (b ed)
    (let* ((s (buffer-string b)) (st (%defun-start b)) (e (sexp-end s st)))
      (if e
          (progn (eval-string-in-editor ed (subseq s st e))
                 (buffer-note-compiled b (lambda (sec) (= (sec-start sec) st)))
                 (let ((form (ignore-errors (%read-from-string s st))))
                   (multiple-value-bind (name kind) (%definition-info form)
                     (when (and name (buffer-file b)) (%note-definition name kind (buffer-file b) st)
                           (ed-message* ed "~A  [~A defined]" (or (ed-message ed) "") (string-downcase (symbol-name name)))))))
          (ed-message* ed "No form at point")))))
(defcmd compile-defun (ed arg) (run-editor-command ed 'eval-defun nil))
(defcmd eval-region (ed arg)
  (with-b (b ed) (multiple-value-bind (l1 c1 l2 c2) (region-bounds b)
                   (if l1
                       (let ((o1 (buf-offset b l1 c1)) (o2 (buf-offset b l2 c2)))
                         (eval-string-in-editor ed (%raw-text b l1 c1 l2 c2))
                         (buffer-note-compiled b (lambda (sec) (and (<= o1 (sec-start sec)) (<= (sec-end sec) o2)))))
                       (ed-message* ed "No region")))))
(defcmd eval-buffer (ed arg)
  (with-b (b ed) (eval-string-in-editor ed (buffer-string b)) (buffer-reset-baselines b '(:compiled))))

;;; ---- commands: search and replace ---------------------------------------------------------------------------

(defcmd isearch-forward-regexp (ed arg) (isearch-start ed t t))
(defcmd isearch-backward-regexp (ed arg) (isearch-start ed nil t))
(defcmd query-replace (ed arg) (%read-replacement ed "Query replace" nil nil))
(defcmd query-replace-regexp (ed arg) (%read-replacement ed "Query replace regexp" t nil))
(defcmd replace-regexp (ed arg) (%read-replacement ed "Replace regexp" t t))

(defun %count-matches (b re from-line from-col)
  (let ((n 0) (l from-line) (c from-col))
    (loop (let ((p (buf-find b re l c)))
            (unless p (return n))
            (setq n (+ n 1) l (car p) c (if (= (cadr p) (caddr p)) (%after-char b l (caddr p)) (caddr p)))))))

(defcmd count-matches (ed arg)
  (ed-read-string ed "Count matches for regexp: "
                  (lambda (pat) (with-b (b ed) (let ((n (%count-matches b (search-regex pat t) (point-line b) (point-col b))))
                                                 (ed-message* ed "~D occurrence~:P" n))))))

(defcmd occur (ed arg)
  (ed-read-string ed "List lines matching regexp: "
                  (lambda (pat)
                    (let* ((b (ed-buffer ed)) (re (search-regex pat t)) (entries nil))
                      (dotimes (l (buf-nlines b))
                        (multiple-value-bind (ms) (regex-find re (buf-line b l))
                          (when ms
                            (let ((line l) (col ms) (buf b))
                              (setq entries (cons (make-possibility (format nil "~D: ~A" (+ l 1) (string-trim " " (buf-line b l)))
                                                                    (lambda (e) (ed-switch-to-buffer e buf) (buf-goto buf line col)))
                                                  entries))))))
                      (ed-set-possibilities ed (format nil "lines matching ~A" pat) (nreverse entries) :go nil)))))

;;; ---- commands: views ----------------------------------------------------------------------------------------

(defcmd split-window-below (ed arg) (ed-split-view ed :below))
(defcmd split-window-right (ed arg) (ed-split-view ed :right))
(defcmd other-window (ed arg)
  (let* ((vs (ed-views ed)) (i (position (ed-selected ed) vs)))
    (ed-select-view ed (nth (mod (+ i (%n arg)) (length vs)) vs))))
(defcmd delete-window (ed arg)
  (unless (ed-delete-view ed (ed-selected ed)) (ed-message* ed "Attempt to delete the only view")))
(defcmd delete-other-windows (ed arg) (setf (ed-layout ed) (ed-selected ed)))

(defun %other-view (ed)
  "Another view of ED (made by splitting if there is only one)."
  (let* ((vs (ed-views ed)) (i (position (ed-selected ed) vs)))
    (if (cdr vs) (nth (mod (+ i 1) (length vs)) vs) (ed-split-view ed :below))))

(defcmd scroll-other-window (ed arg)
  (let* ((o (%other-view ed)) (b (view-buffer o)) (step (if (integerp arg) arg (max 1 (- (view-height o) 2)))))
    (setf (view-top o) (max 0 (min (- (buf-nlines b) 1) (+ (view-top o) step))))
    (when (< (view-line o) (view-top o)) (setf (view-line o) (view-top o) (view-col o) 0))))
(defcmd switch-to-buffer-other-window (ed arg) (ed-select-view ed (%other-view ed)) (run-editor-command ed 'switch-to-buffer arg))
(defcmd find-file-other-window (ed arg) (ed-select-view ed (%other-view ed)) (run-editor-command ed 'find-file arg))

;;; ---- commands: keyboard macros ------------------------------------------------------------------------------

(defparameter *macro-repeat-limit* 10000 "Most repetitions of C-u 0 C-x e (repeat until an error).")

(defun execute-kbd-macro (ed keys &optional (times 1))
  "Replay KEYS in editor ED TIMES times (0: until a command fails), stopping at the first failing command."
  (let ((*ed-executing-macro* t) (*ed-command-failed* nil) (n 0))
    (loop
      (when (or (and (> times 0) (>= n times)) (>= n *macro-repeat-limit*)) (return))
      (dolist (k keys) (window-key ed k) (when *ed-command-failed* (return)))
      (when *ed-command-failed* (return))
      (setq n (+ n 1)))
    n))

(defcmd start-kbd-macro (ed arg)
  (if (ed-recording ed)
      (ed-message* ed "Already defining a keyboard macro")
      (progn (setf (ed-recording ed) (list :macro)) (ed-message* ed "Defining kbd macro..."))))

(defcmd end-kbd-macro (ed arg)
  (if (not (ed-recording ed))
      (ed-message* ed "Not defining a keyboard macro")
      (let ((keys (reverse (cdr (ed-recording ed)))) (n (length (ed-command-keys ed))))
        (setf (ed-macro ed) (subseq keys 0 (max 0 (- (length keys) n))) (ed-recording ed) nil)
        (ed-message* ed "Keyboard macro defined (~D key~:P)" (length (ed-macro ed))))))

(defcmd call-last-kbd-macro (ed arg)
  (cond ((ed-recording ed) (ed-message* ed "Finish the macro being defined first (C-x ))"))
        ((null (ed-macro ed)) (ed-message* ed "No keyboard macro defined"))
        (t (execute-kbd-macro ed (ed-macro ed) (if (integerp arg) arg 1))
           (setf (ed-macro-repeat ed) t)
           (unless (ed-message ed) (ed-message* ed "(Type e to repeat the macro)")))))

(defcmd kmacro-start-or-insert (ed arg) (run-editor-command ed 'start-kbd-macro arg))
(defcmd kmacro-end-or-call (ed arg)
  (if (ed-recording ed) (run-editor-command ed 'end-kbd-macro arg) (run-editor-command ed 'call-last-kbd-macro arg)))

(defcmd name-last-kbd-macro (ed arg)
  (if (null (ed-macro ed))
      (ed-message* ed "No keyboard macro defined")
      (ed-read-string ed "Name for last kbd macro: "
                      (lambda (name)
                        (let ((sym (intern (string-upcase (substitute #\- #\Space name)) "COMMON-LISP")) (keys (ed-macro ed)))
                          (%put sym '%ecommand (lambda (e a) (execute-kbd-macro e keys (if (integerp a) a 1))))
                          (unless (member sym *editor-commands*) (setq *editor-commands* (cons sym *editor-commands*)))
                          (ed-message* ed "M-x ~A runs the macro" (string-downcase (symbol-name sym))))))))

;;; ---- commands: completion -----------------------------------------------------------------------------------

(defun %symbol-constituent-p (c) (not (member c '(#\Space #\Tab #\Newline #\( #\) #\" #\; #\' #\` #\, #\#))))

(defun %token-start-before (line col pred)
  (let ((s col)) (loop (when (or (<= s 0) (not (funcall pred (char line (- s 1))))) (return s)) (setq s (- s 1)))))

(defun symbol-completions (prefix)
  "Completions of the symbol name PREFIX (which may start with a package prefix or a colon), in the case typed."
  (let* ((colon (position #\: prefix))
         (names (cond ((null colon) (complete-symbol-names prefix))
                      ((= colon 0)
                       (let ((up (string-upcase (subseq prefix 1))) (acc nil))
                         (dolist (s (%package-symbols (find-package "KEYWORD") nil))
                           (let ((nm (symbol-name s)))
                             (when (and (>= (length nm) (length up)) (string= (subseq nm 0 (length up)) up))
                               (setq acc (adjoin (%string-concat ":" (string-downcase nm)) acc :test #'string=)))))
                         (sort acc #'string<)))
                      (t (let* ((pname (subseq prefix 0 colon)) (pkg (find-package (string-upcase pname)))
                                (rest (string-left-trim ":" (subseq prefix colon)))
                                (sep (subseq prefix colon (- (length prefix) (length rest)))) (acc nil))
                           (when pkg
                             (dolist (s (%package-symbols pkg (= (length sep) 1)))
                               (let ((nm (symbol-name s)) (up (string-upcase rest)))
                                 (when (and (>= (length nm) (length up)) (string= (subseq nm 0 (length up)) up))
                                   (setq acc (adjoin (%string-concat pname sep (string-downcase nm)) acc :test #'string=))))))
                           (sort acc #'string<))))))
    (if (some #'upper-case-p prefix) (mapcar #'string-upcase names) names)))

(defun %show-candidates (ed cands)
  (ed-message* ed "~{~A~^  ~}~A" (subseq cands 0 (min 12 (length cands))) (if (> (length cands) 12) (format nil "  ... (~D)" (length cands)) "")))

(defcmd complete-symbol (ed arg)
  (with-b (b ed)
    (let* ((line (buf-current-line b)) (col (point-col b)) (start (%token-start-before line col #'%symbol-constituent-p))
           (prefix (subseq line start col)))
      (if (= (length prefix) 0)
          (ed-message* ed "Nothing to complete")
          (let ((cands (let ((*package* (or (ignore-errors (buffer-package-at b (point-offset b))) *ed-package* *package*)))
                         (symbol-completions prefix))))
            (cond ((null cands) (ed-message* ed "No match"))
                  ((null (cdr cands))
                   (if (string-equal (car cands) prefix) (ed-message* ed "Sole completion")
                       (progn (buf-delete b (point-line b) start (point-line b) col) (buf-insert b (car cands)))))
                  (t (let ((cp (common-prefix cands)))
                       (when (> (length cp) (length prefix))
                         (buf-delete b (point-line b) start (point-line b) col) (buf-insert b cp))
                       (%show-candidates ed cands)))))))))

(defun %dabbrev-word-p (lispy) (if lispy #'%symbol-constituent-p (lambda (c) (or (alphanumericp c) (char= c #\_) (>= (char-code c) 128)))))

(defun %dabbrev-candidates (ed b prefix pred)
  "Words starting with PREFIX: in B before point (nearest first), then after it, then in the other buffers."
  (let ((acc nil) (count 0) (n (length prefix)))
    (labels ((scan-line (text from-end)
               (let ((words nil) (i 0) (len (length text)))
                 (loop (when (>= i len) (return))
                       (if (funcall pred (char text i))
                           (let ((j i)) (loop (when (or (>= j len) (not (funcall pred (char text j)))) (return)) (setq j (+ j 1)))
                             (let ((w (subseq text i j)))
                               (when (and (> (length w) n) (string= (subseq w 0 n) prefix)) (setq words (cons w words))))
                             (setq i j))
                           (setq i (+ i 1))))
                 (dolist (w (if from-end words (reverse words)))
                   (unless (or (member w acc :test #'string=) (> count 200)) (setq acc (cons w acc) count (+ count 1)))))))
      (let ((l (point-line b)) (col (point-col b)))
        (scan-line (subseq (buf-line b l) 0 (%token-start-before (buf-line b l) col pred)) t)
        (do ((i (- l 1) (- i 1))) ((< i 0)) (scan-line (buf-line b i) t))
        (scan-line (subseq (buf-line b l) col) nil)
        (do ((i (+ l 1) (+ i 1))) ((>= i (buf-nlines b))) (scan-line (buf-line b i) nil))
        (dolist (other (ed-buffers ed))
          (unless (eq other b) (dotimes (i (min 5000 (buf-nlines other))) (scan-line (buf-line other i) nil))))))
    (nreverse acc)))

(defcmd dabbrev-expand (ed arg)
  (with-b (b ed)
    (let ((state (buf-local b 'dabbrev)) (lispy (eq (buffer-mode b) 'lisp-mode)))
      (if (and state (eq (ed-last-command ed) 'dabbrev-expand) (= (point-line b) (car state)))
          ;; again: replace the last expansion with the next one
          (destructuring-bind (line start prefix cands index) state
            (buf-delete b line (+ start (length prefix)) line (point-col b))
            (let ((i (+ index 1)))
              (if (< i (length cands))
                  (progn (buf-insert b (subseq (nth i cands) (length prefix))) (setf (buf-local b 'dabbrev) (list line start prefix cands i)))
                  (progn (setf (buf-local b 'dabbrev) nil) (ed-message* ed "No further expansions of ~A" prefix)))))
          (let* ((line (buf-current-line b)) (col (point-col b)) (pred (%dabbrev-word-p lispy))
                 (start (%token-start-before line col pred)) (prefix (subseq line start col)))
            (if (= (length prefix) 0)
                (ed-message* ed "Nothing to expand")
                (let ((cands (%dabbrev-candidates ed b prefix pred)))
                  (if (null cands)
                      (progn (setf (buf-local b 'dabbrev) nil) (ed-message* ed "No expansion for ~A" prefix))
                      (progn (buf-insert b (subseq (car cands) (length prefix)))
                             (setf (buf-local b 'dabbrev) (list (point-line b) start prefix cands 0)))))))))))

;;; ---- the arglist hint ------------------------------------------------------------------------------------------
;;; While point is inside a call in Lisp mode, the echo area shows the lambda list of the function with the
;;; parameter for the argument at point highlighted.

(defun %form-text-before-point (b)
  "The text from the start of the top-level form around point (a line starting with an open paren, at most
200 lines up) to point, and the line it starts on."
  (let ((l (point-line b)) (stop (max 0 (- (point-line b) 200))))
    (loop (when (or (<= l stop) (let ((text (buf-line b l))) (and (> (length text) 0) (char= (char text 0) #\()))) (return))
          (setq l (- l 1)))
    (values (%raw-text b l 0 (point-line b) (point-col b)) l)))

(defun %call-at-point (b)
  "The innermost call around point: (values operator-token argument-index argument-texts), or NIL."
  (let* ((text (%form-text-before-point b)) (len (length text)))
    (multiple-value-bind (stack in) (sexp-context text len)
      (when (and stack (null in))
        (let* ((open (car stack)) (hs (%skip-ws text (+ open 1))) (he (and (< hs len) (sexp-end text hs))))
          (when (and he (< he len))
            (let ((head (subseq text hs he)) (j he) (idx 0) (args nil))
              (loop
                (let ((k (%skip-ws text j)))
                  (when (>= k len) (return))
                  (let ((e (sexp-end text k)))
                    (when (or (null e) (>= e len)) (return))
                    (setq args (append args (list (subseq text k e))) idx (+ idx 1) j e))))
              (values head idx args))))))))

(defun %param-name (p) (let ((x (if (consp p) (car p) p))) (if (consp x) (cadr x) x)))
(defun %param-keyword (p) (let ((x (if (consp p) (car p) p))) (if (consp x) (symbol-name (car x)) (and (symbolp x) (symbol-name x)))))

(defun arglist-hint (sym lambda-list index args)
  "The lambda list of SYM as text, with the byte range of the parameter that receives argument INDEX (given
the argument texts ARGS so far): (values text start end)."
  (let ((parts nil) (len 0) (mode :req) (pos 0) (hs nil) (he nil) (*package* (or (symbol-package sym) *package*)))
    (flet ((add (s) (setq parts (cons s parts) len (+ len (length s))))
           (emit (x target)
             (setq parts (cons " " parts) len (+ len 1))
             (let ((s (string-downcase (princ-to-string x))))
               (when target (setq hs len he (+ len (length s))))
               (setq parts (cons s parts) len (+ len (length s))))))
      (add "(") (add (string-downcase (symbol-name sym)))
      (let ((items lambda-list) (skip-next nil))
        (loop
          (when (null items) (return))
          (let ((x (if (consp items) (car items) items)))
            (setq items (if (consp items) (cdr items) nil))
            (cond (skip-next (setq skip-next nil) (emit x nil))
                  ((member x '(&optional &rest &body &key &aux &allow-other-keys &whole &environment))
                   (emit x nil)
                   (case x (&optional (setq mode :opt)) ((&rest &body) (setq mode :rest)) (&key (setq mode :key))
                     (&aux (setq mode :aux)) ((&whole &environment) (setq skip-next t))))
                  ((member mode '(:req :opt)) (emit x (= pos index)) (setq pos (+ pos 1)))
                  ((eq mode :rest) (emit x (and (>= index pos) (not (member '&key lambda-list)))))
                  ((eq mode :key)
                   (let* ((k (- index pos)) (kw (and (>= k 1) (oddp k) (nth (+ pos k -1) args))))
                     (emit x (and kw (> (length kw) 1) (char= (char kw 0) #\:) (string-equal (subseq kw 1) (or (%param-keyword x) ""))))))
                  (t (emit x nil))))))
      (add ")"))
    (values (apply #'%string-concat (nreverse parts)) hs he)))

(defun ed-update-hint (w)
  "Recompute the arglist hint of W after a command."
  (setf (ed-hint w) nil)
  (let ((b (ed-buffer w)))
    (when (and b (eq (buffer-mode b) 'lisp-mode) (not (ed-minibuf w)) (not (ed-isearch w)) (not (ed-query w)) (not (ed-pending w))
               (not *ed-executing-macro*))
      (ignore-errors
        (multiple-value-bind (head index args) (%call-at-point b)
          (when head
            (let* ((*ed-package* (or (ignore-errors (buffer-package-at b (point-offset b))) *ed-package*))
                   (sym (%find-symbol-named head)))
              (when (and sym (or (fboundp sym) (macro-function sym)))
                (multiple-value-bind (ll found) (symbol-arglist sym)
                  (when found
                    (multiple-value-bind (text hs he) (arglist-hint sym ll index args)
                      (setf (ed-hint w) (list text hs he)))))))))))))

;;; ---- commands: Unicode and case ------------------------------------------------------------------------------

(defcmd what-cursor-position (ed arg)
  (with-b (b ed)
    (let* ((line (buf-current-line b)) (c (point-col b)) (off (point-offset b)) (total (length (buffer-string b))))
      (if (>= c (length line))
          (ed-message* ed "point=~D of ~D (~D%)  column=~D  [end of line]" off total (if (> total 0) (floor (* 100 off) total) 0)
                       (display-col line c))
          (let* ((end (char-end line c)) (codes (utf8-codes (subseq line c end))))
            (ed-message* ed "Char: ~A (~{U+~4,'0X~^ ~}, ~D byte~:P, width ~D)  point=~D of ~D  column=~D"
                         (let ((s (subseq line c end))) (if (< (char-code (char s 0)) 32) (format nil "^~C" (code-char (+ 64 (char-code (char s 0))))) s))
                         codes (reduce #'+ (mapcar #'utf8-byte-count codes)) (string-display-width (subseq line c end)) off total (display-col line c)))))))

(defun utf8-byte-count (code)
  "How many bytes the code point CODE takes in UTF-8."
  (cond ((< code #x80) 1) ((< code #x800) 2) ((< code #x10000) 3) (t 4)))

(defun %parse-code-point (text)
  (let* ((s (string-trim " " text))
         (s (cond ((and (> (length s) 2) (string-equal (subseq s 0 2) "U+")) (subseq s 2))
                  ((and (> (length s) 2) (string-equal (subseq s 0 2) "#x")) (subseq s 2))
                  ((and (> (length s) 2) (string-equal (subseq s 0 2) "0x")) (subseq s 2))
                  (t s)))
         (n (and (> (length s) 0) (every (lambda (c) (digit-char-p c 16)) s) (parse-integer s :radix 16))))
    (and n (< 0 n #x110000) (not (<= #xD800 n #xDFFF)) n)))

(defcmd insert-char (ed arg)
  (ed-read-string ed "Insert character (hexadecimal code point): "
                  (lambda (text)
                    (let ((code (%parse-code-point text)))
                      (if code
                          (dotimes (i (%n arg)) (buf-insert (ed-buffer ed) (utf8-from-codes (list code))))
                          (ed-message* ed "Not a code point: ~A" text))))))

(defun %case-region (ed fn)
  (with-b (b ed)
    (multiple-value-bind (l1 c1 l2 c2) (region-bounds b)
      (unless l1 (error "The mark is not set now, so there is no region"))
      (let ((text (buf-delete b l1 c1 l2 c2)))
        (buf-goto b l1 c1)
        (buf-insert b (funcall fn text))
        (setf (buffer-mark b) (cons l1 c1))))))
(defcmd upcase-region (ed arg) (%case-region ed #'utf8-upcase))
(defcmd downcase-region (ed arg) (%case-region ed #'utf8-downcase))
(defcmd capitalize-region (ed arg) (%case-region ed #'utf8-capitalize))

;;; ---- default key bindings ------------------------------------------------------------------------------------------------------------

(dolist (b '(("C-f" forward-char) ("<right>" forward-char) ("C-b" backward-char) ("<left>" backward-char)
             ("C-n" next-line) ("<down>" next-line) ("C-p" previous-line) ("<up>" previous-line)
             ("C-a" beginning-of-line) ("<home>" beginning-of-line) ("C-e" end-of-line) ("<end>" end-of-line)
             ("M-f" forward-word) ("M-b" backward-word) ("M-<" beginning-of-buffer) ("M->" end-of-buffer)
             ("C-v" scroll-up-command) ("<pgdn>" scroll-up-command) ("M-v" scroll-down-command) ("<pgup>" scroll-down-command)
             ("M-g g" goto-line) ("C-l" recenter) ("C-x C-x" exchange-point-and-mark) ("C-x h" mark-whole-buffer)
             ("<return>" newline) ("C-j" newline-and-indent) ("C-o" open-line) ("C-d" delete-char) ("<delete>" delete-char)
             ("<backspace>" delete-backward-char) ("C-k" kill-line) ("M-d" kill-word) ("M-<backspace>" backward-kill-word)
             ("C-w" kill-region) ("M-w" copy-region-as-kill) ("C-y" yank) ("M-y" yank-pop)
             ("C-SPC" set-mark-command) ("C-t" transpose-chars) ("M-u" upcase-word) ("M-l" downcase-word) ("M-c" capitalize-word)
             ("C-/" undo) ("C-_" undo) ("C-x u" undo) ("M-_" redo) ("M-r" redo) ("C-g" keyboard-quit) ("C-u" universal-argument)
             ("C-x r k" kill-rectangle) ("C-x r y" yank-rectangle) ("C-x r t" string-rectangle) ("C-x r o" open-rectangle)
             ("C-x r c" clear-rectangle) ("C-s" isearch-forward) ("C-r" isearch-backward) ("M-x" execute-extended-command)
             ("M-," pop-mark)
             ;; regular expressions and replacing
             ("C-M-s" isearch-forward-regexp) ("C-M-r" isearch-backward-regexp) ("M-%" query-replace) ("C-M-%" query-replace-regexp)
             ;; views
             ("C-x 2" split-window-below) ("C-x 3" split-window-right) ("C-x o" other-window) ("C-x 0" delete-window)
             ("C-x 1" delete-other-windows) ("C-M-v" scroll-other-window) ("C-x 4 b" switch-to-buffer-other-window)
             ("C-x 4 f" find-file-other-window) ("C-x 4 C-f" find-file-other-window)
             ;; keyboard macros
             ("C-x (" start-kbd-macro) ("C-x )" end-kbd-macro) ("C-x e" call-last-kbd-macro)
             ("<f3>" kmacro-start-or-insert) ("<f4>" kmacro-end-or-call)
             ;; completion, characters and case
             ("M-/" dabbrev-expand) ("C-x =" what-cursor-position) ("C-x 8 RET" insert-char)
             ("C-x C-u" upcase-region) ("C-x C-l" downcase-region)))
  (bind (car b) (cadr b)))

(dolist (b '(("<tab>" indent-line) ("<return>" newline-and-indent) ("C-M-f" forward-sexp) ("C-M-b" backward-sexp)
             ("C-M-u" backward-up-list) ("C-M-d" down-list) ("C-M-k" kill-sexp) ("C-M-a" beginning-of-defun)
             ("C-M-e" end-of-defun) ("C-M-q" indent-sexp) ("C-x C-e" eval-last-sexp) ("C-M-x" eval-defun)
             ("C-c C-c" compile-defun) ("C-c C-r" eval-region) ("C-c C-b" eval-buffer) ("M-." find-definition)
             ("C-M-n" up-list) ("M-<tab>" complete-symbol)))
  (bind-lisp (car b) (cadr b)))
(define-key *text-mode-map* "<tab>" 'indent-line)
