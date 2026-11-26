;;;; 00-buffer.lisp -- editor buffers: text, undo tree, kill ring, motion, rectangles.
;;;;
;;;; A buffer is a vector of lines (UTF-8 byte strings; columns are byte offsets, and character
;;;; motion steps over whole characters, keeping combining marks with the character before them).  EVERY edit goes through BUF-INSERT / BUF-DELETE, which
;;;; record the change in the UNDO TREE: undoing and then editing starts a new branch, and the old
;;;; branch stays reachable (UNDO-TREE-SELECT).  Positions are (line . column).

(in-package :cl)

(defstruct (unode (:conc-name un-) (:constructor make-unode (parent)) (:copier nil))
  parent (children nil) (ops nil) (redo-child nil) (id 0))

(defclass buffer ()
  ((name :initarg :name :accessor buffer-name)
   (file :initform nil :initarg :file :accessor buffer-file)
   (lines :initform (vector "") :accessor buffer-lines)
   (line :initform 0 :accessor point-line)
   (col :initform 0 :accessor point-col)
   (goal :initform nil :accessor buffer-goal)
   (mark :initform nil :accessor buffer-mark)
   (modified :initform nil :accessor buffer-modified)
   (mode :initform 'fundamental-mode :accessor buffer-mode)
   (minor-modes :initform nil :accessor buffer-minor-modes)
   (read-only :initform nil :accessor buffer-read-only)
   (undo-root :accessor buffer-undo-root)
   (undo-current :accessor buffer-undo-current)
   (undo-open :initform nil :accessor buffer-undo-open)
   (undo-count :initform 0 :accessor buffer-undo-count)
   (locals :initform nil :accessor buffer-locals)
   (backed-up :initform nil :accessor buffer-backed-up)))

(defmethod initialize-instance :after ((b buffer) &rest args)
  (declare (ignore args))
  (let ((root (make-unode nil)))
    (setf (buffer-undo-root b) root (buffer-undo-current b) root)))

(defmethod print-object ((b buffer) stream) (format stream "#<BUFFER ~A>" (buffer-name b)))

(defun make-buffer (name &optional text)
  (let ((b (make-instance 'buffer :name name)))
    (when text (buffer-set-text b text))
    b))

(defun buf-nlines (b) (length (buffer-lines b)))
(defun buf-line (b n) (aref (buffer-lines b) n))
(defun buf-current-line (b) (buf-line b (point-line b)))
(defun buf-local (b key &optional default) (let ((c (assq key (buffer-locals b)))) (if c (cdr c) default)))
(defun (setf buf-local) (v b key)
  (let ((c (assq key (buffer-locals b)))) (if c (rplacd c v) (setf (buffer-locals b) (cons (cons key v) (buffer-locals b))))) v)

(defun split-lines (string)
  (let ((acc nil) (start 0))
    (loop (let ((nl (%string-position-char string #\Newline start)))
            (setq acc (cons (subseq string start (or nl (length string))) acc))
            (if nl (setq start (+ nl 1)) (return))))
    (nreverse acc)))

(defun join-lines (lines) (%join-strings lines (string #\Newline)))

(defun buffer-string (b) (join-lines (coerce (buffer-lines b) 'list)))

(defun %note-change (b line)
  "Forget cached per-line information (the syntax state) after LINE, which has changed."
  (let ((v (buf-local b 'syntax-states)))
    (when (and v (> (fill-pointer v) (+ line 1))) (setf (fill-pointer v) (+ line 1)))))

(defun buffer-set-text (b text)
  "Replace the contents without recording undo (used when loading a file)."
  (%note-change b 0)
  (setf (buffer-lines b) (coerce (split-lines text) 'vector))
  (setf (point-line b) 0 (point-col b) 0 (buffer-mark b) nil (buffer-modified b) nil)
  b)

;;; ---- raw text operations ----------------------------------------------------------------------

(defun %raw-insert (b line col string)
  "Insert STRING at (LINE . COL); returns the end position."
  (%note-change b line)
  (let* ((parts (split-lines string)) (lines (buffer-lines b)) (cur (aref lines line))
         (prefix (subseq cur 0 col)) (suffix (subseq cur col)))
    (if (null (cdr parts))
        (progn (setf (aref lines line) (%string-concat prefix (car parts) suffix))
               (cons line (+ col (length (car parts)))))
        (let* ((n (length parts)) (new (list (%string-concat prefix (car parts)))))
          (dolist (m (butlast (cdr parts))) (setq new (append new (list m))))
          (setq new (append new (list (%string-concat (car (last parts)) suffix))))
          (setf (buffer-lines b)
                (coerce (append (coerce (subseq lines 0 line) 'list) new (coerce (subseq lines (+ line 1)) 'list)) 'vector))
          (cons (+ line n -1) (length (car (last parts))))))))

(defun %raw-delete (b l1 c1 l2 c2)
  "Delete from (L1 . C1) to (L2 . C2); returns the deleted text."
  (%note-change b l1)
  (let* ((lines (buffer-lines b)) (text (%raw-text b l1 c1 l2 c2)))
    (setf (aref lines l1) (%string-concat (subseq (aref lines l1) 0 c1) (subseq (aref lines l2) c2)))
    (when (> l2 l1)
      (setf (buffer-lines b)
            (coerce (append (coerce (subseq lines 0 (+ l1 1)) 'list) (coerce (subseq lines (+ l2 1)) 'list)) 'vector)))
    text))

(defun %raw-text (b l1 c1 l2 c2)
  (let ((lines (buffer-lines b)))
    (if (= l1 l2)
        (subseq (aref lines l1) c1 c2)
        (let ((parts (list (subseq (aref lines l1) c1))))
          (do ((i (+ l1 1) (+ i 1))) ((>= i l2)) (setq parts (append parts (list (aref lines i)))))
          (setq parts (append parts (list (subseq (aref lines l2) 0 c2))))
          (join-lines parts)))))

(defun %pos< (l1 c1 l2 c2) (or (< l1 l2) (and (= l1 l2) (< c1 c2))))

(defun %end-after-insert (line col string)
  (let ((parts (split-lines string)))
    (if (null (cdr parts)) (cons line (+ col (length (car parts)))) (cons (+ line (length parts) -1) (length (car (last parts)))))))

;;; ---- undo tree ------------------------------------------------------------------------------------

(defun buf-undo-boundary (b) (setf (buffer-undo-open b) nil))

(defun %open-undo-node (b)
  (or (buffer-undo-open b)
      (let* ((cur (buffer-undo-current b)) (node (make-unode cur)))
        (setf (un-id node) (setf (buffer-undo-count b) (+ 1 (buffer-undo-count b))))
        (setf (un-children cur) (cons node (un-children cur)))
        (setf (un-redo-child cur) node)
        (setf (buffer-undo-current b) node (buffer-undo-open b) node)
        node)))

(defun %record (b op) (let ((n (%open-undo-node b))) (setf (un-ops n) (cons op (un-ops n)))))

(defun %check-writable (b) (when (buffer-read-only b) (error "Buffer ~A is read-only" (buffer-name b))))

(defun buf-insert (b string)
  "Insert STRING at point (undoable); point moves to the end of the new text."
  (%check-writable b)
  (when (> (length string) 0)
    (let ((l (point-line b)) (c (point-col b)))
      (%record b (list :ins l c string))
      (let ((end (%raw-insert b l c string)))
        (setf (point-line b) (car end) (point-col b) (cdr end) (buffer-modified b) t (buffer-goal b) nil)
        (let ((m (buffer-mark b)))               ; keep the mark in place
          (when (and m (= (car m) l) (> (cdr m) c) (null (cdr (split-lines string))))
            (setf (buffer-mark b) (cons l (+ (cdr m) (length string)))))))))
  b)

(defun buf-delete (b l1 c1 l2 c2)
  "Delete the region between two positions (either order); returns the deleted text."
  (%check-writable b)
  (when (%pos< l2 c2 l1 c1) (psetq l1 l2 c1 c2 l2 l1 c2 c1))
  (if (and (= l1 l2) (= c1 c2))
      ""
      (let ((text (%raw-delete b l1 c1 l2 c2)))
        (%record b (list :del l1 c1 text))
        (setf (point-line b) l1 (point-col b) c1 (buffer-modified b) t (buffer-goal b) nil)
        (setf (buffer-mark b) nil)
        text)))

(defun %apply-inverse (b op)
  (case (car op)
    (:ins (let ((end (%end-after-insert (cadr op) (caddr op) (cadddr op))))
            (%raw-delete b (cadr op) (caddr op) (car end) (cdr end))
            (setf (point-line b) (cadr op) (point-col b) (caddr op))))
    (:del (let ((end (%raw-insert b (cadr op) (caddr op) (cadddr op))))
            (setf (point-line b) (car end) (point-col b) (cdr end))))))

(defun %apply-forward (b op)
  (case (car op)
    (:ins (let ((end (%raw-insert b (cadr op) (caddr op) (cadddr op))))
            (setf (point-line b) (car end) (point-col b) (cdr end))))
    (:del (let ((end (%end-after-insert (cadr op) (caddr op) (cadddr op))))
            (%raw-delete b (cadr op) (caddr op) (car end) (cdr end))
            (setf (point-line b) (cadr op) (point-col b) (caddr op))))))

(defun buf-undo (b)
  "Undo the most recent change group; returns NIL at the root of the tree."
  (buf-undo-boundary b)
  (let ((node (buffer-undo-current b)))
    (if (null (un-parent node))
        nil
        (progn (dolist (op (un-ops node)) (%apply-inverse b op))     ; ops are stored newest first
               (setf (un-redo-child (un-parent node)) node)
               (setf (buffer-undo-current b) (un-parent node) (buffer-modified b) t (buffer-mark b) nil)
               t))))

(defun buf-redo (b)
  (buf-undo-boundary b)
  (let* ((cur (buffer-undo-current b)) (child (un-redo-child cur)))
    (if (null child)
        nil
        (progn (dolist (op (reverse (un-ops child))) (%apply-forward b op))
               (setf (buffer-undo-current b) child (buffer-modified b) t (buffer-mark b) nil)
               t))))

(defun undo-tree-select (b n)
  "Make the Nth child of the current node the branch that REDO follows."
  (let* ((cur (buffer-undo-current b)) (kids (reverse (un-children cur))))
    (when (and (>= n 0) (< n (length kids))) (setf (un-redo-child cur) (nth n kids)) t)))

(defun undo-tree-lines (b)
  "A textual picture of the undo tree; * marks the current state."
  (let ((acc nil))
    (labels ((walk (node depth)
               (push (format nil "~A~A~A ~A" (make-string (* 2 depth) #\Space) (if (eq node (buffer-undo-current b)) "*" "o")
                             (if (un-parent node) (format nil "#~D" (un-id node)) "root")
                             (if (un-ops node) (%abbrev (car (last (un-ops node))) 40) ""))
                     acc)
               (dolist (k (reverse (un-children node))) (walk k (+ depth 1)))))
      (walk (buffer-undo-root b) 0))
    (nreverse acc)))

;;; ---- kill ring ---------------------------------------------------------------------------------------

(defvar *kill-ring* nil)
(defvar *kill-ring-max* 60)
(defvar *last-kill-command* nil)

(defun kill-new (text &optional append-to-last prepend)
  (if (and append-to-last *kill-ring*)
      (setf (car *kill-ring*) (if prepend (%string-concat text (car *kill-ring*)) (%string-concat (car *kill-ring*) text)))
      (progn (setq *kill-ring* (cons text *kill-ring*))
             (when (> (length *kill-ring*) *kill-ring-max*) (setq *kill-ring* (subseq *kill-ring* 0 *kill-ring-max*)))))
  text)

(defun current-kill (n) (and *kill-ring* (nth (mod n (length *kill-ring*)) *kill-ring*)))

;;; ---- positions and motion ----------------------------------------------------------------------------------

(defun cont-byte-p (line i)
  "Strings hold characters, not UTF-8 bytes: nothing is a continuation byte.  (Kept for old callers.)"
  (declare (ignore line i))
  nil)

(defun %zero-width-at-p (line i)
  (and (< i (length line)) (>= (char-code (char line i)) #xC0)
       (let ((code (utf8-char-at line i))) (and (>= code #x300) (= 0 (char-display-width code))))))

(defun char-end (line i)
  "Byte index after the character at I in LINE, with the combining marks that follow it."
  (let ((c (+ i 1)) (n (length line)))
    (loop (unless (cont-byte-p line c) (return)) (setq c (+ c 1)))
    (loop (unless (and (< c n) (%zero-width-at-p line c)) (return)) (setq c (nth-value 1 (utf8-char-at line c))))
    (min c n)))

(defun char-start (line i)
  "Byte index of the character (with its combining marks) that ends at I in LINE."
  (let ((c i))
    (loop
      (when (<= c 0) (return 0))
      (setq c (- c 1))
      (loop (unless (and (> c 0) (cont-byte-p line c)) (return)) (setq c (- c 1)))
      (unless (%zero-width-at-p line c) (return c)))))

(defun code-at (line i) "The code point at byte I of LINE." (values (utf8-char-at line i)))
(defun code-before (line i) "The code point of the character ending at byte I of LINE (ignoring combining marks)."
  (let ((c (- i 1))) (loop (unless (and (> c 0) (cont-byte-p line c)) (return)) (setq c (- c 1))) (code-at line c)))

(defun buf-forward-char (b &optional (n 1))
  (dotimes (i n)
    (let* ((line (buf-current-line b)) (c (point-col b)))
      (cond ((< c (length line)) (setf (point-col b) (char-end line c)))
            ((< (point-line b) (- (buf-nlines b) 1)) (setf (point-line b) (+ 1 (point-line b)) (point-col b) 0)))))
  (setf (buffer-goal b) nil))

(defun buf-backward-char (b &optional (n 1))
  (dotimes (i n)
    (let ((c (point-col b)))
      (cond ((> c 0) (setf (point-col b) (char-start (buf-current-line b) c)))
            ((> (point-line b) 0) (setf (point-line b) (- (point-line b) 1)) (setf (point-col b) (length (buf-current-line b)))))))
  (setf (buffer-goal b) nil))

(defun %char-cols (line i d)
  "Columns taken by the character at byte I of LINE when it starts at column D, and the byte after it."
  (let ((c (char-code (char line i))))
    (cond ((= c 9) (values (- (* 8 (+ 1 (floor d 8))) d) (+ i 1)))
          ((< c 32) (values 2 (+ i 1)))                          ; shown as ^X
          ((< c 128) (values 1 (+ i 1)))
          (t (multiple-value-bind (code next) (%utf8-decode line i) (values (char-display-width code) next))))))

(defun display-col (line col)
  "Screen column of byte offset COL in LINE: tabs advance to multiples of 8, wide characters take two columns,
combining marks none, control characters two (^X)."
  (let ((d 0) (i 0) (end (min col (length line))))
    (loop (when (>= i end) (return d))
          (multiple-value-bind (w next) (%char-cols line i d) (setq d (+ d w) i next)))))

(defun byte-col-for-display (line target)
  "Byte offset in LINE of the character at (or just after) screen column TARGET."
  (let ((i 0) (d 0) (n (length line)))
    (loop (when (or (>= i n) (>= d target)) (return i))
          (multiple-value-bind (w next) (%char-cols line i d) (setq d (+ d w) i next))
          (loop (unless (and (< i n) (%zero-width-at-p line i)) (return)) (setq i (nth-value 1 (utf8-char-at line i)))))))

(defun buf-move-vertically (b n)
  (let ((goal (or (buffer-goal b) (display-col (buf-current-line b) (point-col b)))))
    (let ((nl (max 0 (min (- (buf-nlines b) 1) (+ (point-line b) n)))))
      (setf (point-line b) nl (point-col b) (byte-col-for-display (buf-line b nl) goal))
      (setf (buffer-goal b) goal))))

(defun word-char-p (code) (unicode-word-char-p code))

(defun buf-forward-word (b)
  (let ((moved nil))
    (loop
      (let* ((line (buf-current-line b)) (c (point-col b)))
        (cond ((>= c (length line)) (if (< (point-line b) (- (buf-nlines b) 1)) (buf-forward-char b) (return)))
              ((word-char-p (code-at line c)) (return))
              (t (buf-forward-char b)))))
    (loop (let* ((line (buf-current-line b)) (c (point-col b)))
            (if (and (< c (length line)) (word-char-p (code-at line c))) (progn (buf-forward-char b) (setq moved t)) (return))))
    moved))

(defun buf-backward-word (b)
  (loop
    (let* ((line (buf-current-line b)) (c (point-col b)))
      (cond ((<= c 0) (if (> (point-line b) 0) (buf-backward-char b) (return)))
            ((word-char-p (code-before line c)) (return))
            (t (buf-backward-char b)))))
  (loop (let* ((line (buf-current-line b)) (c (point-col b)))
          (if (and (> c 0) (word-char-p (code-before line c))) (buf-backward-char b) (return)))))

(defun buf-goto (b line col)
  (let ((l (max 0 (min line (- (buf-nlines b) 1)))))
    (setf (point-line b) l (point-col b) (max 0 (min col (length (buf-line b l)))) (buffer-goal b) nil)))

(defun buf-offset (b line col)
  (let ((off 0)) (dotimes (i line) (setq off (+ off (length (buf-line b i)) 1))) (+ off col)))

(defun buf-pos-of-offset (b offset)
  (let ((l 0) (n (buf-nlines b)))
    (loop (let ((len (+ 1 (length (buf-line b l)))))
            (if (and (>= offset len) (< l (- n 1))) (progn (setq offset (- offset len)) (setq l (+ l 1)))
                (return (cons l (min offset (length (buf-line b l))))))))))

(defun point-offset (b) (buf-offset b (point-line b) (point-col b)))
(defun goto-offset (b offset) (let ((p (buf-pos-of-offset b offset))) (buf-goto b (car p) (cdr p))))

(defun buf-char-after (b) (let ((line (buf-current-line b)) (c (point-col b))) (and (< c (length line)) (char line c))))
(defun buf-char-before (b) (let ((line (buf-current-line b)) (c (point-col b))) (and (> c 0) (char line (- c 1)))))

(defun region-bounds (b)
  "Returns (values l1 c1 l2 c2) ordered, or NIL when there is no mark."
  (let ((m (buffer-mark b)))
    (when m
      (if (%pos< (car m) (cdr m) (point-line b) (point-col b))
          (values (car m) (cdr m) (point-line b) (point-col b))
          (values (point-line b) (point-col b) (car m) (cdr m))))))

;;; ---- search ---------------------------------------------------------------------------------------------------

(defun buf-search-forward (b string &optional (from-line (point-line b)) (from-col (point-col b)))
  "Position (line . col) of the next occurrence at or after FROM, or NIL."
  (let ((l from-line) (c from-col) (n (buf-nlines b)))
    (loop (when (>= l n) (return nil))
          (let ((p (%string-search string (buf-line b l) c)))
            (when p (return (cons l p))))
          (setq l (+ l 1) c 0))))

(defun buf-search-backward (b string &optional (from-line (point-line b)) (from-col (point-col b)))
  "Position of the last occurrence that ends at or before FROM."
  (let ((l from-line) (limit from-col))
    (loop (when (< l 0) (return nil))
          (let* ((line (buf-line b l)) (best nil) (start 0))
            (loop (let ((p (%string-search string line start)))
                    (if (and p (<= (+ p (length string)) limit)) (progn (setq best p) (setq start (+ p 1))) (return))))
            (when best (return (cons l best))))
          (setq l (- l 1))
          (when (>= l 0) (setq limit (length (buf-line b l)))))))

;;; ---- rectangles ----------------------------------------------------------------------------------------------------

(defvar *killed-rectangle* nil)

(defun rect-bounds (b)
  "(values top bottom left-display-col right-display-col) of the rectangle between mark and point."
  (let ((m (buffer-mark b)))
    (unless m (error "The mark is not set"))
    (let* ((l1 (min (car m) (point-line b))) (l2 (max (car m) (point-line b)))
           (c1 (display-col (buf-line b (car m)) (cdr m))) (c2 (display-col (buf-current-line b) (point-col b))))
      (values l1 l2 (min c1 c2) (max c1 c2)))))

(defun rect-lines (b)
  (multiple-value-bind (top bottom left right) (rect-bounds b)
    (let ((acc nil))
      (do ((l top (+ l 1))) ((> l bottom))
        (let* ((line (buf-line b l)) (s (byte-col-for-display line left)) (e (byte-col-for-display line right)))
          (setq acc (cons (subseq line s (max s e)) acc))))
      (nreverse acc))))

(defun buf-kill-rectangle (b)
  (multiple-value-bind (top bottom left right) (rect-bounds b)
    (setq *killed-rectangle* (rect-lines b))
    (let ((cuts nil))
      (do ((l bottom (- l 1))) ((< l top))
        (let* ((line (buf-line b l)) (s (byte-col-for-display line left)) (e (max s (byte-col-for-display line right))))
          (setq cuts (cons (list l s e) cuts))))
      (dolist (cut (reverse cuts)) (buf-delete b (car cut) (cadr cut) (car cut) (caddr cut)))
      (buf-goto b top (byte-col-for-display (buf-line b top) left))
      (setf (buffer-mark b) nil))
    *killed-rectangle*))

(defun buf-yank-rectangle (b)
  (let ((line (point-line b)) (col (display-col (buf-current-line b) (point-col b))))
    (dolist (text *killed-rectangle*)
      (when (>= line (buf-nlines b)) (buf-goto b (- (buf-nlines b) 1) (length (buf-line b (- (buf-nlines b) 1)))) (buf-insert b (string #\Newline)))
      (let* ((l (buf-line b line)) (w (display-col l (length l))))
        (when (< w col) (buf-goto b line (length l)) (buf-insert b (make-string (- col w) #\Space)))
        (buf-goto b line (byte-col-for-display (buf-line b line) col))
        (buf-insert b text))
      (setq line (+ line 1)))))

(defun buf-string-rectangle (b string)
  (multiple-value-bind (top bottom left right) (rect-bounds b)
    (do ((l bottom (- l 1))) ((< l top))
      (let* ((line (buf-line b l)) (s (byte-col-for-display line left)) (e (max s (byte-col-for-display line right))))
        (when (> e s) (buf-delete b l s l e))
        (buf-goto b l s)
        (buf-insert b string)))
    (setf (buffer-mark b) nil)))

(defun buf-open-rectangle (b)
  (multiple-value-bind (top bottom left right) (rect-bounds b)
    (let ((w (- right left)))
      (do ((l bottom (- l 1))) ((< l top))
        (let ((s (byte-col-for-display (buf-line b l) left)))
          (buf-goto b l s) (buf-insert b (make-string w #\Space))))
      (setf (buffer-mark b) nil))))

(defun buf-clear-rectangle (b)
  (multiple-value-bind (top bottom left right) (rect-bounds b)
    (do ((l bottom (- l 1))) ((< l top))
      (let* ((line (buf-line b l)) (s (byte-col-for-display line left)) (e (max s (byte-col-for-display line right))))
        (when (> e s) (buf-delete b l s l e) (buf-goto b l s) (buf-insert b (make-string (- right left) #\Space)))))
    (setf (buffer-mark b) nil)))

;;; ---- sexp scanning (Lisp syntax on the flat buffer text) ---------------------------------------------------------------

(defun %delimiter-p (c) (member c '(#\Space #\Tab #\Newline #\( #\) #\" #\; #\')))

(defun %skip-ws (s i)
  "Skip whitespace and comments starting at I."
  (let ((n (length s)))
    (loop
      (cond ((>= i n) (return i))
            ((member (char s i) '(#\Space #\Tab #\Newline #\Return)) (setq i (+ i 1)))
            ((char= (char s i) #\;) (loop (when (or (>= i n) (char= (char s i) #\Newline)) (return)) (setq i (+ i 1))))
            ((and (char= (char s i) #\#) (< (+ i 1) n) (char= (char s (+ i 1)) #\|))
             (let ((depth 1)) (setq i (+ i 2))
               (loop (when (or (>= i n) (<= depth 0)) (return))
                     (cond ((and (char= (char s i) #\|) (< (+ i 1) n) (char= (char s (+ i 1)) #\#)) (setq depth (- depth 1)) (setq i (+ i 2)))
                           ((and (char= (char s i) #\#) (< (+ i 1) n) (char= (char s (+ i 1)) #\|)) (setq depth (+ depth 1)) (setq i (+ i 2)))
                           (t (setq i (+ i 1)))))))
            (t (return i))))))

(defun sexp-end (s i)
  "Offset just after the sexp that starts at or after I, or NIL if none."
  (let* ((n (length s)) (i (%skip-ws s i)))
    (when (< i n)
      (let ((c (char s i)))
        (cond ((char= c #\() (list-end s (+ i 1)))
              ((char= c #\)) nil)
              ((char= c #\") (let ((j (+ i 1)))
                               (loop (cond ((>= j n) (return nil)) ((char= (char s j) #\\) (setq j (+ j 2)))
                                           ((char= (char s j) #\") (return (+ j 1))) (t (setq j (+ j 1)))))))
              ((member c '(#\' #\` #\,)) (sexp-end s (+ i (if (and (char= c #\,) (< (+ i 1) n) (char= (char s (+ i 1)) #\@)) 2 1))))
              ((and (char= c #\#) (< (+ i 1) n) (member (char s (+ i 1)) '(#\' #\( #\: #\+ #\-)))
               (if (char= (char s (+ i 1)) #\() (list-end s (+ i 2)) (sexp-end s (+ i 2))))
              ((and (char= c #\#) (< (+ i 1) n) (char= (char s (+ i 1)) #\\))
               (let ((j (+ i 3))) (loop (when (or (>= j n) (%delimiter-p (char s j))) (return j)) (setq j (+ j 1)))))
              ((char= c #\|) (let ((j (+ i 1))) (loop (cond ((>= j n) (return nil)) ((char= (char s j) #\|) (return (+ j 1))) (t (setq j (+ j 1)))))))
              (t (let ((j i)) (loop (when (or (>= j n) (%delimiter-p (char s j))) (return j)) (setq j (+ j 1))))))))))

(defun list-end (s i)
  "Offset after the closing paren of the list whose contents start at I."
  (let ((n (length s)))
    (loop
      (let ((j (%skip-ws s i)))
        (cond ((>= j n) (return nil))
              ((char= (char s j) #\)) (return (+ j 1)))
              (t (let ((e (sexp-end s j))) (if e (setq i e) (return nil)))))))))

(defun sexp-context (s limit)
  "Scan S from the start up to LIMIT.  Returns (values stack in-string-or-comment) where STACK is the
list of offsets of the unclosed open parens (innermost first)."
  (let ((stack nil) (i 0) (in nil))
    (loop
      (when (>= i limit) (return))
      (let ((c (char s i)))
        (cond ((char= c #\;) (let ((j i)) (loop (when (or (>= j limit) (char= (char s j) #\Newline)) (return)) (setq j (+ j 1)))
                                 (when (>= j limit) (setq in :comment)) (setq i j)))
              ((char= c #\") (let ((j (+ i 1)))
                               (loop (cond ((>= j limit) (setq in :string) (return)) ((char= (char s j) #\\) (setq j (+ j 2)))
                                           ((char= (char s j) #\") (setq j (+ j 1)) (return)) (t (setq j (+ j 1)))))
                               (setq i j)))
              ((and (char= c #\#) (< (+ i 2) limit) (char= (char s (+ i 1)) #\\)) (setq i (+ i 3)))
              ((and (char= c #\#) (< (+ i 1) limit) (char= (char s (+ i 1)) #\|))
               (let ((j (%skip-ws s i))) (setq i (if (> j i) j (+ i 2)))))
              ((char= c #\() (setq stack (cons i stack)) (setq i (+ i 1)))
              ((char= c #\)) (setq stack (cdr stack)) (setq i (+ i 1)))
              (t (setq i (+ i 1))))))
    (values stack in)))

(defun sexp-start-before (s i)
  "Start offset of the sexp that ends at or before I (backward-sexp), or NIL."
  (multiple-value-bind (stack in) (sexp-context s i)
    (declare (ignore in))
    (let ((j (if stack (+ (car stack) 1) 0)) (last nil))
      (loop
        (let ((k (%skip-ws s j)))
          (when (>= k i) (return))
          (let ((e (sexp-end s k)))
            (when (or (null e) (> e i)) (return))
            (setq last k) (setq j e))))
      last)))

(defun %list-start-column (b off)
  (let ((p (buf-pos-of-offset b off))) (display-col (buf-line b (car p)) (cdr p))))

(defparameter *indent-specs*
  '((defun . 2) (defmacro . 2) (defmethod . 2) (defgeneric . 2) (destructuring-bind . 2) (multiple-value-bind . 2)
    (define-condition . 2) (defclass . 2) (deftest . 1) (let . 1) (let* . 1) (flet . 1) (labels . 1) (macrolet . 1)
    (when . 1) (unless . 1) (dolist . 1) (dotimes . 1) (lambda . 1) (block . 1) (case . 1) (ecase . 1) (typecase . 1)
    (handler-case . 1) (handler-bind . 1) (restart-case . 1) (unwind-protect . 1) (catch . 1) (with-open-file . 1)
    (with-slots . 2) (with-accessors . 2) (with-output-to-string . 1) (defvar . 1) (defparameter . 1) (defstruct . 1)
    (progn . 0) (loop . 0) (prog1 . 1) (eval-when . 1) (symbol-macrolet . 1) (do . 2) (do* . 2)))

(defun lisp-indent-for-line (b line)
  "Column LINE of buffer B should be indented to."
  (let* ((s (buffer-string b)) (ls (buf-offset b line 0)))
    (multiple-value-bind (stack in) (sexp-context s ls)
      (cond ((eq in :string) nil)                        ; inside a string: leave alone
            ((null stack) 0)
            (t (let* ((open (car stack)) (ocol (%list-start-column b open))
                      (els nil) (j (+ open 1)))
                 (loop (let ((k (%skip-ws s j)))
                         (when (>= k ls) (return))
                         (let ((e (sexp-end s k))) (if (null e) (return) (progn (setq els (append els (list k))) (setq j e))))))
                 (cond ((null els) (+ ocol 1))
                       (t (let* ((head-start (car els)) (head-end (or (sexp-end s head-start) head-start))
                                 (head (subseq s head-start head-end)))
                            (if (or (char= (char head 0) #\() (char= (char head 0) #\") (digit-char-p (char head 0)) (char= (char head 0) #\'))
                                (%list-start-column b head-start)
                                (let* ((spec (cdr (assq (intern (string-upcase head) "COMMON-LISP") *indent-specs*))) (nargs (- (length els) 1)))
                                  (cond (spec (if (< nargs spec) (+ ocol 4) (+ ocol 2)))
                                        ((>= nargs 1) (%list-start-column b (cadr els)))
                                        (t (+ ocol 2))))))))))))))

(defun buf-indent-line (b line)
  "Re-indent LINE according to Lisp conventions; returns T if something changed."
  (let* ((text (buf-line b line)) (col (lisp-indent-for-line b line)))
    (when col
      (let* ((ws (let ((i 0)) (loop (when (or (>= i (length text)) (not (member (char text i) '(#\Space #\Tab)))) (return i)) (setq i (+ i 1))))))
        (unless (and (= ws col) (every (lambda (c) (char= c #\Space)) (subseq text 0 ws)))
          (buf-delete b line 0 line ws)
          (buf-goto b line 0)
          (buf-insert b (make-string col #\Space))
          t)))))
