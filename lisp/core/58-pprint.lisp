;;;; 58-pprint.lisp -- the pretty printer: PPRINT and logical blocks.
;;;;
;;;; PPRINT fills to *PRINT-RIGHT-MARGIN*: forms that fit are printed flat; longer lists
;;;; break one element per line, aligned under the first argument (calls), with a
;;;; two-space body indent for definition/binding forms, or under the first
;;;; element (data lists).
;;;;
;;;; PPRINT-LOGICAL-BLOCK (and FORMAT's ~<...~:>) collects its output, with the conditional newlines
;;;; (PPRINT-NEWLINE), indentation changes (PPRINT-INDENT) and tabs (PPRINT-TAB) in it, into a tree of blocks
;;;; on a %PRETTY-STREAM; when the outermost block ends the tree is laid out to the real stream.  A block that
;;;; fits on the rest of the line is printed flat; otherwise its linear newlines break, a fill newline breaks
;;;; when the next section would not fit (or the previous one did not), a miser newline breaks like a linear
;;;; one when less than *PRINT-MISER-WIDTH* columns are left, and a mandatory newline always breaks (and keeps
;;;; the blocks around it from being flat).

(in-package :cl)

(defvar *print-right-margin* 80)
(defvar *print-length* nil "If an integer, logical blocks print at most this many elements of a list (then \"...\").")
(defvar *print-pretty* nil "When true, PRIN1, PRINC, PRINT, WRITE and the string printers lay out lists with the pretty printer (as PPRINT).")
(defvar *print-base* 10 "The radix (2-36) in which integers and ratios are printed.")
(defvar *print-radix* nil "When true, integers and ratios are printed with their radix: #x1F, #b101, #o17, #3r12; decimal integers end in a point.")
(defvar *pp-escape* t "Whether the pretty printer prints with escape characters (as PRIN1) or without (as PRINC).")

(defun %pp-flat (x) (if *pp-escape* (prin1-to-string x) (princ-to-string x)))

(defun %pp-hook (obj escape stream)
  "Called by the printer when *PRINT-PRETTY* is true: OBJ (a list) laid out from STREAM's current column."
  (let ((*print-pretty* nil) (*pp-escape* escape))
    (%pp-string obj (if stream (%stream-column stream) 0) (or *print-right-margin* 80))))

(defun write (obj &key stream (escape *print-escape*) (base *print-base*) (radix *print-radix*)
                    (pretty *print-pretty*) (length *print-length*) (right-margin *print-right-margin*)
                    &allow-other-keys)
  "Print OBJ to STREAM, binding the printer variables named by the keywords :ESCAPE :BASE :RADIX :PRETTY :LENGTH :RIGHT-MARGIN."
  (let ((*print-escape* escape) (*print-base* base) (*print-radix* radix)
        (*print-pretty* pretty) (*print-length* length) (*print-right-margin* right-margin))
    (if escape (prin1 obj stream) (princ obj stream)))
  obj)

(defun write-to-string (obj &rest keys)
  "Print OBJ to a string; the keywords are those of WRITE (except :STREAM)."
  (with-output-to-string (s) (apply #'write obj :stream s keys)))

(defparameter *pp-body-ops*
  '(defun defmacro defvar defparameter defconstant defstruct defclass defgeneric defmethod
    lambda let let* flet labels macrolet symbol-macrolet when unless dolist dotimes block progn
    prog1 handler-case handler-bind restart-case destructuring-bind multiple-value-bind
    with-open-file with-slots with-accessors with-output-to-string unwind-protect catch loop do do*
    case ecase typecase etypecase define-condition deftest eval-when))

(defun %proper-list-p (x) (and (listp x) (>= (list-length-safe x) 0)))
(defun list-length-safe (x)
  (let ((n 0))
    (loop (cond ((null x) (return n)) ((atom x) (return -1)) (t (setq x (cdr x)) (setq n (+ n 1)))))))

(defun %pp-indent (out n) (dotimes (i n) (write-char #\Space out)))

(defun %pp-distinguished (head)
  (case head
    ((defun defmacro defmethod defgeneric destructuring-bind multiple-value-bind define-condition deftest) 2)
    ((progn prog1 block catch unwind-protect loop eval-when) 0)
    (t 1)))

(defun %pp-string (obj col width)
  (let ((flat (%pp-flat obj)))
    (cond ((or (atom obj) (<= (+ col (length flat)) width) (not (%proper-list-p obj))) flat)
          ((and (symbolp (car obj)) (member (car obj) *pp-body-ops*)) (%pp-body obj col width))
          (t (%pp-call obj col width flat)))))

(defun %pp-call (obj col width flat)
  (let ((head (car obj)) (args (cdr obj)))
    (cond ((null args) flat)
          ((symbolp head)
           (let* ((hs (%pp-flat head)) (acol (+ col 2 (length hs))))
             (with-output-to-string (o)
               (write-string "(" o) (write-string hs o) (write-char #\Space o)
               (write-string (%pp-string (car args) acol width) o)
               (dolist (a (cdr args))
                 (terpri o) (%pp-indent o acol)
                 (write-string (%pp-string a acol width) o))
               (write-char #\) o))))
          (t (with-output-to-string (o)
               (write-char #\( o)
               (write-string (%pp-string head (+ col 1) width) o)
               (dolist (a args)
                 (terpri o) (%pp-indent o (+ col 1))
                 (write-string (%pp-string a (+ col 1) width) o))
               (write-char #\) o))))))

(defun %pp-body (obj col width)
  (let* ((head (car obj)) (hs (%pp-flat head)) (nd (%pp-distinguished head)) (n 0) (icol (+ col 2)))
    (with-output-to-string (o)
      (write-string "(" o) (write-string hs o)
      (dolist (a (cdr obj))
        (if (< n nd)
            (progn (write-char #\Space o) (write-string (%pp-string a (+ col 2 (length hs)) width) o))
            (progn (terpri o) (%pp-indent o icol) (write-string (%pp-string a icol width) o)))
        (setq n (+ n 1)))
      (write-char #\) o))))

(defun pprint (obj &optional stream)
  (let ((s (%out stream)) (*print-pretty* nil) (*pp-escape* t))
    (terpri s)
    (write-string (%pp-string obj (%stream-column s) *print-right-margin*) s)
    (values)))

(defun pretty-string (obj &optional (width *print-right-margin*))
  (let ((*print-pretty* nil) (*pp-escape* t)) (%pp-string obj 0 width)))

;;; ---- logical blocks ----------------------------------------------------------------------------------

(defvar *print-miser-width* 40 "When a logical block starts this close to the right margin (or closer), its fill and miser newlines act as linear ones.")

(defstruct (%ppblock (:constructor %make-ppblock (prefix per-line-prefix suffix)) (:copier nil))
  prefix per-line-prefix suffix (items nil) (flat nil))   ; ITEMS newest first while collecting

(defclass %pretty-stream (fundamental-character-output-stream)
  ((target :initarg :target)
   (stack :initform nil)))            ; open blocks, innermost first
(defun %pps-stack (s) (slot-value s 'stack))

(defun %pps-add (s item)
  (let ((b (car (%pps-stack s)))) (setf (%ppblock-items b) (cons item (%ppblock-items b)))))

(defun %pps-text (s string)
  (let ((start 0) (n (length string)))
    (loop
      (let ((nl (position #\Newline string :start start)))
        (when (> (or nl n) start) (%pps-add s (subseq string start (or nl n))))
        (unless nl (return))
        (%pps-add s (list :newline :literal))
        (setq start (+ nl 1))))))

(defmethod stream-write-char ((s %pretty-stream) ch) (%pps-text s (string ch)) ch)
(defmethod stream-write-string ((s %pretty-stream) string &optional start end)
  (%pps-text s (subseq string (or start 0) (or end (length string)))) string)
(defmethod stream-line-column ((s %pretty-stream)) nil)

(defun %pp-width (s) (if (fboundp 'string-display-width) (funcall 'string-display-width s) (length s)))

(defun %ppblock-flat-width (b)
  "Width of block B printed on one line, or NIL if it contains a mandatory newline."
  (or (%ppblock-flat b)
      (let ((w (+ (%pp-width (or (%ppblock-per-line-prefix b) (%ppblock-prefix b))) (%pp-width (%ppblock-suffix b)))))
        (dolist (it (%ppblock-items b))
          (cond ((null w) (return))
                ((stringp it) (setq w (+ w (%pp-width it))))
                ((%ppblock-p it) (let ((iw (%ppblock-flat-width it))) (setq w (and iw (+ w iw)))))
                ((and (eq (car it) :newline) (member (cadr it) '(:mandatory :literal))) (setq w nil))))
        (setf (%ppblock-flat b) (or w :broken))))
  (let ((v (%ppblock-flat b))) (if (eq v :broken) nil v)))

(defun %pp-section-width (items)
  "Flat width of ITEMS up to the next conditional newline of the same block (NIL if it cannot be flat)."
  (let ((w 0))
    (dolist (it items w)
      (cond ((stringp it) (setq w (+ w (%pp-width it))))
            ((%ppblock-p it) (let ((iw (%ppblock-flat-width it))) (if iw (setq w (+ w iw)) (return nil))))
            ((eq (car it) :newline) (return w))))))

(defvar *pp-col* 0 "The column reached while laying out a logical block.")
(defvar *pp-lines* 0 "Lines broken so far while laying out a logical block.")
(defvar *pp-margin* 80 "The right margin of the logical block being laid out.")
(defvar *pp-line-prefixes* nil "(column . prefix) for the per-line prefixes of the blocks being laid out, innermost first.")

(defvar *pp-line* nil "The line being laid out (a string output stream): trailing spaces are dropped at a break.")

(defun %pp-out (out s)
  (write-string s (or *pp-line* out))
  (setq *pp-col* (+ *pp-col* (%pp-width s))))

(defun %pp-flush-line (out &optional trim)
  (when *pp-line*
    (let ((text (get-output-stream-string *pp-line*)))
      (write-string (if trim (string-right-trim " " text) text) out))))
(defun %pp-spaces (out n) (when (> n 0) (%pp-out out (make-string n :initial-element #\Space))))

(defun %pp-break (out indent)
  (%pp-flush-line out t)
  (terpri out)
  (setq *pp-col* 0 *pp-lines* (+ *pp-lines* 1))
  (dolist (lp (reverse *pp-line-prefixes*))
    (%pp-spaces out (- (car lp) *pp-col*))
    (%pp-out out (cdr lp)))
  (%pp-spaces out (- indent *pp-col*)))

(defun %pp-lay-block (b out)
  (let* ((plp (%ppblock-per-line-prefix b))
         (prefix-col *pp-col*)
         (*pp-line-prefixes* (if plp (cons (cons prefix-col plp) *pp-line-prefixes*) *pp-line-prefixes*)))
    (%pp-out out (or plp (%ppblock-prefix b)))
    (let* ((start *pp-col*) (indent start) (items (reverse (%ppblock-items b)))
           (flat (%ppblock-flat-width b))
           (fits (and flat (<= (+ prefix-col flat) *pp-margin*)))
           (miser (and *print-miser-width* (<= (- *pp-margin* start) *print-miser-width*)))
           (lines-at-section *pp-lines*))
      (loop
        (when (null items) (return))
        (let ((it (pop items)))
          (cond ((stringp it) (%pp-out out it))
                ((%ppblock-p it) (%pp-lay-block it out))
                ((eq (car it) :newline)
                 (let* ((kind (cadr it))
                        (brk (case kind
                               ((:mandatory :literal) t)
                               (:linear (not fits))
                               (:miser (and miser (not fits)))
                               (:fill (and (not fits)
                                           (or (/= *pp-lines* lines-at-section)
                                               (and miser t)
                                               (let ((w (%pp-section-width items))) (or (null w) (> (+ *pp-col* w) *pp-margin*))))))
                               (t nil))))
                   (when brk (%pp-break out (if (and miser (not (member kind '(:mandatory :literal)))) start indent)))
                   (setq lines-at-section *pp-lines*)))
                ((eq (car it) :indent)
                 (unless miser (setq indent (max 0 (if (eq (cadr it) :block) (+ start (caddr it)) (+ *pp-col* (caddr it)))))))
                ((eq (car it) :tab)
                 (destructuring-bind (kind colnum colinc) (cdr it)
                   (let* ((base (if (member kind '(:section :section-relative)) start 0))
                          (col (- *pp-col* base)))
                     (if (member kind '(:line :section))
                         (%pp-spaces out (cond ((< col colnum) (- colnum col))
                                               ((and colinc (> colinc 0)) (- colinc (mod (- col colnum) colinc)))
                                               (t 0)))
                         (let ((target (+ col colnum)))
                           (when (and colinc (> colinc 1)) (setq target (* colinc (ceiling target colinc))))
                           (%pp-spaces out (- target col)))))))))))
    (%pp-out out (%ppblock-suffix b))))

(defun %pprint-logical-block (stream object prefix per-line-prefix suffix body)
  "Run BODY (a function of the pretty stream) as a logical block printing OBJECT."
  (let ((s (cond ((null stream) *standard-output*) ((eq stream t) *terminal-io*) (t stream))))
    (cond ((not (listp object)) (write object :stream s))
          ((typep s '%pretty-stream)
           (let ((b (%make-ppblock prefix per-line-prefix suffix)))
             (%pps-add s b)
             (setf (slot-value s 'stack) (cons b (%pps-stack s)))
             (unwind-protect (funcall body s) (setf (slot-value s 'stack) (cdr (%pps-stack s))))))
          (t
           (let* ((ps (make-instance '%pretty-stream :target s)) (root (%make-ppblock "" nil "")))
             (setf (slot-value ps 'stack) (list root))
             (let ((b (%make-ppblock prefix per-line-prefix suffix)))
               (%pps-add ps b)
               (setf (slot-value ps 'stack) (cons b (%pps-stack ps)))
               (funcall body ps))
             (let ((*pp-col* (%stream-column s)) (*pp-lines* 0) (*pp-margin* (or *print-right-margin* 80)) (*pp-line-prefixes* nil)
                   (*pp-line* (make-string-output-stream)))
               (dolist (it (reverse (%ppblock-items root))) (when (%ppblock-p it) (%pp-lay-block it s)))
               (%pp-flush-line s)))))
    nil))

(defmacro pprint-logical-block ((stream-symbol object &key (prefix "") per-line-prefix (suffix "")) &body body)
  "Print OBJECT (a list) as a logical block on the stream named by STREAM-SYMBOL (NIL: *STANDARD-OUTPUT*, T:
*TERMINAL-IO*), which BODY uses.  PREFIX and SUFFIX surround it; PER-LINE-PREFIX starts every line of it.
In BODY, (PPRINT-POP) takes the next element and (PPRINT-EXIT-IF-LIST-EXHAUSTED) ends the block when there
are none left; *PRINT-LENGTH* is respected and a dotted tail is printed after \". \"."
  (let ((var (if (member stream-symbol '(nil t)) (gensym "S") stream-symbol))
        (list (gensym "LIST")) (count (gensym "COUNT")) (blk (gensym "BLOCK")))
    `(%pprint-logical-block ,(cond ((null stream-symbol) '*standard-output*) ((eq stream-symbol t) '*terminal-io*) (t stream-symbol))
                            ,object ,prefix ,per-line-prefix ,suffix
                            (lambda (,var)
                              (declare (ignorable ,var))
                              (let ((,list ,object) (,count 0))
                                (declare (ignorable ,list ,count))
                                (block ,blk
                                  (macrolet ((pprint-pop ()
                                               '(cond ((and ,list (atom ,list))
                                                       (write-string ". " ,var) (write ,list :stream ,var) (return-from ,blk nil))
                                                      ((and *print-length* (>= ,count *print-length*))
                                                       (write-string "..." ,var) (return-from ,blk nil))
                                                      (t (setq ,count (+ ,count 1)) (pop ,list))))
                                             (pprint-exit-if-list-exhausted () '(when (null ,list) (return-from ,blk nil))))
                                    ,@body)))))))

(defun pprint-newline (kind &optional stream)
  "A conditional newline of KIND (:LINEAR :FILL :MISER :MANDATORY) in the logical block being printed."
  (let ((s (%out stream)))
    (unless (member kind '(:linear :fill :miser :mandatory)) (error "Bad newline kind ~S" kind))
    (when (typep s '%pretty-stream) (%pps-add s (list :newline kind)))
    nil))

(defun pprint-indent (relative-to n &optional stream)
  "Indent the following lines of the logical block N columns from its start (:BLOCK) or from here (:CURRENT)."
  (let ((s (%out stream)))
    (unless (member relative-to '(:block :current)) (error "Bad indentation kind ~S" relative-to))
    (when (typep s '%pretty-stream) (%pps-add s (list :indent relative-to n)))
    nil))

(defun pprint-tab (kind colnum colinc &optional stream)
  "Move to column COLNUM (:LINE, :SECTION) or COLNUM columns on (:LINE-RELATIVE, :SECTION-RELATIVE), then to a
multiple of COLINC."
  (let ((s (%out stream)))
    (if (typep s '%pretty-stream)
        (%pps-add s (list :tab kind colnum colinc))
        (let ((col (%stream-column s)))
          (dotimes (i (if (member kind '(:line :section)) (max 0 (- colnum col)) colnum)) (write-char #\Space s))))
    nil))

(defun %pprint-list (stream list colon-p kind)
  (pprint-logical-block (stream list :prefix (if colon-p "(" "") :suffix (if colon-p ")" ""))
    (pprint-exit-if-list-exhausted)
    (loop (write (pprint-pop) :stream stream)
          (pprint-exit-if-list-exhausted)
          (write-char #\Space stream)
          (pprint-newline kind stream))))

(defun pprint-fill (stream list &optional (colon-p t) at-sign-p)
  "Print LIST with as many elements on each line as fit (in parentheses if COLON-P)."
  (declare (ignore at-sign-p))
  (%pprint-list (%out stream) list colon-p :fill) nil)

(defun pprint-linear (stream list &optional (colon-p t) at-sign-p)
  "Print LIST on one line if it fits, else one element per line (in parentheses if COLON-P)."
  (declare (ignore at-sign-p))
  (%pprint-list (%out stream) list colon-p :linear) nil)

(defun pprint-tabular (stream list &optional (colon-p t) at-sign-p (tabsize 16))
  "Print LIST like PPRINT-FILL with its elements in columns TABSIZE wide."
  (declare (ignore at-sign-p))
  (let ((stream (%out stream)))
    (pprint-logical-block (stream list :prefix (if colon-p "(" "") :suffix (if colon-p ")" ""))
      (pprint-exit-if-list-exhausted)
      (loop (write (pprint-pop) :stream stream)
            (pprint-exit-if-list-exhausted)
            (write-char #\Space stream)
            (pprint-tab :section-relative 0 tabsize stream)
            (pprint-newline :fill stream))))
  nil)
