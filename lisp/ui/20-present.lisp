;;;; 20-present.lisp -- the presentation system.
;;;;
;;;; Output written to a pane is kept as styled segments in an OUTPUT BUFFER;
;;;; a segment may carry a PRESENTATION (an object plus a presentation type), which
;;;; makes it mouse-sensitive.  Presentation types inherit through the CLOS class
;;;; precedence list.  PRESENTATION COMMANDS are registered per type; clicking a
;;;; presentation runs the default command for its type, and the right button
;;;; offers a menu of all applicable commands (most specific type first).

(in-package :cl)

(defstruct (seg (:constructor make-seg (text style pres)) (:copier nil)) text style pres)
(defstruct (oline (:constructor make-oline ()) (:copier nil)) (rsegs nil))
(defstruct (obuf (:constructor %make-obuf) (:conc-name ob-) (:copier nil))
  (rlines nil) (count 1) (max-lines 3000))
(defstruct (pres (:constructor make-pres (object type)) (:copier nil)) object type)

(defun make-obuf () (let ((b (%make-obuf))) (setf (ob-rlines b) (list (make-oline))) b))

(defun ob-write (b string style pres)
  "Append STRING (which may contain newlines) to the buffer."
  (let ((start 0) (n (length string)))
    (loop
      (let ((nl (%string-position-char string #\Newline start)))
        (let ((end (or nl n)))
          (when (> end start)
            (let ((line (car (ob-rlines b))))
              (setf (oline-rsegs line) (cons (make-seg (subseq string start end) style pres) (oline-rsegs line)))))
          (cond (nl (setf (ob-rlines b) (cons (make-oline) (ob-rlines b)))
                    (setf (ob-count b) (+ 1 (ob-count b)))
                    (setq start (+ nl 1)))
                (t (return)))))))
  (when (> (ob-count b) (+ (ob-max-lines b) 200))
    (setf (ob-rlines b) (subseq (ob-rlines b) 0 (ob-max-lines b)))
    (setf (ob-count b) (ob-max-lines b)))
  b)

(defun ob-clear (b) (setf (ob-rlines b) (list (make-oline)) (ob-count b) 1))
(defun ob-current-column (b)
  (let ((n 0)) (dolist (s (oline-rsegs (car (ob-rlines b)))) (setq n (+ n (utf8-length (seg-text s))))) n))

;;; ---- UTF-8 helpers ------------------------------------------------------------------------------

(defun utf8-length (str)
  "The number of columns the UTF-8 string STR takes (wide characters count twice, combining marks not at all)."
  (string-display-width str))

(defun utf8-prefix-end (str start max-chars)
  "Byte index just after the characters of STR, from byte START, that fit in MAX-CHARS columns (at least one
character, with the combining marks that follow it)."
  (let ((i start) (n (length str)) (count 0))
    (loop
      (when (>= i n) (return i))
      (multiple-value-bind (code next) (utf8-char-at str i)
        (let ((w (if (< code 32) 1 (char-display-width code))))
          (when (and (> count 0) (> (+ count w) max-chars)) (return i))
          (when (and (= count 0) (>= count max-chars)) (return i))
          (setq i next count (+ count w))
          (loop (when (>= i n) (return))                       ; keep combining marks with their character
                (multiple-value-bind (c2 n2) (utf8-char-at str i)
                  (if (and (>= c2 #x300) (= 0 (char-display-width c2))) (setq i n2) (return)))))))))

;;; ---- layout: wrap logical lines into display rows ----------------------------------------------------

(defun ob-layout (b width &optional (max-lines 800) word-wrap)
  "Returns a vector of display rows.  A row is a list of chunks (text style pres col).
With WORD-WRAP, lines break between words (see %LAYOUT-LINE-WORDS) rather than at the last column."
  (let ((rows nil) (lines nil) (k 0))
    (dolist (l (ob-rlines b)) (when (>= k max-lines) (return)) (setq lines (cons l lines)) (setq k (+ k 1)))
    (dolist (line lines)
      (if (and word-wrap (> width 0))
          (dolist (r (%layout-line-words (reverse (oline-rsegs line)) width)) (setq rows (cons r rows)))
          (let ((cur nil) (col 0) (segs (reverse (oline-rsegs line))))
            (dolist (sg segs)
              (let ((text (seg-text sg)) (pos 0))
                (loop
                  (when (>= pos (length text)) (return))
                  (when (>= col width) (setq rows (cons (nreverse cur) rows)) (setq cur nil) (setq col 0))
                  (let ((end (utf8-prefix-end text pos (- width col))))
                    (setq cur (cons (list (subseq text pos end) (seg-style sg) (seg-pres sg) col) cur))
                    (setq col (+ col (utf8-length (subseq text pos end))))
                    (setq pos end)))))
            (setq rows (cons (nreverse cur) rows)))))
    (coerce (nreverse rows) 'vector)))

(defun %line-units (segs)
  "Split SEGS into units: runs of spaces or of non-spaces, each a list (spacep width pieces),
where a piece is (text style pres).  A word may span several segments (styles)."
  (let ((units nil) (cur nil) (cur-space nil) (cur-w 0))
    (dolist (sg segs)
      (let* ((text (seg-text sg)) (n (length text)) (pos 0))
        (loop
          (when (>= pos n) (return))
          (let* ((sp (char= (char text pos) #\Space)) (end pos))
            (loop (when (or (>= end n) (not (eq (char= (char text end) #\Space) sp))) (return)) (setq end (+ end 1)))
            (unless (and cur (eq sp cur-space))
              (when cur (setq units (cons (list cur-space cur-w (nreverse cur)) units)))
              (setq cur nil cur-w 0 cur-space sp))
            (let ((piece (subseq text pos end)))
              (setq cur (cons (list piece (seg-style sg) (seg-pres sg)) cur))
              (setq cur-w (+ cur-w (utf8-length piece))))
            (setq pos end)))))
    (when cur (setq units (cons (list cur-space cur-w (nreverse cur)) units)))
    (nreverse units)))

(defun %list-marker-p (text)
  "True for a list bullet such as *, - or 12."
  (let ((n (length text)))
    (or (string= text "*") (string= text "-")
        (and (> n 1) (char= (char text (- n 1)) #\.)
             (let ((ok t)) (dotimes (i (- n 1)) (unless (digit-char-p (char text i)) (setq ok nil))) ok)))))

(defun %hanging-indent (units width)
  "Continuation-row indent for a wrapped line: its leading spaces, plus a list marker and its space."
  (let ((ind 0) (u units))
    (when (and u (first (car u))) (setq ind (second (car u)) u (cdr u)))
    (when (and u (cdr u) (first (cadr u)) (= (length (third (car u))) 1) (%list-marker-p (first (car (third (car u))))))
      (setq ind (+ ind (second (car u)) (second (cadr u)))))
    (if (> (* ind 2) width) 0 ind)))

(defun %layout-line-words (segs width)
  "Lay out one logical line as word-wrapped rows; words too long for a row are broken by character."
  (let* ((units (%line-units segs)) (indent (%hanging-indent units width))
         (rows nil) (cur nil) (col 0) (row-start 0) (pending nil) (wrapped nil))
    (flet ((newline () (setq rows (cons (nreverse cur) rows) cur nil col indent row-start indent pending nil wrapped t))
           (place (pieces) (dolist (p pieces)
                             (setq cur (cons (list (first p) (second p) (third p) col) cur))
                             (setq col (+ col (utf8-length (first p)))))))
      (dolist (u units)
        (let ((w (second u)))
          (cond ((first u)                      ; spaces: held until the next word lands on this row
                 (unless (and wrapped (null cur)) (setq pending u)))
                ((<= (+ col (if pending (second pending) 0) w) width)
                 (when pending (place (third pending)) (setq pending nil))
                 (place (third u)))
                ((and (> col row-start) (<= w (- width indent)))
                 (newline) (place (third u)))
                (t                              ; too long for any row: break it by character
                 (when (> col row-start) (newline))
                 (setq pending nil)
                 (dolist (p (third u))
                   (let ((text (first p)) (pos 0))
                     (loop
                       (when (>= pos (length text)) (return))
                       (when (>= col width) (newline))
                       (let ((end (utf8-prefix-end text pos (- width col))))
                         (setq cur (cons (list (subseq text pos end) (second p) (third p) col) cur))
                         (setq col (+ col (utf8-length (subseq text pos end))))
                         (setq pos end)))))))))
      (when (and pending (null cur)) (place (third pending)))  ; a line of nothing but spaces
      (setq rows (cons (nreverse cur) rows))
      (nreverse rows))))

(defun row-chunk-at (row col)
  (dolist (ch row nil)
    (when (and (>= col (fourth ch)) (< col (+ (fourth ch) (utf8-length (first ch))))) (return ch))))

;;; ---- the pane output stream -----------------------------------------------------------------------------

(defclass pane-stream (fundamental-character-output-stream)
  ((obuf :initarg :obuf :accessor pane-stream-obuf)
   (style :initform :normal :accessor pane-stream-style)
   (pres :initform nil :accessor pane-stream-pres)))

(defmethod stream-write-char ((s pane-stream) ch)
  (ob-write (pane-stream-obuf s) (if (< (char-code ch) 128) (string ch) (%utf8-encode (char-code ch)))
            (pane-stream-style s) (pane-stream-pres s)))
(defmethod stream-write-string ((s pane-stream) string &optional start end)
  (ob-write (pane-stream-obuf s) (if (or start end) (subseq string (or start 0) end) string)
            (pane-stream-style s) (pane-stream-pres s))
  string)
(defmethod stream-line-column ((s pane-stream)) (ob-current-column (pane-stream-obuf s)))

(defmacro with-style ((stream style) &body body)
  (let ((s (gensym)) (old (gensym)))
    `(let* ((,s ,stream) (,old (pane-stream-style ,s)))
       (setf (pane-stream-style ,s) ,style)
       (unwind-protect (progn ,@body) (setf (pane-stream-style ,s) ,old)))))

(defmacro with-output-as-presentation ((stream object type) &body body)
  (let ((s (gensym)) (old (gensym)))
    `(let* ((,s ,stream) (,old (pane-stream-pres ,s)))
       (setf (pane-stream-pres ,s) (make-pres ,object ,type))
       (unwind-protect (progn ,@body) (setf (pane-stream-pres ,s) ,old)))))

(defgeneric present-graphically (object stream)
  (:documentation "Draw OBJECT on the pane STREAM as a picture and return true, or return NIL to have it printed."))
(defmethod present-graphically ((object t) stream) (declare (ignore stream)) nil)

(defun present (object &key (type (presentation-type-of object)) (stream *standard-output*) (escape t))
  (if (typep stream 'pane-stream)
      (with-output-as-presentation (stream object type)
        (unless (present-graphically object stream)
          (write-string (if escape (prin1-to-string object) (princ-to-string object)) stream)))
      (if escape (prin1 object stream) (princ object stream)))
  object)

;;; ---- images in output ----------------------------------------------------------------------------------------

(defun draw-image (image &key (stream *standard-output*) cols rows)
  "Show IMAGE on STREAM, COLS x ROWS cells (by default its pixel size in cells).  On a window pane the image
becomes part of the output (drawn by the terminal in kitty and Ghostty, with half blocks elsewhere); on the
terminal of the line Listener in kitty or Ghostty it is shown at the cursor; on other streams it is printed."
  (multiple-value-bind (c r) (image-cells-for image cols rows)
    (cond ((typep stream 'pane-stream)
           (let ((key (image-placement-key image c r)) (b (pane-stream-obuf stream)) (text (%image-row-text c)))
             (unless (= 0 (ob-current-column b)) (ob-write b (string #\Newline) :normal nil))
             (dotimes (i r)
               (ob-write b text (list :image key i) (pane-stream-pres stream))
               (ob-write b (string #\Newline) :normal nil))))
          ((and (%stream-terminal-p stream) (%kitty-like-p ""))
           (fresh-line stream) (finish-output stream)
           (%term-image-inline (image-pixels image) (image-width image) (image-height image) c r)
           (terpri stream))
          (t (prin1 image stream) (terpri stream))))
  image)

(defmethod present-graphically ((object image) stream) (draw-image object :stream stream) t)

(defun draw-image-row (stream image row cols rows)
  "Write row ROW of IMAGE shown on COLS x ROWS cells to the pane STREAM, at the current column (no newline)."
  (ob-write (pane-stream-obuf stream) (%image-row-text cols) (list :image (image-placement-key image cols rows) row) (pane-stream-pres stream)))

;;; ---- presentation types ----------------------------------------------------------------------------------

(defun presentation-type-of (obj)
  (cond ((null obj) 'null) ((integerp obj) 'integer) ((ratiop obj) 'ratio) ((floatp obj) 'float)
        ((stringp obj) 'string) ((characterp obj) 'character) ((symbolp obj) (if (keywordp obj) 'keyword 'symbol))
        ((consp obj) 'list) ((functionp obj) 'function) ((packagep obj) 'package)
        ((structp obj) (%cls-name (class-of obj)))
        ((vectorp obj) 'vector) ((streamp obj) 'stream)
        (t 't)))

(defun define-presentation-type (name &optional parents)
  (%put name '%ptype-parents parents) name)

(define-presentation-type 'keyword '(symbol))
(define-presentation-type 'frame '(t))
(define-presentation-type 'test-failure '(t))

(defun presentation-supertypes (type)
  "TYPE followed by its supertypes, most specific first, ending in T."
  (let ((acc (list type)) (c (and (symbolp type) (find-class type nil))))
    (when c (dolist (k (cdr (%cls-cpl c))) (unless (eq (%cls-name k) 't) (setq acc (append acc (list (%cls-name k)))))))
    (dolist (p (get type '%ptype-parents)) (dolist (s (presentation-supertypes p)) (unless (member s acc) (setq acc (append acc (list s))))))
    (unless (member 't acc) (setq acc (append acc (list 't))))
    (if (and (eq type 'keyword) (not (member 'symbol acc))) (append acc '(symbol)) acc)))

;;; ---- presentation commands --------------------------------------------------------------------------------

(defstruct (pcmd (:constructor %make-pcmd) (:copier nil)) name type doc fn default)
(defvar *pcommands* nil)

(defun %register-pcommand (name type doc default fn)
  (setq *pcommands* (cons (%make-pcmd :name name :type type :doc doc :fn fn :default default)
                          (remove-if (lambda (c) (and (eq (pcmd-name c) name) (eq (pcmd-type c) type))) *pcommands*)))
  name)

(defmacro define-presentation-command ((name type &key (documentation "") default) (var) &body body)
  `(%register-pcommand ',name ',type ,documentation ,default (lambda (,var) ,@body)))

(defun applicable-pcommands (type)
  "Commands for TYPE and its supertypes, most specific type first, one per command name."
  (let ((acc nil))
    (dolist (ty (presentation-supertypes type))
      (dolist (c (reverse *pcommands*))
        (when (and (eq (pcmd-type c) ty) (not (find (pcmd-name c) acc :key #'pcmd-name)))
          (setq acc (append acc (list c))))))
    acc))

(defun default-pcommand (type)
  (let ((cmds (applicable-pcommands type)))
    (or (find-if #'pcmd-default cmds) (car cmds))))

(defun pcmd-label (c) (if (> (length (pcmd-doc c)) 0) (pcmd-doc c) (string-capitalize (substitute #\Space #\- (symbol-name (pcmd-name c))))))

(defun presentation-doc (p)
  (let ((d (default-pcommand (pres-type p))) (s (prin1-to-string (pres-object p))))
    (format nil "L: ~A   M: Copy to input   R: menu   [~A] ~A"
            (if d (pcmd-label d) "-") (string-downcase (symbol-name (pres-type p)))
            (if (> (length s) 60) (%string-concat (subseq s 0 57) "...") s))))

;;; ---- DESCRIBE ----------------------------------------------------------------------------------------------

(defun describe (object &optional (stream *standard-output*))
  (let ((class (class-of object)))
    (format stream "~&~S~%  is of type ~A~%" object (string-downcase (symbol-name (%cls-name class))))
    (cond ((and (symbolp object) object)
           (when (boundp object) (format stream "  value: ~S~%" (symbol-value object)))
           (when (fboundp object) (format stream "  function: ~S~%" (symbol-function object)))
           (when (documentation object 'function) (format stream "  documentation: ~A~%" (documentation object 'function)))
           (when (documentation object 'variable) (format stream "  variable documentation: ~A~%" (documentation object 'variable)))
           (when (symbol-plist object) (format stream "  plist: ~S~%" (symbol-plist object)))
           (format stream "  package: ~S~%" (symbol-package object)))
          ((and (structp object) (not (%hash-table-p object)))
           (let ((d (%struct-type object)))
             (if (classp d)
                 (dolist (sd (%cls-slots d))
                   (format stream "  ~A: ~S~%" (symbol-name (slotd-name sd))
                           (if (slot-boundp object (slotd-name sd)) (slot-value object (slotd-name sd)) '#:unbound)))
                 (let ((i 1)) (dolist (n (cdr d)) (format stream "  ~A: ~S~%" (symbol-name n) (%struct-ref object i)) (setq i (+ i 1)))))))
          ((%hash-table-p object)
           (format stream "  ~D entries, test ~S~%" (hash-table-count object) (hash-table-test object)))
          ((consp object) (format stream "  a list of length ~D~%" (list-length-safe object)))
          ((stringp object) (format stream "  a string of length ~D~%" (length object)))
          ((vectorp object) (format stream "  a vector of length ~D~%" (length object)))
          ((integerp object) (format stream "  integer, ~D bits~%" (integer-length object)))
          ((functionp object) (format stream "  name: ~S~%" (%function-name object))))
    (values)))
