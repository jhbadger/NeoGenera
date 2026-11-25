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
  (let ((n 0)) (dotimes (i (length str)) (unless (= (logand (char-code (char str i)) #xC0) #x80) (setq n (+ n 1)))) n))

(defun utf8-prefix-end (str start max-chars)
  "Byte index just after at most MAX-CHARS characters of STR starting at byte START."
  (let ((i start) (n (length str)) (count 0))
    (loop
      (when (or (>= i n) (>= count max-chars)) (return i))
      (setq i (+ i 1))
      (loop (when (or (>= i n) (/= (logand (char-code (char str i)) #xC0) #x80)) (return)) (setq i (+ i 1)))
      (setq count (+ count 1)))))

;;; ---- layout: wrap logical lines into display rows ----------------------------------------------------

(defun ob-layout (b width &optional (max-lines 800))
  "Returns a vector of display rows.  A row is a list of chunks (text style pres col)."
  (let ((rows nil) (lines nil) (k 0))
    (dolist (l (ob-rlines b)) (when (>= k max-lines) (return)) (setq lines (cons l lines)) (setq k (+ k 1)))
    (dolist (line lines)
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
        (setq rows (cons (nreverse cur) rows))))
    (coerce (nreverse rows) 'vector)))

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

(defun present (object &key (type (presentation-type-of object)) (stream *standard-output*) (escape t))
  (if (typep stream 'pane-stream)
      (with-output-as-presentation (stream object type)
        (write-string (if escape (prin1-to-string object) (princ-to-string object)) stream))
      (if escape (prin1 object stream) (princ object stream)))
  object)

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
