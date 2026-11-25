;;;; 20-seq.lisp -- sequences, strings, characters, types.

(in-package :cl)

(defconstant most-positive-fixnum 1152921504606846975)
(defconstant most-negative-fixnum -1152921504606846976)
(defconstant pi 3.141592653589793)
(defconstant internal-time-units-per-second 1000000)
(defconstant call-arguments-limit 1000000)
(defconstant array-dimension-limit 4294967296)

;;; ---- types ----------------------------------------------------------------

(defmacro typecase (key &rest clauses)
  (let ((k (gensym)))
    `(let ((,k ,key))
       (cond ,@(mapcar (lambda (c)
                         (if (or (eq (car c) t) (eq (car c) 'otherwise))
                             `(t ,@(cdr c))
                             `((typep ,k ',(car c)) ,@(or (cdr c) '(nil)))))
                       clauses)))))
(defmacro etypecase (key &rest clauses)
  (let ((k (gensym)))
    `(let ((,k ,key)) (typecase ,k ,@clauses (t (error "~S fell through an ETYPECASE" ,k))))))
(defmacro deftype (name args &body body)
  `(%put ',name '%deftype (lambda ,args ,@body)))

;;; ---- characters -----------------------------------------------------------

(defun char= (a &rest more) (every (lambda (b) (eql a b)) more))
(defun char/= (a b) (not (eql a b)))
(defun char< (a b) (< (char-code a) (char-code b)))
(defun char> (a b) (> (char-code a) (char-code b)))
(defun char<= (a b) (<= (char-code a) (char-code b)))
(defun char>= (a b) (>= (char-code a) (char-code b)))
(defun char-equal (a b) (eql (char-downcase a) (char-downcase b)))
(defun graphic-char-p (c) (and (>= (char-code c) 32) (< (char-code c) 127)))
(defun whitespace-char-p (c) (member (char-code c) '(32 9 10 13 12 11)))
(defun char-name (c)
  (case (char-code c) (32 "Space") (10 "Newline") (9 "Tab") (13 "Return") (127 "Rubout") (t nil)))

;;; ---- strings --------------------------------------------------------------

(defun string< (a b) (< (%string-compare a b nil) 0))
(defun string> (a b) (> (%string-compare a b nil) 0))
(defun string<= (a b) (<= (%string-compare a b nil) 0))
(defun string>= (a b) (>= (%string-compare a b nil) 0))
(defun string/= (a b) (not (string= a b)))
(defun string-equal (a b) (= (%string-compare a b t) 0))
(defun string-lessp (a b) (< (%string-compare a b t) 0))
(defun string-greaterp (a b) (> (%string-compare a b t) 0))

(defun %trim-pred (bag) (lambda (c) (find c bag)))
(defun string-left-trim (bag s)
  (let ((s (string s)) (i 0))
    (loop-while-trim s i bag)))
(defun loop-while-trim (s i bag)
  (let ((n (length s)))
    (block nil
      (tagbody top
         (when (and (< i n) (find (char s i) bag)) (setq i (+ i 1)) (go top))))
    (subseq s i)))
(defun string-right-trim (bag s)
  (let* ((s (string s)) (n (length s)))
    (block nil
      (tagbody top
         (when (and (> n 0) (find (char s (- n 1)) bag)) (setq n (- n 1)) (go top))))
    (subseq s 0 n)))
(defun string-trim (bag s) (string-left-trim bag (string-right-trim bag s)))

(defun string-capitalize (s)
  (let* ((s (copy-seq (string s))) (start t))
    (dotimes (i (length s))
      (let ((c (char s i)))
        (if (alpha-char-p c)
            (progn (%aset s i (if start (char-upcase c) (char-downcase c))) (setq start nil))
            (setq start t))))
    s))
(defun nstring-upcase (s) (string-upcase s))
(defun nstring-downcase (s) (string-downcase s))

(defun search (needle hay &key start2 (start1 0) from-end test key)
  (declare (ignore start1 from-end key test))
  (if (and (stringp needle) (stringp hay))
      (%string-search needle hay (or start2 0))
      (let ((n (%elt-list needle)) (h (%elt-list hay)) (i (or start2 0)))
        (setq h (nthcdr i h))
        (block found
          (do-list-tails (l h)
            (when (%prefix-p n l) (return-from found i))
            (setq i (+ i 1)))
          nil))))
(defun %prefix-p (n l)
  (dolist (x n t)
    (when (or (null l) (not (eql x (car l)))) (return nil))
    (setq l (cdr l))))

(defun mismatch (a b &key test)
  (let ((f (%test-fn test nil)) (la (%elt-list a)) (lb (%elt-list b)) (i 0))
    (block nil
      (tagbody top
         (cond ((and (null la) (null lb)) (return nil))
               ((or (null la) (null lb)) (return i))
               ((not (funcall f (car la) (car lb))) (return i)))
         (setq la (cdr la) lb (cdr lb) i (+ i 1))
         (go top)))))

(defun %starts-with (s prefix)
  (let ((n (length prefix)))
    (and (>= (length s) n) (string= (subseq s 0 n) prefix))))

(defun parse-integer (s &key (start 0) end (radix 10) junk-allowed)
  (let ((v (%parse-integer s start end radix)))
    (cond (v v)
          (junk-allowed nil)
          (t (error "Not an integer: ~S" s)))))

(defun string-to-list (s) (%vector-to-list s))
(defun %split-string (s ch)
  (let ((parts nil) (start 0))
    (block nil
      (tagbody top
         (let ((p (%string-position-char s ch start)))
           (cond (p (setq parts (cons (subseq s start p) parts)) (setq start (+ p 1)) (go top))
                 (t (setq parts (cons (subseq s start) parts)) (return))))))
    (nreverse parts)))
(defun %join-strings (strings sep)
  (let ((acc nil) (first t))
    (dolist (s strings)
      (unless first (setq acc (cons sep acc)))
      (setq first nil)
      (setq acc (cons s acc)))
    (apply #'%string-concat (nreverse acc))))

;;; ---- generic sequence operations ------------------------------------------

(defun elt (s i) (if (listp s) (car (nthcdr i s)) (aref s i)))

(let ((%make-string-prim #'make-string))
  (defun make-string (n &rest args)
    "A string of N characters; the fill character is the optional second argument or the :INITIAL-ELEMENT keyword."
    (if (and args (eq (car args) :initial-element))
        (funcall %make-string-prim n (cadr args))
        (apply %make-string-prim n args))))

(defun make-array (dims &key initial-element initial-contents fill-pointer adjustable element-type)
  (let ((n (if (consp dims) (car dims) dims)) (stringp (eq element-type 'character)))
    (cond ((or fill-pointer adjustable)
           (let ((v (%make-adjustable (max n 4) stringp)))
             (if (and fill-pointer (not (eq fill-pointer t)))
                 (%set-fill-pointer v fill-pointer)
                 (when (eq fill-pointer t) (%set-fill-pointer v n)))
             (when (and fill-pointer initial-element)
               (dotimes (i (if (eq fill-pointer t) n fill-pointer)) (%aset v i initial-element)))
             v))
          (stringp (let ((s (make-string n (or initial-element #\Space))))
                     (when initial-contents (let ((i 0)) (dolist (c (%elt-list initial-contents)) (%aset s i c) (setq i (+ i 1)))))
                     s))
          (t (let ((v (%make-vector n initial-element)))
               (when initial-contents
                 (let ((i 0)) (dolist (x (%elt-list initial-contents)) (%aset v i x) (setq i (+ i 1)))))
               v)))))

(defun vector-push (x v) (vector-push-extend x v))
(defun vector-pop (v)
  (let ((f (fill-pointer v)))
    (when (= f 0) (error "VECTOR-POP: empty vector"))
    (let ((x (aref v (- f 1))))
      (%set-fill-pointer v (- f 1))
      x)))
(defun array-total-size (v) (length v))
(defun array-dimension (v n) (declare (ignore n)) (length v))
(defun array-has-fill-pointer-p (v) (and (%advec-p v) t))

(defun coerce (x type)
  (let ((ty (if (consp type) (car type) type)))
    (case ty
      ((list) (cond ((listp x) x) (t (%vector-to-list x))))
      ((vector simple-vector array) (cond ((simple-vector-p x) x) ((listp x) (%list-to-vector x)) (t (%list-to-vector (%vector-to-list x)))))
      ((string simple-string) (cond ((stringp x) x)
                                    ((listp x) (apply #'%string-concat (mapcar #'string x)))
                                    ((%advec-p x) (apply #'%string-concat (mapcar #'string (%vector-to-list x))))
                                    (t (string x))))
      ((character) (if (characterp x) x (char (string x) 0)))
      ((float single-float double-float) (float x))
      ((function) (if (functionp x) x (symbol-function x)))
      ((t) x)
      (t (error "COERCE: cannot coerce to ~S" type)))))

(defun concatenate (type &rest seqs)
  (let ((ty (if (consp type) (car type) type)))
    (case ty
      ((string simple-string) (apply #'%string-concat (mapcar (lambda (s) (if (stringp s) s (coerce s 'string))) seqs)))
      ((list) (apply #'append (mapcar #'%elt-list seqs)))
      (t (%list-to-vector (apply #'append (mapcar #'%elt-list seqs)))))))

(defun map (type fn seq &rest more)
  (let ((r (apply #'mapcar fn (%elt-list seq) (mapcar #'%elt-list more))))
    (cond ((null type) nil)
          (t (coerce r type)))))

(defun fill (seq item &key (start 0) end)
  (if (listp seq)
      (let ((i 0))
        (do-list-tails (l seq)
          (when (and (>= i start) (or (null end) (< i end))) (rplaca l item))
          (setq i (+ i 1))))
      (dotimes (i (or end (length seq))) (when (>= i start) (%aset seq i item))))
  seq)

(defun replace (to from &key (start1 0) end1 (start2 0) end2)
  (let ((n (min (- (or end1 (length to)) start1) (- (or end2 (length from)) start2))))
    (dotimes (i n) (setf (elt to (+ start1 i)) (elt from (+ start2 i))))
    to))

(defun substitute (new old seq &key test key)
  (let ((f (%test-fn test nil)))
    (let ((r (mapcar (lambda (x) (if (funcall f old (if key (funcall key x) x)) new x)) (%elt-list seq))))
      (if (listp seq) r (coerce r (if (stringp seq) 'string 'vector))))))

(defun %remf-place (plist key) (declare (ignore key)) plist)
