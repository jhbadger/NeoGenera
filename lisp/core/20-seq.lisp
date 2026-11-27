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

(defun %string-mismatch (a b ci start1 end1 start2 end2)
  "Compare the designated substrings of A and B: (values index order), where INDEX is the position in A of the first
difference (NIL when equal) and ORDER is -1, 0 or 1.  CI compares ignoring case."
  (let* ((a (string a)) (b (string b))
         (i (or start1 0)) (e1 (or end1 (length a))) (j (or start2 0)) (e2 (or end2 (length b))))
    (block nil
      (tagbody top
         (cond ((and (>= i e1) (>= j e2)) (return (values nil 0)))
               ((>= i e1) (return (values i -1)))
               ((>= j e2) (return (values i 1))))
         (let ((x (char a i)) (y (char b j)))
           (when ci (setq x (char-downcase x) y (char-downcase y)))
           (cond ((char< x y) (return (values i -1)))
                 ((char> x y) (return (values i 1)))))
         (setq i (+ i 1) j (+ j 1))
         (go top)))))

(defun %string-order (a b ci keys test)
  (let ((start1 (getf keys :start1)) (end1 (getf keys :end1)) (start2 (getf keys :start2)) (end2 (getf keys :end2)))
    (multiple-value-bind (i order) (%string-mismatch a b ci start1 end1 start2 end2)
      (and (funcall test order) (or i (or end1 (length (string a))))))))

(defun string< (a b &rest keys) "The mismatch index in A if A sorts before B, else NIL." (%string-order a b nil keys (lambda (o) (< o 0))))
(defun string> (a b &rest keys) "The mismatch index in A if A sorts after B, else NIL." (%string-order a b nil keys (lambda (o) (> o 0))))
(defun string<= (a b &rest keys) "The mismatch index in A if A sorts before or equal to B, else NIL." (%string-order a b nil keys (lambda (o) (<= o 0))))
(defun string>= (a b &rest keys) "The mismatch index in A if A sorts after or equal to B, else NIL." (%string-order a b nil keys (lambda (o) (>= o 0))))
(defun string/= (a b &rest keys) "The mismatch index in A if the strings differ, else NIL." (%string-order a b nil keys (lambda (o) (/= o 0))))
(defun string-equal (a b) (= (%string-compare a b t) 0))
(defun string-lessp (a b &rest keys) "Like STRING< ignoring case." (%string-order a b t keys (lambda (o) (< o 0))))
(defun string-greaterp (a b &rest keys) "Like STRING> ignoring case." (%string-order a b t keys (lambda (o) (> o 0))))
(defun string-not-equal (a b &rest keys) "Like STRING/= ignoring case." (%string-order a b t keys (lambda (o) (/= o 0))))

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
  "Parse an integer from S[START, END).  With JUNK-ALLOWED the longest numeric prefix is parsed (NIL when there is none)
instead of signalling an error."
  (let ((v (%parse-integer s start end radix)))
    (cond (v v)
          (junk-allowed
           (let* ((e (or end (length s))) (i start) (neg nil) (n 0) (digits 0))
             (loop while (and (< i e) (member (char s i) '(#\Space #\Tab #\Newline))) do (setq i (+ i 1)))
             (when (and (< i e) (member (char s i) '(#\+ #\-))) (setq neg (char= (char s i) #\-) i (+ i 1)))
             (loop while (and (< i e) (digit-char-p (char s i) radix))
                   do (setq n (+ (* n radix) (digit-char-p (char s i) radix)) digits (+ digits 1) i (+ i 1)))
             (and (> digits 0) (if neg (- n) n))))
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

(defun %flatten-contents (contents rank)
  "The elements of the nested sequence CONTENTS (RANK levels deep) in row-major order."
  (if (= rank 0) (list contents)
      (let ((acc nil)) (dolist (x (%elt-list contents)) (setq acc (append (reverse (%flatten-contents x (- rank 1))) acc))) (nreverse acc))))

(defun %make-md-array-of (dims initial-element initial-contents element-type)
  (let* ((total (let ((p 1)) (dolist (d dims p) (unless (and (integerp d) (>= d 0)) (error "Bad array dimension ~S" d)) (setq p (* p d)))))
         (data (if (eq element-type 'character) (make-string total (or initial-element #\Space)) (%make-vector total initial-element))))
    (when initial-contents
      (let ((elts (%flatten-contents initial-contents (length dims))) (i 0))
        (unless (= (length elts) total) (error "Initial contents do not match the dimensions ~S" dims))
        (dolist (x elts) (%aset data i x) (setq i (+ i 1)))))
    (%make-md-array data (copy-list dims))))

(defun make-array (dims &key initial-element initial-contents fill-pointer adjustable element-type displaced-to displaced-index-offset)
  "An array of dimensions DIMS (an integer for a vector, or a list): filled with INITIAL-ELEMENT or the nested
INITIAL-CONTENTS.  A vector may have a FILL-POINTER and be ADJUSTABLE; ELEMENT-TYPE CHARACTER makes strings."
  (declare (ignore displaced-index-offset))
  (when displaced-to (error "Displaced arrays are not supported"))
  (when (and (listp dims) (/= (length dims) 1))
    (when fill-pointer (error "Only vectors have fill pointers"))
    (return-from make-array (%make-md-array-of dims initial-element initial-contents element-type)))
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
(defun array-rank (a) "The number of dimensions of the array A." (if (%md-array-p a) (length (%md-dims a)) 1))
(defun array-dimensions (a) "The dimensions of the array A, as a list." (if (%md-array-p a) (copy-list (%md-dims a)) (list (length a))))
(defun array-dimension (a n) "Dimension N of the array A." (nth n (array-dimensions a)))
(defun array-total-size (a) "The number of elements of the array A." (if (%md-array-p a) (length (%md-data a)) (length a)))
(defun array-row-major-index (a &rest subscripts)
  "The index in row-major order of the element of A at SUBSCRIPTS."
  (let ((dims (array-dimensions a)) (idx 0))
    (unless (= (length dims) (length subscripts)) (error "Wrong number of subscripts ~S for ~S" subscripts dims))
    (dolist (d dims idx)
      (let ((s (pop subscripts))) (unless (and (integerp s) (< -1 s d)) (error "Subscript ~S out of bounds" s)) (setq idx (+ (* idx d) s))))))
(defun array-in-bounds-p (a &rest subscripts)
  "True if SUBSCRIPTS are valid for the array A."
  (let ((dims (array-dimensions a)))
    (and (= (length dims) (length subscripts)) (every (lambda (s d) (and (integerp s) (< -1 s d))) subscripts dims))))
(defun row-major-aref (a i) "The element of A at row-major index I." (if (%md-array-p a) (aref (%md-data a) i) (aref a i)))
(defun %row-major-aset (a i v) (if (%md-array-p a) (%aset (%md-data a) i v) (%aset a i v)))
(defun array-element-type (a) "The element type of A: CHARACTER for strings, else T."
  (if (or (stringp a) (and (%md-array-p a) (stringp (%md-data a)))) 'character t))
(defun adjustable-array-p (a) "True for an adjustable vector." (and (%advec-p a) t))
(defun adjust-array (a new-dims &key initial-element initial-contents fill-pointer element-type)
  "An array with dimensions NEW-DIMS holding the elements of A that fit (a new array; A is not changed)."
  (let* ((dims (if (listp new-dims) new-dims (list new-dims)))
         (new (make-array (if (= (length dims) 1) (car dims) dims) :initial-element initial-element
                          :initial-contents initial-contents :fill-pointer fill-pointer :adjustable (%advec-p a)
                          :element-type (or element-type (array-element-type a)))))
    (unless initial-contents
      (let ((old-dims (array-dimensions a)))
        (when (= (length old-dims) (length dims))
          (labels ((copy (subs od nd)
                     (if (null od) (apply #'%aset new (append (reverse subs) (list (apply #'aref a (reverse subs)))))
                         (dotimes (i (min (car od) (car nd))) (copy (cons i subs) (cdr od) (cdr nd))))))
            (if (%advec-p a)
                (dotimes (i (min (length a) (car dims))) (vector-push-extend (aref a i) new))
                (copy nil old-dims dims))))))
    new))
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
      ((complex) (if (complexp x) x (complex x)))
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
