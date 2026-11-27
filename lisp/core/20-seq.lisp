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
  (%symbol-lock-check name "define a type on")
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

;;; ---- element types ---------------------------------------------------------------------------------------
;;; Specialised arrays: MAKE-ARRAY :ELEMENT-TYPE gives an array whose stores are checked against the (upgraded) element
;;; type, whose elements default to 0 / 0.0, and that ARRAY-ELEMENT-TYPE reports.  The storage is still one word per
;;; element.  The codes below are the AE_ codes of src/neogenera.h; only strings (CHARACTER) are stored differently.

(defparameter %array-etypes
  '(bit (unsigned-byte 2) (unsigned-byte 4) (unsigned-byte 8) (unsigned-byte 16) (unsigned-byte 32) (unsigned-byte 64)
    (signed-byte 8) (signed-byte 16) (signed-byte 32) (signed-byte 64) fixnum single-float double-float character)
  "The upgraded element types, in AE_ code order (code 1 is the first).")

(defun %etype-code (type)
  "The AE_ code of the upgraded element type TYPE, or NIL for T."
  (let ((i 1))
    (dolist (x %array-etypes nil)
      (when (equal x type) (return i))
      (setq i (+ i 1)))))

(defun %etype-from-code (code) (if code (nth (- code 1) %array-etypes) t))

(defun %int-etype (lo hi)
  (cond ((not (and (integerp lo) (integerp hi))) t)
        ((>= lo 0)
         (let ((n (integer-length hi)))
           (cond ((<= n 1) 'bit) ((<= n 2) '(unsigned-byte 2)) ((<= n 4) '(unsigned-byte 4)) ((<= n 8) '(unsigned-byte 8))
                 ((<= n 16) '(unsigned-byte 16)) ((<= n 32) '(unsigned-byte 32)) ((<= n 64) '(unsigned-byte 64)) (t t))))
        (t (let ((n (+ 1 (max (integer-length lo) (integer-length hi)))))
             (cond ((<= n 8) '(signed-byte 8)) ((<= n 16) '(signed-byte 16)) ((<= n 32) '(signed-byte 32))
                   ((<= n 64) '(signed-byte 64)) (t t))))))

(defun upgraded-array-element-type (type &optional environment)
  "The element type arrays actually specialise on for TYPE: BIT, (UNSIGNED-BYTE n) for n 2 to 64, (SIGNED-BYTE n) for n
8 to 64, FIXNUM, SINGLE-FLOAT, DOUBLE-FLOAT, CHARACTER, or T."
  (declare (ignore environment))
  (cond ((member type '(bit)) 'bit)
        ((member type '(character base-char standard-char)) 'character)
        ((member type '(fixnum)) 'fixnum)
        ((member type '(single-float short-float)) 'single-float)
        ((member type '(double-float long-float)) 'double-float)
        ((consp type)
         (let ((h (car type)) (n (cadr type)))
           (cond ((and (eq h 'unsigned-byte) (integerp n)) (%int-etype 0 (- (expt 2 n) 1)))
                 ((and (eq h 'signed-byte) (integerp n) (> n 0)) (%int-etype (- (expt 2 (- n 1))) (- (expt 2 (- n 1)) 1)))
                 ((and (eq h 'mod) (integerp n) (> n 0)) (%int-etype 0 (- n 1)))
                 ((eq h 'integer) (%int-etype (if (consp (cdr type)) (cadr type) '*) (if (consp (cddr type)) (caddr type) '*)))
                 ((member h '(single-float short-float)) (if (cdr type) t 'single-float))
                 (t t))))
        (t t)))

(defun %etype-default (code)
  (cond ((null code) nil) ((= code 15) #\Space) ((<= code 12) 0) (t 0.0)))

(defun %dims-total (dims)
  (let ((p 1)) (dolist (d dims p) (unless (and (integerp d) (>= d 0)) (error "Bad array dimension ~S" d)) (setq p (* p d)))))

(defun %make-md-array-of (dims initial-element initial-contents element-type)
  (let* ((total (%dims-total dims))
         (code (%etype-code (upgraded-array-element-type element-type)))
         (chars (eql code 15))
         (data (cond (chars (make-string total (or initial-element #\Space)))
                     (code (%make-vector total (%etype-default code)))
                     (t (%make-vector total initial-element))))
         (a (%make-md-array data (copy-list dims) nil (and (not chars) code))))
    (when (and code (not chars) initial-element (> total 0))
      (%row-major-set a 0 initial-element)             ; type-check the initial element
      (dotimes (i total) (%aset data i initial-element)))
    (when initial-contents
      (let ((elts (%flatten-contents initial-contents (length dims))) (i 0))
        (unless (= (length elts) total) (error "Initial contents do not match the dimensions ~S" dims))
        (dolist (x elts) (%row-major-set a i x) (setq i (+ i 1)))))
    a))

(defun make-array (dims &key (initial-element nil ie-p) initial-contents fill-pointer adjustable (element-type t et-p)
                          displaced-to (displaced-index-offset 0))
  "An array of dimensions DIMS (an integer for a vector, or a list): filled with INITIAL-ELEMENT or the nested
INITIAL-CONTENTS.  A vector may have a FILL-POINTER and be ADJUSTABLE.  ELEMENT-TYPE CHARACTER makes strings; the other
types ARRAY-ELEMENT-TYPE reports (BIT, (UNSIGNED-BYTE 8), DOUBLE-FLOAT, ...) make specialised arrays whose stores are
type-checked.  With DISPLACED-TO (and DISPLACED-INDEX-OFFSET) the array shares the elements of another array; its element
type then defaults to the one of that array."
  (let* ((dl (if (listp dims) dims (list dims)))
         (vector-p (= (length dl) 1))
         (code (if (and displaced-to (not et-p))
                   (%array-etype displaced-to)
                   (%etype-code (upgraded-array-element-type element-type))))
         (chars (eql code 15))
         (total (%dims-total dl)))
    (when (and fill-pointer (not vector-p)) (error "Only vectors have fill pointers"))
    (when (and (consp dims) (null (cdr dims)) (not (integerp (car dims)))) (error "Bad array dimension ~S" (car dims)))
    (cond
      (displaced-to
       (when (or ie-p initial-contents) (error "MAKE-ARRAY: initial contents cannot be given for a displaced array"))
       (unless (arrayp displaced-to) (error "MAKE-ARRAY: ~S is not an array to displace to" displaced-to))
       (if vector-p
           (%make-advec nil (cond ((eq fill-pointer t) total) (fill-pointer fill-pointer) (t total))
                        (logior (if fill-pointer 0 1) (if adjustable 0 2)) code
                        displaced-to displaced-index-offset total)
           (%make-md-array displaced-to (copy-list dl) displaced-index-offset code)))
      ((not vector-p) (%make-md-array-of dl initial-element initial-contents (%etype-from-code code)))
      ((and code (not chars))
       (let* ((n total)
              (data (%make-vector n (%etype-default code)))
              (fp (cond ((eq fill-pointer t) n) (fill-pointer fill-pointer) (t n)))
              (v (%make-advec data fp (logior (if fill-pointer 0 1) (if adjustable 0 2)) code nil 0 n)))
         (when (and ie-p (> n 0))
           (%row-major-set v 0 initial-element)
           (dotimes (i n) (%aset data i initial-element)))
         (when initial-contents
           (let ((i 0))
             (unless (= (length initial-contents) n) (error "Initial contents do not match the dimension ~S" n))
             (dolist (x (%elt-list initial-contents)) (%row-major-set v i x) (setq i (+ i 1)))))
         v))
      (t
       (let ((n (car dl)) (stringp chars))
         (cond ((or fill-pointer adjustable)
                (let ((v (%make-adjustable (max n 4) stringp)))
                  (if (and fill-pointer (not (eq fill-pointer t)))
                      (%set-fill-pointer v fill-pointer)
                      (when (eq fill-pointer t) (%set-fill-pointer v n)))
                  (when (and fill-pointer initial-element)
                    (dotimes (i (if (eq fill-pointer t) n fill-pointer)) (%aset v i initial-element)))
                  (when (and fill-pointer initial-contents)
                    (let ((i 0))
                      (dolist (x (%elt-list initial-contents))
                        (when (>= i (if (eq fill-pointer t) n fill-pointer)) (error "Initial contents do not fit the fill pointer"))
                        (%aset v i x) (setq i (+ i 1)))))
                  v))
               (stringp (let ((s (make-string n (or initial-element #\Space))))
                          (when initial-contents (let ((i 0)) (dolist (c (%elt-list initial-contents)) (%aset s i c) (setq i (+ i 1)))))
                          s))
               (t (let ((v (%make-vector n initial-element)))
                    (when initial-contents
                      (let ((i 0)) (dolist (x (%elt-list initial-contents)) (%aset v i x) (setq i (+ i 1)))))
                    v))))))))

(defun vector-push (x v) (vector-push-extend x v))
(defun vector-pop (v)
  (let ((f (fill-pointer v)))
    (when (= f 0) (error "VECTOR-POP: empty vector"))
    (let ((x (aref v (- f 1))))
      (%set-fill-pointer v (- f 1))
      x)))
(defun array-rank (a) "The number of dimensions of the array A." (if (%md-array-p a) (length (%md-dims a)) 1))
(defun array-dimensions (a) "The dimensions of the array A, as a list." (if (%md-array-p a) (copy-list (%md-dims a)) (list (%array-total-size a))))
(defun array-dimension (a n) "Dimension N of the array A." (nth n (array-dimensions a)))
(defun array-total-size (a) "The number of elements of the array A." (%array-total-size a))
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
(defun row-major-aref (a i) "The element of A at row-major index I." (%row-major-ref a i))
(defun %row-major-aset (a i v) (%row-major-set a i v))
(defun array-element-type (a)
  "The (upgraded) element type of the array A: CHARACTER for strings, BIT or (UNSIGNED-BYTE 8) etc. for specialised arrays, else T."
  (%etype-from-code (%array-etype a)))
(defun array-displacement (a)
  "Two values: the array A is displaced to (or NIL) and the index offset into it (0 when not displaced)."
  (let ((to (%array-displaced-to a))) (if to (values to (%array-displacement-offset a)) (values nil 0))))
(defun adjustable-array-p (a)
  "True for an array made :ADJUSTABLE (or with a fill pointer): adjustable vectors."
  (and (%advec-p a) (zerop (logand 2 (%advec-flags a)))))
(defun bit (bit-array &rest subscripts) "The element of the bit array BIT-ARRAY at SUBSCRIPTS." (apply #'aref bit-array subscripts))
(defun sbit (bit-array &rest subscripts) "The element of the simple bit array BIT-ARRAY at SUBSCRIPTS." (apply #'aref bit-array subscripts))
(defun bit-vector-p (x) "True when X is a vector whose elements are bits." (and (vectorp x) (eq (array-element-type x) 'bit)))
(defun simple-bit-vector-p (x) "True when X is a bit vector." (bit-vector-p x))
(defun array-has-fill-pointer-p (v) "True for a vector with a fill pointer." (and (%advec-p v) (zerop (logand 1 (%advec-flags v)))))
(defun adjust-array (a new-dims &key (initial-element nil ie-p) initial-contents fill-pointer element-type displaced-to (displaced-index-offset 0))
  "Change the dimensions of A.  An adjustable vector is changed in place and returned; any other array is not changed
and a new array holding the elements that fit is returned.  With DISPLACED-TO the result shares the elements of that array."
  (let* ((dims (if (listp new-dims) new-dims (list new-dims)))
         (vector-p (null (cdr dims)))
         (et (or element-type (array-element-type a)))
         (arg (if vector-p (car dims) dims))
         (new nil))
    (unless (= (length dims) (array-rank a)) (error "ADJUST-ARRAY: the new array must have rank ~D, not ~D" (array-rank a) (length dims)))
    (if displaced-to
        (setq new (make-array arg :element-type et :displaced-to displaced-to :displaced-index-offset displaced-index-offset
                                  :adjustable t
                                  :fill-pointer (and vector-p (or fill-pointer (and (array-has-fill-pointer-p a) (fill-pointer a))))))
        (let ((fp (and vector-p (or fill-pointer (array-has-fill-pointer-p a)))))
          (setq new (apply #'make-array arg :fill-pointer (and fp t) :adjustable (adjustable-array-p a) :element-type et
                           (append (and ie-p (list :initial-element initial-element))
                                   (and initial-contents (list :initial-contents initial-contents)))))
          (when (and vector-p (array-has-fill-pointer-p new)) (%set-fill-pointer new (car dims)))
          (unless initial-contents
            (if vector-p
                (dotimes (i (min (length a) (car dims))) (%aset new i (aref a i)))
                (let ((old-dims (array-dimensions a)))
                  (labels ((copy (subs od nd)
                             (if (null od) (apply #'%aset new (append (reverse subs) (list (apply #'aref a (reverse subs)))))
                                 (dotimes (i (min (car od) (car nd))) (copy (cons i subs) (cdr od) (cdr nd))))))
                    (copy nil old-dims dims)))))
          (when (and vector-p (array-has-fill-pointer-p new))
            (%set-fill-pointer new (or fill-pointer (if initial-contents (car dims) (min (length a) (car dims))))))))
    (if (adjustable-array-p a) (%array-become a new) new)))

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
