;;;; 20-runtime.lisp -- the FORTRAN Tool Kit: storage, arithmetic, characters and intrinsic functions.
;;;;
;;;; Compiled FORTRAN is Lisp.  Storage is kept in two spaces, as on Genera: a NUMBER space of one-word entries
;;;; (INTEGER, REAL and LOGICAL take one entry, DOUBLE PRECISION and COMPLEX two, DOUBLE COMPLEX four) and a
;;;; CHARACTER space of one-character entries (a CHARACTER*n variable takes n entries; its value, a string of length n,
;;;; is kept in the first).  Every variable lives in a simple vector at an integer offset: locals in the routine's
;;;; static frame, COMMON variables in the block's vectors, and arguments are references (VECTOR . OFFSET), so
;;;; FORTRAN's call by reference, storage association (EQUIVALENCE, COMMON, array dummies that are sections of
;;;; actual arrays) and Lisp's WITH-FORTRAN-NUMBER-DATA all use one mechanism.  An entry that has not been given a
;;;; value holds the symbol F77-UNDEFINED, and every read checks for it ("detection of uninitialized values").
;;;; INTEGERs are Lisp integers (they never overflow), REAL values are doubles rounded to single precision when
;;;; stored, DOUBLE PRECISION values doubles, COMPLEX values Lisp complex numbers, LOGICAL values T and NIL.

(in-package :cl)

(defvar *f77-line* 0 "The source line of the FORTRAN statement being executed.")
(defvar *f77-routine* nil "The name of the FORTRAN routine being executed.")
(defvar *f77-program* nil "The FORTRAN program being executed (an F77PROGRAM), or NIL.")
(defvar *f77-env* nil "The execution environment (an FENV) of the running program, or NIL.")
(defvar *f77-global-env* nil "The environment used by routines called from Lisp outside F77:EXECUTE.")
(defvar *f77-pathname-defaults* "" "Directory that file names in OPEN are relative to.")
(defvar *f77-running* nil "True while F77:EXECUTE is running a program.")
(defvar *f77-init-to-zero* nil "When true, the compiler and run time initialise storage to zero instead of undefined.")
(defvar *f77-check-undefined* t "When true, compiled code checks every variable read for an undefined value.")
(defvar *f77-debug-info* t "When true, compiled code records the routine and source line being executed.")

(define-condition f77-runtime-error (error)
  ((message :initarg :message :reader f77-error-message)
   (routine :initarg :routine :initform nil :reader f77-error-routine)
   (line :initarg :line :initform nil :reader f77-error-line)
   (code :initarg :code :initform 1 :reader f77-error-code)
   (original :initarg :original :initform nil :reader f77-error-original))
  (:report (lambda (c s)
             (format s "~A~@[~%  (in FORTRAN routine ~A~]~:[~;, line ~D~]~:[~;)~]"
                     (f77-error-message c) (and (f77-error-routine c) (string-downcase (princ-to-string (f77-error-routine c))))
                     (and (f77-error-routine c) (f77-error-line c) (> (f77-error-line c) 0)) (f77-error-line c)
                     (f77-error-routine c)))))

(define-condition f77-io-error (f77-runtime-error) ()
  (:documentation "An input/output error; CODE is the IOSTAT value (-1 at the end of a file)."))

(defun %f7-rerror (fmt &rest args)
  "Signal an F77-RUNTIME-ERROR in the routine and line being executed."
  (error 'f77-runtime-error :message (apply #'format nil fmt args) :routine *f77-routine* :line *f77-line*))

(defun %f7-ioerror (code fmt &rest args)
  (error 'f77-io-error :message (apply #'format nil fmt args) :routine *f77-routine* :line *f77-line* :code code))

(defun %f7-undef (name)
  (%f7-rerror "The variable ~A is used before it has been given a value" (string-downcase (princ-to-string name))))

(defmacro %f7-rd (vec idx name)
  "The entry IDX of VEC, which must not be undefined."
  (let ((v (gensym "V")))
    `(let ((,v (svref ,vec ,idx))) (if (eq ,v 'f77-undefined) (%f7-undef ',name) ,v))))

(defmacro %f7-ix (i lo hi name)
  "The zero-based offset of subscript I of an array dimension LO..HI (HI NIL: assumed size)."
  (let ((v (gensym "I")))
    `(let ((,v ,i))
       (if (and (integerp ,v) (>= ,v ,lo) ,@(when hi `((<= ,v ,hi))))
           (- ,v ,lo)
           (%f7-bad-index ,v ,lo ,hi ',name)))))

(defun %f7-bad-index (i lo hi name)
  (cond ((eq i 'f77-undefined) (%f7-undef "subscript"))
        ((not (integerp i)) (%f7-rerror "The subscript ~S of ~A is not an integer" i (string-downcase (princ-to-string name))))
        (t (%f7-rerror "The subscript ~D of ~A is outside the array bounds ~D:~A" i (string-downcase (princ-to-string name)) lo (or hi "*")))))

;;; ---- environments and storage ---------------------------------------------------------------------------------

(defstruct (fenv (:constructor %make-fenv) (:copier nil))
  (frames (make-hash-table :test 'eq)) (commons (make-hash-table :test 'equal)) (units (make-hash-table))
  init-zero input output streams pathname-defaults program extra-number extra-character externals)

(defun %f7-env ()
  (or *f77-env* *f77-global-env* (setq *f77-global-env* (%make-fenv))))

(defun %f7-zero-value (kind)
  (case kind (:integer 0) (:real 0d0) (:double 0d0) (:complex (complex 0d0 0d0)) (:dcomplex (complex 0d0 0d0)) (:logical nil) (t 0)))

(defun %f7-apply-zspecs (nv cv zspecs)
  "Fill the entries described by ZSPECS, lists (space offset count kind-or-length stride), with typed zeros."
  (dolist (z zspecs)
    (let ((space (first z)) (off (second z)) (count (third z)) (kind (fourth z)) (stride (fifth z)))
      (dotimes (k count)
        (if (eq space :chr)
            (setf (svref cv (+ off (* k stride))) (make-string kind :initial-element #\Space))
            (setf (svref nv (+ off (* k stride))) (%f7-zero-value kind)))))))

(defun %f7-make-space (n zero)
  (make-array n :initial-element (if zero 0 'f77-undefined)))

(defun %f7-frame (key nn nc zspecs)
  "The static storage of the routine KEY for the running program: (values (numbers . characters) new-p)."
  (let* ((env (%f7-env)) (cell (gethash key (fenv-frames env))))
    (if (and cell (>= (length (car cell)) nn) (>= (length (cdr cell)) nc))
        (values cell nil)
        (let* ((z (fenv-init-zero env)) (new (cons (%f7-make-space nn z) (%f7-make-space nc z))))
          (when z (%f7-apply-zspecs (car new) (cdr new) zspecs))
          (setf (gethash key (fenv-frames env)) new)
          (values new t)))))

(defun %f7-common (name nn nc zspecs)
  "The storage of the COMMON block NAME: (values (numbers . characters) new-p).  The block grows if a later routine needs more."
  (let* ((env (%f7-env)) (cell (gethash name (fenv-commons env))))
    (cond ((and cell (>= (length (car cell)) nn) (>= (length (cdr cell)) nc)) (values cell nil))
          (t
           (let* ((z (fenv-init-zero env))
                  (nv (%f7-make-space (max nn (if cell (length (car cell)) 0)) z))
                  (cv (%f7-make-space (max nc (if cell (length (cdr cell)) 0)) z)))
             (when cell
               (replace nv (car cell)) (replace cv (cdr cell)))
             (when (and z (not cell)) (%f7-apply-zspecs nv cv zspecs))
             (let ((new (cons nv cv)))
               (setf (gethash name (fenv-commons env)) new)
               (values new (not cell))))))))

(defun %f7-arg (x)
  "An actual argument as a reference (vector . offset); a plain Lisp value (from a Lisp caller) is boxed."
  (if (consp x) x (cons (vector x) 0)))

(defun %f7-box (x) (cons (vector x) 0))

(defun f77-make-ref (value)
  "A FORTRAN reference holding VALUE, for passing a variable to a FORTRAN routine from Lisp."
  (%f7-box value))
(defun f77-ref-value (ref)
  "The value a FORTRAN reference (as made by F77-MAKE-REF) now holds."
  (svref (car ref) (cdr ref)))

;;; ---- single precision ------------------------------------------------------------------------------------------

(defun %f7-finite-p (x) (and (= x x) (< (abs x) 1.7976931348623157d308)))

(defun %f7-single (x)
  "The double nearest to X with a 24-bit significand (REAL precision)."
  (let ((x (float x 1d0)))
    (if (or (zerop x) (not (%f7-finite-p x)))
        x
        (multiple-value-bind (m e s) (integer-decode-float x)
          (let* ((len (integer-length m)) (drop (max 0 (- len 24))))
            (* s (float (* (round m (expt 2 drop)) (expt 2 drop)) 1d0) (expt 2d0 e)))))))

(defun %f7-cx (x)
  "X as a complex number with double parts."
  (if (complexp x) x (complex (float x 1d0) 0d0)))
(defun %f7-cxs (x)
  "X as a complex number with single-precision parts."
  (let ((z (%f7-cx x))) (complex (%f7-single (realpart z)) (%f7-single (imagpart z)))))

;;; ---- arithmetic ----------------------------------------------------------------------------------------------

(defun %f7-idiv (a b)
  (when (eql b 0) (%f7-rerror "Integer division by zero: ~D/0" a))
  (values (truncate a b)))

(defun %f7-ipow (a b)
  (when (and (> b 0) (> (abs a) 1) (> (* (integer-length a) b) 10000000))
    (%f7-rerror "~D ** ~D is too large to compute" a b))
  (cond ((>= b 0) (expt a b))
        ((eql a 0) (%f7-rerror "0 raised to the negative power ~D" b))
        ((eql a 1) 1)
        ((eql a -1) (if (evenp b) 1 -1))
        (t 0)))

(defun %f7-rpow (x y)
  "X ** Y for real X and Y."
  (cond ((and (integerp y)) (if (and (zerop x) (< y 0)) (%f7-rerror "0.0 raised to the negative power ~D" y) (expt x y)))
        ((zerop x) (if (> y 0) (float 0 x) (%f7-rerror "0.0 raised to the non-positive power ~A" y)))
        ((< x 0) (if (= y (ftruncate y)) (expt x (values (truncate y))) (%f7-rerror "A negative base ~A cannot be raised to the fractional power ~A" x y)))
        (t (expt x y))))

(defun %f7-cpow (z y)
  (cond ((integerp y) (if (zerop z) (if (> y 0) (complex 0d0 0d0) (%f7-rerror "Complex zero to a non-positive power")) (expt z y)))
        ((zerop z) (complex 0d0 0d0))
        (t (expt z y))))

(defun %f7-mod (a b)
  (cond ((and (integerp a) (integerp b)) (when (eql b 0) (%f7-rerror "MOD(~D, 0)" a)) (rem a b))
        (t (when (zerop b) (%f7-rerror "MOD(~A, 0.0)" a)) (- a (* b (values (truncate (/ a b))))))))

(defun %f7-sign (a b)
  "|A| with the sign of B."
  (if (minusp b) (- (abs a)) (abs a)))

(defun %f7-dim (a b) (if (> a b) (- a b) (if (floatp a) 0d0 0)))

(defun %f7-aint (x) (if (%f7-finite-p x) (float (values (truncate x)) 1d0) x))
(defun %f7-anint (x) (if (%f7-finite-p x) (float (if (>= x 0) (values (floor (+ x 1/2))) (values (ceiling (- x 1/2)))) 1d0) x))
(defun %f7-nint (x) (if (>= x 0) (values (floor (+ x 1/2))) (values (ceiling (- x 1/2)))))

(defun %f7-sqrt (x)
  (cond ((complexp x) (sqrt x))
        ((< x 0) (%f7-rerror "SQRT of the negative number ~A" x))
        (t (sqrt (float x 1d0)))))
(defun %f7-log (x)
  (cond ((complexp x) (if (zerop x) (%f7-rerror "LOG of zero") (log x)))
        ((<= x 0) (%f7-rerror "LOG of the non-positive number ~A" x))
        (t (log (float x 1d0)))))
(defun %f7-log10 (x)
  (cond ((<= x 0) (%f7-rerror "LOG10 of the non-positive number ~A" x))
        (t (/ (log (float x 1d0)) (log 10d0)))))
(defun %f7-asin (x) (if (> (abs x) 1) (%f7-rerror "ASIN of ~A, which is outside -1 to 1" x) (asin (float x 1d0))))
(defun %f7-acos (x) (if (> (abs x) 1) (%f7-rerror "ACOS of ~A, which is outside -1 to 1" x) (acos (float x 1d0))))
(defun %f7-atan2 (y x)
  (when (and (zerop x) (zerop y)) (%f7-rerror "ATAN2(0.0, 0.0)"))
  (atan (float y 1d0) (float x 1d0)))

;;; ---- bit operations (the Department of Defense extensions) -----------------------------------------------------

(defun %f7-shift-check (n)
  (unless (and (integerp n) (<= (abs n) 10000000)) (%f7-rerror "The bit position or shift ~A is too large" n))
  n)
(defun %f7-ishft (i n) (ash i (%f7-shift-check n)))
(defun %f7-ishftc (i n size)
  "I rotated by N places within its low SIZE bits."
  (%f7-shift-check size)
  (when (< size 1) (%f7-rerror "ISHFTC needs a size of 1 or more, not ~D" size))
  (let* ((mask (- (ash 1 size) 1)) (low (logand i mask)) (n (mod n size)))
    (logior (logand i (lognot mask)) (logand mask (logior (ash low n) (ash low (- n size)))))))
(defun %f7-ibits (i pos len)
  (%f7-shift-check pos) (%f7-shift-check len)
  (when (< len 0) (%f7-rerror "IBITS needs a length of 0 or more, not ~D" len))
  (logand (ash i (- pos)) (- (ash 1 len) 1)))
(defun %f7-ibset (i pos) (logior i (ash 1 (%f7-shift-check pos))))
(defun %f7-ibclr (i pos) (logand i (lognot (ash 1 (%f7-shift-check pos)))))
(defun %f7-btest (i pos) (logbitp (max 0 (%f7-shift-check pos)) i))
(defun %f7-mvbits (from frompos len to topos)
  "TO with LEN bits starting at TOPOS replaced by LEN bits of FROM starting at FROMPOS."
  (%f7-shift-check len) (%f7-shift-check frompos) (%f7-shift-check topos)
  (when (< len 0) (%f7-rerror "MVBITS needs a length of 0 or more, not ~D" len))
  (let ((mask (- (ash 1 len) 1)))
    (logior (logand to (lognot (ash mask topos))) (ash (logand (ash from (- frompos)) mask) topos))))
(defun %f7-eqv (a b) (lognot (logxor a b)))

(defun %f7-check-bit-range (lo hi)
  (unless (and (integerp lo) (integerp hi) (<= 0 lo) (<= lo hi))
    (%f7-rerror "The bit substring ~A:~A is not a range of bit positions (0 is the least significant bit)" lo hi)))
(defun %f7-bits (i lo hi)
  "Bits LO through HI of the integer I: the bit substring I(LO:HI)."
  (%f7-check-bit-range lo hi)
  (%f7-ibits i lo (+ (- hi lo) 1)))
(defun %f7-deposit-bits (value cur lo hi)
  "CUR with bits LO through HI replaced by the low bits of VALUE: the assignment I(LO:HI) = VALUE."
  (%f7-check-bit-range lo hi)
  (%f7-mvbits value 0 (+ (- hi lo) 1) cur lo))

(defun %f7-ltoi (x) (if x 1 0))

;;; ---- characters ----------------------------------------------------------------------------------------------------

(defun %f7-cfit (s n)
  "The string S padded with blanks or truncated to exactly N characters."
  (let ((len (length s)))
    (cond ((= len n) s)
          ((> len n) (subseq s 0 n))
          (t (let ((r (make-string n :initial-element #\Space))) (replace r s) r)))))

(defun %f7-cstore-len (old rhs n)
  "The length a character store into a slot holding OLD keeps: the existing string's, else the declared N, else RHS's."
  (cond ((stringp old) (length old)) (n n) (t (length rhs))))

(defun %f7-str (x what)
  (cond ((stringp x) x)
        ((eq x 'f77-undefined) (%f7-undef what))
        (t (%f7-rerror "~S is not a character value" x))))

(defun %f7-substr (s lo hi)
  "Characters LO to HI (1-based, inclusive) of S."
  (let ((n (length s)))
    (unless (and (integerp lo) (integerp hi) (>= lo 1) (<= hi n) (>= hi (- lo 1)))
      (%f7-rerror "The substring ~A:~A is outside the character string of length ~D" lo hi n))
    (subseq s (- lo 1) hi)))

(defun %f7-substr-set (s lo hi val)
  "A copy of S with characters LO to HI replaced by VAL (padded or truncated to fit)."
  (let ((n (length s)))
    (unless (and (integerp lo) (integerp hi) (>= lo 1) (<= hi n) (>= hi (- lo 1)))
      (%f7-rerror "The substring ~A:~A is outside the character string of length ~D" lo hi n))
    (let ((r (copy-seq s)) (v (%f7-cfit val (+ (- hi lo) 1))))
      (replace r v :start1 (- lo 1))
      r)))

(defun %f7-ccmp (a b)
  "-1, 0 or 1 comparing the strings A and B as FORTRAN does: the shorter is padded with blanks."
  (let ((la (length a)) (lb (length b)))
    (dotimes (i (max la lb) 0)
      (let ((ca (if (< i la) (char-code (char a i)) 32)) (cb (if (< i lb) (char-code (char b i)) 32)))
        (cond ((< ca cb) (return -1)) ((> ca cb) (return 1)))))))

(defun %f7-index (s sub)
  (let ((p (search sub s))) (if p (+ p 1) 0)))

(defun %f7-char (n)
  (unless (and (integerp n) (<= 0 n 255)) (%f7-rerror "CHAR(~A): the code is outside 0 to 255" n))
  (string (code-char n)))
(defun %f7-ichar (s)
  (if (> (length s) 0) (char-code (char s 0)) (%f7-rerror "ICHAR of an empty string")))

;;; ---- control -------------------------------------------------------------------------------------------------------

(defun %f7-stop (code)
  "STOP: print the code or message (when given) and end the program."
  (when code
    (format (%f7-console) "~&STOP ~A~%" (if (stringp code) code (princ-to-string code))))
  (%f7-exit))

(defun %f7-console ()
  "The stream behind unit 6 of the running program."
  (or (fenv-output (%f7-env)) *standard-output*))

(defun %f7-exit ()
  "End the running FORTRAN program (STOP, CALL EXIT)."
  (if *f77-running*
      (throw '%f77-halt nil)
      (%f7-rerror "STOP: the program was not started by F77:EXECUTE")))

(defun %f7-pause (code)
  (format (%f7-console) "~&PAUSE~@[ ~A~]~%" (and code (if (stringp code) code (princ-to-string code))))
  nil)

(defun %f7-trips (start limit step)
  "The iteration count of DO var = START, LIMIT, STEP."
  (when (zerop step) (%f7-rerror "The step of a DO loop is zero"))
  (let ((n (values (truncate (/ (+ (- limit start) step) step)))))
    (if (> n 0) n 0)))

(defun %f7-wrap-error (c)
  "Give errors signalled by Lisp inside a FORTRAN program the FORTRAN routine and line."
  (when (and (not (typep c 'f77-runtime-error)) *f77-routine*)
    (error 'f77-runtime-error :message (princ-to-string c) :routine *f77-routine* :line *f77-line* :original c)))
