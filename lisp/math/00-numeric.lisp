;;;; 00-numeric.lisp -- numeric extensions: hyperbolic functions, IEEE helpers, complex numbers,
;;;; special functions used by the statistics code.
;;;;
;;;; Complex numbers are a library type (struct CX printed as #C(re im)); the arithmetic operators
;;;; + - * / stay real-only, the C+ C- C* C/ family accepts reals and complex numbers.

(in-package :cl)

;;; ---- IEEE 754 helpers -------------------------------------------------------------------------------------

(defvar +infinity+ (exp 1000.0) "Positive floating-point infinity.")
(defvar -infinity+ (- (exp 1000.0)) "Negative floating-point infinity.")
(defvar +nan+ (- (exp 1000.0) (exp 1000.0)) "A quiet NaN.")

(defun float-nan-p (x)
  "True when X is a NaN."
  (and (floatp x) (string= (princ-to-string x) "#<NaN>")))

(defun float-infinity-p (x)
  "True when X is a floating-point infinity."
  (and (floatp x) (= x x) (or (> x 1.7976931348623157e308) (< x -1.7976931348623157e308))))

(defun %sdiv (a b)
  "A / B for floats following IEEE rules: division by zero gives an infinity (or NaN for 0/0)."
  (if (zerop b)
      (cond ((zerop a) +nan+) ((> a 0) +infinity+) (t -infinity+))
      (/ a b)))

(defun finite-p (x)
  "True when X is a number that is neither NaN nor infinite."
  (and (realp x) (not (float-nan-p x)) (not (float-infinity-p x))))

(defun ffloor (x) "Largest integral float not above X." (float (floor x)))
(defun fceiling (x) "Smallest integral float not below X." (float (ceiling x)))
(defun fround (x) "Nearest integral float to X (ties to even)." (float (round x)))
(defun ftruncate (x) "Integral float of X truncated toward zero." (float (truncate x)))

(defun %decode-float (x)
  "Returns (values mantissa exponent sign) with X = sign * mantissa * 2^exponent, mantissa an integer of 53 bits."
  (cond ((zerop x) (values 0 0 1))
        (t (let ((sign (if (< x 0) -1 1)) (a (abs x)) (e 0))
             (loop while (>= a 9007199254740992.0) do (setq a (/ a 2.0) e (+ e 1)))
             (loop while (< a 4503599627370496.0) do (setq a (* a 2.0) e (- e 1)))
             (values (floor a) e sign)))))

(defun integer-decode-float (x)
  "Returns (values integer-mantissa exponent sign) for the float X."
  (%decode-float x))

(defun float-digits (x) "Number of bits in the float mantissa (53)." (declare (ignore x)) 53)

(defun scale-float (x k)
  "X multiplied by 2^K."
  (* x (expt 2.0 k)))

(defconstant +double-epsilon+ 2.220446049250313e-16 "Machine epsilon for double-precision floats.")

(defun approx= (a b &optional (rel 1e-9) (abs 1e-12))
  "True when A and B agree within relative tolerance REL or absolute tolerance ABS."
  (or (= a b) (<= (abs (- a b)) (max abs (* rel (max (abs a) (abs b)))))))

;;; ---- hyperbolic functions ---------------------------------------------------------------------------------------

(defun sinh (x) "Hyperbolic sine."
  (if (< (abs x) 1e-5) (+ x (/ (* x x x) 6.0))
      (/ (- (exp x) (exp (- x))) 2.0)))
(defun cosh (x) "Hyperbolic cosine." (/ (+ (exp x) (exp (- x))) 2.0))
(defun tanh (x) "Hyperbolic tangent."
  (cond ((> x 20.0) 1.0) ((< x -20.0) -1.0)
        (t (let ((e (exp (* 2.0 x)))) (/ (- e 1.0) (+ e 1.0))))))
(defun asinh (x) "Inverse hyperbolic sine."
  (if (< x 0) (- (asinh (- x))) (log (+ x (sqrt (+ (* x x) 1.0))))))
(defun acosh (x) "Inverse hyperbolic cosine (X >= 1)." (log (+ x (sqrt (- (* x x) 1.0)))))
(defun atanh (x) "Inverse hyperbolic tangent (|X| < 1)." (* 0.5 (log (/ (+ 1.0 x) (- 1.0 x)))))

(defun log1p (x) "log(1+X), accurate for small X."
  (if (< (abs x) 1e-4) (- x (/ (* x x) 2.0) (- (/ (* x x x) 3.0))) (log (+ 1.0 x))))
(defun expm1 (x) "exp(X)-1, accurate for small X."
  (if (< (abs x) 1e-5) (+ x (/ (* x x) 2.0) (/ (* x x x) 6.0)) (- (exp x) 1.0)))

(defun logtest (a b) "True when integers A and B have a 1 bit in common." (not (zerop (logand a b))))

(defun sign (x) "-1, 0 or 1 according to the sign of X." (cond ((> x 0) 1) ((< x 0) -1) (t 0)))
(defun square (x) "X multiplied by itself." (* x x))
(defun cube (x) "X cubed." (* x x x))

;;; ---- complex numbers ------------------------------------------------------------------------------------------

(defstruct (cx (:constructor %make-cx (re im)) (:copier nil)) re im)

(defmethod print-object ((z cx) stream)
  (format stream "#C(~A ~A)" (cx-re z) (cx-im z)))

(defun complex (re &optional (im 0))
  "A complex number; a real number when IM is the exact integer 0."
  (if (and (integerp im) (zerop im)) re (%make-cx re im)))

(defun complexp (x) "True when X is a complex number." (cx-p x))
(defun realpart (z) "Real part of Z." (if (cx-p z) (cx-re z) z))
(defun imagpart (z) "Imaginary part of Z." (if (cx-p z) (cx-im z) 0))
(defun conjugate (z) "Complex conjugate of Z." (if (cx-p z) (%make-cx (cx-re z) (- (cx-im z))) z))

(defun cabs (z) "Magnitude of Z."
  (if (cx-p z)
      (let ((a (abs (cx-re z))) (b (abs (cx-im z))))
        (cond ((zerop a) b) ((zerop b) a)
              ((> a b) (let ((r (/ b a))) (* a (sqrt (+ 1.0 (* r r))))))
              (t (let ((r (/ a b))) (* b (sqrt (+ 1.0 (* r r))))))))
      (abs z)))

(defun phase (z) "Argument (angle) of Z in radians."
  (if (cx-p z) (atan (cx-im z) (cx-re z)) (if (< z 0) pi 0.0)))

(defun %simplify-c (re im) (if (and (rationalp im) (zerop im)) re (%make-cx re im)))

(defun c+ (a b) "Sum of two numbers, real or complex."
  (if (or (cx-p a) (cx-p b)) (%simplify-c (+ (realpart a) (realpart b)) (+ (imagpart a) (imagpart b))) (+ a b)))
(defun c- (a &optional (b nil bp)) "Difference (or negation) of numbers, real or complex."
  (cond ((not bp) (if (cx-p a) (%make-cx (- (cx-re a)) (- (cx-im a))) (- a)))
        ((or (cx-p a) (cx-p b)) (%simplify-c (- (realpart a) (realpart b)) (- (imagpart a) (imagpart b))))
        (t (- a b))))
(defun c* (a b) "Product of two numbers, real or complex."
  (if (or (cx-p a) (cx-p b))
      (let ((ar (realpart a)) (ai (imagpart a)) (br (realpart b)) (bi (imagpart b)))
        (%simplify-c (- (* ar br) (* ai bi)) (+ (* ar bi) (* ai br))))
      (* a b)))
(defun c/ (a b) "Quotient of two numbers, real or complex."
  (if (or (cx-p a) (cx-p b))
      (let* ((ar (realpart a)) (ai (imagpart a)) (br (realpart b)) (bi (imagpart b)) (d (+ (* br br) (* bi bi))))
        (%simplify-c (/ (+ (* ar br) (* ai bi)) d) (/ (- (* ai br) (* ar bi)) d)))
      (/ a b)))
(defun c= (a b) "True when two numbers (real or complex) are equal."
  (and (= (realpart a) (realpart b)) (= (imagpart a) (imagpart b))))

(defun cexp (z) "e raised to the complex power Z."
  (let ((m (exp (realpart z))) (y (imagpart z))) (%simplify-c (* m (cos y)) (* m (sin y)))))
(defun clog (z) "Natural logarithm of the complex number Z (principal branch)."
  (%simplify-c (log (cabs z)) (phase z)))
(defun csqrt (z) "Principal square root; negative reals give imaginary results."
  (let ((re (realpart z)) (im (imagpart z)))
    (if (and (rationalp im) (zerop im))
        (if (>= re 0) (sqrt re) (%make-cx 0.0 (sqrt (- re))))
        (let* ((m (cabs z)) (a (sqrt (/ (+ m re) 2.0))) (b (sqrt (/ (- m re) 2.0))))
          (%make-cx a (if (< im 0) (- b) b))))))
(defun cexpt (z w) "Z raised to the power W (either may be complex)."
  (cond ((and (not (cx-p z)) (not (cx-p w))) (expt z w))
        ((and (integerp w) (>= w 0)) (let ((r 1)) (dotimes (i w) (setq r (c* r z))) r))
        ((c= z 0) 0)
        (t (cexp (c* w (clog z))))))
(defun csin (z) "Complex sine."
  (%simplify-c (* (sin (realpart z)) (cosh (imagpart z))) (* (cos (realpart z)) (sinh (imagpart z)))))
(defun ccos (z) "Complex cosine."
  (%simplify-c (* (cos (realpart z)) (cosh (imagpart z))) (- (* (sin (realpart z)) (sinh (imagpart z))))))

(let ((%real-sqrt #'sqrt) (%real-log #'log))
  (defun sqrt (x)
    "Square root; a negative real argument returns a complex number."
    (cond ((cx-p x) (csqrt x))
          ((and (realp x) (< x 0)) (%make-cx 0.0 (funcall %real-sqrt (- x))))
          (t (funcall %real-sqrt x))))
  (defun log (x &optional (base nil bp))
    "Natural logarithm, or logarithm to BASE; negative reals and complex numbers give complex results."
    (let ((v (cond ((cx-p x) (clog x))
                   ((and (realp x) (< x 0)) (%make-cx (funcall %real-log (- x)) pi))
                   (t (funcall %real-log x)))))
      (if bp (c/ v (log base)) v))))

(defun polynomial-roots (coeffs)
  "Roots of the polynomial with COEFFS (highest degree first) by the Durand-Kerner method; a list of numbers."
  (let* ((cs (let ((l (coerce coeffs 'list))) (loop while (and l (zerop (car l))) do (setq l (cdr l))) l))
         (n (- (length cs) 1)))
    (if (< n 1) nil
        (let* ((lead (float (car cs))) (a (mapcar (lambda (c) (/ c lead)) cs))
               (roots (let ((acc nil))
                        (dotimes (k n) (setq acc (cons (cexpt (%make-cx 0.4 0.9) k) acc)))
                        (reverse acc))))
          (dotimes (iter 500)
            (let ((delta 0.0) (new nil))
              (dolist (r roots)
                (let ((p (let ((v 0)) (dolist (c a) (setq v (c+ (c* v r) c))) v)) (q 1))
                  (dolist (s roots) (unless (eq s r) (setq q (c* q (c- r s)))))
                  (let ((step (if (c= q 0) 0.0 (c/ p q))))
                    (setq delta (max delta (cabs step)))
                    (setq new (cons (c- r step) new)))))
              (setq roots (reverse new))
              (when (< delta 1e-14) (return))))
          (mapcar (lambda (r) (if (< (abs (imagpart r)) 1e-9) (float (realpart r)) r)) roots)))))

;;; ---- special functions -------------------------------------------------------------------------------------------

(defvar *lanczos* '(0.99999999999980993 676.5203681218851 -1259.1392167224028 771.32342877765313
                    -176.61502916214059 12.507343278686905 -0.13857109526572012 9.9843695780195716e-6 1.5056327351493116e-7))

(defun log-gamma (x)
  "Natural logarithm of the gamma function for X > 0 (Lanczos approximation, ~15 digits)."
  (if (< x 0.5)
      (- (log (/ pi (abs (sin (* pi x))))) (log-gamma (- 1.0 x)))
      (let* ((x (- x 1.0)) (a (car *lanczos*)) (tt (+ x 7.5)))
        (let ((i 1)) (dolist (c (cdr *lanczos*)) (setq a (+ a (/ c (+ x i)))) (setq i (+ i 1))))
        (+ (* 0.5 (log (* 2.0 pi))) (* (+ x 0.5) (log tt)) (- tt) (log a)))))

(defun gamma-function (x)
  "The gamma function; exact factorials for small positive integers."
  (cond ((and (integerp x) (> x 0) (< x 171)) (let ((f 1)) (loop for i from 2 below x do (setq f (* f i))) (float f)))
        ((< x 0.5) (/ pi (* (sin (* pi x)) (gamma-function (- 1.0 x)))))
        (t (exp (log-gamma x)))))

(defun log-beta (a b) "Logarithm of the beta function." (- (+ (log-gamma a) (log-gamma b)) (log-gamma (+ a b))))
(defun log-factorial (n) "Logarithm of N!." (if (< n 171) (log (float (let ((f 1)) (loop for i from 2 to n do (setq f (* f i))) f))) (log-gamma (+ n 1.0))))
(defun log-choose (n k) "Logarithm of the binomial coefficient." (- (log-factorial n) (log-factorial k) (log-factorial (- n k))))
(defun choose (n k) "Binomial coefficient (exact integer)."
  (if (or (< k 0) (> k n)) 0
      (let ((r 1) (k (min k (- n k)))) (dotimes (i k) (setq r (/ (* r (- n i)) (+ i 1)))) r)))

(defun erf (x)
  "The error function."
  (cond ((< x 0) (- (erf (- x))))
        ((< x 2.5) ; Maclaurin series
         (let ((sum x) (term x) (n 0))
           (loop (setq n (+ n 1))
                 (setq term (/ (* (- term) x x) n))
                 (let ((d (/ term (+ (* 2 n) 1)))) (setq sum (+ sum d)) (when (< (abs d) 1e-17) (return))))
           (* sum (/ 2.0 (sqrt pi)))))
        (t (- 1.0 (erfc x)))))

(defun erfc (x)
  "The complementary error function."
  (cond ((< x 2.5) (- 1.0 (erf x)))
        (t ; continued fraction
         (let ((f 0.0))
           (loop for k from 60 downto 1 do (setq f (/ (* k 0.5) (+ x f))))
           (/ (exp (- (* x x))) (* (sqrt pi) (+ x f)))))))

(defun %gamma-p-series (a x)
  (let ((sum (/ 1.0 a)) (term (/ 1.0 a)) (n a))
    (loop (setq n (+ n 1.0) term (* term (/ x n)) sum (+ sum term))
          (when (< (abs term) (* (abs sum) 1e-16)) (return)))
    (* sum (exp (- (* a (log x)) x (log-gamma a))))))

(defun %gamma-q-cf (a x)
  (let* ((tiny 1e-300) (b (+ x 1.0 (- a))) (c (/ 1.0 tiny)) (d (/ 1.0 b)) (h d))
    (loop for i from 1 to 500
          do (let ((an (* (- i) (- i a))))
               (setq b (+ b 2.0))
               (setq d (+ (* an d) b)) (when (< (abs d) tiny) (setq d tiny))
               (setq c (+ b (/ an c))) (when (< (abs c) tiny) (setq c tiny))
               (setq d (/ 1.0 d))
               (let ((del (* d c))) (setq h (* h del)) (when (< (abs (- del 1.0)) 1e-16) (return)))))
    (* h (exp (- (* a (log x)) x (log-gamma a))))))

(defun gamma-p (a x)
  "Regularized lower incomplete gamma function P(A, X)."
  (cond ((<= x 0) 0.0)
        ((< x (+ a 1.0)) (%gamma-p-series a x))
        (t (- 1.0 (%gamma-q-cf a x)))))

(defun gamma-q (a x)
  "Regularized upper incomplete gamma function Q(A, X) = 1 - P(A, X)."
  (cond ((<= x 0) 1.0)
        ((< x (+ a 1.0)) (- 1.0 (%gamma-p-series a x)))
        (t (%gamma-q-cf a x))))

(defun %beta-cf (a b x)
  (let* ((tiny 1e-300) (qab (+ a b)) (qap (+ a 1.0)) (qam (- a 1.0)) (c 1.0) (d (- 1.0 (/ (* qab x) qap))))
    (when (< (abs d) tiny) (setq d tiny))
    (setq d (/ 1.0 d))
    (let ((h d))
      (loop for m from 1 to 1000
            do (let* ((m2 (* 2 m)) (aa (/ (* m (- b m) x) (* (+ qam m2) (+ a m2)))))
                 (setq d (+ 1.0 (* aa d))) (when (< (abs d) tiny) (setq d tiny))
                 (setq c (+ 1.0 (/ aa c))) (when (< (abs c) tiny) (setq c tiny))
                 (setq d (/ 1.0 d)) (setq h (* h d c))
                 (setq aa (/ (* (- (+ a m)) (+ qab m) x) (* (+ a m2) (+ qap m2))))
                 (setq d (+ 1.0 (* aa d))) (when (< (abs d) tiny) (setq d tiny))
                 (setq c (+ 1.0 (/ aa c))) (when (< (abs c) tiny) (setq c tiny))
                 (setq d (/ 1.0 d))
                 (let ((del (* d c))) (setq h (* h del)) (when (< (abs (- del 1.0)) 1e-16) (return)))))
      h)))

(defun beta-inc (a b x)
  "Regularized incomplete beta function I_X(A, B)."
  (cond ((<= x 0) 0.0) ((>= x 1) 1.0)
        (t (let ((bt (exp (+ (- (log-gamma (+ a b)) (log-gamma a) (log-gamma b)) (* a (log x)) (* b (log (- 1.0 x)))))))
             (if (< x (/ (+ a 1.0) (+ a b 2.0)))
                 (/ (* bt (%beta-cf a b x)) a)
                 (- 1.0 (/ (* bt (%beta-cf b a (- 1.0 x))) b)))))))
