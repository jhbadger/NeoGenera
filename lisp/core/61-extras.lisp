;;;; 61-extras.lisp -- Common Lisp functions found missing by the conformance suite.

(in-package :cl)

(defun signum (x)
  "-1, 0 or 1 (a float of the same type for floats) according to the sign of X."
  (cond ((zerop x) x) ((floatp x) (if (> x 0) 1.0 -1.0)) ((> x 0) 1) (t -1)))

(defun lcm (&rest integers)
  "The least common multiple of INTEGERS (1 for none)."
  (let ((r 1)) (dolist (n integers r) (when (zerop n) (return 0)) (setq r (/ (abs (* r n)) (gcd r n))))))

(let ((%ne #'/=))
  (defun /= (a b &rest more)
    "True when no two arguments are equal."
    (if more
        (let ((all (cons a (cons b more))))
          (do-list-tails (l all) (dolist (y (cdr l)) (when (= (car l) y) (return-from /= nil))))
          t)
        (funcall %ne a b))))

(defun rationalize (x)
  "The simplest rational that converts back to the float X (continued fractions); rationals are returned unchanged."
  (if (rationalp x) x
      (let* ((sign (if (< x 0) -1 1)) (a (abs x)) (p0 0) (q0 1) (p1 1) (q1 0) (r a))
        (dotimes (i 64)
          (let* ((ai (floor r)) (p2 (+ (* ai p1) p0)) (q2 (+ (* ai q1) q0)))
            (setq p0 p1 q0 q1 p1 p2 q1 q2)
            (when (= (float (/ p1 q1)) a) (return))
            (when (= r ai) (return))
            (setq r (/ 1.0 (- r ai)))))
        (* sign (/ p1 q1)))))

(defun logtest (a b) "True when integers A and B have a 1 bit in common." (not (zerop (logand a b))))
(defun logbitp (index integer) "True when bit INDEX (from 0) of INTEGER is 1." (logtest (ash 1 index) integer))
(defun logcount (n)
  "The number of 1 bits of the non-negative integer N (of 0 bits for a negative one)."
  (let ((m (if (< n 0) (lognot n) n)) (c 0)) (loop while (> m 0) do (when (oddp m) (setq c (+ c 1))) (setq m (ash m -1))) c))

(defun arrayp (x) "True when X is an array (vectors and strings are)." (vectorp x))

(defun sublis (alist tree &key test)
  "Substitute in TREE every atom found as a key of ALIST."
  (let ((f (or test #'eql)))
    (labels ((walk (x) (let ((hit (assoc x alist :test f))) (cond (hit (cdr hit)) ((consp x) (cons (walk (car x)) (walk (cdr x)))) (t x)))))
      (walk tree))))

(defun mapcon (fn list &rest more)
  "Apply FN to successive tails and NCONC the results."
  (apply #'nconc (apply #'maplist fn list more)))
