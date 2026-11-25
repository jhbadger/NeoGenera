;;;; 10-random.lisp -- seedable, reproducible random numbers.
;;;;
;;;; The generator is xoshiro128** (32-bit words), seeded through splitmix64, so a given seed gives the same
;;;; stream on every platform.  The default generator is *RNG*; every function takes an optional RNG argument.

(in-package :cl)

(defstruct (rng (:constructor %make-rng (state)) (:copier nil)) state (spare nil))

(defun %splitmix64-step (z)
  "One splitmix64 step on the integer Z; returns (values next-state output)."
  (let* ((z (logand (+ z #x9E3779B97F4A7C15) #xFFFFFFFFFFFFFFFF))
         (x z))
    (setq x (logand (* (logxor x (ash x -30)) #xBF58476D1CE4E5B9) #xFFFFFFFFFFFFFFFF))
    (setq x (logand (* (logxor x (ash x -27)) #x94D049BB133111EB) #xFFFFFFFFFFFFFFFF))
    (values z (logxor x (ash x -31)))))

(defun make-rng (&optional (seed 42))
  "A new generator seeded from the integer SEED."
  (let ((st (make-array 4 :initial-element 0)) (z (logand seed #xFFFFFFFFFFFFFFFF)))
    (dotimes (i 2)
      (multiple-value-bind (z2 out) (%splitmix64-step z)
        (setq z z2)
        (setf (aref st (* 2 i)) (logand out #xFFFFFFFF))
        (setf (aref st (+ 1 (* 2 i))) (ash out -32))))
    (when (and (zerop (aref st 0)) (zerop (aref st 1)) (zerop (aref st 2)) (zerop (aref st 3))) (setf (aref st 0) 1))
    (%make-rng st)))

(defvar *rng* (make-rng 42) "The default random number generator.")

(defun rng-seed (seed &optional (rng *rng*))
  "Reseed RNG with SEED; the stream restarts."
  (let ((new (make-rng seed))) (setf (rng-state rng) (rng-state new) (rng-spare rng) nil) rng))

(defun rng-copy (&optional (rng *rng*))
  "An independent copy of RNG at its present position."
  (let ((c (%make-rng (copy-seq (rng-state rng))))) (setf (rng-spare c) (rng-spare rng)) c))

(defun %rotl32 (x k) (logand (logior (ash x k) (ash x (- k 32))) #xFFFFFFFF))

(defun rand-u32 (&optional (rng *rng*))
  "The next 32 random bits as an integer."
  (let* ((s (rng-state rng)) (s0 (aref s 0)) (s1 (aref s 1)) (s2 (aref s 2)) (s3 (aref s 3))
         (result (logand (* (%rotl32 (logand (* s1 5) #xFFFFFFFF) 7) 9) #xFFFFFFFF))
         (tt (logand (ash s1 9) #xFFFFFFFF)))
    (setq s2 (logxor s2 s0) s3 (logxor s3 s1) s1 (logxor s1 s2) s0 (logxor s0 s3) s2 (logxor s2 tt) s3 (%rotl32 s3 11))
    (setf (aref s 0) s0 (aref s 1) s1 (aref s 2) s2 (aref s 3) s3)
    result))

(defun rand-uniform (&optional (rng *rng*))
  "A float uniformly distributed in [0, 1) with 53 random bits."
  (let ((a (ash (rand-u32 rng) -5)) (b (ash (rand-u32 rng) -6)))
    (/ (+ (* a 67108864.0) b) 9007199254740992.0)))

(defun rand-range (lo hi &optional (rng *rng*))
  "A float uniformly distributed in [LO, HI)."
  (+ lo (* (- hi lo) (rand-uniform rng))))

(defun rand-int (n &optional (rng *rng*))
  "An integer uniformly distributed in [0, N), without modulo bias."
  (cond ((<= n 1) 0)
        ((<= n 4294967296)
         (let ((limit (- 4294967296 (mod 4294967296 n))))
           (loop (let ((r (rand-u32 rng))) (when (< r limit) (return (mod r n)))))))
        (t (let* ((bits (integer-length n)) (words (ceiling bits 32)))
             (loop (let ((r 0))
                     (dotimes (i words) (setq r (+ (ash r 32) (rand-u32 rng))))
                     (setq r (logand r (- (ash 1 bits) 1)))
                     (when (< r n) (return r))))))))

(defun rand-bool (&optional (p 0.5) (rng *rng*))
  "True with probability P."
  (< (rand-uniform rng) p))

(defun rand-normal (&optional (mean 0.0) (sd 1.0) (rng *rng*))
  "A normally distributed float (Marsaglia polar method)."
  (let ((spare (rng-spare rng)))
    (if spare
        (progn (setf (rng-spare rng) nil) (+ mean (* sd spare)))
        (loop (let* ((u (- (* 2.0 (rand-uniform rng)) 1.0)) (v (- (* 2.0 (rand-uniform rng)) 1.0)) (s (+ (* u u) (* v v))))
                (when (and (< s 1.0) (> s 0.0))
                  (let ((m (sqrt (/ (* -2.0 (log s)) s))))
                    (setf (rng-spare rng) (* v m))
                    (return (+ mean (* sd u m))))))))))

(defun rand-exponential (&optional (rate 1.0) (rng *rng*))
  "An exponentially distributed float with the given RATE."
  (/ (- (log (- 1.0 (rand-uniform rng)))) rate))

(defun rand-gamma (shape &optional (scale 1.0) (rng *rng*))
  "A gamma-distributed float (Marsaglia-Tsang)."
  (if (< shape 1.0)
      (* (rand-gamma (+ shape 1.0) scale rng) (expt (rand-uniform rng) (/ 1.0 shape)))
      (let* ((d (- shape (/ 1.0 3.0))) (c (/ 1.0 (sqrt (* 9.0 d)))))
        (loop (let* ((x (rand-normal 0.0 1.0 rng)) (v (expt (+ 1.0 (* c x)) 3)))
                (when (> v 0.0)
                  (let ((u (rand-uniform rng)))
                    (when (or (< u (- 1.0 (* 0.0331 x x x x)))
                              (< (log u) (+ (* 0.5 x x) (* d (+ (- 1.0 v) (log v))))))
                      (return (* d v scale))))))))))

(defun rand-beta (a b &optional (rng *rng*))
  "A beta-distributed float."
  (let ((x (rand-gamma a 1.0 rng)) (y (rand-gamma b 1.0 rng))) (/ x (+ x y))))

(defun rand-chisq (df &optional (rng *rng*)) "A chi-square distributed float." (rand-gamma (/ df 2.0) 2.0 rng))
(defun rand-t (df &optional (rng *rng*)) "A Student t distributed float."
  (/ (rand-normal 0.0 1.0 rng) (sqrt (/ (rand-chisq df rng) df))))

(defun rand-binomial (n p &optional (rng *rng*))
  "A binomially distributed integer (sum of Bernoulli trials; exact)."
  (let ((k 0)) (dotimes (i n) (when (< (rand-uniform rng) p) (setq k (+ k 1)))) k))

(defun rand-poisson (lambda* &optional (rng *rng*))
  "A Poisson distributed integer (Knuth's method on pieces of at most 30)."
  (let ((total 0) (rest lambda*))
    (loop while (> rest 0.0)
          do (let* ((l (min rest 30.0)) (limit (exp (- l))) (k 0) (p (rand-uniform rng)))
               (loop while (> p limit) do (setq k (+ k 1) p (* p (rand-uniform rng))))
               (setq total (+ total k) rest (- rest l))))
    total))

(defun rand-geometric (p &optional (rng *rng*))
  "Number of failures before the first success with probability P."
  (floor (/ (log (- 1.0 (rand-uniform rng))) (log (- 1.0 p)))))

(defun shuffle (sequence &optional (rng *rng*))
  "A new vector with the elements of SEQUENCE in random order (Fisher-Yates)."
  (let* ((v (copy-seq (coerce sequence 'vector))) (n (length v)))
    (loop for i from (- n 1) downto 1
          do (let* ((j (rand-int (+ i 1) rng)) (tmp (aref v i))) (setf (aref v i) (aref v j)) (setf (aref v j) tmp)))
    v))

(defun sample (sequence k &optional (rng *rng*))
  "K distinct elements of SEQUENCE chosen at random, as a vector."
  (subseq (shuffle sequence rng) 0 k))

(defun sample-with-replacement (sequence k &optional (rng *rng*))
  "K elements of SEQUENCE drawn independently, as a vector."
  (let* ((v (coerce sequence 'vector)) (n (length v)) (out (make-array k :initial-element nil)))
    (dotimes (i k) (setf (aref out i) (aref v (rand-int n rng))))
    out))

(defun rand-choice (sequence &optional (rng *rng*))
  "One random element of SEQUENCE."
  (let ((v (coerce sequence 'vector))) (aref v (rand-int (length v) rng))))
