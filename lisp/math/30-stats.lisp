;;;; 30-stats.lisp -- descriptive statistics and probability distributions.
;;;;
;;;; Data arguments are lists or vectors of real numbers.  Distribution functions follow R's naming:
;;;; DNORM (density), PNORM (cumulative, :LOWER NIL for the upper tail), QNORM (quantile); the same prefixes
;;;; for T, CHISQ, F, GAMMA, BETA, BINOM, POIS, EXP, UNIF and HYPER.  Sampling is in 10-random.lisp.

(in-package :cl)

(defun %seq->list (x) (if (listp x) x (coerce x 'list)))
(defun %seq->vec (x) (if (vectorp x) x (coerce x 'vector)))

;;; ---- descriptive statistics -------------------------------------------------------------------------------------

(defun sum (data) "Sum of the numbers in DATA." (let ((s 0.0)) (dolist (x (%seq->list data)) (setq s (+ s x))) s))
(defun mean (data) "Arithmetic mean of DATA." (let ((l (%seq->list data))) (/ (sum l) (length l))))
(defun weighted-mean (data weights)
  "Mean of DATA weighted by WEIGHTS."
  (/ (sum (mapcar #'* (%seq->list data) (%seq->list weights))) (sum weights)))
(defun geometric-mean (data) "Geometric mean of positive DATA." (exp (mean (mapcar #'log (%seq->list data)))))
(defun harmonic-mean (data) "Harmonic mean of positive DATA." (let ((l (%seq->list data))) (/ (length l) (sum (mapcar (lambda (x) (/ 1.0 x)) l)))))

(defun variance (data)
  "Sample variance (divisor n-1) of DATA, computed with a two-pass algorithm."
  (let* ((l (%seq->list data)) (n (length l)) (m (mean l)) (s 0.0) (c 0.0))
    (when (< n 2) (error "Variance needs at least two observations"))
    (dolist (x l) (let ((d (- x m))) (setq s (+ s (* d d)) c (+ c d))))
    (/ (- s (/ (* c c) n)) (- n 1))))
(defun std-dev (data) "Sample standard deviation of DATA." (sqrt (variance data)))
(defun std-error (data) "Standard error of the mean of DATA." (/ (std-dev data) (sqrt (float (length (%seq->list data))))))
(defun population-variance (data) "Population variance (divisor n)." (let ((n (length (%seq->list data)))) (* (variance data) (/ (- n 1.0) n))))

(defun minimum (data) "Smallest element of DATA." (reduce #'min (%seq->list data)))
(defun maximum (data) "Largest element of DATA." (reduce #'max (%seq->list data)))
(defun data-range (data) "A list (min max) for DATA." (list (minimum data) (maximum data)))

(defun %sorted-vector (data) (sort (copy-seq (%seq->vec data)) #'<))

(defun quantile (data p)
  "The P-quantile (0 <= P <= 1) of DATA by linear interpolation (R's type 7)."
  (let* ((v (%sorted-vector data)) (n (length v)) (h (* (- n 1) p)) (lo (floor h)) (hi (min (- n 1) (ceiling h))))
    (+ (aref v lo) (* (- h lo) (- (aref v hi) (aref v lo))))))
(defun median (data) "Median of DATA." (quantile data 0.5))
(defun iqr (data) "Interquartile range of DATA." (- (quantile data 0.75) (quantile data 0.25)))
(defun mad (data) "Median absolute deviation of DATA, scaled to estimate the standard deviation." (let ((m (median data))) (* 1.482602218505602 (median (mapcar (lambda (x) (abs (- x m))) (%seq->list data))))))
(defun five-number-summary (data) "Minimum, lower quartile, median, upper quartile, maximum of DATA." (list (minimum data) (quantile data 0.25) (median data) (quantile data 0.75) (maximum data)))
(defun trimmed-mean (data fraction) "Mean of DATA after trimming FRACTION from each tail."
  (let* ((v (%sorted-vector data)) (n (length v)) (k (floor (* n fraction)))) (mean (subseq v k (- n k)))))

(defun mode-value (data)
  "The most frequent value of DATA (the smallest one on ties)."
  (let ((h (make-hash-table :test 'equal)) (best nil) (bc 0))
    (dolist (x (%seq->list data)) (setf (gethash x h) (+ 1 (gethash x h 0))))
    (maphash (lambda (k c) (when (or (> c bc) (and (= c bc) (or (null best) (and (realp k) (< k best))))) (setq best k bc c))) h)
    best))

(defun skewness (data)
  "Sample skewness (moment coefficient g1) of DATA."
  (let* ((l (%seq->list data)) (n (length l)) (m (mean l)) (m2 0.0) (m3 0.0))
    (dolist (x l) (let ((d (- x m))) (setq m2 (+ m2 (* d d)) m3 (+ m3 (* d d d)))))
    (/ (/ m3 n) (expt (/ m2 n) 1.5))))
(defun kurtosis (data)
  "Excess kurtosis of DATA (0 for a normal distribution)."
  (let* ((l (%seq->list data)) (n (length l)) (m (mean l)) (m2 0.0) (m4 0.0))
    (dolist (x l) (let ((d (- x m))) (setq m2 (+ m2 (* d d)) m4 (+ m4 (* d d d d)))))
    (- (/ (/ m4 n) (square (/ m2 n))) 3.0)))

(defun covariance (x y)
  "Sample covariance of X and Y."
  (let* ((a (%seq->list x)) (b (%seq->list y)) (n (length a)) (ma (mean a)) (mb (mean b)) (s 0.0))
    (loop for u in a for v in b do (setq s (+ s (* (- u ma) (- v mb)))))
    (/ s (- n 1))))
(defun correlation (x y)
  "Pearson correlation coefficient of X and Y."
  (/ (covariance x y) (* (std-dev x) (std-dev y))))

(defun ranks (data)
  "Ranks of DATA (1 = smallest); tied values get the average of their ranks.  Returns a list of floats."
  (let* ((v (%seq->vec data)) (n (length v)) (idx (sort (loop for i from 0 below n collect i) (lambda (i j) (< (aref v i) (aref v j)))))
         (r (make-array n :initial-element 0.0)) (i 0) (iv (coerce idx 'vector)))
    (loop while (< i n)
          do (let ((j i))
               (loop while (and (< (+ j 1) n) (= (aref v (aref iv (+ j 1))) (aref v (aref iv i)))) do (setq j (+ j 1)))
               (let ((avg (/ (+ i j 2.0) 2.0))) (loop for k from i to j do (setf (aref r (aref iv k)) avg)))
               (setq i (+ j 1))))
    (coerce r 'list)))

(defun spearman (x y) "Spearman rank correlation of X and Y." (correlation (ranks x) (ranks y)))

(defun cumulative-sum (data) "Running totals of DATA as a list." (let ((s 0.0)) (mapcar (lambda (x) (setq s (+ s x))) (%seq->list data))))
(defun standardize (data) "DATA as z-scores." (let ((m (mean data)) (s (std-dev data))) (mapcar (lambda (x) (/ (- x m) s)) (%seq->list data))))

(defun correlation-matrix (columns)
  "The Pearson correlation matrix of a list of data columns."
  (let* ((cs (coerce columns 'list)) (n (length cs)) (m (make-matrix n n)))
    (dotimes (i n) (dotimes (j n) (setf (mref m i j) (if (= i j) 1.0 (correlation (nth i cs) (nth j cs)))))) m))

(defun histogram-counts (data bins &optional lo hi)
  "Counts of DATA in BINS equal-width bins over [LO, HI]; returns (values counts edges)."
  (let* ((l (%seq->list data)) (lo (or lo (minimum l))) (hi (or hi (maximum l))) (w (/ (- hi lo) (float bins)))
         (counts (make-array bins :initial-element 0)) (edges (make-array (+ bins 1) :initial-element 0.0)))
    (when (zerop w) (setq w 1.0))
    (dotimes (i (+ bins 1)) (setf (aref edges i) (+ lo (* i w))))
    (dolist (x l) (let ((k (min (- bins 1) (max 0 (floor (/ (- x lo) w)))))) (incf (aref counts k))))
    (values counts edges)))

(defun describe-data (data)
  "A plist of summary statistics for DATA (:n :mean :sd :min :q1 :median :q3 :max)."
  (let ((l (%seq->list data)))
    (list :n (length l) :mean (mean l) :sd (if (> (length l) 1) (std-dev l) nil) :min (minimum l) :q1 (quantile l 0.25)
          :median (median l) :q3 (quantile l 0.75) :max (maximum l))))

;;; ---- distribution helpers ---------------------------------------------------------------------------------------

(defun %tail (p lower) (if lower p (- 1.0 p)))

(defun %solve-monotone (cdf p lo hi)
  "The X in [LO, HI] where the nondecreasing function CDF reaches P (bisection, then 60 more halvings)."
  (let ((a lo) (b hi))
    (dotimes (i 200)
      (let ((mid (* 0.5 (+ a b))))
        (if (< (funcall cdf mid) p) (setq a mid) (setq b mid))
        (when (<= (- b a) (* 1e-15 (max 1.0 (abs mid)))) (return))))
    (* 0.5 (+ a b))))

(defun %bracket-up (cdf p start)
  (let ((hi start)) (loop while (and (< (funcall cdf hi) p) (< hi 1e300)) do (setq hi (* hi 2.0))) hi))

;;; ---- normal ----------------------------------------------------------------------------------------------------------

(defun dnorm (x &key (mean 0.0) (sd 1.0))
  "Normal density."
  (let ((z (/ (- x mean) sd))) (/ (exp (* -0.5 z z)) (* sd (sqrt (* 2.0 pi))))))

(defun pnorm (x &key (mean 0.0) (sd 1.0) (lower t))
  "Normal cumulative distribution function."
  (let* ((z (/ (- x mean) sd)) (p (* 0.5 (erfc (/ (- z) (sqrt 2.0))))))
    (if lower p (* 0.5 (erfc (/ z (sqrt 2.0)))))))

(defun qnorm (p &key (mean 0.0) (sd 1.0) (lower t))
  "Normal quantile function (Acklam's approximation refined by two Halley steps)."
  (let ((p (%tail p lower)))
    (cond ((<= p 0.0) -infinity+) ((>= p 1.0) +infinity+)
          (t (let* ((a '(-39.69683028665376 220.9460984245205 -275.9285104469687 138.357751867269 -30.66479806614716 2.506628277459239))
                    (b '(-54.47609879822406 161.5858368580409 -155.6989798598866 66.80131188771972 -13.28068155288572))
                    (c '(-0.007784894002430293 -0.3223964580411365 -2.400758277161838 -2.549732539343734 4.374664141464968 2.938163982698783))
                    (d '(0.007784695709041462 0.3224671290700398 2.445134137142996 3.754408661907416))
                    (x (cond ((< p 0.02425) (let ((q (sqrt (* -2.0 (log p)))))
                                              (/ (+ (* (+ (* (+ (* (+ (* (+ (* (nth 0 c) q) (nth 1 c)) q) (nth 2 c)) q) (nth 3 c)) q) (nth 4 c)) q) (nth 5 c))
                                                 (+ (* (+ (* (+ (* (+ (* (nth 0 d) q) (nth 1 d)) q) (nth 2 d)) q) (nth 3 d)) q) 1.0))))
                             ((> p (- 1.0 0.02425)) (let ((q (sqrt (* -2.0 (log (- 1.0 p))))))
                                                      (- (/ (+ (* (+ (* (+ (* (+ (* (+ (* (nth 0 c) q) (nth 1 c)) q) (nth 2 c)) q) (nth 3 c)) q) (nth 4 c)) q) (nth 5 c))
                                                            (+ (* (+ (* (+ (* (+ (* (nth 0 d) q) (nth 1 d)) q) (nth 2 d)) q) (nth 3 d)) q) 1.0)))))
                             (t (let* ((q (- p 0.5)) (r (* q q)))
                                  (/ (* (+ (* (+ (* (+ (* (+ (* (+ (* (nth 0 a) r) (nth 1 a)) r) (nth 2 a)) r) (nth 3 a)) r) (nth 4 a)) r) (nth 5 a)) q)
                                     (+ (* (+ (* (+ (* (+ (* (+ (* (nth 0 b) r) (nth 1 b)) r) (nth 2 b)) r) (nth 3 b)) r) (nth 4 b)) r) 1.0)))))))
               (dotimes (i 2)
                 (let* ((e (- (pnorm x) p)) (u (* e (sqrt (* 2.0 pi)) (exp (* 0.5 x x)))))
                   (setq x (- x (/ u (+ 1.0 (* 0.5 x u)))))))
               (+ mean (* sd x)))))))

;;; ---- Student t ------------------------------------------------------------------------------------------------------

(defun dt (x df)
  "Student t density with DF degrees of freedom."
  (exp (- (log-gamma (/ (+ df 1.0) 2.0)) (log-gamma (/ df 2.0)) (* 0.5 (log (* df pi))) (* (/ (+ df 1.0) 2.0) (log (+ 1.0 (/ (* x x) df)))))))

(defun pt (x df &key (lower t))
  "Student t cumulative distribution function."
  (let* ((tail (* 0.5 (beta-inc (/ df 2.0) 0.5 (/ df (+ df (* x x))))))   ; P(T > |x|)
         (p (if (> x 0) (- 1.0 tail) tail)))
    (if lower p (if (> x 0) tail (- 1.0 tail)))))

(defun qt (p df &key (lower t))
  "Student t quantile function."
  (let ((p (%tail p lower)))
    (cond ((<= p 0.0) -infinity+) ((>= p 1.0) +infinity+) ((= p 0.5) 0.0)
          ((< p 0.5) (- (qt (- 1.0 p) df)))
          (t (let ((hi (%bracket-up (lambda (x) (pt x df)) p 2.0)))
               (%solve-monotone (lambda (x) (pt x df)) p 0.0 hi))))))

;;; ---- chi-square, gamma, F, beta ----------------------------------------------------------------------------

(defun dgamma (x shape &key (scale 1.0))
  "Gamma density."
  (if (< x 0) 0.0 (if (zerop x) (if (< shape 1) +infinity+ (if (= shape 1) (/ 1.0 scale) 0.0))
                      (exp (- (* (- shape 1.0) (log (/ x scale))) (/ x scale) (log-gamma shape) (log scale))))))
(defun pgamma (x shape &key (scale 1.0) (lower t)) "Gamma cumulative distribution function."
  (let ((p (gamma-p shape (/ x scale)))) (if lower p (gamma-q shape (/ x scale)))))
(defun qgamma (p shape &key (scale 1.0) (lower t)) "Gamma quantile function."
  (let ((p (%tail p lower)))
    (cond ((<= p 0.0) 0.0) ((>= p 1.0) +infinity+)
          (t (* scale (%solve-monotone (lambda (x) (gamma-p shape x)) p 0.0 (%bracket-up (lambda (x) (gamma-p shape x)) p (max 1.0 shape))))))))

(defun dchisq (x df) "Chi-square density." (dgamma x (/ df 2.0) :scale 2.0))
(defun pchisq (x df &key (lower t)) "Chi-square cumulative distribution function." (pgamma x (/ df 2.0) :scale 2.0 :lower lower))
(defun qchisq (p df &key (lower t)) "Chi-square quantile function." (qgamma p (/ df 2.0) :scale 2.0 :lower lower))

(defun dbeta (x a b) "Beta density." (if (or (< x 0) (> x 1)) 0.0 (exp (- (+ (* (- a 1.0) (log x)) (* (- b 1.0) (log (- 1.0 x)))) (log-beta a b)))))
(defun pbeta (x a b &key (lower t)) "Beta cumulative distribution function." (let ((p (beta-inc a b x))) (if lower p (- 1.0 p))))
(defun qbeta (p a b &key (lower t)) "Beta quantile function." (%solve-monotone (lambda (x) (beta-inc a b x)) (%tail p lower) 0.0 1.0))

(defun df (x d1 d2)
  "F density with D1 and D2 degrees of freedom."
  (if (<= x 0) 0.0
      (exp (- (+ (* 0.5 d1 (log (/ d1 d2))) (* (- (* 0.5 d1) 1.0) (log x))) (* 0.5 (+ d1 d2) (log (+ 1.0 (/ (* d1 x) d2)))) (log-beta (* 0.5 d1) (* 0.5 d2))))))
(defun pf (x d1 d2 &key (lower t))
  "F cumulative distribution function."
  (if (<= x 0) (if lower 0.0 1.0)
      (let ((p (beta-inc (* 0.5 d1) (* 0.5 d2) (/ (* d1 x) (+ (* d1 x) d2)))))
        (if lower p (beta-inc (* 0.5 d2) (* 0.5 d1) (/ d2 (+ (* d1 x) d2)))))))
(defun qf (p d1 d2 &key (lower t))
  "F quantile function."
  (let ((p (%tail p lower)))
    (cond ((<= p 0.0) 0.0) ((>= p 1.0) +infinity+)
          (t (%solve-monotone (lambda (x) (pf x d1 d2)) p 0.0 (%bracket-up (lambda (x) (pf x d1 d2)) p 2.0))))))

;;; ---- simple continuous --------------------------------------------------------------------------------------------

(defun dexp (x &key (rate 1.0)) "Exponential density." (if (< x 0) 0.0 (* rate (exp (- (* rate x))))))
(defun pexp (x &key (rate 1.0) (lower t)) "Exponential cumulative distribution function." (let ((p (if (< x 0) 0.0 (- 1.0 (exp (- (* rate x))))))) (if lower p (- 1.0 p))))
(defun qexp (p &key (rate 1.0) (lower t)) "Exponential quantile function." (/ (- (log (- 1.0 (%tail p lower)))) rate))
(defun dunif (x &key (min 0.0) (max 1.0)) "Uniform density." (if (and (>= x min) (<= x max)) (/ 1.0 (- max min)) 0.0))
(defun punif (x &key (min 0.0) (max 1.0) (lower t)) "Uniform cumulative distribution function." (let ((p (max 0.0 (min 1.0 (/ (- x min) (- max min)))))) (if lower p (- 1.0 p))))
(defun qunif (p &key (min 0.0) (max 1.0) (lower t)) "Uniform quantile function." (+ min (* (- max min) (%tail p lower))))

;;; ---- discrete -----------------------------------------------------------------------------------------------------------

(defun dbinom (k n p)
  "Binomial probability mass function."
  (cond ((or (< k 0) (> k n)) 0.0) ((zerop p) (if (= k 0) 1.0 0.0)) ((= p 1) (if (= k n) 1.0 0.0))
        (t (exp (+ (log-choose n k) (* k (log p)) (* (- n k) (log (- 1.0 p))))))))
(defun pbinom (k n p &key (lower t))
  "Binomial cumulative distribution function P(X <= K)."
  (let ((k (floor k)))
    (let ((q (cond ((< k 0) 0.0) ((>= k n) 1.0) (t (beta-inc (float (- n k)) (+ k 1.0) (- 1.0 p))))))
      (if lower q (- 1.0 q)))))
(defun %discrete-quantile (cdf p lo hi)
  (let ((a lo) (b hi))
    (loop while (< a b) do (let ((mid (floor (+ a b) 2))) (if (>= (funcall cdf mid) (* p (- 1.0 1e-12))) (setq b mid) (setq a (+ mid 1)))))
    a))
(defun qbinom (p n prob &key (lower t)) "Binomial quantile function." (%discrete-quantile (lambda (k) (pbinom k n prob)) (%tail p lower) 0 n))

(defun dpois (k lambda*) "Poisson probability mass function." (if (< k 0) 0.0 (exp (- (* k (log lambda*)) lambda* (log-factorial k)))))
(defun ppois (k lambda* &key (lower t)) "Poisson cumulative distribution function P(X <= K)."
  (let* ((k (floor k)) (q (if (< k 0) 0.0 (gamma-q (+ k 1.0) lambda*)))) (if lower q (- 1.0 q))))
(defun qpois (p lambda* &key (lower t)) "Poisson quantile function."
  (let ((p (%tail p lower))) (%discrete-quantile (lambda (k) (ppois k lambda*)) p 0 (max 10 (ceiling (+ lambda* (* 20.0 (sqrt (+ lambda* 1.0)))))))))

(defun dhyper (k m n draws)
  "Hypergeometric probability: K successes in DRAWS draws from M successes and N failures."
  (if (or (< k 0) (> k draws) (> k m) (> (- draws k) n)) 0.0
      (exp (- (+ (log-choose m k) (log-choose n (- draws k))) (log-choose (+ m n) draws)))))
(defun phyper (k m n draws &key (lower t))
  "Hypergeometric cumulative distribution function P(X <= K)."
  (let ((s 0.0)) (loop for i from (max 0 (- draws n)) to (min (floor k) draws m) do (setq s (+ s (dhyper i m n draws))))
    (let ((s (min 1.0 s))) (if lower s (- 1.0 s)))))
