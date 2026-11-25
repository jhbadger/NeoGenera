;;;; 40-tests.lisp -- hypothesis tests, multiple-testing corrections, resampling.

(in-package :cl)

(defun %tie-term (values)
  "Sum of (c^3 - c) over the groups of tied VALUES."
  (let ((h (make-hash-table :test 'equal)) (s 0.0))
    (dolist (v values) (setf (gethash v h) (+ 1 (gethash v h 0))))
    (maphash (lambda (k c) (declare (ignore k)) (setq s (+ s (- (* c c c) c)))) h)
    s))

(defstruct (htest (:copier nil) (:constructor %make-htest))
  name statistic df p-value estimate conf-int alternative method details)

(defun %fmt-num (x)
  (cond ((null x) "NA") ((integerp x) (format nil "~D" x)) ((not (floatp x)) (format nil "~A" x))
        ((and (/= x 0.0) (< (abs x) 1e-4)) (format nil "~,3E" x)) (t (format nil "~,5F" x))))

(defmethod print-object ((h htest) stream)
  (format stream "~%~A~%" (htest-method h))
  (when (htest-name h) (format stream "data: ~A~%" (htest-name h)))
  (when (htest-statistic h) (format stream "statistic = ~A" (%fmt-num (htest-statistic h))))
  (when (htest-df h)
    (format stream ", df = ~A" (if (listp (htest-df h)) (%join-strings (mapcar #'%fmt-num (htest-df h)) ", ") (%fmt-num (htest-df h)))))
  (format stream ", p-value = ~A~%" (%fmt-num (htest-p-value h)))
  (when (htest-alternative h) (format stream "alternative hypothesis: ~A~%" (string-downcase (symbol-name (htest-alternative h)))))
  (when (htest-conf-int h)
    (format stream "~D percent confidence interval: ~A ~A~%" (or (getf (htest-details h) :conf-percent) 95)
            (%fmt-num (first (htest-conf-int h))) (%fmt-num (second (htest-conf-int h)))))
  (when (htest-estimate h)
    (format stream "estimate: ~A~%" (if (listp (htest-estimate h)) (%join-strings (mapcar #'%fmt-num (htest-estimate h)) " ") (%fmt-num (htest-estimate h))))))

(defun %p-from-stat (alternative lower-p upper-p two-sided-p)
  (case alternative (:less lower-p) (:greater upper-p) (t two-sided-p)))

;;; ---- t tests ------------------------------------------------------------------------------------------------------

(defun t-test (x &key y (mu 0.0) paired var-equal (alternative :two-sided) (conf-level 0.95))
  "Student's t test.  One sample (compare the mean of X with MU), two samples (Welch unless VAR-EQUAL), or PAIRED.
ALTERNATIVE is :two-sided, :less or :greater."
  (let (tstat df est ci-center se name method)
    (cond
      ((and y paired)
       (let* ((d (mapcar #'- (%seq->list x) (%seq->list y))) (n (length d)))
         (setq est (mean d) se (/ (std-dev d) (sqrt (float n))) df (- n 1) method "Paired t-test" name "mean difference")
         (setq tstat (/ (- est mu) se) ci-center est)))
      (y
       (let* ((nx (length (%seq->list x))) (ny (length (%seq->list y))) (mx (mean x)) (my (mean y)) (vx (variance x)) (vy (variance y)))
         (setq est (list mx my) name "means")
         (if var-equal
             (let* ((sp (/ (+ (* (- nx 1) vx) (* (- ny 1) vy)) (+ nx ny -2.0))))
               (setq df (+ nx ny -2) se (sqrt (* sp (+ (/ 1.0 nx) (/ 1.0 ny)))) method "Two Sample t-test"))
             (let* ((sx (/ vx nx)) (sy (/ vy ny)))
               (setq se (sqrt (+ sx sy)) df (/ (square (+ sx sy)) (+ (/ (square sx) (- nx 1)) (/ (square sy) (- ny 1)))) method "Welch Two Sample t-test")))
         (setq ci-center (- mx my) tstat (/ (- ci-center mu) se))))
      (t (let ((n (length (%seq->list x))))
           (setq est (mean x) se (/ (std-dev x) (sqrt (float n))) df (- n 1) method "One Sample t-test" name "mean")
           (setq tstat (/ (- est mu) se) ci-center est))))
    (let* ((lower (pt tstat df)) (upper (pt tstat df :lower nil))
           (p (%p-from-stat alternative lower upper (min 1.0 (* 2.0 (min lower upper)))))
           (ci (case alternative
                 (:less (list -infinity+ (+ ci-center (* se (qt conf-level df)))))
                 (:greater (list (- ci-center (* se (qt conf-level df))) +infinity+))
                 (t (let ((q (qt (- 1.0 (/ (- 1.0 conf-level) 2.0)) df))) (list (- ci-center (* q se)) (+ ci-center (* q se))))))))
      (%make-htest :name name :statistic tstat :df df :p-value p :estimate est :conf-int ci :alternative alternative :method method
                   :details (list :conf-percent (round (* 100 conf-level)) :se se)))))

(defun var-test (x y &key (alternative :two-sided))
  "F test comparing the variances of X and Y."
  (let* ((f (/ (variance x) (variance y))) (d1 (- (length (%seq->list x)) 1)) (d2 (- (length (%seq->list y)) 1))
         (lower (pf f d1 d2)) (upper (pf f d1 d2 :lower nil)))
    (%make-htest :name "ratio of variances" :statistic f :df (list d1 d2) :estimate f :alternative alternative :method "F test to compare two variances"
                 :p-value (%p-from-stat alternative lower upper (min 1.0 (* 2.0 (min lower upper)))))))

;;; ---- chi-square -------------------------------------------------------------------------------------------------------

(defun chisq-gof (observed &key probs)
  "Chi-square goodness-of-fit test of OBSERVED counts against PROBS (default uniform)."
  (let* ((o (%seq->list observed)) (k (length o)) (n (sum o))
         (p (or (and probs (%seq->list probs)) (make-list k :initial-element (/ 1.0 k))))
         (stat (sum (mapcar (lambda (oi pi*) (/ (square (- oi (* n pi*))) (* n pi*))) o p))))
    (%make-htest :name "observed vs expected" :statistic stat :df (- k 1) :p-value (pchisq stat (- k 1) :lower nil) :method "Chi-squared test for given probabilities"
                 :details (list :expected (mapcar (lambda (pi*) (* n pi*)) p)))))

(defun chisq-independence (table &key (correct nil))
  "Chi-square test of independence for a contingency TABLE (a matrix or list of rows).  CORRECT applies Yates' continuity
correction to 2x2 tables."
  (let* ((m (if (matrix-p table) table (mat table))) (r (matrix-rows m)) (c (matrix-cols m)) (n (sum (matrix-data m)))
         (rs (loop for i from 0 below r collect (sum (matrix-row m i)))) (cs (loop for j from 0 below c collect (sum (matrix-col m j))))
         (stat 0.0) (expected (make-matrix r c)) (yates (and correct (= r 2) (= c 2))))
    (dotimes (i r)
      (dotimes (j c)
        (let* ((e (/ (* (nth i rs) (nth j cs)) n)) (d (abs (- (mref m i j) e))))
          (setf (mref expected i j) e)
          (setq stat (+ stat (/ (square (if yates (max 0.0 (- d 0.5)) d)) e))))))
    (let ((df (* (- r 1) (- c 1))))
      (%make-htest :name "table" :statistic stat :df df :p-value (pchisq stat df :lower nil)
                   :method (if yates "Pearson's Chi-squared test with Yates' continuity correction" "Pearson's Chi-squared test")
                   :details (list :expected expected)))))

;;; ---- ANOVA and Kruskal-Wallis --------------------------------------------------------------------------------------

(defun anova-oneway (groups)
  "One-way analysis of variance for GROUPS (a list of data sequences).  Details hold the sums of squares."
  (let* ((gs (mapcar #'%seq->list groups)) (k (length gs)) (ns (mapcar #'length gs)) (n (reduce #'+ ns))
         (grand (/ (sum (mapcar #'sum gs)) n))
         (ssb (sum (mapcar (lambda (g ni) (* ni (square (- (mean g) grand)))) gs ns)))
         (ssw (sum (mapcar (lambda (g) (let ((m (mean g))) (sum (mapcar (lambda (x) (square (- x m))) g)))) gs)))
         (dfb (- k 1)) (dfw (- n k)) (msb (/ ssb dfb)) (msw (/ ssw dfw)) (f (/ msb msw)))
    (%make-htest :name "groups" :statistic f :df (list dfb dfw) :p-value (pf f dfb dfw :lower nil) :method "One-way analysis of variance"
                 :details (list :ss-between ssb :ss-within ssw :ms-between msb :ms-within msw :group-means (mapcar #'mean gs)))))

(defun kruskal-wallis (groups)
  "Kruskal-Wallis rank sum test for GROUPS (tie-corrected chi-square approximation)."
  (let* ((gs (mapcar #'%seq->list groups)) (all (apply #'append gs)) (n (length all)) (rk (ranks all)) (pos 0) (h 0.0))
    (dolist (g gs)
      (let ((ni (length g)) (rsum 0.0))
        (dotimes (i ni) (setq rsum (+ rsum (nth (+ pos i) rk))))
        (setq pos (+ pos ni) h (+ h (/ (square rsum) ni)))))
    (setq h (- (* (/ 12.0 (* n (+ n 1))) h) (* 3.0 (+ n 1))))
    (setq h (/ h (- 1.0 (/ (%tie-term all) (- (* n n n) n)))))
    (let ((df (- (length gs) 1))) (%make-htest :name "groups" :statistic h :df df :p-value (pchisq h df :lower nil) :method "Kruskal-Wallis rank sum test"))))

;;; ---- rank tests -----------------------------------------------------------------------------------------------------------

(defun %rank-sum-distribution (n1 n2)
  "Exact distribution of the Mann-Whitney U statistic: a vector of counts indexed by U."
  (let* ((maxu (* n1 n2)) (table (make-hash-table :test 'equal)))
    ;; f(m, n, u) = f(m-1, n, u-n) + f(m, n-1, u)
    (labels ((f (m n u)
               (cond ((or (< u 0) (> u (* m n))) 0)
                     ((or (= m 0) (= n 0)) (if (= u 0) 1 0))
                     (t (let* ((key (list m n u)) (v (gethash key table)))
                          (or v (setf (gethash key table) (+ (f (- m 1) n (- u n)) (f m (- n 1) u)))))))))
      (let ((counts (make-array (+ maxu 1) :initial-element 0)))
        (dotimes (u (+ maxu 1)) (setf (aref counts u) (f n1 n2 u)))
        counts))))

(defun mann-whitney (x y &key (alternative :two-sided) (exact nil exact-p) (correct t))
  "Wilcoxon rank sum (Mann-Whitney U) test of X against Y.  Exact p-values are used for samples under 50 without ties."
  (let* ((a (%seq->list x)) (b (%seq->list y)) (n1 (length a)) (n2 (length b)) (rk (ranks (append a b)))
         (w (- (sum (subseq rk 0 n1)) (/ (* n1 (+ n1 1.0)) 2.0)))
         (ties (< (length (remove-duplicates (append a b))) (+ n1 n2)))
         (use-exact (if exact-p exact (and (< n1 50) (< n2 50) (not ties)))) p)
    (if (and use-exact (not ties))
        (let* ((counts (%rank-sum-distribution n1 n2)) (total (choose (+ n1 n2) n1)) (wi (round w)))
          (flet ((cdf (u) (/ (float (loop for i from 0 to (min u (* n1 n2)) sum (aref counts i))) total)))
            (let ((lower (cdf wi)) (upper (- 1.0 (cdf (- wi 1)))))
              (setq p (%p-from-stat alternative lower upper (min 1.0 (* 2.0 (min lower upper))))))))
        (let* ((tie-term (%tie-term (append a b)))
               (mu (/ (* n1 n2) 2.0)) (nn (+ n1 n2))
               (sigma (sqrt (* (/ (* n1 n2) 12.0) (- (+ nn 1.0) (/ tie-term (* nn (- nn 1.0)))))))
               (z (- w mu)) (corr (if correct (case alternative (:greater 0.5) (:less -0.5) (t (* 0.5 (sign z)))) 0.0)))
          (setq z (/ (- z corr) sigma))
          (setq p (%p-from-stat alternative (pnorm z) (pnorm z :lower nil) (min 1.0 (* 2.0 (pnorm (- (abs z)))))))))
    (%make-htest :name "difference in location" :statistic w :p-value p :alternative alternative :method (if (and use-exact (not ties)) "Wilcoxon rank sum exact test" "Wilcoxon rank sum test with continuity correction"))))

(defun wilcoxon-signed-rank (x &key y (mu 0.0) (alternative :two-sided))
  "Wilcoxon signed rank test of X (or the paired differences X - Y) against MU (exact for n < 50 without ties or zeros)."
  (let* ((d (remove-if #'zerop (mapcar (lambda (v) (- v mu)) (if y (mapcar #'- (%seq->list x) (%seq->list y)) (%seq->list x)))))
         (n (length d)) (rk (ranks (mapcar #'abs d)))
         (v (sum (loop for di in d for r in rk when (> di 0) collect r)))
         (ties (< (length (remove-duplicates (mapcar #'abs d))) n)) p)
    (if (and (< n 50) (not ties))
        (let ((counts (make-array (+ 1 (/ (* n (+ n 1)) 2)) :initial-element 0)))
          (setf (aref counts 0) 1)
          (loop for r from 1 to n do (loop for s from (- (length counts) 1) downto r do (incf (aref counts s) (aref counts (- s r)))))
          (flet ((cdf (u) (/ (float (loop for i from 0 to (min u (- (length counts) 1)) sum (aref counts i))) (expt 2.0 n))))
            (let* ((vi (round v)) (lower (cdf vi)) (upper (- 1.0 (cdf (- vi 1)))))
              (setq p (%p-from-stat alternative lower upper (min 1.0 (* 2.0 (min lower upper))))))))
        (let* ((mu* (/ (* n (+ n 1.0)) 4.0))
               (tie-term (%tie-term (mapcar #'abs d)))
               (sigma (sqrt (- (/ (* n (+ n 1.0) (+ (* 2.0 n) 1.0)) 24.0) (/ tie-term 48.0))))
               (z (/ (- v mu* (* 0.5 (sign (- v mu*)))) sigma)))
          (setq p (%p-from-stat alternative (pnorm z) (pnorm z :lower nil) (min 1.0 (* 2.0 (pnorm (- (abs z)))))))))
    (%make-htest :name "location" :statistic v :p-value p :alternative alternative :method "Wilcoxon signed rank test")))

;;; ---- exact tests -----------------------------------------------------------------------------------------------------

(defun fisher-exact (a b c d &key (alternative :two-sided))
  "Fisher's exact test for the 2x2 table [[A B] [C D]].  The estimate is the sample odds ratio."
  (let* ((m (+ a c)) (n (+ b d)) (k (+ a b)) (lo (max 0 (- k n))) (hi (min k m))
         (pobs (dhyper a m n k)) (lower (phyper a m n k)) (upper (phyper (- a 1) m n k :lower nil)) (two 0.0))
    (loop for i from lo to hi do (let ((pi* (dhyper i m n k))) (when (<= pi* (* pobs (+ 1.0 1e-7))) (setq two (+ two pi*)))))
    (%make-htest :name "odds ratio" :p-value (min 1.0 (%p-from-stat alternative lower upper two)) :alternative alternative
                 :estimate (if (or (zerop b) (zerop c)) +infinity+ (/ (* a d) (float (* b c)))) :method "Fisher's Exact Test for Count Data")))

(defun binom-test (x n &key (p 0.5) (alternative :two-sided))
  "Exact binomial test of X successes in N trials against probability P."
  (let* ((pobs (dbinom x n p)) (lower (pbinom x n p)) (upper (pbinom (- x 1) n p :lower nil)) (two 0.0))
    (dotimes (i (+ n 1)) (let ((pi* (dbinom i n p))) (when (<= pi* (* pobs (+ 1.0 1e-7))) (setq two (+ two pi*)))))
    (%make-htest :name "probability of success" :statistic x :p-value (min 1.0 (%p-from-stat alternative lower upper two))
                 :estimate (/ (float x) n) :alternative alternative :method "Exact binomial test")))

(defun cor-test (x y &key (method :pearson) (alternative :two-sided))
  "Test of association between X and Y: :pearson (t distribution), or :spearman (t approximation to the rank correlation)."
  (let* ((r (if (eq method :spearman) (spearman x y) (correlation x y))) (n (length (%seq->list x))) (df (- n 2))
         (tstat (/ (* r (sqrt (float df))) (sqrt (- 1.0 (* r r))))) (lower (pt tstat df)) (upper (pt tstat df :lower nil)))
    (%make-htest :name "correlation" :statistic tstat :df df :estimate r :alternative alternative
                 :p-value (%p-from-stat alternative lower upper (min 1.0 (* 2.0 (min lower upper))))
                 :method (if (eq method :spearman) "Spearman's rank correlation" "Pearson's product-moment correlation")
                 :conf-int (when (and (eq method :pearson) (> n 3) (< (abs r) 1.0))
                             (let* ((z (atanh r)) (se (/ 1.0 (sqrt (- n 3.0)))) (q (qnorm 0.975)))
                               (list (tanh (- z (* q se))) (tanh (+ z (* q se)))))))))

(defun ks-test (x y &key (alternative :two-sided))
  "Two-sample Kolmogorov-Smirnov test (asymptotic p-value)."
  (declare (ignore alternative))
  (let* ((a (%sorted-vector x)) (b (%sorted-vector y)) (n1 (length a)) (n2 (length b)) (i 0) (j 0) (d 0.0))
    (loop while (and (< i n1) (< j n2))
          do (let ((v (min (aref a i) (aref b j))))
               (loop while (and (< i n1) (<= (aref a i) v)) do (incf i))
               (loop while (and (< j n2) (<= (aref b j) v)) do (incf j))
               (setq d (max d (abs (- (/ i (float n1)) (/ j (float n2))))))))
    (let* ((en (sqrt (/ (* n1 n2) (float (+ n1 n2))))) (lam (* (+ en 0.12 (/ 0.11 en)) d)) (p 0.0))
      (loop for k from 1 to 100 do (setq p (+ p (* 2.0 (if (oddp k) 1.0 -1.0) (exp (* -2.0 k k lam lam))))))
      (%make-htest :name "D" :statistic d :p-value (max 0.0 (min 1.0 p)) :method "Two-sample Kolmogorov-Smirnov test"))))

;;; ---- multiple testing -------------------------------------------------------------------------------------------------

(defun p-adjust (p-values &optional (method :holm))
  "Adjust a list of P-VALUES for multiple testing.  METHOD is :bonferroni, :holm, :hochberg, :bh (Benjamini-Hochberg) or :by."
  (let* ((ps (coerce (%seq->list p-values) 'vector)) (n (length ps))
         (order (sort (loop for i from 0 below n collect i) (lambda (i j) (< (aref ps i) (aref ps j)))))
         (out (make-array n :initial-element 0.0)) (ov (coerce order 'vector)))
    (ecase method
      (:bonferroni (dotimes (i n) (setf (aref out i) (min 1.0 (* n (aref ps i))))))
      (:holm (let ((run 0.0))
               (dotimes (k n) (let ((i (aref ov k))) (setq run (max run (min 1.0 (* (- n k) (aref ps i))))) (setf (aref out i) run)))))
      (:hochberg (let ((run 1.0))
                   (loop for k from (- n 1) downto 0 do (let ((i (aref ov k))) (setq run (min run (min 1.0 (* (- n k) (aref ps i))))) (setf (aref out i) run)))))
      ((:bh :fdr) (let ((run 1.0))
                    (loop for k from (- n 1) downto 0 do (let ((i (aref ov k))) (setq run (min run (min 1.0 (/ (* n (aref ps i)) (+ k 1))))) (setf (aref out i) run)))))
      (:by (let ((run 1.0) (q (loop for i from 1 to n sum (/ 1.0 i))))
             (loop for k from (- n 1) downto 0 do (let ((i (aref ov k))) (setq run (min run (min 1.0 (/ (* q n (aref ps i)) (+ k 1))))) (setf (aref out i) run))))))
    (coerce out 'list)))

;;; ---- resampling -----------------------------------------------------------------------------------------------------------

(defun bootstrap (data statistic &key (replicates 1000) (rng *rng*))
  "Bootstrap distribution of STATISTIC (a function of a vector): a sorted vector of REPLICATES values."
  (let* ((v (%seq->vec data)) (out (make-array replicates :initial-element 0.0)))
    (dotimes (i replicates) (setf (aref out i) (funcall statistic (sample-with-replacement v (length v) rng))))
    (sort out #'<)))

(defun bootstrap-ci (data statistic &key (replicates 1000) (conf-level 0.95) (rng *rng*))
  "Percentile bootstrap confidence interval for STATISTIC: a list (lower upper)."
  (let ((dist (bootstrap data statistic :replicates replicates :rng rng)) (a (/ (- 1.0 conf-level) 2.0)))
    (list (quantile dist a) (quantile dist (- 1.0 a)))))

(defun permutation-test (x y &key (statistic (lambda (a b) (- (mean a) (mean b)))) (replicates 2000) (rng *rng*))
  "Two-sample permutation test of STATISTIC (default: difference of means); the p-value is two-sided and includes the observed split."
  (let* ((a (%seq->list x)) (b (%seq->list y)) (n1 (length a)) (all (coerce (append a b) 'vector))
         (obs (funcall statistic a b)) (extreme 1))
    (dotimes (i replicates)
      (let* ((s (shuffle all rng)) (s1 (coerce (subseq s 0 n1) 'list)) (s2 (coerce (subseq s n1) 'list)))
        (when (>= (abs (funcall statistic s1 s2)) (- (abs obs) 1e-12)) (incf extreme))))
    (%make-htest :name "permutation" :statistic obs :p-value (/ extreme (+ replicates 1.0)) :method "Permutation test")))
