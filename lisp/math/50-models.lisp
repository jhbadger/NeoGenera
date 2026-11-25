;;;; 50-models.lisp -- linear and generalized linear models.

(in-package :cl)

(defstruct (lm-fit (:copier nil) (:constructor %make-lm-fit))
  names coefficients std-errors t-values p-values residuals fitted r-squared adj-r-squared sigma
  df-residual df-model f-statistic f-p-value n design response intercept)

(defun %design-matrix (predictors n intercept)
  "A design matrix from PREDICTORS (a matrix, a list of columns, or one column)."
  (let* ((cols (cond ((matrix-p predictors) (loop for j from 0 below (matrix-cols predictors) collect (matrix-col predictors j)))
                     ((and (listp predictors) (or (null predictors) (not (realp (car predictors))))) predictors)
                     (t (list predictors))))
         (cols (mapcar #'%seq->vec cols)) (p (+ (length cols) (if intercept 1 0))) (x (make-matrix n p)) (j 0))
    (when intercept (dotimes (i n) (setf (mref x i 0) 1.0)) (setq j 1))
    (dolist (c cols) (dotimes (i n) (setf (mref x i j) (float (aref c i)))) (setq j (+ j 1)))
    x))

(defun %coef-names (names ncols intercept)
  (append (if intercept (list "(Intercept)") nil)
          (or names (loop for i from 1 to (- ncols (if intercept 1 0)) collect (format nil "x~D" i)))))

(defun %inverse-xtx (x)
  "(X^T X)^-1 computed from the R factor of a QR decomposition."
  (let* ((r1 (%qr-reduce x nil)) (ri (matrix-inverse r1)))
    (mat* ri (matrix-transpose ri))))

(defun lm (y predictors &key (intercept t) names)
  "Fit an ordinary least squares linear model of the response Y on PREDICTORS (a column, a list of columns, or a matrix)."
  (let* ((yv (%seq->vec y)) (n (length yv)) (x (%design-matrix predictors n intercept)) (p (matrix-cols x))
         (beta (least-squares x yv)) (fitted (mat-vec x beta)) (resid (v- yv fitted))
         (rss (dot resid resid)) (dfr (- n p)) (sigma2 (/ rss dfr)) (cov (mat-scale (%inverse-xtx x) sigma2))
         (se (let ((v (make-array p :initial-element 0.0))) (dotimes (i p) (setf (aref v i) (sqrt (mref cov i i)))) v))
         (tv (map 'vector #'%sdiv beta se)) (pv (map 'vector (lambda (tt) (if (float-infinity-p tt) 0.0 (* 2.0 (pt (- (abs tt)) dfr)))) tv))
         (ybar (mean yv))
         (tss (if intercept (sum (map 'list (lambda (v) (square (- v ybar))) yv)) (dot yv yv)))
         (dfm (if intercept (- p 1) p))
         (r2 (- 1.0 (%sdiv rss tss)))
         (adj (- 1.0 (* (- 1.0 r2) (/ (if intercept (- n 1.0) (float n)) dfr))))
         (fstat (if (> dfm 0) (%sdiv (/ (- tss rss) dfm) sigma2) nil)))
    (%make-lm-fit :names (%coef-names names p intercept) :coefficients beta :std-errors se :t-values tv :p-values pv
                  :residuals resid :fitted fitted :r-squared r2 :adj-r-squared adj :sigma (sqrt sigma2)
                  :df-residual dfr :df-model dfm :f-statistic fstat :f-p-value (and fstat (if (float-infinity-p fstat) 0.0 (pf fstat dfm dfr :lower nil)))
                  :n n :design x :response yv :intercept intercept)))

(defun polyfit (x y degree)
  "Least squares polynomial fit of DEGREE; returns an LM-FIT whose coefficients run from the constant term upward."
  (lm y (loop for k from 1 to degree collect (map 'vector (lambda (v) (expt (float v) k)) (%seq->vec x)))
      :names (loop for k from 1 to degree collect (format nil "x^~D" k))))

(defun lm-predict (fit rows)
  "Predictions of FIT for ROWS, a list of lists of predictor values (without the intercept)."
  (mapcar (lambda (row)
            (let ((s 0.0) (j 0) (b (lm-fit-coefficients fit)))
              (when (lm-fit-intercept fit) (setq s (aref b 0) j 1))
              (dolist (v (%seq->list row)) (setq s (+ s (* v (aref b j)))) (setq j (+ j 1)))
              s))
          rows))

(defun lm-confint (fit &optional (level 0.95))
  "Confidence intervals for the coefficients of FIT: a list of (lower upper)."
  (let ((q (qt (- 1.0 (/ (- 1.0 level) 2.0)) (lm-fit-df-residual fit))))
    (loop for b across (lm-fit-coefficients fit) for s across (lm-fit-std-errors fit) collect (list (- b (* q s)) (+ b (* q s))))))

(defun %sig-stars (p) (cond ((< p 0.001) "***") ((< p 0.01) "**") ((< p 0.05) "*") ((< p 0.1) ".") (t "")))

(defmethod print-object ((f lm-fit) stream)
  (format stream "~%Coefficients:~%~16A ~12A ~12A ~9A ~12A~%" "" "Estimate" "Std. Error" "t value" "Pr(>|t|)")
  (loop for nm in (lm-fit-names f) for b across (lm-fit-coefficients f) for s across (lm-fit-std-errors f)
        for tv across (lm-fit-t-values f) for p across (lm-fit-p-values f)
        do (format stream "~16A ~12A ~12A ~9A ~12A ~A~%" nm (%fmt-num b) (%fmt-num s) (%fmt-num tv) (%fmt-num p) (%sig-stars p)))
  (format stream "~%Residual standard error: ~A on ~D degrees of freedom~%" (%fmt-num (lm-fit-sigma f)) (lm-fit-df-residual f))
  (format stream "Multiple R-squared: ~A, Adjusted R-squared: ~A~%" (%fmt-num (lm-fit-r-squared f)) (%fmt-num (lm-fit-adj-r-squared f)))
  (when (lm-fit-f-statistic f)
    (format stream "F-statistic: ~A on ~D and ~D DF, p-value: ~A~%" (%fmt-num (lm-fit-f-statistic f)) (lm-fit-df-model f) (lm-fit-df-residual f) (%fmt-num (lm-fit-f-p-value f)))))

;;; ---- generalized linear models (IRLS) ---------------------------------------------------------------------------

(defstruct (glm-fit (:copier nil) (:constructor %make-glm-fit))
  family names coefficients std-errors z-values p-values fitted deviance null-deviance df-residual df-null aic iterations converged design response)

(defun %family-parts (family)
  "Returns (values linkinv dmu/deta variance deviance-unit loglik-unit) for the family."
  (ecase family
    (:gaussian (values (lambda (e) e) (lambda (e) (declare (ignore e)) 1.0) (lambda (mu) (declare (ignore mu)) 1.0)
                       (lambda (y mu) (square (- y mu))) nil))
    (:binomial (values (lambda (e) (/ 1.0 (+ 1.0 (exp (- e)))))
                       (lambda (e) (let ((m (/ 1.0 (+ 1.0 (exp (- e)))))) (max 1e-12 (* m (- 1.0 m)))))
                       (lambda (mu) (* mu (- 1.0 mu)))
                       (lambda (y mu) (* 2.0 (+ (if (> y 0) (* y (log (/ y mu))) 0.0) (if (< y 1) (* (- 1.0 y) (log (/ (- 1.0 y) (- 1.0 mu)))) 0.0))))
                       (lambda (y mu) (+ (* y (log mu)) (* (- 1.0 y) (log (- 1.0 mu)))))))
    (:poisson (values (lambda (e) (exp e)) (lambda (e) (exp e)) (lambda (mu) mu)
                      (lambda (y mu) (* 2.0 (- (if (> y 0) (* y (log (/ y mu))) 0.0) (- y mu))))
                      (lambda (y mu) (- (* y (log mu)) mu (log-factorial (round y))))))))

(defun glm (y predictors &key (family :binomial) (intercept t) names (max-iterations 50))
  "Fit a generalized linear model by iteratively reweighted least squares.  FAMILY is :gaussian, :binomial (logit link,
Y in [0,1]) or :poisson (log link)."
  (let* ((yv (map 'vector #'float (%seq->vec y))) (n (length yv)) (x (%design-matrix predictors n intercept)) (p (matrix-cols x)))
    (multiple-value-bind (linkinv dmu var devunit llunit) (%family-parts family)
      (let* ((mu (map 'vector (lambda (v) (case family (:binomial (/ (+ v 0.5) 2.0)) (:poisson (+ v 0.1)) (t v))) yv))
             (eta (map 'vector (lambda (m) (case family (:binomial (log (/ m (- 1.0 m)))) (:poisson (log m)) (t m))) mu))
             (beta (make-array p :initial-element 0.0)) (dev-old 1e300) (dev 0.0) (iters 0) (converged nil))
        (dotimes (it max-iterations)
          (setq iters (+ it 1))
          (let* ((w (make-array n :initial-element 0.0)) (z (make-array n :initial-element 0.0)))
            (dotimes (i n)
              (let* ((d (funcall dmu (aref eta i))) (vv (funcall var (aref mu i))))
                (setf (aref w i) (sqrt (/ (* d d) vv)))
                (setf (aref z i) (* (aref w i) (+ (aref eta i) (/ (- (aref yv i) (aref mu i)) d))))))
            (let ((xw (make-matrix n p)))
              (dotimes (i n) (dotimes (j p) (setf (mref xw i j) (* (aref w i) (mref x i j)))))
              (setq beta (least-squares xw z))))
          (setq eta (mat-vec x beta))
          (setq mu (map 'vector linkinv eta))
          (when (eq family :binomial) (setq mu (map 'vector (lambda (m) (max 1e-14 (min (- 1.0 1e-14) m))) mu)))
          (setq dev (sum (loop for i from 0 below n collect (funcall devunit (aref yv i) (aref mu i)))))
          (when (< (abs (- dev dev-old)) (* 1e-10 (+ (abs dev) 0.1))) (setq converged t) (return))
          (setq dev-old dev))
        (let* ((ybar (mean yv)) (null-dev (sum (loop for i from 0 below n collect (funcall devunit (aref yv i) (case family (:gaussian ybar) (:poisson ybar) (t (max 1e-14 (min (- 1.0 1e-14) ybar)))))))))
          (let* ((w2 (make-matrix n p)) (disp (if (eq family :gaussian) (/ dev (- n p)) 1.0)))
            (dotimes (i n)
              (let* ((d (funcall dmu (aref eta i))) (wi (/ (* d d) (funcall var (aref mu i)))))
                (dotimes (j p) (setf (mref w2 i j) (* (sqrt wi) (mref x i j))))))
            (let* ((cov (mat-scale (%inverse-xtx w2) disp))
                   (se (let ((v (make-array p :initial-element 0.0))) (dotimes (i p) (setf (aref v i) (sqrt (mref cov i i)))) v))
                   (zv (map 'vector #'%sdiv beta se))
                   (pv (map 'vector (lambda (zz) (if (eq family :gaussian) (* 2.0 (pt (- (abs zz)) (- n p))) (* 2.0 (pnorm (- (abs zz)))))) zv))
                   (aic (if llunit (+ (* -2.0 (sum (loop for i from 0 below n collect (funcall llunit (aref yv i) (aref mu i))))) (* 2.0 p))
                            (+ (* n (+ (log (* 2.0 pi (/ dev n))) 1.0)) (* 2.0 (+ p 1))))))
              (%make-glm-fit :family family :names (%coef-names names p intercept) :coefficients beta :std-errors se :z-values zv :p-values pv
                             :fitted mu :deviance dev :null-deviance null-dev :df-residual (- n p) :df-null (- n (if intercept 1 0))
                             :aic aic :iterations iters :converged converged :design x :response yv))))))))

(defun glm-predict (fit rows &key (type :response))
  "Predictions of the GLM FIT for ROWS (lists of predictor values): :response (mean scale) or :link."
  (mapcar (lambda (row)
            (let* ((b (glm-fit-coefficients fit)) (s (aref b 0)) (j 1))
              (dolist (v (%seq->list row)) (setq s (+ s (* v (aref b j)))) (setq j (+ j 1)))
              (if (eq type :link) s
                  (ecase (glm-fit-family fit) (:gaussian s) (:binomial (/ 1.0 (+ 1.0 (exp (- s))))) (:poisson (exp s))))))
          rows))

(defmethod print-object ((f glm-fit) stream)
  (format stream "~%Family: ~A~%Coefficients:~%~16A ~12A ~12A ~9A ~12A~%" (string-downcase (symbol-name (glm-fit-family f))) "" "Estimate" "Std. Error" "z value" "Pr(>|z|)")
  (loop for nm in (glm-fit-names f) for b across (glm-fit-coefficients f) for s across (glm-fit-std-errors f)
        for z across (glm-fit-z-values f) for p across (glm-fit-p-values f)
        do (format stream "~16A ~12A ~12A ~9A ~12A ~A~%" nm (%fmt-num b) (%fmt-num s) (%fmt-num z) (%fmt-num p) (%sig-stars p)))
  (format stream "~%Null deviance: ~A on ~D degrees of freedom~%Residual deviance: ~A on ~D degrees of freedom~%AIC: ~A~%Fisher scoring iterations: ~D~%"
          (%fmt-num (glm-fit-null-deviance f)) (glm-fit-df-null f) (%fmt-num (glm-fit-deviance f)) (glm-fit-df-residual f)
          (%fmt-num (glm-fit-aic f)) (glm-fit-iterations f)))
