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

(defun arrayp (x) "True when X is an array: a vector, a string or a multi-dimensional array." (or (vectorp x) (%md-array-p x)))

(defun sublis (alist tree &key test)
  "Substitute in TREE every atom found as a key of ALIST."
  (let ((f (or test #'eql)))
    (labels ((walk (x) (let ((hit (assoc x alist :test f))) (cond (hit (cdr hit)) ((consp x) (cons (walk (car x)) (walk (cdr x)))) (t x)))))
      (walk tree))))

(defun mapcon (fn list &rest more)
  "Apply FN to successive tails and NCONC the results."
  (apply #'nconc (apply #'maplist fn list more)))

;;; ---- calendar time ----------------------------------------------------------------------------------------
;;; Universal time counts seconds from 1900-01-01 00:00 UTC.  TIME-ZONE follows Common Lisp: hours *west* of
;;; Greenwich, not counting daylight saving time (New York is 5).  Without a TIME-ZONE argument the local zone
;;; of the operating system is used, with its daylight saving rules (%LOCAL-OFFSET asks localtime_r).

(defun %civil-from-days (days)
  "Year, month (1-12) and day of the date DAYS days after 1900-01-01 (proleptic Gregorian calendar)."
  (let* ((z (+ (- days 25567) 719468))                 ; days since 0000-03-01
         (era (floor z 146097)) (doe (- z (* era 146097)))
         (yoe (floor (- doe (floor doe 1460) (- (floor doe 36524)) (floor doe 146096)) 365))
         (doy (- doe (- (+ (* 365 yoe) (floor yoe 4)) (floor yoe 100))))
         (mp (floor (+ (* 5 doy) 2) 153))
         (d (+ (- doy (floor (+ (* 153 mp) 2) 5)) 1))
         (m (if (< mp 10) (+ mp 3) (- mp 9)))
         (y (+ yoe (* era 400) (if (<= m 2) 1 0))))
    (values y m d)))

(defun %days-from-civil (y m d)
  "Days from 1900-01-01 to the date Y-M-D."
  (let* ((y (if (<= m 2) (- y 1) y))
         (era (floor y 400)) (yoe (- y (* era 400)))
         (doy (+ (floor (+ (* 153 (if (> m 2) (- m 3) (+ m 9))) 2) 5) (- d 1)))
         (doe (+ (* yoe 365) (floor yoe 4) (- (floor yoe 100)) doy)))
    (+ (* era 146097) doe -719468 25567)))

(defun decode-universal-time (universal-time &optional time-zone)
  "Decode UNIVERSAL-TIME into nine values: second, minute, hour, date, month, year, day of the week (0 is
Monday), daylight-saving-p and the time zone (hours west of Greenwich).  Without TIME-ZONE, the local time
zone and its daylight saving time are used."
  (multiple-value-bind (offset dst)
      (if time-zone (values (- (round (* time-zone 3600))) nil) (%local-offset universal-time))
    (let* ((local (+ universal-time offset)) (days (floor local 86400)) (secs (- local (* days 86400))))
      (multiple-value-bind (y m d) (%civil-from-days days)
        (values (mod secs 60) (mod (floor secs 60) 60) (floor secs 3600) d m y (mod days 7) (and dst t)
                (or time-zone (/ (- (if dst 3600 0) offset) 3600)))))))

(defun get-decoded-time ()
  "The current time as the nine values of DECODE-UNIVERSAL-TIME, in the local time zone."
  (decode-universal-time (get-universal-time)))

(defun encode-universal-time (second minute hour date month year &optional time-zone)
  "The universal time of a calendar time.  Without TIME-ZONE the local time zone (with daylight saving time)
is used.  A YEAR below 100 means the year within 50 years of now with those last two digits."
  (when (< year 100)
    (let* ((now (nth-value 5 (get-decoded-time))) (y (+ year (* 100 (floor now 100)))))
      (setq year (cond ((> (- y now) 50) (- y 100)) ((> (- now y) 50) (+ y 100)) (t y)))))
  (let ((u (+ (* 86400 (%days-from-civil year month date)) (* 3600 hour) (* 60 minute) second)))
    (if time-zone
        (+ u (round (* time-zone 3600)))
        (let* ((off1 (%local-offset u)) (guess (- u off1)) (off2 (%local-offset guess)))
          (if (= off1 off2) guess (- u off2))))))

(defun function-lambda-expression (fn)
  "Returns (values lambda-expression closure-p name).  The source is not kept, so the first value is always NIL
and the second always true; NAME is the function's name when it is known."
  (values nil t (and (functionp fn) (compiled-function-p fn) (car (%code-parts fn)))))
