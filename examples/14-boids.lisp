;;; Boids: Craig Reynolds' flocking model.  Each bird follows three local rules and a flock emerges.
;;; expect: flock alignment
;;; Positions and velocities are complex numbers (x + yi) on a wrap-around (toroidal) world.

(defparameter *w* 72.0)                 ; world width and height, in world units
(defparameter *h* 36.0)
(defparameter *count* 45)
(defparameter *vision* 12.0)            ; birds closer than this are neighbours
(defparameter *crowd* 2.5)              ; ... and closer than this is too close
(defparameter *min-speed* 0.6)
(defparameter *max-speed* 1.1)

;;; A tiny deterministic generator, so the demo prints the same flock every run.
(defvar *seed* 20261127)
(defun rnd ()
  "A pseudo-random float in [0, 1)."
  (setq *seed* (mod (+ (* *seed* 1103515245) 12345) 2147483648))
  (/ (float *seed*) 2147483648.0))

(defun wrap (x size)
  "X reduced into [0, SIZE) -- birds leaving one edge re-enter at the opposite one."
  (mod x size))

(defun wrap-delta (d)
  "The shortest displacement on the torus equivalent to complex displacement D."
  (flet ((fold (v size) (cond ((> v (/ size 2)) (- v size)) ((< v (- (/ size 2))) (+ v size)) (t v))))
    (complex (fold (realpart d) *w*) (fold (imagpart d) *h*))))

(defun limit-speed (v)
  (let ((s (abs v)))
    (cond ((< s 1e-9) (complex *min-speed* 0.0))
          ((> s *max-speed*) (* v (/ *max-speed* s)))
          ((< s *min-speed*) (* v (/ *min-speed* s)))
          (t v))))

(defstruct flock pos vel)

(defun make-random-flock (n)
  (let ((pos (make-array n)) (vel (make-array n)))
    (dotimes (i n)
      (setf (aref pos i) (complex (* *w* (rnd)) (* *h* (rnd)))
            (aref vel i) (limit-speed (* (+ *min-speed* (* 0.4 (rnd)))
                                         (exp (complex 0.0 (* 2 pi (rnd))))))))   ; random heading: e^(i theta)
    (make-flock :pos pos :vel vel)))

(defun step-flock (f)
  "Advance every bird one time step using separation, alignment and cohesion."
  (let* ((pos (flock-pos f)) (vel (flock-vel f)) (n (length pos))
         (new-pos (make-array n)) (new-vel (make-array n)))
    (dotimes (i n)
      (let ((p (aref pos i)) (v (aref vel i))
            (centre 0) (heading 0) (push 0) (seen 0))
        (dotimes (j n)
          (unless (= i j)
            (let* ((d (wrap-delta (- (aref pos j) p))) (dist (abs d)))
              (when (< dist *vision*)
                (incf seen)
                (setq centre (+ centre d))                    ; offsets to neighbours: their centroid is centre/seen
                (setq heading (+ heading (aref vel j)))
                (when (< dist *crowd*)                        ; separation: step away, harder when closer
                  (setq push (- push (/ d (+ 0.1 (* dist dist))))))))))
        (when (> seen 0)
          (setq v (+ v
                     (* 0.012 (/ centre seen))                ; cohesion
                     (* 0.12 (- (/ heading seen) v))          ; alignment
                     (* 0.9 push))))                          ; separation
        (setq v (limit-speed v))
        (setf (aref new-vel i) v
              (aref new-pos i) (let ((q (+ p v))) (complex (wrap (realpart q) *w*) (wrap (imagpart q) *h*))))))
    (setf (flock-pos f) new-pos (flock-vel f) new-vel)
    f))

(defun alignment (f)
  "Order parameter: 0 for headings pointing every which way, 1 when every bird flies the same direction."
  (let ((sum 0) (vel (flock-vel f)))
    (dotimes (i (length vel)) (setq sum (+ sum (/ (aref vel i) (abs (aref vel i))))))
    (/ (abs sum) (length vel))))

(defun heading-char (v)
  "An arrow-ish character for the direction of V (y grows upward)."
  (let ((k (mod (round (/ (atan (imagpart v) (realpart v)) (/ pi 4))) 8)))
    (char ">/^\\</v\\" k)))

(defun draw-flock (f)
  (let* ((cols (floor *w*)) (rows (floor (/ *h* 2)))
         (grid (make-array rows)))
    (dotimes (r rows) (setf (aref grid r) (make-string cols :initial-element #\.)))
    (dotimes (i (length (flock-pos f)))
      (let* ((p (aref (flock-pos f) i))
             (col (min (- cols 1) (floor (realpart p))))
             (row (min (- rows 1) (floor (/ (- *h* (imagpart p)) 2)))))
        (setf (char (aref grid row) (max 0 col)) (heading-char (aref (flock-vel f) i)))))
    (dotimes (r rows) (write-line (aref grid r)))))

(defun report (f tick)
  (format t "~%--- tick ~D: flock alignment ~,2F ---~%" tick (alignment f))
  (draw-flock f))

;;; ---- run -------------------------------------------------------------------------------------------------------
(let ((f (make-random-flock *count*)) (history nil))
  (format t "~D boids on a ~Dx~D torus; rules: separation, alignment, cohesion.~%" *count* (floor *w*) (floor *h*))
  (report f 0)
  (dotimes (tick 240)
    (step-flock f)
    (push (alignment f) history)
    (when (member (+ tick 1) '(30 240)) (report f (+ tick 1))))
  (setq history (reverse history))
  (format t "~%flock alignment over time (every 20 ticks):~%")
  (loop for a in history for i from 1
        when (zerop (mod i 20)) do (format t "  tick ~3D  ~,2F ~A~%" i a (make-string (round (* 40 a)) :initial-element #\#))))
