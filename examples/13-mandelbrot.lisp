;;; The Mandelbrot set, iterating z -> z^2 + c with built-in complex numbers; ASCII art plus a colour image (PPM).
;;; expect: Mandelbrot set
;;; The image is written to build/examples/mandelbrot.ppm (a plain-text PPM; macOS Preview, GIMP and ImageMagick open it).

(defparameter *max-iter* 80)

(defun escape-time (c)
  "Iterations of z -> z^2 + C (from z = 0) before |z| exceeds 2, or *MAX-ITER* if it never does.
Second value is a smooth (fractional) count for colouring."
  (let ((z 0))
    (dotimes (i *max-iter* (values *max-iter* (float *max-iter*)))
      (setq z (+ (* z z) c))
      (let ((m2 (+ (* (realpart z) (realpart z)) (* (imagpart z) (imagpart z)))))
        (when (> m2 4.0)
          (return (values i (- (+ i 1) (/ (log (/ (log m2) 2.0)) (log 2.0))))))))))

(defun pixel-point (col row width height centre scale &optional (cell-height 1.0))
  "The complex number at pixel (COL, ROW); SCALE is the width of the view on the real axis and CELL-HEIGHT
the height of a cell relative to its width (2.0 for terminal characters)."
  (+ centre (complex (* scale (- (/ col width) 0.5))
                     (* scale cell-height (/ height width) (- 0.5 (/ row height))))))

;;; ---- ASCII rendering ------------------------------------------------------------------------------------------
(defparameter *ramp* " .:-=+*#%@")

(defun ascii-mandelbrot (width height centre scale)
  (let ((inside 0))
    (dotimes (row height)
      (let ((line (make-string width :initial-element #\Space)))
        (dotimes (col width)
          (let ((n (escape-time (pixel-point col row width height centre scale 2.0))))
            (cond ((>= n *max-iter*) (setf (char line col) #\@) (incf inside))
                  (t (setf (char line col) (char *ramp* (floor (* 8 (/ (log (+ n 1.0)) (log (+ *max-iter* 1.0)))))))))))
        (write-line line)))
    inside))

;;; ---- PPM image -------------------------------------------------------------------------------------------------
(defun colour (smooth)
  "An (r g b) list for a smooth escape count; points in the set are black."
  (if (>= smooth *max-iter*)
      (list 0 0 0)
      (let ((x (* 0.18 smooth)))
        (list (floor (* 255 (+ 0.5 (* 0.5 (sin (+ 3.0 x))))))
              (floor (* 255 (+ 0.5 (* 0.5 (sin (+ 4.2 (* 0.9 x)))))))
              (floor (* 255 (+ 0.5 (* 0.5 (sin (+ 5.4 (* 0.7 x)))))))))))

(defun write-ppm (path width height centre scale)
  (ensure-directories-exist path)
  (let ((in-set 0))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (format out "P3~%~D ~D~%255~%" width height)
      (dotimes (row height)
        (dotimes (col width)
          (multiple-value-bind (n smooth) (escape-time (pixel-point col row width height centre scale))
            (when (>= n *max-iter*) (incf in-set))
            (destructuring-bind (r g b) (colour smooth)
              (format out "~D ~D ~D~%" r g b))))))
    in-set))

;;; ---- run ---------------------------------------------------------------------------------------------------------
(format t "The Mandelbrot set: c is a member when z -> z^2 + c stays bounded from z = 0.~%")
(format t "A few points:~%")
(dolist (c (list #C(0 0) #C(-1 0) #C(0.25 0) #C(0.3 0.5) #C(-0.75 0.1) #C(1 1)))
  (format t "  c = ~A: ~A~%" c (if (>= (escape-time c) *max-iter*) "in the set" (format nil "escapes after ~D steps" (escape-time c)))))

(terpri)
(let* ((w 72) (h 28)
       (inside (ascii-mandelbrot w h #C(-0.6 0.0) 3.4)))
  (format t "~%~D of ~D character cells are inside the set (~,1F%)~%" inside (* w h) (* 100.0 (/ inside (* w h)))))

(let* ((w 160) (h 100) (path "build/examples/mandelbrot.ppm")
       (in-set (write-ppm path w h #C(-0.6 0.0) 3.4)))
  ;; the true area of the set is about 1.5066; the view is 3.4 wide and 3.4*100/160 tall
  (format t "Wrote ~Dx~D image to ~A~%" w h path)
  (format t "Area estimate from the image: ~,3F (true value ~~1.507)~%"
          (* (/ in-set (* w h)) 3.4 (* 3.4 (/ h w)))))

(format t "~%Zoom on the seahorse valley (centre -0.745+0.11i):~%")
(let ((*max-iter* 400)) (ascii-mandelbrot 72 22 #C(-0.745 0.11) 0.05))
