;;;; 70-plot.lisp -- text-mode plots (Unicode braille and block characters) and SVG export.
;;;;
;;;; Every plot function returns a string of lines; PRINT-PLOT writes it and SHOW-PLOT opens it in a window.
;;;; SVG-* functions return an SVG document as a string; WRITE-SVG saves one.

(in-package :cl)

(defun %short-num (x)
  (cond ((integerp x) (format nil "~D" x))
        ((and (/= x 0.0) (or (< (abs x) 1e-3) (>= (abs x) 1e6))) (format nil "~E" x))
        ((>= (abs x) 100.0) (format nil "~,0F" x))
        ((>= (abs x) 10.0) (format nil "~,1F" x))
        (t (format nil "~,2F" x))))

(defun %pad-left (s w) (if (>= (length s) w) s (%string-concat (make-string (- w (length s)) :initial-element #\Space) s)))
(defun %pad-right (s w) (if (>= (length s) w) s (%string-concat s (make-string (- w (length s)) :initial-element #\Space))))
(defun %repeat-string (s n) (let ((parts nil)) (dotimes (i n) (setq parts (cons s parts))) (apply #'%string-concat parts)))
(defun %cp-string (cp) (string (code-char cp)))

;;; ---- braille canvas ------------------------------------------------------------------------------------------------

(defstruct (canvas (:copier nil) (:constructor %make-canvas (cols rows bits))) cols rows bits)

(defun make-canvas (cols rows)
  "A braille canvas COLS x ROWS characters (2 x 4 dots per character)."
  (%make-canvas cols rows (make-array (* cols rows) :initial-element 0)))

(defun canvas-width (c) "Width in dots." (* 2 (canvas-cols c)))
(defun canvas-height (c) "Height in dots." (* 4 (canvas-rows c)))

(defun canvas-set (c x y)
  "Set the dot at (X, Y); the origin is the top left."
  (when (and (>= x 0) (>= y 0) (< x (canvas-width c)) (< y (canvas-height c)))
    (let* ((cx (floor x 2)) (cy (floor y 4)) (dx (mod x 2)) (dy (mod y 4))
           (bit (if (< dy 3) (ash 1 (+ dy (* 3 dx))) (if (= dx 0) 64 128))))
      (setf (aref (canvas-bits c) (+ (* cy (canvas-cols c)) cx)) (logior bit (aref (canvas-bits c) (+ (* cy (canvas-cols c)) cx)))))))

(defun canvas-line (c x0 y0 x1 y1)
  "Draw a line between two dots (Bresenham)."
  (let* ((dx (abs (- x1 x0))) (dy (- (abs (- y1 y0)))) (sx (if (< x0 x1) 1 -1)) (sy (if (< y0 y1) 1 -1)) (err (+ dx dy)))
    (loop (canvas-set c x0 y0)
          (when (and (= x0 x1) (= y0 y1)) (return))
          (let ((e2 (* 2 err)))
            (when (>= e2 dy) (setq err (+ err dy) x0 (+ x0 sx)))
            (when (<= e2 dx) (setq err (+ err dx) y0 (+ y0 sy)))))))

(defun canvas-rows-strings (c)
  "The canvas as a list of strings, one per character row."
  (loop for r from 0 below (canvas-rows c)
        collect (apply #'%string-concat (loop for k from 0 below (canvas-cols c) collect (%cp-string (+ 10240 (aref (canvas-bits c) (+ (* r (canvas-cols c)) k))))))))

(defun %canvas-width-chars (lines) (length (car lines)))

(defun %data-range (v)
  (let ((lo (minimum v)) (hi (maximum v))) (if (= lo hi) (values (- lo 0.5) (+ hi 0.5)) (values lo hi))))

(defun plot-scatter (x y &key (width 60) (height 16) title xlabel ylabel)
  "A braille scatter plot of Y against X."
  (let ((c (make-canvas width height)) (xs (%seq->list x)) (ys (%seq->list y)))
    (multiple-value-bind (xmin xmax) (%data-range xs)
      (multiple-value-bind (ymin ymax) (%data-range ys)
        (loop for a in xs for b in ys
              do (canvas-set c (round (* (/ (- a xmin) (- xmax xmin)) (- (canvas-width c) 1)))
                             (- (canvas-height c) 1 (round (* (/ (- b ymin) (- ymax ymin)) (- (canvas-height c) 1))))))
        (%frame-plot-fixed (canvas-rows-strings c) width ymin ymax xmin xmax title xlabel ylabel)))))

(defun plot-line (x y &key (width 60) (height 16) title xlabel ylabel)
  "A braille line plot of Y against X (points are joined in the order given)."
  (let ((c (make-canvas width height)) (xs (%seq->list x)) (ys (%seq->list y)) (px nil) (py nil))
    (multiple-value-bind (xmin xmax) (%data-range xs)
      (multiple-value-bind (ymin ymax) (%data-range ys)
        (loop for a in xs for b in ys
              do (let ((cx (round (* (/ (- a xmin) (- xmax xmin)) (- (canvas-width c) 1))))
                       (cy (- (canvas-height c) 1 (round (* (/ (- b ymin) (- ymax ymin)) (- (canvas-height c) 1))))))
                   (if px (canvas-line c px py cx cy) (canvas-set c cx cy))
                   (setq px cx py cy)))
        (%frame-plot-fixed (canvas-rows-strings c) width ymin ymax xmin xmax title xlabel ylabel)))))

(defun plot-function (f lo hi &key (points 120) (width 60) (height 16) title)
  "A line plot of the function F over [LO, HI]."
  (let* ((xs (loop for i from 0 below points collect (+ lo (* (- hi lo) (/ i (- points 1.0)))))) (ys (mapcar f xs)))
    (plot-line xs ys :width width :height height :title title)))

(defun %frame-plot-fixed (canvas-lines char-width ymin ymax xmin xmax title xlabel ylabel)
  (let* ((labw (max (length (%short-num ymin)) (length (%short-num ymax)))) (rows (length canvas-lines)) (out nil))
    (when title (push (%string-concat (%repeat-string " " (+ labw 1)) title) out))
    (loop for line in canvas-lines for r from 0
          do (push (%string-concat (%pad-left (cond ((= r 0) (%short-num ymax)) ((= r (- rows 1)) (%short-num ymin)) (t "")) labw) "|" line) out))
    (let ((lo (%short-num xmin)) (hi (%short-num xmax)))
      (push (%string-concat (%repeat-string " " labw) "+" (%repeat-string "-" char-width)) out)
      (push (%string-concat (%repeat-string " " (+ labw 1)) lo (%repeat-string " " (max 1 (- char-width (length lo) (length hi)))) hi) out))
    (when (or xlabel ylabel) (push (%string-concat (%repeat-string " " (+ labw 1)) (or xlabel "") (if ylabel (%string-concat "   (y: " ylabel ")") "")) out))
    (join-lines (reverse out))))

;;; ---- histogram, bar chart, boxplot, heatmap --------------------------------------------------------------------

(defun plot-histogram (data &key (bins 10) (width 50) title)
  "A horizontal text histogram of DATA."
  (multiple-value-bind (counts edges) (histogram-counts data bins)
    (let* ((mx (reduce #'max (coerce counts 'list))) (labels (loop for i from 0 below bins collect (format nil "[~A, ~A)" (%short-num (aref edges i)) (%short-num (aref edges (+ i 1))))))
           (lw (reduce #'max (mapcar #'length labels))) (out nil))
      (when title (push title out))
      (loop for i from 0 below bins for lab in labels
            do (let* ((n (aref counts i)) (len (if (zerop mx) 0 (round (* width (/ n (float mx)))))))
                 (push (%string-concat (%pad-right lab lw) " " (%repeat-string (%cp-string 9608) len) (if (> n 0) (format nil " ~D" n) "")) out)))
      (join-lines (reverse out)))))

(defun plot-bars (labels values &key (width 50) title)
  "A horizontal bar chart of VALUES with LABELS."
  (let* ((ls (mapcar (lambda (l) (format nil "~A" l)) (%seq->list labels))) (vs (%seq->list values)) (lw (reduce #'max (mapcar #'length ls)))
         (mx (reduce #'max vs)) (out nil))
    (when title (push title out))
    (loop for l in ls for v in vs
          do (push (%string-concat (%pad-right l lw) " " (%repeat-string (%cp-string 9608) (if (zerop mx) 0 (max 0 (round (* width (/ v (float mx))))))) " " (%short-num v)) out))
    (join-lines (reverse out))))

(defun plot-boxplot (groups &key labels (width 50) title)
  "Horizontal text box plots of GROUPS (a list of data sequences) on a common axis."
  (let* ((gs (mapcar #'%seq->list groups)) (all (apply #'append gs)) (lo (minimum all)) (hi (maximum all)) (span (if (= lo hi) 1.0 (- hi lo)))
         (names (or labels (loop for i from 1 to (length gs) collect (format nil "~D" i)))) (lw (reduce #'max (mapcar (lambda (n) (length (format nil "~A" n))) names))) (out nil))
    (flet ((pos (v) (round (* (- width 1) (/ (- v lo) span)))))
      (when title (push title out))
      (loop for g in gs for nm in names
            do (let* ((f (five-number-summary g)) (line (make-string width :initial-element #\Space)))
                 (loop for k from (pos (first f)) to (pos (fifth f)) do (setf (char line k) #\-))
                 (loop for k from (pos (second f)) to (pos (fourth f)) do (setf (char line k) #\=))
                 (setf (char line (pos (first f))) #\|) (setf (char line (pos (fifth f))) #\|) (setf (char line (pos (third f))) #\#)
                 (push (%string-concat (%pad-right (format nil "~A" nm) lw) " " line) out)))
      (push (%string-concat (%repeat-string " " (+ lw 1)) (%short-num lo) (%repeat-string " " (max 1 (- width (length (%short-num lo)) (length (%short-num hi))))) (%short-num hi)) out))
    (join-lines (reverse out))))

(defun plot-heatmap (m &key title)
  "A shaded text heatmap of the matrix M (two characters per cell)."
  (let* ((lo (reduce #'min (coerce (matrix-data m) 'list))) (hi (reduce #'max (coerce (matrix-data m) 'list))) (span (if (= lo hi) 1.0 (- hi lo)))
         (shades (list " " (%cp-string 9617) (%cp-string 9618) (%cp-string 9619) (%cp-string 9608))) (out nil))
    (when title (push title out))
    (dotimes (i (matrix-rows m))
      (push (apply #'%string-concat (loop for j from 0 below (matrix-cols m)
                                          collect (let ((s (nth (min 4 (floor (* 5 (/ (- (mref m i j) lo) span)))) shades))) (%string-concat s s))))
            out))
    (push (format nil "scale: ~A (light) to ~A (dark)" (%short-num lo) (%short-num hi)) out)
    (join-lines (reverse out))))

;;; ---- output ----------------------------------------------------------------------------------------------------------------

(defun print-plot (plot &optional (stream *standard-output*))
  "Write the plot string PLOT to STREAM."
  (write-string plot stream) (terpri stream) (values))

(defun show-plot (plot &key (title "Plot"))
  "Show the plot string PLOT in a window when the window system is running, otherwise print it."
  (if (and (boundp '*wm*) (symbol-value '*wm*))
      (open-report (symbol-value '*wm*) title (lambda (w out) (declare (ignore w)) (write-string plot out) (terpri out)) :w 76 :h (min 30 (+ 4 (length (split-lines plot)))))
      (print-plot plot))
  (values))

(define-command (histogram :documentation "Show a histogram of the data a Lisp expression returns") ((expr string :prompt "Expression"))
  (show-plot (plot-histogram (eval (read-from-string expr))) :title "Histogram"))

;;; ---- SVG ---------------------------------------------------------------------------------------------------------------------

(defun %svg-escape (s) (let ((o (make-string-output-stream))) (loop for c across s do (case c (#\& (write-string "&amp;" o)) (#\< (write-string "&lt;" o)) (#\> (write-string "&gt;" o)) (t (write-char c o)))) (get-output-stream-string o)))

(defun %svg-frame (width height title body xmin xmax ymin ymax)
  (with-output-to-string (s)
    (format s "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"~D\" height=\"~D\" viewBox=\"0 0 ~D ~D\" font-family=\"sans-serif\" font-size=\"11\">~%" width height width height)
    (format s "<rect width=\"~D\" height=\"~D\" fill=\"white\"/>~%" width height)
    (when title (format s "<text x=\"~D\" y=\"16\" text-anchor=\"middle\" font-size=\"13\">~A</text>~%" (floor width 2) (%svg-escape title)))
    (format s "<line x1=\"40\" y1=\"~D\" x2=\"~D\" y2=\"~D\" stroke=\"black\"/><line x1=\"40\" y1=\"24\" x2=\"40\" y2=\"~D\" stroke=\"black\"/>~%" (- height 30) (- width 10) (- height 30) (- height 30))
    (format s "<text x=\"40\" y=\"~D\">~A</text><text x=\"~D\" y=\"~D\" text-anchor=\"end\">~A</text>~%" (- height 14) (%short-num xmin) (- width 10) (- height 14) (%short-num xmax))
    (format s "<text x=\"36\" y=\"~D\" text-anchor=\"end\">~A</text><text x=\"36\" y=\"32\" text-anchor=\"end\">~A</text>~%" (- height 30) (%short-num ymin) (%short-num ymax))
    (write-string body s)
    (format s "</svg>~%")))

(defun %svg-scale (v lo hi a b) (+ a (* (- b a) (/ (- v lo) (if (= hi lo) 1.0 (- hi lo))))))

(defun svg-scatter (x y &key (width 480) (height 320) title (radius 3))
  "An SVG scatter plot of Y against X."
  (let ((xs (%seq->list x)) (ys (%seq->list y)))
    (multiple-value-bind (xmin xmax) (%data-range xs)
      (multiple-value-bind (ymin ymax) (%data-range ys)
        (%svg-frame width height title
                    (with-output-to-string (s)
                      (loop for a in xs for b in ys
                            do (format s "<circle cx=\"~,2F\" cy=\"~,2F\" r=\"~D\" fill=\"#3366cc\" fill-opacity=\"0.7\"/>~%"
                                       (%svg-scale a xmin xmax 40 (- width 10)) (%svg-scale b ymin ymax (- height 30) 24) radius)))
                    xmin xmax ymin ymax)))))

(defun svg-line (x y &key (width 480) (height 320) title)
  "An SVG line plot of Y against X."
  (let ((xs (%seq->list x)) (ys (%seq->list y)))
    (multiple-value-bind (xmin xmax) (%data-range xs)
      (multiple-value-bind (ymin ymax) (%data-range ys)
        (%svg-frame width height title
                    (format nil "<polyline fill=\"none\" stroke=\"#cc3333\" stroke-width=\"1.5\" points=\"~{~A~^ ~}\"/>~%"
                            (loop for a in xs for b in ys collect (format nil "~,2F,~,2F" (%svg-scale a xmin xmax 40 (- width 10)) (%svg-scale b ymin ymax (- height 30) 24))))
                    xmin xmax ymin ymax)))))

(defun svg-histogram (data &key (bins 10) (width 480) (height 320) title)
  "An SVG histogram of DATA."
  (multiple-value-bind (counts edges) (histogram-counts data bins)
    (let* ((mx (reduce #'max (coerce counts 'list))) (xmin (aref edges 0)) (xmax (aref edges bins)))
      (%svg-frame width height title
                  (with-output-to-string (s)
                    (dotimes (i bins)
                      (let* ((x0 (%svg-scale (aref edges i) xmin xmax 40 (- width 10))) (x1 (%svg-scale (aref edges (+ i 1)) xmin xmax 40 (- width 10)))
                             (h (* (- height 54) (/ (aref counts i) (float (max 1 mx))))))
                        (format s "<rect x=\"~,2F\" y=\"~,2F\" width=\"~,2F\" height=\"~,2F\" fill=\"#3366cc\" stroke=\"white\"/>~%" x0 (- height 30 h) (- x1 x0) h))))
                  xmin xmax 0 mx))))

(defun svg-heatmap (m &key (cell 20) title)
  "An SVG heatmap of the matrix M."
  (let* ((lo (reduce #'min (coerce (matrix-data m) 'list))) (hi (reduce #'max (coerce (matrix-data m) 'list)))
         (w (+ 20 (* cell (matrix-cols m)))) (h (+ 40 (* cell (matrix-rows m)))))
    (with-output-to-string (s)
      (format s "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"~D\" height=\"~D\" font-family=\"sans-serif\" font-size=\"12\">~%<rect width=\"~D\" height=\"~D\" fill=\"white\"/>~%" w h w h)
      (when title (format s "<text x=\"10\" y=\"16\">~A</text>~%" (%svg-escape title)))
      (dotimes (i (matrix-rows m))
        (dotimes (j (matrix-cols m))
          (let ((f (%svg-scale (mref m i j) lo hi 0.0 1.0)))
            (format s "<rect x=\"~D\" y=\"~D\" width=\"~D\" height=\"~D\" fill=\"rgb(~D,~D,~D)\"/>~%" (+ 10 (* j cell)) (+ 30 (* i cell)) cell cell
                    (round (- 255 (* 200 f))) (round (- 255 (* 150 f))) 255))))
      (format s "</svg>~%"))))

(defun write-svg (svg path)
  "Write the SVG document string SVG to PATH."
  (with-open-file (s path :direction :output :if-exists :supersede) (write-string svg s))
  path)
