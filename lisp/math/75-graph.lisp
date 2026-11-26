;;;; 75-graph.lisp -- graphs drawn as images.
;;;;
;;;; GRAPH-SCATTER, GRAPH-LINE, GRAPH-FUNCTION, GRAPH-HISTOGRAM and GRAPH-HEATMAP return a GRAPH: a picture of
;;;; the data in pixels, framed by text labels.  In the window system on a terminal that shows images (kitty,
;;;; Ghostty: see DETECT-GRAPHICS) a graph returned to the Listener, or shown with SHOW-PLOT, is drawn as that
;;;; picture; everywhere else it prints as the corresponding text plot (PLOT-SCATTER, ...), so the same code
;;;; works on any terminal.

(in-package :cl)

(defstruct (graph (:constructor %make-graph) (:copier nil))
  kind title image cols rows xmin xmax ymin ymax xlabel ylabel text)

(defmethod print-object ((g graph) stream)
  (if (and (%stream-terminal-p stream) (%kitty-like-p ""))
      (progn (when (graph-title g) (format stream "~A~%" (graph-title g)))
             (draw-image (graph-image g) :stream stream :cols (graph-cols g) :rows (graph-rows g)))
      (write-string (graph-text g) stream)))

(defparameter *graph-colors* (list (rgb 31 119 180) (rgb 255 127 14) (rgb 44 160 44) (rgb 214 39 40) (rgb 148 103 189))
  "Colours of the data series of graphs, in order.")

(defun %rgb-parts (c) (let ((v (%color-bits c))) (values (floor v 65536) (mod (floor v 256) 256) (mod v 256))))
(defun %mix (a b f)
  (multiple-value-bind (r1 g1 b1) (%rgb-parts a)
    (multiple-value-bind (r2 g2 b2) (%rgb-parts b)
      (rgb (round (+ r1 (* f (- r2 r1)))) (round (+ g1 (* f (- g2 g1)))) (round (+ b1 (* f (- b2 b1))))))))
(defun %theme-rgb (key default) (let ((c (tc key))) (if (>= c #x1000000) c default)))

(defun %graph-image (cols rows)
  "A blank graph image for COLS x ROWS cells, at the terminal's cell resolution (at most 1600 x 1000 pixels)."
  (let* ((w (* cols *cell-pixel-width*)) (h (* rows *cell-pixel-height*)) (k (min 1 (/ 1600 w) (/ 1000 h))))
    (make-image (max 8 (round (* w k))) (max 8 (round (* h k))) (%theme-rgb :bg (rgb 255 255 255)))))

(defun %graph-frame (img)
  "Light grid lines at the quarters and a frame; returns a function mapping data to pixels once given ranges."
  (let* ((w (image-width img)) (h (image-height img)) (ink (%theme-rgb :fg (rgb 20 20 20))) (bg (%theme-rgb :bg (rgb 255 255 255)))
         (grid (%mix bg ink 0.12)))
    (dotimes (i 5)
      (let ((x (* (- w 1) (/ i 4))) (y (* (- h 1) (/ i 4))))
        (image-line img x 0 x (- h 1) (if (or (= i 0) (= i 4)) (%mix bg ink 0.45) grid) 1)
        (image-line img 0 y (- w 1) y (if (or (= i 0) (= i 4)) (%mix bg ink 0.45) grid) 1)))
    img))

(defun %graph-mapper (img xmin xmax ymin ymax)
  (let* ((w (image-width img)) (h (image-height img)) (pad (max 4 (* 0.04 (min w h))))
         (sx (/ (- w (* 2 pad)) (if (= xmax xmin) 1 (- xmax xmin)))) (sy (/ (- h (* 2 pad)) (if (= ymax ymin) 1 (- ymax ymin)))))
    (lambda (x y) (values (+ pad (* sx (- x xmin))) (- h pad (* sy (- y ymin)))))))

(defun %make-xy-graph (kind xs ys width height title xlabel ylabel draw text)
  (multiple-value-bind (xmin xmax) (%data-range xs)
    (multiple-value-bind (ymin ymax) (%data-range ys)
      (let* ((img (%graph-frame (%graph-image width height))) (map (%graph-mapper img xmin xmax ymin ymax)))
        (funcall draw img map)
        (%make-graph :kind kind :title title :image img :cols width :rows height :xmin xmin :xmax xmax :ymin ymin :ymax ymax
                     :xlabel xlabel :ylabel ylabel :text text)))))

(defun graph-scatter (x y &key (width 60) (height 16) title xlabel ylabel (color (first *graph-colors*)) (radius nil))
  "A scatter graph of Y against X, drawn in pixels where the terminal can (else it prints as PLOT-SCATTER)."
  (let ((xs (%seq->list x)) (ys (%seq->list y)))
    (%make-xy-graph :scatter xs ys width height title xlabel ylabel
                    (lambda (img map)
                      (let ((r (or radius (max 2 (/ (min *cell-pixel-width* *cell-pixel-height*) 3.5)))))
                        (loop for a in xs for b in ys do (multiple-value-bind (px py) (funcall map a b) (image-disc img px py r color)))))
                    (plot-scatter xs ys :width width :height height :title title :xlabel xlabel :ylabel ylabel))))

(defun graph-line (x y &key (width 60) (height 16) title xlabel ylabel (color (first *graph-colors*)) (thickness 2))
  "A line graph of Y against X (points joined in the order given)."
  (let ((xs (%seq->list x)) (ys (%seq->list y)))
    (%make-xy-graph :line xs ys width height title xlabel ylabel
                    (lambda (img map)
                      (image-polyline img (loop for a in xs for b in ys collect (multiple-value-bind (px py) (funcall map a b) (cons px py)))
                                      color thickness))
                    (plot-line xs ys :width width :height height :title title :xlabel xlabel :ylabel ylabel))))

(defun graph-function (f lo hi &key (points 300) (width 60) (height 16) title (color (first *graph-colors*)))
  "A line graph of the function F over [LO, HI]."
  (let* ((xs (loop for i from 0 below points collect (+ lo (* (- hi lo) (/ i (- points 1.0)))))) (ys (mapcar f xs)))
    (graph-line xs ys :width width :height height :title title :color color)))

(defun graph-histogram (data &key (bins 10) (width 60) (height 16) title (color (first *graph-colors*)))
  "A histogram of DATA as vertical bars (in text, as PLOT-HISTOGRAM)."
  (multiple-value-bind (counts edges) (histogram-counts data bins)
    (let* ((mx (max 1 (reduce #'max (coerce counts 'list)))) (img (%graph-frame (%graph-image width height)))
           (map (%graph-mapper img (aref edges 0) (aref edges bins) 0 mx)) (edge (%mix color (rgb 0 0 0) 0.35)))
      (dotimes (i bins)
        (multiple-value-bind (x0 y0) (funcall map (aref edges i) (aref counts i))
          (multiple-value-bind (x1 y1) (funcall map (aref edges (+ i 1)) 0)
            (when (> (aref counts i) 0)
              (image-fill-rect img (+ x0 1) y0 (max 1 (- x1 x0 2)) (- y1 y0) color)
              (image-line img (+ x0 1) y0 (- x1 1) y0 edge 1)))))
      (%make-graph :kind :histogram :title title :image img :cols width :rows height :xmin (aref edges 0) :xmax (aref edges bins)
                   :ymin 0 :ymax mx :text (plot-histogram data :bins bins :title title)))))

(defun %viridis (f)
  "A colour from dark blue (0) through green to yellow (1)."
  (let* ((stops (list (rgb 68 1 84) (rgb 59 82 139) (rgb 33 145 140) (rgb 94 201 98) (rgb 253 231 37)))
         (f (max 0 (min 1 f))) (i (min 3 (floor (* f 4)))))
    (%mix (nth i stops) (nth (+ i 1) stops) (- (* f 4) i))))

(defun graph-heatmap (m &key (width nil) (height nil) title)
  "A heatmap of the matrix M, coloured from dark blue (smallest) to yellow (largest)."
  (let* ((nr (matrix-rows m)) (nc (matrix-cols m))
         (width (or width (min 80 (max 10 (* 2 nc))))) (height (or height (min 40 (max 4 nr))))
         (data (coerce (matrix-data m) 'list)) (lo (reduce #'min data)) (hi (reduce #'max data)) (span (if (= lo hi) 1 (- hi lo)))
         (img (%graph-image width height)) (cw (/ (image-width img) nc)) (ch (/ (image-height img) nr)))
    (dotimes (i nr)
      (dotimes (j nc)
        (image-fill-rect img (* j cw) (* i ch) (+ cw 1) (+ ch 1) (%viridis (/ (- (mref m i j) lo) span)))))
    (%make-graph :kind :heatmap :title title :image img :cols width :rows height :xmin 1 :xmax nc :ymin nr :ymax 1
                 :xlabel (format nil "scale: ~A (blue) to ~A (yellow)" (%short-num lo) (%short-num hi))
                 :text (plot-heatmap m :title title))))

;;; ---- showing graphs ------------------------------------------------------------------------------------------

(defun %draw-graph (g out)
  "Draw graph G on the pane stream OUT: the title, the picture with the y range on its left, and the x range."
  (let* ((labw (max (length (%short-num (graph-ymin g))) (length (%short-num (graph-ymax g))))) (rows (graph-rows g)) (cols (graph-cols g)))
    (fresh-line out)
    (when (graph-title g) (with-style (out :bold) (format out "~A~A~%" (%repeat-string " " (+ labw 1)) (graph-title g))))
    (dotimes (r rows)
      (with-style (out :dim)
        (write-string (%pad-left (cond ((= r 0) (%short-num (graph-ymax g))) ((= r (- rows 1)) (%short-num (graph-ymin g))) (t "")) labw) out)
        (write-string " " out))
      (draw-image-row out (graph-image g) r cols rows)
      (terpri out))
    (let ((lo (%short-num (graph-xmin g))) (hi (%short-num (graph-xmax g))))
      (with-style (out :dim)
        (format out "~A~A~A~A~%" (%repeat-string " " (+ labw 1)) lo (%repeat-string " " (max 1 (- cols (length lo) (length hi)))) hi)))
    (when (or (graph-xlabel g) (graph-ylabel g))
      (with-style (out :dim)
        (format out "~A~A~A~%" (%repeat-string " " (+ labw 1)) (or (graph-xlabel g) "") (if (graph-ylabel g) (format nil "   (y: ~A)" (graph-ylabel g)) ""))))
    g))

(defmethod present-graphically ((g graph) stream)
  (when (eq *graphics* :kitty) (%draw-graph g stream) t))

(define-presentation-type 'graph '(t))
