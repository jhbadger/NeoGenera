;;;; graphics-tests.lisp -- images, the kitty placeholder encoding, half blocks, graphs.

(in-package :cl)

(deftest image-drawing
  (let ((img (make-image 40 20 (rgb 255 255 255))))
    (is-equal (rgb 255 255 255) (image-pixel img 0 0))
    (image-fill-rect img 10 5 5 5 (rgb 255 0 0))
    (is-equal (rgb 255 0 0) (image-pixel img 12 7))
    (is-equal (rgb 255 255 255) (image-pixel img 9 7))
    (image-line img 0 15 39 15 (rgb 0 0 255) 3)
    (is-equal (rgb 0 0 255) (image-pixel img 20 15))      ; on the line
    (is-equal (rgb 255 255 255) (image-pixel img 20 10))  ; off it
    (image-disc img 30 5 3 (rgb 0 128 0))
    (is-equal (rgb 0 128 0) (image-pixel img 30 5))
    ;; anti-aliasing: the disc's edge is a blend
    (let ((edge (image-pixel img 32 6))) (is (not (member edge (list (rgb 0 128 0) (rgb 255 255 255))))))
    (is (null (ignore-errors (image-pixel img 40 0))))
    (is (null (ignore-errors (%img-line "abc" 40 20 0 0 1 1 0 1))))))   ; pixel string too short

(deftest image-placeholder-encoding
  (is-equal 297 (length *image-diacritics*))
  (is-equal #x0305 (aref *image-diacritics* 0))
  (is-equal #x1D244 (aref *image-diacritics* 296))
  (let ((code (image-cell-code 7 3 41)))
    (is (image-cell-p code))
    (is-equal '(7 3 41) (multiple-value-list (%image-cell-parts code)))
    (is (not (image-cell-p #x10FFFF)))
    (is-equal " " (%cell-string code)))
  ;; the flush writes U+10EEEE with the image number as colour and the row and column as diacritics
  (let ((*graphics* :kitty) (s (make-screen 3 10)) (img (make-image 20 20)))
    (graphics-reset)
    (let ((key (image-placement-key img 4 2)))
      (screen-put s 1 2 (image-cell-code key 1 3) -1 -1 0)
      (let* ((out (coerce (screen-flush s nil :truecolor) 'list)) (p (position #x10EEEE out)))
        (is p)
        (is-equal (aref *image-diacritics* 1) (nth (+ p 1) out))
        (is-equal (aref *image-diacritics* 3) (nth (+ p 2) out))
        (let ((id (placement-term-id (gethash key *placements*))))
          (is (<= 1 id 255))
          ;; the SGR before it sets the foreground to the 256-colour index ID
          (is (search (map 'list #'char-code (format nil "38;5;~D" id)) out))))
      (graphics-reset)))
  (is (null (ignore-errors (image-placement-key (make-image 10 10) 400 2)))))

(deftest terminal-replies-are-not-keys
  ;; graphics and XTVERSION replies arriving later must not turn into keystrokes
  (let ((esc (string (code-char 27))))
    (is-equal '((:key 97 0)) (%term-parse (%string-concat esc "_Gi=31;OK" esc "\\" "a")))
    (is-equal '((:key 98 0)) (%term-parse (%string-concat esc "P>|kitty(0.35)" esc "\\" "b")))
    (is-equal '((:key 95 2)) (%term-parse (%string-concat esc "_")))))                 ; Meta-_ still works

(deftest image-in-window
  (multiple-value-bind (wm lw) (make-test-wm 30 100)
    (let ((img (make-image 80 40 (rgb 255 0 0))))
      (image-fill-rect img 0 20 80 20 (rgb 0 0 255))
      ;; half blocks when the terminal does not draw images: red over blue
      (let ((*graphics* nil))
        (draw-image img :stream (lw-stream lw) :cols 8 :rows 2)
        (wm-draw wm)
        (let ((pos (block find (dotimes (r 30) (dotimes (c 100) (when (= 9600 (car (screen-cell (wm-screen wm) r c))) (return-from find (cons r c))))))))
          (is pos)
          (when pos
            (destructuring-bind (ch fg bg at) (screen-cell (wm-screen wm) (car pos) (cdr pos))
              (declare (ignore ch at))
              (is-equal (rgb 255 0 0) fg)
              (is-equal (rgb 255 0 0) bg))           ; the first cell row is all red
            (destructuring-bind (ch fg bg at) (screen-cell (wm-screen wm) (+ 1 (car pos)) (cdr pos))
              (declare (ignore ch at))
              (is-equal (rgb 0 0 255) fg)))))
      ;; placeholder cells when it does
      (let ((*graphics* :kitty))
        (wm-draw wm)
        (is (block find (dotimes (r 30) (dotimes (c 100) (when (image-cell-p (car (screen-cell (wm-screen wm) r c))) (return-from find t)))))))
      (graphics-reset))))

(deftest graphs
  (let* ((xs (loop for i below 50 collect (/ i 5.0)))
         (g (graph-line xs (mapcar #'sin xs) :title "Sine" :width 40 :height 10)))
    (is (graph-p g))
    (is-equal 40 (graph-cols g))
    (is (image-p (graph-image g)))
    ;; printed (any stream other than a window pane), a graph is the text plot
    (is (%string-search "Sine" (prin1-to-string g) 0))
    (is (string= (graph-text g) (prin1-to-string g)))
    (is (graph-p (graph-scatter xs xs)))
    (is (graph-p (graph-function #'exp 0 1)))
    (is (graph-p (graph-histogram (loop for i below 100 collect (mod (* i 37) 11)))))
    (let ((m (make-matrix 3 4)))
      (dotimes (i 3) (dotimes (j 4) (setf (mref m i j) (+ i j))))
      (is (graph-p (graph-heatmap m))))
    ;; in a window with terminal graphics: title, image rows and the axis ranges
    (multiple-value-bind (wm lw) (make-test-wm 30 100)
      (let ((*graphics* :kitty))
        (present g :stream (lw-stream lw))
        (wm-draw wm)
        (is (screen-has wm "Sine"))
        (is (block find (dotimes (r 30) (dotimes (c 100) (when (image-cell-p (car (screen-cell (wm-screen wm) r c))) (return-from find t))))))
        (graphics-reset))
      ;; without: the text plot
      (let ((*graphics* nil))
        (type-line wm "(graph-function (function sin) 0 6 :title \"Plain\")")
        (is (screen-has wm "Plain"))
        (is (not (block find (dotimes (r 30) (dotimes (c 100) (when (image-cell-p (car (screen-cell (wm-screen wm) r c))) (return-from find t)))))))))))

(deftest graphics-detection-preferences
  (let ((*graphics-preference* :off)) (is (null (detect-graphics))))
  ;; without a terminal the probe has nothing to ask
  (let ((*graphics-preference* :on)) (is (null (detect-graphics))))
  (is (stringp (graphics-status))))
