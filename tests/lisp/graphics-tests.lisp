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

;;; ---- palette images and the demos --------------------------------------------------------------------------

(deftest palette-images
  (let* ((w 40) (h 40) (ix (make-string (* w h))) (img (make-image w h)) (pal (make-array 256 :initial-element 0)))
    (%idx-fill ix w h 255)
    (%idx-disc ix w h 20 20 15 10 20 0)                          ; a shaded sphere in indices 10..29
    (flet ((at (x y) (char-code (char ix (+ (* y w) x)))))
      (is-equal 255 (at 0 0))                                    ; outside
      (is (<= 10 (at 20 20) 29))
      (is (> (at 14 14) (at 26 26)))                             ; lit from the upper left
      (%idx-disc ix w h 20 20 15 100 5 1)                        ; rings 100..104, centre out
      (is-equal 100 (at 20 20))
      (is-equal 104 (at 20 6)))
    (setf (aref pal 255) #x112233 (aref pal 100) #xFF0000)
    (%idx-map ix (image-pixels img) w h pal)
    (is-equal (rgb #x11 #x22 #x33) (image-pixel img 0 0))
    (is-equal (rgb 255 0 0) (image-pixel img 20 20))
    (is (null (ignore-errors (%idx-disc ix w h 20 20 5 250 10 0))))     ; past index 255
    (is (null (ignore-errors (%idx-map ix (image-pixels img) w h (make-array 3)))))))

(defun find-demo-window (wm) (find-if (lambda (w) (typep w 'demo-window)) (wm-windows wm)))

(deftest demos
  (multiple-value-bind (wm lw) (make-test-wm 30 100)
    (let ((*graphics* nil))
      ;; :demo alone pops up the menu; Return opens the first demo
      (type-line wm ":demo")
      (wm-draw wm)
      (is (screen-has wm "Demos"))
      (is (screen-has wm "Colour Circles"))
      (is (screen-has wm "Munching Squares"))
      (wm-feed wm (string (code-char 13)))
      (let ((d (find-demo-window wm)))
        (is (typep d 'circles-demo))
        (wm-draw wm)
        (is (screen-has wm "Space pause"))
        (is (demo-image d))
        ;; colour-map cycling: the frames differ, the shapes (indices) do not
        (let ((before (copy-seq (image-pixels (demo-image d)))) (ix (copy-seq (circles-indices d))))
          (dotimes (i 5) (wm-animate wm (+ 1000000 (* i 1000))))
          (is (string/= before (image-pixels (demo-image d))))
          (is (string= ix (circles-indices d))))
        (is (> (demo-frames d) 0))
        ;; half blocks on the screen
        (wm-draw wm)
        (is (block find (dotimes (r 30) (dotimes (c 100) (when (= 9600 (car (screen-cell (wm-screen wm) r c))) (return-from find t))))))
        ;; keys: S rings, space pauses (no more frames), + speeds up, Q closes
        (wm-feed wm "s")
        (is-equal 1 (circles-style d))
        (wm-feed wm " ")
        (is (demo-paused d))
        (is (null (window-frame-interval d)))
        (is (null (wm-animate wm)))
        (wm-feed wm " ")
        (is (window-frame-interval d))
        (wm-feed wm "+")
        (is-equal 1 (demo-speed d))
        (wm-feed wm "q")
        (is (null (find-demo-window wm)))
        (is (null (wm-animate wm))))
      ;; every demo runs a few frames by name, in half blocks and with terminal graphics
      (dolist (e *demos*)
        (dolist (g '(nil :kitty))
          (let ((*graphics* g))
            (wm-raise wm lw)
            (type-line wm (format nil ":demo ~A" (string-downcase (symbol-name (car e)))))
            (let ((d (find-demo-window wm)))
              (is (typep d (caddr e)))
              (wm-draw wm)
              (dotimes (i 3) (window-animate d))
              (wm-draw wm)
              (is (null (wm-message wm)))
              (wm-close-window wm d)))))
      (graphics-reset)
      (wm-raise wm lw)
      (type-line wm ":demo nosuch")
      (is (screen-has wm "No demo named nosuch")))))
