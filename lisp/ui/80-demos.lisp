;;;; 80-demos.lisp -- graphics demos, and the :Demo menu that starts them.
;;;;
;;;; In the spirit of the Lisp Machine's display hacks.  Each demo is a DEMO-WINDOW subclass that draws into an
;;;; image covering the window: DEMO-SETUP starts it on a fresh image, DEMO-STEP draws the next frame.  The window
;;;; is animated by the window manager (WINDOW-FRAME-INTERVAL / WINDOW-ANIMATE) and shows the image the same way
;;;; other images are shown: kitty graphics where the terminal has them, half blocks elsewhere.
;;;;
;;;; Colour Circles draws shaded discs once, as palette indices, and then only changes the palette: colour-map
;;;; animation, as colour framebuffers did it.

(in-package :cl)

;;; ---- the registry ---------------------------------------------------------------------------------------------

(defvar *demos* nil "Entries (name label class documentation), in menu order.")

(defun define-demo (name label class documentation)
  "Register a demo for the :Demo menu.  NAME is a keyword, LABEL its title, CLASS a DEMO-WINDOW subclass."
  (let ((entry (list name label class documentation)))
    (setq *demos* (if (assoc name *demos*)
                      (mapcar (lambda (e) (if (eq (car e) name) entry e)) *demos*)
                      (append *demos* (list entry))))
    name))

(defun find-demo (name)
  "The demo whose name or label starts with NAME (a string or keyword), or NIL."
  (let ((s (string-upcase (string name))))
    (or (assoc (intern s "KEYWORD") *demos*)
        (find-if (lambda (e) (or (%word-prefix-p s (symbol-name (car e))) (%word-prefix-p s (string-upcase (cadr e)))))
                 *demos*))))

;;; ---- the demo window ------------------------------------------------------------------------------------------

(defclass demo-window (window)
  ((image :initform nil :accessor demo-image)
   (cols :initform 0 :accessor demo-cols)          ; cells the image covers
   (rows :initform 0 :accessor demo-rows)
   (paused :initform nil :accessor demo-paused)
   (speed :initform 0 :accessor demo-speed)        ; -3 .. 3: slower frames below 0, more steps per frame above
   (frames :initform 0 :accessor demo-frames)))

(defgeneric demo-setup (window)
  (:documentation "Start the demo on its (new, blank) image."))
(defgeneric demo-step (window)
  (:documentation "Advance the demo one step, drawing on its image."))
(defgeneric demo-key (window char)
  (:documentation "Handle a key of the demo's own (a character); true if it did."))
(defgeneric demo-keys-help (window)
  (:documentation "A short description of the demo's own keys, for the bottom line."))
(defgeneric demo-base-interval (window)
  (:documentation "Milliseconds between frames at normal speed."))

(defmethod demo-setup ((w demo-window)) nil)
(defmethod demo-step ((w demo-window)) nil)
(defmethod demo-key ((w demo-window) char) (declare (ignore char)) nil)
(defmethod demo-keys-help ((w demo-window)) "")
(defmethod demo-base-interval ((w demo-window)) (if (eq *graphics* :kitty) 60 40))

(defun demo-width (w) (image-width (demo-image w)))
(defun demo-height (w) (image-height (demo-image w)))

(defun %demo-pixel-size (cols rows)
  "Pixel size of an image covering COLS x ROWS cells with square pixels.  Half blocks show two pixels per cell, so
the image has twice that in each direction (the extra is averaged, smoothing edges); terminal graphics get the
cells' full resolution, scaled down to keep each frame small enough to send."
  (if (eq *graphics* :kitty)
      (let* ((pw (* cols *cell-pixel-width*)) (ph (* rows *cell-pixel-height*)) (budget 60000))
        (if (<= (* pw ph) budget)
            (values pw ph)
            (let ((f (sqrt (/ budget (* pw ph)))))
              (values (max 1 (floor (* pw f))) (max 1 (floor (* ph f)))))))
      (values (* cols 2) (* rows 4))))

(defun demo-restart (w)
  "Start the demo again from the beginning, on a new image."
  (let ((img (demo-image w)))
    (when img
      (image-fill-rect img 0 0 (image-width img) (image-height img) (rgb 0 0 0))
      (setf (demo-frames w) 0)
      (demo-setup w)
      (demo-step w)
      (image-changed img))))

(defun %demo-ensure-image (w cols rows)
  "Make sure the image fits COLS x ROWS cells; a new size starts the demo again."
  (unless (and (demo-image w) (= cols (demo-cols w)) (= rows (demo-rows w))
               (multiple-value-bind (pw ph) (%demo-pixel-size cols rows)
                 (and (= pw (demo-width w)) (= ph (demo-height w)))))
    (when (demo-image w) (forget-image (demo-image w)))
    (multiple-value-bind (pw ph) (%demo-pixel-size cols rows)
      (setf (demo-image w) (make-image pw ph (rgb 0 0 0)) (demo-cols w) cols (demo-rows w) rows))
    (demo-restart w)))

(defmethod window-frame-interval ((w demo-window))
  (and (not (demo-paused w)) (demo-image w)
       (let ((s (demo-speed w))) (if (< s 0) (* (demo-base-interval w) (expt 2 (- s))) (demo-base-interval w)))))

(defmethod window-animate ((w demo-window))
  (let ((img (demo-image w)))
    (when (and img (not (demo-paused w)))
      (dotimes (i (if (> (demo-speed w) 0) (expt 2 (demo-speed w)) 1))
        (demo-step w)
        (setf (demo-frames w) (+ 1 (demo-frames w))))
      (image-changed img)
      ;; the terminal redraws its own copy of the image; half blocks need the cells drawn again
      (not (eq *graphics* :kitty)))))

(defun %demo-help-line (w)
  (format nil "~:[Space pause~;Space resume~]  N new  +/- speed~@[ (~@D)~]  ~A~:[~;  ~]Q quit"
          (demo-paused w) (and (/= 0 (demo-speed w)) (demo-speed w))
          (demo-keys-help w) (> (length (demo-keys-help w)) 0)))

(defmethod window-draw-content ((w demo-window) screen ix iy iw ih focused)
  (declare (ignore focused))
  (let ((rows (- ih 1)))
    (when (and (> iw 0) (> rows 0))
      (%demo-ensure-image w iw rows)
      (draw-image-cells screen iy ix (demo-image w) iw rows))
    (when (> ih 0)
      (screen-fill screen (+ iy ih -1) ix 1 iw 32 (tc :fg) (tc :bg) 0)
      (let ((help (%demo-help-line w)))
        (screen-draw-string screen (+ iy ih -1) (+ ix 1) help (tc :fg) (tc :bg) +dim+ 0 (max 0 (min (length help) (- iw 1)))))))
  (setf (window-content-height w) 0))

(defmethod window-key ((w demo-window) key)
  (let ((c (key-code key)))
    (when (and (integerp c) (zerop (logand (key-mods key) (logior +ctrl+ +meta+))))
      (let ((ch (char-downcase (code-char c))))
        (cond ((char= ch #\Space) (setf (demo-paused w) (not (demo-paused w))) t)
              ((char= ch #\q) (wm-close-window (window-wm w) w) t)
              ((char= ch #\n) (demo-restart w) t)
              ((member ch '(#\+ #\=)) (setf (demo-speed w) (min 3 (+ (demo-speed w) 1))) t)
              ((member ch '(#\- #\_)) (setf (demo-speed w) (max -3 (- (demo-speed w) 1))) t)
              ((demo-key w ch) (image-changed (demo-image w)) t)
              (t nil))))))

(defmethod window-mouse ((w demo-window) kind lx ly button mods)
  (declare (ignore lx ly mods))
  (when (and (eq kind :press) (= button 0)) (setf (demo-paused w) (not (demo-paused w))))
  t)

(defmethod window-hover-doc ((w demo-window) lx ly)
  (declare (ignore lx ly))
  (if (demo-paused w) "L: resume" "L: pause"))

(defmethod window-close ((w demo-window))
  (when (demo-image w) (forget-image (demo-image w)) (setf (demo-image w) nil))
  t)

(defun open-demo (wm name)
  "Open a window running the demo NAME (see *DEMOS*)."
  (let ((e (find-demo name)))
    (unless e (error "No demo named ~A; there are ~{~A~^, ~}" name (mapcar (lambda (d) (string-downcase (symbol-name (car d)))) *demos*)))
    (let* ((w (min 72 (- (wm-cols wm) 2))) (h (min 26 (wm-area-height wm)))
           (win (make-instance (caddr e) :title (cadr e) :w w :h h
                               :x (max 0 (floor (- (wm-cols wm) w) 2)) :y (max 0 (floor (- (wm-area-height wm) h) 2)))))
      (wm-add-window wm win)
      (wm-raise wm win)
      win)))

(defun demo-menu (wm)
  "Pop up the menu of demos; choosing one opens it."
  (let* ((width (reduce #'max (mapcar (lambda (e) (length (cadr e))) *demos*) :initial-value 0))
         (items (mapcar (lambda (e) (cons (format nil "~A  ~A" (%pad-to (cadr e) width) (cadddr e)) (car e))) *demos*))
         (mw (+ 4 (reduce #'max (mapcar (lambda (i) (length (car i))) items) :initial-value 0))))
    (wm-popup-menu wm items (max 0 (floor (- (wm-cols wm) mw) 2)) (max 0 (floor (- (wm-area-height wm) (length items) 2) 2))
                   (lambda (name) (open-demo wm name))
                   :title "Demos")))

;;; ---- colour helpers -------------------------------------------------------------------------------------------

(defun %hsv (h s v)
  "A colour (#xRRGGBB, without RGB's flag) from hue H (0..1, wrapping), saturation S and value V (0..1)."
  (let* ((h6 (* 6 (- h (floor h)))) (i (floor h6)) (f (- h6 i))
         (p (* v (- 1 s))) (q (* v (- 1 (* s f)))) (u (* v (- 1 (* s (- 1 f))))))
    (multiple-value-bind (r g b)
        (case (mod i 6) (0 (values v u p)) (1 (values q v p)) (2 (values p v u)) (3 (values p q v)) (4 (values u p v)) (t (values v p q)))
      (+ (* 65536 (round (* 255 r))) (* 256 (round (* 255 g))) (round (* 255 b))))))

(defun %hsv-rgb (h s v) (+ #x1000000 (%hsv h s v)))

;;; ---- Colour Circles: shaded discs and colour-map cycling -------------------------------------------------------

(defconstant +circle-bands+ 7)
(defconstant +circle-shades+ 36)
(defconstant +circle-background+ 255)

(defclass circles-demo (demo-window)
  ((indices :initform nil :accessor circles-indices)
   (palette :initform (make-array 256 :initial-element 0) :accessor circles-palette)
   (circles :initform nil :accessor circles-list)         ; (cx cy r band), back to front
   (style :initform 0 :accessor circles-style)            ; 0 shaded spheres, 1 rings
   (phase :initform 0 :accessor circles-phase)))

(defmethod demo-keys-help ((w circles-demo)) (if (= 0 (circles-style w)) "S rings" "S spheres"))

(defun %circles-layout (w)
  (let* ((pw (demo-width w)) (ph (demo-height w)) (m (min pw ph)) (n (+ 10 (random 8))) (l nil))
    (dotimes (i n)
      (let ((r (+ (* m 0.07) (random (max 1.0 (* m 0.2))))))
        (setq l (cons (list (random pw) (random ph) r (mod i +circle-bands+)) l))))
    (sort l #'> :key #'caddr)))                         ; big ones at the back

(defun %circles-draw-indices (w)
  (let ((ix (circles-indices w)) (pw (demo-width w)) (ph (demo-height w)))
    (%idx-fill ix pw ph +circle-background+)
    (dolist (c (circles-list w))
      (destructuring-bind (cx cy r band) c
        (%idx-disc ix pw ph cx cy r (* band +circle-shades+) +circle-shades+ (circles-style w))))))

(defmethod demo-setup ((w circles-demo))
  (setf (circles-indices w) (make-string (* (demo-width w) (demo-height w)))
        (circles-list w) (%circles-layout w))
  (%circles-draw-indices w))

(defmethod demo-key ((w circles-demo) ch)
  (when (char= ch #\s)
    (setf (circles-style w) (- 1 (circles-style w)))
    (%circles-draw-indices w)
    (%circles-palette w)
    t))

(defun %circles-palette (w)
  "Fill the palette for the current phase, and map the indices through it."
  (let ((pal (circles-palette w)) (phase (circles-phase w)) (n +circle-shades+))
    (dotimes (b +circle-bands+)
      (let ((hue (+ (/ b +circle-bands+) (* phase 0.003))))
        (dotimes (k n)
          (let ((tt (/ k (- n 1.0))))
            (setf (aref pal (+ (* b n) k))
                  (if (= 0 (circles-style w))
                      ;; dark to full colour, then towards white for the highlight
                      (if (< tt 0.8)
                          (%hsv hue 0.9 (+ 0.12 (* 0.88 (/ tt 0.8))))
                          (%hsv hue (* 0.9 (- 1 (/ (- tt 0.8) 0.2))) 1.0))
                      ;; three bright rings across the radius, moving outwards as the phase grows
                      (let ((wave (+ 0.5 (* 0.5 (cos (* 2 pi (- (* 3 tt) (* phase 0.04))))))))
                        (%hsv (+ hue (* 0.15 tt)) 0.85 (+ 0.2 (* 0.8 wave))))))))))
    (loop for i from (* +circle-bands+ n) below 256 do (setf (aref pal i) #x080A18))
    (let ((img (demo-image w)))
      (%idx-map (circles-indices w) (image-pixels img) (image-width img) (image-height img) pal))))

(defmethod demo-step ((w circles-demo))
  (setf (circles-phase w) (+ 1 (circles-phase w)))
  (%circles-palette w))

(define-demo :circles "Colour Circles" 'circles-demo "shaded circles, colour-map cycling")

;;; ---- Munching Squares ---------------------------------------------------------------------------------------

(defclass munch-demo (demo-window)
  ((n :initform 64 :accessor munch-n) (size :initform 1 :accessor munch-size)
   (x0 :initform 0 :accessor munch-x0) (y0 :initform 0 :accessor munch-y0)
   (bits :initform nil :accessor munch-bits) (tm :initform 0 :accessor munch-t)))

(defmethod demo-base-interval ((w munch-demo)) 30)

(defmethod demo-setup ((w munch-demo))
  (let* ((m (min (demo-width w) (demo-height w))) (n 128))
    (loop while (and (> n 16) (< (floor m n) 1)) do (setq n (floor n 2)))
    (setf (munch-n w) n (munch-size w) (max 1 (floor m n))
          (munch-x0 w) (floor (- (demo-width w) (* n (max 1 (floor m n)))) 2)
          (munch-y0 w) (floor (- (demo-height w) (* n (max 1 (floor m n)))) 2)
          (munch-bits w) (make-array (* n n) :initial-element 0) (munch-t w) 0)))

(defmethod demo-step ((w munch-demo))
  ;; for each X, flip the square at Y = X xor T; as T counts up the pattern munches across the screen
  (let* ((n (munch-n w)) (s (munch-size w)) (tm (munch-t w)) (bits (munch-bits w)) (img (demo-image w))
         (on (%hsv-rgb (/ tm (* 4.0 n)) 0.8 1.0)) (off (rgb 0 0 0)))
    (dotimes (x n)
      (let* ((y (mod (logxor x tm) n)) (i (+ (* y n) x)) (b (- 1 (aref bits i))))
        (setf (aref bits i) b)
        (image-fill-rect img (+ (munch-x0 w) (* x s)) (+ (munch-y0 w) (* y s)) s s (if (= b 1) on off))))
    (setf (munch-t w) (+ tm 1))))

(define-demo :munching "Munching Squares" 'munch-demo "the PDP-1 hack: x xor t")

;;; ---- Lissajous figures -----------------------------------------------------------------------------------------

(defparameter *lissajous-ratios* '((3 2) (3 4) (5 4) (1 2) (5 6) (2 3) (4 5)))

(defclass lissajous-demo (demo-window) ((phase :initform 0.0 :accessor liss-phase)))

(defmethod demo-step ((w lissajous-demo))
  (let* ((img (demo-image w)) (pw (demo-width w)) (ph (demo-height w)) (frames (demo-frames w))
         (ratio (nth (mod (floor frames 400) (length *lissajous-ratios*)) *lissajous-ratios*))
         (a (car ratio)) (b (cadr ratio)) (ph0 (liss-phase w)) (steps 360)
         (rx (* 0.45 pw)) (ry (* 0.45 ph)) (thick (max 1 (floor (min pw ph) 90)))
         (prev nil))
    (image-fill-rect img 0 0 pw ph (rgb 0 0 0))
    (dotimes (i (+ steps 1))
      (let* ((u (/ (* 2 pi i) steps))
             (pt (cons (+ (/ pw 2.0) (* rx (sin (+ (* a u) ph0)))) (+ (/ ph 2.0) (* ry (sin (* b u)))))))
        (when prev
          (image-line img (car prev) (cdr prev) (car pt) (cdr pt) (%hsv-rgb (+ (/ i steps 1.0) (* frames 0.002)) 0.75 1.0) thick))
        (setq prev pt)))
    (setf (liss-phase w) (+ ph0 0.02))))

(define-demo :lissajous "Lissajous" 'lissajous-demo "turning Lissajous figures")

;;; ---- Bouncing lines --------------------------------------------------------------------------------------------

(defclass bounce-demo (demo-window)
  ((ends :initform nil :accessor bounce-ends)          ; ((x y dx dy) (x y dx dy))
   (trail :initform nil :accessor bounce-trail)))      ; newest first: (x0 y0 x1 y1)

(defparameter *bounce-trail-length* 28)

(defmethod demo-setup ((w bounce-demo))
  (let ((pw (demo-width w)) (ph (demo-height w)) (v (max 1.0 (/ (min (demo-width w) (demo-height w)) 60.0))))
    (flet ((end () (list (random pw) (random ph) (* v (+ 0.6 (random 1.0)) (if (= 0 (random 2)) 1 -1))
                         (* v (+ 0.6 (random 1.0)) (if (= 0 (random 2)) 1 -1)))))
      (setf (bounce-ends w) (list (end) (end)) (bounce-trail w) nil))))

(defun %bounce-move (e pw ph)
  (destructuring-bind (x y dx dy) e
    (let ((nx (+ x dx)) (ny (+ y dy)))
      (when (or (< nx 0) (>= nx pw)) (setq dx (- dx) nx (max 0 (min (- pw 1) nx))))
      (when (or (< ny 0) (>= ny ph)) (setq dy (- dy) ny (max 0 (min (- ph 1) ny))))
      (list nx ny dx dy))))

(defmethod demo-step ((w bounce-demo))
  (let* ((img (demo-image w)) (pw (demo-width w)) (ph (demo-height w))
         (ends (mapcar (lambda (e) (%bounce-move e pw ph)) (bounce-ends w)))
         (line (list (car (car ends)) (cadr (car ends)) (car (cadr ends)) (cadr (cadr ends))))
         (trail (cons line (bounce-trail w))) (n 0) (len *bounce-trail-length*) (thick (max 1 (floor (min pw ph) 120))))
    (when (> (length trail) len) (setq trail (subseq trail 0 len)))
    (setf (bounce-ends w) ends (bounce-trail w) trail)
    (image-fill-rect img 0 0 pw ph (rgb 0 0 0))
    (dolist (l (reverse trail))                         ; oldest and dimmest first
      (setq n (+ n 1))
      (image-line img (nth 0 l) (nth 1 l) (nth 2 l) (nth 3 l)
                  (%hsv-rgb (+ (* (demo-frames w) 0.004) (* n 0.012)) 0.7 (/ n (length trail) 1.0)) thick))))

(define-demo :bounce "Bouncing Lines" 'bounce-demo "a line bouncing off the walls, with its trail")

;;; ---- Life -----------------------------------------------------------------------------------------------------

(defclass life-demo (demo-window)
  ((gw :initform 0 :accessor life-gw) (gh :initform 0 :accessor life-gh) (cell :initform 1 :accessor life-cell)
   (grid :initform nil :accessor life-grid) (next :initform nil :accessor life-next)
   (age :initform nil :accessor life-age) (still :initform 0 :accessor life-still)))

(defmethod demo-base-interval ((w life-demo)) 80)
(defmethod demo-keys-help ((w life-demo)) "R random")

(defun %life-seed (w)
  (let ((g (life-grid w)) (age (life-age w)))
    (dotimes (i (length g)) (setf (aref g i) (if (< (random 100) 30) 1 0) (aref age i) 0))
    (setf (life-still w) 0)
    (image-fill-rect (demo-image w) 0 0 (demo-width w) (demo-height w) (rgb 0 0 0))))

(defmethod demo-setup ((w life-demo))
  (let* ((pw (demo-width w)) (ph (demo-height w)) (cell (max 2 (ceiling (max pw ph) 110))))
    (setf (life-cell w) cell (life-gw w) (max 3 (floor pw cell)) (life-gh w) (max 3 (floor ph cell)))
    (let ((n (* (life-gw w) (life-gh w))))
      (setf (life-grid w) (make-array n :initial-element 0) (life-next w) (make-array n :initial-element 0)
            (life-age w) (make-array n :initial-element 0)))
    (%life-seed w)))

(defmethod demo-key ((w life-demo) ch)
  (when (char= ch #\r) (%life-seed w) t))

(defmethod demo-step ((w life-demo))
  (let* ((gw (life-gw w)) (gh (life-gh w)) (g (life-grid w)) (nx (life-next w)) (age (life-age w))
         (cell (life-cell w)) (img (demo-image w)) (changes 0) (black (rgb 0 0 0)))
    (dotimes (y gh)
      (let ((up (* (mod (- y 1) gh) gw)) (mid (* y gw)) (down (* (mod (+ y 1) gh) gw)))
        (dotimes (x gw)
          (let* ((l (mod (- x 1) gw)) (r (mod (+ x 1) gw))
                 (count (+ (aref g (+ up l)) (aref g (+ up x)) (aref g (+ up r))
                           (aref g (+ mid l)) (aref g (+ mid r))
                           (aref g (+ down l)) (aref g (+ down x)) (aref g (+ down r))))
                 (i (+ mid x)) (alive (aref g i))
                 (new (if (or (= count 3) (and (= count 2) (= alive 1))) 1 0)))
            (setf (aref nx i) new)
            (cond ((/= new alive)
                   (setq changes (+ changes 1))
                   (setf (aref age i) 0)
                   (image-fill-rect img (* x cell) (* y cell) (- cell 1) (- cell 1)
                                    (if (= new 1) (%hsv-rgb 0.33 0.6 1.0) black)))
                  ((and (= new 1) (< (aref age i) 40))
                   (setf (aref age i) (+ 1 (aref age i)))
                   (when (= 0 (mod (aref age i) 8))       ; long-lived cells shift from green towards blue
                     (image-fill-rect img (* x cell) (* y cell) (- cell 1) (- cell 1)
                                      (%hsv-rgb (+ 0.33 (* 0.008 (aref age i))) 0.6 (- 1.0 (* 0.01 (aref age i))))))))))))
    (setf (life-grid w) nx (life-next w) g)
    ;; a world that has settled down (only still lifes and blinkers left) starts again
    (setf (life-still w) (if (< changes (max 4 (floor (* gw gh) 400))) (+ 1 (life-still w)) 0))
    (when (> (life-still w) 60) (%life-seed w))))

(define-demo :life "Life" 'life-demo "Conway's Game of Life")

;;; ---- the command ----------------------------------------------------------------------------------------------

(define-command (demo :documentation "Run a graphics demo; without a name, choose one from the menu")
    ((name word :default "menu"))
  (cond ((null *wm*)
         (format t "~&The demos run in the window system (bin/neogenera --ui).  They are:~%")
         (dolist (e *demos*) (format t "  ~12A ~A~%" (string-downcase (symbol-name (car e))) (cadddr e))))
        ((string-equal name "menu") (demo-menu *wm*))
        ((find-demo name) (open-demo *wm* name))
        (t (format t "~&No demo named ~A.  :demo alone shows the menu of~{ ~A~^,~}.~%" name
                   (mapcar (lambda (e) (string-downcase (symbol-name (car e)))) *demos*)))))
