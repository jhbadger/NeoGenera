;;;; 05-graphics.lisp -- images, and showing them in the terminal.
;;;;
;;;; An IMAGE is a grid of RGB pixels with drawing operations (implemented in C: src/prims_gfx.c).  The window
;;;; system shows images in two ways:
;;;;
;;;;  * In kitty and Ghostty (detected when the window system starts, see DETECT-GRAPHICS), with the kitty
;;;;    graphics protocol's Unicode placeholders: the image is sent to the terminal once, and each screen cell
;;;;    it covers holds the placeholder character U+10EEEE, with the image number as its colour and its row
;;;;    and column as combining marks.  The cells go through the ordinary screen diff, so images scroll, clip
;;;;    and hide behind other windows like text.
;;;;  * Elsewhere, with half-block characters: each cell shows the average colours of the top and bottom
;;;;    halves of its part of the image (two "pixels" per cell, in 24-bit or 256 colours).
;;;;
;;;; In the screen buffer an image cell has a code above Unicode (IMAGE-CELL-CODE); the flush turns it into
;;;; placeholder characters.  In an output buffer, a row of an image is a segment of private-use characters
;;;; U+F0000 + column with the style (:IMAGE key row), so line wrapping and scrolling keep working.

(in-package :cl)

;;; ---- images -----------------------------------------------------------------------------------------------

(defstruct (image (:constructor %make-image) (:copier nil))
  width height pixels (keys nil) (cell-colors nil))

(defmethod print-object ((i image) stream)
  (format stream "#<IMAGE ~Dx~D>" (image-width i) (image-height i)))

(defun %color-bits (c) (if (>= c #x1000000) (- c #x1000000) c))

(defun make-image (width height &optional (background (rgb 255 255 255)))
  "A WIDTH x HEIGHT image filled with BACKGROUND (a colour made by RGB)."
  (let ((img (%make-image :width width :height height :pixels (make-string (* width height 3)))))
    (image-fill-rect img 0 0 width height background)
    img))

(defun image-fill-rect (image x y width height color)
  "Fill the rectangle with top left X, Y with COLOR."
  (%img-fill-rect (image-pixels image) (image-width image) (image-height image) x y width height (%color-bits color))
  (setf (image-cell-colors image) nil)
  image)

(defun image-line (image x0 y0 x1 y1 color &optional (thickness 1))
  "Draw an anti-aliased line from X0, Y0 to X1, Y1, THICKNESS pixels wide, with round ends."
  (%img-line (image-pixels image) (image-width image) (image-height image) x0 y0 x1 y1 (%color-bits color) thickness)
  (setf (image-cell-colors image) nil)
  image)

(defun image-polyline (image points color &optional (thickness 1))
  "Draw lines joining POINTS, a list of (x . y)."
  (do ((l points (cdr l))) ((null (cdr l)))
    (image-line image (car (car l)) (cdr (car l)) (car (cadr l)) (cdr (cadr l)) color thickness))
  image)

(defun image-disc (image cx cy radius color)
  "Draw an anti-aliased filled circle."
  (%img-disc (image-pixels image) (image-width image) (image-height image) cx cy radius (%color-bits color))
  (setf (image-cell-colors image) nil)
  image)

(defun image-pixel (image x y)
  "The colour of the pixel at X, Y (as RGB makes them)."
  (+ #x1000000 (%img-pixel (image-pixels image) (image-width image) (image-height image) x y)))

(defun %image-cell-colors (image cols rows)
  (let ((hit (assoc (cons cols rows) (image-cell-colors image) :test #'equal)))
    (if hit (cdr hit)
        (let ((v (%img-cell-colors (image-pixels image) (image-width image) (image-height image) cols rows)))
          (setf (image-cell-colors image) (cons (cons (cons cols rows) v) (image-cell-colors image)))
          v))))

;;; ---- does the terminal show images? ---------------------------------------------------------------------------

(defvar *graphics* nil "The way the window system shows images: :KITTY (the terminal draws them) or NIL (half blocks).")
(defvar *graphics-preference* :auto
  "Whether to use the terminal's image support: :AUTO (in kitty and Ghostty), :ON (whenever the terminal accepts the
kitty graphics protocol) or :OFF.  The environment variable NG_GRAPHICS (on/off) overrides it.")
(defvar *terminal-name* "" "What the terminal reported it is (XTVERSION), when it answered.")
(defvar *cell-pixel-width* 10 "Width of a terminal cell in pixels (from the terminal when it says).")
(defvar *cell-pixel-height* 20 "Height of a terminal cell in pixels.")

(defun %ci-search (needle haystack) (and haystack (%string-search (string-downcase needle) (string-downcase haystack) 0)))

(defun %graphics-pref ()
  (let ((e (getenv "NG_GRAPHICS")))
    (cond ((null e) *graphics-preference*)
          ((member (string-downcase e) '("0" "off" "no" "false") :test #'string=) :off)
          ((member (string-downcase e) '("1" "on" "yes" "true") :test #'string=) :on)
          (t *graphics-preference*))))

(defun %kitty-like-p (name)
  "Kitty and Ghostty draw images placed with Unicode placeholders."
  (or (%ci-search "kitty" name) (%ci-search "ghostty" name)
      (%ci-search "kitty" (getenv "TERM")) (%ci-search "ghostty" (getenv "TERM"))
      (%ci-search "ghostty" (getenv "TERM_PROGRAM")) (getenv "KITTY_WINDOW_ID") (getenv "GHOSTTY_RESOURCES_DIR")))

(defun detect-graphics ()
  "Ask the terminal (the window system must have set raw mode) whether it shows kitty-protocol images, and how
big its cells are.  Sets *GRAPHICS* and returns it."
  (let ((pref (%graphics-pref)))
    (setq *graphics* nil)
    ;; probe only where the environment points at kitty or Ghostty (or when asked to): other terminals see no
    ;; unfamiliar escape sequences and the window system starts without waiting for a reply.  tmux does not pass
    ;; the protocol through by default.
    (when (and (not (eq pref :off)) (not (getenv "TMUX")) (or (eq pref :on) (%kitty-like-p "")))
      (let ((r (ignore-errors (%term-graphics-probe 500))))
        (when r
          (destructuring-bind (ok name cw ch) r
            (setq *terminal-name* name)
            (when (and (> cw 0) (> ch 0)) (setq *cell-pixel-width* cw *cell-pixel-height* ch))
            (when (and ok (or (eq pref :on) (%kitty-like-p name))) (setq *graphics* :kitty))))))
    *graphics*))

(defun graphics-status ()
  "A one-line description of how images are shown."
  (format nil "~:[half blocks~;kitty graphics protocol~] (~@[~A, ~]cells ~Dx~D px)"
          (eq *graphics* :kitty) (and (> (length *terminal-name*) 0) *terminal-name*) *cell-pixel-width* *cell-pixel-height*))

;;; ---- images placed in cells ------------------------------------------------------------------------------------

;;; A placement is an image shown on COLS x ROWS cells; it gets a KEY (a number) the first time it is drawn.
(defstruct (placement (:constructor %make-placement (image cols rows key)) (:copier nil)) image cols rows key (term-id nil) (used 0))

(defvar *placements* (make-hash-table) "KEY -> placement.")
(defvar *placement-counter* 0)
(defparameter *max-placements* 2000)
(defvar *term-image-ids* (make-array 256 :initial-element nil) "Terminal image number -> placement shown with it.")
(defvar *image-clock* 0)

(defun image-placement-key (image cols rows)
  "The key of IMAGE shown on COLS x ROWS cells (made on first use)."
  (when (or (> cols (length *image-diacritics*)) (> rows (length *image-diacritics*)))
    (error "An image can cover at most ~D x ~D cells" (length *image-diacritics*) (length *image-diacritics*)))
  (let ((hit (assoc (cons cols rows) (image-keys image) :test #'equal)))
    (if hit (cdr hit)
        (let ((key (setq *placement-counter* (+ 1 *placement-counter*))))
          (setf (gethash key *placements*) (%make-placement image cols rows key))
          (setf (image-keys image) (cons (cons (cons cols rows) key) (image-keys image)))
          (when (> (hash-table-count *placements*) *max-placements*)      ; forget the oldest placements
            (let ((cut (- key *max-placements*)) (old nil))
              (maphash (lambda (k p) (when (< k cut) (setq old (cons p old)))) *placements*)
              (dolist (p old) (%forget-placement p))))
          key))))

(defun %forget-placement (p)
  (when (placement-term-id p)
    (setf (aref *term-image-ids* (placement-term-id p)) nil)
    (%term-image-delete (placement-term-id p)))
  (setf (image-keys (placement-image p)) (remove (placement-key p) (image-keys (placement-image p)) :key #'cdr))
  (remhash (placement-key p) *placements*))

(defun image-cell-code (key row col) (+ #x200000 (* (+ (* key 1024) row) 1024) col))
(defun image-cell-p (code) (>= code #x200000))
(defun %image-cell-parts (code)
  (let* ((v (- code #x200000)) (col (mod v 1024)) (row (mod (floor v 1024) 1024)) (key (floor v 1048576)))
    (values key row col)))

(defun %terminal-id (key visible)
  "The terminal image number showing placement KEY, sending the image first if needed.  When all 255 numbers
are taken, the least recently used one that is not on the screen (VISIBLE, a hash table of keys) is reused."
  (let ((p (gethash key *placements*)))
    (when p
      (setf (placement-used p) (setq *image-clock* (+ 1 *image-clock*)))
      (or (placement-term-id p)
          (let ((id (loop for i from 1 to 255 unless (aref *term-image-ids* i) return i)))
            (unless id
              (let ((best nil))
                (loop for i from 1 to 255
                      do (let ((q (aref *term-image-ids* i)))
                           (when (and q (not (gethash (placement-key q) visible))
                                      (or (null best) (< (placement-used q) (placement-used (aref *term-image-ids* best)))))
                             (setq best i))))
                (when best
                  (setf (placement-term-id (aref *term-image-ids* best)) nil)
                  (%term-image-delete best)
                  (setq id best))))
            (when id
              (let ((img (placement-image p)))
                (%term-image-send id (image-pixels img) (image-width img) (image-height img) (placement-cols p) (placement-rows p)))
              (setf (aref *term-image-ids* id) p (placement-term-id p) id))
            id)))))

(defun graphics-reset ()
  "Forget which images the terminal has (when the window system starts or ends)."
  (dotimes (i 256) (let ((p (aref *term-image-ids* i))) (when p (setf (placement-term-id p) nil))))
  (fill *term-image-ids* nil))

;;; The combining marks that number rows and columns of placeholder cells, in the order the kitty graphics
;;; protocol defines (its rowcolumn-diacritics.txt: Unicode 6.0 combining class 230).
(defparameter *image-diacritics*
  (coerce '(#x0305 #x030D #x030E #x0310 #x0312 #x033D #x033E #x033F #x0346 #x034A #x034B #x034C #x0350 #x0351 #x0352 #x0357
            #x035B #x0363 #x0364 #x0365 #x0366 #x0367 #x0368 #x0369 #x036A #x036B #x036C #x036D #x036E #x036F #x0483 #x0484
            #x0485 #x0486 #x0487 #x0592 #x0593 #x0594 #x0595 #x0597 #x0598 #x0599 #x059C #x059D #x059E #x059F #x05A0 #x05A1
            #x05A8 #x05A9 #x05AB #x05AC #x05AF #x05C4 #x0610 #x0611 #x0612 #x0613 #x0614 #x0615 #x0616 #x0617 #x0657 #x0658
            #x0659 #x065A #x065B #x065D #x065E #x06D6 #x06D7 #x06D8 #x06D9 #x06DA #x06DB #x06DC #x06DF #x06E0 #x06E1 #x06E2
            #x06E4 #x06E7 #x06E8 #x06EB #x06EC #x0730 #x0732 #x0733 #x0735 #x0736 #x073A #x073D #x073F #x0740 #x0741 #x0743
            #x0745 #x0747 #x0749 #x074A #x07EB #x07EC #x07ED #x07EE #x07EF #x07F0 #x07F1 #x07F3 #x0816 #x0817 #x0818 #x0819
            #x081B #x081C #x081D #x081E #x081F #x0820 #x0821 #x0822 #x0823 #x0825 #x0826 #x0827 #x0829 #x082A #x082B #x082C
            #x082D #x0951 #x0953 #x0954 #x0F82 #x0F83 #x0F86 #x0F87 #x135D #x135E #x135F #x17DD #x193A #x1A17 #x1A75 #x1A76
            #x1A77 #x1A78 #x1A79 #x1A7A #x1A7B #x1A7C #x1B6B #x1B6D #x1B6E #x1B6F #x1B70 #x1B71 #x1B72 #x1B73 #x1CD0 #x1CD1
            #x1CD2 #x1CDA #x1CDB #x1CE0 #x1DC0 #x1DC1 #x1DC3 #x1DC4 #x1DC5 #x1DC6 #x1DC7 #x1DC8 #x1DC9 #x1DCB #x1DCC #x1DD1
            #x1DD2 #x1DD3 #x1DD4 #x1DD5 #x1DD6 #x1DD7 #x1DD8 #x1DD9 #x1DDA #x1DDB #x1DDC #x1DDD #x1DDE #x1DDF #x1DE0 #x1DE1
            #x1DE2 #x1DE3 #x1DE4 #x1DE5 #x1DE6 #x1DFE #x20D0 #x20D1 #x20D4 #x20D5 #x20D6 #x20D7 #x20DB #x20DC #x20E1 #x20E7
            #x20E9 #x20F0 #x2CEF #x2CF0 #x2CF1 #x2DE0 #x2DE1 #x2DE2 #x2DE3 #x2DE4 #x2DE5 #x2DE6 #x2DE7 #x2DE8 #x2DE9 #x2DEA
            #x2DEB #x2DEC #x2DED #x2DEE #x2DEF #x2DF0 #x2DF1 #x2DF2 #x2DF3 #x2DF4 #x2DF5 #x2DF6 #x2DF7 #x2DF8 #x2DF9 #x2DFA
            #x2DFB #x2DFC #x2DFD #x2DFE #x2DFF #xA66F #xA67C #xA67D #xA6F0 #xA6F1 #xA8E0 #xA8E1 #xA8E2 #xA8E3 #xA8E4 #xA8E5
            #xA8E6 #xA8E7 #xA8E8 #xA8E9 #xA8EA #xA8EB #xA8EC #xA8ED #xA8EE #xA8EF #xA8F0 #xA8F1 #xAAB0 #xAAB2 #xAAB3 #xAAB7
            #xAAB8 #xAABE #xAABF #xAAC1 #xFE20 #xFE21 #xFE22 #xFE23 #xFE24 #xFE25 #xFE26 #x10A0F #x10A38 #x1D185 #x1D186
            #x1D187 #x1D188 #x1D189 #x1D1AA #x1D1AB #x1D1AC #x1D1AD #x1D242 #x1D243 #x1D244)
          'vector))

(defun %emit-image-cell (out code bg mode visible)
  "Emit the placeholder character for an image cell (the flush calls this); returns the SGR state it leaves."
  (multiple-value-bind (key row col) (%image-cell-parts code)
    (let ((id (%terminal-id key visible)))
      (%emit-csi out) (%emit-str out "0;38;5;") (%emit-num out (or id 0))
      (%emit-color out 48 bg mode)
      (vector-push-extend 109 out)
      (vector-push-extend #x10EEEE out)
      (vector-push-extend (aref *image-diacritics* row) out)
      (vector-push-extend (aref *image-diacritics* col) out)
      (- -1000 (or id 0)))))

;;; ---- drawing image segments of output buffers ---------------------------------------------------------------

(defun draw-image-chunk (screen r c text key row bg)
  "Draw the image cells of one chunk of an output-buffer row (TEXT holds the column numbers) at R, C."
  (let ((p (gethash key *placements*)) (i 0) (col c) (n (length text)))
    (loop
      (when (>= i n) (return))
      (multiple-value-bind (code next) (%utf8-decode text i)
        (let ((icol (- code #xF0000)))
          (cond ((null p) (screen-put screen r col 32 -1 bg 0))
                ((eq *graphics* :kitty) (screen-put screen r col (image-cell-code key row icol) -1 bg 0))
                (t (let* ((v (%image-cell-colors (placement-image p) (placement-cols p) (placement-rows p)))
                          (k (* 2 (+ (* row (placement-cols p)) icol))))
                     (screen-put screen r col 9600 (+ #x1000000 (aref v k)) (+ #x1000000 (aref v (+ k 1))) 0)))))
        (setq i next col (+ col 1))))))

(defun image-cells-for (image &optional cols rows)
  "Cell size to show IMAGE at: the given COLS and/or ROWS, else its pixel size in cells (at most 120 x 50)."
  (let* ((c (or cols (if rows (max 1 (round (* rows *cell-pixel-height* (image-width image)) (* *cell-pixel-width* (image-height image))))
                         (min 120 (max 1 (ceiling (image-width image) *cell-pixel-width*))))))
         (r (or rows (max 1 (min 50 (round (* c *cell-pixel-width* (image-height image)) (* *cell-pixel-height* (image-width image))))))))
    (values (min c (length *image-diacritics*)) (min r (length *image-diacritics*)))))

(defun %image-row-text (cols)
  (apply #'%string-concat (loop for i from 0 below cols collect (%utf8-encode (+ #xF0000 i)))))
