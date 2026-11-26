;;;; 10-wm.lisp -- the text window manager.
;;;;
;;;; Windows are CLOS objects (generic functions WINDOW-DRAW-CONTENT, WINDOW-KEY,
;;;; WINDOW-MOUSE, ...).  The manager supports overlapping and tiled layouts, a
;;;; focus ring, title bars with a close box, drag-to-move, a bottom-right resize
;;;; grip, scrollbars, popup menus, a mouse-documentation line and a Genera-style
;;;; who-line at the bottom of the screen.  Everything draws into a SCREEN; the
;;;; same code runs on a real terminal (WM-RUN) and headless (WM-RUN-EVENTS), which
;;;; is how it is tested.

(in-package :cl)

(defconstant +h+ #x2500) (defconstant +v+ #x2502) (defconstant +tl+ #x250C) (defconstant +tr+ #x2510)
(defconstant +bl+ #x2514) (defconstant +br+ #x2518)
(defconstant +dh+ #x2550) (defconstant +dv+ #x2551) (defconstant +dtl+ #x2554) (defconstant +dtr+ #x2557)
(defconstant +dbl+ #x255A) (defconstant +dbr+ #x255D)
(defconstant +arrow-up+ #x25B2) (defconstant +arrow-down+ #x25BC) (defconstant +block+ #x2588)
(defconstant +shade+ #x2591) (defconstant +grip+ #x25E2) (defconstant +times+ #xD7)

(defvar *wm* nil)

(defclass window ()
  ((title :initarg :title :initform "" :accessor window-title)
   (x :initarg :x :initform 0 :accessor window-x)
   (y :initarg :y :initform 0 :accessor window-y)
   (w :initarg :w :initform 40 :accessor window-w)
   (h :initarg :h :initform 12 :accessor window-h)
   (border :initarg :border :initform t :accessor window-border)
   (wm :initform nil :accessor window-wm)
   (scroll :initform 0 :accessor window-scroll)
   (content-height :initform 0 :accessor window-content-height)
   (min-w :initform 16 :accessor window-min-w)
   (min-h :initform 5 :accessor window-min-h)
   (cursor :initform nil :accessor window-cursor)))

(defgeneric window-draw-content (window screen ix iy iw ih focused))
(defgeneric window-key (window key))
(defgeneric window-mouse (window kind lx ly button mods))
(defgeneric window-hover-doc (window lx ly))
(defgeneric window-scroll-by (window n))
(defgeneric window-close (window))
(defgeneric window-tick (window))
(defgeneric window-resized (window))
(defgeneric window-package (window))

(defmethod window-draw-content ((w window) screen ix iy iw ih focused) (declare (ignore w screen ix iy iw ih focused)) nil)
(defmethod window-key ((w window) key) (declare (ignore w key)) nil)
(defmethod window-mouse ((w window) kind lx ly button mods) (declare (ignore w kind lx ly button mods)) nil)
(defmethod window-hover-doc ((w window) lx ly) (declare (ignore w lx ly)) nil)
(defmethod window-scroll-by ((w window) n) (declare (ignore w n)) nil)
(defmethod window-close ((w window)) t)
(defmethod window-tick ((w window)) nil)
(defmethod window-resized ((w window)) nil)
(defmethod window-package ((w window)) *package*)
(defgeneric window-quit-warning (window))
(defmethod window-quit-warning ((w window)) nil)      ; a string such as "2 modified buffers" makes F10 mention it

(defun window-inner (win)
  (if (window-border win)
      (values (+ (window-x win) 1) (+ (window-y win) 1) (max 0 (- (window-w win) 2)) (max 0 (- (window-h win) 2)))
      (values (window-x win) (window-y win) (window-w win) (window-h win))))

(defun window-contains-p (win x y)
  (and (>= x (window-x win)) (< x (+ (window-x win) (window-w win)))
       (>= y (window-y win)) (< y (+ (window-y win) (window-h win)))))

;;; ---- a static scrollable text window -------------------------------------------------------

(defclass text-window (window)
  ((lines :initarg :lines :initform nil :accessor text-window-lines)))

(defmethod window-draw-content ((w text-window) screen ix iy iw ih focused)
  (declare (ignore focused))
  (let* ((lines (text-window-lines w)) (n (length lines)))
    (setf (window-content-height w) n)
    (setf (window-scroll w) (max 0 (min (window-scroll w) (max 0 (- n ih)))))
    (dotimes (i ih)
      (let ((line (nth (+ i (window-scroll w)) lines)))
        (when line (screen-draw-string screen (+ iy i) ix line (tc :fg) (tc :bg) 0 0 (min (length line) iw)))))))

(defmethod window-scroll-by ((w text-window) n)
  (setf (window-scroll w) (max 0 (- (window-scroll w) n))))

(defmethod window-key ((w text-window) key)
  (let ((c (key-code key)))
    (cond ((eq c :down) (window-scroll-by w -1) t) ((eq c :up) (window-scroll-by w 1) t)
          ((eq c :pgdn) (window-scroll-by w (- (max 1 (- (window-h w) 3)))) t)
          ((eq c :pgup) (window-scroll-by w (max 1 (- (window-h w) 3))) t)
          ((and (eql c 113) (zerop (key-mods key))) (wm-close-window (window-wm w) w) t)
          (t nil))))

;;; ---- popup menus -------------------------------------------------------------------------------

(defclass menu-window (window)
  ((items :initarg :items :accessor menu-items)        ; list of (label . value)
   (sel :initform 0 :accessor menu-sel)
   (callback :initarg :callback :accessor menu-callback)))

(defun %menu-pick (m)
  (let ((item (nth (menu-sel m) (menu-items m))) (wm (window-wm m)))
    (wm-close-popups wm)
    (when item (funcall (menu-callback m) (cdr item)))))

(defmethod window-draw-content ((m menu-window) screen ix iy iw ih focused)
  (declare (ignore focused))
  (let ((i 0))
    (dolist (it (menu-items m))
      (when (< i ih)
        (let ((sel (= i (menu-sel m))))
          (screen-fill screen (+ iy i) ix 1 iw 32 (if sel (tc :sel-fg) (tc :menu-fg)) (if sel (tc :sel-bg) (tc :menu-bg)) (if (and sel (= -1 (tc :sel-bg))) +reverse+ 0))
          (screen-draw-string screen (+ iy i) (+ ix 1) (car it) (if sel (tc :sel-fg) (tc :menu-fg)) (if sel (tc :sel-bg) (tc :menu-bg))
                              (if (and sel (= -1 (tc :sel-bg))) +reverse+ 0))))
      (setq i (+ i 1)))))

(defmethod window-key ((m menu-window) key)
  (let ((c (key-code key)) (n (length (menu-items m))))
    (cond ((eq c :up) (setf (menu-sel m) (mod (- (menu-sel m) 1) (max 1 n))))
          ((eq c :down) (setf (menu-sel m) (mod (+ (menu-sel m) 1) (max 1 n))))
          ((eq c :return) (%menu-pick m))
          ((eq c :escape) (wm-close-popups (window-wm m)))
          ((and (integerp c) (zerop (key-mods key)))
           (let ((i 0))
             (dolist (it (menu-items m))
               (when (and (> (length (car it)) 0) (char-equal (char (car it) 0) (code-char c)))
                 (setf (menu-sel m) i) (return))
               (setq i (+ i 1))))))
    t))

(defmethod window-mouse ((m menu-window) kind lx ly button mods)
  (declare (ignore button mods))
  (when (and (>= ly 0) (< ly (length (menu-items m))))
    (setf (menu-sel m) ly)
    (when (eq kind :press) (%menu-pick m)))
  t)

;;; ---- the window manager --------------------------------------------------------------------------

(defclass wm ()
  ((screen :initarg :screen :accessor wm-screen)
   (windows :initform nil :accessor wm-windows)
   (focus :initform nil :accessor wm-focus)
   (popups :initform nil :accessor wm-popups)
   (quit :initform nil :accessor wm-quit)
   (dirty :initform t :accessor wm-dirty)
   (drag :initform nil :accessor wm-drag)
   (doc :initform nil :accessor wm-doc)
   (ticks :initform 0 :accessor wm-ticks)
   (busy :initform nil :accessor wm-busy)
   (message :initform nil :accessor wm-message)
   (layout :initform :overlap :accessor wm-layout)
   (last-clock :initform "" :accessor wm-last-clock)
   (pending :initform nil :accessor wm-pending)))      ; :quit or :select while that prompt is up

(defun make-wm (rows cols) (setq *wm* (make-instance 'wm :screen (make-screen rows cols))))
(defun wm-rows (wm) (scr-rows (wm-screen wm)))
(defun wm-cols (wm) (scr-cols (wm-screen wm)))
(defun wm-area-height (wm) (max 1 (- (wm-rows wm) 2)))

(defun wm-fit (wm win)
  (let ((cols (wm-cols wm)) (h (wm-area-height wm)))
    (setf (window-w win) (max (min (window-w win) cols) (min (window-min-w win) cols)))
    (setf (window-h win) (max (min (window-h win) h) (min (window-min-h win) h)))
    (setf (window-x win) (max 0 (min (window-x win) (- cols (window-w win)))))
    (setf (window-y win) (max 0 (min (window-y win) (- h (window-h win)))))
    win))

(defun wm-add-window (wm win &key x y)
  (when x (setf (window-x win) x))
  (when y (setf (window-y win) y))
  (setf (window-wm win) wm)
  (setf (wm-windows wm) (cons win (wm-windows wm)))
  (setf (wm-focus wm) win)
  (wm-fit wm win)
  (setf (wm-dirty wm) t)
  win)

(defun wm-raise (wm win)
  (setf (wm-windows wm) (cons win (remove win (wm-windows wm))))
  (setf (wm-focus wm) win (wm-dirty wm) t))

(defun wm-close-window (wm win)
  (when (window-close win)
    (setf (wm-windows wm) (remove win (wm-windows wm)))
    (when (eq (wm-focus wm) win) (setf (wm-focus wm) (car (wm-windows wm))))
    (setf (wm-dirty wm) t)
    t))

(defun wm-cycle-focus (wm)
  (let ((ws (wm-windows wm)))
    (when (cdr ws)
      (wm-raise wm (car (last ws))))))

(defun wm-window-at (wm x y)
  (dolist (w (wm-windows wm) nil) (when (window-contains-p w x y) (return w))))

(defun wm-tile (wm &optional (mode :horizontal))
  (let* ((ws (reverse (wm-windows wm))) (n (length ws)) (cols (wm-cols wm)) (h (wm-area-height wm)))
    (setf (wm-layout wm) mode)
    (when (> n 0)
      (case mode
        (:vertical (let ((i 0)) (dolist (w ws) (wm-place wm w 0 (floor (* i h) n) cols (- (floor (* (+ i 1) h) n) (floor (* i h) n))) (setq i (+ i 1)))))
        (:main (if (= n 1)
                   (wm-place wm (car ws) 0 0 cols h)
                   (let ((left (floor (* cols 3) 5)) (i 0) (m (- n 1)))
                     (wm-place wm (car ws) 0 0 left h)
                     (dolist (w (cdr ws))
                       (wm-place wm w left (floor (* i h) m) (- cols left) (- (floor (* (+ i 1) h) m) (floor (* i h) m)))
                       (setq i (+ i 1))))))
        (:grid (let* ((nc (ceiling (sqrt n))) (nr (ceiling n nc)) (i 0))
                 (dolist (w ws)
                   (let ((r (floor i nc)) (c (mod i nc)))
                     (wm-place wm w (floor (* c cols) nc) (floor (* r h) nr)
                               (- (floor (* (+ c 1) cols) nc) (floor (* c cols) nc))
                               (- (floor (* (+ r 1) h) nr) (floor (* r h) nr))))
                   (setq i (+ i 1)))))
        (t (let ((i 0)) (dolist (w ws) (wm-place wm w (floor (* i cols) n) 0 (- (floor (* (+ i 1) cols) n) (floor (* i cols) n)) h) (setq i (+ i 1)))))))
    (setf (wm-dirty wm) t)))

(defun wm-place (wm win x y w h)
  (setf (window-x win) x (window-y win) y (window-w win) w (window-h win) h)
  (window-resized win)
  (declare (ignorable wm)))

(defun wm-resize (wm rows cols)
  (screen-resize (wm-screen wm) rows cols)
  (dolist (w (wm-windows wm)) (wm-fit wm w) (window-resized w))
  (setf (wm-dirty wm) t))

;;; ---- popups ------------------------------------------------------------------------------------------

(defun wm-popup-menu (wm items x y callback &key (title ""))
  "ITEMS: list of (label . value).  CALLBACK is called with the chosen value."
  (let* ((width (+ 4 (max (length title) (reduce #'max (mapcar (lambda (i) (length (car i))) items) :initial-value 4))))
         (height (+ 2 (length items)))
         (m (make-instance 'menu-window :items items :callback callback :title title :w width :h height
                           :x (max 0 (min x (- (wm-cols wm) width))) :y (max 0 (min y (- (wm-rows wm) height 1))))))
    (setf (window-wm m) wm)
    (setf (wm-popups wm) (cons m (wm-popups wm)))
    (setf (wm-dirty wm) t)
    m))

(defun wm-close-popups (wm) (setf (wm-popups wm) nil (wm-dirty wm) t))

;;; ---- drawing -----------------------------------------------------------------------------------------

(defun draw-window (wm win focused)
  (let* ((s (wm-screen wm)) (x (window-x win)) (y (window-y win)) (w (window-w win)) (h (window-h win))
         (menu (typep win 'menu-window)))
    (screen-fill s y x h w 32 (if menu (tc :menu-fg) (tc :fg)) (if menu (tc :menu-bg) (tc :bg)) 0)
    (setf (window-cursor win) nil)
    (multiple-value-bind (ix iy iw ih) (window-inner win)
      (with-clip (iy ix (+ iy ih) (+ ix iw))
        (window-draw-content win s ix iy iw ih focused)))
    (when (window-border win) (draw-window-frame wm win focused menu))))

(defun draw-window-frame (wm win focused menu)
  (let* ((s (wm-screen wm)) (x (window-x win)) (y (window-y win)) (w (window-w win)) (h (window-h win))
         (fg (cond (menu (tc :menu-fg)) (focused (tc :active-border)) (t (tc :border))))
         (bg (if menu (tc :menu-bg) (tc :bg)))
         (hc (if focused +dh+ +h+)) (vc (if focused +dv+ +v+))
         (x2 (+ x w -1)) (y2 (+ y h -1)))
    (dotimes (i (- w 2)) (screen-put s y (+ x 1 i) hc fg bg 0) (screen-put s y2 (+ x 1 i) hc fg bg 0))
    (dotimes (i (- h 2)) (screen-put s (+ y 1 i) x vc fg bg 0) (screen-put s (+ y 1 i) x2 vc fg bg 0))
    (screen-put s y x (if focused +dtl+ +tl+) fg bg 0) (screen-put s y x2 (if focused +dtr+ +tr+) fg bg 0)
    (screen-put s y2 x (if focused +dbl+ +bl+) fg bg 0)
    (screen-put s y2 x2 (if (and focused (not menu)) +grip+ (if focused +dbr+ +br+)) fg bg 0)
    ;; title bar
    (unless menu
      (let ((tfg (if focused (tc :title-fg) (tc :inactive-fg))) (tbg (if focused (tc :title-bg) (tc :inactive-bg)))
            (attr (if (and (= -1 (tc :title-bg)) focused) +reverse+ 0)))
        (screen-fill s y (+ x 1) 1 (- w 2) 32 tfg tbg attr)
        (let ((title (%string-concat " " (window-title win) " ")))
          (screen-draw-string s y (+ x 2) title tfg tbg (logior attr +bold+) 0 (min (length title) (max 0 (- w 8)))))
        (when (> w 8)
          (screen-put s y (- x2 3) 91 tfg tbg attr) (screen-put s y (- x2 2) +times+ tfg tbg attr) (screen-put s y (- x2 1) 93 tfg tbg attr))))
    (when (and menu (> (length (window-title win)) 0))
      (screen-draw-string s y (+ x 2) (window-title win) fg bg +bold+))
    ;; scrollbar
    (let ((ch (window-content-height win)) (ih (- h 2)))
      (when (and (not menu) (> ch ih) (> ih 2))
        (dotimes (i ih) (screen-put s (+ y 1 i) x2 +shade+ (tc :scroll-track) bg 0))
        (let* ((size (max 1 (floor (* ih ih) ch))) (pos (floor (* (- ih size) (window-scroll win)) (max 1 (- ch ih)))))
          (dotimes (i size) (screen-put s (+ y 1 (min (- ih 1) (+ pos i))) x2 +block+ (tc :scroll-thumb) bg 0)))))))

(defun %clock-string ()
  "The local time of day as HH:MM, for the who-line."
  (multiple-value-bind (sec min hour) (get-decoded-time)
    (declare (ignore sec))
    (format nil "~2,'0D:~2,'0D" hour min)))

(defun %pad-to (str n) (if (>= (length str) n) (subseq str 0 n) (%string-concat str (make-string (- n (length str)) #\Space))))

(defvar *wm-buttons*
  '(("F6 next window" . :f6) ("F7 tile" . :f7) ("F8 theme" . :f8) ("F9 select" . :f9) ("F10 quit" . :f10))
  "Clickable commands shown on the mouse-documentation line: (label . key it stands for).")

(defun wm-button-spans ()
  "List of (start-col end-col key label) for the buttons, laid out from column 1 with 3 spaces between."
  (let ((col 1) (spans nil))
    (dolist (b *wm-buttons*)
      (setq spans (cons (list col (+ col (length (car b))) (cdr b) (car b)) spans))
      (setq col (+ col (length (car b)) 3)))
    (nreverse spans)))

(defun wm-button-at (wm x y)
  "The key of the command button at screen cell X,Y, or NIL."
  (when (= y (- (wm-rows wm) 2))
    (dolist (sp (wm-button-spans) nil)
      (when (and (>= x (car sp)) (< x (cadr sp))) (return (caddr sp))))))

(defun draw-bottom-lines (wm)
  (let* ((s (wm-screen wm)) (rows (wm-rows wm)) (cols (wm-cols wm)) (r1 (- rows 2)) (r2 (- rows 1))
         (reverse-attr (if (= -1 (tc :wholine-bg)) +reverse+ 0)))
    ;; mouse documentation line
    (screen-fill s r1 0 1 cols 32 (tc :doc-fg) (tc :doc-bg) (if (= -1 (tc :doc-bg)) +dim+ 0))
    (let ((text (or (wm-message wm) (wm-doc wm))))
      (if text
          (screen-draw-string s r1 1 text (tc :doc-fg) (tc :doc-bg) (if (= -1 (tc :doc-bg)) +dim+ 0) 0 (min (length text) (max 0 (- cols 2))))
          ;; default: clickable command buttons, then a hint
          (let ((battr (if (= -1 (tc :sel-bg)) +reverse+ 0)) (end 1))
            (dolist (sp (wm-button-spans))
              (when (<= (cadr sp) cols)
                (screen-draw-string s r1 (car sp) (cadddr sp) (tc :sel-fg) (tc :sel-bg) (logior battr +bold+))
                (setq end (cadr sp))))
            (let ((hint "   mouse: click presentations"))
              (screen-draw-string s r1 end hint (tc :doc-fg) (tc :doc-bg) (if (= -1 (tc :doc-bg)) +dim+ 0)
                                  0 (min (length hint) (max 0 (- cols end 1))))))))
    ;; who-line
    (screen-fill s r2 0 1 cols 32 (tc :wholine-fg) (tc :wholine-bg) reverse-attr)
    (let* ((spin (if (wm-busy wm) (string (char "|/-\\" (mod (wm-ticks wm) 4))) " "))
           (fw (wm-focus wm))
           (nproc (%process-count))
           (left (format nil " ~A  ~A  Package: ~A~A  ~A" (%clock-string) (or (getenv "USER") "user")
                         (package-name (if fw (window-package fw) *package*))
                         (if (> nproc 1) (format nil "  Processes: ~D" nproc) "") spin))
           (right (if fw (%string-concat (window-title fw) " ") "")))
      (screen-draw-string s r2 0 left (tc :wholine-fg) (tc :wholine-bg) reverse-attr 0 (min (length left) cols))
      (when (< (+ (length left) (length right) 2) cols)
        (screen-draw-string s r2 (- cols (length right)) right (tc :wholine-fg) (tc :wholine-bg) (logior reverse-attr +bold+))))))

(defun wm-draw (wm)
  (let ((s (wm-screen wm)))
    (screen-clear s (tc :desktop-fg) (tc :desktop-bg))
    (dolist (w (reverse (wm-windows wm))) (draw-window wm w (and (eq w (wm-focus wm)) (null (wm-popups wm)))))
    (dolist (p (reverse (wm-popups wm))) (draw-window wm p t))
    (draw-bottom-lines wm)
    (let* ((fw (wm-focus wm)) (cur (and fw (null (wm-popups wm)) (window-cursor fw))))
      (if cur
          (progn
            (setf (scr-cursor-row s) (car cur) (scr-cursor-col s) (cdr cur) (scr-cursor-visible s) t)
            ;; themes that define :cursor get a software block cursor in that colour
            (when (>= (tc :cursor) 0)
              (let ((r (car cur)) (c (cdr cur)))
                (when (and (>= r 0) (< r (scr-rows s)) (>= c 0) (< c (scr-cols s)))
                  (let ((cell (screen-cell s r c)))
                    (screen-put s r c (car cell) (tc :cursor-fg) (tc :cursor) 0))))))
          (setf (scr-cursor-visible s) nil)))
    (setf (wm-dirty wm) nil)
    wm))

;;; ---- events --------------------------------------------------------------------------------------------

(defun wm-global-key (wm key)
  (let ((c (key-code key)) (m (key-mods key)))
    (cond ((or (eq c :f6) (and (eql c 111) (= m +meta+))) (wm-cycle-focus wm) t)
          ((or (eq c :f7) (and (eql c 116) (= m +meta+)))
           (wm-tile wm (case (wm-layout wm) (:overlap :horizontal) (:horizontal :vertical) (:vertical :main) (:main :grid) (t :horizontal))) t)
          ((eq c :f8) (next-theme) (screen-invalidate (wm-screen wm))
           (setf (wm-message wm) (format nil "Theme: ~A" (symbol-name (theme-name *theme*)))) t)
          ((eq c :f9) (wm-begin-select wm) t)
          ((eq c :f10) (wm-request-quit wm) t)
          (t nil))))

;;; ---- F10: quit, after a confirmation ---------------------------------------------------------------------

(defun wm-quit-warnings (wm)
  "What quitting now would lose: modified buffers, background processes, ..."
  (let ((acc nil) (n (- (%process-count) 1)))
    (dolist (w (wm-windows wm)) (let ((m (window-quit-warning w))) (when m (setq acc (cons m acc)))))
    (when (> n 0) (setq acc (cons (format nil "~D background process~:P" n) acc)))
    (nreverse acc)))

(defun wm-request-quit (wm)
  "F10: ask before leaving the window system; F10 again (or y) quits, anything else cancels."
  (let ((warnings (wm-quit-warnings wm)))
    (setf (wm-pending wm) :quit
          (wm-message wm) (format nil "Quit NeoGenera?~@[ (~{~A~^, ~})~]   F10 or y: quit   any other key: cancel" warnings)
          (wm-dirty wm) t)))

(defun %quit-confirm-key-p (key)
  (let ((c (key-code key)))
    (or (eq c :f10) (and (integerp c) (zerop (logand (key-mods key) (logior +ctrl+ +meta+))) (member c '(121 89))))))

;;; ---- F9: the Select key -------------------------------------------------------------------------------------
;;; F9 then a letter selects an activity, as Genera's SELECT key did: the most recent window of that kind, the
;;; next one if one is already selected, or a new one if there is none.  Activities are registered by the
;;; files that define their windows.

(defvar *select-activities* nil "Entries (char label predicate open-function), in the order they are listed.")

(defun define-select-activity (char label predicate open-function)
  "Register an activity for F9 CHAR.  PREDICATE recognizes its windows; OPEN-FUNCTION (of the WM) makes a new
one, or is NIL when the activity cannot be started from the key."
  (let ((entry (list (char-upcase char) label predicate open-function)))
    (setq *select-activities*
          (if (find (char-upcase char) *select-activities* :key #'car)
              (mapcar (lambda (e) (if (char= (car e) (char-upcase char)) entry e)) *select-activities*)
              (append *select-activities* (list entry))))
    char))

(defun %select-prompt ()
  (format nil "Select:~{  ~A~}   any other key: cancel"
          (mapcar (lambda (e) (format nil "~A ~A" (car e) (cadr e))) *select-activities*)))

(defun wm-begin-select (wm)
  (setf (wm-pending wm) :select (wm-message wm) (%select-prompt) (wm-dirty wm) t))

(defun wm-select-activity (wm char)
  "Select the activity registered for CHAR; returns the window, or NIL."
  (let ((e (find (char-upcase char) *select-activities* :key #'car)))
    (when e
      (let* ((wins (remove-if-not (cadr (cdr e)) (wm-windows wm)))      ; front to back
             (focus (wm-focus wm)))
        (cond ((and wins (member focus wins) (cdr wins))             ; already there: go to the next one
               (wm-raise wm (car (last wins))) (car (last wins)))
              (wins (wm-raise wm (car wins)) (car wins))
              ((cadddr e) (funcall (cadddr e) wm))
              (t (setf (wm-message wm) (format nil "No ~A window is open" (cadr e))) nil))))))

(defun wm-pending-key (wm key)
  (let ((what (wm-pending wm)) (c (key-code key)))
    (setf (wm-pending wm) nil (wm-message wm) nil)
    (case what
      (:quit (if (%quit-confirm-key-p key) (setf (wm-quit wm) t) (setf (wm-message wm) "Quit cancelled")))
      (:select (if (and (integerp c) (< c 128) (zerop (logand (key-mods key) (logior +ctrl+ +meta+)))
                        (find (char-upcase (code-char c)) *select-activities* :key #'car))
                   (wm-select-activity wm (code-char c))
                   (unless (eq c :f9) (setf (wm-message wm) "Select cancelled")))))
    (setf (wm-dirty wm) t)
    t))

(defun wm-key (wm key)
  (setf (wm-message wm) nil)
  (cond ((wm-pending wm) (wm-pending-key wm key))
        ((wm-popups wm) (window-key (car (wm-popups wm)) key))
        ((wm-global-key wm key) t)
        ((wm-focus wm) (window-key (wm-focus wm) key))))

(defun %inner-coords (win x y)
  (if (window-border win) (values (- x (window-x win) 1) (- y (window-y win) 1)) (values (- x (window-x win)) (- y (window-y win)))))

(defun wm-press (wm x y button mods)
  (let ((w (wm-window-at wm x y)))
    (when w
      (unless (eq w (wm-focus wm)) (wm-raise wm w))
      (let ((lx (- x (window-x w))) (ly (- y (window-y w))) (ww (window-w w)) (hh (window-h w)))
        (cond ((and (window-border w) (= ly 0))
               (if (and (>= lx (- ww 4)) (<= lx (- ww 2)))
                   (wm-close-window wm w)
                   (setf (wm-drag wm) (list :move w lx ly))))
              ((and (window-border w) (= ly (- hh 1)) (= lx (- ww 1))) (setf (wm-drag wm) (list :resize w)))
              ((and (window-border w) (= lx (- ww 1)) (> ly 0) (< ly (- hh 1)))
               (setf (wm-drag wm) (list :scroll w)) (wm-scroll-to wm w ly))
              (t (multiple-value-bind (ilx ily) (%inner-coords w x y)
                   (window-mouse w :press ilx ily button mods))))))))

(defun wm-scroll-to (wm w ly)
  (declare (ignore wm))
  (let* ((ih (- (window-h w) 2)) (ch (window-content-height w)) (range (max 1 (- ch ih)))
         (target (floor (* (- ly 1) range) (max 1 (- ih 1)))))
    (window-scroll-by w (- (window-scroll w) target))))

(defun wm-drag-update (wm kind x y)
  (let* ((d (wm-drag wm)) (w (cadr d)))
    (case (car d)
      (:move (setf (window-x w) (- x (caddr d)) (window-y w) (- y (cadddr d))) (wm-fit wm w))
      (:resize (setf (window-w w) (max (window-min-w w) (+ 1 (- x (window-x w)))) (window-h w) (max (window-min-h w) (+ 1 (- y (window-y w)))))
               (wm-fit wm w) (window-resized w))
      (:scroll (wm-scroll-to wm w (- y (window-y w)))))
    (when (eq kind :release) (setf (wm-drag wm) nil))))

(defun wm-mouse (wm kind x y button mods)
  (when (wm-pending wm)                     ; a prompt is up: clicking its button answers it, any other click cancels
    (when (eq kind :press)
      (let ((b (and (= button 0) (wm-button-at wm x y))))
        (wm-pending-key wm (make-key (if (eq b (if (eq (wm-pending wm) :quit) :f10 :f9)) b :cancel) 0))))
    (return-from wm-mouse nil))
  (when (eq kind :press) (setf (wm-message wm) nil))   ; a message stays until the next click or key
  (cond ((wm-popups wm)
         (let ((p (car (wm-popups wm))))
           (if (window-contains-p p x y)
               (multiple-value-bind (lx ly) (%inner-coords p x y) (window-mouse p kind lx ly button mods))
               (when (eq kind :press) (wm-close-popups wm)))))
        ((and (wm-drag wm) (member kind '(:drag :release))) (wm-drag-update wm kind x y))
        ((and (eq kind :press) (= button 0) (wm-button-at wm x y))
         (wm-global-key wm (make-key (wm-button-at wm x y) 0)))
        ((eq kind :press) (wm-press wm x y button mods))
        ((member kind '(:wheel-up :wheel-down))
         (let ((w (wm-window-at wm x y))) (when w (window-scroll-by w (if (eq kind :wheel-up) 3 -3)))))
        (t (let ((w (wm-window-at wm x y)))
             (setf (wm-doc wm) nil)
             (when w
               (multiple-value-bind (lx ly) (%inner-coords w x y)
                 (when (eq kind :move) (setf (wm-doc wm) (window-hover-doc w lx ly)))
                 (when (member kind '(:drag :release)) (window-mouse w kind lx ly button mods))))))))

(defun wm-tick (wm)
  (setf (wm-ticks wm) (+ 1 (wm-ticks wm)))
  (dolist (w (wm-windows wm)) (window-tick w))
  (let ((clock (%clock-string)))
    (when (or (wm-busy wm) (string/= clock (wm-last-clock wm)))
      (setf (wm-last-clock wm) clock (wm-dirty wm) t))))

(defun wm-handle-event (wm ev)
  (handler-case
      (case (car ev)
        (:key (wm-key wm (normalize-key (cadr ev) (caddr ev))) (setf (wm-dirty wm) t))
        (:mouse (wm-mouse wm (cadr ev) (caddr ev) (cadddr ev) (nth 4 ev) (nth 5 ev)) (setf (wm-dirty wm) t))
        (:resize (wm-resize wm (cadr ev) (caddr ev)))
        (:eof (setf (wm-quit wm) t))
        (:tick (wm-tick wm))
        (:call (funcall (cadr ev))))           ; scripts only: run a function inside the event loop
    (error (e)
      (setf (wm-message wm) (format nil "Error: ~A" e) (wm-dirty wm) t))))

(defvar *wm-script* nil
  "While scripted events run headless (WM-RUN-EVENTS): a cons whose car is the events still to come, so that a
nested event loop (the window debugger) takes its input from the same script.")
(defvar *wm-live* nil "True in the process running WM-RUN on a terminal.")

(defun wm-next-scripted-event ()
  "The next scripted event, removed from the script, or NIL when there is none."
  (let ((q (car *wm-script*)))
    (when q (setf (car *wm-script*) (cdr q)) (car q))))

(defun wm-run-events (wm events)
  "Process scripted EVENTS headlessly and redraw."
  (let ((*wm-script* (list events)))
    (loop (let ((ev (wm-next-scripted-event))) (unless ev (return)) (wm-handle-event wm ev))))
  (wm-draw wm)
  wm)

(defun wm-serve (wm)
  "One turn of the event loop on the terminal: redraw if needed, then wait up to 250 ms for input and handle it."
  (when (wm-dirty wm) (wm-draw wm) (screen-flush (wm-screen wm)))
  (unless (wm-quit wm)
    (let ((ev (%term-read-event 250)))
      (if ev
          (let ((n 0))
            (loop (wm-handle-event wm ev)
                  (setq n (+ n 1))
                  (setq ev (and (< n 60) (not (wm-quit wm)) (%term-read-event 0)))
                  (unless ev (return))))
          (wm-handle-event wm (list :tick))))))

(defun wm-feed (wm bytes)
  "Feed terminal input BYTES (a string of escape sequences / text) through the real parser."
  (wm-run-events wm (%term-parse bytes)))

(defun wm-run (wm &optional on-start)
  (unless (%term-init) (error "The NeoGenera window system needs an interactive terminal"))
  (setq *color-mode* (detect-color-mode))
  (graphics-reset)
  (ignore-errors (detect-graphics))
  (unwind-protect
       (progn
         (multiple-value-bind (r c) (%term-size) (wm-resize wm r c))
         (screen-invalidate (wm-screen wm))
         (when on-start (funcall on-start))
         (let ((*wm-live* t))
           (loop (wm-serve wm) (when (wm-quit wm) (return)))))
    (when (eq *graphics* :kitty) (%term-image-delete 0) (graphics-reset))
    (%term-restore)))
