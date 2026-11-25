;;;; 00-screen.lisp -- screen buffer, colors/themes, key normalization, diff flush.
;;;;
;;;; A SCREEN is a double-buffered grid of cells (code point, fg, bg, attributes).
;;;; Drawing writes the back buffer; SCREEN-FLUSH compares it with the front buffer
;;;; (what the terminal currently shows) and emits only the changes as terminal
;;;; escape sequences (damage tracking).  Colors are a palette index 0-255,
;;;; -1 (default) or an RGB value (+ #x1000000 r*65536 g*256 b); the output mode
;;;; (:truecolor, :256, :mono) is chosen from the environment.  Limitation: every
;;;; code point is assumed to be one column wide.

(in-package :cl)

(defconstant +bold+ 1)
(defconstant +underline+ 2)
(defconstant +reverse+ 4)
(defconstant +dim+ 8)

(defun rgb (r g b) (+ #x1000000 (* r 65536) (* g 256) b))

;;; ---- themes -------------------------------------------------------------------------

(defstruct (theme (:constructor %make-theme) (:copier nil)) name colors)
(defvar *themes* nil)
(defvar *theme* nil)
(defun tc (key) (getf (theme-colors *theme*) key -1))

(defun define-theme (name &rest colors)
  (let ((cell (assq name *themes*)) (th (%make-theme :name name :colors colors)))
    (if cell (rplacd cell th) (setq *themes* (append *themes* (list (cons name th)))))
    th))
(defun find-theme (name) (cdr (assq name *themes*)))
(defun set-theme (name)
  (let ((th (find-theme name)))
    (unless th (error "No such theme: ~S" name))
    (setq *theme* th)))

;; classic: black on white, the 1980s workstation look
(define-theme :classic
  :fg (rgb 20 20 20) :bg (rgb 248 247 240) :dim (rgb 120 120 120)
  :title-fg (rgb 248 247 240) :title-bg (rgb 20 20 20) :inactive-fg (rgb 20 20 20) :inactive-bg (rgb 200 200 190)
  :border (rgb 90 90 90) :active-border (rgb 20 20 20)
  :wholine-fg (rgb 248 247 240) :wholine-bg (rgb 20 20 20) :doc-fg (rgb 60 60 60) :doc-bg (rgb 225 224 215)
  :menu-fg (rgb 20 20 20) :menu-bg (rgb 255 255 235) :sel-fg (rgb 248 247 240) :sel-bg (rgb 20 20 20)
  :hi-fg (rgb 248 247 240) :hi-bg (rgb 30 70 160) :link (rgb 20 60 150) :error (rgb 170 20 20)
  :scroll-track (rgb 200 200 190) :scroll-thumb (rgb 90 90 90) :desktop-bg (rgb 215 214 205) :desktop-fg (rgb 190 189 180)
  :prompt (rgb 120 40 120) :value (rgb 20 100 40))
(define-theme :green
  :fg (rgb 60 255 90) :bg (rgb 0 14 0) :dim (rgb 30 140 50)
  :title-fg (rgb 0 14 0) :title-bg (rgb 60 255 90) :inactive-fg (rgb 60 255 90) :inactive-bg (rgb 0 50 10)
  :border (rgb 30 140 50) :active-border (rgb 60 255 90)
  :wholine-fg (rgb 0 14 0) :wholine-bg (rgb 60 255 90) :doc-fg (rgb 60 255 90) :doc-bg (rgb 0 40 8)
  :menu-fg (rgb 60 255 90) :menu-bg (rgb 0 30 5) :sel-fg (rgb 0 14 0) :sel-bg (rgb 60 255 90)
  :hi-fg (rgb 0 14 0) :hi-bg (rgb 140 255 160) :link (rgb 140 255 160) :error (rgb 255 120 80)
  :scroll-track (rgb 0 50 10) :scroll-thumb (rgb 60 255 90) :desktop-bg (rgb 0 8 0) :desktop-fg (rgb 0 40 8)
  :prompt (rgb 140 255 160) :value (rgb 200 255 200))
(define-theme :amber
  :fg (rgb 255 176 0) :bg (rgb 18 10 0) :dim (rgb 150 100 0)
  :title-fg (rgb 18 10 0) :title-bg (rgb 255 176 0) :inactive-fg (rgb 255 176 0) :inactive-bg (rgb 60 36 0)
  :border (rgb 150 100 0) :active-border (rgb 255 176 0)
  :wholine-fg (rgb 18 10 0) :wholine-bg (rgb 255 176 0) :doc-fg (rgb 255 176 0) :doc-bg (rgb 45 26 0)
  :menu-fg (rgb 255 176 0) :menu-bg (rgb 35 20 0) :sel-fg (rgb 18 10 0) :sel-bg (rgb 255 176 0)
  :hi-fg (rgb 18 10 0) :hi-bg (rgb 255 214 120) :link (rgb 255 214 120) :error (rgb 255 90 60)
  :scroll-track (rgb 60 36 0) :scroll-thumb (rgb 255 176 0) :desktop-bg (rgb 10 6 0) :desktop-fg (rgb 45 26 0)
  :prompt (rgb 255 214 120) :value (rgb 255 230 170))
(define-theme :mono
  :fg -1 :bg -1 :dim -1 :title-fg -1 :title-bg -1 :inactive-fg -1 :inactive-bg -1 :border -1 :active-border -1
  :wholine-fg -1 :wholine-bg -1 :doc-fg -1 :doc-bg -1 :menu-fg -1 :menu-bg -1 :sel-fg -1 :sel-bg -1
  :hi-fg -1 :hi-bg -1 :link -1 :error -1 :scroll-track -1 :scroll-thumb -1 :desktop-bg -1 :desktop-fg -1
  :prompt -1 :value -1)
(set-theme :classic)

(defun theme-names () (mapcar #'car *themes*))
(defun next-theme ()
  (let* ((names (theme-names)) (rest (cdr (member (theme-name *theme*) names))))
    (set-theme (if rest (car rest) (car names)))))

(defvar *color-mode* :truecolor)
(defun detect-color-mode ()
  (let ((ct (getenv "COLORTERM")) (term (or (getenv "TERM") "")))
    (cond ((and ct (or (string= ct "truecolor") (string= ct "24bit"))) :truecolor)
          ((%string-search "256" term 0) :256)
          ((or (string= term "") (string= term "dumb") (string= term "linux")) :mono)
          (t :truecolor))))

(defun %rgb->256 (c)
  (let* ((v (- c #x1000000)) (r (floor v 65536)) (g (mod (floor v 256) 256)) (b (mod v 256)))
    (if (and (< (abs (- r g)) 10) (< (abs (- g b)) 10))
        (let ((gray (floor (+ r g b) 3)))
          (cond ((< gray 8) 16) ((> gray 248) 231) (t (+ 232 (floor (* (- gray 8) 24) 241)))))
        (+ 16 (* 36 (floor (* r 5) 255)) (* 6 (floor (* g 5) 255)) (floor (* b 5) 255)))))

;;; ---- screen buffer ----------------------------------------------------------------------

(defstruct (screen (:constructor %make-screen) (:conc-name scr-) (:copier nil))
  rows cols chars fgs bgs attrs fchars ffgs fbgs fattrs
  (cursor-row 0) (cursor-col 0) (cursor-visible nil))

(defun make-screen (rows cols)
  (let ((n (* rows cols)))
    (%make-screen :rows rows :cols cols
                  :chars (make-array n :initial-element 32) :fgs (make-array n :initial-element -1)
                  :bgs (make-array n :initial-element -1) :attrs (make-array n :initial-element 0)
                  :fchars (make-array n :initial-element -1) :ffgs (make-array n :initial-element -2)
                  :fbgs (make-array n :initial-element -2) :fattrs (make-array n :initial-element -1))))

(defun screen-resize (s rows cols)
  (let ((new (make-screen rows cols)))
    (setf (scr-rows s) rows (scr-cols s) cols
          (scr-chars s) (scr-chars new) (scr-fgs s) (scr-fgs new) (scr-bgs s) (scr-bgs new) (scr-attrs s) (scr-attrs new)
          (scr-fchars s) (scr-fchars new) (scr-ffgs s) (scr-ffgs new) (scr-fbgs s) (scr-fbgs new) (scr-fattrs s) (scr-fattrs new))
    s))

(defun screen-invalidate (s) (fill (scr-fchars s) -1) s)

(defvar *clip-top* 0)
(defvar *clip-left* 0)
(defvar *clip-bottom* 100000)
(defvar *clip-right* 100000)

(defmacro with-clip ((top left bottom right) &body body)
  `(let ((*clip-top* (max *clip-top* ,top)) (*clip-left* (max *clip-left* ,left))
         (*clip-bottom* (min *clip-bottom* ,bottom)) (*clip-right* (min *clip-right* ,right)))
     ,@body))

(defun screen-put (s r c ch fg bg attr)
  (when (and (>= r *clip-top*) (< r *clip-bottom*) (>= c *clip-left*) (< c *clip-right*)
             (>= r 0) (< r (scr-rows s)) (>= c 0) (< c (scr-cols s)))
    (let ((i (+ (* r (scr-cols s)) c)))
      (setf (aref (scr-chars s) i) ch)
      (setf (aref (scr-fgs s) i) fg)
      (setf (aref (scr-bgs s) i) bg)
      (setf (aref (scr-attrs s) i) attr))))

(defun screen-fill (s r c h w ch fg bg attr)
  (dotimes (i h) (dotimes (j w) (screen-put s (+ r i) (+ c j) ch fg bg attr))))

(defun screen-clear (s &optional (fg -1) (bg -1))
  (dotimes (i (* (scr-rows s) (scr-cols s)))
    (setf (aref (scr-chars s) i) 32) (setf (aref (scr-fgs s) i) fg)
    (setf (aref (scr-bgs s) i) bg) (setf (aref (scr-attrs s) i) 0)))

(defun screen-draw-string (s r c string fg bg attr &optional (start 0) end)
  "Draw STRING (UTF-8 bytes) at row R, column C; returns the column after the last character."
  (let ((i start) (n (or end (length string))) (col c))
    (loop
      (when (>= i n) (return))
      (let ((b (char-code (char string i))))
        (if (< b 128)
            (progn (screen-put s r col (if (< b 32) 32 b) fg bg attr) (setq i (+ i 1)))
            (multiple-value-bind (code next) (%utf8-decode string i)
              (screen-put s r col code fg bg attr)
              (setq i next))))
      (setq col (+ col 1)))
    col))

(defun %cell-string (code) (if (< code 128) (string (code-char code)) (%utf8-encode code)))

(defun screen-row-string (s r &optional (trim t))
  (let ((parts nil) (cols (scr-cols s)))
    (dotimes (c cols) (setq parts (cons (%cell-string (aref (scr-chars s) (+ (* r cols) c))) parts)))
    (let ((str (apply #'%string-concat (nreverse parts))))
      (if trim (string-right-trim " " str) str))))

(defun screen-text (s)
  (let ((lines nil))
    (dotimes (r (scr-rows s)) (setq lines (cons (screen-row-string s r) lines)))
    (%join-strings (nreverse lines) (string #\Newline))))

(defun screen-find-text (s text)
  "Returns (row . column) of the first occurrence of TEXT in the back buffer, or NIL."
  (dotimes (r (scr-rows s) nil)
    (let* ((row (screen-row-string s r nil)) (p (%string-search text row 0)))
      ;; P is a byte offset into the UTF-8 row; convert it to a screen column
      (when p (return (cons r (let ((n 0)) (dotimes (i p) (unless (= (logand (char-code (char row i)) #xC0) #x80) (setq n (+ n 1)))) n)))))))

(defun screen-cell (s r c)
  (let ((i (+ (* r (scr-cols s)) c)))
    (list (aref (scr-chars s) i) (aref (scr-fgs s) i) (aref (scr-bgs s) i) (aref (scr-attrs s) i))))

;;; ---- output: diff the back buffer against the front buffer -------------------------------

(defun %emit-str (out str) (dotimes (i (length str)) (vector-push-extend (char-code (char str i)) out)))
(defun %emit-num (out n) (%emit-str out (%number-to-string n)))
(defun %emit-csi (out) (vector-push-extend 27 out) (vector-push-extend 91 out))

(defun %emit-color (out base c mode)
  (cond ((eq mode :mono) nil)
        ((< c 0) (%emit-str out (if (= base 38) ";39" ";49")))
        ((>= c #x1000000)
         (if (eq mode :truecolor)
             (let* ((v (- c #x1000000)))
               (%emit-str out ";") (%emit-num out base) (%emit-str out ";2;")
               (%emit-num out (floor v 65536)) (%emit-str out ";") (%emit-num out (mod (floor v 256) 256))
               (%emit-str out ";") (%emit-num out (mod v 256)))
             (progn (%emit-str out ";") (%emit-num out base) (%emit-str out ";5;") (%emit-num out (%rgb->256 c)))))
        (t (%emit-str out ";") (%emit-num out base) (%emit-str out ";5;") (%emit-num out c))))

(defun %emit-sgr (out fg bg attr mode)
  (%emit-csi out) (%emit-str out "0")
  (when (/= 0 (logand attr +bold+)) (%emit-str out ";1"))
  (when (/= 0 (logand attr +dim+)) (%emit-str out ";2"))
  (when (/= 0 (logand attr +underline+)) (%emit-str out ";4"))
  (when (/= 0 (logand attr +reverse+)) (%emit-str out ";7"))
  (%emit-color out 38 fg mode)
  (%emit-color out 48 bg mode)
  (vector-push-extend 109 out))

(defun screen-flush (s &optional (write t) (mode *color-mode*))
  "Emit the cells that differ from what the terminal shows.  Returns the vector of output codes."
  (let* ((out (make-array 4096 :adjustable t :fill-pointer 0))
         (rows (scr-rows s)) (cols (scr-cols s))
         (cur-fg -3) (cur-bg -3) (cur-attr -3) (pos-r -1) (pos-c -1))
    (dotimes (r rows)
      (let ((base (* r cols)))
        (dotimes (c cols)
          (let ((i (+ base c)))
            (let ((ch (aref (scr-chars s) i)) (fg (aref (scr-fgs s) i)) (bg (aref (scr-bgs s) i)) (at (aref (scr-attrs s) i)))
              (unless (and (eql ch (aref (scr-fchars s) i)) (eql fg (aref (scr-ffgs s) i))
                           (eql bg (aref (scr-fbgs s) i)) (eql at (aref (scr-fattrs s) i)))
                (unless (and (= pos-r r) (= pos-c c))
                  (%emit-csi out) (%emit-num out (+ r 1)) (%emit-str out ";") (%emit-num out (+ c 1))
                  (vector-push-extend 72 out))
                (unless (and (eql fg cur-fg) (eql bg cur-bg) (eql at cur-attr))
                  (%emit-sgr out fg bg at mode)
                  (setq cur-fg fg cur-bg bg cur-attr at))
                (vector-push-extend ch out)
                (setq pos-r r pos-c (+ c 1))
                (setf (aref (scr-fchars s) i) ch) (setf (aref (scr-ffgs s) i) fg)
                (setf (aref (scr-fbgs s) i) bg) (setf (aref (scr-fattrs s) i) at)))))))
    (when (/= cur-attr -3) (%emit-csi out) (%emit-str out "0m"))
    (%emit-csi out) (%emit-num out (+ (scr-cursor-row s) 1)) (%emit-str out ";")
    (%emit-num out (+ (scr-cursor-col s) 1)) (vector-push-extend 72 out)
    (vector-push-extend 27 out)
    (%emit-str out (if (scr-cursor-visible s) "[?25h" "[?25l"))
    (when write (%term-write-codes out))
    out))

;;; ---- keys ----------------------------------------------------------------------------------

(defconstant +shift+ 1)
(defconstant +meta+ 2)
(defconstant +ctrl+ 4)

(defun make-key (code mods) (list code mods))
(defun key-code (k) (car k))
(defun key-mods (k) (cadr k))

(defun normalize-key (code mods)
  "Turn a raw (:KEY code mods) event into a key (code mods): code is a character code or a keyword."
  (cond ((not (integerp code)) (make-key code mods))
        ((or (= code 13) (= code 10)) (make-key :return mods))
        ((= code 9) (make-key :tab mods))
        ((= code 27) (make-key :escape mods))
        ((or (= code 127) (= code 8)) (make-key :backspace mods))
        ((= code 0) (make-key 32 (logior mods +ctrl+)))
        ((<= 1 code 26) (make-key (+ code 96) (logior mods +ctrl+)))
        ((<= 28 code 31) (make-key (+ code 64) (logior mods +ctrl+)))
        (t (make-key code mods))))

(defun key-string (k)
  (let* ((code (key-code k)) (mods (key-mods k))
         (base (cond ((integerp code) (cond ((= code 32) "SPC") (t (%cell-string code))))
                     (t (string-downcase (symbol-name code))))))
    (%string-concat (if (/= 0 (logand mods +ctrl+)) "C-" "") (if (/= 0 (logand mods +meta+)) "M-" "")
                    (if (and (/= 0 (logand mods +shift+)) (not (integerp code))) "S-" "")
                    (if (and (keywordp code) (not (eq code :escape))) (%string-concat "<" base ">") base))))

(defun parse-key (str)
  "\"C-x\" \"M-f\" \"RET\" \"<up>\" \"a\" -> key"
  (let ((mods 0) (s str))
    (loop
      (cond ((and (> (length s) 2) (string= (subseq s 0 2) "C-")) (setq mods (logior mods +ctrl+)) (setq s (subseq s 2)))
            ((and (> (length s) 2) (string= (subseq s 0 2) "M-")) (setq mods (logior mods +meta+)) (setq s (subseq s 2)))
            ((and (> (length s) 2) (string= (subseq s 0 2) "S-")) (setq mods (logior mods +shift+)) (setq s (subseq s 2)))
            (t (return))))
    (make-key (cond ((string= s "RET") :return) ((string= s "TAB") :tab) ((string= s "ESC") :escape)
                    ((string= s "DEL") :backspace) ((string= s "SPC") 32)
                    ((and (> (length s) 2) (char= (char s 0) #\<)) (intern (string-upcase (subseq s 1 (- (length s) 1))) "KEYWORD"))
                    (t (char-code (char s 0))))
              mods)))

(defun parse-key-sequence (str)
  (mapcar #'parse-key (let ((parts (%split-string str #\Space))) (remove "" parts :test #'string=))))
