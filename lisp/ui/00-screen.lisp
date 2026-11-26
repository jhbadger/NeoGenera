;;;; 00-screen.lisp -- screen buffer, colors/themes, key normalization, diff flush.
;;;;
;;;; A SCREEN is a double-buffered grid of cells (code point, fg, bg, attributes).
;;;; Drawing writes the back buffer; SCREEN-FLUSH compares it with the front buffer
;;;; (what the terminal currently shows) and emits only the changes as terminal
;;;; escape sequences (damage tracking).  Colors are a palette index 0-255,
;;;; -1 (default) or an RGB value (+ #x1000000 r*65536 g*256 b); the output mode
;;;; (:truecolor, :256, :mono) is chosen from the environment.  A wide character (CHAR-DISPLAY-WIDTH 2)
;;;; fills two cells, the second holding +WIDE-CONT+; combining marks are merged with the character before them
;;;; into a cluster cell (a code from #x180000 naming an interned string).  Image cells start at #x200000.

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
  :prompt (rgb 120 40 120) :value (rgb 20 100 40)
  ;; the terminal's own cursor is not visible on this light background, so the window
  ;; manager paints a black block at the cursor cell (themes without :cursor use the terminal's)
  :cursor (rgb 0 0 0) :cursor-fg (rgb 248 247 240))
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

(defconstant +wide-cont+ #x110000 "The cell after a wide character.")
(defconstant +cluster-base+ #x180000)

(defvar *screen-clusters* (make-array 16 :adjustable t :fill-pointer 0) "Cluster strings, by code - +CLUSTER-BASE+.")
(defvar *screen-cluster-widths* (make-array 16 :adjustable t :fill-pointer 0))
(defvar *screen-cluster-ids* (make-hash-table :test 'equal))

(defun screen-cluster (string)
  "The cell code for the grapheme STRING (a character followed by combining marks)."
  (or (gethash string *screen-cluster-ids*)
      (let ((code (+ +cluster-base+ (fill-pointer *screen-clusters*))))
        (vector-push-extend string *screen-clusters*)
        (vector-push-extend (max 1 (string-display-width string)) *screen-cluster-widths*)
        (setf (gethash string *screen-cluster-ids*) code))))

(defun %cluster-p (ch) (and (>= ch +cluster-base+) (< ch #x200000)))

(defun cell-width (ch)
  "Columns the cell code CH occupies: 2 for a wide character, 0 for +WIDE-CONT+, otherwise 1."
  (cond ((< ch #x300) 1)
        ((= ch +wide-cont+) 0)
        ((%cluster-p ch) (aref *screen-cluster-widths* (- ch +cluster-base+)))
        ((>= ch #x110000) 1)
        (t (max 1 (char-display-width ch)))))

(defun %set-cell-char (s i ch) (setf (aref (scr-chars s) i) ch))

(defun screen-put (s r c ch fg bg attr)
  (when (and (>= r *clip-top*) (< r *clip-bottom*) (>= c *clip-left*) (< c *clip-right*)
             (>= r 0) (< r (scr-rows s)) (>= c 0) (< c (scr-cols s)))
    (let* ((cols (scr-cols s)) (i (+ (* r cols) c)) (chars (scr-chars s)) (old (aref chars i)) (w 1))
      (cond ((= ch +wide-cont+)                         ; only right after a wide character
             (setq w 0)
             (unless (and (> c 0) (= 2 (cell-width (aref chars (- i 1))))) (setq ch 32 w 1)))
            ((and (>= ch #x300) (< ch #x110000) (= 0 (char-display-width ch)))   ; a lone combining mark
             (setq ch (screen-cluster (utf8-from-codes (list 32 ch)))))
            (t (setq w (cell-width ch))))
      (when (and (= w 2) (or (>= (+ c 1) cols) (>= (+ c 1) *clip-right*))) (setq ch 32 w 1))
      ;; never leave half of a wide character behind
      (when (and (= old +wide-cont+) (/= ch +wide-cont+) (> c 0) (= 2 (cell-width (aref chars (- i 1)))))
        (%set-cell-char s (- i 1) 32))
      (when (and (= 2 (cell-width old)) (/= w 2) (< (+ c 1) cols) (= (aref chars (+ i 1)) +wide-cont+))
        (%set-cell-char s (+ i 1) 32))
      (setf (aref chars i) ch)
      (setf (aref (scr-fgs s) i) fg)
      (setf (aref (scr-bgs s) i) bg)
      (setf (aref (scr-attrs s) i) attr)
      (when (= w 2)
        (let ((j (+ i 1)))
          (when (and (= 2 (cell-width (aref chars j))) (< (+ c 2) cols) (= (aref chars (+ j 1)) +wide-cont+))
            (%set-cell-char s (+ j 1) 32))
          (setf (aref chars j) +wide-cont+ (aref (scr-fgs s) j) fg (aref (scr-bgs s) j) bg (aref (scr-attrs s) j) attr))))))

(defun screen-combine (s r c code)
  "Add the combining mark CODE to the character in the cell at row R, column C."
  (when (and (>= r *clip-top*) (< r *clip-bottom*) (>= c *clip-left*) (< c *clip-right*)
             (>= r 0) (< r (scr-rows s)) (>= c 0) (< c (scr-cols s)))
    (let* ((i (+ (* r (scr-cols s)) c)) (chars (scr-chars s)))
      (when (and (> c 0) (= (aref chars i) +wide-cont+)) (setq i (- i 1)))
      (let ((base (aref chars i)))
        (unless (>= base #x200000)
          (setf (aref chars i) (screen-cluster (%string-concat (%cell-string base) (utf8-from-codes (list code))))))))))

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
            (progn (screen-put s r col (if (< b 32) 32 b) fg bg attr) (setq i (+ i 1)) (setq col (+ col 1)))
            (multiple-value-bind (code next) (%utf8-decode string i)
              (setq i next)
              (let ((w (char-display-width code)))
                (cond ((and (= w 0) (> col c)) (screen-combine s r (- col 1) code))
                      (t (screen-put s r col code fg bg attr) (setq col (+ col (max 1 w))))))))))
    col))

(defun %cell-string (code)
  (cond ((< code 128) (string (code-char code)))
        ((= code +wide-cont+) "")
        ((%cluster-p code) (aref *screen-clusters* (- code +cluster-base+)))
        ((>= code #x110000) " ")                                              ; image cells read as spaces
        (t (%utf8-encode code))))

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
      (when p
        (return (cons r (let ((bytes 0) (col 0) (cols (scr-cols s)))
                          (loop (when (or (>= bytes p) (>= col cols)) (return))
                                (setq bytes (+ bytes (length (%cell-string (aref (scr-chars s) (+ (* r cols) col))))))
                                (setq col (+ col 1)))
                          ;; past the second half of a wide character
                          (loop (when (or (>= col cols) (/= (aref (scr-chars s) (+ (* r cols) col)) +wide-cont+)) (return col))
                                (setq col (+ col 1))))))))))

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

(defun %screen-image-keys (s)
  "A hash table of the image placements the back buffer shows, or NIL if it shows none."
  (let ((tab nil) (chars (scr-chars s)))
    (dotimes (i (length chars))
      (let ((ch (aref chars i)))
        (when (>= ch #x200000)
          (unless tab (setq tab (make-hash-table)))
          (setf (gethash (floor (- ch #x200000) 1048576) tab) t))))
    tab))

(defun screen-flush (s &optional (write t) (mode *color-mode*))
  "Emit the cells that differ from what the terminal shows.  Returns the vector of output codes."
  (let* ((out (make-array 4096 :adjustable t :fill-pointer 0))
         (rows (scr-rows s)) (cols (scr-cols s))
         (cur-fg -3) (cur-bg -3) (cur-attr -3) (pos-r -1) (pos-c -1)
         (visible (and (eq *graphics* :kitty) (%screen-image-keys s))))     ; placements on screen (not to be evicted)
    (dotimes (r rows)
      (let ((base (* r cols)))
        (dotimes (c cols)
          (let ((i (+ base c)))
            (let ((ch (aref (scr-chars s) i)) (fg (aref (scr-fgs s) i)) (bg (aref (scr-bgs s) i)) (at (aref (scr-attrs s) i)))
              (unless (or (and (eql ch (aref (scr-fchars s) i)) (eql fg (aref (scr-ffgs s) i))
                               (eql bg (aref (scr-fbgs s) i)) (eql at (aref (scr-fattrs s) i)))
                          (and (= ch +wide-cont+)                 ; drawn with the wide character before it
                               (progn (setf (aref (scr-fchars s) i) ch (aref (scr-ffgs s) i) fg (aref (scr-fbgs s) i) bg
                                            (aref (scr-fattrs s) i) at)
                                      t)))
                (unless (and (= pos-r r) (= pos-c c))
                  (%emit-csi out) (%emit-num out (+ r 1)) (%emit-str out ";") (%emit-num out (+ c 1))
                  (vector-push-extend 72 out))
                (if (>= ch #x200000)
                    (if (eq *graphics* :kitty)
                        (setq cur-fg (%emit-image-cell out ch bg mode visible) cur-bg bg cur-attr 0)
                        (progn (%emit-sgr out -1 bg 0 mode) (setq cur-fg -1 cur-bg bg cur-attr 0) (vector-push-extend 32 out)))
                    (progn
                      (unless (and (eql fg cur-fg) (eql bg cur-bg) (eql at cur-attr))
                        (%emit-sgr out fg bg at mode)
                        (setq cur-fg fg cur-bg bg cur-attr at))
                      (if (%cluster-p ch)
                          (dolist (code (utf8-codes (%cell-string ch))) (vector-push-extend code out))
                          (vector-push-extend ch out))))
                (setq pos-r r pos-c (+ c (if (>= ch #x200000) 1 (cell-width ch))))
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
