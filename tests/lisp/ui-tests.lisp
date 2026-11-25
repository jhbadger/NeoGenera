;;;; ui-tests.lisp -- terminal input parsing, screen diffing, window manager, Listener.
;;;; Everything runs headless: input bytes go through the real escape-sequence parser
;;;; (%TERM-PARSE) and the result is read back from the screen buffer.

(in-package :cl)

(defun esc (str) (%string-concat (string (code-char 27)) str))
(defun mouse-bytes (kind x y &optional (button 0))
  (let ((b (case kind (:press button) (:release button) (:move 35) (:drag (+ 32 button)) (:wheel-up 64) (:wheel-down 65))))
    (format nil "~C[<~D;~D;~D~A" (code-char 27) b (+ x 1) (+ y 1) (if (eq kind :release) "m" "M"))))

(defun make-test-wm (&optional (rows 24) (cols 80))
  (let* ((wm (make-wm rows cols)) (lw (make-instance 'listener-window :title "Listener")))
    (wm-add-window wm lw)
    (wm-tile wm :horizontal)
    (wm-draw wm)
    (values wm lw)))

(defun type-line (wm text) (wm-feed wm text) (wm-feed wm (string (code-char 13))))
(defun screen-has (wm text) (screen-find-text (wm-screen wm) text))

(deftest terminal-input-parsing
  (is-equal '((:key 97 0)) (%term-parse "a"))
  (is-equal '((:key 97 0) (:key 98 0)) (%term-parse "ab"))
  (is-equal '((:key 13 0)) (%term-parse (string (code-char 13))))
  (is-equal '((:key :up 0) (:key :down 0) (:key :right 0) (:key :left 0))
            (%term-parse (%string-concat (esc "[A") (esc "[B") (esc "[C") (esc "[D"))))
  (is-equal '((:key :home 0) (:key :end 0) (:key :pgup 0) (:key :pgdn 0) (:key :delete 0))
            (%term-parse (%string-concat (esc "[H") (esc "[F") (esc "[5~") (esc "[6~") (esc "[3~"))))
  (is-equal '((:key :f5 0) (:key :f10 0)) (%term-parse (%string-concat (esc "[15~") (esc "[21~"))))
  (is-equal '((:key :f1 0) (:key :f4 0)) (%term-parse (%string-concat (esc "OP") (esc "OS"))))
  (is-equal '((:key :right 4)) (%term-parse (esc "[1;5C")))
  (is-equal '((:key :left 2)) (%term-parse (esc "[1;3D")))
  (is-equal '((:key 102 2)) (%term-parse (esc "f")))
  (is-equal '((:key 27 0)) (%term-parse (string (code-char 27))))
  (is-equal '((:key 9 1)) (%term-parse (esc "[Z")))
  (is-equal '((:key 955 0)) (%term-parse (%utf8-encode 955)))
  (is-equal '((:mouse :press 9 4 0 0)) (%term-parse (mouse-bytes :press 9 4)))
  (is-equal '((:mouse :release 9 4 0 0)) (%term-parse (mouse-bytes :release 9 4)))
  (is-equal '((:mouse :press 0 0 2 0)) (%term-parse (mouse-bytes :press 0 0 2)))
  (is-equal '((:mouse :move 5 6 3 0)) (%term-parse (mouse-bytes :move 5 6)))
  (is-equal '((:mouse :drag 7 8 0 0)) (%term-parse (mouse-bytes :drag 7 8)))
  (is-equal '((:mouse :wheel-up 1 1 0 0) (:mouse :wheel-down 1 1 0 0))
            (%term-parse (%string-concat (mouse-bytes :wheel-up 1 1) (mouse-bytes :wheel-down 1 1))))
  (is-equal '((:key 120 0) (:key :up 0) (:key 121 0)) (%term-parse (%string-concat "x" (esc "[A") "y")))
  (is-equal nil (%term-parse "")))

(deftest key-names
  (is-equal '(120 4) (normalize-key 24 0))
  (is-equal '(:return 0) (normalize-key 13 0))
  (is-equal '(:backspace 0) (normalize-key 127 0))
  (is-equal '(:tab 0) (normalize-key 9 0))
  (is-equal "C-x" (key-string (normalize-key 24 0)))
  (is-equal "M-f" (key-string (normalize-key 102 2)))
  (is-equal "<up>" (key-string (normalize-key :up 0)))
  (is-equal '(120 4) (parse-key "C-x"))
  (is-equal '(102 2) (parse-key "M-f"))
  (is-equal '(:return 0) (parse-key "RET"))
  (is-equal '((120 4) (115 4)) (parse-key-sequence "C-x C-s")))

(deftest screen-buffer
  (let ((s (make-screen 5 20)))
    (screen-clear s)
    (screen-draw-string s 1 2 "hello" -1 -1 0)
    (is-equal "  hello" (screen-row-string s 1))
    (is-equal '(0 . 0) (progn (setf (car (list 1)) 1) (cons 0 0)))
    (is-equal '(1 . 2) (screen-find-text s "hello"))
    ;; clipping
    (screen-clear s)
    (with-clip (1 1 2 4) (screen-draw-string s 0 0 "abcdefg" -1 -1 0) (screen-draw-string s 1 0 "abcdefg" -1 -1 0))
    (is-equal "" (screen-row-string s 0))
    (is-equal " bcd" (screen-row-string s 1))
    ;; UTF-8 text occupies one cell per character
    (screen-clear s)
    (screen-draw-string s 0 0 (%string-concat "a" (%utf8-encode 955) "b") -1 -1 0)
    (is-equal 955 (car (screen-cell s 0 1)))
    (is-equal 98 (car (screen-cell s 0 2)))
    (screen-fill s 2 0 1 5 120 7 -1 0)
    (is-equal "xxxxx" (screen-row-string s 2))
    (is-equal '(120 7 -1 0) (screen-cell s 2 0))))

(deftest screen-diff-flush
  (let ((s (make-screen 10 40)))
    (screen-clear s)
    (screen-draw-string s 0 0 "first frame" -1 -1 0)
    (let ((first (screen-flush s nil :truecolor)))
      (is (> (length first) 300)))                       ; the first flush draws every cell
    (let ((second (screen-flush s nil :truecolor)))
      (is (< (length second) 20)))                       ; nothing changed: only the cursor sequence
    (screen-draw-string s 3 5 "X" -1 -1 0)
    (let ((third (screen-flush s nil :truecolor)))
      (is (< (length third) 40))                         ; one cell changed
      (is (find 88 third)))
    (screen-draw-string s 3 5 "Y" (rgb 255 0 0) -1 +bold+)
    (let ((out (screen-flush s nil :truecolor)))
      (is (> (length out) 10))
      (is (%string-search "38;2;255;0;0" (coerce (mapcar #'code-char (%vector-to-list out)) 'string) 0)))
    (screen-draw-string s 4 0 "Z" (rgb 255 0 0) -1 0)
    (let ((txt (coerce (mapcar #'code-char (%vector-to-list (screen-flush s nil :256))) 'string)))
      (is (%string-search "38;5;196" txt 0)))              ; truecolor red -> xterm-256 index 196
    (screen-draw-string s 5 0 "M" (rgb 255 0 0) (rgb 0 0 255) +reverse+)
    (let ((txt (coerce (mapcar #'code-char (%vector-to-list (screen-flush s nil :mono))) 'string)))
      (is (not (%string-search "38;" txt 0)))
      (is (%string-search ";7" txt 0)))                    ; attributes survive in mono mode
    (screen-invalidate s)
    (is (> (length (screen-flush s nil :truecolor)) 300)))) ; invalidation forces a full redraw

(deftest themes
  (is (member :classic (theme-names))) (is (member :green (theme-names))) (is (member :amber (theme-names)))
  (set-theme :green)
  (is-equal :green (theme-name *theme*))
  (is-equal (rgb 60 255 90) (tc :fg))
  (next-theme)
  (is (not (eq :green (theme-name *theme*))))
  (set-theme :classic)
  (signals-error (set-theme :no-such-theme)))

(deftest listener-evaluation
  (multiple-value-bind (wm lw) (make-test-wm)
    (type-line wm "(+ 1 2)")
    (is (screen-has wm "=> 3"))
    (type-line wm "(defun sq (x) (* x x))")
    (type-line wm "(sq 12)")
    (is (screen-has wm "=> 144"))
    (type-line wm "(values 1 2)")
    (is (screen-has wm "=> 2"))
    (type-line wm "(format t \"hello ~A~%\" 'world)")
    (is (screen-has wm "hello WORLD"))
    (type-line wm "(expt 2 100)")
    (is (screen-has wm "=> 1267650600228229401496703205376"))
    (type-line wm "(/ 22 7)")
    (is (screen-has wm "=> 22/7"))
    (type-line wm "*")
    (is-equal 22/7 *)
    (type-line wm "(values)")
    (is (screen-has wm "; No values"))
    (type-line wm "(car 5)")
    (is (screen-has wm "Error: The value 5 is not of type LIST"))
    (is (lw-last-error lw))
    (type-line wm "(+ 40 2)")                          ; the listener keeps working after an error
    (is (screen-has wm "=> 42"))
    (type-line wm ":bt")
    (is (screen-has wm "0:"))
    (type-line wm "(error \"custom ~D\" 7)")
    (is (screen-has wm "Error: custom 7"))
    (type-line wm "(sq 3) (sq 4)")                     ; several forms on one line
    (is (screen-has wm "=> 9")) (is (screen-has wm "=> 16"))))

(deftest listener-editing
  (multiple-value-bind (wm lw) (make-test-wm)
    (wm-feed wm "(+ 1 2)")
    (is-equal "(+ 1 2)" (lw-input lw))
    (wm-feed wm (esc "[D"))                      ; left
    (wm-feed wm "0")
    (is-equal "(+ 1 20)" (lw-input lw))
    (wm-feed wm (string (code-char 1)))          ; C-a
    (is-equal 0 (lw-cursor-pos lw))
    (wm-feed wm (string (code-char 5)))          ; C-e
    (is-equal 8 (lw-cursor-pos lw))
    (wm-feed wm (esc "[D"))
    (wm-feed wm (string (code-char 127)))        ; backspace deletes the 0
    (is-equal "(+ 1 2)" (lw-input lw))
    (wm-feed wm (string (code-char 21)))         ; C-u kills the line
    (is-equal "" (lw-input lw))
    (wm-feed wm (string (code-char 25)))         ; C-y yanks it back
    (is-equal "(+ 1 2)" (lw-input lw))
    (wm-feed wm (string (code-char 11)))         ; C-k at end: nothing
    (wm-feed wm (string (code-char 13)))
    (is-equal "" (lw-input lw))
    ;; history
    (wm-feed wm (esc "[A"))
    (is-equal "(+ 1 2)" (lw-input lw))
    (type-line wm "")                            ; submit the recalled line again
    (type-line wm "(list 1 2)")
    (wm-feed wm (esc "[A"))
    (is-equal "(list 1 2)" (lw-input lw))
    (wm-feed wm (esc "[A"))
    (is-equal "(+ 1 2)" (lw-input lw))
    (wm-feed wm (esc "[B"))
    (is-equal "(list 1 2)" (lw-input lw))
    (wm-feed wm (string (code-char 21)))
    ;; unbalanced forms continue on the next line
    (wm-feed wm "(+ 1")
    (wm-feed wm (string (code-char 13)))
    (is (%string-search (string #\Newline) (lw-input lw) 0))
    (type-line wm "  2)")
    (is (screen-has wm "=> 3"))
    ;; meta word movement and kill word
    (wm-feed wm "foo bar baz")
    (wm-feed wm (esc "b"))
    (is-equal 8 (lw-cursor-pos lw))
    (wm-feed wm (string (code-char 23)))         ; C-w
    (is-equal "foo baz" (lw-input lw))
    (wm-feed wm (esc "[3~"))                     ; delete
    (is-equal "foo az" (lw-input lw))
    (wm-feed wm (esc ""))))

(deftest listener-completion-and-commands
  (multiple-value-bind (wm lw) (make-test-wm)
    (wm-feed wm "(defu")
    (wm-feed wm (string (code-char 9)))
    (is-equal "(defun" (lw-input lw))
    (wm-feed wm (string (code-char 21)))
    (wm-feed wm ":sh")
    (wm-feed wm (string (code-char 9)))
    (is (%string-search "show" (string-downcase (lw-input lw)) 0))
    (wm-feed wm (string (code-char 21)))
    (type-line wm ":show packages")
    (is (screen-has wm "COMMON-LISP"))
    (type-line wm ":sh pack")                    ; unique-prefix abbreviation
    (is (screen-has wm "KEYWORD"))
    (type-line wm ":package keyword")
    (is-equal "KEYWORD" (package-name (lw-pkg lw)))
    (type-line wm ":package cl-user")
    (type-line wm ":describe 42")
    (is (screen-has wm "is of type fixnum"))
    (type-line wm ":nonsense")
    (is (screen-has wm "Unknown command"))
    ;; prompting for a missing argument
    (type-line wm ":package")
    (is (lw-pending lw))
    (is (screen-has wm "Package:"))
    (type-line wm "common-lisp")
    (is (not (lw-pending lw)))
    (is-equal "COMMON-LISP" (package-name (lw-pkg lw)))
    (type-line wm ":package cl-user")
    (type-line wm ":help")
    (is (screen-has wm ":tile"))                  ; the command list ends with :tile (earlier lines scroll away)
    (type-line wm ":clear screen")
    (is (not (screen-has wm ":tile")))
    (type-line wm ":theme green")
    (is-equal :green (theme-name *theme*))
    (type-line wm ":theme classic")
    (is-equal :classic (theme-name *theme*))))

(deftest presentations-and-mouse
  (multiple-value-bind (wm lw) (make-test-wm)
    (type-line wm "(list 1 2 3)")
    (let* ((pos (screen-has wm "=> (1 2 3)")) (row (car pos)) (col (+ (cdr pos) 4)))
      (is pos)
      ;; hovering over the value shows what a click would do
      (wm-feed wm (mouse-bytes :move col row))
      (is (%string-search "L: Describe" (wm-doc wm) 0))
      (wm-draw wm)
      (is (screen-has wm "L: Describe"))
      (is (lw-hover lw))
      (is-equal 'list (pres-type (lw-hover lw)))
      ;; moving off clears it
      (wm-feed wm (mouse-bytes :move 70 20))
      (is (null (lw-hover lw)))
      ;; left click runs the default command
      (wm-feed wm (mouse-bytes :press col row 0))
      (wm-feed wm (mouse-bytes :release col row 0))
      (is (screen-has wm "a list of length 3"))
      ;; middle click copies the object to the input line
      (wm-feed wm (mouse-bytes :press col row 1))
      (wm-feed wm (mouse-bytes :release col row 1))
      (is-equal "'(1 2 3)" (lw-input lw))
      (wm-feed wm (string (code-char 21)))
      ;; right click opens a menu; keyboard selection runs the command
      (wm-feed wm (mouse-bytes :press col row 2))
      (wm-feed wm (mouse-bytes :release col row 2))
      (is (wm-popups wm))
      (is (screen-has wm "Describe"))
      (is (screen-has wm "Copy to input"))
      (wm-feed wm (esc "[B"))                    ; down: "Copy to input"
      (wm-feed wm (string (code-char 13)))
      (is (null (wm-popups wm)))
      (is-equal "'(1 2 3)" (lw-input lw))
      (wm-feed wm (string (code-char 21)))
      ;; clicking elsewhere dismisses a menu
      (wm-feed wm (mouse-bytes :press col row 2))
      (is (wm-popups wm))
      (wm-feed wm (mouse-bytes :press 2 2 0))
      (is (null (wm-popups wm))))
    ;; objects without a readable form are copied as references
    (type-line wm "(make-hash-table)")
    (let* ((pos (screen-has wm "=> #<HASH-TABLE")) (row (car pos)) (col (+ (cdr pos) 4)))
      (wm-feed wm (mouse-bytes :press col row 1))
      (is (%string-search "(result " (lw-input lw) 0)))
    (is (typep (first *result-history*) 'hash-table))))

(deftest presentation-types
  (is-equal 'integer (presentation-type-of 5))
  (is-equal 'list (presentation-type-of '(1)))
  (is-equal 'null (presentation-type-of nil))
  (is-equal 'keyword (presentation-type-of :a))
  (is-equal 'symbol (presentation-type-of 'a))
  (is-equal 'function (presentation-type-of #'car))
  (is-equal 'animal (presentation-type-of (make-instance 'animal)))
  (is (member 'number (presentation-supertypes 'integer)))
  (is (member 'symbol (presentation-supertypes 'keyword)))
  (is-equal 't (car (last (presentation-supertypes 'integer))))
  (is (member 'animal (presentation-supertypes 'dog)))
  (is-equal 'describe (pcmd-name (default-pcommand 'integer)))
  (is-equal 'disassemble (pcmd-name (find 'disassemble (applicable-pcommands 'function) :key #'pcmd-name)))
  (define-presentation-command (test-only integer :documentation "Test only") (n) (list n))
  (is (find 'test-only (applicable-pcommands 'integer) :key #'pcmd-name))
  (is (not (find 'test-only (applicable-pcommands 'string) :key #'pcmd-name)))
  ;; a more specific type's command wins over the same-named command on a general type
  (define-presentation-command (describe fixnum :documentation "Fixnum describe") (n) n)
  (is-equal "Fixnum describe" (pcmd-label (find 'describe (applicable-pcommands 'fixnum) :key #'pcmd-name))))

(deftest window-management
  (multiple-value-bind (wm lw) (make-test-wm)
    (let ((help (make-instance 'text-window :title "Notes" :lines (list "line one" "line two" "line three") :x 10 :y 3 :w 30 :h 8)))
      (wm-add-window wm help)
      (wm-draw wm)
      (is (screen-has wm "line two"))
      (is (eq help (wm-focus wm)))
      (is (eq help (car (wm-windows wm))))
      ;; focus cycling
      (wm-feed wm (esc "[17~"))                  ; F6
      (is (eq lw (wm-focus wm)))
      (wm-feed wm (esc "[17~"))
      (is (eq help (wm-focus wm)))
      ;; drag the title bar
      (let ((x (window-x help)) (y (window-y help)))
        (wm-feed wm (mouse-bytes :press (+ x 4) y 0))
        (wm-feed wm (mouse-bytes :drag (+ x 14) (+ y 5) 0))
        (wm-feed wm (mouse-bytes :release (+ x 14) (+ y 5) 0))
        (is-equal (+ x 10) (window-x help))
        (is-equal (+ y 5) (window-y help)))
      ;; resize with the corner grip
      (let ((cx (+ (window-x help) (window-w help) -1)) (cy (+ (window-y help) (window-h help) -1)) (w0 (window-w help)))
        (wm-feed wm (mouse-bytes :press cx cy 0))
        (wm-feed wm (mouse-bytes :drag (+ cx 8) (+ cy 2) 0))
        (wm-feed wm (mouse-bytes :release (+ cx 8) (+ cy 2) 0))
        (is-equal (+ w0 8) (window-w help)))
      ;; windows cannot be dragged off screen
      (wm-feed wm (mouse-bytes :press (+ (window-x help) 3) (window-y help) 0))
      (wm-feed wm (mouse-bytes :drag 200 200 0))
      (wm-feed wm (mouse-bytes :release 200 200 0))
      (is (<= (+ (window-x help) (window-w help)) 80))
      (is (<= (+ (window-y help) (window-h help)) 22))
      ;; clicking a background window raises it
      (wm-tile wm :horizontal)
      (wm-feed wm (mouse-bytes :press (+ (window-x lw) 2) (+ (window-y lw) 3) 0))
      (wm-feed wm (mouse-bytes :release (+ (window-x lw) 2) (+ (window-y lw) 3) 0))
      (is (eq lw (wm-focus wm)))
      ;; closing with the [x] box
      (let ((cx (- (+ (window-x help) (window-w help)) 3)) (cy (window-y help)))
        (wm-feed wm (mouse-bytes :press cx cy 0))
        (is (not (member help (wm-windows wm))))
        (is (eq lw (wm-focus wm)))))))

(deftest tiling
  (let ((wm (make-wm 24 80)))
    (let ((a (make-instance 'text-window :title "A")) (b (make-instance 'text-window :title "B")) (c (make-instance 'text-window :title "C")))
      (dolist (w (list a b c)) (wm-add-window wm w))
      (wm-tile wm :horizontal)
      (is-equal '(0 26 53) (mapcar #'window-x (list a b c)))
      (is-equal 22 (window-h a))
      (is-equal 80 (reduce #'+ (mapcar #'window-w (list a b c))))
      (wm-tile wm :vertical)
      (is-equal 80 (window-w b))
      (is-equal 22 (reduce #'+ (mapcar #'window-h (list a b c))))
      (is-equal 0 (window-y a))
      (wm-tile wm :main)
      (is-equal 48 (window-w a))
      (is-equal 32 (window-w b))
      (is-equal 22 (window-h a))
      (is-equal 11 (window-h b))
      (wm-tile wm :grid)
      (is-equal 2 (length (remove-duplicates (mapcar #'window-x (list a b c)))))
      ;; no two tiled windows overlap
      (dolist (p (list a b c))
        (dolist (q (list a b c))
          (unless (eq p q)
            (is (or (<= (+ (window-x p) (window-w p)) (window-x q)) (<= (+ (window-x q) (window-w q)) (window-x p))
                    (<= (+ (window-y p) (window-h p)) (window-y q)) (<= (+ (window-y q) (window-h q)) (window-y p))))))))
    ;; F7 cycles layouts, F8 themes, F10 quits
    (wm-feed wm (esc "[18~"))                    ; F7: grid -> horizontal -> vertical
    (wm-feed wm (esc "[18~"))
    (is-equal :vertical (wm-layout wm))
    (wm-feed wm (esc "[19~"))
    (is (not (eq :classic (theme-name *theme*))))
    (set-theme :classic)
    (is (not (wm-quit wm)))
    (wm-feed wm (esc "[21~"))
    (is (wm-quit wm))))

(deftest scrolling-and-scrollbar
  (multiple-value-bind (wm lw) (make-test-wm)
    (type-line wm "(dotimes (i 80) (format t \"row ~D~%\" i))")
    (is (screen-has wm "row 79"))
    (is (not (screen-has wm "row 0 ")))
    ;; a scrollbar thumb appears on the right border
    (let ((found nil))
      (dotimes (r 22) (when (eql (car (screen-cell (wm-screen wm) r 79)) +block+) (setq found t)))
      (is found))
    (wm-feed wm (mouse-bytes :wheel-up 10 5))
    (is (> (lw-scroll-back lw) 0))
    (is (not (screen-has wm "row 79")))
    (wm-feed wm (esc "[5~"))                     ; page up
    (is (> (lw-scroll-back lw) 3))
    (wm-feed wm (esc "<"))                       ; M-< : top
    (is (screen-has wm "NeoGenera"))
    (wm-feed wm (esc ">"))                       ; M-> : bottom
    (is (screen-has wm "row 79"))
    (is-equal 0 (lw-scroll-back lw))
    (wm-feed wm (mouse-bytes :wheel-down 10 5))
    (is-equal 0 (lw-scroll-back lw))
    ;; typing snaps the view back to the bottom
    (wm-feed wm (mouse-bytes :wheel-up 10 5))
    (wm-feed wm "x")
    (is-equal 0 (lw-scroll-back lw))))

(deftest menus
  (let* ((wm (make-wm 24 80)) (chosen nil))
    (wm-popup-menu wm '(("Alpha" . :a) ("Beta" . :b) ("Gamma" . :g)) 5 5 (lambda (v) (setq chosen v)) :title "Pick")
    (wm-draw wm)
    (is (screen-has wm "Alpha")) (is (screen-has wm "Gamma")) (is (screen-has wm "Pick"))
    (wm-feed wm (esc "[B")) (wm-feed wm (esc "[B"))
    (wm-feed wm (string (code-char 13)))
    (is-equal :g chosen)
    (is (null (wm-popups wm)))
    (wm-popup-menu wm '(("Alpha" . :a) ("Beta" . :b)) 5 5 (lambda (v) (setq chosen v)))
    (wm-feed wm "b")                             ; first-letter selection
    (wm-feed wm (string (code-char 13)))
    (is-equal :b chosen)
    (wm-popup-menu wm '(("Alpha" . :a) ("Beta" . :b)) 5 5 (lambda (v) (setq chosen v)))
    (wm-feed wm (mouse-bytes :press 8 7 0))       ; inside: item row for "Beta"
    (is-equal :b chosen)
    (setq chosen nil)
    (wm-popup-menu wm '(("Alpha" . :a)) 5 5 (lambda (v) (setq chosen v)))
    (wm-feed wm (esc ""))
    (is (null (wm-popups wm)))
    (is (null chosen))
    ;; menus near the screen edge stay on screen
    (let ((m (wm-popup-menu wm '(("One" . 1) ("Two" . 2)) 78 23 (lambda (v) v))))
      (is (<= (+ (window-x m) (window-w m)) 80))
      (is (<= (+ (window-y m) (window-h m)) 24)))))

(deftest who-line-and-resize
  (multiple-value-bind (wm lw) (make-test-wm)
    (declare (ignorable lw))
    (is (screen-has wm "Package: COMMON-LISP-USER"))
    (is (screen-has wm "Listener"))
    (wm-feed wm (mouse-bytes :move 5 5))
    (is (screen-has wm "F6 next window"))
    (wm-handle-event wm '(:resize 30 100))
    (wm-draw wm)
    (is-equal 30 (wm-rows wm))
    (is-equal 100 (wm-cols wm))
    (is (<= (+ (window-y lw) (window-h lw)) 28))
    (is (screen-has wm "Package:"))
    (wm-handle-event wm '(:resize 10 40))        ; very small terminals do not crash
    (wm-draw wm)
    (type-line wm "(+ 1 1)")
    (is (screen-has wm "=> 2"))))
