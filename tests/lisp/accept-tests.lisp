;;;; accept-tests.lisp -- ACCEPT, ACCEPTING-VALUES, presentation translators and clickable command arguments.
;;;; Headless, as in debugger-tests.lisp: a whole session is one script, and (:call fn) events act while an
;;;; ACCEPT is waiting.  Uses helpers from ui-tests.lisp and debugger-tests.lisp.

(in-package :cl)

(defun click-text (wm text &optional (dx 0))
  "Click (press and release the left button) on TEXT on the screen; returns T if it was found."
  (wm-draw wm)
  (let ((pos (screen-has wm text)))
    (when pos
      (wm-handle-event wm (list :mouse :press (+ (cdr pos) dx) (car pos) 0 0))
      (wm-handle-event wm (list :mouse :release (+ (cdr pos) dx) (car pos) 0 0))
      t)))

(defun hover-text (wm text &optional (dx 0))
  (wm-draw wm)
  (let ((pos (screen-has wm text)))
    (and pos (progn (wm-handle-event wm (list :mouse :move (+ (cdr pos) dx) (car pos) 0 0)) (wm-doc wm)))))

(deftest accept-parsing
  (is-equal 42 (accept-from-string 'integer " 42 "))
  (is (handler-case (progn (accept-from-string '(integer 1 10) "11") nil) (input-not-of-required-type () t)))
  (is-equal 10 (accept-from-string '(integer 1 10) "10"))
  (is-equal "hello world" (accept-from-string 'string "hello world"))
  (is-equal "quoted" (accept-from-string 'string "\"quoted\""))
  (is-equal t (accept-from-string 'boolean "Yes"))
  (is-equal nil (accept-from-string 'boolean "n"))
  (is-equal :green (accept-from-string '(member :red :green :blue) "gr"))
  (is (handler-case (progn (accept-from-string '(member :bar :baz) "ba") nil) (input-not-of-required-type (e) (%string-search "ambiguous" (princ-to-string e) 0))))
  (is-equal :foo (accept-from-string 'keyword ":foo"))
  (is-equal :foo (accept-from-string 'keyword "foo"))
  (is-equal 'car (accept-from-string 'symbol "car"))
  (is-equal (find-package "COMMON-LISP") (accept-from-string 'package "common-lisp"))
  (is-equal '(1 2 3) (accept-from-string '(sequence integer) "1, 2,3"))
  (is-equal nil (accept-from-string '(null-or-type integer) ""))
  (is-equal 5 (accept-from-string '(or integer string) "5"))
  (is-equal "x5" (accept-from-string '(or integer string) "x5"))
  (is-equal 1/2 (accept-from-string 'number "1/2"))
  (is-equal '(+ 1 2) (accept-from-string 'expression "(+ 1 2)"))
  (is-equal #\a (accept-from-string 'character "a"))
  (is-equal #\Space (accept-from-string 'character "Space"))
  (is (handler-case (progn (accept-from-string 'integer "#.(error \"no\")") nil) (input-not-of-required-type () t)))  ; no read-time evaluation
  (is (handler-case (progn (accept-from-string 'integer "1 2") nil) (input-not-of-required-type () t)))
  (is-equal "an integer from 1 to 10" (describe-presentation-type '(integer 1 10)))
  (is-equal "one of red, green" (describe-presentation-type '(member :red :green)))
  (is-equal "integers separated by commas" (describe-presentation-type '(sequence integer)))
  (is-equal "a symbol or an integer" (describe-presentation-type '(or symbol integer)))
  (is (presentation-typep 3 '(integer 1 5)))
  (is (not (presentation-typep 7 '(integer 1 5))))
  (is (presentation-typep "x" 'string))
  (is (presentation-typep :a '(member :a :b)))
  (is (presentation-typep nil 'boolean))
  (is-equal '("green") (accept-completions '(member :red :green) "g"))
  (is-equal '("no") (accept-completions 'boolean "n"))
  ;; translators
  (multiple-value-bind (ok obj how) (presentation-acceptable-p (make-pres '(nil mapcar (1 2)) 'frame) 'symbol)
    (is ok) (is-equal 'mapcar obj) (is-equal "its function's name" how))
  (is (not (presentation-acceptable-p (make-pres '(nil nil nil) 'frame) 'symbol)))   ; the tester declines
  (is (not (presentation-acceptable-p (make-pres 5 'integer) 'string)))
  (is (presentation-acceptable-p (make-pres 5 'integer) '(integer 0 9)))
  (is (presentation-acceptable-p (make-pres (list 1 2) 'list) t))
  (define-presentation-translator zz-number-to-string (integer string :documentation "its digits") (n) (format nil "~D" n))
  (multiple-value-bind (ok obj) (presentation-acceptable-p (make-pres 12 'integer) 'string) (is ok) (is-equal "12" obj))
  (setq *presentation-translators* (remove 'zz-number-to-string *presentation-translators* :key #'translator-name)))

(deftest accept-in-the-window-listener
  (multiple-value-bind (wm lw) (make-test-wm 40 100)
    (let ((seen nil))
      (run-script wm
        (script-lines "(+ 3 4)" "(* 2 (accept 'integer :prompt \"How many\"))")
        (at (lambda () (push (list :prompt (%prompt-string lw) :busy (wm-busy wm)) seen)))
        (script-lines "abc" "21")
        (script-lines "(list :got (accept 'integer :prompt \"Pick\" :default 5))" "")         ; empty: the default
        (script-lines "(list :clicked (accept '(integer 0 9) :prompt \"Digit\"))")
        (at (lambda () (push (list :doc (hover-text wm "=> 7" 3)) seen) (click-text wm "=> 7" 3)))
        (script-lines "(list :str (accept 'string))")
        (at (lambda () (push (list :doc (hover-text wm "=> 7" 3)) seen) (click-text wm "=> 7" 3)))  ; does not match: describes
        (script-lines "plain text"))
      (setq seen (reverse seen))
      (is-equal "How many (an integer): " (getf (first seen) :prompt))
      (is (not (getf (first seen) :busy)))
      (is (pane-has lw "abc is not an integer"))
      (is (pane-has lw "=> 42"))
      (is (pane-has lw "=> (:GOT 5)"))
      (is (%string-search "L: use this for Digit" (getf (second seen) :doc) 0))
      (is (pane-has lw "=> (:CLICKED 7)"))
      (is (pane-has lw "Digit (an integer from 0 to 9): 7"))                    ; the click is echoed
      (is (%string-search "L: Describe" (getf (third seen) :doc) 0))
      (is (pane-has lw "=> (:STR \"plain text\")"))
      (is (null (lw-accepts lw)))
      (is-equal "CL-USER> " (%prompt-string lw))))
  ;; C-g on an empty line gives up; an error while waiting enters the debugger, and the ACCEPT resumes after it
  (multiple-value-bind (wm lw) (make-test-wm 40 100)
    (run-script wm
      (script-lines "(progn (accept 'integer) (list :not-reached))")
      (list (list :key 7 0))
      (script-lines "(defun zz-acc-f (x) (+ 1 (car x)))" "(zz-acc-f 5)")
      (script-lines "(list :after (accept 'symbol :prompt \"Sym\"))")
      (at (lambda () (click-text wm "0: (ZZ-ACC-F 5)" 3))))
    (is (pane-has lw "; Back to the Listener"))
    (is (not (pane-has lw ":NOT-REACHED")))
    (is (pane-has lw "=> (:AFTER COMMON-LISP-USER::ZZ-ACC-F)"))              ; a frame, translated to its function
    (is (null (lw-accepts lw))))
  (multiple-value-bind (wm lw) (make-test-wm 40 100)
    (let ((seen nil))
      (run-script wm
        (script-lines "(list :v (accept 'integer :prompt \"V\"))")
        (at (lambda () (wm-handle-event wm (list :mouse :press 0 0 0 0))))          ; harmless
        (script-lines "(car 1)")                                               ; typed at the ACCEPT: not an integer
        (at (lambda () (push (%prompt-string lw) seen)))
        (script-lines "8"))
      (is (pane-has lw "(car 1) is not an integer"))
      (is-equal "V (an integer): " (car seen))
      (is (pane-has lw "=> (:V 8)")))))

(define-command (zz-pick :documentation "test command") ((n (integer 1 5) :prompt "Number"))
  (format t "~&picked ~D~%" n))

(deftest accept-command-arguments
  (multiple-value-bind (wm lw) (make-test-wm 40 100)
    (run-script wm (script-lines ":zz-pick 3" ":zz-pick 9"))
    (is (pane-has lw "picked 3"))
    (is (pane-has lw "9 is not an integer from 1 to 5"))
    ;; a missing argument can be clicked
    (run-script wm (script-lines "(+ 2 2)" ":zz-pick"))
    (is-equal "Number: " (%prompt-string lw))
    (is (%string-search "L: use this for Number" (hover-text wm "=> 4" 3) 0))
    (click-text wm "=> 4" 3)
    (is (pane-has lw "picked 4"))
    (is (null (lw-pending lw)))
    ;; an expression argument gets the clicked object itself (quoted)
    (run-script wm (script-lines "(list 'a 'b)" ":describe"))
    (click-text wm "=> (A B)" 3)
    (is (pane-has lw "a list of length 2"))))

(deftest accepting-values-dialog
  (multiple-value-bind (wm lw) (make-test-wm 40 100)
    (let ((seen nil))
      (run-script wm
        (script-lines "(accepting-values (*standard-output* :label \"Options\") (list (accept 'integer :prompt \"Count\" :default 3) (accept '(member :red :green :blue) :prompt \"Colour\" :default :red) (accept 'boolean :prompt \"Loud\" :default nil) (accept 'string :prompt \"Name\")))")
        (at (lambda () (wm-draw wm)
              (push (list :focus (typep (wm-focus wm) 'avv-window) :count (screen-has wm "Count: 3") :colour (screen-has wm "Colour: [red]  green  blue")
                          :loud (screen-has wm "Loud: No"))
                    seen)))
        (list (list :key 13 0) (list :key 21 0))                               ; Return opens Count; C-u clears; then 12
        (%term-parse "12")
        (script-lines "x")                                                     ; "12x" is not an integer
        (at (lambda () (wm-draw wm) (push (list :error (screen-has wm "12x is not an integer")) seen)))
        (list (list :key :backspace 0) (list :key 13 0))
        (list (list :key :down 0) (list :key 13 0))                            ; Colour: next choice
        (list (list :key :down 0) (list :key 13 0))                            ; Loud: toggles
        (list (list :key :down 0))
        (%term-parse "bob")
        (list (list :key 13 0))
        (at (lambda () (wm-draw wm) (push (list :line (screen-has wm "Count: 12") :colour (screen-has wm "Colour: red  [green]  blue")) seen)))
        (list (list :key :end 0)))
      (setq seen (reverse seen))
      (is (getf (first seen) :focus))
      (is (getf (first seen) :count))
      (is (getf (first seen) :colour))
      (is (getf (first seen) :loud))
      (is (getf (second seen) :error))
      (is (getf (third seen) :line))
      (is (getf (third seen) :colour))
      (is (pane-has lw "=> (12 :GREEN T \"bob\")"))
      (is (not (find-if (lambda (w) (typep w 'avv-window)) (wm-windows wm))))
      (is (eq (wm-focus wm) lw))))
  ;; fields can depend on others; a click on a presentation fills the selected field; Escape aborts
  (multiple-value-bind (wm lw) (make-test-wm 40 100)
    (let ((seen nil))
      (run-script wm
        (script-lines "(+ 20 5)")
        (script-lines "(accepting-values () (let ((n (accept 'integer :prompt \"N\" :default 1))) (if (> n 1) (list n (accept 'string :prompt \"Unit\" :default \"cm\")) (list n))))")
        (at (lambda () (wm-draw wm) (push (screen-has wm "Unit:") seen)))
        (at (lambda () (click-text wm "=> 25" 3)))                             ; N is selected: the click sets it
        (at (lambda () (wm-draw wm) (push (screen-has wm "Unit: cm") seen) (push (screen-has wm "N: 25") seen)))
        (list (list :key :end 0))
        (script-lines "(accepting-values () (accept 'integer :prompt \"Q\"))")
        (list (list :key :escape 0)))
      (setq seen (reverse seen))
      (is (null (first seen)))
      (is (second seen))
      (is (third seen))
      (is (pane-has lw "=> (25 \"cm\")"))
      (is (pane-has lw "; Back to the Listener")))))

(deftest accept-in-line-mode
  (let ((out (make-string-output-stream)))
    (is-equal 5 (let ((*standard-input* (make-string-input-stream (format nil "abc~%5~%"))) (*standard-output* out) (*current-listener* nil))
                  (accept 'integer :prompt "Size")))
    (let ((text (get-output-stream-string out)))
      (is (%string-search "Size (an integer): " text 0))
      (is (%string-search "abc is not an integer" text 0))))
  (is-equal :b (let ((*standard-input* (make-string-input-stream (format nil "~%"))) (*standard-output* (make-string-output-stream)) (*current-listener* nil))
                 (accept '(member :a :b) :default :b)))
  (is-equal '(2 "x") (let ((*standard-input* (make-string-input-stream (format nil "2~%x~%"))) (*standard-output* (make-string-output-stream)) (*current-listener* nil))
                       (accepting-values () (list (accept 'integer) (accept 'string)))))
  (is (handler-case (let ((*standard-input* (make-string-input-stream "")) (*standard-output* (make-string-output-stream)) (*current-listener* nil))
                      (accept 'integer) nil)
        (error () t))))
