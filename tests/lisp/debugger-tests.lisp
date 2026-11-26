;;;; debugger-tests.lisp -- the debugger in the Listener window.
;;;; Headless: input goes through the real parser.  The debugger runs a nested event loop on the same script,
;;;; so a whole session is fed as one script; (:call fn) events look at the state while a level is still active.
;;;; Uses MAKE-TEST-WM, ESC and MOUSE-BYTES from ui-tests.lisp.

(in-package :cl)

(defun %cr () (string (code-char 13)))
(defun script-lines (&rest lines) (%term-parse (apply #'%string-concat (mapcar (lambda (l) (%string-concat l (%cr))) lines))))
(defun at (fn) (list (list :call fn)))
(defun run-script (wm &rest parts) (wm-run-events wm (apply #'append parts)))
(defun pane-text (lw)
  (let ((rows (ob-layout (lw-obuf lw) 200 2000 nil)) (acc nil))
    (dotimes (i (length rows)) (setq acc (cons (apply #'%string-concat (mapcar #'first (aref rows i))) acc)))
    (%join-strings (nreverse acc) (string #\Newline))))
(defun pane-has (lw text) (and (%string-search text (pane-text lw) 0) t))

(defvar *dbg-var* :global)

(deftest window-debugger-levels-and-restarts
  (multiple-value-bind (wm lw) (make-test-wm 40 100)
    (let ((seen nil))
      (run-script wm
        (script-lines "(defun dbg-f1 (x) (+ 1 (car x)))" "(defun dbg-f2 (y) (* 2 (dbg-f1 y)))"
                      "(let ((*dbg-var* :inner)) (dbg-f2 5))")
        (at (lambda ()
              (push (list :level (length (lw-debug lw)) :prompt (%prompt-string lw)
                          :frames (mapcar #'cadr (dbg-frames (car (lw-debug lw))))
                          :restarts (mapcar #'restart-name (dbg-restarts (car (lw-debug lw)))))
                    seen)))
        (script-lines "(list :var *dbg-var*)" "(car 6)")
        (at (lambda () (push (list :level (length (lw-debug lw)) :prompt (%prompt-string lw)
                                   :restarts (mapcar #'restart-report-string (dbg-restarts (car (lw-debug lw)))))
                             seen)))
        (script-lines "0")                                      ; level 2's ABORT: back to level 1
        (at (lambda () (push (list :level (length (lw-debug lw)) :last (condition-report-string (car (lw-last-error lw)))) seen)))
        (script-lines "0" "(list :after *dbg-var*)"))
      (setq seen (reverse seen))
      (is-equal 1 (getf (first seen) :level))
      (is-equal "CL-USER debug 1> " (getf (first seen) :prompt))
      (is-equal '(dbg-f1 dbg-f2) (getf (first seen) :frames))     ; only the user's frames, innermost first
      (is-equal '(abort) (getf (first seen) :restarts))
      (is (pane-has lw "Error: The value 5 is not of type LIST"))
      (is (pane-has lw "0: Return to the Listener"))
      (is (pane-has lw "0: (DBG-F1 5)"))
      (is (pane-has lw "=> (:VAR :INNER)"))                         ; evaluated inside the binding at the error
      (is-equal 2 (getf (second seen) :level))
      (is-equal "CL-USER debug 2> " (getf (second seen) :prompt))
      (is-equal '("Return to debug level 1" "Return to the Listener") (getf (second seen) :restarts))
      (is-equal 1 (getf (third seen) :level))
      (is-equal "The value 5 is not of type LIST" (getf (third seen) :last))   ; :bt shows level 1's error again
      (is (pane-has lw "; Back to debug level 1"))
      (is (pane-has lw "; Back to the Listener"))
      (is (pane-has lw "=> (:AFTER :GLOBAL)"))
      (is (null (lw-debug lw)))
      (is-equal "CL-USER> " (%prompt-string lw))
      (is (not (wm-busy wm))))))

(deftest window-debugger-restart-values-break-cerror
  (multiple-value-bind (wm lw) (make-test-wm 40 100)
    (run-script wm
      (script-lines "(restart-case (error \"pick\") (use-it (v) (* v 2)))" "0" "0 21"
                    "(restart-case (error \"plain\") (skip () :skipped))" "0 5" "0"
                    "(+ 1 (or (break \"hold ~D\" 3) 41))" ":continue"
                    "(progn (cerror \"Go on\" \"cerr ~D\" 1) :went-on)" ":restarts" "0"
                    "(error \"again\")" "7" ":continue" ":abort"))
    (is (pane-has lw "; Restart 0 needs a value: type 0 and a form, as in 0 42"))
    (is (pane-has lw "=> 42"))
    (is (pane-has lw "; Restart 0 takes no value: type just 0"))
    (is (pane-has lw "=> :SKIPPED"))
    (is (pane-has lw "Break: hold 3"))
    (is (pane-has lw "0: Return from BREAK"))
    (is (pane-has lw "=> 42"))
    (is (pane-has lw "Error: cerr 1"))
    (is (pane-has lw "0: Go on"))
    (is (pane-has lw "=> :WENT-ON"))
    (is (pane-has lw "; There is no restart 7"))
    (is (pane-has lw "There is no CONTINUE restart here"))
    (is (null (lw-debug lw)))))

(deftest window-debugger-clicks-and-other-windows
  (multiple-value-bind (wm lw) (make-test-wm 40 100)
    (let ((lw2 (make-instance 'listener-window :title "Listener 2")) (state nil))
      (wm-add-window wm lw2)
      (wm-tile wm :horizontal)
      (wm-raise wm lw)
      (run-script wm
        (script-lines "(error \"first\")")
        (list (list :key :f6 0))                              ; to the other Listener: it still works
        (script-lines "(+ 40 2)" "(error \"second\")")
        (at (lambda () (push (list (length (lw-debug lw)) (length (lw-debug lw2))) state)))
        (list (list :key :f6 0))
        (script-lines "0")                                    ; the first window resumes: the second is unwound too
        (at (lambda () (push (list (length (lw-debug lw)) (length (lw-debug lw2))) state))))
      (setq state (reverse state))
      (is (pane-has lw2 "=> 42"))
      (is-equal '(1 1) (first state))
      (is-equal '(0 0) (second state))
      (is (pane-has lw2 "; Debug level 1 left: a restart was chosen in another window"))
      (is (pane-has lw "; Back to the Listener")))
    ;; clicking a restart invokes it; a restart of a level that has ended is refused
    (let ((old nil))
      (run-script wm
        (script-lines ":clear screen" "(restart-case (error \"click\") (take () :taken))")
        (at (lambda ()
              (wm-draw wm)
              (setq old (car (dbg-restarts (car (lw-debug lw)))))
              (let ((pos (screen-has wm "0: TAKE")))
                (wm-handle-event wm (list :mouse :press (+ (cdr pos) 1) (car pos) 0 0))))))
      (is (pane-has lw "=> :TAKEN"))
      (is (null (lw-debug lw)))
      (run-script wm (at (lambda () (%run-pcmd lw (default-pcommand 'restart) old))))
      (is (pane-has lw "; That restart is no longer active")))))

(deftest window-debugger-exits
  ;; quitting the window system from the debugger leaves every level
  (multiple-value-bind (wm lw) (make-test-wm 40 100)
    (run-script wm (script-lines "(car 5)") (list (list :key :f10 0) (list :key 121 0)))
    (is (wm-quit wm))
    (is (null (lw-debug lw))))
  ;; headless, the end of the script leaves the debugger
  (multiple-value-bind (wm lw) (make-test-wm 40 100)
    (run-script wm (script-lines "(car 5)"))
    (is (null (lw-debug lw)))
    (is (not (wm-busy wm)))
    (run-script wm (script-lines "(+ 1 2)"))
    (is (pane-has lw "=> 3")))
  ;; with the debugger off, or outside the event loop, an error is only reported
  (multiple-value-bind (wm lw) (make-test-wm 40 100)
    (let ((*debugger-enabled* nil))
      (run-script wm (script-lines "(car 5)" "(+ 2 3)")))
    (is (pane-has lw "Error: The value 5 is not of type LIST"))
    (is (not (pane-has lw "debug level 1")))
    (is (pane-has lw "=> 5"))
    (multiple-value-bind (vals ok) (%in-listener lw (lambda () (car 7)))
      (is (null vals)) (is (not ok)))
    (is (pane-has lw "The value 7 is not of type LIST")))
  ;; levels are limited
  (multiple-value-bind (wm lw) (make-test-wm 40 100)
    (let ((deepest 0))
      (run-script wm (script-lines "(car 1)" "(car 2)" "(car 3)" "(car 4)" "(car 5)" "(car 6)")
                  (at (lambda () (setq deepest (length (lw-debug lw))))))
      (is-equal *window-debug-limit* deepest))
    (is (null (lw-debug lw))))
  ;; ABORT in code typed at the Listener returns to it, instead of leaving the window system
  (multiple-value-bind (wm lw) (make-test-wm 40 100)
    (run-script wm (script-lines "(progn (abort) :not-here)" "(+ 1 1)"))
    (is (pane-has lw "; Back to the Listener"))
    (is (not (pane-has lw ":NOT-HERE")))
    (is (pane-has lw "=> 2"))
    (is (not (wm-quit wm)))))

(deftest restart-arguments
  (is-equal '(:required :none :optional)
            (restart-case (let ((rs (compute-restarts)))
                            (mapcar #'restart-arguments (list (find-restart 'a) (find-restart 'b) (find-restart 'c))))
              (a (x) x) (b () nil) (c (&optional y) y)))
  (is-equal :unknown (restart-bind ((r (lambda () nil))) (restart-arguments (find-restart 'r)))))
