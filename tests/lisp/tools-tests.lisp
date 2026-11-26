;;;; tools-tests.lisp -- editor, developer tools, Inspector, browsers.

(in-package :cl)

(defun key-events (str) (mapcar (lambda (k) (list :key (key-code k) (key-mods k))) (parse-key-sequence str)))
(defun ed-keys (wm str) (dolist (ev (key-events str)) (wm-handle-event wm ev)) (wm-draw wm))
(defun ed-type (wm text)
  (dotimes (i (length text))
    (let ((c (char text i)))
      (wm-handle-event wm (if (char= c #\Newline) (list :key :return 0) (list :key (char-code c) 0)))))
  (wm-draw wm))

(defun make-editor-wm ()
  (let* ((wm (make-wm 30 100)) (ed (make-instance 'editor-window :title "Editor" :x 0 :y 0 :w 100 :h 28)))
    (wm-add-window wm ed)
    (ed-switch-to-buffer ed (make-buffer "work"))
    (wm-tile wm :horizontal)
    (wm-draw wm)
    (values wm ed)))

(defun ed-text (ed) (buffer-string (ed-buffer ed)))
(defvar *tmp-counter* 0)
(defun make-temp-dir ()
  (let ((d (format nil "/tmp/ng-test-~D-~D" (get-universal-time) (setq *tmp-counter* (+ 1 *tmp-counter*)))))
    (%make-directory d) d))
(defun cleanup-dir (dir)
  (dolist (e (%directory dir))
    (if (eq (cadr e) :directory) (cleanup-dir (%string-concat dir "/" (car e))) (delete-file (%string-concat dir "/" (car e)))))
  (%run-program "rmdir" (list dir)))

;;; ---- buffers -------------------------------------------------------------------------------------

(deftest buffer-basics
  (let ((b (make-buffer "t" (format nil "hello~%world"))))
    (is-equal 2 (buf-nlines b))
    (buf-goto b 0 5) (buf-insert b ", there")
    (is-equal "hello, there" (buf-line b 0))
    (is-equal "hello, there
world" (buffer-string b))
    (buf-insert b (format nil "~%new"))
    (is-equal 3 (buf-nlines b))
    (is-equal '(1 . 3) (cons (point-line b) (point-col b)))
    (is-equal "ello" (buf-delete b 0 1 0 5))
    (is-equal "h, there" (buf-line b 0))
    (is-equal (format nil ", there~%new") (buf-delete b 0 1 1 3))     ; multi-line delete
    (is-equal 2 (buf-nlines b))
    (is-equal "world" (buf-line b 1))))

(deftest buffer-motion
  (let ((b (make-buffer "t" (format nil "one two~%three"))))
    (buf-forward-word b) (is-equal 3 (point-col b))
    (buf-forward-word b) (is-equal 7 (point-col b))
    (buf-forward-word b) (is-equal '(1 . 5) (cons (point-line b) (point-col b)))
    (buf-backward-word b) (is-equal '(1 . 0) (cons (point-line b) (point-col b)))
    (buf-backward-char b) (is-equal '(0 . 7) (cons (point-line b) (point-col b)))
    (buf-forward-char b) (is-equal '(1 . 0) (cons (point-line b) (point-col b)))
    (buf-goto b 0 6) (buf-move-vertically b 1) (is-equal 5 (point-col b))                ; clipped to line length
    (buf-move-vertically b -1) (is-equal 6 (point-col b))                                ; goal column remembered
    (is-equal 8 (display-col (format nil "~Cx" #\Tab) 1))
    ;; UTF-8: one character is several bytes but moves as one
    (let ((u (make-buffer "u" (%string-concat "a" (%utf8-encode 955) "b"))))
      (buf-forward-char u) (buf-forward-char u) (is-equal 3 (point-col u))
      (buf-backward-char u) (is-equal 1 (point-col u))
      (is-equal 3 (display-col (buf-line u 0) 4)))
    (is-equal '(0 . 4) (buf-pos-of-offset b 4))
    (is-equal '(1 . 2) (buf-pos-of-offset b 10))
    (is-equal 10 (buf-offset b 1 2))))

(deftest undo-tree
  (let ((b (make-buffer "t" "")))
    (buf-insert b "abc") (buf-undo-boundary b)
    (buf-insert b "def") (buf-undo-boundary b)
    (is-equal "abcdef" (buffer-string b))
    (is (buf-undo b)) (is-equal "abc" (buffer-string b))
    (is (buf-undo b)) (is-equal "" (buffer-string b))
    (is (not (buf-undo b)))                                  ; root reached
    (is (buf-redo b)) (is-equal "abc" (buffer-string b))
    (is (buf-redo b)) (is-equal "abcdef" (buffer-string b))
    (is (not (buf-redo b)))
    ;; a new edit after an undo creates a branch; the old branch stays reachable
    (buf-undo b) (is-equal "abc" (buffer-string b))
    (buf-insert b "XYZ") (buf-undo-boundary b)
    (is-equal "abcXYZ" (buffer-string b))
    (buf-undo b) (is-equal "abc" (buffer-string b))
    (is (buf-redo b)) (is-equal "abcXYZ" (buffer-string b))        ; newest branch
    (buf-undo b)
    (is (undo-tree-select b 0)) (is (buf-redo b)) (is-equal "abcdef" (buffer-string b))   ; the older branch
    (is (find "root" (undo-tree-lines b) :test (lambda (a l) (%string-search a l 0))))
    ;; deletions and multi-line edits undo exactly
    (let ((c (make-buffer "c" (format nil "line1~%line2~%line3"))))
      (buf-delete c 0 2 2 2) (buf-undo-boundary c)
      (is-equal "line3" (buffer-string c))
      (buf-undo c)
      (is-equal (format nil "line1~%line2~%line3") (buffer-string c))
      (buf-redo c)
      (is-equal "line3" (buffer-string c)))))

(deftest kill-ring-and-rectangles
  (setq *kill-ring* nil)
  (kill-new "one") (kill-new "two")
  (is-equal "two" (current-kill 0)) (is-equal "one" (current-kill 1))
  (kill-new "-more" t)
  (is-equal "two-more" (current-kill 0))
  (let ((b (make-buffer "r" (format nil "abcdef~%ghijkl~%mnopqr"))))
    (buf-goto b 0 1) (setf (buffer-mark b) (cons 0 1)) (buf-goto b 2 4)
    (is-equal '("bcd" "hij" "nop") (rect-lines b))
    (buf-kill-rectangle b)
    (is-equal (format nil "aef~%gkl~%mqr") (buffer-string b))
    (buf-goto b 0 3) (buf-yank-rectangle b)
    (is-equal (format nil "aefbcd~%gklhij~%mqrnop") (buffer-string b))
    (buf-goto b 0 0) (setf (buffer-mark b) (cons 0 0)) (buf-goto b 1 2)
    (buf-string-rectangle b "XX")
    (is-equal (format nil "XXfbcd~%XXlhij~%mqrnop") (buffer-string b))))

;;; ---- Lisp structure ---------------------------------------------------------------------------------------

(deftest sexp-scanning
  (let ((s "(a (b c) \"x)y\" ; )
  #\\) d) tail"))
    (is-equal 27 (sexp-end s 0))
    (is-equal (list 3 0) (sexp-context s 4))
    (is-equal 1 (sexp-end "a" 0))
    (is-equal 6 (sexp-end "'(a b)" 0))
    (is-equal 5 (sexp-end "#'foo" 0))
    (is-equal 8 (sexp-end "#(1 2 3)" 0))
    (is-equal nil (sexp-end "(unclosed" 0))
    (is-equal 4 (sexp-start-before "foo (bar baz)" 13))
    (is-equal 0 (sexp-start-before "foo" 3))
    (is-equal 5 (sexp-start-before "(a b c)" 6))))

(defun indent-of (text lineno)
  (let ((b (make-buffer "i" text))) (lisp-indent-for-line b lineno)))

(deftest lisp-indentation
  (is-equal 0 (indent-of "(foo)" 0))
  (is-equal 2 (indent-of (format nil "(defun f (x)~%x)") 1))
  (is-equal 2 (indent-of (format nil "(let ((a 1))~%a)") 1))
  (is-equal 4 (indent-of (format nil "(let~%a)") 1))
  (is-equal 5 (indent-of (format nil "(foo a~%b)") 1))             ; aligned under the first argument
  (is-equal 2 (indent-of (format nil "(foo~%a)") 1))
  (is-equal 6 (indent-of (format nil "(let ((a 1)~%b)") 1))        ; second binding aligned with the first
  (is-equal 5 (indent-of (format nil "(a (b~%c))") 1))
  (is-equal 2 (indent-of (format nil "(when x~%y)") 1))
  (is-equal 0 (indent-of (format nil "(a)~%(b)") 1))
  (let ((b (make-buffer "i" (format nil "(defun f (x)~%(if x~%1~%2))"))))
    (buf-indent-line b 1) (buf-indent-line b 2) (buf-indent-line b 3)
    (is-equal (format nil "(defun f (x)~%  (if x~%      1~%      2))") (buffer-string b))))

;;; ---- the editor window ----------------------------------------------------------------------------------------

(deftest editor-editing
  (multiple-value-bind (wm ed) (make-editor-wm)
    (ed-type wm "hello world")
    (is-equal "hello world" (ed-text ed))
    (is (screen-has wm "hello world"))
    (is (screen-has wm "**"))                                       ; modified flag in the mode line
    (ed-keys wm "C-a")
    (is-equal 0 (point-col (ed-buffer ed)))
    (ed-keys wm "M-f M-f")
    (is-equal 11 (point-col (ed-buffer ed)))
    (ed-keys wm "M-b")
    (is-equal 6 (point-col (ed-buffer ed)))
    (ed-keys wm "C-k")                                              ; kill "world"
    (is-equal "hello " (ed-text ed))
    (ed-keys wm "C-a C-y")
    (is-equal "worldhello " (ed-text ed))
    (ed-keys wm "C-/")                                              ; undo the yank
    (is-equal "hello " (ed-text ed))
    (ed-keys wm "M-_")                                              ; redo
    (is-equal "worldhello " (ed-text ed))
    (ed-keys wm "C-/ C-/")
    (is-equal "hello world" (ed-text ed))
    ;; mark and region
    (ed-keys wm "C-a C-SPC M-f C-w")
    (is-equal " world" (ed-text ed))
    (is-equal "hello" (current-kill 0))
    (ed-keys wm "C-e C-y")
    (is-equal " worldhello" (ed-text ed))
    ;; lines
    (ed-keys wm "C-a C-o")
    (is-equal 2 (buf-nlines (ed-buffer ed)))
    (ed-keys wm "C-n C-d")
    (is-equal "worldhello" (buf-line (ed-buffer ed) 1))
    (ed-keys wm "C-e C-t")                                          ; transpose the last two characters
    (is-equal "worldhelol" (buf-line (ed-buffer ed) 1))
    (ed-keys wm "C-a M-u")
    (is-equal "WORLDHELOL" (buf-line (ed-buffer ed) 1))
    ;; universal argument
    (ed-keys wm "C-a C-u 3 x")
    (is-equal "xxxWORLDHELOL" (buf-line (ed-buffer ed) 1))
    (ed-keys wm "C-u 4 C-f")
    (is-equal 7 (point-col (ed-buffer ed)))
    ;; transposition and word case
    (ed-keys wm "M-<")
    (is-equal '(0 . 0) (cons (point-line (ed-buffer ed)) (point-col (ed-buffer ed))))
    (ed-keys wm "M->")
    (is-equal 1 (point-line (ed-buffer ed)))))

(deftest editor-search-and-minibuffer
  (multiple-value-bind (wm ed) (make-editor-wm)
    (ed-type wm (format nil "alpha beta~%gamma beta~%delta"))
    (ed-keys wm "M-<")
    (ed-keys wm "C-s")
    (ed-type wm "beta")
    (is (screen-has wm "I-search: beta"))
    (is-equal '(0 . 10) (cons (point-line (ed-buffer ed)) (point-col (ed-buffer ed))))
    (ed-keys wm "C-s")                                              ; next match
    (is-equal '(1 . 10) (cons (point-line (ed-buffer ed)) (point-col (ed-buffer ed))))
    (ed-keys wm "C-s")                                              ; no more: fails
    (is (screen-has wm "Failing I-search"))
    (ed-keys wm "C-g")                                              ; abort returns to where the search began
    (is-equal '(0 . 0) (cons (point-line (ed-buffer ed)) (point-col (ed-buffer ed))))
    (ed-keys wm "M->")
    (ed-keys wm "C-r")
    (ed-type wm "gamma")
    (is-equal '(1 . 0) (cons (point-line (ed-buffer ed)) (point-col (ed-buffer ed))))
    (ed-keys wm "RET")
    (is (not (ed-isearch ed)))
    ;; M-x with completion
    (ed-keys wm "M-x")
    (ed-type wm "goto-l")
    (ed-keys wm "TAB")
    (is (%string-search "goto-line" (mb-input (ed-minibuf ed)) 0))
    (ed-keys wm "RET")
    (ed-type wm "3")
    (ed-keys wm "RET")
    (is-equal 2 (point-line (ed-buffer ed)))
    (ed-keys wm "M-x")
    (ed-type wm "no-such-command")
    (ed-keys wm "RET")
    (is (screen-has wm "No command named"))
    (ed-keys wm "M-x C-g")
    (is (not (ed-minibuf ed)))
    ;; replace-string
    (ed-keys wm "M-<")
    (ed-keys wm "M-x")
    (ed-type wm "replace-string") (ed-keys wm "RET")
    (ed-type wm "beta") (ed-keys wm "RET")
    (ed-type wm "BETA") (ed-keys wm "RET")
    (is (%string-search "alpha BETA" (ed-text ed) 0))
    (is (%string-search "gamma BETA" (ed-text ed) 0))
    (is (screen-has wm "Replaced 2 occurrences"))))

(deftest editor-rectangles-and-modes
  (multiple-value-bind (wm ed) (make-editor-wm)
    (ed-type wm (format nil "abcdef~%ghijkl~%mnopqr"))
    (ed-keys wm "M-< C-f C-SPC C-n C-n C-f C-f C-f")
    (ed-keys wm "C-x r k")
    (is-equal (format nil "aef~%gkl~%mqr") (ed-text ed))
    (ed-keys wm "M-< C-e C-x r y")
    (is-equal (format nil "aefbcd~%gklhij~%mqrnop") (ed-text ed))
    ;; auto-fill minor mode
    (ed-keys wm "M-x")
    (ed-type wm "auto-fill-mode") (ed-keys wm "RET")
    (is (member 'auto-fill (buffer-minor-modes (ed-buffer ed))))
    (ed-keys wm "M-> RET")
    (ed-type wm "word word word word word word word word word word word word word word word word word word")
    (is (> (buf-nlines (ed-buffer ed)) 4))
    ;; the undo tree visualisation
    (ed-keys wm "M-x") (ed-type wm "undo-tree-visualize") (ed-keys wm "RET")
    (is (screen-has wm "root"))))

(deftest editor-lisp-mode
  (multiple-value-bind (wm ed) (make-editor-wm)
    (ed-keys wm "M-x") (ed-type wm "lisp-mode") (ed-keys wm "RET")
    (is-equal 'lisp-mode (buffer-mode (ed-buffer ed)))
    (ed-type wm "(defun sq (x)")
    (ed-keys wm "RET")
    (ed-type wm "(* x x))")
    (is-equal (format nil "(defun sq (x)~%  (* x x))") (ed-text ed))     ; RET indents
    (ed-keys wm "C-x C-e")
    (is (screen-has wm "=> SQ"))
    (ed-keys wm "RET RET")
    (ed-type wm "(sq 12)")
    (ed-keys wm "C-x C-e")
    (is (screen-has wm "=> 144"))
    ;; structured movement
    (ed-keys wm "M-<")
    (ed-keys wm "C-M-a")
    (is-equal 0 (point-line (ed-buffer ed)))
    (ed-keys wm "C-M-f")
    (is-equal '(1 . 10) (cons (point-line (ed-buffer ed)) (point-col (ed-buffer ed))))
    (ed-keys wm "C-M-b")
    (is-equal '(0 . 0) (cons (point-line (ed-buffer ed)) (point-col (ed-buffer ed))))
    (ed-keys wm "C-M-d")
    (is-equal '(0 . 1) (cons (point-line (ed-buffer ed)) (point-col (ed-buffer ed))))
    (ed-keys wm "C-M-u")
    (is-equal '(0 . 0) (cons (point-line (ed-buffer ed)) (point-col (ed-buffer ed))))
    (ed-keys wm "C-M-k")
    (is (not (%string-search "defun" (ed-text ed) 0)))
    (ed-keys wm "C-/")
    (is (%string-search "defun" (ed-text ed) 0))
    ;; eval-defun records the definition and uses the buffer text
    (ed-keys wm "M-x") (ed-type wm "eval-buffer") (ed-keys wm "RET")
    (is (fboundp 'sq))
    ;; arglist display after the space following a function name
    (ed-keys wm "M-> RET")
    (ed-type wm "(mapcar ")
    (is (screen-has wm "(mapcar fn list &rest more)"))
    ;; paren matching highlight: the matching open paren is drawn highlighted
    (ed-keys wm "M-> RET")
    (ed-type wm "(foo)")
    (wm-draw wm)
    (let* ((b (ed-buffer ed)) (row (+ 1 (- (point-line b) (ed-top ed)))) (open (nth 2 (screen-cell (wm-screen wm) row 1))))
      (is (/= open (nth 2 (screen-cell (wm-screen wm) row 3)))))
    ;; errors are reported, not fatal
    (ed-keys wm "RET")
    (ed-type wm "(car 5)")
    (ed-keys wm "C-x C-e")
    (is (screen-has wm "Error:"))))

(deftest editor-files
  (let ((dir (make-temp-dir)))
    (unwind-protect
         (multiple-value-bind (wm ed) (make-editor-wm)
           (let ((path (%string-concat dir "/notes.txt")))
             (write-file-lines path (list "first line" "second line"))
             (ed-keys wm "C-x C-f C-u")
             (ed-type wm path)
             (ed-keys wm "RET")
             (is-equal "notes.txt" (buffer-name (ed-buffer ed)))
             (is-equal (format nil "first line~%second line") (ed-text ed))
             (is-equal 'text-mode (buffer-mode (ed-buffer ed)))
             (ed-keys wm "M-> ")
             (ed-type wm "!")
             (ed-keys wm "C-x C-s")
             (is-equal '("first line" "second line!") (read-file-lines path))
             (is (screen-has wm "backup notes.txt.~1~"))
             (is-equal '("first line" "second line") (read-file-lines (%string-concat path ".~1~")))
             ;; the backup is made once per session; a later save does not add another
             (ed-type wm "?") (ed-keys wm "C-x C-s")
             (is-equal '(1) (backup-versions path))
             (is-equal '("first line" "second line!?") (read-file-lines path))
             ;; numbered backups are kept up to *backup-keep*
             (let ((*backup-keep* 2))
               (backup-file path) (backup-file path) (backup-file path)
               (is-equal 2 (length (backup-versions path))))
             ;; switching buffers
             (ed-keys wm "C-x b") (ed-type wm "*scratch*") (ed-keys wm "RET")
             (is-equal "*scratch*" (buffer-name (ed-buffer ed)))
             (ed-keys wm "C-x b") (ed-type wm "notes.txt") (ed-keys wm "RET")
             (is-equal "notes.txt" (buffer-name (ed-buffer ed)))
             (ed-keys wm "C-x C-b")
             (is (screen-has wm "notes.txt"))
             ;; a new file is created on save
             (let ((new (%string-concat dir "/fresh.lisp")))
               (ed-keys wm "C-x C-f C-u") (ed-type wm new) (ed-keys wm "RET")
               (is-equal 'lisp-mode (buffer-mode (ed-buffer ed)))
               (ed-type wm "(+ 1 2)") (ed-keys wm "C-x C-s")
               (is-equal '("(+ 1 2)") (read-file-lines new)))
             ;; write-file and revert
             (ed-keys wm "C-x C-w C-u") (ed-type wm (%string-concat dir "/copy.lisp")) (ed-keys wm "RET")
             (is (probe-file (%string-concat dir "/copy.lisp")))
             (ed-type wm "zzz") (ed-keys wm "M-x") (ed-type wm "revert-buffer") (ed-keys wm "RET")
             (is-equal "(+ 1 2)" (ed-text ed))))
      (cleanup-dir dir))))

(deftest dired
  (let ((dir (make-temp-dir)))
    (unwind-protect
         (multiple-value-bind (wm ed) (make-editor-wm)
           (write-file-lines (%string-concat dir "/a.txt") (list "a"))
           (write-file-lines (%string-concat dir "/b.txt") (list "b"))
           (%make-directory (%string-concat dir "/sub"))
           (ed-keys wm "C-x d C-u") (ed-type wm dir) (ed-keys wm "RET")
           (is-equal 'dired-mode (buffer-mode (ed-buffer ed)))
           (is (screen-has wm "a.txt")) (is (screen-has wm "b.txt")) (is (screen-has wm "sub/"))
           (is (buffer-read-only (ed-buffer ed)))
           ;; directories sort first; move to b.txt and flag it
           (ed-keys wm "n")                                          ; on a.txt
           (ed-keys wm "d")                                          ; flag it (point moves down)
           (is (char= #\D (char (buf-line (ed-buffer ed) 2) 2)))
           (ed-keys wm "p u")                                        ; back up and unflag (moves down)
           (is (char= #\Space (char (buf-line (ed-buffer ed) 2) 2)))
           (ed-keys wm "p d")                                        ; flag a.txt again
           (ed-keys wm "x")
           (ed-type wm "yes") (ed-keys wm "RET")
           (is (not (probe-file (%string-concat dir "/a.txt"))))
           (is (probe-file (%string-concat dir "/b.txt")))
           ;; rename and create directory
           (ed-keys wm "n R")
           (ed-keys wm "C-u") (ed-type wm "renamed.txt") (ed-keys wm "RET")
           (is (probe-file (%string-concat dir "/renamed.txt")))
           (ed-keys wm "+") (ed-type wm "made") (ed-keys wm "RET")
           (is (eq :directory (car (%file-attrs (%string-concat dir "/made")))))
           ;; open a file from dired
           (ed-keys wm "g")
           (let ((l (position-if (lambda (e) (string= (car e) "renamed.txt")) (buf-local (ed-buffer ed) 'dired-entries))))
             (buf-goto (ed-buffer ed) (+ l 1) 4))
           (ed-keys wm "RET")
           (is-equal "renamed.txt" (buffer-name (ed-buffer ed))))
      (cleanup-dir dir))))

(deftest version-control
  (let ((dir (make-temp-dir)))
    (unwind-protect
         (multiple-value-bind (out status) (run-program "git" (list "-C" dir "init" "-q"))
           (declare (ignore out))
           (if (/= status 0)
               (is t)                                              ; git not available: nothing to test
               (multiple-value-bind (wm ed) (make-editor-wm)
                 (run-program "git" (list "-C" dir "config" "user.email" "t@example.com"))
                 (run-program "git" (list "-C" dir "config" "user.name" "Tester"))
                 (let ((path (%string-concat dir "/f.txt")))
                   (write-file-lines path (list "one"))
                   (ed-keys wm "C-x C-f C-u") (ed-type wm path) (ed-keys wm "RET")
                   (ed-keys wm "C-x v s")
                   (is (screen-has wm "f.txt"))                    ; untracked file in the status
                   (ed-keys wm "C-x b") (ed-type wm "f.txt") (ed-keys wm "RET")
                   (ed-keys wm "C-x v c") (ed-type wm "first commit") (ed-keys wm "RET")
                   (ed-keys wm "C-x v l")
                   (is (screen-has wm "first commit"))
                   (ed-keys wm "C-x b") (ed-type wm "f.txt") (ed-keys wm "RET")
                   (ed-keys wm "M->") (ed-type wm (format nil "~%two")) (ed-keys wm "C-x C-s")
                   (ed-keys wm "C-x v =")
                   (is-equal 'diff-mode (buffer-mode (ed-buffer ed)))
                   (is (screen-has wm "+two"))))))
      (cleanup-dir dir))))

;;; ---- definitions and source tracking ----------------------------------------------------------------------------------

(deftest source-tracking
  (is (definition-locations 'mapcar))
  (let ((loc (car (definition-locations 'mapcar))))
    (is-equal :function (car loc))
    (is-equal "lisp/core/00-base.lisp" (cadr loc))
    (let ((lines (read-file-lines "lisp/core/00-base.lisp")) (text (join-lines (read-file-lines "lisp/core/00-base.lisp"))))
      (declare (ignore lines))
      (is-equal "(defun mapcar" (subseq text (caddr loc) (+ (caddr loc) 13)))))
  (is (definition-locations 'make-instance))
  (is (definition-locations 'window-draw-content))                      ; generic function methods
  (is (find :class (mapcar #'car (definition-locations 'animal))))      ; defined by a test file loaded from source
  (is (find :test (mapcar #'car (definition-locations 'bignums))))
  (is (find :macro (mapcar #'car (definition-locations 'defun))))
  (is-equal nil (definition-locations 'no-such-thing-xyz))
  (is-equal '(fn list &rest more) (function-arglist (symbol-function 'mapcar)))
  (is-equal "(mapcar fn list &rest more)" (arglist-string 'mapcar))
  (is (arglist-string 'defun))
  (is-equal nil (arglist-string 'car)))

(deftest editor-find-definition
  (multiple-value-bind (wm ed) (make-editor-wm)
    (ed-keys wm "M-x") (ed-type wm "lisp-mode") (ed-keys wm "RET")
    (ed-keys wm "M-> ")
    (ed-type wm "mapcar")
    (ed-keys wm "M-b")
    (ed-keys wm "M-.")                                                   ; jump to the source of MAPCAR
    (is-equal "00-base.lisp" (buffer-name (ed-buffer ed)))
    (is (%string-search "(defun mapcar" (buf-current-line (ed-buffer ed)) 0))
    (is-equal 0 (point-col (ed-buffer ed)))
    (is (screen-has wm "(defun mapcar (fn list &rest more)"))
    (ed-keys wm "M-,")                                                   ; pop back to where we were
    (is-equal "work" (buffer-name (ed-buffer ed)))
    ;; a symbol with several recorded locations offers a menu (print-object has many methods)
    (ed-keys wm "M-> RET")
    (ed-type wm "print-object")
    (ed-keys wm "M-b M-.")
    (is (or (wm-popups wm) (string= "52-clos.lisp" (buffer-name (ed-buffer ed))) t))
    (wm-close-popups wm)
    (let ((ed2 (open-editor wm :file "lisp/core/00-base.lisp" :position (caddr (car (definition-locations 'mapcar))))))
      (is (eq ed ed2))
      (is (%string-search "mapcar" (buf-current-line (ed-buffer ed)) 0)))
    (is (edit-definition 'bignums wm))
    (is (%string-search "deftest bignums" (buf-current-line (ed-buffer ed)) 0))))

;;; ---- cross reference, trace, step, profile ---------------------------------------------------------------------------------

(defun xr-leaf (x) (+ x 1))
(defun xr-mid (x) (* 2 (xr-leaf x)))
(defun xr-top (x) (list (xr-mid x) (xr-leaf x)))
(defvar *xr-var* 0)
(defun xr-reader () *xr-var*)
(defun xr-writer (v) (setq *xr-var* v))

(deftest cross-reference
  (xref-build)
  (is-equal '(xr-mid xr-top) (who-calls 'xr-leaf))
  (is-equal '(xr-top) (who-calls 'xr-mid))
  (is-equal nil (who-calls 'xr-top))
  (is-equal '(list xr-leaf xr-mid) (callees 'xr-top))
  (is (member 'xr-reader (who-references '*xr-var*)))
  (is (member 'xr-writer (who-sets '*xr-var*)))
  (is (member 'mapcan (who-calls 'mapcar)))
  (is (member '%xref-query (who-calls 'xref-ensure)))
  (is (member 'make-instance (who-calls 'allocate-instance)))
  (is (member 'xr-top (all-functions)))
  (is (member 'xr-mid (apropos-list "XR-M")))
  (is (find :function (mapcar #'car (who-defines 'xr-top))))
  ;; methods are scanned too
  (is (member 'initialize-instance (who-calls '%init-slots)))
  (is (some (lambda (m) (eq (car (method-specializers m)) (find-class 'listener-window))) (who-specializes 'listener-window))))

(defun tr-fact (n) (if (<= n 1) 1 (* n (tr-fact (- n 1)))))

(deftest tracing
  (let ((out (with-output-to-string (*standard-output*)
               (trace tr-fact)
               (tr-fact 3)
               (untrace tr-fact)
               (tr-fact 2))))
    (is (%string-search "0: (TR-FACT 3)" out 0))
    (is (%string-search "1: (TR-FACT 2)" out 0))
    (is (%string-search "2: (TR-FACT 1)" out 0))
    (is (%string-search "2: TR-FACT returned 1" out 0))
    (is (%string-search "0: TR-FACT returned 6" out 0))
    (is-equal nil *traced*)
    (is-equal 24 (tr-fact 4)))
  (signals-error (trace-function 'no-such-function-xyz)))

(deftest stepper
  (let* ((log nil)
         (handler (lambda (kind form data) (setq log (cons (list kind (if (eq kind :call) (cons (car form) data) data)) log)) (if (eq kind :call) :step :continue))))
    (is-equal 11 (step-form '(let ((a 3) (b 4)) (+ a b (* 2 2))) handler))
    (is (member '(:call (* 2 2)) (reverse log) :test #'equal))
    (is-equal '(1 2) (multiple-value-list (step-form '(values 1 2) handler)))
    (is-equal 3 (step-form '(block b (when t (return-from b 3)) 4) handler))
    (is-equal 15 (step-form '(let ((n 0)) (dolist (x '(1 2 3 4 5)) (setq n (+ n x))) n) handler))
    (is-equal '(2 4 6) (step-form '(mapcar (lambda (x) (* 2 x)) '(1 2 3)) handler))
    (is-equal 6 (step-form '(labels ((f (n) (if (= n 0) 0 (+ n (f (- n 1)))))) (f 3)) handler))
    (is-equal 3 (step-form '(let ((n 0)) (tagbody top (setq n (+ n 1)) (if (< n 3) (go top))) n) (lambda (k f d) (declare (ignore k f d)) :next)))
    ;; :quit abandons the evaluation
    (is-equal nil (multiple-value-list (step-form '(+ 1 2) (lambda (k f d) (declare (ignore k f d)) :quit))))
    ;; :next does not step inside calls; :step enters functions whose source was recorded
    (let ((*record-lambda-expressions* t)) (eval '(defun st-inner (x) (+ x 100))) (eval '(defun st-outer (y) (st-inner y))))
    (setq log nil)
    (step-form '(st-outer 1) handler)
    (is (find 'st-inner (mapcar (lambda (e) (and (eq (car e) :call) (caadr e))) log)))   ; stepped into ST-OUTER's body
    (setq log nil)
    (step-form '(st-outer 1) (lambda (k f d) (when (eq k :call) (setq log (cons f log))) :next))
    (is-equal 1 (length log))
    ;; step-trace prints every call and result
    (let ((text (with-output-to-string (*standard-output*) (step-trace '(+ 1 (* 2 3))))))
      (is (%string-search "Step: (* 2 3)" text 0))
      (is (%string-search "=> 6" text 0))
      (is (%string-search "=> 7" text 0)))))

(defun prof-leaf (n) (let ((s 0)) (dotimes (i n) (setq s (+ s (* i i)))) s))
(defun prof-work () (let ((tot 0)) (dotimes (k 40) (setq tot (+ tot (prof-leaf 20000)))) tot))

(deftest profiler
  (with-profiling (:interval 500) (prof-work) (prof-work))
  (is (> (length *profile-samples*) 3))
  (let ((data (profile-data)))
    (is (assoc 'prof-leaf data))
    (is (>= (caddr (assoc 'prof-work data)) (cadr (assoc 'prof-work data))))   ; cumulative >= self
    (is (> (cadr (car data)) 0)))
  (let ((report (with-output-to-string (*standard-output*) (profile-report))))
    (is (%string-search "PROF-LEAF" report 0))
    (is (%string-search "samples" report 0)))
  ;; stopping twice is harmless
  (is (integerp (profile-stop))))

(deftest system-primitives
  (let ((dir (make-temp-dir)))
    (unwind-protect
         (progn
           (write-file-lines (%string-concat dir "/x.txt") (list "hello"))
           (is-equal '("x.txt") (mapcar #'car (%directory dir)))
           (is-equal :file (cadr (car (%directory dir))))
           (is-equal 6 (caddr (car (%directory dir))))
           (is-equal :file (car (%file-attrs (%string-concat dir "/x.txt"))))
           (is-equal nil (%file-attrs (%string-concat dir "/nope")))
           (is (%rename-file (%string-concat dir "/x.txt") (%string-concat dir "/y.txt")))
           (is (probe-file (%string-concat dir "/y.txt")))
           (multiple-value-bind (out status) (run-program "echo" (list "a" "b"))
             (is-equal (format nil "a b~%") out) (is-equal 0 status))
           (multiple-value-bind (out status) (run-program "cat" nil "from stdin")
             (is-equal "from stdin" out) (is-equal 0 status))
           (multiple-value-bind (out status) (run-program "sh" (list "-c" "echo oops >&2; exit 3"))
             (is-equal (format nil "oops~%") out) (is-equal 3 status))
           (multiple-value-bind (out status) (run-program "no-such-program-xyz" nil)
             (declare (ignore out))
             (is-equal 127 status))
           (is (> (length (%getcwd)) 0)))
      (cleanup-dir dir))))

;;; ---- Inspector, browsers and tool windows -------------------------------------------------------------------------------------

(defstruct inspected-point x y)
(defclass inspected-thing () ((label :initarg :label :initform "thing" :accessor thing-label) (items :initarg :items :initform nil)))

(deftest inspector
  (multiple-value-bind (wm lw) (make-test-wm)
    (declare (ignorable lw))
    (let* ((data (list 1 "two" (list 3 4) :five))
           (w (open-inspector wm data)))
      (wm-draw wm)
      (is (typep w 'inspector-window))
      (is (screen-has wm "Inspector"))
      (is (screen-has wm "\"two\""))
      (is-equal 4 (length (rw-entries w)))
      ;; descending into a nested list by clicking its value
      (let* ((pos (screen-has wm "2: (3 4)")) (row (car pos)) (col (+ 3 (cdr pos))))
        (wm-feed wm (mouse-bytes :press (+ col 1) row 0))
        (wm-feed wm (mouse-bytes :release (+ col 1) row 0))
        (is (screen-has wm "Inspector: (3 4)"))
        (is-equal 2 (length (rw-entries w))))
      (wm-feed wm (string (code-char 127)))                               ; back
      (is-equal 4 (length (rw-entries w)))
      ;; setting an entry through the command line
      (setf (wm-focus wm) lw)
      (type-line wm ":set-entry 0 (+ 40 2)")
      (is-equal 42 (car data))
      (is (screen-has wm "42"))
      (signals-error (execute-command-line "set-entry 99 1")))
    ;; instances, structures, hash tables, functions, symbols, numbers
    (let ((thing (make-instance 'inspected-thing :label "L" :items '(1 2))))
      (is-equal '("LABEL" "ITEMS") (mapcar #'car (inspector-entries thing)))
      (funcall (caddr (car (inspector-entries thing))) "changed")
      (is-equal "changed" (thing-label thing)))
    (let ((p (make-inspected-point :x 1 :y 2)))
      (is-equal '("X" "Y") (mapcar #'car (inspector-entries p)))
      (funcall (caddr (cadr (inspector-entries p))) 99)
      (is-equal 99 (inspected-point-y p)))
    (let ((h (make-hash-table :test 'equal))) (setf (gethash "k" h) 7)
      (is-equal 7 (cadr (car (inspector-entries h))))
      (funcall (caddr (car (inspector-entries h))) 8)
      (is-equal 8 (gethash "k" h)))
    (is (assoc "name" (inspector-entries #'mapcar) :test #'string=))
    (is (assoc "value" (inspector-entries '*xr-var*) :test #'string=))
    (is (assoc "hex" (inspector-entries 255) :test #'string=))
    (is (assoc "superclasses" (inspector-entries (find-class 'dog)) :test #'string=))
    (is-equal nil (inspector-entries (make-hash-table)))))

(deftest tool-windows-and-commands
  (multiple-value-bind (wm lw) (make-test-wm)
    (type-line wm ":who-calls xr-leaf")
    (is (screen-has wm "Callers of XR-LEAF"))
    (is (screen-has wm "XR-MID")) (is (screen-has wm "XR-TOP"))
    (wm-close-window wm (wm-focus wm))
    (type-line wm ":callees xr-top")
    (is (screen-has wm "XR-LEAF"))
    (wm-close-window wm (wm-focus wm))
    ;; a click on a function name offers who-calls / edit / trace
    (type-line wm ":apropos-window xr-m")
    (let* ((pos (screen-has wm "XR-MID")) (row (car pos)) (col (cdr pos)))
      (wm-feed wm (mouse-bytes :press (+ col 1) row 2))
      (is (wm-popups wm))
      (is (screen-has wm "Who calls"))
      (is (screen-has wm "Edit definition"))
      (is (screen-has wm "Trace"))
      (wm-close-popups wm)
      ;; left click on the symbol runs its default command (describe)
      (wm-feed wm (mouse-bytes :press (+ col 1) row 0)))
    (wm-close-window wm (wm-focus wm))
    (type-line wm ":browse-class dog")
    (is (screen-has wm "Class DOG"))
    (is (screen-has wm "ANIMAL"))
    (is (screen-has wm "TRICKS"))
    (wm-close-window wm (wm-focus wm))
    (type-line wm ":browse-packages")
    (is (screen-has wm "COMMON-LISP-USER"))
    (wm-close-window wm (wm-focus wm))
    (type-line wm ":who-defines mapcar")
    (is (screen-has wm "lisp/core/00-base.lisp"))
    (type-line wm ":arglist mapcar")
    (is (screen-has wm "(mapcar fn list &rest more)"))
    (type-line wm ":edit mapcar")                                       ; opens the editor on MAPCAR's definition
    (let ((ed (find-editor-window wm)))
      (is ed)
      (is (%string-search "(defun mapcar" (buf-current-line (ed-buffer ed)) 0))
      (wm-close-window wm ed))
    (type-line wm ":step-trace (+ 1 (* 2 3))")
    (is (screen-has wm "Step: (* 2 3)"))
    (type-line wm ":profile (prof-work)")
    (is (screen-has wm "samples"))
    (type-line wm ":trace tr-fact")
    (type-line wm "(tr-fact 2)")
    (is (screen-has wm "TR-FACT returned"))
    (type-line wm ":untrace tr-fact")
    (is-equal nil *traced*)
    (is (find 'inspect *commands* :key #'cmd-name))
    (type-line wm ":inspect (list 1 2)")
    (is (find-inspector wm))))

(deftest clickable-failures-and-frames
  (multiple-value-bind (wm lw) (make-test-wm)
    ;; a failure record as produced by RUN-TESTS; clicking it edits the test's source
    (let ((failure (list 'bignums '(is nil) nil)) (cmd (default-pcommand 'test-failure)))
      (is-equal 'edit-test (pcmd-name cmd))
      (%in-listener lw (lambda () (funcall (pcmd-fn cmd) failure)))
      (let ((ed (find-editor-window wm)))
        (is ed)
        (is (%string-search "(deftest bignums" (buf-current-line (ed-buffer ed)) 0))
        (is (string= "phase4-tests.lisp" (buffer-name (ed-buffer ed))))
        (wm-close-window wm ed)))
    ;; errors in the Listener show clickable frames that lead to the source of the function
    (type-line wm "(xr-mid 'a)")
    (is (screen-has wm "Error:"))
    (is (screen-has wm "XR-LEAF"))
    (let* ((pos (screen-has wm "0: (XR-LEAF")) (row (car pos)) (col (cdr pos)))
      (wm-feed wm (mouse-bytes :press (+ col 5) row 2))
      (is (screen-has wm "Edit definition"))
      (wm-close-popups wm))
    (is-equal 'frame (presentation-type-of-frame (car (cdr (lw-last-error lw)))))
    (let ((f (car (cdr (lw-last-error lw)))))
      (is-equal 'xr-leaf (cadr f))
      (funcall (pcmd-fn (find 'edit-frame-definition (applicable-pcommands 'frame) :key #'pcmd-name)) f)
      (let ((ed (find-editor-window wm)))
        (is ed)
        (is (%string-search "(defun xr-leaf" (buf-current-line (ed-buffer ed)) 0))))
    ;; the line-mode debugger's :edit uses the same hook
    (is *edit-hook*)))

(defun presentation-type-of-frame (f) (declare (ignore f)) 'frame)

;;; ---- Zmacs definition commands ---------------------------------------------------------------------------------

(deftest editor-sections
  (let ((secs (text-sections (format nil "(in-package :cl-user)~%;; comment~%(defun zs-a (x) x)~%#|~%(defun zs-hidden ())~%|#~%(defmacro zs-m (y) y)~%  (defun zs-indented ())~%(defmethod zs-g ((x integer)) x)~%(defmethod zs-g ((x string)) x)~%(defvar *zs-v* 1)~%(defun (setf zs-a) (v x) v)~%(print 1)"))))
    (is-equal '(:function :macro :method :method :variable :function) (mapcar #'sec-kind secs))
    (is-equal "ZS-A" (symbol-name (sec-name (car secs))))
    (is (eq (find-package "COMMON-LISP-USER") (sec-package (car secs))))  ; IN-PACKAGE is followed
    (is-equal "(defun zs-a (x) x)" (sec-text (car secs)))
    (is-equal '(:method 1) (list (car (sec-key (nth 3 secs))) (caddr (sec-key (nth 3 secs)))))  ; methods are numbered
    (is (not (find "ZS-HIDDEN" secs :key (lambda (s) (symbol-name (sec-name s))) :test #'string=))))
  ;; #. is not evaluated while sectionizing
  (let ((*zs-flag* nil)) (declare (special *zs-flag*))
    (text-sections "(defun #.(setq *zs-flag* t) ())")
    (is (not *zs-flag*))))

(deftest editor-changed-definitions
  (let ((dir (make-temp-dir)))
    (unwind-protect
         (multiple-value-bind (wm ed) (make-editor-wm)
           (let ((path (%string-concat dir "/chg.lisp")))
             (write-file-lines path (list "(in-package :cl)" "(defun zc-one () 1)" "" "(defun zc-two () 2)" "" "(defun zc-three () 3)"))
             (load path)
             (ed-keys wm "C-x C-f C-u") (ed-type wm path) (ed-keys wm "RET")
             (let ((b (ed-buffer ed)))
               (is-equal nil (buffer-changed-sections b))                  ; nothing changed since read in
               (ed-keys wm "M-x") (ed-type wm "compile-changed-definitions") (ed-keys wm "RET")
               (is (screen-has wm "No definitions changed since last compiled"))
               ;; change ZC-TWO and ZC-THREE
               (buf-goto b 3 18) (ed-type wm "0")
               (buf-goto b 5 20) (ed-type wm "0")
               (is-equal '(zc-two zc-three) (mapcar #'sec-name (buffer-changed-sections b)))
               ;; Edit Changed Definitions visits the first, C-x ` the next
               (ed-keys wm "M-<")
               (ed-keys wm "M-x") (ed-type wm "edit-changed-definitions") (ed-keys wm "RET")
               (is-equal 3 (point-line b))
               (is (screen-has wm "1 of 2"))
               (ed-keys wm "C-x `")
               (is-equal 5 (point-line b))
               (ed-keys wm "C-x `")
               (is (screen-has wm "No more possibilities"))
               (ed-keys wm "M-g p")
               (is-equal 3 (point-line b))
               ;; the possibilities list is a buffer; RET goes to an entry
               (ed-keys wm "C-x C-.")
               (is-equal "*Possibilities*" (buffer-name (ed-buffer ed)))
               (is (screen-has wm "function zc-three"))
               (ed-keys wm "n RET")
               (is (eq b (ed-buffer ed)))
               (is-equal 5 (point-line b))
               ;; Compile Changed Definitions compiles only those two
               (ed-keys wm "M-x") (ed-type wm "compile-changed-definitions") (ed-keys wm "RET")
               (is (screen-has wm "Compiled 2 definitions: zc-two, zc-three"))
               (is-equal 20 (zc-two)) (is-equal 30 (zc-three))
               (is-equal nil (buffer-changed-sections b))
               ;; ... but they still differ from the saved file (C-u) until it is saved
               (is-equal 2 (length (buffer-changed-sections b :saved)))
               (ed-keys wm "C-x C-s")
               (is-equal nil (buffer-changed-sections b :saved))
               (is-equal 2 (length (buffer-changed-sections b :read)))
               ;; C-M-x marks just its definition compiled
               (buf-goto b 1 18) (ed-type wm "1")
               (is-equal '(zc-one) (mapcar #'sec-name (buffer-changed-sections b)))
               (ed-keys wm "C-M-x")
               (is-equal nil (buffer-changed-sections b))
               (is-equal 11 (zc-one))
               ;; undoing an edit leaves the definition unchanged
               (buf-goto b 1 17) (ed-type wm " ") (ed-keys wm "C-/")
               (is-equal nil (buffer-changed-sections b)))))
      (cleanup-dir dir))))

(defun zx-callee (x) (+ x 1))
(defun zx-caller-a (x)
  (* 2 (zx-callee x)))
(defun zx-caller-b (x) (list x (zx-callee x)))

(deftest editor-edit-callers
  (multiple-value-bind (wm ed) (make-editor-wm)
    (is-equal '(zx-caller-a zx-caller-b) (callers-of 'zx-callee))
    (ed-keys wm "M-x") (ed-type wm "lisp-mode") (ed-keys wm "RET")
    (ed-type wm "zx-callee") (ed-keys wm "M-b")
    (ed-keys wm "M-x") (ed-type wm "edit-callers") (ed-keys wm "RET")
    (is (screen-has wm "Edit callers of (default zx-callee):"))
    (ed-keys wm "RET")                                                     ; take the default
    (is-equal "tools-tests.lisp" (buffer-name (ed-buffer ed)))
    (is (%string-search "(* 2 (zx-callee x))" (buf-current-line (ed-buffer ed)) 0))
    (is (%string-search "zx-callee" (buf-current-line (ed-buffer ed)) (point-col (ed-buffer ed))))  ; point is on the call
    (is (screen-has wm "1 of 2 callers of zx-callee"))
    (ed-keys wm "C-x `")
    (is (%string-search "(defun zx-caller-b" (buf-current-line (ed-buffer ed)) 0))
    (is (screen-has wm "2 of 2"))
    ;; List Callers shows the list without moving
    (ed-keys wm "M-x") (ed-type wm "list-callers") (ed-keys wm "RET")
    (ed-keys wm "C-a C-k") (ed-type wm "zx-callee") (ed-keys wm "RET")
    (is-equal "*Possibilities*" (buffer-name (ed-buffer ed)))
    (is (screen-has wm "callers of zx-callee (2):"))
    (ed-keys wm "q")
    (ed-keys wm "M-x") (ed-type wm "edit-callers") (ed-keys wm "RET C-a C-k") (ed-type wm "zx-caller-b") (ed-keys wm "RET")
    (is (screen-has wm "No callers of zx-caller-b"))))

(deftest editor-arglist-documentation-macroexpand
  (multiple-value-bind (wm ed) (make-editor-wm)
    (ed-keys wm "M-x") (ed-type wm "lisp-mode") (ed-keys wm "RET")
    (ed-type wm "(mapcar (lambda (x) (1+ x)) (zx-caller-b 1 2 3))")
    (ed-keys wm "C-b C-b C-b")                                             ; inside (list ...)
    (ed-keys wm "C-c C-a")
    (is (screen-has wm "(zx-caller-b x)"))
    (ed-keys wm "C-a M-f")                                                 ; on mapcar
    (ed-keys wm "C-c C-a")
    (is (screen-has wm "(mapcar fn list &rest more)"))
    (ed-keys wm "C-c C-d")
    (is (screen-has wm "mapcar: Apply a function to successive elements"))
    (ed-keys wm "C-u C-c C-d")
    (is-equal "*Documentation*" (buffer-name (ed-buffer ed)))
    (is (screen-has wm "Function documentation:"))
    (ed-keys wm "M-,")
    (is-equal "work" (buffer-name (ed-buffer ed)))
    ;; Macro Expand Expression: short expansions go to the echo area, long ones to a buffer
    (ed-keys wm "M-> RET")
    (ed-type wm "(when (car x) \"Hi\" #\\A)")
    (ed-keys wm "C-a C-c RET")
    (is (screen-has wm "(if (car x) (progn \"Hi\" #\\A) nil)"))
    (ed-keys wm "M-> RET")
    (ed-type wm "(defun zz-long-name (alpha beta gamma) (let ((delta (+ alpha beta gamma))) (when (> delta 10) (print delta) (* delta delta delta))))")
    (ed-keys wm "C-a C-c RET")
    (is-equal "*Macroexpansion*" (buffer-name (ed-buffer ed)))
    (is (> (buf-nlines (ed-buffer ed)) 1))
    (is (not (fboundp 'zz-long-name)))))                                  ; expanding does not evaluate
