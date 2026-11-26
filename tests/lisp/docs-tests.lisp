;;;; docs-tests.lisp -- the documentation system: docstrings, Markdown, symbol documents, search,
;;;; viewer, HTML and PDF export, tutorial, coverage.

(in-package :cl)

(deftest docstrings
  (eval '(defun doc-test-fn (x) "Doubles X. See `list`." (* 2 x)))
  (is-equal "Doubles X. See `list`." (documentation 'doc-test-fn 'function))
  (is-equal 8 (doc-test-fn 4))                                   ; the body is unaffected by the docstring
  (eval '(defun doc-test-no-doc (x) x))
  (is-equal nil (documentation 'doc-test-no-doc 'function))
  (eval '(defmacro doc-test-mac (x) "A macro." x))
  (is-equal "A macro." (documentation 'doc-test-mac 'function))
  (eval '(defvar *doc-test-var* 1 "A variable."))
  (is-equal "A variable." (documentation '*doc-test-var* 'variable))
  (eval '(defparameter *doc-test-par* 2 "A parameter."))
  (is-equal "A parameter." (documentation '*doc-test-par* 'variable))
  (setf (documentation 'doc-test-fn 'function) "Changed.")
  (is-equal "Changed." (documentation 'doc-test-fn 'function))
  (is-equal nil (documentation 'doc-test-fn 'variable))
  (is (stringp (documentation 'mapcar 'function)))               ; system docstrings
  (is (stringp (documentation '*package* 'variable)))
  (is (stringp (documentation 'hash-table 'type)))
  ;; describe shows the docstring
  (is (%string-search "Changed." (with-output-to-string (*standard-output*) (describe 'doc-test-fn)) 0)))

(deftest markdown-parsing
  (let ((nodes (markdown->nodes "# Title

A paragraph with `code`, **strong**, *emphasis* and a [link](topic:other).
It continues here.

## Section

- first item
- second [fn](fn:car) item
  continued

1. one
2. two

```
(+ 1 2)
```

| Key | Action |
|---|---|
| `C-x` | prefix |
")))
    (is-equal '(:h 1 "Title") (first nodes))
    (is-equal :p (car (second nodes)))
    (let ((p (cdr (second nodes))))
      (is (member '(:code "code") p :test #'equal))
      (is (member '(:strong "strong") p :test #'equal))
      (is (member '(:em "emphasis") p :test #'equal))
      (is (member '(:ref :other "link") p :test #'equal))
      (is (find "It continues here." (list (inline-text p)) :test (lambda (a b) (%string-search a b 0)))))
    (is-equal '(:h 2 "Section") (third nodes))
    (is-equal :ul (car (fourth nodes)))
    (is-equal 2 (length (cdr (fourth nodes))))
    (is (member (list :fn 'car) (cadr (cdr (fourth nodes))) :test #'equal))
    (is (%string-search "continued" (inline-text (cadr (cdr (fourth nodes)))) 0))
    (is-equal :ol (car (fifth nodes)))
    (is-equal '(:example "(+ 1 2)") (sixth nodes))
    (let ((table (nth 6 nodes)))
      (is-equal :table (car table))
      (is-equal 2 (length (cdr table)))                                   ; the separator row is dropped
      (is-equal "Key" (inline-text (car (cadr table))))
      (is-equal "C-x" (inline-text (car (caddr table)))))
    (is-equal "Title" (markdown-title nodes)))
  ;; stray markers stay literal
  (is-equal '("2 * 3 = 6") (%md-inline "2 * 3 = 6"))
  (is-equal '("a_b_c and `x") (%md-inline "a_b_c and `x"))
  (is-equal '((:link "http://example.com" "site")) (%md-inline "[site](http://example.com)")))

(deftest documents-and-text
  (define-document :doc-test-a (:title "Alpha Document" :keywords (alpha greek) :category :test)
    (:h 1 "Alpha Document")
    (:p "The first letter. " (:code "car") " is a function.")
    (:example "(car '(1 2))")
    (:ul ("one") ("two" (:strong " items")))
    (:see-also :doc-test-b))
  (define-document :doc-test-b (:title "Beta Document" :keywords (greek) :category :test)
    (:p "The second letter, see " (:ref :doc-test-a "alpha") "."))
  (is (find-document :doc-test-a))
  (is-equal "Alpha Document" (doc-title (find-document :doc-test-a)))
  (is (member :doc-test-b (document-names)))
  (is (eq (find-document :doc-test-a) (resolve-topic :doc-test-a)))
  (is (eq (find-document :doc-test-a) (resolve-topic "doc-test-a")))
  (is-equal nil (resolve-topic :no-such-doc-topic))
  (let ((text (doc-text (find-document :doc-test-a))))
    (is (%string-search "Alpha Document" text 0))
    (is (%string-search "The first letter. car is a function." text 0))
    (is (%string-search "    (car '(1 2))" text 0))
    (is (%string-search "  * one" text 0))
    (is (%string-search "See also" text 0)))
  (is-equal '("aaa bbb" "ccc") (wrap-words "aaa bbb ccc" 8))
  (is-equal '("  * x yy" "    zz") (wrap-words "x yy zz" 8 "    " "  * "))
  (is-equal "The first letter." (doc-summary (find-document :doc-test-a)))
  ;; redefining replaces
  (define-document :doc-test-a (:title "Alpha Two") (:p "new"))
  (is-equal "Alpha Two" (doc-title (find-document :doc-test-a)))
  (is-equal 1 (count :doc-test-a (document-names)))
  ;; loading Markdown at run time
  (let ((path "/tmp/ng-doc-test.md"))
    (write-file-lines path (list "# From a file" "" "Some *text* here."))
    (let ((d (load-markdown-document :doc-test-md path :category :test)))
      (is-equal "From a file" (doc-title d))
      (is (find-document :doc-test-md)))
    (delete-file path)))

(deftest symbol-documents
  (let ((d (symbol-document 'mapcar)))
    (is-equal "MAPCAR" (doc-title d))
    (let ((text (doc-text d)))
      (is (%string-search "Function (mapcar fn list &rest more)" text 0))
      (is (%string-search "successive elements" text 0))
      (is (%string-search "lisp/core/00-base.lisp" text 0))))
  (let ((text (doc-text (resolve-topic 'defun))))
    (is (%string-search "Macro" text 0)))
  (let ((text (doc-text (symbol-document '*package*))))
    (is (%string-search "Variable" text 0))
    (is (%string-search "current package" text 0)))
  (let ((text (doc-text (symbol-document 'standard-object))))
    (is (%string-search "Class" text 0)))
  (let ((text (doc-text (symbol-document 'initialize-instance))))
    (is (%string-search "Generic function" text 0)))
  (is (%string-search "No definition is recorded" (doc-text (symbol-document '|NO-SUCH-SYMBOL-XYZ|)) 0))
  (is (equal '(:function) (symbol-kinds 'mapcar)))
  (is (equal '(:macro) (symbol-kinds 'when)))
  (is (member :special-operator (symbol-kinds 'if))))

(deftest documentation-search
  (let ((r (search-documentation "undo tree editor")))
    (is r)
    (is-equal :editor-manual (second (car r)))                         ; the manual about the editor ranks first
    (is (> (first (car r)) (first (car (last r))))))
  (let ((r (search-documentation "mapcar")))
    (is-equal 'mapcar (second (car r)))                                 ; an exact symbol name scores highest
    (is-equal :symbol (third (car r))))
  (is (find :documentation-system (mapcar #'second (search-documentation "docstring markdown export"))))
  (is (find 'with-profiling (mapcar #'second (search-documentation "profiler"))))
  (is (find :tools-manual (mapcar #'second (search-documentation "inspect"))))     ; prefix match: inspector
  (is-equal nil (search-documentation "zzzzqqqq"))
  (is-equal nil (search-documentation "the and"))                       ; only stop words
  (is (<= (length (search-documentation "function" 5)) 5))
  ;; a document registered later is found after the index is rebuilt
  (define-document :doc-test-search (:title "Zorblax Frobnication") (:p "Frobnicate the zorblax."))
  (is-equal :doc-test-search (second (car (search-documentation "zorblax")))))

(deftest documentation-coverage
  (multiple-value-bind (covered total missing) (doc-coverage)
    (is (> total 400))
    (is-equal nil (mapcar #'symbol-name missing))                       ; every public core symbol is documented
    (is-equal total covered)))

;;; ---- viewer ---------------------------------------------------------------------------------------------

(defun scroll-find (wm w text)
  "Find TEXT in window W, scrolling from the top downward; returns (row . col) or NIL."
  (window-scroll-by w 1000000)
  (dotimes (i 60 nil)
    (wm-draw wm)
    (let ((p (screen-has wm text))) (when p (return p)))
    (window-scroll-by w -4)))

(defun click-at (wm pos &optional (dx 1) (button 0))
  (wm-feed wm (mouse-bytes :press (+ (cdr pos) dx) (car pos) button))
  (wm-feed wm (mouse-bytes :release (+ (cdr pos) dx) (car pos) button)))

(deftest viewer
  (multiple-value-bind (wm lw) (make-test-wm)
    (setq *doc-bookmarks* nil)
    (type-line wm ":doc")
    (let ((w (find-doc-window wm)))
      (is w)
      (is (typep w 'doc-window))
      (is (screen-has wm "NeoGenera Documentation"))
      (is (screen-has wm "NeoGenera User's Guide"))
      (is (screen-has wm "The Editor"))
      ;; follow a link from the contents
      (click-at wm (screen-has wm "NeoGenera User's Guide") 2)
      (is-equal :user-guide (rw-subject w))
      (is (screen-has wm "NeoGenera User's Guide"))
      (is (scroll-find wm w "Build the system once"))
      (is (scroll-find wm w "Candidates"))
      (is (scroll-find wm w "Developer Tools"))                            ; related topics are listed
      (is (member :tools-manual (doc-candidates (find-document :user-guide))))
      ;; links inside the text; backspace returns
      (let ((pos (scroll-find wm w "Window System Manual")))
        (is pos)
        (click-at wm pos 2))
      (is-equal :window-system (rw-subject w))
      (is (screen-has wm "The Window System"))
      (wm-feed wm (string (code-char 127)))
      (is-equal :user-guide (rw-subject w))
      ;; bookmarks
      (wm-feed wm "b")
      (is-equal '(:user-guide) *doc-bookmarks*)
      (wm-feed wm "t")
      (is-equal :contents (rw-subject w))
      (is (scroll-find wm w "Bookmarks"))
      (wm-feed wm (string (code-char 127)))
      ;; a function name in a link shows the symbol's documentation
      (let ((pos (scroll-find wm w "mapcar")))
        (is pos)
        (click-at wm pos 1))
      (is (screen-has wm "Function (mapcar fn list &rest more)"))
      (is (scroll-find wm w "lisp/core/00-base.lisp"))
      (wm-feed wm (string (code-char 127)))
      ;; examples run in the Listener
      (let ((pos (scroll-find wm w "(loop for i from 1 to 5")))
        (is pos)
        (click-at wm pos 2 2)
        (is (wm-popups wm))
        (is (screen-has wm "Run in Listener"))
        (is (screen-has wm "Copy to Listener input"))
        (click-at wm (screen-has wm "Run in Listener") 2)                 ; choose the first item
        (is (null (wm-popups wm))))
      (wm-close-window wm (find-doc-window wm))
      (wm-draw wm)
      (is (screen-has wm "(1 4 9 16 25)")))
    ;; the other commands
    (type-line wm ":doc mapcar")
    (is (screen-has wm "Function (mapcar fn list &rest more)"))
    (wm-close-window wm (find-doc-window wm))
    (type-line wm ":search-doc undo editor")
    (is (screen-has wm "Search: undo editor"))
    (is (screen-has wm "The Editor"))
    (let* ((pos (screen-has wm "The Editor")) (row (car pos)) (col (cdr pos)))
      (wm-feed wm (mouse-bytes :press (+ col 1) row 0))
      (wm-feed wm (mouse-bytes :release (+ col 1) row 0)))
    (is (screen-has wm "Moving"))
    (wm-close-window wm (find-doc-window wm))
    (type-line wm ":doc no-such-topic-xyz")
    (is (screen-has wm "No documentation for"))
    (type-line wm ":bookmarks")
    (is (screen-has wm "NeoGenera User's Guide"))
    (type-line wm ":doc editor-manual")
    (is (scroll-find wm (find-doc-window wm) "The undo tree"))
    (is (scroll-find wm (find-doc-window wm) "C-x C-s"))                  ; tables render
    (is (find-doc-window wm))))

(deftest documentation-in-other-tools
  (is (assoc "documentation" (inspector-entries 'mapcar) :test #'string=))
  (is (null (assoc "documentation" (inspector-entries 'doc-test-no-doc) :test #'string=)))
  (let ((text (with-output-to-string (*standard-output*) (doc 'mapcar))))
    (is (%string-search "Function (mapcar fn list &rest more)" text 0)))
  (is (%string-search "No documentation" (with-output-to-string (*standard-output*) (doc :no-such-topic-q)) 0))
  (is (find 'show-symbol-documentation (applicable-pcommands 'symbol) :key #'pcmd-name))
  (is-equal 'show-documentation (pcmd-name (default-pcommand 'doc-ref)))
  (is-equal 'run-example (pcmd-name (default-pcommand 'doc-example)))
  (is-equal 'copy-code (pcmd-name (default-pcommand 'doc-code))))

(deftest copying-code-blocks
  ;; the fence's language is kept; Lisp examples stay runnable, others are copied by a click
  (is-equal '((:example "x := 1" "pascal") (:example "(+ 1 2)") (:example "(car x)" "lisp"))
            (markdown->nodes (format nil "```pascal~%x := 1~%```~%~%```~%(+ 1 2)~%```~%~%``` lisp~%(car x)~%```")))
  (is-equal "TWFu" (%base64 "Man"))
  (is-equal "TWE=" (%base64 "Ma"))
  (multiple-value-bind (wm lw) (make-test-wm 40 120)
    (declare (ignore lw))
    (type-line wm ":doc pascal-manual")
    (let* ((w (find-doc-window wm)) (pos (scroll-find wm w "PROGRAM quadratic;")))
      (is pos)
      (click-at wm pos 2)                                                   ; left click: copy
      (is (%starts-with (current-kill 0) "{-*- Mode: PASCAL; Dialect: ISO; Package: PASCAL-USER -*- }"))
      (is (%string-search (format nil "~%PROGRAM quadratic;~%VAR a,b,c") (current-kill 0) 0))   ; without the viewer's indentation
      (is (%string-search "C-y pastes them in the editor" (wm-message wm) 0))
      (wm-draw wm)
      (is (screen-has wm "Copied 12 lines"))
      ;; a Lisp example offers Copy in its menu
      (let ((lpos (scroll-find wm w "(pascal-execute 'pascal-user::quadratic)")))
        (is lpos)
        (click-at wm lpos 2 2)
        (is (screen-has wm "Copy (C-y pastes it in the editor)"))
        (click-at wm (screen-has wm "Copy (C-y pastes it in the editor)") 2)
        (is-equal "(pascal-execute 'pascal-user::quadratic)" (current-kill 0))))
    (wm-close-window wm (find-doc-window wm)))
  ;; C-y in the editor pastes it
  (multiple-value-bind (wm ed) (make-editor-wm)
    (kill-new (format nil "begin~%  writeln('hi')~%end."))
    (ed-keys wm "C-y")
    (is-equal (format nil "begin~%  writeln('hi')~%end.") (buffer-string (ed-buffer ed)))))

;;; ---- export -------------------------------------------------------------------------------------------------

(defun %file-text (path) (join-lines (read-file-lines path)))

(deftest html-export
  (is-equal "a &lt;b&gt; &amp; &quot;c&quot;" (html-escape "a <b> & \"c\""))
  (is-equal "<strong>x</strong> <code>y</code>" (inlines->html '((:strong "x") " " (:code "y"))))
  (is (%string-search "href=\"user-guide.html\"" (inlines->html '((:ref :user-guide "guide"))) 0))
  (is (%string-search "href=\"reference.html#sym-mapcar\"" (inlines->html '((:fn mapcar))) 0))
  (is-equal "x" (inlines->html '((:ref :no-such-topic-q "x"))))              ; unknown topics are plain text
  (is (%string-search "<pre class=\"example\"><code>(car &#39;" (%string-concat "<pre class=\"example\"><code>(car &#39;") 0))
  (let ((dir (make-temp-dir)))
    (unwind-protect
         (let ((files (export-documentation dir :pdf nil)))
           (is (> (length files) 7))
           (is (probe-file (%string-concat dir "/index.html")))
           (is (probe-file (%string-concat dir "/user-guide.html")))
           (is (probe-file (%string-concat dir "/reference.html")))
           (let ((guide (%file-text (%string-concat dir "/user-guide.html"))))
             (is (%string-search "<!DOCTYPE html>" guide 0))
             (is (%string-search "<h1>NeoGenera User's Guide</h1>" guide 0))
             (is (%string-search "<a href=\"window-system.html\">Window System Manual</a>" guide 0))
             (is (%string-search "<table>" guide 0))
             (is (%string-search "<th>Command</th>" guide 0))
             (is (%string-search "<pre class=\"example\">" guide 0))
             (is (%string-search "<a href=\"reference.html#sym-mapcar\"><code>mapcar</code></a>" guide 0))
             (is (%string-search "<nav>" guide 0)))
           (let ((ref (%file-text (%string-concat dir "/reference.html"))))
             (is (%string-search "id=\"sym-mapcar\"" ref 0))
             (is (%string-search "(mapcar fn list &amp;rest more)" ref 0))        ; & is escaped
             (is (%string-search "collect the results" ref 0)))
           (let ((index (%file-text (%string-concat dir "/index.html"))))
             (is (%string-search "editor-manual.html" index 0))
             (is (%string-search "reference.html" index 0)))
           (dolist (f files) (delete-file f)))
      (cleanup-dir dir))))

(deftest pdf-export
  (is-equal "a\\(b\\)\\\\c" (pdf-escape "a(b)\\c"))
  (is-equal "x?y" (pdf-escape (%string-concat "x" (%utf8-encode 955) "y")))
  (let ((dir (make-temp-dir)))
    (unwind-protect
         (let ((path (%string-concat dir "/t.pdf")))
           (write-pdf path (list (cons :title "Title") (cons :blank "") (cons :text "hello (world)")))
           (let ((text (%file-text path)))
             (is (%string-search "%PDF-1.4" text 0))
             (is (%string-search "(hello \\(world\\)) Tj" text 0))
             (is (%string-search "startxref" text 0))
             (is (%string-search "%%EOF" text 0)))
           ;; a long document paginates
           (let ((lines nil)) (dotimes (i 400) (setq lines (cons (cons :text (format nil "line ~D" i)) lines)))
             (write-pdf path (nreverse lines))
             (is (> (length (read-file-lines path)) 400)))
           ;; the complete manual, validated by external tools when they are installed
           (let ((full (%string-concat dir "/manual.pdf")) (files (export-documentation dir)))
             (is (member full files :test #'string=))
             (is (> (cadr (%file-attrs full)) 20000))
             (multiple-value-bind (out status) (run-program "qpdf" (list "--check" full))
               (is (or (= status 127) (= status 0) (= status 3)))
               (when (= status 0) (is (%string-search "No syntax or stream encoding errors" out 0))))
             (multiple-value-bind (out status) (run-program "pdftotext" (list "-layout" full "-"))
               (when (= status 0)
                 (is (%string-search "NeoGenera User's Guide" out 0))
                 (is (%string-search "(mapcar fn list &rest more)" out 0))
                 (is (%string-search "The undo tree" out 0))))
             (dolist (f files) (delete-file f))))
      (cleanup-dir dir))))

;;; ---- tutorial -----------------------------------------------------------------------------------------------

(deftest tutorial
  (multiple-value-bind (wm lw) (make-test-wm)
    (is (>= (length *lessons*) 6))
    (is (find-lesson "first-steps"))
    (is (eq (find-lesson "first-steps") (find-lesson 1)))
    (type-line wm ":lessons")
    (is (screen-has wm "first-steps"))
    (is (screen-has wm "Defining functions"))
    (type-line wm ":tutorial first-steps")
    (is (lw-lesson lw))
    (is (screen-has wm "Lesson: First steps"))
    (is (screen-has wm "Step 1 of 3"))
    (type-line wm "(+ 1 1)")                                              ; wrong answer
    (is (screen-has wm "Not quite"))
    (is-equal 0 (cadr (lw-lesson lw)))
    (type-line wm "(+ 2 3)")
    (is (screen-has wm "Correct."))
    (is (screen-has wm "Step 2 of 3"))
    (type-line wm "(/ 1 2)")
    (type-line wm "(/ 1 4)")
    (is (screen-has wm "Type :hint for a hint"))
    (type-line wm ":hint")
    (is (screen-has wm "the ratio 1/3"))
    (type-line wm ":skip")
    (is (screen-has wm "Step 3 of 3"))
    (type-line wm "(expt 2 100)")
    (is (screen-has wm "Lesson complete: First steps"))
    (is (screen-has wm ":tutorial lists"))
    (is (null (lw-lesson lw)))
    ;; a full lesson with definitions made by the learner
    (type-line wm ":tutorial functions")
    (type-line wm "(defun square (x) (* x x))")
    (type-line wm "(square 12)")
    (type-line wm "(mapcar #'square '(1 2 3))")
    (is (screen-has wm "Lesson complete: Defining functions"))
    ;; leaving a lesson
    (type-line wm ":tutorial lists")
    (is (lw-lesson lw))
    (type-line wm ":end-tutorial")
    (is (null (lw-lesson lw)))
    (type-line wm ":tutorial no-such-lesson")
    (is (screen-has wm "No lesson named"))
    ;; a lesson defined in code
    (deflesson doc-test-lesson "Test lesson" "intro" (:text "say 7" :check (eql value 7) :hint "7") (:text "free step"))
    (type-line wm ":tutorial doc-test-lesson")
    (type-line wm "(+ 3 4)")
    (type-line wm "\"anything\"")
    (is (screen-has wm "Lesson complete: Test lesson"))))
