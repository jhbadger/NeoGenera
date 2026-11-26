;;;; 10-viewer.lisp -- the documentation viewer (a Document Examiner for NeoGenera).
;;;;
;;;; Documents are shown in a DOC-WINDOW, a report window whose references are presentations:
;;;; clicking a reference navigates (BACKSPACE goes back), clicking an example offers to run it in
;;;; the Listener, and every page ends with CANDIDATES, the related topics.  Keys: b bookmark,
;;;; t contents, g refresh, q close.  Commands: :doc, :search-doc, :bookmark, :bookmarks.

(in-package :cl)

(defclass doc-window (report-window) ())
(defmethod window-word-wrap ((w doc-window)) t)

;; documents are read from the top: scroll all the way back (the draw clamps it)
(defmethod report-refreshed ((w doc-window)) (setf (lw-scroll-back w) 1000000))

(define-presentation-type 'doc-ref '(t))
(define-presentation-type 'doc-example '(t))      ; a Lisp example: clicking runs it
(define-presentation-type 'doc-code '(t))         ; code in another language (Pascal, shell, ...): clicking copies it

(defun %lisp-example-p (lang) (or (null lang) (member lang '("lisp" "common-lisp" "cl" "commonlisp") :test #'string=)))

(defvar *doc-bookmarks* nil)

;;; ---- rendering --------------------------------------------------------------------------------------

(defun %doc-link-symbol (str)
  (and (> (length str) 0) (< (length str) 60) (not (%string-position-char str #\Space 0))
       (ignore-errors (%find-symbol-named str))))

(defun render-inlines (nodes out)
  (dolist (n nodes)
    (cond ((stringp n) (write-string n out))
          ((eq (car n) :code)
           (let ((sym (%doc-link-symbol (cadr n))))
             (with-style (out :value)
               (if (and sym (or (symbol-kinds sym) (symbol-documented-p sym)))
                   (with-output-as-presentation (out sym 'doc-ref) (write-string (cadr n) out))
                   (write-string (cadr n) out)))))
          ((eq (car n) :strong) (with-style (out :bold) (render-inlines (cdr n) out)))
          ((eq (car n) :em) (with-style (out :em) (render-inlines (cdr n) out)))
          ((eq (car n) :ref)
           (with-style (out :link) (with-output-as-presentation (out (cadr n) 'doc-ref) (write-string (caddr n) out))))
          ((eq (car n) :fn)
           (with-style (out :link)
             (with-output-as-presentation (out (cadr n) 'doc-ref) (write-string (string-downcase (symbol-name (cadr n))) out))))
          ((eq (car n) :link)
           (with-style (out :link) (write-string (caddr n) out))
           (with-style (out :dim) (format out " <~A>" (cadr n)))))))

(defun doc-candidates (doc)
  "Related topics: references made by DOC, its :see-also nodes and documents sharing a keyword."
  (let ((acc nil))
    (labels ((add (topic) (when (and topic (not (eq topic (doc-name doc))) (not (member topic acc)) (resolve-topic topic)) (setq acc (append acc (list topic))))))
      (labels ((scan (nodes)
                 (dolist (n nodes)
                   (when (consp n)
                     (cond ((eq (car n) :ref) (add (cadr n)))
                           ((eq (car n) :fn) (add (cadr n)))
                           ((member (car n) '(:p :em :strong)) (scan (cdr n)))
                           ((eq (car n) :see-also) (dolist (x (cdr n)) (add x)))
                           ((member (car n) '(:ul :ol)) (dolist (i (cdr n)) (scan i))))))))
        (scan (doc-nodes doc)))
      (dolist (d *documents*)
        (when (intersection (doc-keywords d) (doc-keywords doc)) (add (doc-name d)))))
    ;; documents before symbols, otherwise in order of appearance
    (let ((sorted (append (remove-if-not #'find-document acc) (remove-if #'find-document acc))))
      (if (> (length sorted) 8) (subseq sorted 0 8) sorted))))

(defun render-document (doc out)
  (with-style (out :bold) (format out "~A~%" (doc-title doc)))
  (with-style (out :dim) (format out "~A~%" (make-string (min 70 (max 3 (length (doc-title doc)))) #\=)))
  (dolist (n (doc-nodes doc))
    (case (car n)
      (:h (unless (and (eql (cadr n) 1) (string= (inline-text (cddr n)) (doc-title doc)))
            (terpri out)
            (with-style (out :bold) (render-inlines (cddr n) out))
            (terpri out)
            (when (<= (cadr n) 2)
              (with-style (out :dim) (format out "~A~%" (make-string (min 60 (max 3 (length (inline-text (cddr n))))) (if (= (cadr n) 1) #\= #\-)))))))
      (:p (terpri out) (render-inlines (cdr n) out) (terpri out))
      (:example (terpri out)
       (with-output-as-presentation (out (cadr n) (if (%lisp-example-p (caddr n)) 'doc-example 'doc-code))
         (with-style (out :value)
           (dolist (l (split-lines (cadr n))) (format out "    ~A~%" l)))))
      (:ul (terpri out) (dolist (item (cdr n)) (write-string "  * " out) (render-inlines item out) (terpri out)))
      (:ol (terpri out) (let ((i 0)) (dolist (item (cdr n)) (setq i (+ i 1)) (format out " ~D. " i) (render-inlines item out) (terpri out))))
      (:table (terpri out)
       (let ((lines (%table-lines (cdr n))) (first t))
         (dolist (l lines) (with-style (out (if first :bold :normal)) (format out "  ~A~%" l)) (setq first nil))))
      (:see-also (terpri out) (write-string "See also: " out)
       (let ((first t)) (dolist (x (cdr n)) (unless first (write-string ", " out)) (setq first nil)
                          (with-style (out :link) (with-output-as-presentation (out x 'doc-ref) (write-string (princ-to-string x) out)))))
       (terpri out))
      (t nil)))
  (let ((cands (doc-candidates doc)))
    (when cands
      (terpri out)
      (with-style (out :bold) (format out "Candidates~%"))
      (dolist (c cands)
        (write-string "  " out)
        (with-style (out :link) (with-output-as-presentation (out c 'doc-ref) (write-string (doc-title (resolve-topic c)) out)))
        (terpri out)))))

(defun doc-producer (topic)
  (lambda (w out)
    (declare (ignore w))
    (let ((d (resolve-topic topic)))
      (if d (render-document d out) (format out "No documentation for ~A~%" topic)))))

;;; ---- contents, search results, bookmarks ------------------------------------------------------------

(defun contents-producer ()
  (lambda (w out)
    (declare (ignore w))
    (with-style (out :bold) (format out "NeoGenera Documentation~%"))
    (with-style (out :dim) (format out "~A~%" (make-string 24 #\=)))
    (let ((cats nil))
      (dolist (d *documents*) (unless (member (doc-category d) cats) (setq cats (append cats (list (doc-category d))))))
      (dolist (c cats)
        (terpri out)
        (with-style (out :bold) (format out "~A~%" (string-capitalize (substitute #\Space #\- (if c (symbol-name c) "other")))))
        (dolist (d *documents*)
          (when (eq (doc-category d) c)
            (write-string "  " out)
            (with-style (out :link) (with-output-as-presentation (out (doc-name d) 'doc-ref) (write-string (doc-title d) out)))
            (with-style (out :dim) (format out "  ~A" (let ((s (doc-summary d))) (if (> (length s) 40) (%string-concat (subseq s 0 37) "...") s))))
            (terpri out)))))
    (when (and (boundp '*lessons*) (symbol-value '*lessons*))
      (terpri out) (with-style (out :bold) (format out "Tutorial lessons~%"))
      (with-style (out :dim) (format out "  :tutorial~A to begin; :lessons lists them~%" "")))
    (terpri out)
    (with-style (out :dim)
      (format out "Search with :search-doc WORDS.  :doc NAME shows a topic or a symbol.  b bookmarks a page.~%"))
    (when *doc-bookmarks*
      (terpri out) (with-style (out :bold) (format out "Bookmarks~%"))
      (dolist (b *doc-bookmarks*)
        (let ((d (resolve-topic b)))
          (when d (write-string "  " out)
                (with-style (out :link) (with-output-as-presentation (out b 'doc-ref) (write-string (doc-title d) out)))
                (terpri out)))))))

(defun search-producer (query)
  (lambda (w out)
    (declare (ignore w))
    (let ((results (search-documentation query)))
      (with-style (out :bold) (format out "Search: ~A  (~D result~:P)~%" query (length results)))
      (terpri out)
      (dolist (r results)
        (write-string "  " out)
        (with-style (out :link) (with-output-as-presentation (out (second r) 'doc-ref) (write-string (fourth r) out)))
        (with-style (out :dim)
          (format out "  [~A, score ~D]" (string-downcase (symbol-name (third r))) (first r))
          (let ((d (resolve-topic (second r)))) (when d (format out "  ~A" (let ((s (doc-summary d))) (if (> (length s) 50) (%string-concat (subseq s 0 47) "...") s))))))
        (terpri out))
      (when (null results) (format out "  nothing found~%")))))

(defun bookmark-topic (topic)
  (when (and topic (not (member topic *doc-bookmarks*))) (setq *doc-bookmarks* (append *doc-bookmarks* (list topic))))
  *doc-bookmarks*)

;;; ---- windows -------------------------------------------------------------------------------------------

(defun doc-window-title (topic)
  (let ((d (and (not (member topic '(:contents :bookmarks))) (not (and (consp topic) (eq (car topic) :search))) (resolve-topic topic))))
    (cond (d (%string-concat "Documentation: " (doc-title d)))
          ((and (consp topic) (eq (car topic) :search)) (%string-concat "Documentation: search " (cdr topic)))
          (t "Documentation"))))

(defun doc-topic-producer (topic)
  (cond ((eq topic :contents) (contents-producer))
        ((and (consp topic) (eq (car topic) :search)) (search-producer (cdr topic)))
        (t (doc-producer topic))))

(defun find-doc-window (wm) (dolist (w (wm-windows wm) nil) (when (typep w 'doc-window) (return w))))

(defun open-doc-window (wm topic)
  (open-report wm (doc-window-title topic) (doc-topic-producer topic) :class 'doc-window :subject topic :w 84 :h 26))

(defun show-doc (topic &optional (wm *wm*))
  "Show TOPIC in the documentation window (reusing the one the click came from)."
  (let ((w (cond ((typep *current-listener* 'doc-window) *current-listener*) (t (find-doc-window wm)))))
    (if w
        (progn (report-navigate w (doc-topic-producer topic) topic (doc-window-title topic)) (wm-raise wm w) w)
        (open-doc-window wm topic))))

(defmethod window-key ((w doc-window) key)
  (let ((c (key-code key)) (m (key-mods key)))
    (cond ((and (eql c 98) (zerop m))
           (bookmark-topic (rw-subject w))
           (setf (wm-message (window-wm w)) (format nil "Bookmarked: ~A" (doc-window-title (rw-subject w)))) t)
          ((and (eql c 116) (zerop m)) (report-navigate w (contents-producer) :contents "Documentation: Contents") t)
          (t (call-next-method)))))

(defun find-plain-listener (wm)
  (dolist (w (wm-windows wm) nil) (when (and (typep w 'listener-window) (not (lw-read-only w))) (return w))))

(defun doc-example-to-listener (text run)
  (let* ((wm (if (typep *current-listener* 'window) (window-wm *current-listener*) *wm*))
         (l (and wm (find-plain-listener wm))))
    (if (null l)
        (format t "~&No Listener window~%")
        (progn (wm-raise wm l)
               (setf (lw-input l) text (lw-cursor-pos l) (length text))
               (when run (%submit l))))))

(define-presentation-command (show-documentation doc-ref :documentation "Show documentation" :default t) (topic) (show-doc topic))
(define-presentation-command (show-symbol-documentation symbol :documentation "Documentation") (sym) (show-doc sym))
(define-presentation-command (run-example doc-example :documentation "Run in Listener" :default t) (text)
  (doc-example-to-listener text t))
(define-presentation-command (copy-example doc-example :documentation "Copy to Listener input") (text)
  (doc-example-to-listener text nil))

;;; Copying a code block: the text as written in the manual (without the viewer's indentation) goes on the kill ring,
;;; so C-y pastes it in the editor, and, in a terminal, on the system clipboard too (OSC 52).
(defun %base64 (string)
  (let ((chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/") (out (make-string-output-stream))
        (n (length string)) (i 0))
    (loop
      (when (>= i n) (return))
      (let* ((b0 (char-code (char string i)))
             (b1 (if (< (+ i 1) n) (char-code (char string (+ i 1))) 0))
             (b2 (if (< (+ i 2) n) (char-code (char string (+ i 2))) 0))
             (w (+ (ash (logand b0 255) 16) (ash (logand b1 255) 8) (logand b2 255))))
        (write-char (char chars (logand (ash w -18) 63)) out)
        (write-char (char chars (logand (ash w -12) 63)) out)
        (write-char (if (< (+ i 1) n) (char chars (logand (ash w -6) 63)) #\=) out)
        (write-char (if (< (+ i 2) n) (char chars (logand w 63)) #\=) out))
      (setq i (+ i 3)))
    (get-output-stream-string out)))

(defun %terminal-clipboard-copy (text)
  "Ask the terminal to put TEXT on the system clipboard (OSC 52); terminals that do not support it ignore it."
  (when (and *wm-live* (fboundp '%term-write-codes))
    (let ((seq (%string-concat (string (code-char 27)) "]52;c;" (%base64 text) (string (code-char 7)))))
      (ignore-errors (%term-write-codes (map 'vector #'char-code seq))))))

(defun doc-copy-code (text)
  "Copy the code block TEXT: onto the kill ring (C-y in the editor pastes it) and the terminal's clipboard."
  (kill-new text)
  (%terminal-clipboard-copy text)
  (let ((wm (if (and (boundp '*current-listener*) (typep *current-listener* 'window)) (window-wm *current-listener*) *wm*))
        (n (length (split-lines text))))
    (when wm
      (setf (wm-message wm) (format nil "Copied ~D line~:P: C-y pastes ~:*~[them~;it~:;them~] in the editor" n)
            (wm-dirty wm) t)))
  text)

(define-presentation-command (copy-code doc-example :documentation "Copy (C-y pastes it in the editor)") (text) (doc-copy-code text))
(define-presentation-command (copy-code doc-code :documentation "Copy (C-y pastes it in the editor)" :default t) (text) (doc-copy-code text))

;;; ---- commands --------------------------------------------------------------------------------------------

(define-command (doc :documentation "Show a manual topic or the documentation of a symbol") ((topic word :default ""))
  (show-doc (cond ((string= topic "") :contents)
                  ((resolve-topic topic) (let ((k (intern (string-upcase topic) "KEYWORD"))) (if (find-document k) k (%find-symbol-named topic))))
                  (t (error "No documentation for ~A (try :search-doc ~A)" topic topic)))))
(define-command (search-doc :documentation "Full-text search of the documentation") ((query string :prompt "Search for"))
  (show-doc (cons :search query)))
(define-command (contents :documentation "Show the documentation contents page") () (show-doc :contents))
(define-command (bookmark :documentation "Bookmark the topic shown in the documentation window") ()
  (let ((w (find-doc-window *wm*)))
    (if w (progn (bookmark-topic (rw-subject w)) (format t "~&Bookmarked ~A~%" (doc-window-title (rw-subject w))))
        (format t "~&No documentation window~%"))))
(define-command (bookmarks :documentation "List documentation bookmarks") ()
  (if (null *doc-bookmarks*) (format t "~&No bookmarks~%")
      (dolist (b *doc-bookmarks*)
        (let ((d (resolve-topic b)) (out *standard-output*))
          (when d (write-string "  " out) (present b :type 'doc-ref :stream out :escape nil) (format out "  ~A~%" (doc-title d)))))))

;;; ---- saved with the settings (see lisp/ui/65-settings.lisp) -------------------------------------------

(defun %validate-bookmarks (v)
  "Keep only bookmarks that name a topic or symbol that still has documentation."
  (unless (listp v) (error "bookmarks must be a list"))
  (remove-if-not (lambda (x)
                   (and x (or (symbolp x) (stringp x)) (not (eq x :contents))
                        (handler-case (resolve-topic x) (error (e) (declare (ignore e)) nil))))
                 v))

(define-setting :bookmarks :list "Documentation pages you bookmarked (b in the documentation window)."
  :session t :default nil
  :validate #'%validate-bookmarks
  :get (lambda () *doc-bookmarks*)
  :set (lambda (v) (setq *doc-bookmarks* v)))

(define-select-activity #\D "Documentation" (lambda (w) (typep w 'doc-window)) (lambda (wm) (open-doc-window wm :contents)))
