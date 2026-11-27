;;;; 30-definitions.lisp -- Zmacs-style definition commands.
;;;;
;;;; A Lisp buffer is divided into SECTIONS, one per top-level definition (a form starting in
;;;; column 0 whose head is DEFUN, DEFMACRO, DEFMETHOD, DEFVAR, ...).  Each buffer keeps three
;;;; BASELINES -- the text of every section when the file was read, when it was last saved, and when
;;;; the section was last compiled -- so the editor can find the definitions that changed since then:
;;;; Edit / List / Compile Changed Definitions.  Edit Callers, Edit Changed Definitions and friends
;;;; leave a POSSIBILITIES list that C-. (or C-x `, M-g n) steps through, as in Zmacs.  Also here:
;;;; Quick Arglist (c-sh-A), Documentation (c-sh-D) and Macro Expand Expression (c-sh-M).

(in-package :cl)

;;; ---- sections --------------------------------------------------------------------------------------

(defstruct (section (:conc-name sec-) (:constructor %make-section) (:copier nil))
  name kind start end text package key)

(defmethod print-object ((s section) stream)
  (format stream "#<SECTION ~A ~S>" (sec-kind s) (sec-name s)))

(defun %read-at (s pos package)
  "Read one object from S at POS in PACKAGE with *READ-EVAL* off.  Returns (values object next-position ok)."
  (let ((r (let ((*package* package) (*read-eval* nil))
             (handler-case (multiple-value-list (%read-from-string s pos)) (error () nil)))))
    (if (and r (not (eq (car r) (%eof-object)))) (values (car r) (cadr r) t) (values nil nil nil))))

(defun %line-start-after (s i)
  (let ((nl (%string-position-char s #\Newline i))) (if nl (+ nl 1) (length s))))

(defun %in-package-target (s pos package)
  "The package named by the IN-PACKAGE form whose argument starts at POS, or NIL."
  (multiple-value-bind (name next ok) (%read-at s pos package)
    (declare (ignore next))
    (and ok (or (symbolp name) (stringp name)) (find-package name))))

(defun text-sections (s &optional (package (or *ed-package* *package*)))
  "The definitions in the Lisp source text S, in order, as SECTION structures.  A definition is a
form that starts in column 0 and whose head names a definer.  IN-PACKAGE forms change the package
used to read the names that follow.  Nothing is evaluated."
  (let ((n (length s)) (i 0) (pkg package) (acc nil) (counts nil))
    (loop
      (when (>= i n) (return))
      (cond ((and (< (+ i 1) n) (char= (char s i) #\#) (char= (char s (+ i 1)) #\|))
             (let ((close (%string-search "|#" s (+ i 2))))       ; skip a block comment
               (setq i (if close (%line-start-after s close) n))))
            ((char= (char s i) #\()
             (let ((e (sexp-end s i)))
               (if (null e)
                   (setq i (%line-start-after s i))
                   (progn
                     (multiple-value-bind (head p ok) (%read-at s (+ i 1) pkg)
                       (when (and ok (symbolp head))
                         (if (eq head 'in-package)
                             (let ((np (%in-package-target s p pkg))) (when np (setq pkg np)))
                             (multiple-value-bind (name p2 ok2) (%read-at s p pkg)
                               (declare (ignore p2))
                               (when ok2
                                 (multiple-value-bind (dname kind) (%definition-info (list head name))
                                   (when dname
                                     (let* ((k (list kind dname)) (cell (assoc k counts :test #'equal)) (idx 0))
                                       (if cell (progn (setq idx (cdr cell)) (rplacd cell (+ idx 1))) (setq counts (cons (cons k 1) counts)))
                                       (setq acc (cons (%make-section :name dname :kind kind :start i :end e :text (subseq s i e)
                                                                      :package pkg :key (list kind dname idx))
                                                       acc))))))))))
                     (setq i (%line-start-after s e))))))
            (t (setq i (%line-start-after s i)))))
    (nreverse acc)))

(defun buffer-sections (b) (text-sections (buffer-string b)))

(defun section-at-offset (b offset)
  "The section of B containing OFFSET (or the last one starting before it), or NIL."
  (let ((best nil))
    (dolist (sec (buffer-sections b) best)
      (when (<= (sec-start sec) offset) (setq best sec)))))

(defun section-label (sec)
  (let ((*package* (or (sec-package sec) *package*)))
    (format nil "~A ~A" (string-downcase (symbol-name (sec-kind sec))) (string-downcase (prin1-to-string (sec-name sec))))))

;;; ---- baselines and changed definitions -------------------------------------------------------------------

(defparameter *baseline-kinds* '(:read :saved :compiled))

(defun %baseline (b which) (cdr (assoc which (buf-local b 'def-baselines))))

(defun %set-baseline (b which table)
  (setf (buf-local b 'def-baselines) (cons (cons which table) (remove which (buf-local b 'def-baselines) :key #'car))))

(defun buffer-reset-baselines (b &optional (which *baseline-kinds*))
  "Record the current text of every definition in B as the baseline for each of WHICH (:READ :SAVED :COMPILED)."
  (when (eq (buffer-mode b) 'lisp-mode)
    (let ((secs (buffer-sections b)))
      (dolist (w which)
        (let ((h (make-hash-table :test 'equal)))
          (dolist (sec secs) (setf (gethash (sec-key sec) h) (sec-text sec)))
          (%set-baseline b w h)))))
  (when (eq (buffer-mode b) 'fortran-mode) (f77-reset-baselines b which))
  b)

(defun buffer-note-compiled (b &optional (pred (constantly t)) sections)
  "Mark the sections of B satisfying PRED (or the given SECTIONS) as compiled with their current text."
  (when (eq (buffer-mode b) 'lisp-mode)
    (let ((h (or (%baseline b :compiled) (let ((new (make-hash-table :test 'equal))) (%set-baseline b :compiled new) new))))
      (dolist (sec (or sections (buffer-sections b)))
        (when (funcall pred sec) (setf (gethash (sec-key sec) h) (sec-text sec))))))
  b)

(defun buffer-changed-sections (b &optional (which :compiled))
  "Sections of B whose text differs from the WHICH baseline (all of them if B has none)."
  (if (eq (buffer-mode b) 'lisp-mode)
      (let ((h (%baseline b which)))
        (remove-if (lambda (sec) (and h (equal (gethash (sec-key sec) h) (sec-text sec)))) (buffer-sections b)))
      nil))

(defun %changed-which (arg)
  "No argument: since last compiled; C-u: since last saved; C-u C-u: since the file was read."
  (cond ((null arg) :compiled) ((and (integerp arg) (>= arg 16)) :read) (t :saved)))

(defun %which-phrase (which)
  (case which (:compiled "since last compiled") (:saved "since last saved") (t "since read in")))

;;; ---- possibilities (C-.) -------------------------------------------------------------------------------------

(defstruct (possibilities (:conc-name poss-) (:constructor make-possibilities (title entries)) (:copier nil))
  title entries (index -1))

(defun make-possibility (label goto) (cons label goto))

(defun ed-goto-possibility (ed n)
  (let* ((p (ed-possibilities ed)) (entries (and p (poss-entries p))))
    (cond ((null entries) (ed-message* ed "No possibilities"))
          ((or (< n 0) (>= n (length entries)))
           (ed-message* ed "No ~A possibilities (~A)" (if (< n 0) "previous" "more") (poss-title p)))
          (t (setf (poss-index p) n)
             (let ((e (nth n entries)))
               (funcall (cdr e) ed)
               (ed-message* ed "~D of ~D ~A: ~A" (+ n 1) (length entries) (poss-title p) (car e)))))))

(defun ed-set-possibilities (ed title entries &key (go t))
  "Install ENTRIES (from MAKE-POSSIBILITY) as the possibilities list; with GO, visit the first."
  (setf (ed-possibilities ed) (make-possibilities title entries))
  (cond ((null entries) (ed-message* ed "No ~A" title))
        (go (ed-goto-possibility ed 0))
        (t (list-possibilities ed))))

(defcmd next-possibility (ed arg)
  (let ((p (ed-possibilities ed)))
    (if p (ed-goto-possibility ed (+ (poss-index p) (if (integerp arg) arg 1))) (ed-message* ed "No possibilities"))))
(defcmd previous-possibility (ed arg)
  (let ((p (ed-possibilities ed)))
    (if p (ed-goto-possibility ed (- (poss-index p) (if (integerp arg) arg 1))) (ed-message* ed "No possibilities"))))

(defun list-possibilities (ed)
  (let ((p (ed-possibilities ed)))
    (if (null p)
        (ed-message* ed "No possibilities")
        (let ((b (ed-get-buffer ed "*Possibilities*" t)) (i 0))
          (push-mark ed)
          (setf (buffer-read-only b) nil)
          (buffer-set-text b (join-lines (cons (format nil "~A (~D):" (poss-title p) (length (poss-entries p)))
                                               (mapcar (lambda (e) (setq i (+ i 1)) (format nil "~A ~3D  ~A" (if (= (- i 1) (poss-index p)) ">" " ") i (car e)))
                                                       (poss-entries p)))))
          (setf (buffer-read-only b) t (buffer-mode b) 'possibilities-mode)
          (ed-switch-to-buffer ed b)
          (buf-goto b (min (- (buf-nlines b) 1) (+ 1 (max 0 (poss-index p)))) 0)
          b))))

(defcmd list-possibilities (ed arg) (list-possibilities ed))
(defcmd possibilities-goto (ed arg)
  (with-b (b ed) (let ((n (- (point-line b) 1))) (if (>= n 0) (ed-goto-possibility ed n) (ed-message* ed "Not on a possibility")))))
(defcmd possibilities-next-line (ed arg) (with-b (b ed) (buf-move-vertically b 1)))
(defcmd possibilities-previous-line (ed arg) (with-b (b ed) (buf-move-vertically b -1) (when (< (point-line b) 1) (buf-goto b (min 1 (- (buf-nlines b) 1)) 0))))
(defcmd possibilities-quit (ed arg) (run-editor-command ed 'kill-buffer nil))

;;; ---- going to a section --------------------------------------------------------------------------------

(defun ed-show-offset (ed b offset)
  (push-mark ed)
  (unless (member b (ed-buffers ed)) (error "Buffer ~A has been killed" (buffer-name b)))
  (ed-switch-to-buffer ed b)
  (goto-offset b (max 0 (min offset (length (buffer-string b)))))
  (setf (ed-top ed) (max 0 (- (point-line b) 3)))
  b)

(defun %section-possibility (b sec)
  "A possibility that visits SEC in B, found again by its key in case the buffer was edited since."
  (let ((key (sec-key sec)))
    (make-possibility (format nil "~A  (~A)" (section-label sec) (buffer-name b))
                      (lambda (ed)
                        (let ((cur (find key (buffer-sections b) :key #'sec-key :test #'equal)))
                          (ed-show-offset ed b (sec-start (or cur sec))))))))

;;; ---- changed definitions ---------------------------------------------------------------------------------

(defun %lisp-buffers (ed) (remove-if-not (lambda (b) (eq (buffer-mode b) 'lisp-mode)) (ed-buffers ed)))

(defun changed-definitions (ed buffers which)
  "List of (buffer . section) for every changed definition in BUFFERS."
  (let ((acc nil))
    (dolist (b buffers) (dolist (sec (buffer-changed-sections b which)) (setq acc (cons (cons b sec) acc))))
    (nreverse acc)))

(defun %edit-changed (ed buffers arg go)
  (let ((which (%changed-which arg)))
    (ed-set-possibilities ed (format nil "definitions changed ~A" (%which-phrase which))
                          (mapcar (lambda (c) (%section-possibility (car c) (cdr c))) (changed-definitions ed buffers which))
                          :go go)))

(defcmd edit-changed-definitions (ed arg) (%edit-changed ed (%lisp-buffers ed) arg t))
(defcmd edit-changed-definitions-of-buffer (ed arg) (%edit-changed ed (list (ed-buffer ed)) arg t))
(defcmd list-changed-definitions (ed arg) (%edit-changed ed (%lisp-buffers ed) arg nil))
(defcmd list-changed-definitions-of-buffer (ed arg) (%edit-changed ed (list (ed-buffer ed)) arg nil))

(defun compile-section (b sec)
  "Evaluate (compile) the definition SEC of buffer B in its package; record its source location when B
visits a file and mark it compiled.  Returns NIL, or the error."
  (handler-case
      (let ((*package* (or (sec-package sec) *package*)) (text (sec-text sec)))
        (let ((*standard-output* (make-string-output-stream)))
          (multiple-value-bind (form p ok) (%read-at text 0 *package*)
            (declare (ignore p))
            (unless ok (error "Cannot read ~A" (section-label sec)))
            (eval form)))
        (when (buffer-file b) (%note-definition (sec-name sec) (sec-kind sec) (buffer-file b) (sec-start sec)))
        (buffer-note-compiled b (constantly t) (list sec))
        nil)
    (error (e) e)))

(defun %compile-changed (ed buffers arg)
  (let* ((which (%changed-which arg)) (changed (changed-definitions ed buffers which)) (done nil) (failed nil))
    (dolist (c changed)
      (let ((err (compile-section (car c) (cdr c))))
        (if err (setq failed (cons (cons (cdr c) err) failed)) (setq done (cons (cdr c) done)))))
    (setq done (nreverse done) failed (nreverse failed))
    (cond ((null changed) (ed-message* ed "No definitions changed ~A" (%which-phrase which)))
          (failed (ed-message* ed "Compiled ~D, ~D failed: ~A: ~A" (length done) (length failed)
                               (section-label (car (car failed))) (cdr (car failed))))
          (t (ed-message* ed "Compiled ~D definition~:P: ~{~A~^, ~}" (length done)
                          (mapcar (lambda (sec) (let ((*package* (sec-package sec))) (string-downcase (prin1-to-string (sec-name sec))))) done))))
    (values done failed)))

(defcmd compile-changed-definitions (ed arg) (%compile-changed ed (%lisp-buffers ed) arg))
(defcmd compile-changed-definitions-of-buffer (ed arg) (%compile-changed ed (list (ed-buffer ed)) arg))

;;; ---- symbols near point ---------------------------------------------------------------------------------

(defun buffer-package-at (b offset)
  "The package in effect at OFFSET of B: the last column-0 IN-PACKAGE form before it, else the editor's package."
  (let* ((s (buffer-string b)) (pkg (or *ed-package* *package*)) (i 0) (pat "(in-package "))
    (loop
      (let ((p (%string-search pat s i)))
        (when (or (null p) (>= p offset)) (return pkg))
        (when (or (= p 0) (char= (char s (- p 1)) #\Newline))
          (let ((np (%in-package-target s (+ p (length pat)) pkg))) (when np (setq pkg np))))
        (setq i (+ p 1))))))

(defmacro with-point-package ((b) &body body)
  `(let ((*ed-package* (buffer-package-at ,b (point-offset ,b)))) ,@body))

(defun %token-after (s i)
  "The token starting at I (after whitespace), or NIL."
  (let ((n (length s)))
    (loop (when (or (>= i n) (not (member (char s i) '(#\Space #\Tab #\Newline)))) (return)) (setq i (+ i 1)))
    (let ((e i))
      (loop (when (or (>= e n) (%delimiter-p (char s e))) (return)) (setq e (+ e 1)))
      (and (> e i) (subseq s i e)))))

(defun %operator-p (sym) (and sym (symbolp sym) (or (fboundp sym) (macro-function sym) (%find-gf sym))))

(defun function-at-point (b)
  "The function named by the operator of the innermost list around point that names one, else the
function named by the symbol at point, else NIL."
  (let* ((s (buffer-string b)) (off (point-offset b)))
    (or (dolist (open (sexp-context s off) nil)
          (let* ((tok (%token-after s (+ open 1))) (sym (and tok (%find-symbol-named tok))))
            (when (%operator-p sym) (return sym))))
        (let* ((tok (symbol-at-point b)) (sym (and tok (%find-symbol-named tok))))
          (and (%operator-p sym) sym)))))

(defun %symbol-at-point* (b)
  (let ((tok (symbol-at-point b))) (and tok (%find-symbol-named tok))))

(defun %lisp-downcase (string)
  "Downcase printed Lisp, leaving string literals, |escaped| names and character names alone."
  (let ((out (make-string-output-stream)) (i 0) (n (length string)) (state nil))
    (loop
      (when (>= i n) (return))
      (let ((c (char string i)))
        (cond ((eq state :string)
               (write-char c out)
               (cond ((char= c #\\) (when (< (+ i 1) n) (write-char (char string (+ i 1)) out) (setq i (+ i 1))))
                     ((char= c #\") (setq state nil))))
              ((eq state :bar) (write-char c out) (when (char= c #\|) (setq state nil)))
              ((char= c #\") (write-char c out) (setq state :string))
              ((char= c #\|) (write-char c out) (setq state :bar))
              ((and (char= c #\#) (< (+ i 2) n) (char= (char string (+ i 1)) #\\))
               (write-char c out) (write-char #\\ out) (write-char (char string (+ i 2)) out) (setq i (+ i 2)))
              (t (write-char (char-downcase c) out))))
      (setq i (+ i 1)))
    (get-output-stream-string out)))

;;; ---- Quick Arglist, Documentation, Macro Expand Expression ----------------------------------------------------

(defun %fn-label (sym) (string-downcase (prin1-to-string sym)))

(defcmd quick-arglist (ed arg)
  (with-b (b ed)
    (with-point-package (b)
      (let ((sym (function-at-point b)))
        (if (null sym)
            (ed-message* ed "No function near point")
            (let ((str (arglist-string sym)))
              (ed-message* ed "~A~A" (or str (format nil "~A: lambda list unknown" (%fn-label sym)))
                           (if (macro-function sym) "  [macro]" (if (%find-gf sym) "  [generic]" "")))))))))

(defun %symbol-doc (sym)
  (or (documentation sym 'function) (documentation sym 'variable) (documentation sym 'type)))

(defun %doc-symbol-near-point (b)
  (let ((here (%symbol-at-point* b)))
    (if (and here (%symbol-doc here)) here (or (function-at-point b) here))))

(defun show-documentation-buffer (ed sym)
  (let* ((lines nil) (arglist (and (%operator-p sym) (arglist-string sym))))
    (when arglist (setq lines (list arglist "")))
    (dolist (spec '((function "Function") (variable "Variable") (type "Type")))
      (let ((d (documentation sym (car spec))))
        (when d (setq lines (append lines (list (format nil "~A documentation:" (cadr spec))) (split-lines d) (list ""))))))
    (let ((locs (definition-locations sym)))
      (when locs (setq lines (append lines (list "Defined in:")
                                     (mapcar (lambda (l) (format nil "  ~A ~A" (string-downcase (symbol-name (car l))) (cadr l))) locs)))))
    (push-mark ed)
    (%show-text-buffer ed "*Documentation*"
                       (join-lines (cons (format nil "~A" (%fn-label sym)) (cons "" (or lines (list "No documentation.")))))
                       'text-mode)))

;;; Brief documentation of the symbol or function near point in the echo area; with an argument, all of it in a buffer.
(defcmd show-documentation (ed arg)
  (with-b (b ed)
    (with-point-package (b)
      (let ((sym (%doc-symbol-near-point b)))
        (cond ((null sym) (ed-message* ed "No symbol near point"))
              (arg (show-documentation-buffer ed sym))
              (t (let ((d (%symbol-doc sym)))
                   (ed-message* ed "~A: ~A" (%fn-label sym)
                                (if d (car (split-lines d)) "not documented")))))))))
(defcmd long-documentation (ed arg)
  (with-b (b ed)
    (with-point-package (b)
      (let ((sym (%doc-symbol-near-point b)))
        (if sym (show-documentation-buffer ed sym) (ed-message* ed "No symbol near point"))))))

(defun %expression-start (b)
  "Offset of the expression following point, or of the innermost list around point."
  (let* ((s (buffer-string b)) (i (%skip-ws s (point-offset b))))
    (cond ((and (< i (length s)) (char= (char s i) #\()) i)
          (t (car (sexp-context s (point-offset b)))))))

(defun macro-expand-expression (ed all)
  (with-b (b ed)
    (with-point-package (b)
      (let* ((s (buffer-string b)) (st (%expression-start b)))
        (multiple-value-bind (form p ok) (if st (%read-at s st *ed-package*) (values nil nil nil))
          (declare (ignore p))
          (if (not ok)
              (ed-message* ed "No expression at point")
              (let* ((*package* *ed-package*)
                     (exp (if all (macroexpand form) (macroexpand-1 form)))
                     (one (%lisp-downcase (prin1-to-string exp))))
                (if (and (not (%string-position-char one #\Newline 0)) (<= (length one) (max 20 (- (window-w ed) 4))))
                    (ed-message* ed "~A" one)
                    (progn (push-mark ed)
                           (%show-text-buffer ed "*Macroexpansion*" (%lisp-downcase (pretty-string exp (max 40 (- (window-w ed) 4)))) 'lisp-mode))))))))))

(defcmd macro-expand-expression (ed arg) (macro-expand-expression ed arg))
(defcmd macro-expand-expression-all (ed arg) (macro-expand-expression ed t))

;;; ---- Edit Callers ----------------------------------------------------------------------------------------

(defun callers-of (sym)
  "Every function, macro or method whose compiled code calls, references, sets or binds SYM (the
cross-reference is rebuilt first, so new definitions are included).  Open-coded primitives are not seen."
  (xref-build)
  (let ((acc nil))
    (dolist (f (list #'who-calls #'who-references #'who-sets #'who-binds))
      (dolist (s (funcall f sym)) (setq acc (adjoin s acc))))
    (sort acc (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(defun %goto-name-in-definition (b name)
  "Move point from the start of a definition to the first use of NAME inside it."
  (let* ((s (buffer-string b)) (start (point-offset b)) (end (or (sexp-end s start) (length s)))
         (lower (string-downcase name)) (i (+ start 1)))
    (loop
      (let ((p (or (%string-search lower s i) (%string-search name s i))))
        (when (or (null p) (>= p end)) (return nil))
        (when (and (> p start) (%delimiter-p (char s (- p 1)))
                   (or (>= (+ p (length name)) (length s)) (%delimiter-p (char s (+ p (length name))))))
          (unless (= p (+ start 1 (length (or (%token-after s (+ start 1)) "")) 1))   ; not the definition's own name
            (goto-offset b p) (return p)))
        (setq i (+ p 1))))))

(defun %caller-possibilities (sym callers)
  (let ((acc nil) (name (symbol-name sym)))
    (dolist (c callers)
      (let ((locs (definition-locations c)))
        (if (null locs)
            (setq acc (cons (make-possibility (format nil "~A  (no source recorded)" (%fn-label c))
                                              (lambda (ed) (ed-message* ed "No source recorded for ~A" (%fn-label c))))
                            acc))
            (dolist (l locs)
              (let ((file (cadr l)) (pos (caddr l)))
                (setq acc (cons (make-possibility (format nil "~A ~A  (~A)" (string-downcase (symbol-name (car l))) (%fn-label c) (path-filename file))
                                                  (lambda (ed)
                                                    (let ((b (ed-goto-location ed file pos)))
                                                      (%goto-name-in-definition b name))))
                                acc)))))))
    (nreverse acc)))

(defun %read-function-name (ed prompt fn)
  (let* ((b (ed-buffer ed)) (default (with-point-package (b) (or (%symbol-at-point* b) (function-at-point b))))
         (pkg (buffer-package-at b (point-offset b))))
    (ed-read-string ed (if default (format nil "~A (default ~A): " prompt (%fn-label default)) (format nil "~A: " prompt))
                    (lambda (text)
                      (let ((sym (if (string= text "") default (let ((*ed-package* pkg)) (%find-symbol-named text)))))
                        (if sym (funcall fn sym) (ed-message* ed "No symbol named ~A" text)))))))

(defun edit-callers (ed sym &key (go t))
  (ed-set-possibilities ed (format nil "callers of ~A" (%fn-label sym)) (%caller-possibilities sym (callers-of sym)) :go go))

(defcmd edit-callers (ed arg) (%read-function-name ed "Edit callers of" (lambda (sym) (edit-callers ed sym))))
(defcmd list-callers (ed arg) (%read-function-name ed "List callers of" (lambda (sym) (edit-callers ed sym :go nil))))

;;; ---- key bindings ----------------------------------------------------------------------------------------
;;; Terminals cannot send c-sh-A or C-., so each command also has a C-c / C-x binding.

(dolist (b '(("C-." next-possibility) ("C-x `" next-possibility) ("M-g n" next-possibility) ("M-g p" previous-possibility)
             ("C-x C-." list-possibilities)))
  (bind (car b) (cadr b)))

(dolist (b '(("C-S-a" quick-arglist) ("C-c C-a" quick-arglist)
             ("C-S-d" show-documentation) ("C-c C-d" show-documentation) ("M-S-d" long-documentation) ("C-c M-d" long-documentation)
             ("C-S-m" macro-expand-expression) ("C-c RET" macro-expand-expression)
             ("M-S-m" macro-expand-expression-all) ("C-c M-RET" macro-expand-expression-all)))
  (bind-lisp (car b) (cadr b)))

(dolist (b '(("<return>" possibilities-goto) ("n" possibilities-next-line) ("p" possibilities-previous-line)
             ("<down>" possibilities-next-line) ("<up>" possibilities-previous-line) ("q" possibilities-quit)))
  (define-key *possibilities-mode-map* (car b) (cadr b)))
