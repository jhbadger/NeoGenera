;;;; 45-accept.lisp -- typed input: ACCEPT, ACCEPTING-VALUES and presentation translators.
;;;;
;;;; ACCEPT reads an object of a presentation type.  In the window Listener the prompt replaces the Listener's
;;;; prompt and a nested event loop runs until the answer is typed or a matching presentation is clicked,
;;;; anywhere on the screen.  Elsewhere it reads a line.  A presentation matches when its object is of the
;;;; type, or when a PRESENTATION TRANSLATOR turns it into one.  ACCEPTING-VALUES shows the ACCEPTs of its
;;;; body as the fields of one dialog, which can be filled in any order; the body runs again after each change.
;;;;
;;;; Presentation types are the CLOS classes and types (integer, string, symbol, ...), the types defined with
;;;; DEFINE-PRESENTATION-TYPE, and BOOLEAN, EXPRESSION, PATHNAME, (MEMBER ...), (INTEGER low high),
;;;; (OR type ...), (SEQUENCE type) and (NULL-OR-TYPE type).

(in-package :cl)

(define-condition input-not-of-required-type (simple-error) ()
  (:documentation "Signalled by ACCEPT-FROM-STRING for text that is not of the type asked for."))

(defun %bad-input (fmt &rest args) (error 'input-not-of-required-type :format-control fmt :format-arguments args))

(defun %ptype-name (type) (if (consp type) (car type) type))

(defun %plural-type (type)
  (let ((d (describe-presentation-type type)))
    (cond ((and (> (length d) 3) (string= (subseq d 0 3) "an ")) (%string-concat (subseq d 3) "s"))
          ((and (> (length d) 2) (string= (subseq d 0 2) "a ")) (%string-concat (subseq d 2) "s"))
          (t d))))

(defun describe-presentation-type (type)
  "A phrase describing presentation TYPE for a prompt, such as \"an integer from 1 to 10\"."
  (let ((name (%ptype-name type)) (args (and (consp type) (cdr type))))
    (case name
      ((t) "an object")
      (expression "an expression")
      (integer (cond ((and args (integerp (car args)) (integerp (cadr args))) (format nil "an integer from ~D to ~D" (car args) (cadr args)))
                     ((and args (integerp (car args))) (format nil "an integer of at least ~D" (car args)))
                     ((and (cdr args) (integerp (cadr args))) (format nil "an integer up to ~D" (cadr args)))
                     (t "an integer")))
      (member (format nil "one of ~{~A~^, ~}" (mapcar (lambda (m) (string-downcase (princ-to-string m))) args)))
      (or (format nil "~{~A~^ or ~}" (mapcar #'describe-presentation-type args)))
      (sequence (format nil "~A separated by commas" (%plural-type (car args))))
      (null-or-type (format nil "~A, or nothing" (describe-presentation-type (car args))))
      (boolean "yes or no")
      (pathname "a file name")
      (t (let ((s (string-downcase (substitute #\Space #\- (symbol-name name)))))
           (format nil "~A ~A" (if (find (char s 0) "aeiou") "an" "a") s))))))

(defun %in-bounds-p (x args)
  (let ((lo (car args)) (hi (cadr args)))
    (and (or (not (realp lo)) (>= x lo)) (or (not (realp hi)) (<= x hi)))))

(defun presentation-typep (object type)
  "True if OBJECT is of presentation TYPE."
  (let ((name (%ptype-name type)) (args (and (consp type) (cdr type))))
    (case name
      ((t expression) t)
      (boolean (or (eq object t) (eq object nil)))
      (integer (and (integerp object) (%in-bounds-p object args)))
      ((number real float rational ratio) (and (ignore-errors (typep object name)) (or (not (realp object)) (%in-bounds-p object args))))
      (member (and (member object args :test #'equal) t))
      (or (some (lambda (ty) (presentation-typep object ty)) args))
      (null-or-type (or (null object) (presentation-typep object (car args))))
      (sequence (and (listp object) (every (lambda (x) (presentation-typep x (car args))) object)))
      (pathname (stringp object))
      (t (and (symbolp name) (or (and (ignore-errors (typep object name)) t)
                                 (member name (presentation-supertypes (presentation-type-of object)))))))))

;;; ---- translators ----------------------------------------------------------------------------------------

(defstruct (translator (:constructor %make-translator) (:copier nil)) name from to documentation fn tester)
(defvar *presentation-translators* nil)

(defun %register-translator (name from to doc fn tester)
  (setq *presentation-translators*
        (cons (%make-translator :name name :from from :to to :documentation doc :fn fn :tester tester)
              (remove name *presentation-translators* :key #'translator-name)))
  name)

(defmacro define-presentation-translator (name (from-type to-type &key (documentation "") tester) (object) &body body)
  "Let a presentation of FROM-TYPE be used where TO-TYPE is accepted: BODY computes the object to use from
OBJECT.  TESTER, a function of OBJECT, can decline."
  `(%register-translator ',name ',from-type ',to-type ,documentation (lambda (,object) ,@body)
                         ,(if tester `(function ,tester) nil)))

(defun presentation-acceptable-p (pres type)
  "Whether presentation PRES can be used to answer an ACCEPT of TYPE: (values ok object how), where HOW is NIL
for the object itself or the documentation of the translator used."
  (let ((obj (pres-object pres)) (ptype (pres-type pres)))
    (cond ((or (and (symbolp type) (member type (presentation-supertypes ptype))) (presentation-typep obj type))
           (values t obj nil))
          (t (let ((supers (presentation-supertypes ptype)))
               (dolist (tr *presentation-translators* (values nil nil nil))
                 (when (and (member (translator-from tr) supers)
                            (or (null (translator-tester tr)) (ignore-errors (funcall (translator-tester tr) obj))))
                   (multiple-value-bind (v ok) (handler-case (values (funcall (translator-fn tr) obj) t) (error () (values nil nil)))
                     (when (and ok (presentation-typep v type))
                       (return (values t v (translator-documentation tr))))))))))))

(define-presentation-translator frame-function (frame symbol :documentation "its function's name" :tester cadr) (f) (cadr f))
(define-presentation-translator condition-message (condition string :documentation "its message") (c) (condition-report-string c))
(define-presentation-translator process-name (process string :documentation "its name") (p) (process-name p))

;;; ---- parsing typed text -----------------------------------------------------------------------------------

(defun %first-token (text) (car (%split-tokens text)))

(defun %read-one (text)
  (let ((*read-eval* nil))
    (multiple-value-bind (obj pos) (handler-case (%read-from-string text 0) (error (e) (%bad-input "~A" e)))
      (when (eq obj (%eof-object)) (%bad-input "Nothing typed"))
      (unless (= 0 (length (string-trim " " (subseq text pos)))) (%bad-input "Extra text after ~A" (subseq text 0 pos)))
      obj)))

(defun accept-from-string (type string)
  "Parse STRING as an object of presentation TYPE.  Signals INPUT-NOT-OF-REQUIRED-TYPE if it is not one."
  (let* ((text (string-trim '(#\Space #\Tab) string)) (name (%ptype-name type)) (args (and (consp type) (cdr type))))
    (case name
      (string (if (and (> (length text) 1) (char= (char text 0) #\")) (%read-one text) text))
      (pathname (when (= (length text) 0) (%bad-input "No file name"))
                (if (and (> (length text) 1) (string= (subseq text 0 2) "~/") (getenv "HOME"))
                    (%string-concat (getenv "HOME") (subseq text 1)) text))
      (null-or-type (if (or (= (length text) 0) (string-equal text "none")) nil (accept-from-string (car args) text)))
      (or (dolist (ty args (%bad-input "~A is not ~A" text (describe-presentation-type type)))
            (multiple-value-bind (v ok) (handler-case (values (accept-from-string ty text) t) (error () (values nil nil)))
              (when ok (return v)))))
      (sequence (mapcar (lambda (part) (accept-from-string (car args) part))
                        (remove "" (mapcar (lambda (s) (string-trim " " s)) (%split-string text #\,)) :test #'string=)))
      (t
       (when (= (length text) 0) (%bad-input "Nothing typed: ~A wanted" (describe-presentation-type type)))
       (case name
         (boolean (cond ((member text '("yes" "y" "t" "true" "on") :test #'string-equal) t)
                        ((member text '("no" "n" "nil" "false" "off") :test #'string-equal) nil)
                        (t (%bad-input "~A is not yes or no" text))))
         (member (let* ((names (mapcar (lambda (m) (string-downcase (princ-to-string m))) args))
                        (exact (position text names :test #'string-equal))
                        (prefixed (remove-if-not (lambda (n) (and (<= (length text) (length n)) (string-equal text (subseq n 0 (length text))))) names)))
                   (cond (exact (nth exact args))
                         ((and prefixed (null (cdr prefixed))) (nth (position (car prefixed) names :test #'string=) args))
                         (prefixed (%bad-input "~A is ambiguous: ~{~A~^, ~}" text prefixed))
                         (t (%bad-input "~A is not ~A" text (describe-presentation-type type))))))
         (package (or (find-package (string-upcase (string-left-trim ":" text))) (%bad-input "There is no package named ~A" text)))
         (keyword (intern (string-upcase (string-left-trim ":" text)) "KEYWORD"))
         (character (cond ((= (utf8-length text) 1) (if (< (char-code (char text 0)) 128) (char text 0) (code-char (utf8-char-at text 0))))
                          (t (let ((c (ignore-errors (%read-one (if (and (> (length text) 1) (string= (subseq text 0 2) "#\\")) text (%string-concat "#\\" text))))))
                               (if (characterp c) c (%bad-input "~A is not a character" text))))))
         (t (let ((obj (%read-one text)))
              (unless (presentation-typep obj type) (%bad-input "~A is not ~A" text (describe-presentation-type type)))
              obj)))))))

(defun accept-completions (type prefix)
  "Completions of PREFIX for presentation TYPE (names of members, packages, symbols ...)."
  (let ((name (%ptype-name type)) (args (and (consp type) (cdr type))))
    (flet ((starting (names) (sort (remove-if-not (lambda (n) (and (<= (length prefix) (length n)) (string-equal prefix (subseq n 0 (length prefix))))) names) #'string<)))
      (case name
        (member (starting (mapcar (lambda (m) (string-downcase (princ-to-string m))) args)))
        (boolean (starting '("yes" "no")))
        (package (starting (mapcar (lambda (p) (string-downcase (package-name p))) (list-all-packages))))
        ((symbol function) (complete-symbol-names prefix))
        (keyword (starting (mapcar (lambda (s) (string-downcase (symbol-name s))) (%package-symbols (find-package "KEYWORD") nil))))
        (pathname (and (fboundp 'file-completions) (funcall 'file-completions prefix)))
        (or (let ((acc nil)) (dolist (ty args) (setq acc (union acc (accept-completions ty prefix) :test #'string=))) (sort acc #'string<)))
        (null-or-type (accept-completions (car args) prefix))
        (t nil)))))

;;; ---- input contexts: what a click on a presentation would supply ----------------------------------------------

(defstruct (input-context (:conc-name ic-) (:constructor make-input-context) (:copier nil)) type-fn label supply)

(defvar *input-contexts* nil "The input contexts of the ACCEPTs waiting for input, innermost first.")
(defvar *pending-listener* nil "The Listener whose command is waiting for an argument.")

(defun %command-arg-ptype (type) (if (eq type 'word) 'string type))

(defun current-input-context ()
  "The input context a click on a presentation answers, or NIL."
  (or (car *input-contexts*)
      (let ((w *pending-listener*))
        (when (and w (lw-pending w))
          (let ((spec (car (cadddr (lw-pending w)))))
            (make-input-context :type-fn (let ((ty (%command-arg-ptype (cadr spec)))) (lambda () ty))
                                :label (cadddr spec) :supply (lambda (obj) (%supply-pending w obj))))))))

(defun %context-match (pres)
  "If the current input context accepts PRES: (values context object how)."
  (let ((ctx (current-input-context)))
    (when ctx
      (let ((type (funcall (ic-type-fn ctx))))
        (when type
          (multiple-value-bind (ok obj how) (presentation-acceptable-p pres type)
            (when ok (values ctx obj how))))))))

(defun %supply-pending (w obj)
  "A click answered the argument the command in listener W is waiting for."
  (let ((pending (lw-pending w)) (out (lw-stream w)) (result nil))
    (setf (lw-pending w) nil *pending-listener* nil)
    (with-style (out :prompt) (write-string (%prompt-string-for-pending pending) out))
    (present obj :stream out) (terpri out)
    (let ((value (if (and (eq (cadr (car (cadddr pending))) 'expression)    ; such arguments are forms: quote the object
                          (not (or (numberp obj) (stringp obj) (characterp obj) (keywordp obj) (eq obj t) (eq obj nil))))
                     (list 'quote obj) obj)))
      (%in-listener w (lambda () (setq result (continue-command-with-value pending value)))))
    (when (and (consp result) (eq (car result) :prompt)) (setf (lw-pending w) result *pending-listener* w))
    (setf (wm-dirty (window-wm w)) t)))

(defun %prompt-string-for-pending (pending) (%string-concat (%pending-prompt pending) ": "))

;;; ---- serving the window system while waiting ---------------------------------------------------------------

(defun %nested-serve (w done-p)
  "Serve the window system (as the top-level loop would) until DONE-P returns true.  Quitting, or the end of a
headless script, abandons the Listener evaluation in progress."
  (let* ((wm (window-wm w)) (o *outside-listener*)
         (*standard-output* (first o)) (*error-output* (second o)) (*standard-input* (third o))
         (*package* (fourth o)) (*debugger-hook* (fifth o)) (*current-listener* (sixth o)))
    (setf (wm-busy wm) nil (wm-dirty wm) t)
    (loop
      (when (funcall done-p) (return))
      (when (wm-quit wm) (throw 'listener-abort nil))
      (if *wm-script*
          (let ((ev (wm-next-scripted-event)))
            (unless ev (throw 'listener-abort nil))
            (wm-handle-event wm ev))
          (wm-serve wm)))
    (setf (wm-busy wm) t (wm-dirty wm) t)))

(defun %interactive-listener-p (w stream)
  (and w (typep w 'listener-window) (typep stream 'pane-stream) (window-wm w) (or *wm-script* *wm-live*)))

;;; ---- ACCEPT ----------------------------------------------------------------------------------------------

(defstruct (accept-state (:conc-name as-) (:constructor make-accept-state) (:copier nil))
  type prompt default default-p (done nil) value)

(defvar *accepting-values* nil "The ACCEPTING-VALUES dialog whose body is running, or NIL.")

(defun %accept-prompt (type prompt default default-p)
  (format nil "~A (~A)~:[~; [default ~A]~]" (or prompt "Enter") (describe-presentation-type type) default-p
          (and default-p (if (stringp default) default (%abbrev default 30)))))

(defun accept (type &key prompt (default nil default-p) (stream *standard-output*))
  "Read an object of presentation TYPE and return it (and TYPE).  PROMPT names what is wanted; DEFAULT is used
when nothing is typed.  In the window Listener, clicking any presentation on the screen that matches (or that a
translator turns into one) answers too; inside ACCEPTING-VALUES, the ACCEPT is a field of the dialog."
  (cond (*accepting-values* (values (%avv-accept *accepting-values* type prompt default default-p) type))
        ((%interactive-listener-p *current-listener* stream)
         (values (%window-accept *current-listener* type prompt default default-p) type))
        (t (values (%line-accept type prompt default default-p) type))))

(defun %line-accept (type prompt default default-p)
  (loop
    (format t "~&~A: " (%accept-prompt type prompt default default-p))
    (finish-output)
    (when (fboundp '%stdin-wait) (ignore-errors (%stdin-wait *standard-input*)))
    (let ((line (read-line *standard-input* nil :eof)))
      (when (eq line :eof) (error "End of input while reading ~A" (describe-presentation-type type)))
      (if (and default-p (= 0 (length (string-trim " " line))))
          (return default)
          (handler-case (return (accept-from-string type line))
            (input-not-of-required-type (e) (format t "~&~A~%" e)))))))

(defun %accept-finish (w st obj &optional echo)
  (when echo
    (let ((out (lw-stream w)))
      (with-style (out :prompt) (write-string (%prompt-string w) out))
      (present obj :stream out)
      (terpri out)))
  (setf (as-value st) obj (as-done st) t)
  (let ((wm (window-wm w))) (when (member w (wm-windows wm)) (wm-raise wm w)) (setf (wm-dirty wm) t)))

(defun %window-accept (w type prompt default default-p)
  (let* ((st (make-accept-state :type type :prompt (%accept-prompt type prompt default default-p) :default default :default-p default-p))
         (ctx (make-input-context :type-fn (lambda () type) :label (or prompt (describe-presentation-type type))
                                  :supply (lambda (obj) (%accept-finish w st obj t))))
         (*input-contexts* (cons ctx *input-contexts*)))
    (setf (lw-accepts w) (cons st (lw-accepts w)))
    (unwind-protect (%nested-serve w (lambda () (as-done st)))
      (setf (lw-accepts w) (remove st (lw-accepts w))))
    (as-value st)))

(defun %accept-submit (w text)
  "The Listener's input line was submitted while an ACCEPT waits: parse it, or report why not."
  (let ((st (car (lw-accepts w))) (out (lw-stream w)))
    (if (and (as-default-p st) (= 0 (length (string-trim " " text))))
        (%accept-finish w st (as-default st))
        (handler-case (%accept-finish w st (accept-from-string (as-type st) text))
          (error (e) (with-style (out :error) (format out "~A~%" e)))))))

(defun %abort-accept (w)
  "Give up the ACCEPT waiting in W, abandoning its evaluation."
  (declare (ignore w))
  (let ((r (find-restart 'abort))) (when r (invoke-restart r))))

;;; ---- ACCEPTING-VALUES ------------------------------------------------------------------------------------

(defstruct (avv (:constructor make-avv) (:copier nil))
  label (values (make-hash-table :test 'equal)) fields new-fields (counts nil) (selected 0)
  editing (input "") (pos 0) error done aborted thunk window)

;;; A field: (key type prompt value)
(defun %avv-accept (st type prompt default default-p)
  (let* ((label (or prompt (describe-presentation-type type)))
         (n (let ((c (assoc label (avv-counts st) :test #'string=))) (if c (setf (cdr c) (+ 1 (cdr c))) (progn (push (cons label 0) (avv-counts st)) 0))))
         (key (cons label n)))
    (multiple-value-bind (v found) (gethash key (avv-values st))
      (let ((value (if found v (if default-p default nil))))
        (unless found (when default-p (setf (gethash key (avv-values st)) default)))
        (push (list key type label value) (avv-new-fields st))
        value))))

(defun %avv-run (st &optional (output (make-broadcast-stream)))
  "Run the body to find the fields and their values; returns the body's values."
  (setf (avv-new-fields st) nil (avv-counts st) nil)
  (let ((vals (let ((*accepting-values* st) (*standard-output* output)) (multiple-value-list (funcall (avv-thunk st))))))
    (setf (avv-fields st) (nreverse (avv-new-fields st)))
    (setf (avv-selected st) (max 0 (min (avv-selected st) (- (length (avv-fields st)) 1))))
    vals))

(defmacro accepting-values ((&optional (stream '*standard-output*) &key (label "Values")) &body body)
  "Run BODY, whose ACCEPTs become the fields of one dialog: the fields can be filled in any order, and BODY
runs again after each change (so fields may depend on each other).  Done (End) runs BODY a last time with the
values and returns what it returns; Abort (Escape) abandons the evaluation.  Outside the window system the
ACCEPTs simply prompt one after another."
  `(%accepting-values (lambda () ,@body) ,label ,stream))

(defun %accepting-values (thunk label stream)
  (if (not (%interactive-listener-p *current-listener* stream))
      (funcall thunk)
      (let* ((w *current-listener*) (wm (window-wm w)) (st (make-avv :label label :thunk thunk)))
        (%avv-run st)
        (let* ((win (make-instance 'avv-window :avv st :title label))
               (ctx (make-input-context :type-fn (lambda () (let ((f (nth (avv-selected st) (avv-fields st)))) (and f (cadr f))))
                                        :label label
                                        :supply (lambda (obj) (%avv-set st (nth (avv-selected st) (avv-fields st)) obj)))))
          (setf (avv-window st) win)
          (%avv-fit st wm)
          (wm-add-window wm win)
          (let ((*input-contexts* (cons ctx *input-contexts*)))
            (unwind-protect (%nested-serve w (lambda () (or (avv-done st) (avv-aborted st))))
              (wm-close-window wm win)
              (when (member w (wm-windows wm)) (wm-raise wm w))))
          (when (avv-aborted st) (let ((r (find-restart 'abort))) (if r (invoke-restart r) (error "Aborted"))))
          (values-list (%avv-run st *standard-output*))))))

(defun %avv-set (st field obj)
  (when field
    (setf (gethash (car field) (avv-values st)) obj (avv-error st) nil (avv-editing st) nil)
    (%avv-run st)
    (let ((win (avv-window st)))
      (when (and win (window-wm win))
        (%avv-fit st (window-wm win))
        (wm-raise (window-wm win) win)                    ; a click in another window must not leave the dialog behind
        (setf (wm-dirty (window-wm win)) t)))))

;;; ---- the dialog window ---------------------------------------------------------------------------------

(defclass avv-window (window) ((avv :initarg :avv :accessor avv-window-avv)))

(defun %avv-choices (type)
  (case (%ptype-name type) (member (cdr type)) (boolean '(t nil)) (t nil)))

(defun %avv-value-text (type value)
  (cond ((eq (%ptype-name type) 'boolean) (if value "Yes" "No"))
        ((and (null value) (eq (%ptype-name type) 'null-or-type)) "None")
        ((stringp value) value)
        (t (prin1-to-string value))))

(defun %avv-line (field)
  (destructuring-bind (key type label value) field
    (declare (ignore key))
    (let ((choices (%avv-choices type)))
      (if (and choices (not (eq (%ptype-name type) 'boolean)))
          (format nil "~A: ~{~A~^  ~}" label (mapcar (lambda (c) (let ((s (string-downcase (princ-to-string c)))) (if (equal c value) (format nil "[~A]" s) s))) choices))
          (format nil "~A: ~A" label (%avv-value-text type value))))))

(defparameter *avv-help* "[Done]  [Abort]   Up/Down: field  Return: change  End: done  Esc: abort")

(defun %avv-fit (st wm)
  (let* ((win (avv-window st))
         (width (min (- (wm-cols wm) 2) (max 50 (+ 6 (reduce #'max (mapcar (lambda (f) (utf8-length (%avv-line f))) (avv-fields st)) :initial-value 0))
                                            (+ 4 (length *avv-help*)))))
         (height (min (wm-area-height wm) (+ 5 (length (avv-fields st))))))
    (setf (window-w win) width (window-h win) height
          (window-x win) (max 0 (floor (- (wm-cols wm) width) 2)) (window-y win) (max 0 (floor (- (wm-area-height wm) height) 3)))))

(defmethod window-draw-content ((win avv-window) screen ix iy iw ih focused)
  (let* ((st (avv-window-avv win)) (row 0))
    (dolist (f (avv-fields st))
      (when (< row (- ih 2))
        (let* ((sel (= row (avv-selected st))) (editing (and sel (avv-editing st)))
               (fg (if (and sel (not editing)) (tc :sel-fg) (tc :fg))) (bg (if (and sel (not editing)) (tc :sel-bg) (tc :bg)))
               (attr (if (and sel (not editing) (= -1 (tc :sel-bg))) +reverse+ 0)))
          (screen-fill screen (+ iy row) ix 1 iw 32 fg bg attr)
          (if editing
              (let* ((head (format nil "~A: " (caddr f))) (text (%string-concat head (avv-input st))))
                (screen-draw-string screen (+ iy row) (+ ix 1) text (tc :fg) (tc :bg) +underline+ 0 (utf8-prefix-end text 0 (max 1 (- iw 2))))
                (when focused
                  (setf (window-cursor win) (cons (+ iy row) (+ ix 1 (min (- iw 2) (utf8-length (%string-concat head (subseq (avv-input st) 0 (avv-pos st))))))))))
              (let ((text (%avv-line f))) (screen-draw-string screen (+ iy row) (+ ix 1) text fg bg attr 0 (utf8-prefix-end text 0 (max 1 (- iw 2))))))))
      (setq row (+ row 1)))
    (when (avv-error st)
      (let ((e (avv-error st))) (screen-draw-string screen (+ iy ih -2) (+ ix 1) e (tc :error) (tc :bg) +bold+ 0 (utf8-prefix-end e 0 (max 1 (- iw 2))))))
    (screen-draw-string screen (+ iy ih -1) (+ ix 1) *avv-help* (tc :dim) (tc :bg) 0 0 (min (length *avv-help*) (max 0 (- iw 2))))
    (screen-draw-string screen (+ iy ih -1) (+ ix 1) "[Done]" (tc :link) (tc :bg) +bold+)
    (screen-draw-string screen (+ iy ih -1) (+ ix 9) "[Abort]" (tc :link) (tc :bg) +bold+)))

(defun %avv-start-edit (st)
  (let ((f (nth (avv-selected st) (avv-fields st))))
    (when f
      (let ((choices (%avv-choices (cadr f))))
        (if choices                                           ; Return steps through the choices
            (let* ((i (position (cadddr f) choices :test #'equal)) (next (nth (mod (+ 1 (or i -1)) (length choices)) choices)))
              (%avv-set st f next))
            (let ((text (if (null (cadddr f)) "" (%avv-value-text (cadr f) (cadddr f)))))
              (setf (avv-editing st) t (avv-input st) text (avv-pos st) (length text) (avv-error st) nil)))))))

(defun %avv-commit (st)
  (let ((f (nth (avv-selected st) (avv-fields st))))
    (handler-case (%avv-set st f (accept-from-string (cadr f) (avv-input st)))
      (error (e) (setf (avv-error st) (princ-to-string e))))))

(defmethod window-key ((win avv-window) key)
  (let* ((st (avv-window-avv win)) (c (key-code key)) (m (key-mods key)) (ctrl (/= 0 (logand m +ctrl+)))
         (n (length (avv-fields st))) (in (avv-input st)) (pos (avv-pos st)))
    (if (avv-editing st)
        (cond ((eq c :return) (%avv-commit st))
              ((or (eq c :escape) (and ctrl (eql c 103))) (setf (avv-editing st) nil (avv-error st) nil))
              ((eq c :backspace) (when (> pos 0) (let ((p (char-start in pos))) (setf (avv-input st) (%string-concat (subseq in 0 p) (subseq in pos)) (avv-pos st) p))))
              ((or (eq c :left) (and ctrl (eql c 98))) (setf (avv-pos st) (char-start in pos)))
              ((or (eq c :right) (and ctrl (eql c 102))) (setf (avv-pos st) (min (length in) (char-end in pos))))
              ((or (eq c :home) (and ctrl (eql c 97))) (setf (avv-pos st) 0))
              ((or (eq c :end) (and ctrl (eql c 101))) (setf (avv-pos st) (length in)))
              ((and ctrl (eql c 117)) (setf (avv-input st) "" (avv-pos st) 0))
              ((eq c :tab)
               (let* ((f (nth (avv-selected st) (avv-fields st))) (cands (accept-completions (cadr f) in)))
                 (cond ((null cands) (setf (avv-error st) "No completions"))
                       (t (let ((cp (if (cdr cands) (common-prefix cands) (car cands))))
                            (when (> (length cp) (length in)) (setf (avv-input st) cp (avv-pos st) (length cp)))
                            (setf (avv-error st) (and (cdr cands) (format nil "~{~A~^  ~}" (subseq cands 0 (min 8 (length cands)))))))))))
              ((and (integerp c) (not ctrl) (zerop (logand m +meta+)) (>= c 32))
               (let ((s (if (< c 128) (string (code-char c)) (%utf8-encode c))))
                 (setf (avv-input st) (%string-concat (subseq in 0 pos) s (subseq in pos)) (avv-pos st) (+ pos (length s))))))
        (cond ((or (eq c :up) (and ctrl (eql c 112)) (and (eq c :tab) (/= 0 (logand m +shift+)))) (setf (avv-selected st) (mod (- (avv-selected st) 1) (max 1 n))))
              ((or (eq c :down) (eq c :tab) (and ctrl (eql c 110))) (setf (avv-selected st) (mod (+ (avv-selected st) 1) (max 1 n))))
              ((eq c :return) (%avv-start-edit st))
              ((eq c :end) (setf (avv-done st) t))
              ((or (eq c :escape) (and ctrl (eql c 103))) (setf (avv-aborted st) t))
              ((and (integerp c) (not ctrl) (zerop (logand m +meta+)) (>= c 32))   ; typing starts a new value
               (let ((f (nth (avv-selected st) (avv-fields st))))
                 (when (and f (null (%avv-choices (cadr f))))
                   (let ((s (if (< c 128) (string (code-char c)) (%utf8-encode c))))
                     (setf (avv-editing st) t (avv-input st) s (avv-pos st) (length s) (avv-error st) nil)))))))
    (setf (wm-dirty (window-wm win)) t)
    t))

(defmethod window-mouse ((win avv-window) kind lx ly button mods)
  (declare (ignore button mods))
  (when (eq kind :press)
    (let* ((st (avv-window-avv win)) (ih (- (window-h win) 2)))
      (cond ((= ly (- ih 1))
             (cond ((<= 1 lx 6) (setf (avv-done st) t))
                   ((<= 9 lx 15) (setf (avv-aborted st) t))))
            ((and (>= ly 0) (< ly (length (avv-fields st))))
             (let* ((f (nth ly (avv-fields st))) (choices (%avv-choices (cadr f))))
               (setf (avv-selected st) ly)
               (if (and choices (not (eq (%ptype-name (cadr f)) 'boolean)))
                   ;; a click on one of the choices picks it
                   (let ((col (+ 3 (utf8-length (caddr f)))) (picked nil))
                     (dolist (ch choices)                     ; laid out as in %AVV-LINE: the current one in brackets
                       (let ((w (+ (utf8-length (string-downcase (princ-to-string ch))) (if (equal ch (cadddr f)) 2 0))))
                         (when (and (>= lx col) (< lx (+ col w))) (setq picked ch))
                         (setq col (+ col w 2))))
                     (if picked (%avv-set st f picked) (%avv-start-edit st)))
                   (%avv-start-edit st)))))
      (setf (wm-dirty (window-wm win)) t)))
  t)

(defmethod window-close ((win avv-window))
  (let ((st (avv-window-avv win))) (unless (avv-done st) (setf (avv-aborted st) t)))
  t)

;;; ---- the Listener's side --------------------------------------------------------------------------------------

(defun %accept-hover-doc (p)
  "Mouse documentation for presentation P while input is wanted, or NIL if P does not answer it."
  (multiple-value-bind (ctx obj how) (%context-match p)
    (when ctx
      (let ((s (%abbrev obj 50)))
        (format nil "L: use ~A for ~A   M: Copy to input   R: menu   ~A" (or how "this") (ic-label ctx) s)))))

(defun %accept-click (p)
  "If presentation P answers the input wanted, supply it and return T."
  (multiple-value-bind (ctx obj) (%context-match p)
    (when ctx (funcall (ic-supply ctx) obj) t)))
