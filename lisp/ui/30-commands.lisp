;;;; 30-commands.lisp -- the command processor.
;;;;
;;;; Commands are typed as ":Show Packages" or abbreviated (any unique prefix of
;;;; each word).  DEFINE-COMMAND declares typed arguments; missing arguments are
;;;; prompted for by the Listener.  Argument types: SYMBOL, STRING, INTEGER,
;;;; PACKAGE, EXPRESSION, WORD.  Lines that do not start with ":" are Lisp forms.

(in-package :cl)

(defstruct (command (:conc-name cmd-) (:constructor %make-command) (:copier nil)) name words fn args doc)
(defvar *commands* nil)
(defvar *current-listener* nil)

(defun %name-words (name) (%split-string (substitute #\Space #\- (string-upcase (symbol-name name))) #\Space))

(defun %register-command (name args doc fn)
  (let ((c (%make-command :name name :words (%name-words name) :fn fn :args args :doc doc)))
    (setq *commands* (cons c (remove name *commands* :key #'cmd-name)))
    name))

(defmacro define-command ((name &key (documentation "")) args &body body)
  "ARGS: list of (var type &key default prompt).  The body runs with output going to the Listener."
  `(%register-command ',name
                      (list ,@(mapcar (lambda (a) `(list ',(car a) ',(cadr a) ,(if (getf (cddr a) :default) `(lambda () ,(getf (cddr a) :default)) nil)
                                                         ,(or (getf (cddr a) :prompt) (string-capitalize (symbol-name (car a))))))
                                      args))
                      ,documentation
                      (lambda ,(mapcar #'car args) ,@body)))

(defun command-full-name (c) (%join-strings (cmd-words c) " "))

(defun %word-prefix-p (token word) (and (<= (length token) (length word)) (string= token (subseq word 0 (length token)))))

(defun find-commands (tokens)
  "Commands whose leading words each start with the corresponding token, with the number of tokens used."
  (let ((exact nil) (matches nil))
    (dolist (c *commands*)
      (let ((words (cmd-words c)) (n 0) (ok t))
        (dolist (tk tokens)
          (if (and ok (< n (length words)) (%word-prefix-p tk (nth n words))) (setq n (+ n 1)) (return)))
        (when (and (> n 0) (= n (length words)))
          (setq matches (cons (cons c n) matches))
          (when (every #'string= (subseq tokens 0 n) words) (setq exact (cons (cons c n) exact))))))
    (or exact matches)))

(defun %split-tokens (text)
  (remove "" (%split-string (substitute #\Space #\Tab text) #\Space) :test #'string=))

(defun %rest-after-tokens (text n)
  "TEXT with its first N whitespace-separated tokens removed."
  (let ((i 0) (len (length text)) (k 0))
    (loop
      (loop (when (or (>= i len) (not (member (char text i) '(#\Space #\Tab)))) (return)) (setq i (+ i 1)))
      (when (or (>= k n) (>= i len)) (return))
      (loop (when (or (>= i len) (member (char text i) '(#\Space #\Tab))) (return)) (setq i (+ i 1)))
      (setq k (+ k 1)))
    (subseq text i)))

(defun %parse-arg (type text)
  "Parse one argument of TYPE from TEXT.  Returns (values value rest) or :missing."
  (let ((text (string-left-trim " " text)))
    (if (= (length text) 0)
        :missing
        (case type
          ((expression symbol)
           (multiple-value-bind (obj pos) (%read-from-string text 0)
             (values obj (subseq text pos))))
          (integer (let ((toks (%split-tokens text)))
                     (values (parse-integer (car toks)) (%rest-after-tokens text 1))))
          (package (let* ((toks (%split-tokens text)) (p (find-package (string-upcase (car toks)))))
                     (unless p (error "No package named ~A" (car toks)))
                     (values p (%rest-after-tokens text 1))))
          (string (if (char= (char text 0) #\")
                      (multiple-value-bind (obj pos) (%read-from-string text 0) (values obj (subseq text pos)))
                      (values (string-trim " " text) "")))
          (word (let ((toks (%split-tokens text))) (values (car toks) (%rest-after-tokens text 1))))
          ;; any other presentation type: one word, parsed as ACCEPT would (a sequence takes the rest of the line)
          (t (if (and (consp type) (eq (car type) 'sequence))
                 (values (accept-from-string type text) "")
                 (let ((toks (%split-tokens text))) (values (accept-from-string type (car toks)) (%rest-after-tokens text 1)))))))))

(defun execute-command-line (line)
  "LINE is the text after the colon.  Returns (:done) or (:prompt command args spec rest) when an
argument is missing, or signals an error for unknown/ambiguous commands."
  (let* ((tokens (mapcar #'string-upcase (%split-tokens line))) (hits (find-commands tokens)))
    ;; the hyphenated spelling :who-calls means the two words WHO CALLS
    (when (and (null hits) tokens (%string-position-char (car tokens) #\- 0))
      (let* ((words (remove "" (%split-string (car tokens) #\-) :test #'string=)) (h2 (find-commands words)))
        (when h2
          (let ((best (remove-if-not (lambda (h) (= (cdr h) (length words))) h2)))
            (when best (setq hits (mapcar (lambda (h) (cons (car h) 1)) best)))))))
    ;; prefer the commands that matched the most words (:apropos-window over :apropos)
    (when (and hits (cdr hits))
      (let ((maxn (reduce #'max (mapcar #'cdr hits))))
        (setq hits (remove-if-not (lambda (h) (= (cdr h) maxn)) hits))))
    (cond ((null tokens) (error "Empty command"))
          ((null hits) (error "Unknown command: ~A  (try :HELP)" (car tokens)))
          ((and (cdr hits) (> (length hits) 1)
                (not (every (lambda (h) (eq (car h) (car (car hits)))) hits)))
           (error "Ambiguous command ~A: ~{~A~^, ~}" (car tokens) (mapcar (lambda (h) (command-full-name (car h))) hits)))
          (t (let* ((hit (car hits)) (c (car hit)) (rest (%rest-after-tokens line (cdr hit))))
               (%collect-args c nil (cmd-args c) rest))))))

(defun %collect-args (c done specs rest)
  (if (null specs)
      (progn (apply (cmd-fn c) (reverse done)) (list :done))
      (let* ((spec (car specs)) (type (cadr spec)))
        (multiple-value-bind (val more) (%parse-arg type rest)
          (cond ((eq val :missing)
                 (if (caddr spec)
                     (%collect-args c (cons (funcall (caddr spec)) done) (cdr specs) rest)
                     (list :prompt c done specs rest)))
                (t (%collect-args c (cons val done) (cdr specs) more)))))))

(defun continue-command-with-value (pending value)
  "Supply VALUE (an object, from a click) for the first missing argument of the PENDING command."
  (destructuring-bind (c done specs rest) (cdr pending)
    (declare (ignore rest))
    (%collect-args c (cons value done) (cdr specs) "")))

(defun continue-command (pending text)
  "PENDING is the (:prompt ...) list; TEXT is the user's answer for the first missing argument."
  (destructuring-bind (c done specs rest) (cdr pending)
    (declare (ignore rest))
    (multiple-value-bind (val more) (%parse-arg (cadr (car specs)) text)
      (if (eq val :missing)
          pending
          (%collect-args c (cons val done) (cdr specs) more)))))

;;; ---- completion --------------------------------------------------------------------------------------------

(defun common-prefix (strings)
  (if (null strings) ""
      (let ((p (car strings)))
        (dolist (s (cdr strings))
          (let ((n 0) (m (min (length p) (length s))))
            (loop (when (or (>= n m) (char/= (char p n) (char s n))) (return)) (setq n (+ n 1)))
            (setq p (subseq p 0 n))))
        p)))

(defun complete-command-words (tokens)
  "Completions for the last token of a partially typed command."
  (let* ((n (length tokens)) (last (nth (- n 1) tokens)) (acc nil))
    (dolist (c *commands*)
      (let ((words (cmd-words c)))
        (when (and (<= n (length words))
                   (every #'%word-prefix-p (subseq tokens 0 (- n 1)) words)
                   (%word-prefix-p last (nth (- n 1) words)))
          (setq acc (adjoin (nth (- n 1) words) acc :test #'string=)))))
    (sort acc #'string<)))

(defun complete-symbol-names (prefix)
  "Lower-case names of symbols accessible in *PACKAGE* that start with PREFIX."
  (let ((up (string-upcase prefix)) (acc nil))
    (flet ((scan (pkg external-only)
             (dolist (s (%package-symbols pkg external-only))
               (let ((nm (symbol-name s)))
                 (when (and (>= (length nm) (length up)) (string= (subseq nm 0 (length up)) up) (char/= (char nm 0) #\%))
                   (setq acc (adjoin (string-downcase nm) acc :test #'string=)))))))
      (scan *package* nil)
      (dolist (u (package-use-list *package*)) (scan u t)))
    (sort acc #'string<)))

;;; ---- built-in commands -------------------------------------------------------------------------------------

(define-command (help :documentation "Show help about the Listener and its commands") ()
  (format t "~&NeoGenera Listener~%")
  (format t "  Type Lisp forms, or commands starting with a colon (abbreviations work: :sh pack).~%")
  (format t "  Tab completes commands and symbols.  Up/Down browse history.  C-l clears the screen.~%")
  (format t "  Results are mouse-sensitive: left click = default action, middle = copy to input, right = menu.~%")
  (format t "  Window keys: F6 next window, F7 re-tile, F8 theme, F9 select (then L E P D I S), F10 quit (asks first).  Drag title bars to move, the corner to resize.~%")
  (format t "~&Documentation:~%")
  (format t "  :tutorial        learn NeoGenera step by step (:lessons lists the lessons)~%")
  (format t "  :doc [topic]     open the manual, or the documentation of a topic or symbol~%")
  (format t "  :search-doc WORDS  full-text search of the documentation~%")
  (format t "~&Commands:~%")
  (dolist (c (sort (copy-list *commands*) (lambda (a b) (string< (command-full-name a) (command-full-name b)))))
    (format t "  :~A~A~%" (string-downcase (command-full-name c)) (if (> (length (cmd-doc c)) 0) (%string-concat " -- " (cmd-doc c)) ""))))

(define-command (show-commands :documentation "List all commands") ()
  (dolist (c (sort (copy-list *commands*) (lambda (a b) (string< (command-full-name a) (command-full-name b)))))
    (format t "  :~A~%" (string-downcase (command-full-name c)))))

(define-command (show-packages :documentation "List all packages") ()
  (dolist (p (sort (copy-list (list-all-packages)) (lambda (a b) (string< (package-name a) (package-name b)))))
    (format t "  ~A~%" (package-name p))))

(define-command (package :documentation "Select the current package") ((package package :prompt "Package"))
  (setq *package* package)
  (format t "~&Package is now ~A~%" (package-name package)))

(define-command (describe :documentation "Describe an object") ((object expression :prompt "Expression"))
  (describe (eval object)))

(define-command (apropos :documentation "List symbols containing a string") ((string string :prompt "Substring"))
  (apropos string))

(define-command (clear-screen :documentation "Clear the Listener") ()
  (when *current-listener* (ob-clear (lw-obuf *current-listener*))))

(define-command (clear-history :documentation "Forget command history") ()
  (when *current-listener* (setf (lw-history *current-listener*) nil)))

(define-command (history :documentation "Show recent input") ()
  (when *current-listener*
    (let ((i 0)) (dolist (h (reverse (lw-history *current-listener*))) (setq i (+ i 1)) (format t "~4D  ~A~%" i h)))))

(define-command (theme :documentation "Switch colour theme (classic green amber mono)") ((name word :default "classic"))
  (set-theme (intern (string-upcase name) "KEYWORD"))
  (screen-invalidate (wm-screen *wm*))
  (format t "~&Theme: ~A~%" (symbol-name (theme-name *theme*))))

(define-command (tile :documentation "Re-tile windows (horizontal vertical main grid)") ((mode word :default "horizontal"))
  (wm-tile *wm* (intern (string-upcase mode) "KEYWORD")))

(define-command (quit :documentation "Leave the window system") ()
  (setf (wm-quit *wm*) t))
