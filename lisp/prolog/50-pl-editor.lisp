;;;; 50-pl-editor.lisp -- the Prolog Tool Kit: PROLOG mode in the editor.
;;;;
;;;; PROLOG mode knows clauses (up to the full stop that ends them), predicates (the consecutive clauses of one name and
;;;; arity), directives and the layout conventions of Prolog source: heads at the left margin, body goals indented four
;;;; columns, and the `(   If  ->  Then  ;   Else  )' block layout.  Compiling is incremental: the clauses of a region
;;;; replace the earlier definitions of those predicates from the same buffer.
;;;;
;;;; Keys:
;;;;   TAB     indent the line                          C-j     end the line and indent the next
;;;;   C-M-a C-M-e  clause start / end                  C-M-f C-M-b  next / previous clause     C-M-h  mark clause
;;;;   C-c C-c compile the region or clause at point    C-c C-b  compile the buffer
;;;;   C-c C-q run a query                              C-c C-e  run the query on the current line (a ?- goal)
;;;;   C-c C-l list the predicate at point              C-c C-t  toggle tracing     C-c C-w  edit compiler warnings
;;;;   M-.     edit the definition of the predicate named at point

(in-package :cl)

(defvar *pl-mode-map* (make-keymap "prolog"))
(defvar *pl-indent* 4 "Columns per level of body indentation in PROLOG mode.")

(setf (get 'prolog-mode 'mode-keymap) *pl-mode-map*
      (get 'prolog-mode 'mode-name) "PROLOG")

(dolist (suffix '(".pl" ".pro" ".prolog"))
  (unless (assoc suffix *auto-mode-alist* :test #'string=) (push (cons suffix 'prolog-mode) *auto-mode-alist*)))

(defun %pl-buffer-p (b) (eq (buffer-mode b) 'prolog-mode))

(defun %pl-buffer-source (b) (or (buffer-file b) (buffer-name b)))

;;; ---- colouring ------------------------------------------------------------------------------------------------------------

(defun %pl-skip-quoted-in-line (line i q)
  "The index after the quoted item that continues at I in LINE (quote char Q), and whether it is still open."
  (let ((n (length line)))
    (loop
      (cond ((>= i n) (return (values n t)))
            ((char= (char line i) #\\) (setq i (+ i 2)))
            ((char= (char line i) q)
             (if (and (< (+ i 1) n) (char= (char line (+ i 1)) q)) (setq i (+ i 2)) (return (values (+ i 1) nil))))
            (t (setq i (+ i 1)))))))

(defun pl-line-syntax (line state)
  "Syntax spans of one line of Prolog: (values spans end-state).  A span is (start end class), CLASS :COMMENT :STRING
:BUILTIN (:- --> -> ; ! and the control constructs) :KEYWORD (built-in predicates) :DEFNAME (the head of a clause)
:CONSTANT (variables).  STATE is NIL, :BLOCK (inside /* */) or a quote character (inside a quoted item)."
  (let ((i 0) (n (length line)) (spans nil) (start 0) (head-pending (and (null state) (> (length line) 0) (not (%pl-layout-p (char line 0))))))
    (loop
      (when (>= i n) (return))
      (cond
        ((eq state :block)
         (let ((e (search "*/" line :start2 i)))
           (push (list start (if e (+ e 2) n) :comment) spans)
           (setq i (if e (+ e 2) n) state (if e nil :block))))
        ((characterp state)
         (multiple-value-bind (j open) (%pl-skip-quoted-in-line line i state)
           (push (list start j :string) spans)
           (setq i j state (if open state nil))))
        (t
         (let ((c (char line i)))
           (cond
             ((char= c #\%) (push (list i n :comment) spans) (setq i n))
             ((and (char= c #\/) (< (+ i 1) n) (char= (char line (+ i 1)) #\*)) (setq state :block start i i (+ i 2)))
             ((or (char= c #\") (char= c #\`)) (setq state c start i i (+ i 1)))
             ((char= c #\')
              (multiple-value-bind (j open) (%pl-skip-quoted-in-line line (+ i 1) #\')
                (if open (setq state #\' start i i n)
                    (progn (if head-pending (push (list i j :defname) spans) (push (list i j :string) spans)) (setq i j)))
                (setq head-pending nil)))
             ((and (char= c #\0) (< (+ i 2) n) (char= (char line (+ i 1)) #\'))
              (push (list i (+ i 3) :string) spans) (setq i (+ i 3) head-pending nil))
             ((digit-char-p c)
              (let ((j i)) (loop (when (or (>= j n) (not (or (alphanumericp (char line j)) (char= (char line j) #\.) (char= (char line j) #\_)))) (return))
                                 (when (and (char= (char line j) #\.) (not (and (< (+ j 1) n) (digit-char-p (char line (+ j 1)))))) (return))
                                 (setq j (+ j 1)))
                (setq i j head-pending nil)))
             ((%pl-upper-start-p c)
              (let ((j i)) (loop (when (or (>= j n) (not (%pl-alnum-p (char line j)))) (return)) (setq j (+ j 1)))
                (push (list i j :constant) spans)
                (setq i j head-pending nil)))
             ((%pl-lower-start-p c)
              (let ((j i)) (loop (when (or (>= j n) (not (%pl-alnum-p (char line j)))) (return)) (setq j (+ j 1)))
                (let* ((name (subseq line i j)) (atom (pl-atom name)) (call (and (< j n) (char= (char line j) #\()))
                       (pred (and call (pl-find-pred atom 1))))
                  (declare (ignorable pred))
                  (cond (head-pending (push (list i j :defname) spans))
                        ((and call (let ((found nil)) (dotimes (k 8) (let ((p (pl-find-pred atom k))) (when (and p (or (plpred-fn p) (plpred-ctl p))) (setq found t)))) found))
                         (push (list i j :keyword) spans))
                        ((and (not call) (member name '("is" "mod" "rem" "div" "xor" "rdiv") :test #'string=)) (push (list i j :builtin) spans))))
                (setq i j head-pending nil)))
             ((%pl-symch-p c)
              (let ((j i)) (loop (when (or (>= j n) (not (%pl-symch-p (char line j)))) (return)) (setq j (+ j 1)))
                (let ((sym (subseq line i j)))
                  (when (member sym '(":-" "-->" "?-" "->" "*->" "\\+" "=" "\\=" "==" "\\==" "=.." "=:=" "=\\=") :test #'string=)
                    (push (list i j :builtin) spans)))
                (setq i j head-pending nil)))
             ((or (char= c #\!) (char= c #\;)) (push (list i (+ i 1) :builtin) spans) (setq i (+ i 1) head-pending nil))
             (t (setq i (+ i 1)) (unless (%pl-layout-p c) (setq head-pending nil))))))))
    (when (and (eq state :block) (null spans)) (push (list 0 n :comment) spans))
    (values (nreverse spans) state)))

(setf (get 'prolog-mode 'line-syntax) #'pl-line-syntax)

;;; ---- clauses --------------------------------------------------------------------------------------------------------------

(defstruct (plunit (:copier nil)) start end line kind name arity text)

(defun %pl-head-key (term)
  "(values name arity) of the predicate TERM defines: a clause, a fact or a DCG rule."
  (let ((h (%deref term)))
    (cond ((or (pl-functor-p h *pl-neck* 2)) (setq h (%deref (pl-arg h 0))))
          ((pl-functor-p h *pl-dcg-arrow* 2) (setq h (%deref (pl-arg h 0)))
           (when (pl-functor-p h *pl-comma* 2) (setq h (%deref (pl-arg h 0))))
           (return-from %pl-head-key (if (pl-callable-p h) (values (pl-name h) (+ 2 (pl-arity h))) (values nil nil)))))
    (if (pl-callable-p h) (values (pl-name h) (pl-arity h)) (values nil nil))))

(defun %pl-scan-clauses (text)
  "The clauses of TEXT as a list of PLUNITs (including directives and unreadable clauses)."
  (let ((pos 0) (acc nil) (n (length text)))
    (loop
      (when (>= pos n) (return))
      (let ((r (handler-case (multiple-value-list (pl-read-term-from-string text pos))
                 (pl-error () (let* ((next (pl-read-recover-position text pos)) (s (nth-value 0 (%pl-skip-layout text pos))))
                                (list :error nil s next next))))))
        (cond ((eq (car r) :eof) (return))
              ((eq (car r) :error)
               (let ((start (third r)) (end (fourth r)))
                 (push (make-plunit :start start :end (max end (+ start 1)) :line (%pl-line-col text start) :kind :error :text (subseq text start (min n end))) acc)
                 (setq pos (max (fifth r) (+ pos 1)))))
              (t
               (let* ((term (%deref (first r))) (start (third r)) (end (fourth r)) (kind :clause) (name nil) (arity nil))
                 (cond ((or (pl-functor-p term *pl-neck* 1) (pl-functor-p term (pl-atom "?-") 1)) (setq kind :directive))
                       (t (multiple-value-setq (name arity) (%pl-head-key term))))
                 (push (make-plunit :start start :end end :line (%pl-line-col text start) :kind kind :name name :arity arity) acc)
                 (setq pos (max (fifth r) (+ pos 1))))))))
    (nreverse acc)))

(defun pl-buffer-clauses (b)
  "The clauses of buffer B (cached until it changes)."
  (let ((text (buffer-string b)) (cache (buf-local b 'pl-clauses)))
    (if (and cache (string= (car cache) text))
        (cdr cache)
        (let ((units (%pl-scan-clauses text)))
          (setf (buf-local b 'pl-clauses) (cons text units))
          units))))

(defun %pl-unit-label (u)
  (case (plunit-kind u)
    (:directive "directive")
    (:error "unreadable clause")
    (t (if (plunit-name u) (format nil "~A/~D" (pl-atom-text (plunit-name u) t) (plunit-arity u)) "clause"))))

(defstruct (plgroup (:copier nil)) key start end line units)

(defun %pl-predicates-of-buffer (b)
  "The predicates of B in order, as PLGROUPs: the consecutive clauses of one name and arity grouped."
  (let ((groups nil))
    (dolist (u (pl-buffer-clauses b))
      (when (eq (plunit-kind u) :clause)
        (let* ((key (list (plunit-name u) (plunit-arity u))) (g (car groups)))
          (if (and g (equal (plgroup-key g) key))
              (setf (plgroup-units g) (append (plgroup-units g) (list u)) (plgroup-end g) (plunit-end u))
              (push (make-plgroup :key key :start (plunit-start u) :end (plunit-end u) :line (plunit-line u) :units (list u)) groups)))))
    (nreverse groups)))

(defun %pl-unit-at (b off)
  "The clause around OFFSET, else the nearest one before it, else the first."
  (let ((units (pl-buffer-clauses b)))
    (or (find-if (lambda (u) (and (<= (plunit-start u) off) (<= off (plunit-end u)))) units)
        (let ((best nil)) (dolist (u units best) (when (and (<= (plunit-end u) off) (or (null best) (> (plunit-end u) (plunit-end best)))) (setq best u))))
        (car units))))

(defun %pl-goto (b off) (goto-offset b (max 0 (min off (length (buffer-string b))))))

(defcmd prolog-mode (ed arg)
  (with-b (b ed)
    (set-buffer-mode b 'prolog-mode)
    (ed-message* ed "PROLOG mode")))

;;; ---- indentation -------------------------------------------------------------------------------------------------------------

(defun %pl-bracket-stack (text from to)
  "The brackets open at offset TO of TEXT scanning from offset FROM: a list (innermost first) of (char . offset)."
  (let ((stack nil) (i from) (n (min to (length text))))
    (loop
      (when (>= i n) (return))
      (let ((c (char text i)))
        (cond ((char= c #\%) (loop (when (or (>= i n) (char= (char text i) #\Newline)) (return)) (setq i (+ i 1))))
              ((and (char= c #\/) (< (+ i 1) n) (char= (char text (+ i 1)) #\*))
               (let ((e (search "*/" text :start2 (+ i 2)))) (setq i (if e (+ e 2) n))))
              ((or (char= c #\') (char= c #\") (char= c #\`))
               (if (and (char= c #\') (> i 0) (char= (char text (- i 1)) #\0) (or (= i 1) (not (alphanumericp (char text (- i 2))))))
                   (setq i (+ i 2))
                   (let ((j (+ i 1)))
                     (loop (cond ((>= j n) (return)) ((char= (char text j) #\\) (setq j (+ j 2)))
                                 ((char= (char text j) c) (if (and (< (+ j 1) n) (char= (char text (+ j 1)) c)) (setq j (+ j 2)) (progn (setq j (+ j 1)) (return))))
                                 (t (setq j (+ j 1)))))
                     (setq i j))))
              ((find c "([{") (push (cons c i) stack) (setq i (+ i 1)))
              ((find c ")]}") (pop stack) (setq i (+ i 1)))
              (t (setq i (+ i 1))))))
    stack))

(defun %pl-offset-column (text off)
  (let ((nl (position #\Newline text :end off :from-end t))) (- off (if nl (+ nl 1) 0))))

(defun %pl-line-indent-column (text off)
  "The indentation of the line containing offset OFF of TEXT."
  (let* ((nl (position #\Newline text :end off :from-end t)) (s (if nl (+ nl 1) 0)) (i s))
    (loop (when (or (>= i (length text)) (not (member (char text i) '(#\Space #\Tab)))) (return)) (setq i (+ i 1)))
    (- i s)))

(defun pl-indent-column (b line)
  "The column the Prolog line LINE of B should start in."
  (let* ((text (buffer-string b)) (off (buf-offset b line 0)) (units (pl-buffer-clauses b))
         (unit (find-if (lambda (u) (and (< (plunit-start u) off) (<= off (plunit-end u)))) units))
         (ltext (string-left-trim '(#\Space #\Tab) (buf-line b line))))
    (cond
      ((null unit) 0)
      (t
       (let* ((stack (%pl-bracket-stack text (plunit-start unit) off))
              (top (car stack)) (first-char (if (> (length ltext) 0) (char ltext 0) #\Space)))
         (cond
           ((null top)
            (if (> (length ltext) 0) *pl-indent* *pl-indent*))
           (t
            (let* ((col (%pl-offset-column text (cdr top))) (open (car top))
                   (functor-call (and (char= open #\() (> (cdr top) 0) (%pl-alnum-p (char text (- (cdr top) 1)))))
                   (last-on-line (let ((e (position #\Newline text :start (cdr top)))) (or (null e) (every #'%pl-layout-p (subseq text (+ (cdr top) 1) e))))))
              (cond
                ((find first-char ")]}") col)
                ((and (char= open #\() (not functor-call) (or (find first-char ";|") (and (> (length ltext) 1) (string= (subseq ltext 0 2) "->"))
                                                          (and (> (length ltext) 2) (string= (subseq ltext 0 3) "*->"))))
                 col)
                ((and (char= open #\() (not functor-call)) (+ col *pl-indent*))
                (last-on-line (+ (%pl-line-indent-column text (cdr top)) *pl-indent*))
                (t (+ col 1)))))))))))

(defun pl-indent-line (b line)
  (let* ((text (buf-line b line)) (col (pl-indent-column b line)) (body (string-left-trim '(#\Space #\Tab) text)))
    (when (> (length body) 0)
      (let ((new (concatenate 'string (make-string col :initial-element #\Space) body)))
        (unless (string= new text)
          (buf-delete b line 0 line (length text))
          (buf-goto b line 0)
          (buf-insert b new))))))

(defcmd prolog-tab (ed arg)
  (with-b (b ed)
    (let* ((line (point-line b)) (col (point-col b)) (text (buf-current-line b)) (lead (or (position-if-not (lambda (c) (member c '(#\Space #\Tab))) text) (length text))))
      (cond ((= (length (string-trim '(#\Space #\Tab) text)) 0)
             (let ((c (pl-indent-column b line)))
               (buf-delete b line 0 line (length text)) (buf-goto b line 0) (buf-insert b (make-string c :initial-element #\Space))))
            (t (pl-indent-line b line)
               (let ((nlead (or (position-if-not (lambda (c) (member c '(#\Space #\Tab))) (buf-current-line b)) 0)))
                 (when (<= col lead) (buf-goto b line nlead))))))))

(defcmd prolog-newline-and-indent (ed arg)
  (with-b (b ed)
    (let ((ended (point-line b)))
      (pl-indent-line b ended)
      (buf-goto b (point-line b) (length (buf-current-line b)))
      (buf-insert b (string #\Newline))
      (let ((c (pl-indent-column b (point-line b))))
        (buf-insert b (make-string c :initial-element #\Space))))))

;;; ---- clause motion ------------------------------------------------------------------------------------------------------------

(defcmd prolog-beginning-of-clause (ed arg)
  (with-b (b ed)
    (let ((off (point-offset b)) (best nil))
      (dolist (u (pl-buffer-clauses b)) (when (and (< (plunit-start u) off) (or (null best) (> (plunit-start u) (plunit-start best)))) (setq best u)))
      (if best (progn (push-mark ed) (%pl-goto b (plunit-start best))) (ed-message* ed "No clause before point")))))

(defcmd prolog-end-of-clause (ed arg)
  (with-b (b ed)
    (let ((off (point-offset b)) (best nil))
      (dolist (u (pl-buffer-clauses b)) (when (and (> (plunit-end u) off) (or (null best) (< (plunit-end u) (plunit-end best)))) (setq best u)))
      (if best (progn (push-mark ed) (%pl-goto b (plunit-end best))) (ed-message* ed "No clause after point")))))

(defcmd prolog-forward-clause (ed arg)
  (with-b (b ed)
    (dotimes (i (%n arg))
      (let ((off (point-offset b)) (best nil))
        (dolist (u (pl-buffer-clauses b)) (when (and (> (plunit-start u) off) (or (null best) (< (plunit-start u) (plunit-start best)))) (setq best u)))
        (if best (%pl-goto b (plunit-start best)) (error "No more clauses"))))))

(defcmd prolog-backward-clause (ed arg)
  (with-b (b ed)
    (dotimes (i (%n arg))
      (let ((off (point-offset b)) (best nil))
        (dolist (u (pl-buffer-clauses b)) (when (and (< (plunit-start u) off) (or (null best) (> (plunit-start u) (plunit-start best)))) (setq best u)))
        (if best (%pl-goto b (plunit-start best)) (error "No previous clause"))))))

(defcmd prolog-mark-clause (ed arg)
  (with-b (b ed)
    (let ((u (%pl-unit-at b (point-offset b))))
      (unless u (error "No clause here"))
      (let ((p (buf-pos-of-offset b (plunit-end u)))) (setf (buffer-mark b) (cons (car p) (cdr p))))
      (%pl-goto b (plunit-start u))
      (ed-message* ed "Marked ~A" (%pl-unit-label u)))))

;;; ---- compiling --------------------------------------------------------------------------------------------------------------

(defun %pl-set-warning-possibilities (ed)
  (let ((ws (pl-compiler-warnings)))
    (setf (ed-possibilities ed)
          (make-possibilities "compiler warnings"
                              (mapcar (lambda (w)
                                        (make-possibility (format nil "~A:~D: ~A" (path-filename (princ-to-string (plwarning-source w))) (plwarning-line w) (plwarning-message w))
                                                          (lambda (e) (%pl-visit-warning e w))))
                                      ws)))
    ws))

(defun %pl-visit-warning (ed w)
  (let* ((src (plwarning-source w))
         (b (or (find src (ed-buffers ed) :key #'buffer-file :test #'equal)
                (find src (ed-buffers ed) :key #'buffer-name :test #'equal)
                (and (stringp src) (probe-file src) (ed-visit-file ed src)))))
    (unless b (error "The source ~A of the warning is not available" src))
    (ed-switch-to-buffer ed b)
    (buf-goto b (max 0 (min (- (plwarning-line w) 1) (- (buf-nlines b) 1))) 0)))

(defun %pl-compile-in-editor (ed start end)
  "Consult the text of the current buffer between offsets START and END (the whole buffer if NIL)."
  (let* ((b (ed-buffer ed)) (text (buffer-string b)) (src (%pl-buffer-source b))
         (piece (if start (subseq text start end) text)))
    (let ((*pl-quiet* t) (before (length (pl-compiler-warnings src))))
      (declare (ignorable before))
      (multiple-value-bind (n warnings)
          (pl-consult-string (if start (concatenate 'string (make-string (max 0 (- (%pl-line-col text start) 1)) :initial-element #\Newline) piece) piece)
                             :file (buffer-file b) :source src)
        (let ((errors (count :error warnings :key #'plwarning-kind)))
          (pl-note-compiled b)
          (cond ((null warnings) (ed-message* ed "~D clause~:P compiled" n))
                (t (%pl-set-warning-possibilities ed)
                   (let ((w (car warnings)))
                     (ed-message* ed "~D clause~:P compiled, ~D problem~:P; line ~D: ~A  (C-. visits them)"
                                  n (length warnings) (plwarning-line w) (plwarning-message w)))))
          (values n errors))))))

(defmacro %pl-chain (name (ed arg) prolog-form)
  "Define the editor command NAME to run PROLOG-FORM in PROLOG buffers and whatever it did before elsewhere."
  `(let ((%previous (get ',name '%ecommand)))
     (defcmd ,name (,ed ,arg)
       (if (%pl-buffer-p (ed-buffer ,ed))
           ,prolog-form
           (if %previous (funcall %previous ,ed ,arg) (ed-message* ,ed "~A is not available in this buffer" ',name))))))

(%pl-chain compile-region (ed arg)
  (with-b (b ed)
    (multiple-value-bind (l1 c1 l2 c2) (region-bounds b)
      (cond (l1 (let ((s (buf-offset b l1 c1)) (e (buf-offset b l2 c2)))
                  (setf (buffer-mark b) nil)
                  (%pl-compile-in-editor ed s e)))
            (t (let ((u (%pl-unit-at b (point-offset b))))
                 (if u
                     ;; the whole predicate around point, so its clauses replace the old ones together
                     (let ((g (find-if (lambda (g) (member u (plgroup-units g))) (%pl-predicates-of-buffer b))))
                       (if g (%pl-compile-in-editor ed (plgroup-start g) (plgroup-end g)) (%pl-compile-in-editor ed (plunit-start u) (plunit-end u))))
                     (ed-message* ed "Nothing to compile"))))))))

(%pl-chain compile-buffer (ed arg) (%pl-compile-in-editor ed nil nil))

(%pl-chain compile-file (ed arg)
  (with-b (b ed)
    (unless (buffer-file b) (error "The buffer is not visiting a file"))
    (when (buffer-modified b) (ed-save-buffer ed b))
    (handler-case (ed-message* ed "Wrote ~A" (pl-compile-file (buffer-file b)))
      (error (e) (ed-message* ed "~A" (car (%split-string (princ-to-string e) #\Newline)))))))

(%pl-chain compiler-warnings (ed arg)
  (let ((ws (pl-compiler-warnings)))
    (%show-text-buffer ed "*Compiler-Warnings-1*"
                       (if ws (format nil "~{~A~%~}" (mapcar (lambda (w) (format nil "~A: line ~D: ~A" (plwarning-source w) (plwarning-line w) (plwarning-message w))) ws))
                           "No compiler warnings."))))

(%pl-chain edit-compiler-warnings (ed arg)
  (let ((ws (%pl-set-warning-possibilities ed)))
    (if ws (ed-goto-possibility ed 0) (ed-message* ed "No compiler warnings"))))

;;; ---- queries -----------------------------------------------------------------------------------------------------------------------

(defun pl-query-output (goal-text &optional (limit 50))
  "Run the query GOAL-TEXT and return the text of its answers, as :Prolog Query prints them."
  (with-output-to-string (out)
    (let ((*standard-output* out))
      (handler-case
          (multiple-value-bind (term varinfo) (pl-parse-string goal-text)
            (let ((n 0))
              (pl-do-solutions term
                               (lambda ()
                                 (setq n (+ n 1))
                                 (let ((a (%pl-answer-string varinfo))) (format out "~A~%" (if (string= a "") "true" a)))
                                 (and (>= n limit) :stop)))
              (when (= n 0) (format out "false~%"))))
        (pl-error (c) (format out "ERROR: ~A~%" (pl-error-message (pl-error-ball c))))
        (pl-halt () (format out "% halt~%"))))))

(defun %pl-run-query-in-editor (ed text)
  (let ((answers (pl-query-output text)))
    (%show-text-buffer ed "*Prolog Output*" (format nil "?- ~A~%~A" (string-trim '(#\Space #\Newline) text) answers))))

(defcmd prolog-query (ed arg)
  (ed-read-string ed "Query: " (lambda (s) (when (> (length (string-trim " " s)) 0) (%pl-run-query-in-editor ed s)))))

(defcmd prolog-run-line-query (ed arg)
  ;; run the goal on the current line: text after `?-' if present, else the whole line
  (with-b (b ed)
    (let* ((line (string-trim '(#\Space #\Tab) (buf-current-line b))) (p (search "?-" line))
           (text (string-trim '(#\Space #\Tab) (if p (subseq line (+ p 2)) line))))
      (when (= (length text) 0) (error "The line holds no goal"))
      (%pl-run-query-in-editor ed text))))

(defcmd prolog-toggle-trace (ed arg)
  (setq *pl-trace* (not *pl-trace*))
  (ed-message* ed "Prolog tracing ~:[off~;on~]" *pl-trace*))

;;; ---- the predicate at point -------------------------------------------------------------------------------------------------------

(defun %pl-word-at (b)
  (let* ((text (buf-current-line b)) (col (min (point-col b) (length text))) (s col) (e col))
    (loop (when (or (<= s 0) (not (%pl-alnum-p (char text (- s 1))))) (return)) (setq s (- s 1)))
    (loop (when (or (>= e (length text)) (not (%pl-alnum-p (char text e)))) (return)) (setq e (+ e 1)))
    (and (> e s) (subseq text s e))))

(defun %pl-arity-at (b)
  "The arity of the call whose name is at point: counted from the text after the name (0 when there is no argument list)."
  (let* ((text (buf-current-line b)) (col (min (point-col b) (length text))) (e col))
    (loop (when (or (>= e (length text)) (not (%pl-alnum-p (char text e)))) (return)) (setq e (+ e 1)))
    (if (and (< e (length text)) (char= (char text e) #\())
        (let ((depth 0) (commas 0) (i e))
          (loop
            (when (>= i (length text)) (return (+ commas 1)))
            (let ((c (char text i)))
              (cond ((find c "([{") (setq depth (+ depth 1)))
                    ((find c ")]}") (setq depth (- depth 1)) (when (= depth 0) (return (+ commas 1))))
                    ((and (char= c #\,) (= depth 1)) (setq commas (+ commas 1)))))
            (setq i (+ i 1))))
        0)))

(defun %pl-edit-definition (ed)
  (with-b (b ed)
    (let ((word (%pl-word-at b)))
      (unless word (error "Point is not on a name"))
      (let* ((atom (pl-atom word)) (arity (%pl-arity-at b))
             (local (find-if (lambda (g) (eq (first (plgroup-key g)) atom)) (%pl-predicates-of-buffer b))))
        (cond
          (local (push-mark ed) (%pl-goto b (plgroup-start local)) (ed-message* ed "~A/~D" word (second (plgroup-key local))))
          (t
           (let* ((pred (or (pl-find-pred atom arity) (find-if (lambda (p) (eq (plpred-name p) atom)) *pl-all-preds*)))
                  (c (and pred (car (plpred-clauses pred)))) (file (and c (plclause-file c))))
             (cond ((and file (probe-file file))
                    (let ((nb (ed-visit-file ed file)))
                      (when (plclause-line c) (buf-goto nb (max 0 (min (- (plclause-line c) 1) (- (buf-nlines nb) 1))) 0))
                      (ed-message* ed "~A" file)))
                   (t (ed-message* ed "No definition of ~A is known" word))))))))))

(%pl-chain edit-definition (ed arg) (%pl-edit-definition ed))

(defcmd prolog-edit-definition (ed arg) (%pl-edit-definition ed))

(defcmd prolog-list-predicate (ed arg)
  (with-b (b ed)
    (let* ((word (%pl-word-at b)) (preds (and word (%pl-user-preds-named (pl-atom word) nil))))
      (if preds
          (%show-text-buffer ed "*Prolog Listing*" (apply #'concatenate 'string (mapcar #'pl-listing-string preds)))
          (ed-message* ed "~:[No predicate here~;No predicate named ~:*~A is defined~]" word)))))

(%pl-chain list-definitions (ed arg)
  (with-b (b ed)
    (%show-text-buffer ed "*Definitions*"
                       (format nil "~{~A~%~}" (mapcar (lambda (g) (format nil "~A/~D  (line ~D, ~D clause~:P)" (pl-atom-text (first (plgroup-key g)) t) (second (plgroup-key g)) (plgroup-line g) (length (plgroup-units g))))
                                                      (%pl-predicates-of-buffer b))))))

;;; ---- changed definitions ---------------------------------------------------------------------------------------------------------------

(defun %pl-predicate-texts (b)
  "Hash of predicate key -> text for the predicates of B."
  (let ((h (make-hash-table :test 'equal)) (text (buffer-string b)))
    (dolist (g (%pl-predicates-of-buffer b) h)
      (setf (gethash (list (symbol-name (first (plgroup-key g))) (second (plgroup-key g))) h) (subseq text (plgroup-start g) (min (length text) (plgroup-end g)))))))

(defun pl-reset-baselines (b &optional (which '(:read :saved :compiled)))
  "Record the current text of every predicate of B as the baseline for each of WHICH."
  (dolist (w which)
    (setf (buf-local b (intern (format nil "PL-BASELINE-~A" w) "KEYWORD")) (%pl-predicate-texts b))))

(defun %pl-baseline (b which)
  (let ((key (intern (format nil "PL-BASELINE-~A" which) "KEYWORD")))
    (or (buf-local b key) (progn (pl-reset-baselines b (list which)) (buf-local b key)))))

(defun pl-note-compiled (b)
  "Mark every predicate of B as compiled with its current text."
  (setf (buf-local b (intern "PL-BASELINE-COMPILED" "KEYWORD")) (%pl-predicate-texts b)))

(defun pl-changed-predicates (b which)
  "The predicates of B (as (key start end line units)) whose text differs from the WHICH baseline."
  (let ((h (%pl-baseline b which)) (text (buffer-string b)) (acc nil))
    (dolist (g (%pl-predicates-of-buffer b) (nreverse acc))
      (let ((k (list (symbol-name (first (plgroup-key g))) (second (plgroup-key g)))))
        (unless (equal (gethash k h) (subseq text (plgroup-start g) (min (length text) (plgroup-end g)))) (push g acc))))))

(defun %pl-changed-which (arg)
  (cond ((eql arg 1) :read) ((eql arg 2) :saved) (t :compiled)))

(defun %pl-predicate-possibility (b g)
  (make-possibility (format nil "~A/~D (~A)" (pl-atom-text (first (plgroup-key g)) t) (second (plgroup-key g)) (buffer-name b))
                    (lambda (e) (ed-switch-to-buffer e b) (%pl-goto b (plgroup-start g)))))

(%pl-chain edit-changed-definitions-of-buffer (ed arg)
  (let* ((b (ed-buffer ed)) (which (%pl-changed-which arg)))
    (ed-set-possibilities ed (format nil "definitions changed ~(~A~)" which)
                          (mapcar (lambda (g) (%pl-predicate-possibility b g)) (pl-changed-predicates b which)) :go t)))

(%pl-chain list-changed-definitions (ed arg)
  (let* ((b (ed-buffer ed)) (which (%pl-changed-which arg)))
    (ed-set-possibilities ed (format nil "definitions changed ~(~A~)" which)
                          (mapcar (lambda (g) (%pl-predicate-possibility b g)) (pl-changed-predicates b which)) :go nil)))

(%pl-chain compile-changed-definitions-of-buffer (ed arg)
  (let* ((b (ed-buffer ed)) (changed (pl-changed-predicates b (%pl-changed-which arg))))
    (if (null changed)
        (ed-message* ed "No definitions changed")
        (let ((n 0))
          (dolist (g changed) (%pl-compile-in-editor ed (plgroup-start g) (plgroup-end g)) (setq n (+ n 1)))
          (ed-message* ed "~D predicate~:P compiled" n)))))

;;; ---- keys --------------------------------------------------------------------------------------------------------------------------

(dolist (k '(("<tab>" prolog-tab) ("C-j" prolog-newline-and-indent)
             ("C-M-a" prolog-beginning-of-clause) ("C-M-e" prolog-end-of-clause) ("C-M-h" prolog-mark-clause)
             ("C-M-f" prolog-forward-clause) ("C-M-b" prolog-backward-clause)
             ("C-c C-c" compile-region) ("C-c C-b" compile-buffer) ("C-c C-q" prolog-query) ("C-c C-e" prolog-run-line-query)
             ("C-c C-l" prolog-list-predicate) ("C-c C-t" prolog-toggle-trace) ("C-c C-w" edit-compiler-warnings)
             ("M-." prolog-edit-definition)))
  (define-key *pl-mode-map* (car k) (cadr k)))
