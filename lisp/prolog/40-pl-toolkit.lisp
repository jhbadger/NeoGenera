;;;; 40-pl-toolkit.lisp -- the Prolog Tool Kit: consulting, the compiler warnings database, the interactive toplevel, calling
;;;; Prolog from Lisp and Lisp from Prolog, the PROLOG package, systems integration and the Listener commands.

(in-package :cl)

;;; ---- warnings --------------------------------------------------------------------------------------------------------------------------

(defstruct (plwarning (:copier nil)) source start end line col message kind)

(defvar *pl-compiler-warnings* nil
  "The compiler warnings database: (source . list-of-PLWARNING) for each source consulted with warnings or errors.")

(defun %pl-record-warnings (source warnings)
  (setq *pl-compiler-warnings* (remove source *pl-compiler-warnings* :key #'car :test #'equal))
  (when warnings
    (setq *pl-compiler-warnings* (append *pl-compiler-warnings* (list (cons source warnings))))))

(defun pl-compiler-warnings (&optional source)
  "The pending compiler warnings (all of them, or those of SOURCE) as PLWARNINGs."
  (if source (cdr (assoc source *pl-compiler-warnings* :test #'equal)) (apply #'append (mapcar #'cdr *pl-compiler-warnings*))))

(defvar *pl-quiet* nil "True to suppress printing of warnings while consulting (they are still recorded).")

(defun %pl-offset-of-line (text line)
  (let ((l 1) (i 0) (n (length text)))
    (loop (when (or (>= l line) (>= i n)) (return i))
          (when (char= (char text i) #\Newline) (setq l (+ l 1)))
          (setq i (+ i 1)))))

(defun %pl-warn (text source warnings-cell start line col message kind)
  (let ((w (make-plwarning :source source :start start :end (min (length text) (+ start 1)) :line line :col col :message message :kind kind)))
    (setf (car warnings-cell) (append (car warnings-cell) (list w)))
    (unless *pl-quiet*
      (format *error-output* "~&~A: ~A:~D~@[:~D~]:~%~A:    ~A~%" (if (eq kind :error) "ERROR" "Warning") source line col
              (if (eq kind :error) "ERROR" "Warning") message))
    w))

;;; ---- consulting ----------------------------------------------------------------------------------------------------------------------

(defvar *pl-initialization-goals* nil)
(defvar *pl-consult-depth* 0)

(defun %pl-expand-term (term)
  "The list of clause terms TERM stands for: user term_expansion/2 first, then DCG translation."
  (let ((te (pl-find-pred (pl-atom "term_expansion") 2)) (result nil) (found nil))
    (when (and te (plpred-clauses te))
      (let ((out (pl-new-var)))
        (pl-do-solutions (pl-s "term_expansion" term out)
                         (lambda () (setq result (pl-resolve out) found t) :stop))))
    (let ((items (if found (if (or (pl-is-cons result) (eq result *pl-nil*)) (%pl-list-arg result) (list result)) (list term))))
      (mapcar (lambda (x) (let ((v (%deref x))) (if (pl-functor-p v *pl-dcg-arrow* 2) (pl-dcg-rule v) v))) items))))

(defun %pl-run-directive (goal text source cell start line col)
  (handler-case
      (unless (pl-prove goal)
        (%pl-warn text source cell start line col (format nil "Goal (directive) failed: user:~A" (pl-term-to-string goal :quoted t :numbervars t)) :warning))
    (pl-error (c) (%pl-warn text source cell start line col (pl-error-message (pl-error-ball c)) :error))))

(defun %pl-singletons (varinfo)
  (let ((acc nil))
    (dolist (e varinfo (nreverse acc))
      (when (and (= (third e) 1) (> (length (first e)) 0) (char/= (char (first e) 0) #\_)) (push (first e) acc)))))

(defun pl-consult-string (text &key file library (source (or file "Prolog text")))
  "Consult the Prolog TEXT: clauses are added to the database, directives run.  A predicate previously defined from the same
SOURCE is replaced.  Returns (values clause-count warnings); the warnings are also recorded for SOURCE."
  (let ((pos 0) (count 0) (cell (list nil)) (seen nil) (inits nil) (*pl-load-file* (or file *pl-load-file*))
        (*pl-consult-depth* (+ *pl-consult-depth* 1)))
    (loop
      (let ((r (handler-case (multiple-value-list (pl-read-term-from-string text pos))
                 (pl-error (c)
                   (let* ((b (%deref (pl-error-ball c))) (ctx (and (pl-functor-p b *pl-error* 2) (%deref (pl-arg b 1))))
                          (position (and ctx (pl-functor-p ctx (pl-atom "context") 2) (%deref (pl-arg ctx 0)))))
                     (let ((line (if (and position (pl-functor-p position (pl-atom "position") 3)) (%deref (pl-arg position 1)) 1))
                           (col (if (and position (pl-functor-p position (pl-atom "position") 3)) (%deref (pl-arg position 2)) 0)))
                       (setq pos (pl-read-recover-position text pos))
                       (%pl-warn text source cell (%pl-offset-of-line text line) line col (pl-error-message b) :error)
                       (list :error)))))))
        (cond
          ((eq (car r) :error) nil)
          ((eq (car r) :eof) (return))
          (t
           (setq pos (fifth r))
           (let* ((term (%deref (first r))) (varinfo (second r)) (start (third r)) (line (%pl-line-col text start)))
             (cond
               ((or (pl-functor-p term *pl-neck* 1) (pl-functor-p term (pl-atom "?-") 1))
                (let ((goal (%deref (pl-arg term 0))))
                  (cond ((pl-functor-p goal (pl-atom "initialization") 1) (push goal inits))
                        ((pl-functor-p goal (pl-atom "initialization") 2)
                         (if (eq (%deref (pl-arg goal 1)) (pl-atom "now")) (%pl-run-directive (pl-arg goal 0) text source cell start line nil)
                             (push (%make-pls (pl-atom "initialization") (vector (pl-arg goal 0))) inits)))
                        (t (%pl-run-directive goal text source cell start line nil)))))
               (t
                (let ((singles (and (not library) (%pl-singletons varinfo))))
                  (when singles
                    (%pl-warn text source cell start line nil (format nil "Singleton variables: [~{~A~^,~}]" singles) :warning)))
                (handler-case
                    (dolist (clause (%pl-expand-term term))
                      (multiple-value-bind (head body) (pl-split-clause clause)
                        (declare (ignore body))
                        (let* ((h (%deref head)) (pred (and (pl-callable-p h) (pl-find-pred (pl-name h) (pl-arity h)))))
                          (when (and pred (not (memq pred seen)) (if library (plpred-library pred) (equal (plpred-file pred) source)))
                            (pl-wipe-pred pred))
                          (pl-assert-clause clause :file source :offset start :line line :library library)
                          (let ((p2 (pl-find-pred (pl-name (%deref head)) (pl-arity (%deref head)))))
                            (when p2 (pushnew p2 seen) (unless library (setf (plpred-file p2) source)))))
                        (setq count (+ count 1))))
                  (pl-error (c) (%pl-warn text source cell start line nil (pl-error-message (pl-error-ball c)) :error))))))))))
    (%pl-record-warnings source (car cell))
    (dolist (g (reverse inits)) (%pl-run-directive (pl-arg g 0) text source cell 0 1 nil))
    (when inits (%pl-record-warnings source (car cell)))
    (values count (car cell))))

(defun pl-source-file-p (path)
  "True when PATH has a Prolog file type: .pl, .pro or .prolog."
  (let ((n (string-downcase (namestring path))))
    (or (%ends-with n ".pl") (%ends-with n ".pro") (%ends-with n ".prolog"))))

(defun %pl-resolve-file (name)
  "The existing file NAME denotes: relative to the file being consulted, with .pl, .pro or .prolog added if needed."
  (let* ((n (%expand-home name))
         (dir (and *pl-load-file* (not (and (> (length n) 0) (char= (char n 0) #\/))) (directory-namestring *pl-load-file*)))
         (cands (append (list n) (when dir (list (concatenate 'string dir n)))))
         (found nil))
    (dolist (c cands)
      (dolist (ext '("" ".pl" ".pro" ".prolog"))
        (let ((f (concatenate 'string c ext)))
          (when (and (not found) (probe-file f) (not (and (string= ext "") (let ((pp (namestring (probe-file f)))) (and (> (length pp) 0) (char= (char pp (- (length pp) 1)) #\/))))))
            (setq found f)))))
    found))

(defun pl-consult-file (path &key library)
  "Consult the Prolog file PATH.  Returns (values clause-count warnings)."
  (let* ((file (or (%pl-resolve-file (namestring path)) (pl-existence-error "source_sink" (pl-atom (namestring path)))))
         (text (%read-file-string file)))
    (when (and (> (length text) 1) (string= (subseq text 0 2) "#!"))
      (setq text (concatenate 'string "%" (subseq text 1))))
    (pl-consult-string text :file (namestring (probe-file file)) :library library)))

(defun %pl-consult-spec (spec)
  (let ((s (%deref spec)))
    (cond ((plv-p s) (pl-instantiation-error))
          ((or (pl-is-cons s) (eq s *pl-nil*)) (dolist (x (%pl-list-arg s)) (%pl-consult-spec x)))
          ((pl-functor-p s (pl-atom "library") 1) nil)
          (t (pl-consult-file (%pl-text-arg s))))
    t))

(pl-def "consult" 1 (spec) (%pl-consult-spec spec))
(pl-def "ensure_loaded" 1 (spec) (%pl-consult-spec spec))
(pl-def "include" 1 (spec) (%pl-consult-spec spec))
(pl-def "load_files" 2 (spec opts) (declare (ignore opts)) (%pl-consult-spec spec))
(pl-def "." 2 (a b) (%pl-consult-spec (pl-cons a b)))
(pl-def "initialization" 1 (goal) (pl-solve-once goal) t)
(pl-def "initialization" 2 (goal when) (declare (ignore when)) (pl-solve-once goal) t)
(pl-def "expand_term" 2 (term out)
  (let ((v (%deref term))) (pl-unify out (if (pl-functor-p v *pl-dcg-arrow* 2) (pl-dcg-rule v) v))))
(pl-def "dcg_translate_rule" 2 (rule out) (pl-unify out (pl-dcg-rule (%deref rule))))
(pl-def "$dcg_body" 4 (body s0 s goal) (pl-unify goal (pl-dcg-body body s0 s)))

;;; ---- the top level ------------------------------------------------------------------------------------------------------------------------

(defun %pl-answer-string (varinfo)
  "The bindings of the query variables in VARINFO as SWI-Prolog prints them: X = a, Y = f(b)."
  (let ((parts nil))
    (dolist (e varinfo)
      (let ((v (%deref (second e))))
        (unless (or (plv-p v) (and (> (length (first e)) 0) (char= (char (first e) 0) #\_)))
          (push (format nil "~A = ~A" (first e) (pl-term-to-string v :quoted t :numbervars t :spacing t :max-depth 12 :priority 699)) parts))))
    (format nil "~{~A~^,~%~}" (nreverse parts))))

(defun pl-run-toplevel-query (text input output)
  (setq *pl-trace-counter* 0)
  "Run the query TEXT, printing the answers to OUTPUT as the Prolog top level does; after a non-final answer a line is read from
INPUT: `;' asks for the next solution."
  (handler-case
      (multiple-value-bind (goal varinfo) (pl-parse-string text)
        (let* ((saved-hb *pl-hb*) (m (%pl-new-machine goal)) (barrier (plm-cps m)) (found nil))
          (unwind-protect
               (let ((r (%pl-drive m)))
                 (loop
                   (when (null r)
                     (unless found (format output "false.~%"))
                     (return))
                   (setq found t)
                   (let ((answer (%pl-answer-string varinfo)) (more (not (eq (plm-cps m) barrier))))
                     (cond ((string= answer "") (format output "true"))
                           (t (write-string answer output)))
                     (cond ((not more) (format output ".~%") (return))
                           (t (format output " ") (force-output output)
                              (let ((line (read-line input nil nil)))
                                (cond ((and line (> (length (string-trim " " line)) 0) (char= (char (string-trim " " line) 0) #\;))
                                       (format output ";~%")
                                       (setq r (%pl-drive m t)))
                                      (t (format output ".~%") (return)))))))))
            (%pl-undo-to (plcp-trail barrier))
            (setq *pl-hb* saved-hb))))
    (pl-error (c) (format output "~&ERROR: ~A~%" (pl-error-message (pl-error-ball c))))))

(defun pl-toplevel (&key (input *standard-input*) (output *standard-output*) (prompt "?- "))
  "Read queries from INPUT and answer them on OUTPUT until end of file, `halt.' or `end_of_file.'."
  (let ((*standard-output* output))
    (loop
      (format output "~&~A" prompt) (force-output output)
      (let ((text (pl-read-clause-text input)))
        (when (null text) (return))
        (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
          (cond ((member trimmed '("halt." "end_of_file." "halt .") :test #'string=) (return))
                ((string= trimmed ".") nil)
                (t (handler-case (pl-run-toplevel-query text input output)
                     (pl-halt () (return))))))))
    (format output "~&% halt~%")
    t))

;;; ---- calling Prolog from Lisp ---------------------------------------------------------------------------------------------------------------

(defun %pl-goal-term (goal)
  (if (stringp goal) (pl-parse-string goal) goal))

(defun pl-query (goal &key limit raw)
  "All solutions of GOAL (a string or a term) as a list, one element per solution: an alist (name . value) of the variables
of the query (values converted to Lisp unless RAW), so a successful query without variables gives (NIL).  LIMIT bounds the count."
  (multiple-value-bind (term varinfo) (if (stringp goal) (pl-parse-string goal) (values goal (mapcar (lambda (v) (list (format nil "_G~D" (pl-var-id v)) v 2)) (pl-variables-of goal))))
    (let ((acc nil) (n 0))
      (pl-do-solutions term
                       (lambda ()
                         (push (mapcar (lambda (e) (cons (first e) (if raw (pl-resolve (second e)) (pl-to-lisp (second e))))) varinfo) acc)
                         (setq n (+ n 1))
                         (and limit (>= n limit) :stop)))
      (nreverse acc))))

(defun pl-prove-goal (goal)
  "True if GOAL (a string or a term) has a solution."
  (pl-prove (%pl-goal-term goal)))

(defmacro pl-do-query ((var goal &key limit) &body body)
  "Run BODY once for each solution of the Prolog query GOAL (a string), with VAR bound to the alist of the query variables."
  `(dolist (,var (pl-query ,goal :limit ,limit)) ,@body))

(defun pl-assert-fact (clause)
  "Assert the clause (a string or a term) at the end of its predicate."
  (pl-assert-clause (pl-copy-term (%pl-goal-term clause))) t)

(defun pl-reset ()
  "Remove the user's predicates and global variables; restore the standard operators and flags."
  (dolist (p *pl-all-preds*)
    (unless (or (plpred-ctl p) (plpred-fn p) (plpred-library p))
      (pl-wipe-pred p) (setf (plpred-defined p) nil (plpred-dynamic p) nil (plpred-file p) nil (plpred-spy p) nil)))
  (clrhash *pl-globals*)
  (%pl-install-default-ops)
  (%pl-reset-flags)
  (setq *pl-compiler-warnings* nil *pl-trace* nil *pl-trace-counter* 0)
  (%pl-load-library)
  t)

;;; ---- calling Lisp from Prolog -------------------------------------------------------------------------------------------------------------------

(defun %pl-lisp-form (text)
  (handler-case (let ((*package* (find-package "CL-USER"))) (read-from-string text))
    (error (e) (pl-throw (pl-error-term (pl-s "system_error" (format nil "~A" e)))))))

(defun %pl-lisp-eval (form-text)
  (let ((form (%pl-lisp-form form-text)))
    (handler-case (eval form)
      (pl-error (c) (error c))
      (pl-halt (c) (error c))
      (error (e) (pl-throw (pl-error-term (pl-s "system_error" (format nil "~A" e))))))))

(pl-def "lisp" 1 (form) (and (%pl-lisp-eval (%pl-text-arg form)) t))
(pl-def "lisp" 2 (result form) (pl-unify result (pl-from-lisp (%pl-lisp-eval (%pl-text-arg form)))))
(pl-def "lisp_eval" 2 (form result) (pl-unify result (pl-from-lisp (%pl-lisp-eval (%pl-text-arg form)))))
(pl-def "lisp_call" 3 (name args result)
  (let* ((n (%pl-text-arg name))
         (sym (let ((u (string-upcase n))) (or (find-symbol u "CL-USER") (find-symbol (substitute #\- #\_ u) "CL-USER"))))
         (fn (and sym (fboundp sym) (symbol-function sym))))
    (unless fn (pl-existence-error "lisp_function" (%deref name)))
    (let ((argv (mapcar #'pl-to-lisp (%pl-list-arg args))))
      (pl-unify result (pl-from-lisp (handler-case (apply fn argv)
                                       (pl-error (c) (error c)) (pl-halt (c) (error c))
                                       (error (e) (pl-throw (pl-error-term (pl-s "system_error" (format nil "~A" e)))))))))))

(defun pl-define-predicate (name arity fn &key library)
  "Define the deterministic Prolog predicate NAME/ARITY implemented by the Lisp function FN, which receives the argument
terms and returns true or false (use PL-UNIFY to bind)."
  (%pl-register name arity :fn fn :library library))

(defmacro pl-defpredicate (name (&rest params) &body body)
  "Define the deterministic Prolog predicate NAME in Lisp: PARAMS name the argument terms (see PL-TO-LISP, PL-FROM-LISP and PL-UNIFY)."
  `(pl-define-predicate ,name ,(length params) (lambda ,params ,@body) :library t))

(defun pl-define-solutions (name arity fn &key library)
  "Define the non-deterministic predicate NAME/ARITY: FN receives the argument terms and returns a list of solutions, each a
list of Lisp values to be unified (as terms) with the arguments in turn."
  (%pl-register name arity :kind :nondet :library library
                :fn (lambda (&rest args)
                      (let ((sols (apply fn args)))
                        (%pl-alternatives sols (lambda (sol) (every (lambda (a v) (pl-unify a (pl-from-lisp v))) args sol)))))))

(defmacro pl-defsolutions (name (&rest params) &body body)
  `(pl-define-solutions ,name ,(length params) (lambda ,params ,@body) :library t))

;;; ---- compiling files ----------------------------------------------------------------------------------------------------------------------------

(defun pl-load-file (path)
  "Consult the Prolog file PATH; signals an error listing its problems if there were any."
  (multiple-value-bind (n warnings) (pl-consult-file path)
    (declare (ignore n))
    (let ((errs (remove-if-not (lambda (w) (eq (plwarning-kind w) :error)) warnings)))
      (when errs (error "Prolog file ~A has errors:~%~{~A~%~}" path (mapcar #'plwarning-message errs))))
    t))

(defun pl-compile-file (path &key output-file load)
  "Compile the Prolog file PATH to a compiled-code file (by default PATH with type nfasl) that consults the program when loaded."
  (let* ((path (namestring path)) (text (%read-file-string path))
         (out (or output-file (namestring (make-pathname :type "nfasl" :defaults (pathname path)))))
         (lisp-file (concatenate 'string out ".lisp")))
    (with-open-file (o lisp-file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (let ((*package* (find-package "COMMON-LISP")))
        (format o ";;; The Prolog file ~A, for loading as compiled code -- do not edit.~%" path)
        (prin1 `(pl-consult-string ,text :file ,path) o) (terpri o)))
    (unwind-protect (compile-file lisp-file :output-file out)
      (ignore-errors (delete-file lisp-file)))
    (when load (load out))
    out))

(push (list #'pl-source-file-p #'pl-load-file (lambda (file fasl) (pl-compile-file file :output-file fasl)))
      *system-file-handlers*)

;;; ---- the PROLOG package ---------------------------------------------------------------------------------------------------------------------------------

(let ((p (or (find-package "PROLOG") (%make-package "PROLOG" nil nil))))
  (flet ((fn (name f) (setf (symbol-function (intern name p)) f) (export (intern name p) p))
         (mac (name target)
           (let ((s (intern name p)))
             (eval `(defmacro ,s (&rest args) (cons ',target args)))
             (export s p)))
         (var (name value)
           (let ((s (intern name p)))
             (eval `(defvar ,s ,value))
             (export s p))))
    (fn "CONSULT" (lambda (path) (pl-consult-file path)))
    (fn "CONSULT-STRING" #'pl-consult-string)
    (fn "COMPILE-FILE" #'pl-compile-file)
    (fn "LOAD-FILE" #'pl-load-file)
    (fn "QUERY" #'pl-query)
    (fn "PROVE" #'pl-prove-goal)
    (fn "ASSERT" #'pl-assert-fact)
    (fn "PARSE" (lambda (text) (values (pl-parse-string text))))
    (fn "TERM-STRING" (lambda (term &key quoted) (pl-term-to-string term :quoted quoted :numbervars t)))
    (fn "FROM-LISP" #'pl-from-lisp)
    (fn "TO-LISP" #'pl-to-lisp)
    (fn "UNIFY" #'pl-unify)
    (fn "DEREF" #'pl-deref)
    (fn "MAKE-VARIABLE" #'pl-new-var)
    (fn "MAKE-COMPOUND" (lambda (name &rest args) (apply #'pl-s name args)))
    (fn "ATOM" #'pl-atom)
    (fn "RESET" #'pl-reset)
    (fn "TOPLEVEL" #'pl-toplevel)
    (fn "WARNINGS" #'pl-compiler-warnings)
    (fn "DEFINE-PREDICATE" #'pl-define-predicate)
    (fn "DEFINE-SOLUTIONS" #'pl-define-solutions)
    (fn "TRACE" (lambda (&optional (on t)) (setq *pl-trace* on)))
    (mac "DO-QUERY" 'pl-do-query)
    (mac "DEFPREDICATE" 'pl-defpredicate)
    (mac "DEFSOLUTIONS" 'pl-defsolutions)))

;;; ---- the library -----------------------------------------------------------------------------------------------------------------------------------------

(defmacro %pl-library-text (path)
  "The text of the Prolog library file PATH, read when the form is compiled (like the manuals)."
  (join-lines (read-file-lines (resolve-source-path path))))

(defun %pl-load-library ()
  (pl-consult-string (%pl-library-text "lisp/prolog/library.pl") :library t :source "library"))

(%pl-load-library)

;;; ---- Listener commands -------------------------------------------------------------------------------------------------------------------------------

(defun %pl-report-consult (path)
  (handler-case
      (multiple-value-bind (n warnings) (pl-consult-file path)
        (let ((errors (count :error warnings :key #'plwarning-kind)))
          (format t "~A consulted, ~D clause~:P~@[, ~D error~:P~]~%" path n (and (> errors 0) errors))))
    (pl-error (c) (format t "ERROR: ~A~%" (pl-error-message (pl-error-ball c))))
    (pl-halt () (format t "% halt~%"))))

(define-command (consult-prolog :documentation "Load a Prolog file into the Prolog database") ((path string :prompt "Prolog file"))
  (%pl-report-consult path))

(define-command (compile-prolog :documentation "Load a Prolog file into the Prolog database") ((path string :prompt "Prolog file"))
  (%pl-report-consult path))

(define-command (load-prolog :documentation "Load a Prolog file into the Prolog database") ((path string :prompt "Prolog file"))
  (%pl-report-consult path))

(define-command (prolog :documentation "Enter the Prolog top level: type goals ending with a full stop; ; asks for the next answer; halt. returns")
    ()
  (pl-toplevel))

(define-command (prolog-query :documentation "Run a Prolog query and print all its answers")
    ((goal string :prompt "Goal"))
  (handler-case
      (multiple-value-bind (term varinfo) (pl-parse-string goal)
        (let ((n 0))
          (pl-do-solutions term
                           (lambda ()
                             (setq n (+ n 1))
                             (let ((a (%pl-answer-string varinfo))) (format t "~A~%" (if (string= a "") "true" a)))
                             (and (>= n 100) :stop)))
          (when (= n 0) (format t "false~%"))))
    (pl-error (c) (format t "ERROR: ~A~%" (pl-error-message (pl-error-ball c))))))

(define-command (show-prolog-predicates :documentation "List the user's Prolog predicates and their clause counts") ()
  (let ((ps (remove-if-not (lambda (p) (and (%pl-user-pred-p p) (not (plpred-library p)))) (reverse *pl-all-preds*))))
    (if ps
        (dolist (p ps) (format t "~A/~D~:[~; (dynamic)~]  ~D clause~:P~%" (pl-atom-text (plpred-name p) t) (plpred-arity p) (plpred-dynamic p) (plpred-nclauses p)))
        (format t "No Prolog predicates are defined.~%"))))

(define-command (show-prolog-warnings :documentation "List the pending Prolog compiler warnings") ()
  (let ((ws (pl-compiler-warnings)))
    (if ws (dolist (w ws) (format t "~A: line ~D: ~A~%" (plwarning-source w) (plwarning-line w) (plwarning-message w)))
        (format t "No compiler warnings.~%"))))

(define-command (reset-prolog :documentation "Remove the user's Prolog predicates and restore operators and flags") ()
  (pl-reset) (format t "Prolog database reset~%"))

(define-command (trace-prolog :documentation "Turn tracing of Prolog calls on or off") ((state word :prompt "On or Off" :default "on"))
  (setq *pl-trace* (and (member (string-downcase state) '("on" "yes" "t" "true") :test #'string=) t))
  (format t "Prolog tracing ~:[off~;on~]~%" *pl-trace*))
