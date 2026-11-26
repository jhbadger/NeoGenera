;;;; 40-toolkit.lisp -- the Pascal Tool Kit: running and compiling programs, units, systems, commands.
;;;;
;;;; PASCAL-EXECUTE runs a compiled main program, as pascal:execute did on Genera, with the same options
;;;; (:INPUT, :OUTPUT, :INIT-TO-ZERO, :SAVE-ENVIRONMENT, ...).  Compiling a string, a file or an editor buffer
;;;; translates the Pascal to Lisp and compiles that; the warnings of the last compilation of each file are kept
;;;; in a database that the editor's Compiler Warnings commands show.  DEF-PROGRAM declares a system of Pascal
;;;; (and Lisp) files for LOAD-SYSTEM and COMPILE-SYSTEM.

(in-package :cl)

;;; ---- programs ---------------------------------------------------------------------------------------------

(defstruct (pprogram (:copier nil)) name package main globals uses file dialect saved)

(defmethod print-object ((p pprogram) s)
  (format s "#<Pascal program ~A:~A>" (pprogram-package p) (string-downcase (pprogram-name p))))

(defvar *pascal-programs* (make-hash-table :test 'equal) "Compiled Pascal main programs, by \"PACKAGE:NAME\".")
(defvar *pascal-unit-info* (make-hash-table :test 'equal)
  "For each unit \"PACKAGE:NAME\": (:uses names :implemented flag :file file).")

(defun %pascal-define-program (name package main globals uses file dialect)
  (let ((old (gethash (%string-concat package ":" name) *pascal-programs*)))
    (setf (gethash (%string-concat package ":" name) *pascal-programs*)
          (make-pprogram :name name :package package :main main :globals globals :uses uses :file file :dialect dialect
                         :saved (and old (pprogram-saved old))))
    (intern name package)))

(defun %pascal-note-unit-spec (name package uses source dialect file)
  "Record the unit specification NAME; when it is loaded from a compiled file, translate its SOURCE again so
later compilations see its declarations."
  (let ((key (%string-concat package ":" name)))
    (unless (gethash key *pascal-units*)
      (pascal-translate source :dialect dialect :package package :file file))
    (setf (gethash key *pascal-unit-info*)
          (list :uses uses :implemented (getf (gethash key *pascal-unit-info*) :implemented) :file file))
    (intern name package)))

(defun %pascal-note-unit-impl (name package)
  (let ((key (%string-concat package ":" name)))
    (setf (getf (gethash key *pascal-unit-info*) :implemented) t)
    (intern name package)))

(defun pascal-programs ()
  "The compiled Pascal main programs, as symbols (package-qualified program names)."
  (let ((acc nil))
    (maphash (lambda (k p) (declare (ignore k)) (push (intern (pprogram-name p) (pprogram-package p)) acc)) *pascal-programs*)
    (sort acc #'string< :key #'symbol-name)))

(defun %pascal-find-program (name)
  (let* ((str (string-upcase (if (symbolp name) (symbol-name name) (string name))))
         (colon (%string-position-char str #\: 0))
         (pkg (cond (colon (subseq str 0 colon))
                    ((and (symbolp name) (symbol-package name)) (package-name (symbol-package name)))
                    (t nil)))
         (short (if colon (string-left-trim ":" (subseq str colon)) str)))
    (or (and pkg (gethash (%string-concat pkg ":" short) *pascal-programs*))
        (let ((found nil))
          (maphash (lambda (k p) (declare (ignore k)) (when (string= (pprogram-name p) short) (push p found))) *pascal-programs*)
          (cond ((null found) (error "~A is not a compiled Pascal main program" (string-downcase short)))
                ((cdr found) (error "There are Pascal programs named ~A in several packages (~{~A~^, ~}): give the package"
                                    (string-downcase short) (mapcar #'pprogram-package found)))
                (t (car found)))))))

;;; ---- running ------------------------------------------------------------------------------------------------

(defun %pascal-file-from (source direction name)
  "A PFILE for the :INPUT or :OUTPUT option SOURCE: a file name, a pathname or a stream."
  (let ((textfile (%make-pfile :name name :mode direction)))
    (if (streamp source)
        (progn (setf (pfile-stream textfile) source)
               (when (eq direction :read) (setf (pfile-reader textfile) (lambda () (read-line source nil :eof)))))
        (let ((path (namestring (merge-pathnames source (pathname (or *pascal-pathname-defaults* ""))))))
          (if (eq direction :read)
              (let ((s (open path)))
                (setf (pfile-stream textfile) s (pfile-owned textfile) t (pfile-reader textfile) (lambda () (read-line s nil :eof))))
              (setf (pfile-stream textfile) (open path :direction :output :if-exists :supersede :if-does-not-exist :create)
                    (pfile-owned textfile) t))))
    textfile))

(defun %pascal-wrap-error (c)
  "Give errors signalled by Lisp inside a Pascal program the Pascal routine and line."
  (when (and (not (typep c 'pascal-runtime-error)) *pascal-routine*)
    (error 'pascal-runtime-error :message (princ-to-string c) :routine *pascal-routine* :line *pascal-line* :original c)))

(defun pascal-execute (name &key input output init-to-zero save-environment reload pathname-defaults streams
                              stack-size (trap-underflow t) use-abort-mode)
  "Run the compiled Pascal main program NAME (a symbol or string, optionally package-qualified).  INPUT and OUTPUT
give the standard files: a file name, a pathname or a stream (by default the Listener).  INIT-TO-ZERO starts the
program's variables at zero instead of undefined.  SAVE-ENVIRONMENT keeps the program's global variables from one
run to the next (until RELOAD).  PATHNAME-DEFAULTS is the directory for file names in RESET and REWRITE.  STREAMS
is an alist (pascal-file-name . lisp-stream).  STACK-SIZE, TRAP-UNDERFLOW and USE-ABORT-MODE are accepted for
compatibility and ignored.  Returns NIL."
  (declare (ignore stack-size trap-underflow use-abort-mode))
  (let* ((prog (%pascal-find-program name))
         (*pascal-pathname-defaults* (if pathname-defaults (namestring pathname-defaults) (or *pascal-pathname-defaults* "")))
         (syms (mapcar #'car (pprogram-globals prog)))
         (vals (if (and save-environment (pprogram-saved prog) (not reload))
                   (pprogram-saved prog)
                   (mapcar (lambda (g) (%pmake (cadr g) init-to-zero)) (pprogram-globals prog))))
         (in (cond (input (%pascal-file-from input :read "INPUT"))
                   (t (%make-pfile :name "INPUT" :mode :read :reader (%pascal-terminal-reader) :interactive t))))
         (out (cond (output (%pascal-file-from output :write "OUTPUT"))
                    (t (%make-pfile :name "OUTPUT" :mode :write :stream *standard-output*)))))
    (when reload (setf (pprogram-saved prog) nil))
    (progv syms vals
      (let ((*pascal-input* in) (*pascal-output* out) (*pascal-open-files* nil) (*pascal-program* prog)
            (*pascal-line* 0) (*pascal-routine* nil) (*pascal-streams* streams))
        (unwind-protect
             (catch '%pascal-halt
               (handler-bind ((error #'%pascal-wrap-error))
                 (funcall (pprogram-main prog))))
          (ignore-errors (finish-output (pfile-stream out)))
          (dolist (f *pascal-open-files*) (ignore-errors (%pclose f)))
          (when (pfile-owned in) (ignore-errors (close (pfile-stream in))))
          (when (pfile-owned out) (ignore-errors (close (pfile-stream out))))
          (when save-environment (setf (pprogram-saved prog) (mapcar #'symbol-value syms))))))
    nil))

(defun xqt (name &rest options)
  "A synonym for PASCAL-EXECUTE."
  (apply #'pascal-execute name options))

(defun pascal-run-string (source &rest options &key (package "PASCAL-USER") (dialect *pascal-default-dialect*) &allow-other-keys)
  "Compile the Pascal SOURCE and run its (first) main program with OPTIONS; output goes to *STANDARD-OUTPUT*
unless :OUTPUT says otherwise.  Returns the program's name."
  (let ((names (pascal-compile-string source :package package :dialect dialect)))
    (let ((prog (find-if (lambda (n) (gethash (%string-concat (package-name (symbol-package n)) ":" (symbol-name n)) *pascal-programs*)) names)))
      (unless prog (error "The Pascal text has no main program"))
      (let ((opts nil))
        (do ((l options (cddr l))) ((null l))
          (unless (member (car l) '(:package :dialect)) (setq opts (list* (car l) (cadr l) opts))))
        (apply #'pascal-execute prog opts))
      prog)))

;;; ---- compiler warnings ------------------------------------------------------------------------------------------

(defstruct (pwarning (:copier nil)) source start end line col message)

(defvar *pascal-compiler-warnings* nil
  "The compiler warnings database: (source . list-of-PWARNING) for each file or buffer compiled with errors.")

(defun %pascal-record-warnings (source errors)
  (setq *pascal-compiler-warnings* (remove source *pascal-compiler-warnings* :key #'car :test #'equal))
  (when errors
    (setq *pascal-compiler-warnings*
          (append *pascal-compiler-warnings*
                  (list (cons source (mapcar (lambda (c) (make-pwarning :source source :start (pascal-error-start c) :end (pascal-error-end c)
                                                                       :line (pascal-error-line c) :col (pascal-error-col c)
                                                                       :message (pascal-error-message c)))
                                             errors)))))))

(defun pascal-compiler-warnings (&optional source)
  "The pending compiler warnings (all of them, or those of SOURCE), as PWARNINGs."
  (if source (cdr (assoc source *pascal-compiler-warnings* :test #'equal)) (apply #'append (mapcar #'cdr *pascal-compiler-warnings*))))

(defun %pascal-form-names (forms)
  "The names of what FORMS define: routines, programs and units."
  (let ((acc nil))
    (dolist (f forms)
      (when (consp f)
        (case (car f)
          (defun (push (cadr f) acc))
          ((%pascal-define-program %pascal-note-unit-spec %pascal-note-unit-impl) (push (intern (cadr f) (caddr f)) acc)))))
    (nreverse acc)))

(defun pascal-compile-string (source &key (package "PASCAL-USER") (dialect *pascal-default-dialect*) file (directory "")
                                       only-at (warnings-key file))
  "Compile the Pascal SOURCE into the running world: (values names errors).  NAMES are the programs, units and
routines defined; ERRORS the syntax and semantic errors (then nothing is defined).  ONLY-AT compiles just the unit or
top-level routine at that offset."
  (multiple-value-bind (forms errors) (pascal-translate source :package package :dialect dialect :file file :directory directory :only-at only-at)
    (%pascal-record-warnings (or warnings-key "Pascal text") errors)
    (if errors
        (values nil errors)
        (progn
          (dolist (f forms) (eval f))
          (values (%pascal-form-names forms) nil)))))

(defun %pascal-errors-string (errors)
  (format nil "~{~A~^~%~}" (mapcar #'princ-to-string errors)))

;;; ---- files -----------------------------------------------------------------------------------------------------

(defun pascal-file-attributes (text)
  "The attribute list on the first line of TEXT, {-*- Mode: PASCAL; Dialect: ISO; Package: PASCAL-USER -*- }, as a
plist (:MODE :DIALECT :PACKAGE), strings upper-cased."
  (let* ((nl (or (%string-position-char text #\Newline 0) (length text))) (line (subseq text 0 nl))
         (s (search "-*-" line)) (e (and s (search "-*-" line :start2 (+ s 3)))))
    (when e
      (let ((out nil))
        (dolist (part (%split-string (subseq line (+ s 3) e) #\;) out)
          (let ((colon (%string-position-char part #\: 0)))
            (when colon
              (let ((key (string-upcase (string-trim " " (subseq part 0 colon)))) (val (string-upcase (string-trim " " (subseq part (+ colon 1))))))
                (cond ((string= key "MODE") (setf (getf out :mode) val))
                      ((string= key "DIALECT") (setf (getf out :dialect) val))
                      ((string= key "PACKAGE") (setf (getf out :package) val)))))))))))

(defun %pascal-dialect-of (attrs)
  (let ((d (getf attrs :dialect)))
    (cond ((null d) *pascal-default-dialect*)
          ((member d '("VS" "PASCAL/VS" "PASCALVS") :test #'string=) :vs)
          (t :iso))))

(defvar *pascal-file-packages* (make-hash-table :test 'equal)
  "The package of Pascal files whose attribute list names none (set by DEF-PROGRAM's :DEFAULT-PACKAGE).")

(defun %pascal-file-package (path attrs)
  (or (getf attrs :package) (gethash (namestring path) *pascal-file-packages*) "PASCAL-USER"))

(defun pascal-source-file-p (path)
  "True when PATH has a Pascal file type: .pas, .pascal or .p."
  (let ((n (string-downcase (namestring path))))
    (or (%ends-with n ".pas") (%ends-with n ".pascal") (%ends-with n ".p"))))

(defun pascal-load-file (path)
  "Compile the Pascal file PATH into the running world.  Signals an error listing the compiler warnings, if any.
Returns the names defined."
  (let* ((path (namestring path)) (text (%read-file-string path)) (attrs (pascal-file-attributes text)))
    (multiple-value-bind (names errors)
        (pascal-compile-string text :package (%pascal-file-package path attrs) :dialect (%pascal-dialect-of attrs)
                                    :file path :directory (directory-namestring path))
      (when errors (error "Pascal file ~A has errors:~%~A" path (%pascal-errors-string errors)))
      names)))

(defun pascal-compile-file (path &key output-file (load nil))
  "Compile the Pascal file PATH to a compiled-code file (by default PATH with type nfasl), as Compile File did; the
routines are defined when that file is loaded (immediately with LOAD).  Returns the output file's name."
  (let* ((path (namestring path)) (text (%read-file-string path)) (attrs (pascal-file-attributes text))
         (package (%pascal-file-package path attrs))
         (out (or output-file (namestring (make-pathname :type "nfasl" :defaults (pathname path)))))
         (lisp-file (%string-concat out ".lisp")))
    (multiple-value-bind (forms errors)
        (pascal-translate text :package package :dialect (%pascal-dialect-of attrs) :file path :directory (directory-namestring path))
      (%pascal-record-warnings path errors)
      (when errors (error "Pascal file ~A has errors:~%~A" path (%pascal-errors-string errors)))
      (with-open-file (o lisp-file :direction :output :if-exists :supersede :if-does-not-exist :create)
        (let ((*package* (find-package "COMMON-LISP")))
          (format o ";;; Translated from the Pascal file ~A -- do not edit.~%" path)
          (prin1 `(pascal-ensure-package ,package) o) (terpri o)
          (dolist (f forms) (prin1 f o) (terpri o))))
      (unwind-protect (compile-file lisp-file :output-file out)
        (ignore-errors (delete-file lisp-file)))
      (when load (load out))
      out)))

;;; ---- systems: DEF-PROGRAM --------------------------------------------------------------------------------------

(defun %def-program-components (body)
  "Flatten a DEF-PROGRAM body into file names in load order: (:using A rest...) puts A's files before the rest's."
  (let ((acc nil))
    (labels ((walk (x)
               (cond ((stringp x) (push x acc))
                     ((symbolp x) (push (string-downcase (symbol-name x)) acc))
                     ((and (consp x) (member (car x) '(:using :serial :parallel))) (dolist (y (cdr x)) (walk y)))
                     ((consp x) (dolist (y x) (walk y))))))
      (dolist (b body) (walk b)))
    (nreverse acc)))

(defun %def-program-file (name dir)
  "NAME with its file type: the first of .pas, .pascal, .p and .lisp that exists in DIR (else .pas)."
  (if (pathname-type name) name
      (dolist (ty '(".pas" ".pascal" ".p" ".lisp") (%string-concat name ".pas"))
        (when (probe-file (%string-concat (%expand-home dir) name ty)) (return (%string-concat name ty))))))

(defmacro def-program (name (&key default-pathname default-package full-name version description) &rest body)
  "Declare the Pascal system NAME, made of the Pascal (and Lisp) files in BODY, for LOAD-SYSTEM and COMPILE-SYSTEM.
A file is a string; (:USING spec-files using-files...) loads the files with unit specifications first.  Pascal files
whose attribute list names no package are compiled into DEFAULT-PACKAGE.
  (def-program plot (:default-pathname \"~/plot/\" :default-package \"PLOT\") (:using \"axislabels\" \"onedplot\"))"
  `(%def-program ',name ,default-pathname ,default-package ,(or description full-name) ,version ',body))

(defun %def-program (name pathname package description version body)
  (let* ((dir (cond (pathname (namestring pathname)) (*load-pathname* (directory-namestring *load-pathname*)) (t "")))
         (files (mapcar (lambda (f) (%def-program-file f dir)) (%def-program-components body))))
    (when package
      (package-declare-with-no-superiors package)
      (dolist (f files)
        (when (pascal-source-file-p f)
          (setf (gethash (namestring (merge-pathnames f (pathname (%expand-home dir)))) *pascal-file-packages*) (string-upcase (string package))))))
    (%define-system name dir description version nil files)))

;;; Pascal files in systems are compiled with the Pascal compiler
(push (list #'pascal-source-file-p #'pascal-load-file (lambda (file fasl) (pascal-compile-file file :output-file fasl)))
      *system-file-handlers*)

;;; ---- the predeclared package ------------------------------------------------------------------------------------

(package-declare-with-no-superiors "PASCAL-USER")

;;; ---- the PASCAL package: the names the Genera Tool Kit used ----------------------------------------------------
;;; (pascal:execute 'pascal-user::quadratic), pascal:*pascal-init-to-zero*, pascal:def-program, ...

(let ((p (or (find-package "PASCAL") (%make-package "PASCAL" nil nil))))
  (import '(*pascal-init-to-zero* *pascal-default-dialect* *pascal-check-undefined* *pascal-debug-info*
            def-program package-declare-with-no-superiors xqt) p)
  (setf (symbol-function (intern "EXECUTE" p)) #'pascal-execute)
  (export (list (intern "EXECUTE" p) '*pascal-init-to-zero* '*pascal-default-dialect* '*pascal-check-undefined*
                '*pascal-debug-info* 'def-program 'package-declare-with-no-superiors 'xqt)
          p))

;;; ---- Listener commands ------------------------------------------------------------------------------------------

(define-command (execute-pascal :documentation "Run a compiled Pascal main program (Init_to_zero: start variables at zero)")
    ((name word :prompt "Pascal program") (init-to-zero word :prompt "Init_to_zero (yes or no)" :default "no"))
  (pascal-execute name :init-to-zero (and init-to-zero (member (string-downcase init-to-zero) '("yes" "y" "t" "true") :test #'string=))))

(define-command (compile-pascal-file :documentation "Compile a Pascal file into the running world") ((path string :prompt "Pascal file"))
  (let ((names (handler-case (pascal-load-file path)
                 (error (e) (format t "~A~%" e) nil))))
    (when names (format t "~{~A~^, ~} compiled~%" (mapcar (lambda (n) (string-downcase (symbol-name n))) names)))))

(define-command (show-pascal-programs :documentation "List the compiled Pascal main programs") ()
  (let ((ps (pascal-programs)))
    (if ps (dolist (p ps) (format t "~A:~(~A~)~%" (package-name (symbol-package p)) (symbol-name p)))
        (format t "No Pascal programs are compiled.~%"))))

(define-command (show-pascal-warnings :documentation "List the pending Pascal compiler warnings") ()
  (let ((ws (pascal-compiler-warnings)))
    (if ws (dolist (w ws) (format t "~A: line ~D, column ~D: ~A~%" (pwarning-source w) (pwarning-line w) (pwarning-col w) (pwarning-message w)))
        (format t "No compiler warnings.~%"))))
