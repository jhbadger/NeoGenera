;;;; 40-toolkit.lisp -- the FORTRAN Tool Kit: running and compiling programs, libraries, systems, commands.
;;;;
;;;; F77:EXECUTE (also F77:XQT) runs a compiled main program with the options of the Genera Execute FORTRAN command:
;;;; :INIT-TO-ZERO, :SAVE-ENVIRONMENT, :RELOAD, :PATHNAME-DEFAULTS, :BLOCKDATA, :LIBRARIES, :UNITS, :SYSTEM,
;;;; :ADDITIONAL-EXTERNALS, :EXTRA-NUMBER-SPACE, :EXTRA-CHARACTER-SPACE and :TRAP-UNDERFLOW.  Compiling a string, a
;;;; file or an editor buffer translates the FORTRAN to Lisp and compiles that; the warnings of the last compilation
;;;; of each file are kept in a database that the editor's Compiler Warnings commands show.  F77:DEF-LIBRARY and
;;;; F77:DEF-PROGRAM declare libraries and programs made of FORTRAN (and Lisp) files for LOAD-SYSTEM and COMPILE-SYSTEM.

(in-package :cl)

;;; ---- programs ---------------------------------------------------------------------------------------------

(defvar *f77-extra-number-space* nil "The extra number space reserved by F77:EXECUTE, a reference, or NIL.")
(defvar *f77-extra-character-space* nil "The extra character space reserved by F77:EXECUTE, a reference, or NIL.")

(defstruct (f77program (:copier nil)) name package main file saved)

(defmethod print-object ((p f77program) s)
  (format s "#<FORTRAN program ~A:~A>" (f77program-package p) (string-downcase (f77program-name p))))

(defvar *f77-programs* (make-hash-table :test 'equal) "Compiled FORTRAN main programs, by \"PACKAGE:NAME\".")
(defvar *f77-blockdata* (make-hash-table :test 'equal) "Compiled BLOCK DATA units, by \"PACKAGE:NAME\": (thunk . file).")
(defvar *f77-routine-info* (make-hash-table :test 'equal) "Compiled routines, by \"PACKAGE:NAME\": (kind file).")

(defun %f77-define-program (name package main file)
  (let* ((key (%string-concat package ":" name)) (old (gethash key *f77-programs*)))
    (setf (gethash key *f77-programs*)
          (make-f77program :name name :package package :main main :file file :saved (and old (f77program-saved old))))
    (intern name package)))

(defun %f77-define-blockdata (name package thunk file)
  (setf (gethash (%string-concat package ":" name) *f77-blockdata*) (cons thunk file))
  (intern (if (string= name "") "BLOCK-DATA" name) package))

(defun %f77-note-routine (name package kind file)
  (setf (gethash (%string-concat package ":" name) *f77-routine-info*) (list kind file))
  (%fc-routine-sym name (f77-ensure-package package)))

(defun f77-programs ()
  "The compiled FORTRAN main programs, as symbols (package-qualified program names)."
  (let ((acc nil))
    (maphash (lambda (k p) (declare (ignore k)) (push (intern (f77program-name p) (f77program-package p)) acc)) *f77-programs*)
    (sort acc #'string< :key #'symbol-name)))

(defun f77-routines ()
  "The compiled FORTRAN subroutines and functions, as symbols."
  (let ((acc nil))
    (maphash (lambda (k v) (declare (ignore v)) (let ((c (%string-position-char k #\: 0))) (push (intern (subseq k (+ c 1)) (subseq k 0 c)) acc))) *f77-routine-info*)
    (sort acc #'string< :key #'symbol-name)))

(defun %f77-find-program (name)
  (let* ((str (string-upcase (if (symbolp name) (symbol-name name) (string name))))
         (colon (%string-position-char str #\: 0))
         (pkg (cond (colon (subseq str 0 colon))
                    ((and (symbolp name) (symbol-package name)) (package-name (symbol-package name)))
                    (t nil)))
         (short (if colon (string-left-trim ":" (subseq str colon)) str)))
    (or (and pkg (gethash (%string-concat pkg ":" short) *f77-programs*))
        (let ((found nil))
          (maphash (lambda (k p) (declare (ignore k)) (when (string= (f77program-name p) short) (push p found))) *f77-programs*)
          (cond ((null found) (error "~A is not a compiled FORTRAN main program" (string-downcase short)))
                ((cdr found) (error "There are FORTRAN programs named ~A in several packages (~{~A~^, ~}): give the package"
                                    (string-downcase short) (mapcar #'f77program-package found)))
                (t (car found)))))))

;;; ---- calling routines by name -----------------------------------------------------------------------------------

(defun %f7-extern (sym)
  (let* ((env (%f7-env)) (name (symbol-name sym)) (hit (assoc name (fenv-externals env) :test #'string-equal)))
    (cond (hit (let ((f (cdr hit))) (if (symbolp f) (symbol-function f) f)))
          (t (%f7-rerror "The FORTRAN routine ~A is not defined (compile it, or give it in :ADDITIONAL-EXTERNALS)" (string-downcase name))))))

(defun %f7-call (sym &rest args)
  "Call the FORTRAN routine SYM (or an additional external of that name) with reference arguments."
  (apply (if (fboundp sym) (symbol-function sym) (%f7-extern sym)) args))

;;; ---- running ------------------------------------------------------------------------------------------------------------

(defun %f77-output-stream (output)
  (cond ((null output) nil)
        ((streamp output) output)
        (t (open (namestring output) :direction :output :if-exists :supersede :if-does-not-exist :create))))

(defun %f77-input-source (input)
  (cond ((null input) nil)
        ((or (streamp input) (functionp input)) input)
        (t (open (namestring input)))))

(defun %f77-ensure-library (name)
  (let ((sys (find-system name nil)))
    (when sys (load-system name :verbose nil))))

(defun f77-execute (name &key input output init-to-zero save-environment reload pathname-defaults streams additional-externals
                           blockdata libraries units system extra-number-space extra-character-space (trap-underflow t))
  "Run the compiled FORTRAN main program NAME (a symbol or string, optionally package-qualified).  INPUT and OUTPUT give the
standard units 5 and 6: a file name, a pathname or a stream (by default the Listener).  INIT-TO-ZERO starts all storage at zero
instead of undefined.  SAVE-ENVIRONMENT keeps the program's storage from one run to the next (until RELOAD).  PATHNAME-DEFAULTS
is the directory for file names in OPEN.  STREAMS is an alist (unit-or-file-name . lisp-stream).  LIBRARIES and SYSTEM name systems
loaded first; UNITS names routines that must be defined; BLOCKDATA names the BLOCK DATA units to run (by default all in the
program's package); ADDITIONAL-EXTERNALS is an alist (fortran-name . function) of Lisp functions FORTRAN may CALL.
EXTRA-NUMBER-SPACE and EXTRA-CHARACTER-SPACE reserve that many more entries, available to Lisp as *F77-EXTRA-NUMBER-SPACE* and
*F77-EXTRA-CHARACTER-SPACE* (references to pass to FORTRAN).  TRAP-UNDERFLOW is accepted and ignored.  Returns NIL."
  (declare (ignore trap-underflow))
  (when system (load-system system :verbose nil))
  (dolist (l libraries) (%f77-ensure-library l))
  (let* ((prog (%f77-find-program name))
         (out (%f77-output-stream output)) (in (%f77-input-source input))
         (env (if (and save-environment (f77program-saved prog) (not reload))
                  (f77program-saved prog)
                  (%make-fenv :init-zero init-to-zero)))
         (extra-n (and extra-number-space (cons (make-array extra-number-space :initial-element (if init-to-zero 0 'f77-undefined)) 0)))
         (extra-c (and extra-character-space (cons (make-array extra-character-space :initial-element (if init-to-zero " " 'f77-undefined)) 0))))
    (dolist (u units)
      (let ((s (intern (string-upcase (string u)) (f77program-package prog))))
        (unless (or (fboundp s) (assoc (string u) additional-externals :test #'string-equal))
          (error "The FORTRAN unit ~A is not defined" (string-downcase (string u))))))
    (when reload (setf (f77program-saved prog) nil))
    (setf (fenv-input env) in (fenv-output env) out (fenv-streams env) streams (fenv-externals env) additional-externals
          (fenv-program env) prog (fenv-extra-number env) extra-n (fenv-extra-character env) extra-c)
    (let ((*f77-env* env) (*f77-program* prog) (*f77-line* 0) (*f77-routine* nil) (*f77-running* t)
          (*f77-pathname-defaults* (if pathname-defaults (namestring pathname-defaults) (or *f77-pathname-defaults* "")))
          (*f77-extra-number-space* extra-n) (*f77-extra-character-space* extra-c))
      (unwind-protect
           (catch '%f77-halt
             (handler-bind ((error #'%f7-wrap-error))
               (%f77-run-blockdata prog blockdata)
               (funcall (f77program-main prog))))
        (ignore-errors (when (and out (output-stream-p out)) (finish-output out)))
        (ignore-errors (%f7-close-all))
        (when (and output (not (streamp output))) (ignore-errors (close out)))
        (when (and input (not (streamp input)) (not (functionp input))) (ignore-errors (close in)))
        (setf (f77program-saved prog) (and save-environment env))))
    nil))

(defun %f77-run-blockdata (prog names)
  (let ((pkg (f77program-package prog)))
    (if names
        (dolist (n names)
          (let ((bd (gethash (%string-concat pkg ":" (string-upcase (string n))) *f77-blockdata*)))
            (unless bd (error "There is no BLOCK DATA unit named ~A" (string-downcase (string n))))
            (funcall (car bd))))
        (maphash (lambda (k bd)
                   (let ((c (%string-position-char k #\: 0)))
                     (when (string= (subseq k 0 c) pkg) (funcall (car bd)))))
                 *f77-blockdata*))))

(defun f77-xqt (name &rest options)
  "A synonym for F77-EXECUTE."
  (apply #'f77-execute name options))

;;; ---- compiling ---------------------------------------------------------------------------------------------------------

(defstruct (fwarning (:copier nil)) source start end line col message)

(defvar *f77-compiler-warnings* nil
  "The compiler warnings database: (source . list-of-FWARNING) for each file or buffer compiled with errors.")

(defun %f77-record-warnings (source errors)
  (setq *f77-compiler-warnings* (remove source *f77-compiler-warnings* :key #'car :test #'equal))
  (when errors
    (setq *f77-compiler-warnings*
          (append *f77-compiler-warnings*
                  (list (cons source (mapcar (lambda (c) (make-fwarning :source source :start (f77-error-start c) :end (f77-error-end c)
                                                                       :line (f77-error-line c) :col (f77-error-col c)
                                                                       :message (f77-error-message c)))
                                             errors)))))))

(defun f77-compiler-warnings (&optional source)
  "The pending compiler warnings (all of them, or those of SOURCE), as FWARNINGs."
  (if source (cdr (assoc source *f77-compiler-warnings* :test #'equal)) (apply #'append (mapcar #'cdr *f77-compiler-warnings*))))

(defun %f77-form-names (forms)
  "The names of what FORMS define: programs, routines and BLOCK DATA units."
  (let ((acc nil))
    (dolist (f forms)
      (when (consp f)
        (case (car f)
          (defun (push (cadr f) acc))
          ((%f77-define-program %f77-define-blockdata) (push (intern (if (string= (cadr f) "") "BLOCK-DATA" (cadr f)) (caddr f)) acc)))))
    (nreverse acc)))

(defun f77-compile-string (source &key (package *f77-default-package*) file (directory "") only-at (warnings-key file))
  "Compile the fixed-format FORTRAN SOURCE into the running world: (values names errors).  NAMES are the programs, routines
and BLOCK DATA units defined; ERRORS the syntax and semantic errors (then nothing is defined).  ONLY-AT compiles just the
program unit at that offset."
  (multiple-value-bind (forms errors) (f77-translate source :package package :file file :directory directory :only-at only-at)
    (%f77-record-warnings (or warnings-key "FORTRAN text") errors)
    (if errors
        (values nil errors)
        (progn
          (dolist (f forms) (eval f))
          (values (%f77-form-names forms) nil)))))

(defun %f77-errors-string (errors)
  (format nil "~{~A~^~%~}" (mapcar #'princ-to-string errors)))

(defun f77-run-string (source &rest options &key (package *f77-default-package*) &allow-other-keys)
  "Compile the FORTRAN SOURCE and run its (first) main program with OPTIONS.  Returns the program's name."
  (multiple-value-bind (names errors) (f77-compile-string source :package package)
    (when errors (error "The FORTRAN text has errors:~%~A" (%f77-errors-string errors)))
    (let ((prog (find-if (lambda (n) (gethash (%string-concat (package-name (symbol-package n)) ":" (symbol-name n)) *f77-programs*)) names)))
      (unless prog (error "The FORTRAN text has no main program"))
      (let ((opts nil))
        (do ((l options (cddr l))) ((null l))
          (unless (eq (car l) :package) (setq opts (list* (car l) (cadr l) opts))))
        (apply #'f77-execute prog opts))
      prog)))

;;; ---- files ---------------------------------------------------------------------------------------------------------------

(defun f77-file-attributes (text)
  "The attribute list on the first line of TEXT, C -*- Mode: FORTRAN; Package: FTN-USER -*-, as a plist (:MODE :PACKAGE),
strings upper-cased."
  (let* ((nl (or (%string-position-char text #\Newline 0) (length text))) (line (subseq text 0 nl))
         (s (search "-*-" line)) (e (and s (search "-*-" line :start2 (+ s 3)))))
    (when e
      (let ((out nil))
        (dolist (part (%split-string (subseq line (+ s 3) e) #\;) out)
          (let ((colon (%string-position-char part #\: 0)))
            (when colon
              (let ((key (string-upcase (string-trim " " (subseq part 0 colon)))) (val (string-upcase (string-trim " " (subseq part (+ colon 1))))))
                (cond ((string= key "MODE") (setf (getf out :mode) val))
                      ((string= key "PACKAGE") (setf (getf out :package) val)))))))))))

(defvar *f77-file-packages* (make-hash-table :test 'equal)
  "The package of FORTRAN files whose attribute list names none (set by F77:DEF-PROGRAM's :DEFAULT-PACKAGE).")

(defun %f77-file-package (path attrs)
  (or (getf attrs :package) (gethash (namestring path) *f77-file-packages*) *f77-default-package*))

(defun f77-source-file-p (path)
  "True when PATH has a FORTRAN file type: .f, .for, .ftn, .f77 or .fortran."
  (let ((n (string-downcase (namestring path))))
    (or (%ends-with n ".f") (%ends-with n ".for") (%ends-with n ".ftn") (%ends-with n ".f77") (%ends-with n ".fortran"))))

(defun f77-load-file (path)
  "Compile the FORTRAN file PATH into the running world.  Signals an error listing the compiler warnings, if any.
Returns the names defined."
  (let* ((path (namestring path)) (text (%read-file-string path)) (attrs (f77-file-attributes text)))
    (multiple-value-bind (names errors)
        (f77-compile-string text :package (%f77-file-package path attrs) :file path :directory (directory-namestring path))
      (when errors (error "FORTRAN file ~A has errors:~%~A" path (%f77-errors-string errors)))
      names)))

(defun f77-compile-file (path &key output-file (load nil))
  "Compile the FORTRAN file PATH to a compiled-code file (by default PATH with type nfasl), as Compile File did; the
routines are defined when that file is loaded (immediately with LOAD).  Returns the output file's name."
  (let* ((path (namestring path)) (text (%read-file-string path)) (attrs (f77-file-attributes text))
         (package (%f77-file-package path attrs))
         (out (or output-file (namestring (make-pathname :type "nfasl" :defaults (pathname path)))))
         (lisp-file (%string-concat out ".lisp")))
    (multiple-value-bind (forms errors) (f77-translate text :package package :file path :directory (directory-namestring path))
      (%f77-record-warnings path errors)
      (when errors (error "FORTRAN file ~A has errors:~%~A" path (%f77-errors-string errors)))
      (with-open-file (o lisp-file :direction :output :if-exists :supersede :if-does-not-exist :create)
        (let ((*package* (find-package "COMMON-LISP")))
          (format o ";;; Translated from the FORTRAN file ~A -- do not edit.~%" path)
          (prin1 `(f77-ensure-package ,package) o) (terpri o)
          (dolist (f forms) (prin1 f o) (terpri o))))
      (unwind-protect (compile-file lisp-file :output-file out)
        (ignore-errors (delete-file lisp-file)))
      (when load (load out))
      out)))

;;; ---- systems: DEF-LIBRARY and DEF-PROGRAM ------------------------------------------------------------------------------------

(defvar *f77-libraries* (make-hash-table :test 'equal) "Libraries declared with F77:DEF-LIBRARY: name -> list of files.")
(defvar *f77-program-definitions* (make-hash-table :test 'equal) "Programs declared with F77:DEF-PROGRAM: name -> plist.")

(defun %f77-def-components (body)
  (let ((acc nil))
    (labels ((walk (x)
               (cond ((stringp x) (push x acc))
                     ((symbolp x) (push (string-downcase (symbol-name x)) acc))
                     ((consp x) (dolist (y (if (member (car x) '(:serial :parallel)) (cdr x) x)) (walk y))))))
      (dolist (b body) (walk b)))
    (nreverse acc)))

(defun %f77-def-file (name dir)
  "NAME with its file type: the first of .f, .for, .ftn, .f77, .fortran and .lisp that exists in DIR (else .f)."
  (if (pathname-type name) name
      (dolist (ty '(".f" ".for" ".ftn" ".f77" ".fortran" ".lisp") (%string-concat name ".f"))
        (when (probe-file (%string-concat (%expand-home dir) name ty)) (return (%string-concat name ty))))))

(defun %f77-def-system (name pathname package description version depends body)
  (let* ((dir (cond (pathname (namestring pathname)) (*load-pathname* (directory-namestring *load-pathname*)) (t "")))
         (files (mapcar (lambda (f) (%f77-def-file f dir)) (%f77-def-components body))))
    (when package
      (package-declare-with-no-superiors package)
      (dolist (f files)
        (when (f77-source-file-p f)
          (setf (gethash (namestring (merge-pathnames f (pathname (%expand-home dir)))) *f77-file-packages*) (string-upcase (string package))))))
    (%define-system name dir description version depends files)
    files))

(defmacro f77-def-library (name (&key default-pathname default-package full-name description version) &rest body)
  "Declare the FORTRAN library NAME, made of the FORTRAN (and Lisp) files in BODY, for LOAD-SYSTEM, COMPILE-SYSTEM and the
:LIBRARIES option of F77:EXECUTE.
  (f77:def-library numerics (:default-pathname \"~/num/\") \"matmul\" \"solve\")"
  `(let ((files (%f77-def-system ',name ,default-pathname ,default-package ,(or description full-name) ,version nil ',body)))
     (setf (gethash ,(string-upcase (string name)) *f77-libraries*) files)
     ',name))

(defmacro f77-def-program (name (&key default-pathname default-package full-name description version libraries blockdata main) &rest body)
  "Declare the FORTRAN program NAME: the files in BODY (and the LIBRARIES it needs) form its call tree; MAIN names the main
program (by default NAME) and BLOCKDATA the BLOCK DATA units it runs.  LOAD-SYSTEM and COMPILE-SYSTEM load and compile it.
  (f77:def-program solver (:default-pathname \"~/solver/\" :libraries (numerics)) \"main\" \"io\")"
  `(let ((files (%f77-def-system ',name ,default-pathname ,default-package ,(or description full-name) ,version ',libraries ',body)))
     (setf (gethash ,(string-upcase (string name)) *f77-program-definitions*)
           (list :files files :libraries ',libraries :blockdata ',blockdata :main ',(or main name)))
     ',name))

;;; FORTRAN files in systems are compiled with the FORTRAN compiler
(push (list #'f77-source-file-p #'f77-load-file (lambda (file fasl) (f77-compile-file file :output-file fasl)))
      *system-file-handlers*)

;;; ---- Lisp access to FORTRAN storage -------------------------------------------------------------------------------------------

(defun f77-make-number-data (size &optional (initial 0))
  "A reference to SIZE fresh entries of the number space, for passing to a FORTRAN routine as an array argument."
  (cons (make-array size :initial-element initial) 0))

(defun f77-make-character-data (count &key (length 1) (initial ""))
  "A reference to COUNT character entries of LENGTH characters each, for passing to a FORTRAN routine as a CHARACTER*LENGTH array."
  (let ((v (make-array (* count length) :initial-element 'f77-undefined)))
    (dotimes (i count) (setf (svref v (* i length)) (%f7-cfit initial length)))
    (cons v 0)))

(defmacro f77-with-number-data ((var size &key (initial 0)) &body body)
  "Run BODY with VAR bound to a reference to SIZE entries of FORTRAN number space, addressed by FORTRAN as an array by integer offset."
  `(let ((,var (f77-make-number-data ,size ,initial))) ,@body))

(defmacro f77-with-character-data ((var count &key (length 1) (initial "")) &body body)
  "Run BODY with VAR bound to a reference to COUNT FORTRAN CHARACTER*LENGTH entries."
  `(let ((,var (f77-make-character-data ,count :length ,length :initial ,initial))) ,@body))

(defun f77-ref-element (ref i)
  "Entry I (zero-based) of the FORTRAN storage REF."
  (svref (car ref) (+ (cdr ref) i)))
(defun f77-set-ref-element (ref i value)
  (setf (svref (car ref) (+ (cdr ref) i)) value))

;;; ---- the predeclared packages --------------------------------------------------------------------------------------------------

(package-declare-with-no-superiors "FTN-USER")

(let ((p (or (find-package "F77") (%make-package "F77" nil nil))))
  (import '(*f77-init-to-zero* *f77-debug-info* *f77-check-undefined* package-declare-with-no-superiors) p)
  (flet ((fn (name f) (setf (symbol-function (intern name p)) f) (export (intern name p) p))
         (mac (name target)
           (let ((s (intern name p)))
             (eval `(defmacro ,s (&rest args) (cons ',target args)))
             (export s p)))
         (var (name value)
           (let ((s (intern name p)))
             (eval `(defvar ,s ,value))
             (export s p))))
    (fn "EXECUTE" #'f77-execute)
    (fn "XQT" #'f77-xqt)
    (fn "COMPILE-STRING" #'f77-compile-string)
    (fn "LOAD-FILE" #'f77-load-file)
    (fn "COMPILE-FILE" #'f77-compile-file)
    (fn "TRANSLATE" #'f77-translate)
    (fn "PARSE-FORMAT" #'f77-parse-format)
    (fn "MAKE-REF" #'f77-make-ref)
    (fn "REF-VALUE" #'f77-ref-value)
    (fn "MAKE-NUMBER-DATA" #'f77-make-number-data)
    (fn "MAKE-CHARACTER-DATA" #'f77-make-character-data)
    (mac "DEF-LIBRARY" 'f77-def-library)
    (mac "DEF-PROGRAM" 'f77-def-program)
    (mac "WITH-FORTRAN-NUMBER-DATA" 'f77-with-number-data)
    (mac "WITH-FORTRAN-CHARACTER-DATA" 'f77-with-character-data)
    (var "WRAPAROUND-COLUMN" 40)
    (var "CONTINUATION-CHARACTER" "$")
    (export (list '*f77-init-to-zero* '*f77-debug-info* '*f77-check-undefined* 'package-declare-with-no-superiors) p)))

;;; ---- Listener commands ------------------------------------------------------------------------------------------------------------

(defun %f77-yes-p (s) (and s (member (string-downcase s) '("yes" "y" "t" "true" "on") :test #'string=) t))

(defparameter *f77-execute-options*
  '(("init to zero" :init-to-zero :boolean) ("reload" :reload :boolean) ("save environment" :save-environment :boolean)
    ("trap underflow" :trap-underflow :boolean) ("pathname default" :pathname-defaults :string) ("libraries" :libraries :names)
    ("system" :system :name) ("units" :units :names) ("blockdata" :blockdata :names)
    ("extra character space" :extra-character-space :integer) ("extra number space" :extra-number-space :integer))
  "The options of the Execute FORTRAN command: (name keyword type).")

(defun f77-parse-execute-options (text)
  "The F77-EXECUTE keyword arguments for the Execute FORTRAN option text TEXT, a comma-separated list such as
\"Init to Zero yes, Libraries (numerics), Pathname Default /tmp/\".  An option name may be abbreviated by its
leading words; a Boolean option with no value means yes."
  (let ((plist nil))
    (dolist (part (%split-string text #\,))
      (let ((toks (%split-tokens part)))
        (when toks
          (let ((opt nil) (used 0))
            (dolist (o *f77-execute-options*)
              (let* ((words (%split-string (car o) #\Space)) (n (min (length words) (length toks))))
                (when (and (>= n (length words)) (every #'%word-prefix-p (mapcar #'string-downcase (subseq toks 0 n)) words)
                           (> n used))
                  (setq opt o used n))))
            (unless opt
              ;; allow the option name itself to be abbreviated: every token a prefix of the option's words
              (dolist (o *f77-execute-options*)
                (let ((words (%split-string (car o) #\Space)))
                  (when (and (not opt) (<= (length toks) (length words)) (%word-prefix-p (string-downcase (car toks)) (car words)))
                    (let ((k (min (length toks) (length words))))
                      (when (every #'%word-prefix-p (mapcar #'string-downcase (subseq toks 0 k)) (subseq words 0 k))
                        (setq opt o used k)))))))
            (unless opt (error "Unknown Execute FORTRAN option ~A" (string-trim " " part)))
            (let ((value-toks (subseq toks used)))
              (setf (getf plist (cadr opt))
                    (ecase (caddr opt)
                      (:boolean (if value-toks (%f77-yes-p (car value-toks)) t))
                      (:string (%join-strings value-toks " "))
                      (:name (let ((v (string-trim "()" (%join-strings value-toks " ")))) (intern (string-upcase v) "KEYWORD")))
                      (:names (mapcar (lambda (n) (intern (string-upcase n) "KEYWORD"))
                                      (%split-tokens (substitute #\Space #\) (substitute #\Space #\( (%join-strings value-toks " "))))))
                      (:integer (parse-integer (car value-toks))))))))))
    plist))

(define-command (execute-fortran :documentation "Run a compiled FORTRAN main program; options: Init to Zero, Reload, Save Environment, Trap Underflow, Pathname Default, Libraries, System, Units, Blockdata, Extra Character Space, Extra Number Space")
    ((name word :prompt "FORTRAN program") (options string :prompt "Options" :default ""))
  (apply #'f77-execute name (f77-parse-execute-options options)))

(defun %f77-compile-file-command (path)
  (let ((names (handler-case (f77-load-file path)
                 (error (e) (format t "~A~%" e) nil))))
    (when names (format t "~{~A~^, ~} compiled~%" (mapcar (lambda (n) (string-downcase (symbol-name n))) names)))))

(define-command (compile-fortran-file :documentation "Compile a FORTRAN file into the running world") ((path string :prompt "FORTRAN file"))
  (%f77-compile-file-command path))

(define-command (compile-fortran :documentation "Compile a FORTRAN file into the running world") ((path string :prompt "FORTRAN file"))
  (%f77-compile-file-command path))

(define-command (show-fortran-programs :documentation "List the compiled FORTRAN main programs") ()
  (let ((ps (f77-programs)))
    (if ps (dolist (p ps) (format t "~A:~(~A~)~%" (package-name (symbol-package p)) (symbol-name p)))
        (format t "No FORTRAN programs are compiled.~%"))))

(define-command (show-fortran-warnings :documentation "List the pending FORTRAN compiler warnings") ()
  (let ((ws (f77-compiler-warnings)))
    (if ws (dolist (w ws) (format t "~A: line ~D, column ~D: ~A~%" (fwarning-source w) (fwarning-line w) (fwarning-col w) (fwarning-message w)))
        (format t "No compiler warnings.~%"))))
