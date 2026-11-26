;;;; 20-systems.lisp -- DEFSYSTEM, patches and SAVE-WORLD.
;;;;
;;;; A system names the files of a program and the systems it needs.  LOAD-SYSTEM loads them in order (compiling
;;;; the changed ones with :COMPILE), and loads again only the files changed since they were loaded.  A system's
;;;; patches are numbered files in its patches/ directory, NAME-1.lisp, NAME-2.lisp, ...; LOAD-PATCHES loads the
;;;; ones not loaded yet, so a running world can be brought up to date without reloading everything, as Genera
;;;; did.  SAVE-WORLD writes the running world to an image that BIN/NEOGENERA -i can start later.

(in-package :cl)

(defstruct (system (:constructor %make-system) (:copier nil))
  name pathname description version depends-on components (loaded nil) (patch-level 0) (patches nil))

(defvar *systems* (make-hash-table :test 'equal) "Defined systems, by upper-case name.")
(defvar *system-search-path* '("" "~/.neogenera/systems/")
  "Directories where FIND-SYSTEM looks for NAME.system files that define systems it does not know yet.")

(defun %system-key (name) (string-upcase (if (symbolp name) (symbol-name name) (string name))))

(defun %expand-home (dir)
  (if (and (> (length dir) 1) (string= (subseq dir 0 2) "~/")) (%string-concat (or (getenv "HOME") "") (subseq dir 1)) dir))

(defmacro defsystem (name (&key pathname description version depends-on) &rest components)
  "Define the system NAME: its COMPONENTS are file names (without .lisp) relative to PATHNAME (by default the
directory of the file being loaded), loaded in order after the systems in DEPENDS-ON.
  (defsystem my-app (:version \"1.0\" :depends-on (my-utils)) \"package\" \"model\" \"ui\")"
  `(%define-system ',name ,pathname ,description ,version ',depends-on ',components))

(defun %define-system (name pathname description version depends-on components)
  (let* ((dir (cond (pathname (%expand-home (namestring pathname)))
                    (*load-pathname* (directory-namestring *load-pathname*))
                    (t "")))
         (old (gethash (%system-key name) *systems*))
         (sys (%make-system :name (%system-key name) :pathname (pathname dir) :description description :version version
                            :depends-on depends-on :components components)))
    (when old (setf (system-loaded sys) (system-loaded old) (system-patch-level sys) (system-patch-level old)
                    (system-patches sys) (system-patches old)))
    (setf (gethash (%system-key name) *systems*) sys)
    (system-name sys)))

(defun find-system (name &optional (errorp t))
  "The system NAME, loading its definition from NAME.system on *SYSTEM-SEARCH-PATH* if it is not known yet."
  (or (gethash (%system-key name) *systems*)
      (dolist (dir *system-search-path* nil)
        (let ((file (%string-concat (%expand-home dir) (string-downcase (%system-key name)) ".system")))
          (when (probe-file file)
            (load file)
            (return (gethash (%system-key name) *systems*)))))
      (and errorp (error "There is no system named ~A (define it with DEFSYSTEM or put ~A.system on *SYSTEM-SEARCH-PATH*)"
                         (%system-key name) (string-downcase (%system-key name))))))

(defun system-files (name)
  "The source files of the system NAME, as pathnames, in load order."
  (let ((sys (find-system name)))
    (mapcar (lambda (c) (merge-pathnames (if (pathname-type c) c (%string-concat (string c) ".lisp")) (system-pathname sys)))
            (system-components sys))))

(defun %fasl-for (source) (make-pathname :type "nfasl" :defaults source))

(defun %loaded-date (sys file) (cdr (assoc (namestring file) (system-loaded sys) :test #'string=)))

(defun load-system (name &key compile reload (verbose t) (patches t))
  "Load the system NAME and the systems it depends on: every file the first time, then only the files changed since
they were loaded (all of them with RELOAD).  With COMPILE, changed files are compiled and the compiled files loaded.
Then its new patches are loaded (unless PATCHES is false).  Returns the system's name."
  (let ((sys (find-system name)) (visited nil))
    (labels ((run (s)
               (unless (member (system-name s) visited :test #'string=)
                 (push (system-name s) visited)
                 (dolist (d (system-depends-on s)) (run (find-system d)))
                 (dolist (file (system-files (system-name s)))
                   (unless (probe-file file) (error "System ~A: no file ~A" (system-name s) (namestring file)))
                   (let ((date (file-write-date file)))
                     (when (or reload (not (eql (%loaded-date s file) date)))
                       (let ((what file))
                         (when compile
                           (let ((fasl (%fasl-for file)))
                             (when (or reload (not (probe-file fasl)) (< (or (file-write-date fasl) 0) date))
                               (when verbose (format t "~&; Compiling ~A~%" (namestring file)))
                               (compile-file (namestring file) :output-file (namestring fasl)))
                             (setq what fasl)))
                         (when verbose (format t "~&; Loading ~A~%" (namestring what)))
                         (load (namestring what)))
                       (setf (system-loaded s) (cons (cons (namestring file) date)
                                                     (remove (namestring file) (system-loaded s) :key #'car :test #'string=))))))
                 (when patches (load-patches (system-name s) :verbose verbose)))))
      (run sys))
    (when verbose (format t "~&; ~A is loaded~%" (system-version-string name)))
    (system-name sys)))

(defun compile-system (name &key reload (verbose t))
  "Compile the changed files of the system NAME (all with RELOAD) and load them."
  (load-system name :compile t :reload reload :verbose verbose))

;;; ---- patches ---------------------------------------------------------------------------------------------

(defun %patch-directory (sys) (merge-pathnames "patches/" (system-pathname sys)))

(defun %patch-number (sys file)
  "The number of the patch FILE (NAME-N.lisp) of SYS, or NIL."
  (let* ((stem (pathname-name file)) (prefix (%string-concat (string-downcase (system-name sys)) "-")))
    (and stem (> (length stem) (length prefix)) (string-equal prefix (subseq stem 0 (length prefix)))
         (parse-integer (subseq stem (length prefix)) :junk-allowed t))))

(defun system-patch-files (name)
  "The patch files of the system NAME, as (number . pathname), in order."
  (let ((sys (find-system name)))
    (sort (remove nil (mapcar (lambda (f) (let ((n (%patch-number sys f))) (and n (cons n f))))
                              (directory (merge-pathnames "*.lisp" (%patch-directory sys)))))
          #'< :key #'car)))

(defun load-patches (name &key (verbose t))
  "Load the patches of the system NAME that have not been loaded, in order.  Returns how many were loaded."
  (let ((sys (find-system name)) (n 0))
    (dolist (p (system-patch-files name) n)
      (when (> (car p) (system-patch-level sys))
        (when verbose (format t "~&; Loading patch ~A.~D: ~A~%" (system-name sys) (car p) (%patch-description (cdr p))))
        (load (namestring (cdr p)))
        (setf (system-patch-level sys) (car p)
              (system-patches sys) (append (system-patches sys) (list (list (car p) (%patch-description (cdr p)) (get-universal-time)))))
        (incf n)))))

(defun %patch-description (file)
  "The description on the first line of a patch file (after ;;; Patch N:), or the file name."
  (or (ignore-errors
        (with-open-file (s file)
          (let* ((line (read-line s nil "")) (colon (position #\: line)))
            (and colon (string-trim " " (subseq line (+ colon 1)))))))
      (file-namestring file)))

(defun make-patch (name description)
  "Create the next patch file of the system NAME, with DESCRIPTION on its first line, and return its pathname.
Write the changed definitions into it; LOAD-PATCHES loads it here and in every other world that loads the system."
  (let* ((sys (find-system name))
         (n (+ 1 (reduce #'max (mapcar #'car (system-patch-files name)) :initial-value (system-patch-level sys))))
         (file (merge-pathnames (format nil "~(~A~)-~D.lisp" (system-name sys) n) (%patch-directory sys))))
    (ensure-directories-exist file)
    (with-open-file (s file :direction :output)
      (format s ";;; Patch ~D: ~A~%;;; Made ~A for ~A~%~%(in-package :cl-user)~%~%" n description (%date-string (get-universal-time))
              (system-version-string name)))
    file))

(defun %date-string (ut)
  (multiple-value-bind (s mi h d mo y) (decode-universal-time ut)
    (format nil "~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D" y mo d h mi s)))

(defun system-version-string (name)
  "NAME, its version and its patch level, as in \"MY-APP 1.2.3\"."
  (let ((sys (find-system name)))
    (format nil "~A~@[ ~A~]~:[~;.~D~]" (system-name sys) (system-version sys) (> (system-patch-level sys) 0) (system-patch-level sys))))

(defun describe-system (name &optional (stream *standard-output*))
  "Print what the system NAME is: its files and whether they are loaded, its patches."
  (let ((sys (find-system name)))
    (format stream "~&~A~@[ -- ~A~]~%  directory: ~A~@[~%  needs: ~{~A~^, ~}~]~%"
            (system-version-string name) (system-description sys) (namestring (system-pathname sys))
            (mapcar #'%system-key (system-depends-on sys)))
    (dolist (f (system-files name))
      (let ((loaded (%loaded-date sys f)) (now (and (probe-file f) (file-write-date f))))
        (format stream "  ~A~30T~A~%" (file-namestring f)
                (cond ((null now) "missing") ((null loaded) "not loaded") ((eql loaded now) "loaded") (t "changed since loaded")))))
    (dolist (p (system-patches sys)) (format stream "  patch ~D: ~A~%" (car p) (cadr p)))
    (let ((pending (count-if (lambda (p) (> (car p) (system-patch-level sys))) (system-patch-files name))))
      (when (> pending 0) (format stream "  ~D patch~:P not loaded (LOAD-PATCHES)~%" pending)))
    (values)))

(defun list-systems () "The names of the defined systems." (sort (let ((acc nil)) (maphash (lambda (k v) (declare (ignore v)) (push k acc)) *systems*) acc) #'string<))

;;; ---- saving the world ------------------------------------------------------------------------------------

(defvar *world-saved* nil "When this world was saved: (pathname universal-time description), or NIL.")

(defun save-world (path &key description)
  "Save the running world (every definition and value) as an image file, to be started with bin/neogenera -i PATH.
Background processes cannot be saved, so none may be running; open files and the terminal are not saved."
  (let ((others (remove (current-process) (remove-if-not #'process-active-p (all-processes)))))
    (when others (error "Background processes are running (~{~A~^, ~}): stop them first" (mapcar #'process-name others))))
  (let ((file (%native-namestring path)))
    (setq *world-saved* (list (pathname file) (get-universal-time) description))
    (%request-save file)
    (%save-safe-point)                                    ; the image is written at this call's safe point
    (let ((rc (%save-result)))
      (unless (eql rc 0) (setq *world-saved* nil) (error "Could not save the world to ~A (error ~A)" file rc))
      (values (pathname file) (file-length-of file)))))

(defun %save-safe-point () nil)

(defun file-length-of (file)
  "The size of FILE in bytes, or NIL."
  (let ((attrs (ignore-errors (%file-attrs (%native-namestring file))))) (and attrs (cadr attrs))))

;;; ---- commands ----------------------------------------------------------------------------------------------

(define-command (load-system :documentation "Load a system (its changed files) and its new patches") ((name word :prompt "System"))
  (load-system name))
(define-command (compile-system :documentation "Compile and load the changed files of a system") ((name word :prompt "System"))
  (compile-system name))
(define-command (load-patches :documentation "Load the new patches of a system") ((name word :prompt "System"))
  (format t "~&~D patch~:P loaded~%" (load-patches name)))
(define-command (describe-system :documentation "Describe a system: its files and patches") ((name word :prompt "System"))
  (describe-system name))
(define-command (show-systems :documentation "List the defined systems") ()
  (let ((names (list-systems)))
    (if names (dolist (n names) (format t "~&  ~A~%" (system-version-string n))) (format t "~&No systems are defined~%"))))
(define-command (make-patch :documentation "Start the next patch of a system and edit it") ((name word :prompt "System") (description string :prompt "Description"))
  (let ((file (make-patch name description)))
    (format t "~&Patch file ~A~%" (namestring file))
    (when (and (boundp '*wm*) *wm* (fboundp 'open-editor)) (funcall 'open-editor *wm* :file (namestring file)))))
(define-command (save-world :documentation "Save the running world as an image file") ((path string :prompt "Image file"))
  (multiple-value-bind (file size) (save-world path)
    (format t "~&Saved ~A (~:D bytes).  Start it with: bin/neogenera -i ~A~%" (namestring file) (or size 0) (namestring file))))
