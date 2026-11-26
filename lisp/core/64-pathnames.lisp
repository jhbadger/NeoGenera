;;;; 64-pathnames.lisp -- pathnames and logical pathnames.
;;;;
;;;; A PATHNAME has the six components of Common Lisp: host, device (always NIL here), directory
;;;; ((:absolute|:relative . components), components being strings, :WILD (*), :WILD-INFERIORS (**) or :UP
;;;; (..)), name, type and version.  Native namestrings are Unix paths: "/usr/lib/foo.tar.gz" has the
;;;; directory (:absolute "usr" "lib"), the name "foo.tar" and the type "gz"; a name starting with a dot and
;;;; having no other dot (".profile") has no type.  A LOGICAL-PATHNAME is written HOST:DIR;DIR;NAME.TYPE.VERSION
;;;; for a host given translations with (SETF LOGICAL-PATHNAME-TRANSLATIONS), and TRANSLATE-LOGICAL-PATHNAME
;;;; turns it into a physical one.  The file functions accept pathnames, logical pathnames and namestrings.

(in-package :cl)

(defstruct (pathname (:constructor %make-pathname (host device directory name type version)) (:conc-name %pn-)
                     (:copier nil) (:predicate pathnamep))
  host device directory name type version)

(defstruct (logical-pathname (:include pathname) (:constructor %make-logical-pathname (host device directory name type version))
                             (:copier nil) (:predicate logical-pathname-p)))

(defvar *default-pathname-defaults* nil "Defaults for MERGE-PATHNAMES (an empty pathname: the current directory).")
(defvar *logical-hosts* (make-hash-table :test 'equal) "Logical host name (upper case) -> its translations.")

(defmethod print-object ((p pathname) stream)
  (if *print-escape*
      (progn (write-string "#P" stream) (prin1 (namestring p) stream))
      (write-string (namestring p) stream)))

;;; ---- parsing ----------------------------------------------------------------------------------------------

(defun %wild-component (s)
  (cond ((string= s "*") :wild) ((string= s "**") :wild-inferiors) ((string= s "..") :up) (t s)))

(defun %split-on (s ch)
  (let ((parts nil) (start 0))
    (loop (let ((p (position ch s :start start)))
            (setq parts (cons (subseq s start (or p (length s))) parts))
            (if p (setq start (+ p 1)) (return))))
    (nreverse parts)))

(defun %parse-native (s)
  (let* ((slash (position #\/ s :from-end t))
         (dir-part (if slash (subseq s 0 (+ slash 1)) ""))
         (file (if slash (subseq s (+ slash 1)) s))
         (directory (cond ((string= dir-part "") nil)
                          (t (cons (if (char= (char dir-part 0) #\/) :absolute :relative)
                                   (mapcar #'%wild-component
                                           (remove-if (lambda (c) (or (string= c "") (string= c "."))) (%split-on dir-part #\/)))))))
         (name nil) (type nil))
    (cond ((string= file "") nil)
          ((or (string= file ".") (string= file ".."))
           (setq directory (append (or directory (list :relative)) (list (%wild-component file)))))
          (t (let ((dot (position #\. file :from-end t)))
               (if (or (null dot) (= dot 0))
                   (setq name file)
                   (setq name (subseq file 0 dot) type (subseq file (+ dot 1)))))))
    (%make-pathname nil nil directory (and name (%wild-component name)) (and type (%wild-component type)) nil)))

(defun %logical-host-of (s)
  "The logical host S starts with (HOST:...), if it is a defined one."
  (let ((colon (position #\: s)))
    (and colon (> colon 0) (let ((host (string-upcase (subseq s 0 colon)))) (and (gethash host *logical-hosts*) host)))))

(defun %parse-logical (s host)
  (let* ((rest (string-upcase (subseq s (+ 1 (position #\: s)))))
         (semi (position #\; rest :from-end t))
         (dir-part (if semi (subseq rest 0 (+ semi 1)) ""))
         (file (if semi (subseq rest (+ semi 1)) rest))
         (comps (remove "" (%split-on dir-part #\;) :test #'string=))
         (relative (and (> (length dir-part) 0) (char= (char dir-part 0) #\;)))
         (directory (and comps (cons (if relative :relative :absolute) (mapcar #'%wild-component comps))))
         (parts (if (string= file "") nil (%split-on file #\.))))
    (%make-logical-pathname host nil directory
                            (and (car parts) (%wild-component (car parts)))
                            (and (cadr parts) (%wild-component (cadr parts)))
                            (let ((v (caddr parts)))
                              (cond ((null v) nil) ((string= v "*") :wild) ((string-equal v "NEWEST") :newest)
                                    (t (or (parse-integer v :junk-allowed t) (error "Bad version in ~S" s))))))))

(defun parse-namestring (thing &optional host defaults &key (start 0) end junk-allowed)
  "Parse the namestring THING into a pathname: (values pathname end)."
  (declare (ignore defaults junk-allowed))
  (cond ((pathnamep thing) (values thing start))
        ((stringp thing)
         (let* ((e (or end (length thing))) (s (subseq thing start e))
                (lhost (or (and host (string-upcase (string host))) (%logical-host-of s))))
           (values (if (and lhost (gethash lhost *logical-hosts*) (position #\: s)) (%parse-logical s lhost) (%parse-native s)) e)))
        ((symbolp thing) (parse-namestring (string-downcase (symbol-name thing))))
        (t (error "~S cannot be turned into a pathname" thing))))

(defun pathname (thing)
  "THING as a pathname: a pathname, a namestring, or a file stream's file."
  (if (pathnamep thing) thing (values (parse-namestring thing))))

(defun logical-pathname (thing)
  "THING (a logical namestring for a defined host) as a logical pathname."
  (let ((p (pathname thing)))
    (unless (logical-pathname-p p) (error "~S is not a logical pathname (is its host defined?)" thing))
    p))

;;; ---- components ----------------------------------------------------------------------------------------

(defun pathname-host (p &key case) "The host of the pathname designator P (a logical host name, or NIL)." (declare (ignore case)) (%pn-host (pathname p)))
(defun pathname-device (p &key case) "The device of P (always NIL)." (declare (ignore case)) (%pn-device (pathname p)))
(defun pathname-directory (p &key case) "The directory of P: (:ABSOLUTE ...) or (:RELATIVE ...), or NIL." (declare (ignore case)) (%pn-directory (pathname p)))
(defun pathname-name (p &key case) "The name of P (a string, :WILD or NIL)." (declare (ignore case)) (%pn-name (pathname p)))
(defun pathname-type (p &key case) "The type (extension) of P (a string, :WILD or NIL)." (declare (ignore case)) (%pn-type (pathname p)))
(defun pathname-version (p) "The version of P (NIL, :NEWEST, :WILD or an integer)." (%pn-version (pathname p)))

(defun make-pathname (&key (host nil host-p) (device nil device-p) (directory nil directory-p) (name nil name-p)
                           (type nil type-p) (version nil version-p) defaults case)
  "A pathname from the given components; the others come from DEFAULTS (if given)."
  (declare (ignore case))
  (let* ((d (and defaults (pathname defaults)))
         (dir (if directory-p directory (and d (%pn-directory d))))
         (dir (cond ((stringp dir) (list :absolute dir)) ((eq dir :wild) (list :absolute :wild-inferiors)) (t dir)))
         (h (if host-p host (and d (%pn-host d)))))
    (funcall (if (and h (gethash (string-upcase (string h)) *logical-hosts*)) #'%make-logical-pathname #'%make-pathname)
             (and h (string-upcase (string h))) (if device-p device (and d (%pn-device d))) dir
             (if name-p name (and d (%pn-name d))) (if type-p type (and d (%pn-type d))) (if version-p version (and d (%pn-version d))))))

(defun %component-string (c)
  (case c (:wild "*") (:wild-inferiors "**") (:up "..") (:back "..") (:newest "NEWEST") (t (if (integerp c) (princ-to-string c) c))))

(defun directory-namestring (p)
  "The directory part of P as a namestring."
  (let* ((p (pathname p)) (dir (%pn-directory p)) (logical (logical-pathname-p p)))
    (if (null dir) ""
        (let ((comps (mapcar #'%component-string (cdr dir))))
          (if logical
              (format nil "~:[~;;~]~{~A;~}" (eq (car dir) :relative) comps)
              (format nil "~:[~;/~]~{~A/~}" (eq (car dir) :absolute) comps))))))

(defun file-namestring (p)
  "The name, type (and version) of P as a namestring."
  (let ((p (pathname p)))
    (format nil "~@[~A~]~@[.~A~]~@[.~A~]" (and (%pn-name p) (%component-string (%pn-name p)))
            (and (%pn-type p) (%component-string (%pn-type p)))
            (and (logical-pathname-p p) (%pn-version p) (%component-string (%pn-version p))))))

(defun host-namestring (p) "The host of P as a string (empty for a physical pathname)." (let ((h (%pn-host (pathname p)))) (if h (string h) "")))

(defun namestring (p)
  "The namestring of the pathname designator P."
  (let ((p (pathname p)))
    (%string-concat (if (logical-pathname-p p) (%string-concat (%pn-host p) ":") "") (directory-namestring p) (file-namestring p))))

(defun %dir-prefix-p (prefix dir)
  (and (eq (car prefix) (car dir)) (<= (length prefix) (length dir))
       (every #'equal (cdr prefix) (subseq (cdr dir) 0 (- (length prefix) 1)))))

(defun enough-namestring (pathname &optional (defaults *default-pathname-defaults*))
  "The shortest namestring that gives PATHNAME when merged with DEFAULTS."
  (let* ((p (pathname pathname)) (d (and defaults (pathname defaults))) (pd (%pn-directory p)) (dd (and d (%pn-directory d))))
    (if (and dd pd (eq (car dd) :absolute) (%dir-prefix-p dd pd))
        (namestring (%make-pathname nil nil (and (> (length pd) (length dd)) (cons :relative (nthcdr (length dd) pd)))
                                    (%pn-name p) (%pn-type p) (%pn-version p)))
        (namestring p))))

(defun merge-pathnames (pathname &optional (defaults *default-pathname-defaults*) (default-version :newest))
  "PATHNAME with its missing components taken from DEFAULTS; a relative directory is appended to the
default's directory."
  (declare (ignore default-version))
  (let* ((p (pathname pathname)) (d (if defaults (pathname defaults) (%make-pathname nil nil nil nil nil nil)))
         (pd (%pn-directory p)) (dd (%pn-directory d))
         (dir (cond ((null pd) dd)
                    ((and (eq (car pd) :relative) dd)
                     (let ((acc (reverse (cdr dd))))
                       (dolist (c (cdr pd)) (if (and (eq c :up) acc (stringp (car acc))) (pop acc) (push c acc)))
                       (cons (car dd) (nreverse acc))))
                    (t pd))))
    (funcall (if (logical-pathname-p (if (%pn-host p) p d)) #'%make-logical-pathname #'%make-pathname)
             (or (%pn-host p) (%pn-host d)) (or (%pn-device p) (%pn-device d)) dir
             (or (%pn-name p) (%pn-name d)) (or (%pn-type p) (%pn-type d))
             (or (%pn-version p) (%pn-version d)))))

;;; ---- wildcards --------------------------------------------------------------------------------------------

(defun wild-pathname-p (pathname &optional field-key)
  "True if PATHNAME (or its FIELD-KEY component: :NAME, :TYPE, :DIRECTORY, :VERSION) has wildcards."
  (let* ((p (pathname pathname))
         (wild (lambda (c) (or (member c '(:wild :wild-inferiors)) (and (stringp c) (find #\* c))))))
    (case field-key
      (:name (and (funcall wild (%pn-name p)) t))
      (:type (and (funcall wild (%pn-type p)) t))
      (:version (eq (%pn-version p) :wild))
      (:directory (and (some wild (cdr (%pn-directory p))) t))
      (t (or (wild-pathname-p p :name) (wild-pathname-p p :type) (wild-pathname-p p :version) (wild-pathname-p p :directory))))))

(defun %glob-match (pattern string)
  "Match STRING against a component PATTERN with * wildcards; returns the list of strings the *s matched, or :FAIL."
  (labels ((m (pi si acc)
             (cond ((>= pi (length pattern)) (if (>= si (length string)) (list (reverse acc)) nil))
                   ((char= (char pattern pi) #\*)
                    (let ((k (length string)))
                      (loop (when (< k si) (return nil))
                            (let ((r (m (+ pi 1) k (cons (subseq string si k) acc)))) (when r (return r)))
                            (setq k (- k 1)))))
                   ((and (< si (length string)) (char-equal (char pattern pi) (char string si))) (m (+ pi 1) (+ si 1) acc))
                   (t nil))))
    (let ((r (m 0 0 nil))) (if r (car r) :fail))))

(defun %match-component (pattern value)
  "Captures (a list) if VALUE matches PATTERN, else :FAIL."
  (cond ((or (eq pattern :wild) (null pattern)) (if (eq pattern :wild) (list value) nil))
        ((and (stringp pattern) (find #\* pattern)) (if (stringp value) (%glob-match pattern value) :fail))
        ((stringp pattern) (if (and (stringp value) (string-equal pattern value)) nil :fail))
        (t (if (equal pattern value) nil :fail))))

(defun %match-directory (pattern dir)
  "Captures for each wildcard of directory PATTERN (lists of components for **), or :FAIL."
  (labels ((m (ps ds acc)
             (cond ((null ps) (if (null ds) (list (reverse acc)) nil))
                   ((eq (car ps) :wild-inferiors)
                    (let ((k (length ds)))
                      (loop (when (< k 0) (return nil))
                            (let ((r (m (cdr ps) (nthcdr k ds) (cons (subseq ds 0 k) acc)))) (when r (return r)))
                            (setq k (- k 1)))))
                   ((null ds) nil)
                   (t (let ((c (%match-component (car ps) (car ds))))
                        (if (eq c :fail) nil (m (cdr ps) (cdr ds) (append (reverse c) acc))))))))
    (cond ((null pattern) nil)
          ((and (null dir) (equal pattern '(:absolute :wild-inferiors))) (list nil))
          ((not (eq (car pattern) (car dir))) :fail)
          (t (let ((r (m (cdr pattern) (cdr dir) nil))) (if r (car r) :fail))))))

(defun pathname-match-p (pathname wildcard)
  "True if PATHNAME matches the (wild) pathname WILDCARD, component by component."
  (let ((p (pathname pathname)) (w (pathname wildcard)))
    (and (not (eq :fail (if (%pn-directory w) (%match-directory (%pn-directory w) (%pn-directory p)) nil)))
         (or (null (%pn-name w)) (not (eq :fail (%match-component (%pn-name w) (%pn-name p)))))
         (or (null (%pn-type w)) (not (eq :fail (%match-component (%pn-type w) (%pn-type p)))))
         t)))

(defun %fill-component (pattern captures)
  "Fill the wildcards of the TO component PATTERN from CAPTURES: (values filled remaining-captures)."
  (cond ((eq pattern :wild) (values (if captures (car captures) nil) (cdr captures)))
        ((and (stringp pattern) (find #\* pattern))
         (let ((out (make-string-output-stream)) (cs captures))
           (dotimes (i (length pattern))
             (if (char= (char pattern i) #\*) (progn (write-string (or (car cs) "") out) (setq cs (cdr cs))) (write-char (char pattern i) out)))
           (values (get-output-stream-string out) cs)))
        (t (values pattern captures))))

(defun translate-pathname (source from-wildcard to-wildcard)
  "SOURCE (matching FROM-WILDCARD) rewritten to TO-WILDCARD, each wildcard of TO taking what the corresponding one
of FROM matched."
  (let* ((s (pathname source)) (from (pathname from-wildcard)) (to (pathname to-wildcard))
         (dcaps (if (%pn-directory from) (%match-directory (%pn-directory from) (%pn-directory s)) nil))
         (ncaps (if (%pn-name from) (%match-component (%pn-name from) (%pn-name s)) (list (%pn-name s))))
         (tcaps (if (%pn-type from) (%match-component (%pn-type from) (%pn-type s)) (list (%pn-type s)))))
    (when (or (eq dcaps :fail) (eq ncaps :fail) (eq tcaps :fail)) (error "~S does not match ~S" source from-wildcard))
    (let ((dir (and (%pn-directory to)
                    (cons (car (%pn-directory to))
                          (let ((acc nil) (caps dcaps))
                            (dolist (c (cdr (%pn-directory to)) (nreverse acc))
                              (cond ((eq c :wild-inferiors) (dolist (x (if (listp (car caps)) (car caps) (list (car caps)))) (push x acc)) (setq caps (cdr caps)))
                                    (t (multiple-value-bind (v rest) (%fill-component c caps) (push v acc) (setq caps rest))))))))))
      (funcall (if (logical-pathname-p to) #'%make-logical-pathname #'%make-pathname)
               (%pn-host to) nil (or dir (and (null (%pn-directory to)) (%pn-directory s)))
               (if (%pn-name to) (%fill-component (%pn-name to) ncaps) (%pn-name s))
               (if (%pn-type to) (%fill-component (%pn-type to) tcaps) (%pn-type s))
               (%pn-version s)))))

;;; ---- logical pathnames -------------------------------------------------------------------------------------

(defun logical-pathname-translations (host)
  "The translations of the logical HOST: a list of (from-wildcard to-pathname)."
  (or (gethash (string-upcase (string host)) *logical-hosts*) (error "~A is not a logical host" host)))

(defun (setf logical-pathname-translations) (translations host)
  "Define the logical HOST with TRANSLATIONS: (from-wildcard to-pathname) pairs, such as (\"**;*.*\" \"/src/**/*.*\")."
  (let ((h (string-upcase (string host))))
    (setf (gethash h *logical-hosts*) (list :defining))       ; so the FROM patterns parse as logical pathnames
    (setf (gethash h *logical-hosts*)
          (mapcar (lambda (tr)
                    (list (let ((f (car tr))) (if (pathnamep f) f (%parse-logical (if (position #\: f) f (%string-concat h ":" f)) h)))
                          (pathname (cadr tr))))
                  translations))
    translations))

(defun load-logical-pathname-translations (host)
  "True if HOST is a defined logical host (translations are not loaded from files here)."
  (if (gethash (string-upcase (string host)) *logical-hosts*) nil (error "Logical host ~A is not defined" host)))

(defun translate-logical-pathname (pathname &key)
  "The physical pathname a logical PATHNAME stands for (other pathnames are returned as they are)."
  (let ((p (pathname pathname)))
    (if (not (logical-pathname-p p))
        p
        (let ((tr (find-if (lambda (tr) (pathname-match-p p (car tr))) (logical-pathname-translations (%pn-host p)))))
          (unless tr (error "No translation for ~S" p))
          ;; logical names are upper case: the parts they give a physical pathname are in lower case
          (let* ((down (lambda (c) (if (stringp c) (string-downcase c) c)))
                 (source (if (logical-pathname-p (cadr tr)) p
                             (%make-logical-pathname (%pn-host p) nil (and (%pn-directory p) (cons (car (%pn-directory p)) (mapcar down (cdr (%pn-directory p)))))
                                                     (funcall down (%pn-name p)) (funcall down (%pn-type p)) (%pn-version p))))
                 (r (translate-pathname source (car tr) (cadr tr))))
            (translate-logical-pathname r))))))

;;; ---- the file system ---------------------------------------------------------------------------------------

(defun %native-namestring (x)
  "The Unix path for the pathname designator X (logical pathnames translated)."
  (cond ((stringp x) (if (%logical-host-of x) (namestring (translate-logical-pathname (pathname x))) x))
        ((pathnamep x) (namestring (translate-logical-pathname x)))
        ((streamp x) (error "~S is not a file name" x))
        (t (namestring (pathname x)))))

(defun probe-file (pathspec)
  "The truename of the file PATHSPEC names, or NIL if there is none."
  (let ((s (%native-namestring pathspec)))
    (and (%c-probe-file s) (pathname (or (%truename s) s)))))

(defun truename (pathspec)
  "The absolute pathname of the existing file PATHSPEC, with links resolved."
  (or (probe-file pathspec) (error 'simple-error :format-control "The file ~A does not exist" :format-arguments (list (%native-namestring pathspec)))))

(defun delete-file (pathspec) "Delete the file PATHSPEC; true if it was deleted." (%c-delete-file (%native-namestring pathspec)))
(defun file-write-date (pathspec) "The time the file PATHSPEC was last written (universal time), or NIL." (%c-file-write-date (%native-namestring pathspec)))

(defun rename-file (file new-name)
  "Rename FILE to NEW-NAME (merged with FILE): (values new-name old-truename new-truename)."
  (let* ((old (probe-file file)) (new (merge-pathnames new-name (pathname file))))
    (unless old (error "Cannot rename ~A: no such file" file))
    (%rename-file (%native-namestring file) (%native-namestring new))
    (values new old (probe-file new))))

(defun ensure-directories-exist (pathspec &key verbose)
  "Create the directories of PATHSPEC that do not exist: (values pathspec created-p)."
  (let* ((p (translate-logical-pathname (pathname pathspec))) (dir (%pn-directory p)) (created nil) (path (if (eq (car dir) :absolute) "/" "")))
    (dolist (c (cdr dir))
      (setq path (%string-concat path (%component-string c) "/"))
      (unless (%c-probe-file path)
        (%make-directory path) (setq created t)
        (when verbose (format t "~&; created directory ~A~%" path))))
    (values pathspec created)))

(defun user-homedir-pathname (&optional host)
  "The user's home directory, as a pathname."
  (declare (ignore host))
  (pathname (%string-concat (or (getenv "HOME") "") "/")))

(defun %list-directory (dir-path)
  "Entries of the directory DIR-PATH (a string ending in /): (name kind) lists."
  (ignore-errors (%directory (if (string= dir-path "") "." dir-path))))

(defun directory (pathspec &key)
  "The pathnames of the files matching the (possibly wild) PATHSPEC, sorted; ** matches any directories."
  (let* ((p (translate-logical-pathname (merge-pathnames pathspec))) (dir (or (%pn-directory p) (list :relative)))
         (results nil))
    (labels ((walk (path comps)
               (cond ((null comps)
                      (dolist (e (%list-directory path))
                        (let* ((name (car e)) (kind (cadr e)))
                          (unless (or (string= name ".") (string= name ".."))
                            (let ((candidate (pathname (%string-concat path name (if (eq kind :directory) "/" "")))))
                              (when (if (eq kind :directory)
                                        (and (null (%pn-name p)) (null (%pn-type p)) nil)
                                        (and (or (null (%pn-name p)) (not (eq :fail (%match-component (%pn-name p) (%pn-name candidate)))))
                                             (or (null (%pn-type p)) (not (eq :fail (%match-component (%pn-type p) (%pn-type candidate)))))))
                                (push candidate results))))))
                      (when (and (null (%pn-name p)) (null (%pn-type p)) (%c-probe-file (if (string= path "") "." path)))
                        (push (pathname path) results)))
                     ((eq (car comps) :wild-inferiors)
                      (walk path (cdr comps))
                      (dolist (e (%list-directory path))
                        (when (and (eq (cadr e) :directory) (not (member (car e) '("." "..") :test #'string=)))
                          (walk (%string-concat path (car e) "/") comps))))
                     ((or (eq (car comps) :wild) (and (stringp (car comps)) (find #\* (car comps))))
                      (dolist (e (%list-directory path))
                        (when (and (eq (cadr e) :directory) (not (member (car e) '("." "..") :test #'string=))
                                   (not (eq :fail (%match-component (car comps) (car e)))))
                          (walk (%string-concat path (car e) "/") (cdr comps)))))
                     (t (walk (%string-concat path (%component-string (car comps)) "/") (cdr comps))))))
      (walk (if (eq (car dir) :absolute) "/" "") (cdr dir)))
    (sort (remove-duplicates results :test #'equal) #'string< :key #'namestring)))

;;; #P"..." reads a pathname
(set-dispatch-macro-character #\# #\P (lambda (stream c n) (declare (ignore c n)) (pathname (read stream t nil t))) %standard-readtable)
(set-dispatch-macro-character #\# #\P (lambda (stream c n) (declare (ignore c n)) (pathname (read stream t nil t))) *readtable*)
(setq *default-pathname-defaults* (%make-pathname nil nil nil nil nil nil))
