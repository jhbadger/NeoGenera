;;;; 20-runtime.lisp -- the Pascal Tool Kit: the run-time library.
;;;;
;;;; Compiled Pascal is Lisp, so most of Pascal's data is Lisp data: integers are Lisp integers (bignums: they
;;;; never overflow), real and shortreal are double floats, boolean is T/NIL, char is a Lisp character, an
;;;; enumeration value is its ordinal, a set is an integer used as a bit mask (so sets have no size limit), and
;;;; lispobject is any Lisp object.  Arrays are simple vectors (a multi-dimensional array is a vector of vectors);
;;;; a record is a simple vector whose element 0 is the record's type name; a pointer is a PBOX; a file is a PFILE.
;;;; A variable that has not been given a value holds the symbol PASCAL-UNDEFINED, and every read checks for it.
;;;; Errors are PASCAL-RUNTIME-ERRORs, which name the routine and the source line that was running.

(in-package :cl)

(defvar *pascal-line* 0 "The source line of the Pascal statement being executed.")
(defvar *pascal-routine* nil "The name of the Pascal routine being executed.")
(defvar *pascal-program* nil "The Pascal program being executed (a PPROGRAM), or NIL.")
(defvar *pascal-input* nil "The PFILE of the standard file INPUT in the running program.")
(defvar *pascal-output* nil "The PFILE of the standard file OUTPUT in the running program.")
(defvar *pascal-open-files* nil "Files opened by the running program, closed when it ends.")
(defvar *pascal-pathname-defaults* "" "Directory that file names in RESET and REWRITE are relative to.")
(defvar *pascal-streams* nil "An alist (NAME . stream) binding Pascal file variables to Lisp streams.")
(defvar *pascal-integer-field-width* 10 "The default field width of integers written by WRITE.")
(defvar *pascal-real-field-width* 20 "The default field width of reals written by WRITE (floating-point form).")

(define-condition pascal-runtime-error (error)
  ((message :initarg :message :reader pascal-error-message)
   (routine :initarg :routine :initform nil :reader pascal-error-routine)
   (line :initarg :line :initform nil :reader pascal-error-line)
   (original :initarg :original :initform nil :reader pascal-error-original))
  (:report (lambda (c s)
             (format s "~A~@[~%  (in Pascal routine ~A~]~:[~;, line ~D~]~:[~;)~]"
                     (pascal-error-message c) (and (pascal-error-routine c) (string-downcase (symbol-name (pascal-error-routine c))))
                     (and (pascal-error-routine c) (pascal-error-line c) (> (pascal-error-line c) 0)) (pascal-error-line c)
                     (pascal-error-routine c)))))

(defun %perror (fmt &rest args)
  "Signal a PASCAL-RUNTIME-ERROR in the routine and line being executed."
  (error 'pascal-runtime-error :message (apply #'format nil fmt args) :routine *pascal-routine* :line *pascal-line*))

(defun %pundef (name)
  (%perror "The variable ~A is used before it has been given a value" (string-downcase (string name))))

(defmacro %pchk (form name)
  "FORM's value, which must not be the undefined marker."
  (let ((v (if (symbolp form) form (gensym "V"))))
    (if (symbolp form)
        `(if (eq ,v 'pascal-undefined) (%pundef ',name) ,v)
        `(let ((,v ,form)) (if (eq ,v 'pascal-undefined) (%pundef ',name) ,v)))))

;;; ---- arithmetic -------------------------------------------------------------------------------------------

(defun %pnum (x what)
  (if (numberp x) x (if (eq x 'pascal-undefined) (%pundef what) (%perror "~S is not a number" x))))

(defun %pdivi (a b)
  (when (eql b 0) (%perror "Division by zero: ~D div 0" a))
  (values (truncate a b)))
(defun %pmod (a b)
  (when (<= b 0) (%perror "The divisor of ~D mod ~D is not positive" a b))
  (mod a b))
(defun %prdiv (a b)
  (when (zerop b) (%perror "Division by zero: ~A / 0" a))
  (/ (float a 1d0) b))
(defun %psqrt (x)
  (when (< x 0) (%perror "Attempt to take the square root of ~A, which is negative" (float x 1d0)))
  (sqrt (float x 1d0)))
(defun %pln (x)
  (when (<= x 0) (%perror "Attempt to take the logarithm of ~A, which is not positive" (float x 1d0)))
  (log (float x 1d0)))
(defun %pexp (x) (exp (float x 1d0)))
(defun %ptrunc (x) (values (truncate x)))
(defun %pround (x) (if (>= x 0) (values (floor (+ x 1/2))) (values (ceiling (- x 1/2)))))
(defun %pchr (n)
  (unless (and (integerp n) (<= 0 n #x10FFFF)) (%perror "chr(~A): no such character" n))
  (code-char n))
(defun %psingle (x)
  "The double nearest to X with a 24-bit significand (the precision of shortreal)."
  (let ((x (float x 1d0)))
    (if (zerop x) x
        (multiple-value-bind (m e s) (integer-decode-float x)
          (let* ((len (integer-length m)) (drop (max 0 (- len 24))))
            (* s (float (* (round m (expt 2 drop)) (expt 2 drop)) 1d0) (expt 2d0 e)))))))

(defun %pidx (i lo hi)
  "The zero-based index of subscript I of an array indexed LO..HI."
  (if (and (integerp i) (<= lo i) (<= i hi)) (- i lo)
      (if (eq i 'pascal-undefined) (%pundef "subscript") (%perror "Index ~A is out of the array bounds ~D..~D" i lo hi))))

(defun %prange (v lo hi)
  "V, which must be in LO..HI (either bound may be NIL)."
  (when (or (and lo (< v lo)) (and hi (> v hi))) (%perror "The value ~A is out of the range ~A..~A" v (or lo "") (or hi "")))
  v)
(defun %prange-char (c lo hi)
  (let ((n (char-code c)))
    (when (or (< n lo) (> n hi)) (%perror "The character ~S is out of the range ~S..~S" c (code-char lo) (code-char hi)))
    c))

;;; ---- sets: integers used as bit masks ---------------------------------------------------------------------

(defun %pset-elt (n)
  (when (or (not (integerp n)) (< n 0)) (%perror "~A cannot be a member of a set (ordinals must be 0 or more)" n))
  (ash 1 n))
(defun %pset-range (lo hi)
  (if (> lo hi) 0 (progn (%pset-elt lo) (ash (- (ash 1 (+ (- hi lo) 1)) 1) lo))))
(defun %pin (n set) (and (integerp n) (>= n 0) (logbitp n set)))
(defun %psubset (a b) (zerop (logand a (lognot b))))
(defun %pset-members (set)
  "The ordinals in SET, ascending."
  (let ((acc nil) (i 0))
    (loop (when (zerop set) (return (nreverse acc)))
          (when (oddp set) (push i acc))
          (setq set (ash set -1) i (+ i 1)))))

;;; ---- values: initial values, copies, records, pointers ------------------------------------------------------

(defstruct (pbox (:constructor %make-pbox (value)) (:copier nil)) value)

(defmethod print-object ((p pbox) s)
  (format s "#<Pascal pointer to ~A>" (if (eq (pbox-value p) 'pascal-disposed) "disposed storage" (%pabbrev (pbox-value p)))))

(defun %pabbrev (x) (let ((s (prin1-to-string x))) (if (> (length s) 40) (%string-concat (subseq s 0 37) "...") s)))

(defun %pmake (desc zero)
  "A fresh value for a variable of type DESC: undefined, or with ZERO, zero/false/empty."
  (if (consp desc)
      (case (car desc)
        (:array (let ((v (make-array (cadr desc))) (e (caddr desc)))
                  (dotimes (i (cadr desc)) (setf (svref v i) (%pmake e zero)))
                  v))
        (:record (let* ((fields (caddr desc)) (v (make-array (+ 1 (length fields)))))
                   (setf (svref v 0) (cadr desc))
                   (let ((i 1)) (dolist (f fields) (setf (svref v i) (%pmake f zero)) (setq i (+ i 1))))
                   v))
        (:file (%make-pfile :textp (cadr desc) :elem (caddr desc) :name (cadddr desc)))
        (:string (if zero "" 'pascal-undefined))
        (t 'pascal-undefined))
      (if zero
          (case desc (:integer 0) (:real 0d0) (:boolean nil) (:char (code-char 0)) (:enum 0) (:set 0) (:pointer nil)
                (:string "") (:lispobject nil) (t 0))
          (case desc ((:set :lispobject) (if (eq desc :set) 0 nil)) (t 'pascal-undefined)))))

(defun %pcopy (x)
  "A copy of the Pascal value X: arrays and records are copied (deeply); everything else is shared."
  (if (simple-vector-p x)
      (let ((v (copy-seq x)))
        (dotimes (i (length v)) (let ((e (svref v i))) (when (simple-vector-p e) (setf (svref v i) (%pcopy e)))))
        v)
      x))

(defun %pnew (desc zero) (%make-pbox (%pmake desc zero)))
(defun %pderef (p)
  (cond ((null p) (%perror "Attempt to follow the pointer NIL"))
        ((eq p 'pascal-undefined) (%pundef "pointer"))
        ((not (pbox-p p)) (%perror "~S is not a pointer" p))
        ((eq (pbox-value p) 'pascal-disposed) (%perror "Attempt to follow a pointer to storage that has been disposed of"))
        (t p)))
(defun %pdispose (p) (setf (pbox-value (%pderef p)) 'pascal-disposed) nil)

(defun %precord-check (r)
  (cond ((simple-vector-p r) r)
        ((eq r 'pascal-undefined) (%pundef "record"))
        (t (%perror "~S is not a record" r))))
(defun %parray-check (a)
  (cond ((simple-vector-p a) a)
        ((eq a 'pascal-undefined) (%pundef "array"))
        (t (%perror "~S is not an array" a))))

;;; references, for VAR parameters: (getter . setter)
(defmacro %pref (place-reader setter-var setter-form)
  `(cons (lambda () ,place-reader) (lambda (,setter-var) ,setter-form)))
(defun %rget (r) (funcall (car r)))
(defun %rset (r v) (funcall (cdr r) v))

;;; ---- strings and packed arrays of char --------------------------------------------------------------------

(defun %pstring->chars (s n)
  "A packed array of N characters holding the string S, padded with spaces."
  (when (> (length s) n) (%perror "The string ~S is longer than ~D characters" s n))
  (let ((v (make-array n :initial-element #\Space)))
    (dotimes (i (length s)) (setf (svref v i) (char s i)))
    v))
(defun %pchars->string (v)
  (cond ((stringp v) v)
        ((characterp v) (string v))
        ((eq v 'pascal-undefined) (%pundef "string"))
        (t (let ((out (make-string-output-stream)))
             (dotimes (i (length v)) (let ((c (svref v i))) (write-char (if (characterp c) c #\Space) out)))
             (get-output-stream-string out)))))
(defun %pstr-compare (a b)
  "-1, 0 or 1 comparing the strings (or packed char arrays) A and B."
  (let ((a (%pchars->string a)) (b (%pchars->string b)))
    (cond ((string= a b) 0) ((string< a b) -1) (t 1))))

;;; Pascal/VS strings: Lisp strings with a declared maximum length
(defun %pvs (s maxlen)
  (let ((s (%pchars->string s)))
    (when (and maxlen (> (length s) maxlen)) (%perror "The string ~S is longer than its maximum length ~D" s maxlen))
    s))
(defun %pvs-char (s i)
  (let ((s (%pchars->string s)))
    (unless (and (integerp i) (<= 1 i (length s))) (%perror "Index ~A is outside the string ~S (length ~D)" i s (length s)))
    (char s (- i 1))))
(defun %pvs-set-char (s i c)
  (let ((s (copy-seq (%pchars->string s))))
    (unless (and (integerp i) (<= 1 i (length s))) (%perror "Index ~A is outside the string ~S (length ~D)" i s (length s)))
    (setf (char s (- i 1)) c)
    s))
(defun %pvs-substr (s start &optional len)
  (let* ((s (%pchars->string s)) (n (length s)) (len (or len (- n (- start 1)))))
    (when (or (< start 1) (< len 0) (> (+ start len -1) n))
      (%perror "substr(~S, ~D, ~D) is outside the string" s start len))
    (subseq s (- start 1) (+ start len -1))))
(defun %pvs-index (s sub)
  (let ((p (search (%pchars->string sub) (%pchars->string s)))) (if p (+ p 1) 0)))
(defun %pvs-trim (s) (string-right-trim '(#\Space) (%pchars->string s)))
(defun %pvs-ltrim (s) (string-left-trim '(#\Space) (%pchars->string s)))
(defun %pvs-compress (s)
  "S with each run of blanks reduced to one blank."
  (let ((out (make-string-output-stream)) (blank nil))
    (dolist (c (coerce (%pchars->string s) 'list))
      (if (char= c #\Space) (unless blank (write-char c out) (setq blank t)) (progn (write-char c out) (setq blank nil))))
    (get-output-stream-string out)))
(defun %pvs-str (x)
  (cond ((characterp x) (string x)) ((stringp x) x) ((simple-vector-p x) (%pchars->string x))
        ((integerp x) (format nil "~D" x)) ((floatp x) (%pformat-real x nil nil)) (t (princ-to-string x))))
(defun %pvs-concat (a b) (%string-concat (%pchars->string a) (%pchars->string b)))

;;; ---- files ------------------------------------------------------------------------------------------------

(defstruct (pfile (:constructor %make-pfile) (:copier nil))
  name (textp t) elem mode stream reader line (pos 0) (eof nil) (buffer 'pascal-undefined) elements interactive owned
  (last-char nil))

(defmethod print-object ((f pfile) s)
  (format s "#<Pascal ~:[file~;text file~] ~A~@[ ~(~A~)~]>" (pfile-textp f) (or (pfile-name f) "?") (pfile-mode f)))

(defun %pfile-check (f what)
  (cond ((pfile-p f) f)
        ((eq f 'pascal-undefined) (%pundef what))
        (t (%perror "~S is not a file" f))))

(defun %pstd-output ()
  (or *pascal-output* (setq *pascal-output* (%make-pfile :name "OUTPUT" :mode :write :stream *standard-output*))))
(defun %pstd-input ()
  (or *pascal-input* (setq *pascal-input* (%make-pfile :name "INPUT" :mode :read :reader (%pascal-terminal-reader) :interactive t))))

(defun %pascal-terminal-reader ()
  "A function returning the next line typed by the user (or :EOF).  In the window Listener the line is read
with the Listener's input line; elsewhere from *STANDARD-INPUT*."
  (let ((in *standard-input*) (out *standard-output*) (listener (and (boundp '*current-listener*) *current-listener*)))
    (lambda ()
      (finish-output out)
      (if (and listener (fboundp '%interactive-listener-p) (%interactive-listener-p listener out))
          (%pascal-listener-line listener)
          (read-line in nil :eof)))))

(defun %pascal-listener-line (w)
  "A line typed in the Listener W, read as a plain line of input (no prompt of its own: the program's shows)."
  (let* ((st (make-accept-state :type 'string :prompt "" :default "" :default-p t))
         (ctx (make-input-context :type-fn (lambda () 'string) :label "Pascal input"
                                  :supply (lambda (obj) (%accept-finish w st obj t))))
         (*input-contexts* (cons ctx *input-contexts*)))
    (setf (lw-accepts w) (cons st (lw-accepts w)))
    (unwind-protect (%nested-serve w (lambda () (as-done st)))
      (setf (lw-accepts w) (remove st (lw-accepts w))))
    (let ((v (as-value st))) (if (stringp v) v (princ-to-string v)))))

(defun %pfile-path (f opts)
  "The file name a RESET or REWRITE of F opens: the NAME= option, else the file variable's name."
  (let ((name (or (%popt opts "NAME") (%popt opts "DDNAME") (string-downcase (or (pfile-name f) "pascal.data")))))
    (if (and (> (length name) 0) (or (char= (char name 0) #\/) (char= (char name 0) #\~)))
        name
        (%string-concat (or *pascal-pathname-defaults* "") name))))

(defun %popt (opts key)
  "The value of KEY in a file-opening option string 'name = x, access = sequential'."
  (when opts
    (dolist (part (%split-string (%pchars->string opts) #\,) nil)
      (let ((eq (%string-position-char part #\= 0)))
        (when (and eq (string-equal (string-trim " " (subseq part 0 eq)) key))
          (return (string-trim " " (subseq part (+ eq 1)))))))))

(defun %pclose (f)
  (let ((f (%pfile-check f "file")))
    (when (and (pfile-owned f) (pfile-stream f)) (close (pfile-stream f)))
    (when (and (eq (pfile-mode f) :write) (not (pfile-textp f)) (pfile-stream f)) nil)
    (setf (pfile-stream f) nil (pfile-mode f) nil (pfile-owned f) nil (pfile-reader f) nil)
    (setq *pascal-open-files* (remove f *pascal-open-files*))
    nil))

(defun %pstream-for (f)
  (cdr (assoc (pfile-name f) *pascal-streams* :test #'string-equal)))

(defun %preset (f &optional opts)
  (let ((f (%pfile-check f "file")))
    (cond ((eq f *pascal-input*) (setf (pfile-eof f) nil) f)
          (t
           (when (pfile-owned f) (%pclose f))
           (let* ((stream (or (%pstream-for f)
                              (let ((path (%pfile-path f opts)))
                                (unless (probe-file path) (%perror "reset: there is no file ~A" path))
                                (open path))))
                  (owned (not (%pstream-for f))))
             (setf (pfile-mode f) :read (pfile-stream f) stream (pfile-owned f) owned (pfile-line f) nil (pfile-pos f) 0
                   (pfile-eof f) nil (pfile-interactive f) nil)
             (when owned (push f *pascal-open-files*))
             (if (pfile-textp f)
                 (setf (pfile-reader f) (lambda () (read-line stream nil :eof)))
                 (let ((elems nil))
                   (loop (let ((x (read stream nil :eof))) (when (eq x :eof) (return)) (push x elems)))
                   (setf (pfile-elements f) (nreverse elems))
                   (%pbin-load f)))
             f)))))

(defun %prewrite (f &optional opts)
  (let ((f (%pfile-check f "file")))
    (cond ((eq f *pascal-output*) f)
          (t
           (when (pfile-owned f) (%pclose f))
           (let* ((s (%pstream-for f))
                  (stream (or s (open (%pfile-path f opts) :direction :output :if-exists :supersede :if-does-not-exist :create))))
             (setf (pfile-mode f) :write (pfile-stream f) stream (pfile-owned f) (not s) (pfile-eof f) t)
             (unless s (push f *pascal-open-files*))
             f)))))

(defun %pneed-mode (f mode what)
  (unless (eq (pfile-mode f) mode)
    (%perror "~A: the file ~A is not open for ~:[writing~;reading~] (use ~:[rewrite~;reset~] first)" what (or (pfile-name f) "?")
             (eq mode :read) (eq mode :read))))

;;; text input: lines are fetched lazily, so prompts written before a read appear first
(defun %pfetch (f)
  "Make sure F has a current line (unless at end of file)."
  (unless (or (pfile-line f) (pfile-eof f))
    (let ((l (funcall (pfile-reader f))))
      (if (eq l :eof) (setf (pfile-eof f) t) (setf (pfile-line f) l (pfile-pos f) 0)))))

(defun %peof (f)
  (let ((f (%pfile-check f "file")))
    (cond ((eq (pfile-mode f) :write) t)
          ((null (pfile-mode f)) (%perror "eof: the file ~A is not open" (pfile-name f)))
          ((pfile-textp f) (%pfetch f) (pfile-eof f))
          (t (null (pfile-elements f))))))

(defun %peoln (f)
  (let ((f (%pfile-check f "file")))
    (%pneed-mode f :read "eoln")
    (%pfetch f)
    (when (pfile-eof f) (%perror "eoln: the file ~A is at its end" (pfile-name f)))
    (>= (pfile-pos f) (length (pfile-line f)))))

(defun %ptext-peek (f)
  "The character at the file position (a space at the end of a line), or NIL at the end of the file."
  (%pfetch f)
  (cond ((pfile-eof f) nil)
        ((>= (pfile-pos f) (length (pfile-line f))) #\Space)
        (t (char (pfile-line f) (pfile-pos f)))))

(defun %ptext-advance (f)
  (%pfetch f)
  (when (pfile-eof f) (%perror "get: the file ~A is at its end" (pfile-name f)))
  (if (>= (pfile-pos f) (length (pfile-line f)))
      (setf (pfile-line f) nil (pfile-pos f) 0)
      (setf (pfile-pos f) (+ 1 (pfile-pos f)))))

(defun %pread-char (f)
  (let ((f (%pfile-check f "file")))
    (%pneed-mode f :read "read")
    (let ((c (%ptext-peek f)))
      (unless c (%perror "read: attempt to read past the end of the file ~A" (pfile-name f)))
      (%ptext-advance f)
      c)))

(defun %pskip-blanks (f)
  (loop (let ((c (%ptext-peek f)))
          (cond ((null c) (%perror "read: attempt to read past the end of the file ~A" (pfile-name f)))
                ((member c '(#\Space #\Tab #\Return #\Page)) (%ptext-advance f))
                (t (return))))))

(defun %pread-number-text (f realp)
  (%pfetch f)
  (%pskip-blanks f)
  (let* ((line (pfile-line f)) (n (length line)) (i (pfile-pos f)) (start i))
    (when (and (< i n) (member (char line i) '(#\+ #\-))) (setq i (+ i 1)))
    (let ((d0 i))
      (loop (when (or (>= i n) (not (digit-char-p (char line i)))) (return)) (setq i (+ i 1)))
      (when (= i d0) (%perror "read: ~:[an integer~;a number~] was expected in the input, not ~S" realp (subseq line start (min n (+ start 20)))))
      (when realp
        (when (and (< (+ i 1) n) (char= (char line i) #\.) (digit-char-p (char line (+ i 1))))
          (setq i (+ i 1))
          (loop (when (or (>= i n) (not (digit-char-p (char line i)))) (return)) (setq i (+ i 1))))
        (when (and (< i n) (char-equal (char line i) #\E))
          (let ((j (+ i 1)))
            (when (and (< j n) (member (char line j) '(#\+ #\-))) (setq j (+ j 1)))
            (when (and (< j n) (digit-char-p (char line j)))
              (setq i j)
              (loop (when (or (>= i n) (not (digit-char-p (char line i)))) (return)) (setq i (+ i 1))))))))
    (setf (pfile-pos f) i)
    (subseq line start i)))

(defun %pread-int (f)
  (let ((f (%pfile-check f "file")))
    (%pneed-mode f :read "read")
    (parse-integer (%pread-number-text f nil))))

(defun %pread-real (f)
  (let ((f (%pfile-check f "file")))
    (%pneed-mode f :read "read")
    (let* ((text (%pread-number-text f t)) (neg (and (> (length text) 0) (char= (char text 0) #\-)))
           (body (if (and (> (length text) 0) (member (char text 0) '(#\+ #\-))) (subseq text 1) text)))
      (let ((v (%pas-parse-real body))) (if neg (- v) v)))))

(defun %pread-line-string (f &optional maxlen)
  "The rest of the current line (at most MAXLEN characters)."
  (let ((f (%pfile-check f "file")))
    (%pneed-mode f :read "read")
    (%pfetch f)
    (when (pfile-eof f) (%perror "read: attempt to read past the end of the file ~A" (pfile-name f)))
    (let* ((line (pfile-line f)) (end (if maxlen (min (length line) (+ (pfile-pos f) maxlen)) (length line)))
           (s (subseq line (pfile-pos f) end)))
      (setf (pfile-pos f) end)
      s)))

(defun %pread-chars (f n)
  (let ((v (make-array n :initial-element #\Space)))
    (dotimes (i n) (if (%peoln f) (return) (setf (svref v i) (%pread-char f))))
    v))

(defun %preadln (f)
  (let ((f (%pfile-check f "file")))
    (%pneed-mode f :read "readln")
    (%pfetch f)
    (when (pfile-eof f) (%perror "readln: attempt to read past the end of the file ~A" (pfile-name f)))
    (setf (pfile-line f) nil (pfile-pos f) 0)
    nil))

;;; non-text files: elements are Lisp data, one per line
(defun %pbin-load (f)
  (setf (pfile-buffer f) (if (pfile-elements f) (car (pfile-elements f)) 'pascal-undefined)
        (pfile-eof f) (null (pfile-elements f))))

(defun %pfile-buf (f)
  (let ((f (%pfile-check f "file")))
    (if (pfile-textp f)
        (if (eq (pfile-mode f) :read) (or (%ptext-peek f) (%perror "The file ~A is at its end" (pfile-name f))) (pfile-buffer f))
        (pfile-buffer f))))
(defun %pfile-set-buf (f v) (setf (pfile-buffer (%pfile-check f "file")) v))

(defun %pget (f)
  (let ((f (%pfile-check f "file")))
    (%pneed-mode f :read "get")
    (if (pfile-textp f)
        (%ptext-advance f)
        (progn (when (null (pfile-elements f)) (%perror "get: the file ~A is at its end" (pfile-name f)))
               (setf (pfile-elements f) (cdr (pfile-elements f)))
               (%pbin-load f)))
    nil))

(defun %pput (f)
  (let ((f (%pfile-check f "file")))
    (%pneed-mode f :write "put")
    (if (pfile-textp f)
        (let ((c (pfile-buffer f))) (when (characterp c) (write-char c (pfile-stream f))))
        (progn (prin1 (pfile-buffer f) (pfile-stream f)) (terpri (pfile-stream f))))
    nil))

(defun %pread-elem (f)
  (let ((f (%pfile-check f "file")))
    (%pneed-mode f :read "read")
    (when (null (pfile-elements f)) (%perror "read: attempt to read past the end of the file ~A" (pfile-name f)))
    (prog1 (%pcopy (pfile-buffer f)) (%pget f))))

(defun %pwrite-elem (f x)
  (let ((f (%pfile-check f "file")))
    (%pneed-mode f :write "write")
    (setf (pfile-buffer f) x)
    (%pput f)))

;;; text output
(defun %pout-stream (f)
  (let ((f (%pfile-check f "file")))
    (%pneed-mode f :write "write")
    (pfile-stream f)))

(defun %ppad (s w stream)
  (when (and w (> w (length s))) (dotimes (i (- w (length s))) (write-char #\Space stream)))
  (write-string s stream))

(defun %pwrite-int (f x &optional w)
  (unless (integerp x) (if (eq x 'pascal-undefined) (%pundef "value written") (%perror "write: ~S is not an integer" x)))
  (%ppad (format nil "~D" x) (or w *pascal-integer-field-width*) (%pout-stream f)))

(defun %pformat-real (x w d)
  "X written as Pascal does: with D, in fixed-point form with D decimals; otherwise in floating-point form
(sign or blank, one digit, decimals, E, sign, three exponent digits) filling W."
  (let ((x (float x 1d0)))
    (if d
        (format nil "~,vF" (max 0 d) x)
        (let* ((w (or w *pascal-real-field-width*)) (frac (max 1 (- w 8))))
          (let ((s (format nil (%string-concat "~," (princ-to-string frac) ",3,,,,'EE") (abs x))))
            ;; the exponent sign is always written
            (let ((epos (position #\E s)))
              (when (and epos (< (+ epos 1) (length s)) (digit-char-p (char s (+ epos 1))))
                (setq s (%string-concat (subseq s 0 (+ epos 1)) "+" (subseq s (+ epos 1))))))
            (%string-concat (if (minusp x) "-" " ") s))))))

(defun %pwrite-real (f x &optional w d)
  (unless (realp x) (if (eq x 'pascal-undefined) (%pundef "value written") (%perror "write: ~S is not a real number" x)))
  (%ppad (%pformat-real x w d) w (%pout-stream f)))

(defun %pwrite-char (f c &optional w)
  (unless (characterp c) (if (eq c 'pascal-undefined) (%pundef "value written") (%perror "write: ~S is not a character" c)))
  (%ppad (string c) w (%pout-stream f)))

(defun %pwrite-str (f s &optional w)
  (let ((s (%pchars->string s)))
    (if (and w (< w (length s))) (write-string (subseq s 0 (max 0 w)) (%pout-stream f)) (%ppad s w (%pout-stream f)))))

(defun %pwrite-bool (f b &optional w)
  (when (eq b 'pascal-undefined) (%pundef "value written"))
  (%ppad (if b "TRUE" "FALSE") w (%pout-stream f)))

(defun %pwrite-any (f x &optional w)
  "Write a lispobject (with PRINC)."
  (%ppad (princ-to-string x) w (%pout-stream f)))

(defun %pwriteln (f) (terpri (%pout-stream f)) nil)
(defun %ppage (f) (write-char #\Page (%pout-stream f)) nil)

;;; ---- the Lisp interface -----------------------------------------------------------------------------------

(defun %plisp-value (x)
  "A Pascal value as passed to Lisp: packed arrays of characters become strings."
  (if (and (simple-vector-p x) (> (length x) 0) (every #'characterp x)) (%pchars->string x) x))

(defun %pbool (x) (if x t nil))
