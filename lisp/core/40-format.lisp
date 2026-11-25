;;;; 40-format.lisp -- FORMAT, string streams, errors, FILE I/O macros, LOAD.

(in-package :cl)

;;; ---- bootstrap error (replaced by the condition system in 54-conditions.lisp) ----

(defun error (fmt &rest args)
  (%error-to-top (if (stringp fmt) (apply #'format nil fmt args) (format nil "~A~{ ~S~}" fmt args))))

(defun warn (fmt &rest args)
  (format *error-output* "~&WARNING: ~A~%" (apply #'format nil fmt args))
  nil)

;;; ---- time ---------------------------------------------------------------------

(defmacro time (form) `(%time-it (lambda () ,form)))
(defun %time-it (thunk)
  (let* ((start (get-internal-real-time))
         (gcs (%gc-count))
         (r (multiple-value-list (funcall thunk)))
         (us (- (get-internal-real-time) start)))
    (format *error-output* "~&; Evaluation took ~D.~3,'0D seconds (~D GCs)~%"
            (floor us 1000000) (floor (mod us 1000000) 1000) (- (%gc-count) gcs))
    (values-list r)))

;;; ---- streams and file I/O -------------------------------------------------------

(defmacro with-output-to-string ((var) &body body)
  `(let ((,var (make-string-output-stream)))
     ,@body
     (get-output-stream-string ,var)))
(defmacro with-input-from-string ((var string) &body body)
  `(let ((,var (make-string-input-stream ,string))) ,@body))

(defun open (path &key (direction :input) (if-exists :supersede) (if-does-not-exist :default) element-type)
  (declare (ignore element-type))
  (let ((p (if (stringp path) path (string path))))
    (when (and (eq direction :input) (not (probe-file p)))
      (if (eq if-does-not-exist :create) nil (error "File not found: ~A" p)))
    (let ((s (%open p (if (and (eq direction :output) (eq if-exists :append)) :append direction) if-exists)))
      (unless s (error "Cannot open file: ~A" p))
      s)))

(defmacro with-open-file ((var path &rest options) &body body)
  `(let ((,var (open ,path ,@options)))
     (unwind-protect (progn ,@body) (close ,var))))

(defun read-from-string (string &optional (eof-error-p t) eof-value &key (start 0) end)
  (declare (ignore end))
  (multiple-value-bind (obj pos) (%read-from-string string start)
    (if (eq obj (%eof-object))
        (if eof-error-p (error "End of file in READ-FROM-STRING") eof-value)
        (values obj pos))))
(defun %eof-object () (%read-from-string "" 0))

(defun write-to-string (obj &rest r) (declare (ignore r)) (prin1-to-string obj))
(defun write (obj &key stream escape &allow-other-keys)
  (if (or escape (null escape))
      (prin1 obj stream)
      (princ obj stream))
  obj)

(defvar *load-pathname* nil)
(defvar *load-verbose* nil)

(defun eval (form) (%eval form))

(defun load (path &key verbose print if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (let ((*package* *package*) (*load-pathname* path))
    (with-open-file (s path)
      (let ((eof (list nil)))
        (block nil
          (tagbody top
             (let ((form (read s nil eof)))
               (when (eq form eof) (return))
               (let ((v (eval form)))
                 (when print (format t "~&; ~S~%" v))))
             (go top))))))
  (when verbose (format *error-output* "~&; loaded ~A~%" path))
  t)

(defun %read-all-forms (path)
  (with-open-file (s path)
    (let ((eof (list nil)) (acc nil))
      (block nil
        (tagbody top
           (let ((form (read s nil eof)))
             (when (eq form eof) (return))
             (setq acc (cons form acc)))
           (go top)))
      (nreverse acc))))

;;; ---- FORMAT ----------------------------------------------------------------------

(defun format (dest control &rest args)
  (let ((stream (cond ((null dest) (make-string-output-stream))
                      ((eq dest t) *standard-output*)
                      ((stringp dest) (make-string-output-stream))
                      (t dest))))
    (%format-run stream control args)
    (if (or (null dest) (stringp dest)) (get-output-stream-string stream) nil)))

(defun %fmt-pad (stream s mincol padchar left)
  (let ((n (length s)))
    (if (and mincol (< n mincol))
        (if left
            (progn (write-string s stream) (dotimes (i (- mincol n)) (write-char padchar stream)))
            (progn (dotimes (i (- mincol n)) (write-char padchar stream)) (write-string s stream)))
        (write-string s stream))))

(defun %fmt-find-close (control start open close)
  ;; index of the directive character matching CLOSE, scanning from START
  (let ((depth 1) (i start) (n (length control)))
    (block nil
      (tagbody top
         (when (>= i n) (error "FORMAT: unmatched ~~~A" open))
         (if (char= (char control i) #\~)
             (let ((j (+ i 1)))
               (block skip
                 (tagbody skiploop
                    (when (and (< j n) (or (digit-char-p (char control j)) (find (char control j) ",:@'v#")))
                      (when (char= (char control j) #\') (setq j (+ j 1)))
                      (setq j (+ j 1)) (go skiploop))))
               (when (< j n)
                 (let ((c (char control j)))
                   (cond ((char= c open) (setq depth (+ depth 1)))
                         ((char= c close) (setq depth (- depth 1))
                          (when (= depth 0) (return (list i j))))
                         ((and (char= c #\;) (= depth 1) nil) nil)))
                 (setq i (+ j 1))))
             (setq i (+ i 1)))
         (go top)))))

(defun %fmt-split-clauses (s)
  ;; split on ~; at nesting depth 0
  (let ((parts nil) (start 0) (i 0) (n (length s)) (depth 0))
    (block nil
      (tagbody top
         (when (>= i n) (return))
         (if (and (char= (char s i) #\~) (< (+ i 1) n))
             (let ((c (char s (+ i 1))))
               (cond ((find c "[{(") (setq depth (+ depth 1)))
                     ((find c "]})") (setq depth (- depth 1)))
                     ((and (char= c #\;) (= depth 0))
                      (setq parts (cons (subseq s start i) parts))
                      (setq start (+ i 2))))
               (setq i (+ i 2)))
             (setq i (+ i 1)))
         (go top)))
    (nreverse (cons (subseq s start) parts))))

(defun %format-run (stream control args)
  (let ((i 0) (n (length control)) (prev nil) (start-args nil))
    (block run
      (tagbody top
         (when (>= i n) (return-from run args))
         (let ((c (char control i)))
           (if (not (char= c #\~))
               (progn (write-char c stream) (setq i (+ i 1)))
               (let ((params nil) (colon nil) (at nil) (j (+ i 1)) (dchar nil))
                 ;; parse prefix parameters
                 (block parse
                   (tagbody ploop
                      (let ((ch (char control j)) (val :none))
                        (cond ((or (digit-char-p ch) (and (char= ch #\-) (digit-char-p (char control (+ j 1)))))
                               (let ((k j) (neg nil) (v 0))
                                 (when (char= ch #\-) (setq neg t) (setq k (+ k 1)))
                                 (block nil
                                   (tagbody dl
                                      (when (and (< k n) (digit-char-p (char control k)))
                                        (setq v (+ (* v 10) (digit-char-p (char control k))))
                                        (setq k (+ k 1)) (go dl))))
                                 (setq val (if neg (- v) v))
                                 (setq j k)))
                              ((char= ch #\') (setq val (char control (+ j 1))) (setq j (+ j 2)))
                              ((char= ch #\v) (setq val (car args)) (setq args (cdr args)) (setq j (+ j 1)))
                              ((char= ch #\#) (setq val (length args)) (setq j (+ j 1))))
                        (cond ((eq val :none)
                               (if (char= ch #\,)
                                   (progn (setq params (cons nil params)) (setq j (+ j 1)) (go ploop))
                                   (return-from parse nil)))
                              (t (setq params (cons val params))
                                 (if (char= (char control j) #\,)
                                     (progn (setq j (+ j 1)) (go ploop))
                                     (return-from parse nil)))))))
                 (setq params (nreverse params))
                 (block mods
                   (tagbody mloop
                      (let ((ch (char control j)))
                        (cond ((char= ch #\:) (setq colon t) (setq j (+ j 1)) (go mloop))
                              ((char= ch #\@) (setq at t) (setq j (+ j 1)) (go mloop))))))
                 (setq dchar (char-upcase (char control j)))
                 (setq i (+ j 1))
                 (setq start-args args)
                 (case dchar
                   (#\A (let ((s (if (and colon (null (car args))) "()" (princ-to-string (car args)))))
                          (setq args (cdr args))
                          (%fmt-pad stream s (car params) (or (caddr params) #\Space) (not at))))
                   (#\S (let ((s (prin1-to-string (car args))))
                          (setq args (cdr args))
                          (%fmt-pad stream s (car params) (or (caddr params) #\Space) (not at))))
                   ((#\D #\B #\O #\X)
                    (let* ((v (car args))
                           (radix (case dchar (#\D 10) (#\B 2) (#\O 8) (t 16)))
                           (s (if (integerp v) (%number-to-string v radix) (princ-to-string v))))
                      (setq args (cdr args))
                      (when (and at (integerp v) (>= v 0)) (setq s (%string-concat "+" s)))
                      (when (and colon (integerp v))
                        (setq s (%fmt-commas s (or (cadddr params) #\,))))
                      (when (and (eq dchar #\X)) (setq s (string-upcase s)))
                      (%fmt-pad stream s (car params) (or (cadr params) #\Space) nil)))
                   (#\C (let ((ch (car args))) (setq args (cdr args))
                          (if at (prin1 ch stream) (write-char ch stream))))
                   (#\F (let ((v (car args)))
                          (setq args (cdr args))
                          (%fmt-pad stream (%fmt-fixed v (cadr params)) (car params) #\Space nil)))
                   (#\$ (let ((v (car args)))
                          (setq args (cdr args))
                          (%fmt-pad stream (%fmt-fixed v (or (car params) 2)) (cadr params) #\Space nil)))
                   (#\E (let ((v (car args))) (setq args (cdr args)) (write-string (princ-to-string (float v)) stream)))
                   (#\P (let ((v (if colon (car prev) (let ((x (car args))) (setq args (cdr args)) x))))
                          (if at
                              (write-string (if (eql v 1) "y" "ies") stream)
                              (unless (eql v 1) (write-char #\s stream)))))
                   (#\% (dotimes (k (or (car params) 1)) (write-char #\Newline stream)))
                   (#\& (fresh-line stream) (dotimes (k (- (or (car params) 1) 1)) (write-char #\Newline stream)))
                   (#\~ (dotimes (k (or (car params) 1)) (write-char #\~ stream)))
                   (#\| (write-char #\Page stream))
                   (#\* (if colon
                            (setq args (nthcdr (- (length args) 1) args))
                            (setq args (nthcdr (or (car params) 1) args))))
                   (#\T (let ((col (%stream-column stream)))
                          (when (and (car params) (< col (car params)))
                            (dotimes (k (- (car params) col)) (write-char #\Space stream)))))
                   (#\Newline (when at (write-char #\Newline stream))
                    (block nil (tagbody sk (when (and (< i n) (whitespace-char-p (char control i))) (setq i (+ i 1)) (go sk)))))
                   (#\^ (when (null args) (return-from run (cons '%fmt-terminate args))))
                   (#\( (let* ((m (%fmt-find-close control i #\( #\))) (body (subseq control i (car m)))
                               (tmp (make-string-output-stream)))
                          (setq args (%format-run tmp body args))
                          (let ((s (get-output-stream-string tmp)))
                            (write-string (cond ((and colon at) (string-upcase s))
                                                (colon (string-capitalize s))
                                                (at (%capitalize-first s))
                                                (t (string-downcase s)))
                                          stream))
                          (setq i (+ (cadr m) 1))))
                   (#\{ (let* ((m (%fmt-find-close control i #\{ #\})) (body (subseq control i (car m)))
                               (body (if (string= body "") (progn (let ((a (car args))) (setq args (cdr args)) a)) body)))
                          (setq i (+ (cadr m) 1))
                          (let ((lst (if at args (car args))))
                            (unless at (setq args (cdr args)))
                            (let ((count 0) (maxn (car params)))
                              (block iter
                                (tagbody it
                                   (when (or (null lst) (and maxn (>= count maxn))) (return-from iter nil))
                                   (if colon
                                       (let ((r (%format-run stream body (car lst))))
                                         (setq lst (cdr lst))
                                         (when (and (consp r) (eq (car r) '%fmt-terminate)) (return-from iter nil)))
                                       (let ((r (%format-run stream body lst)))
                                         (when (and (consp r) (eq (car r) '%fmt-terminate)) (return-from iter nil))
                                         (setq lst r)))
                                   (setq count (+ count 1))
                                   (go it))))
                            (when at (setq args nil)))))
                   (#\[ (let* ((m (%fmt-find-close control i #\[ #\])) (body (subseq control i (car m)))
                               (clauses (%fmt-split-clauses body)))
                          (setq i (+ (cadr m) 1))
                          (let ((sel (cond (at (if (car args) 0 1))
                                           (colon (if (car args) 1 0))
                                           (t (let ((v (or (car params) (car args)))) v)))))
                            (unless (or at (car params)) nil)
                            (unless (car params) (unless at (setq args (cdr args))))
                            (when (and at (car args)) nil)
                            (let ((clause (cond ((and (integerp sel) (< sel (length clauses)) (>= sel 0)) (nth sel clauses))
                                                (t (and (> (length clauses) 0)
                                                        (let ((last (car (last clauses))))
                                                          (and (> (length last) 0) (char= (char last 0) #\:) nil)))))))
                              (when clause
                                (setq args (let ((r (%format-run stream clause args))) r)))))))
                   (t (error "FORMAT: unknown directive ~~~A" dchar)))
                 (setq prev start-args)))
           (go top))))))

(defun %capitalize-first (s)
  (if (= (length s) 0) s (%string-concat (string (char-upcase (char s 0))) (subseq s 1))))

(defun %fmt-commas (s ch)
  (let* ((neg (and (> (length s) 0) (char= (char s 0) #\-)))
         (digits (if neg (subseq s 1) s)) (acc nil) (k 0))
    (dolist (c (reverse (%vector-to-list digits)))
      (when (and (> k 0) (= (mod k 3) 0)) (setq acc (cons ch acc)))
      (setq acc (cons c acc))
      (setq k (+ k 1)))
    (%string-concat (if neg "-" "") (coerce acc 'string))))

(defun %fmt-fixed (v d)
  (let* ((x (float v)) (d (or d 6))
         (scale (expt 10 d))
         (r (round (* (abs x) scale)))
         (int (floor r scale)) (frac (mod r scale))
         (fs (%number-to-string frac)))
    (%string-concat (if (< x 0) "-" "") (%number-to-string int)
                    (if (> d 0) "." "")
                    (if (> d 0) (%string-concat (make-string (- d (length fs)) #\0) fs) ""))))
