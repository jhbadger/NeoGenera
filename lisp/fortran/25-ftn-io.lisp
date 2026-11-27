;;;; 25-io.lisp -- the FORTRAN Tool Kit: formatted, list-directed and unformatted input/output.
;;;;
;;;; A READ, WRITE or PRINT statement is compiled to calls on a statement state (FST): %F7-BEGIN, one %F7-PUT or
;;;; %F7-GET for each data item as the compiled loops reach it, and %F7-END.  The format is a list of edit
;;;; descriptors parsed by F77-PARSE-FORMAT; the state walks it as items arrive (so implied-DO bounds read by the
;;;; same statement work), reverting to the last parenthesised group when the format runs out first.  Units are
;;;; FIOs: the console (units 5 and 6), files held as vectors of records and written back when closed, and internal
;;;; files (CHARACTER variables).  No carriage-control character is interpreted.

(in-package :cl)

;;; ---- number formatting --------------------------------------------------------------------------------------

(defun %f7-exp10 (r)
  "The integer E with 10^(E-1) <= R < 10^E, for a positive rational R."
  (let ((e (+ 1 (values (floor (* (- (integer-length (numerator r)) (integer-length (denominator r))) 0.30103d0))))))
    (loop (if (>= r (expt 10 e)) (setq e (+ e 1)) (return)))
    (loop (if (< r (expt 10 (- e 1))) (setq e (- e 1)) (return)))
    e))

(defun %f7-decompose (r ndig)
  "For a positive rational R: (values N E) with N an NDIG-digit integer and R about 0.N * 10^E, rounded half up."
  (let* ((e (%f7-exp10 r)) (n (values (floor (+ (/ r (expt 10 (- e ndig))) 1/2)))))
    (when (>= n (expt 10 ndig))
      (setq e (+ e 1) n (values (floor (+ (/ r (expt 10 (- e ndig))) 1/2)))))
    (values n e)))

(defun %f7-zeros (n) (make-string (max 0 n) :initial-element #\0))
(defun %f7-stars (w) (make-string w :initial-element #\*))

(defun %f7-field (s w)
  "S right-justified in W columns (all stars when it does not fit; W 0 means as wide as needed)."
  (cond ((= w 0) s)
        ((> (length s) w) (%f7-stars w))
        (t (%string-concat (make-string (- w (length s)) :initial-element #\Space) s))))

(defun %f7-nonfinite (x)
  (cond ((/= x x) "NaN") ((> x 0) "Infinity") (t "-Infinity")))

(defun %f7-fixed-string (r d)
  "The non-negative rational R in fixed point with D decimals."
  (let* ((n (values (floor (+ (* r (expt 10 d)) 1/2)))) (s (format nil "~D" n)))
    (when (<= (length s) d) (setq s (%string-concat (%f7-zeros (- (+ d 1) (length s))) s)))
    (%string-concat (subseq s 0 (- (length s) d)) "." (subseq s (- (length s) d)))))

(defun %f7-sign-string (neg plus nonzero)
  (cond ((and neg nonzero) "-") (plus "+") (t "")))

(defun %f7-fit-real (sign body w)
  "SIGN and BODY (like \"0.25\") in W columns, dropping the leading zero when the field is tight."
  (let ((s (%string-concat sign body)))
    (when (and (> w 0) (> (length s) w) (>= (length body) 2) (string= (subseq body 0 2) "0."))
      (setq s (%string-concat sign (subseq body 1))))
    (%f7-field s w)))

(defun %f7-edit-f (x w d scale plus)
  (let ((x (float x 1d0)))
    (if (not (%f7-finite-p x))
        (%f7-field (%f7-nonfinite x) w)
        (let* ((r (* (rational (abs x)) (expt 10 scale))) (body (%f7-fixed-string r d))
               (nonzero (some (lambda (c) (and (digit-char-p c) (char/= c #\0))) body)))
          (%f7-fit-real (%f7-sign-string (< x 0) plus nonzero) body w)))))

(defun %f7-exponent-string (ex e letter)
  (let ((a (abs ex)) (sign (if (< ex 0) "-" "+")))
    (cond ((and e (> e 0))
           (let ((ds (format nil "~D" a)))
             (if (> (length ds) e) nil (%string-concat (string letter) sign (%f7-zeros (- e (length ds))) ds))))
          ((<= a 99) (%string-concat (string letter) sign (if (< a 10) "0" "") (format nil "~D" a)))
          ((<= a 999) (%string-concat sign (format nil "~D" a)))
          (t nil))))

(defun %f7-edit-e (x w d e scale plus letter)
  (let ((x (float x 1d0)))
    (if (not (%f7-finite-p x))
        (%f7-field (%f7-nonfinite x) w)
        (let* ((ax (abs x)) (sig (if (> scale 0) (+ d 1) (+ d scale))))
          (when (< sig 1) (return-from %f7-edit-e (%f7-stars w)))
          (multiple-value-bind (n ex)
              (if (zerop ax) (values 0 0) (%f7-decompose (rational ax) sig))
            (let* ((ds (let ((s (format nil "~D" n))) (%string-concat (%f7-zeros (- sig (length s))) s)))
                   (mant (if (> scale 0)
                             (%string-concat (subseq ds 0 (min scale (length ds))) "." (subseq ds (min scale (length ds))))
                             (%string-concat "0." (%f7-zeros (- scale)) ds)))
                   (expo (%f7-exponent-string (if (zerop ax) 0 (- ex scale)) e letter)))
              (if (null expo)
                  (%f7-stars w)
                  (%f7-fit-real (%f7-sign-string (< x 0) plus (not (zerop ax))) (%string-concat mant expo) w))))))))

(defun %f7-edit-g (x w d e scale plus)
  (let ((x (float x 1d0)))
    (if (not (%f7-finite-p x))
        (%f7-field (%f7-nonfinite x) w)
        (multiple-value-bind (n ex) (if (zerop x) (values 0 0) (%f7-decompose (rational (abs x)) (max d 1)))
          (declare (ignore n))
          (if (or (zerop x) (and (>= ex 0) (<= ex d)))
              (let ((fw (if (> w 4) (- w (if e (+ e 2) 4)) w)))
                (%string-concat (%f7-edit-f x fw (max 0 (- d ex)) 0 plus) (make-string (if (> w 4) (- w fw) 0) :initial-element #\Space)))
              (%f7-edit-e x w d e scale plus #\E))))))

(defun %f7-edit-i (n w m plus)
  (let* ((ds (format nil "~D" (abs n))))
    (when (and m (= m 0) (zerop n)) (setq ds ""))
    (when (and m (< (length ds) m)) (setq ds (%string-concat (%f7-zeros (- m (length ds))) ds)))
    (%f7-field (%string-concat (%f7-sign-string (< n 0) plus t) ds) w)))

(defun %f7-radix-string (n radix)
  "N (non-negative) written in RADIX with upper-case digits."
  (if (zerop n)
      "0"
      (let ((acc nil))
        (loop (when (zerop n) (return))
              (push (char-upcase (digit-char (rem n radix) radix)) acc)
              (setq n (values (floor n radix))))
        (coerce acc 'string))))

(defun %f7-edit-radix (n w m radix)
  (if (< n 0)
      (%f7-stars (max w 1))
      (let ((ds (%f7-radix-string n radix)))
        (when (and m (< (length ds) m)) (setq ds (%string-concat (%f7-zeros (- m (length ds))) ds)))
        (%f7-field ds w))))

;;; the shortest digits that read back as the same number: list-directed output
(defun %f7-shortest (x single)
  "(values digit-string E) with X = 0.DIGITS * 10^E for a positive finite X; shortest digits at the given precision."
  (let ((r (rational x)))
    (block found
      (do ((nd 1 (+ nd 1))) ((> nd 17))
        (multiple-value-bind (n e) (%f7-decompose r nd)
          (let ((back (%f7-rational-double (* n (expt 10 (- e nd))))))
            (when (= (if single (%f7-single back) back) x)
              (return-from found (values (let ((s (string-right-trim "0" (format nil "~D" n)))) (if (string= s "") "0" s)) e))))))
      (multiple-value-bind (n e) (%f7-decompose r 17)
        (values (string-right-trim "0" (format nil "~D" n)) e)))))

(defun %f7-list-real (x single)
  "X as list-directed output writes a real: 3.14159, 1.0E-5, 2.5D10."
  (let ((x (float x 1d0)))
    (cond ((not (%f7-finite-p x)) (%f7-nonfinite x))
          ((zerop x) "0.0")
          (t
           (multiple-value-bind (ds e) (%f7-shortest (abs x) single)
             (let* ((neg (if (< x 0) "-" "")) (ax (abs x)) (n (length ds)))
               (if (and (>= ax 1d-3) (< ax (if single 1d7 1d15)))
                   (%string-concat neg
                                   (cond ((<= e 0) (%string-concat "0." (%f7-zeros (- e)) ds))
                                         ((>= e n) (%string-concat ds (%f7-zeros (- e n)) ".0"))
                                         (t (%string-concat (subseq ds 0 e) "." (subseq ds e)))))
                   (%string-concat neg (subseq ds 0 1) "." (if (> n 1) (subseq ds 1) "0")
                                   (if single "E" "D") (if (< (- e 1) 0) "-" "+")
                                   (let ((a (abs (- e 1)))) (if (< a 10) (%string-concat "0" (format nil "~D" a)) (format nil "~D" a)))))))))))

;;; ---- parsing numbers in input fields ---------------------------------------------------------------------------

(defun %f7-input-text (text blank)
  "TEXT with its blanks removed (BLANK :NULL) or read as zeros (:ZERO), upper case."
  (string-upcase (if (eq blank :zero)
                     (substitute #\0 #\Space (string-right-trim " " (string-left-trim " " text)))
                     (remove #\Space text))))

(defun %f7-bad-input (what text)
  (%f7-ioerror 1 "Invalid ~A in the input: ~S" what text))

(defun %f7-read-int (text blank)
  (let* ((s (%f7-input-text text blank)) (n (length s)) (i 0) (neg nil))
    (when (= n 0) (return-from %f7-read-int 0))
    (when (member (char s 0) '(#\+ #\-)) (setq neg (char= (char s 0) #\-) i 1))
    (when (or (>= i n) (notevery #'digit-char-p (subseq s i))) (%f7-bad-input "integer" text))
    (let ((v (parse-integer s :start i))) (if neg (- v) v))))

(defun %f7-read-real (text d blank)
  "The double written in TEXT with the F/E/D/G rules (D implied decimals when there is no decimal point)."
  (let* ((s (%f7-input-text text blank)) (n (length s)) (i 0) (neg nil) (idig nil) (fdig nil) (dot nil) (ex 0))
    (when (= n 0) (return-from %f7-read-real 0d0))
    (when (member (char s 0) '(#\+ #\-)) (setq neg (char= (char s 0) #\-) i 1))
    (let ((st i))
      (loop (when (or (>= i n) (not (digit-char-p (char s i)))) (return)) (setq i (+ i 1)))
      (setq idig (subseq s st i)))
    (when (and (< i n) (char= (char s i) #\.))
      (setq dot t i (+ i 1))
      (let ((st i))
        (loop (when (or (>= i n) (not (digit-char-p (char s i)))) (return)) (setq i (+ i 1)))
        (setq fdig (subseq s st i))))
    (when (and (string= idig "") (or (null fdig) (string= fdig ""))) (%f7-bad-input "real number" text))
    (when (< i n)
      (let ((c (char s i)))
        (cond ((member c '(#\E #\D)) (setq i (+ i 1))))
        (let ((sgn 1))
          (when (and (< i n) (member (char s i) '(#\+ #\-))) (setq sgn (if (char= (char s i) #\-) -1 1) i (+ i 1)))
          (when (or (>= i n) (notevery #'digit-char-p (subseq s i))) (%f7-bad-input "real number" text))
          (setq ex (* sgn (parse-integer s :start i)) i n))))
    (let* ((digits (%string-concat idig (or fdig "")))
           (scale (+ (if fdig (length fdig) 0) (if (and (not dot) d) (or d 0) 0)))
           (v (* (parse-integer (if (string= digits "") "0" digits)) (expt 10 (- ex scale)))))
      (let ((f (%f7-rational-double v))) (if neg (- f) f)))))

(defun %f7-read-logical (text)
  (let* ((s (string-upcase (string-trim " " text))) (s (if (and (> (length s) 0) (char= (char s 0) #\.)) (subseq s 1) s)))
    (cond ((= (length s) 0) (%f7-bad-input "logical value" text))
          ((char= (char s 0) #\T) t)
          ((char= (char s 0) #\F) nil)
          (t (%f7-bad-input "logical value" text)))))

(defun %f7-read-radix (text radix)
  (let ((s (remove #\Space (string-upcase text))))
    (if (and (> (length s) 0) (every (lambda (c) (digit-char-p c radix)) s)) (parse-integer s :radix radix)
        (if (= (length s) 0) 0 (%f7-bad-input "number" text)))))

;;; ---- format specifications --------------------------------------------------------------------------------------
;;; An item is (:I rep w m) (:F rep w d) (:E rep w d e) (:D rep w d) (:G rep w d e) (:A rep w) (:L rep w) (:Z rep w m)
;;; (:O rep w m) (:B rep w m) (:X n) (:T n) (:TL n) (:TR n) (:SLASH n) (:COLON) (:STR text) (:P k) (:S flag)
;;; (:BLANK :null/:zero) or (:GROUP rep items).

(defvar *f77-format-cache* (make-hash-table :test 'equal))

(defun %f7-format-error (s msg)
  (error 'f77-syntax-error :message (format nil "FORMAT ~A: ~A" s msg)))

(defun %f7-parse-format-text (s)
  "Parse the squeezed format text S, \"(...)\"."
  (let ((i 0) (n (length s)))
    (labels ((err (msg) (%f7-format-error s msg))
             (peek () (if (< i n) (char s i) nil))
             (num ()
               (let ((st i))
                 (loop (when (or (>= i n) (not (digit-char-p (char s i)))) (return)) (setq i (+ i 1)))
                 (if (> i st)
                     (let ((v (parse-integer s :start st :end i)))
                       (when (> v 1000000) (err "a number in the format is too large"))
                       v)
                     nil)))
             (two (a b) (and (< (+ i 1) n) (char= (char s i) a) (char= (char s (+ i 1)) b)))
             (descriptor (rep)
               (let ((c (char s i)))
                 (cond
                   ((two #\T #\L) (setq i (+ i 2)) (list :tl (or (num) (err "TL needs a column count"))))
                   ((two #\T #\R) (setq i (+ i 2)) (list :tr (or (num) (err "TR needs a column count"))))
                   ((two #\S #\P) (setq i (+ i 2)) (list :s :plus))
                   ((two #\S #\S) (setq i (+ i 2)) (list :s :none))
                   ((two #\B #\N) (setq i (+ i 2)) (list :blank :null))
                   ((two #\B #\Z) (setq i (+ i 2)) (list :blank :zero))
                   ((char= c #\S) (setq i (+ i 1)) (list :s :none))
                   ((char= c #\X) (setq i (+ i 1)) (list :x (or rep 1)))
                   ((char= c #\P) (setq i (+ i 1)) (list :p (or rep 0)))
                   ((char= c #\T) (setq i (+ i 1)) (list :t (or (num) (err "T needs a column"))))
                   ((member c '(#\I #\Z #\O #\B))
                    (setq i (+ i 1))
                    (let ((w (or (num) (err "The descriptor needs a width"))) (m nil))
                      (when (eql (peek) #\.) (setq i (+ i 1)) (setq m (or (num) (err "A digit count was expected"))))
                      (list (cond ((char= c #\I) :i) ((char= c #\Z) :z) ((char= c #\O) :o) (t :b)) (or rep 1) w m)))
                   ((member c '(#\F #\E #\D #\G))
                    (setq i (+ i 1))
                    (let ((w (or (num) (err "The descriptor needs a width"))) (d nil) (e nil))
                      (unless (eql (peek) #\.) (if (char= c #\G) (setq d 0) (err "The descriptor needs .d")))
                      (when (eql (peek) #\.) (setq i (+ i 1)) (setq d (or (num) (err "A digit count was expected"))))
                      (when (and (member c '(#\E #\G)) (eql (peek) #\E)) (setq i (+ i 1)) (setq e (or (num) (err "An exponent width was expected"))))
                      (list (cond ((char= c #\F) :f) ((char= c #\E) :e) ((char= c #\D) :d) (t :g)) (or rep 1) w d e)))
                   ((char= c #\A) (setq i (+ i 1)) (list :a (or rep 1) (num)))
                   ((char= c #\L) (setq i (+ i 1)) (list :l (or rep 1) (or (num) (err "L needs a width"))))
                   (t (err (format nil "unknown edit descriptor ~A" c))))))
             (items ()
               (let ((acc nil))
                 (loop
                   (let ((c (peek)))
                     (cond
                       ((null c) (err "missing )"))
                       ((char= c #\)) (setq i (+ i 1)) (return))
                       ((char= c #\,) (setq i (+ i 1)))
                       ((char= c #\')
                        (let ((buf (make-string-output-stream)))
                          (setq i (+ i 1))
                          (loop
                            (when (>= i n) (err "unterminated string"))
                            (let ((d (char s i)))
                              (cond ((and (char= d #\') (< (+ i 1) n) (char= (char s (+ i 1)) #\')) (write-char #\' buf) (setq i (+ i 2)))
                                    ((char= d #\') (setq i (+ i 1)) (return))
                                    (t (write-char d buf) (setq i (+ i 1))))))
                          (push (list :str (get-output-stream-string buf)) acc)))
                       ((char= c #\/) (setq i (+ i 1)) (push (list :slash 1) acc))
                       ((char= c #\:) (setq i (+ i 1)) (push (list :colon) acc))
                       ((char= c #\$) (setq i (+ i 1)))
                       ((char= c #\() (setq i (+ i 1)) (push (list :group 1 (items)) acc))
                       ((or (digit-char-p c) (char= c #\+) (char= c #\-))
                        (let ((neg nil) (v nil))
                          (when (member c '(#\+ #\-)) (setq neg (char= c #\-)) (setq i (+ i 1)))
                          (setq v (or (num) (err "a number was expected")))
                          (when neg (setq v (- v)))
                          (let ((c2 (peek)))
                            (cond ((null c2) (err "missing )"))
                                  ((char= c2 #\() (setq i (+ i 1)) (push (list :group v (items)) acc))
                                  ((char= c2 #\/) (setq i (+ i 1)) (push (list :slash v) acc))
                                  (t (push (descriptor v) acc))))))
                       ((alpha-char-p c) (push (descriptor nil) acc))
                       (t (err (format nil "unexpected ~A" c))))))
                 (nreverse acc))))
      (unless (and (> n 0) (char= (char s 0) #\()) (err "a format must start with a left parenthesis"))
      (setq i 1)
      (let ((result (items)))
        (when (< i n) (err "unexpected text after the closing parenthesis"))
        result))))

(defun f77-parse-format (text)
  "The edit descriptor list for the FORMAT specification TEXT, a string such as \"(1X,I5,F8.3)\" (blanks and case are
not significant).  Signals F77-SYNTAX-ERROR when it is malformed."
  (or (gethash text *f77-format-cache*)
      (setf (gethash text *f77-format-cache*) (%f7-parse-format-text (%f7-squeeze text)))))

(defun %f7-runtime-format (text)
  "The parsed format for TEXT at run time: a malformed format is an I/O error."
  (let ((text (%f7-str text "format")))
    (handler-case (f77-parse-format text)
      (f77-syntax-error (c) (%f7-ioerror 1 "~A" (f77-error-message c))))))

(defun %f7-last-group (items)
  "The tail of ITEMS starting at its last group (to revert to), or ITEMS itself."
  (let ((tail nil))
    (do ((l items (cdr l))) ((null l))
      (when (eq (car (car l)) :group) (setq tail l)))
    (or tail items)))

;;; ---- units ----------------------------------------------------------------------------------------------------------

(defstruct (fio (:constructor %make-fio) (:copier nil))
  unit name kind (form :formatted) (access :sequential) status (blank :null) recl
  (recs (make-array 16)) (nrec 0) (pos 0) path owned dirty stream reader setter len)

(defmethod print-object ((io fio) s)
  (format s "#<FORTRAN unit ~A~@[ ~A~]>" (fio-unit io) (fio-name io)))

(defun %f7-recs-get (io i) (svref (fio-recs io) i))
(defun %f7-recs-set (io i rec)
  (when (>= i (length (fio-recs io)))
    (let ((new (make-array (max (+ i 1) (* 2 (length (fio-recs io)))))))
      (replace new (fio-recs io))
      (setf (fio-recs io) new)))
  (when (> i (fio-nrec io))
    (dotimes (k (- i (fio-nrec io)))
      (setf (svref (fio-recs io) (+ (fio-nrec io) k)) (if (eq (fio-form io) :formatted) "" nil))))
  (setf (svref (fio-recs io) i) rec)
  (when (>= i (fio-nrec io)) (setf (fio-nrec io) (+ i 1))))

(defun %f7-eof (io)
  (%f7-ioerror -1 "End of file on FORTRAN unit ~A" (fio-unit io)))

(defun %f7-console-reader (env)
  (let ((in (fenv-input env)))
    (cond ((functionp in) in)
          ((streamp in) (lambda () (read-line in nil :eof)))
          ((stringp in) (let ((s (make-string-input-stream in))) (lambda () (read-line s nil :eof))))
          (t (%pascal-terminal-reader)))))

(defun %f7-standard-unit (n env)
  (cond ((eql n 5) (%make-fio :unit 5 :name "INPUT" :kind :console-in :reader (%f7-console-reader env)))
        ((member n '(6 0)) (%make-fio :unit n :name "OUTPUT" :kind :console-out :stream (fenv-output env)))
        (t nil)))

(defun %f7-resolve-path (name)
  (let ((name (%expand-home name)))
    (if (and (> (length name) 0) (char= (char name 0) #\/)) name (%string-concat (or *f77-pathname-defaults* "") name))))

(defun %f7-load-records (io)
  (let ((path (fio-path io)))
    (if (eq (fio-form io) :formatted)
        (dolist (line (%split-string (%read-file-string path) #\Newline))
          (%f7-recs-set io (fio-nrec io) line))
        (with-open-file (s path)
          (loop (let ((x (read s nil :eof))) (when (eq x :eof) (return)) (%f7-recs-set io (fio-nrec io) x)))))
    ;; a trailing newline leaves one empty line that is not a record
    (when (and (eq (fio-form io) :formatted) (> (fio-nrec io) 0) (string= (%f7-recs-get io (- (fio-nrec io) 1)) ""))
      (setf (fio-nrec io) (- (fio-nrec io) 1)))))

(defun %f7-unit (n)
  "The open unit N, connecting the standard units and an unnamed file fort.N on first use."
  (unless (integerp n) (%f7-ioerror 2 "~S is not a unit number" n))
  (let* ((env (%f7-env)) (io (gethash n (fenv-units env))))
    (or io
        (let ((std (%f7-standard-unit n env)))
          (if std
              (setf (gethash n (fenv-units env)) std)
              (progn (%f7-open n :file (format nil "fort.~D" n) :status :unknown)
                     (gethash n (fenv-units env))))))))

(defun %f7-flush (io)
  (when (and (fio-dirty io) (fio-path io) (eq (fio-kind io) :file))
    (with-open-file (s (fio-path io) :direction :output :if-exists :supersede :if-does-not-exist :create)
      (dotimes (i (fio-nrec io))
        (if (eq (fio-form io) :formatted)
            (write-line (%f7-recs-get io i) s)
            (progn (prin1 (%f7-recs-get io i) s) (terpri s)))))
    (setf (fio-dirty io) nil)))

(defun %f7-keyword (x what)
  (cond ((null x) nil)
        ((keywordp x) x)
        ((stringp x) (intern (string-upcase (string-trim " " x)) "KEYWORD"))
        (t (%f7-ioerror 2 "Bad ~A specifier ~S" what x))))

(defun %f7-close (n &optional status)
  (let* ((env (%f7-env)) (io (gethash n (fenv-units env))))
    (when io
      (%f7-flush io)
      (when (and (eq (%f7-keyword status "STATUS") :delete) (fio-path io) (probe-file (fio-path io))) (delete-file (fio-path io)))
      (remhash n (fenv-units env)))
    nil))

(defun %f7-close-all ()
  (let ((env (%f7-env)) (units nil))
    (maphash (lambda (k io) (declare (ignore io)) (push k units)) (fenv-units env))
    (dolist (n units) (let ((io (gethash n (fenv-units env)))) (when io (ignore-errors (%f7-flush io)))))
    (clrhash (fenv-units env))))

(defun %f7-open (unit &key file status access form recl blank position action)
  "OPEN: connect UNIT to FILE.  STATUS is OLD, NEW, SCRATCH, REPLACE or UNKNOWN; ACCESS SEQUENTIAL or DIRECT; FORM
FORMATTED or UNFORMATTED."
  (declare (ignore position action))
  (unless (integerp unit) (%f7-ioerror 2 "~S is not a unit number" unit))
  (let* ((env (%f7-env)) (status (or (%f7-keyword status "STATUS") :unknown)) (access (or (%f7-keyword access "ACCESS") :sequential))
         (form (or (%f7-keyword form "FORM") (if (eq access :direct) :unformatted :formatted)))
         (blank (or (%f7-keyword blank "BLANK") :null))
         (name (and file (%f7-str file "file name"))))
    (when (gethash unit (fenv-units env)) (%f7-close unit))
    (let* ((stream-entry (and (fenv-streams env)
                              (or (assoc unit (fenv-streams env))
                                  (and name (assoc name (fenv-streams env) :test #'equal))
                                  (and name (assoc name (fenv-streams env) :test #'string-equal)))))
           (path (cond ((eq status :scratch) nil) (name (%f7-resolve-path (string-right-trim " " name)))
                       (t (%f7-resolve-path (format nil "fort.~D" unit)))))
           (io (%make-fio :unit unit :name (or name (format nil "fort.~D" unit)) :kind :file :form form :access access :status status
                          :blank blank :recl recl :path path :owned t)))
      (cond
        (stream-entry
         (let ((s (cdr stream-entry)))
           (if (and (streamp s) (output-stream-p s) (not (input-stream-p s)))
               (setf (fio-kind io) :console-out (fio-stream io) s (fio-path io) nil)
               (let ((st (if (streamp s) s (make-string-input-stream (princ-to-string s)))))
                 (loop (let ((l (read-line st nil :eof))) (when (eq l :eof) (return)) (%f7-recs-set io (fio-nrec io) l)))))))
        ((eq status :scratch) nil)
        (t
         (let ((exists (and path (probe-file path))))
           (case status
             (:old (unless exists (%f7-ioerror 29 "OPEN: the file ~A does not exist (STATUS='OLD')" path)))
             (:new (when exists (%f7-ioerror 17 "OPEN: the file ~A already exists (STATUS='NEW')" path)))
             (t nil))
           (when (and exists (not (member status '(:replace :new))))
             (%f7-load-records io)))))
      (setf (gethash unit (fenv-units env)) io)
      nil)))

(defun %f7-inquire (unit file)
  "INQUIRE: a plist of the properties of UNIT (an integer) or FILE (a string)."
  (let* ((env (%f7-env))
         (io (cond (unit (or (gethash unit (fenv-units env)) (%f7-standard-peek unit env)))
                   (file (let ((found nil) (path (%f7-resolve-path (string-right-trim " " file))))
                           (maphash (lambda (k v) (declare (ignore k)) (when (equal (fio-path v) path) (setq found v))) (fenv-units env))
                           found))))
         (path (cond (io (fio-path io)) (file (%f7-resolve-path (string-right-trim " " file))) (t nil))))
    (list :exist (if file (and (probe-file path) t) (if unit (and (integerp unit) (>= unit 0) t) nil))
          :opened (and io t)
          :number (and io (fio-unit io))
          :named (and io (fio-name io) t)
          :name (and io (fio-name io))
          :access (and io (string-upcase (symbol-name (fio-access io))))
          :sequential (if io (if (eq (fio-access io) :sequential) "YES" "NO") "UNKNOWN")
          :direct (if io (if (eq (fio-access io) :direct) "YES" "NO") "UNKNOWN")
          :form (and io (string-upcase (symbol-name (fio-form io))))
          :formatted (if io (if (eq (fio-form io) :formatted) "YES" "NO") "UNKNOWN")
          :unformatted (if io (if (eq (fio-form io) :unformatted) "YES" "NO") "UNKNOWN")
          :recl (and io (fio-recl io))
          :nextrec (and io (+ 1 (fio-pos io)))
          :blank (and io (if (eq (fio-blank io) :zero) "ZERO" "NULL")))))

(defun %f7-standard-peek (n env)
  (and (member n '(5 6 0)) (%f7-standard-unit n env)))

(defun %f7-rewind (unit)
  (let ((io (%f7-unit unit))) (setf (fio-pos io) 0) nil))
(defun %f7-backspace (unit)
  (let ((io (%f7-unit unit))) (setf (fio-pos io) (max 0 (- (fio-pos io) 1))) nil))
(defun %f7-endfile (unit)
  (let ((io (%f7-unit unit))) (setf (fio-nrec io) (fio-pos io) (fio-dirty io) t) nil))

;;; ---- records ----------------------------------------------------------------------------------------------------------

(defun %f7-next-record (io)
  "The next record of IO for reading: a string (formatted) or a list of values (unformatted)."
  (case (fio-kind io)
    (:console-in (let ((l (funcall (fio-reader io)))) (if (eq l :eof) (%f7-eof io) l)))
    ((:file :internal-in)
     (let ((p (fio-pos io)))
       (when (>= p (fio-nrec io)) (%f7-eof io))
       (setf (fio-pos io) (+ p 1))
       (%f7-recs-get io p)))
    (t (%f7-ioerror 5 "FORTRAN unit ~A is not connected for reading" (fio-unit io)))))

(defun %f7-write-record (io rec)
  (case (fio-kind io)
    (:console-out (write-line rec (or (fio-stream io) *standard-output*)))
    (:file
     (let ((p (fio-pos io)))
       (%f7-recs-set io p rec)
       (when (eq (fio-access io) :sequential) (setf (fio-nrec io) (+ p 1)))
       (setf (fio-pos io) (+ p 1) (fio-dirty io) t)))
    (:internal-out (funcall (fio-setter io) (%f7-cfit rec (fio-len io))))
    (t (%f7-ioerror 5 "FORTRAN unit ~A is not connected for writing" (fio-unit io)))))

;;; ---- the statement state ---------------------------------------------------------------------------------------------

(defstruct (ffr (:constructor %make-ffr (rest orig reps)) (:copier nil)) rest orig reps)

(defstruct (fst (:constructor %make-fst) (:copier nil))
  io mode kind items stack revert (buf "") (col 0) (scale 0) (plus nil) (blank :null)
  (since-rev 0) vals (after-value nil) (rep-left 0) (rep-val nil) (done nil) (started nil))

(defun %f7-begin (mode unit fmt &optional rec)
  "Start a READ (MODE :READ) or WRITE (:WRITE).  UNIT is an integer, :STAR, or (:INTERNAL string . setter); FMT is NIL
for list-directed, :UNFORMATTED, or a parsed format; REC is a direct-access record number."
  (let* ((io (cond ((consp unit)
                    (if (eq mode :read)
                        (let ((io (%make-fio :unit "internal" :kind :internal-in :name "internal file")))
                          (%f7-recs-set io 0 (%f7-str (cadr unit) "internal file")) io)
                        (%make-fio :unit "internal" :kind :internal-out :name "internal file" :setter (cddr unit) :len (cadr unit))))
                   (t (%f7-unit (if (eq unit :star) (if (eq mode :read) 5 6) unit)))))
         (kind (cond ((eq fmt :unformatted) :unf) ((null fmt) :list) (t :fmt)))
         (st (%make-fst :io io :mode mode :kind kind :items (and (eq kind :fmt) fmt) :blank (fio-blank io))))
    (when (and (eq kind :unf) (eq (fio-form io) :formatted) (eq (fio-kind io) :file))
      (%f7-ioerror 3 "Unformatted input/output on the formatted unit ~A" (fio-unit io)))
    (when (and (not (eq kind :unf)) (eq (fio-form io) :unformatted) (eq (fio-kind io) :file))
      (%f7-ioerror 3 "Formatted input/output on the unformatted unit ~A" (fio-unit io)))
    (when rec
      (unless (and (integerp rec) (>= rec 1)) (%f7-ioerror 4 "Bad record number ~S" rec))
      (setf (fio-pos io) (- rec 1)))
    (when (eq kind :fmt)
      (setf (fst-revert st) (%f7-last-group fmt))
      (setf (fst-stack st) (list (%make-ffr fmt fmt 1))))
    (when (eq mode :read)
      (let ((r (%f7-next-record io)))
        (if (eq kind :unf) (setf (fst-vals st) (copy-list r)) (setf (fst-buf st) r))))
    st))

(defun %f7-next-desc (st)
  "The next primitive edit descriptor of the format (data descriptors with repeat 1), or :END at the end of the format."
  (loop
    (let ((fr (car (fst-stack st))))
      (cond ((null fr) (return :end))
            ((null (ffr-rest fr))
             (if (> (ffr-reps fr) 1)
                 (setf (ffr-reps fr) (- (ffr-reps fr) 1) (ffr-rest fr) (ffr-orig fr))
                 (setf (fst-stack st) (cdr (fst-stack st)))))
            (t (let ((item (car (ffr-rest fr))))
                 (setf (ffr-rest fr) (cdr (ffr-rest fr)))
                 (cond ((eq (car item) :group)
                        (when (> (cadr item) 0) (push (%make-ffr (caddr item) (caddr item) (cadr item)) (fst-stack st))))
                       ((and (member (car item) '(:i :f :e :d :g :a :l :z :o :b)) (> (cadr item) 1))
                        (setf (ffr-rest fr) (cons (list* (car item) (- (cadr item) 1) (cddr item)) (ffr-rest fr)))
                        (return (list* (car item) 1 (cddr item))))
                       (t (return item)))))))))

(defun %f7-revert (st)
  "The format ran out with data items left: start the last group again on a new record."
  (when (= (fst-since-rev st) 0) (%f7-ioerror 6 "The format has no data edit descriptor for the items of the statement"))
  (setf (fst-since-rev st) 0)
  (if (eq (fst-mode st) :write) (%f7-emit st) (%f7-advance st))
  (setf (fst-stack st) (list (%make-ffr (fst-revert st) (fst-revert st) 1))))

(defun %f7-emit (st)
  (%f7-write-record (fst-io st) (fst-buf st))
  (setf (fst-buf st) "" (fst-col st) 0))

(defun %f7-advance (st)
  (setf (fst-buf st) (%f7-next-record (fst-io st)) (fst-col st) 0))

(defun %f7-out-text (st text)
  (let* ((col (fst-col st)) (buf (fst-buf st)) (end (+ col (length text))))
    (when (> col (length buf)) (setq buf (%string-concat buf (make-string (- col (length buf)) :initial-element #\Space))))
    (setf (fst-buf st) (%string-concat (subseq buf 0 col) text (if (< end (length buf)) (subseq buf end) ""))
          (fst-col st) end)))

(defun %f7-control (st d)
  "Carry out a non-data edit descriptor D for the statement ST."
  (let ((w (eq (fst-mode st) :write)))
    (case (car d)
      (:str (if w (%f7-out-text st (cadr d)) (setf (fst-col st) (+ (fst-col st) (length (cadr d))))))
      (:x (setf (fst-col st) (+ (fst-col st) (cadr d))))
      (:t (setf (fst-col st) (max 0 (- (cadr d) 1))))
      (:tl (setf (fst-col st) (max 0 (- (fst-col st) (cadr d)))))
      (:tr (setf (fst-col st) (+ (fst-col st) (cadr d))))
      (:slash (dotimes (k (cadr d)) (if w (%f7-emit st) (%f7-advance st))))
      (:p (setf (fst-scale st) (cadr d)))
      (:s (setf (fst-plus st) (eq (cadr d) :plus)))
      (:blank (setf (fst-blank st) (cadr d)))
      (:colon nil))))

(defun %f7-data-desc-p (d) (member (car d) '(:i :f :e :d :g :a :l :z :o :b)))

;;; ---- output ------------------------------------------------------------------------------------------------------------

(defun %f7-format-value (st d value kind)
  "The text a data descriptor D gives to VALUE."
  (let ((w (caddr d)) (plus (fst-plus st)) (scale (fst-scale st)))
    (ecase (car d)
      (:i (unless (integerp value) (%f7-ioerror 7 "Edit descriptor I needs an INTEGER value, not ~A" (%f7-describe-kind value kind)))
       (%f7-edit-i value w (cadddr d) plus))
      ((:z :o :b)
       (unless (integerp value) (%f7-ioerror 7 "Edit descriptor ~A needs an INTEGER value" (car d)))
       (%f7-edit-radix value w (cadddr d) (case (car d) (:z 16) (:o 8) (t 2))))
      ((:f :e :d :g)
       (unless (realp value) (%f7-ioerror 7 "Edit descriptor ~A needs a REAL value, not ~A" (car d) (%f7-describe-kind value kind)))
       (let ((x (float value 1d0)) (dd (cadddr d)) (e (car (cddddr d))))
         (ecase (car d)
           (:f (%f7-edit-f x w dd scale plus))
           (:e (%f7-edit-e x w dd e scale plus #\E))
           (:d (%f7-edit-e x w dd e scale plus #\D))
           (:g (%f7-edit-g x w dd e scale plus)))))
      (:a (unless (stringp value) (%f7-ioerror 7 "Edit descriptor A needs a CHARACTER value, not ~A" (%f7-describe-kind value kind)))
       (cond ((null w) value)
             ((>= w (length value)) (%f7-field value w))
             (t (subseq value 0 w))))
      (:l (unless (or (eq value t) (null value)) (%f7-ioerror 7 "Edit descriptor L needs a LOGICAL value, not ~A" (%f7-describe-kind value kind)))
       (%f7-field (if value "T" "F") w)))))

(defun %f7-describe-kind (value kind)
  (declare (ignore kind))
  (cond ((integerp value) "an INTEGER") ((floatp value) "a REAL") ((stringp value) "a CHARACTER string")
        ((or (eq value t) (null value)) "a LOGICAL") ((complexp value) "a COMPLEX") (t (format nil "~S" value))))

(defun %f7-ld-text (value kind)
  "VALUE as a list-directed output item."
  (cond ((integerp value) (format nil "~D" value))
        ((stringp value) value)
        ((or (eq value t) (null value)) (if value "T" "F"))
        ((complexp value) (%string-concat "(" (%f7-list-real (realpart value) (eq kind :complex)) "," (%f7-list-real (imagpart value) (eq kind :complex)) ")"))
        ((floatp value) (%f7-list-real value (eq kind :real)))
        ((rationalp value) (%f7-list-real (float value 1d0) (eq kind :real)))
        (t (%f7-rerror "Cannot write ~S" value))))

(defun %f7-put (st value kind)
  "Output VALUE, an item of KIND (:INTEGER :REAL :DOUBLE :COMPLEX :DCOMPLEX :LOGICAL :CHARACTER)."
  (when (eq value 'f77-undefined) (%f7-undef "value written"))
  (case (fst-kind st)
    (:list
     (let ((text (%f7-ld-text value kind)))
       (%f7-out-text st (%string-concat " " text))))
    (:unf (setf (fst-vals st) (cons value (fst-vals st))))
    (t
     (if (complexp value)
         (progn (%f7-put-fmt st (realpart value) kind) (%f7-put-fmt st (imagpart value) kind))
         (%f7-put-fmt st value kind)))))

(defun %f7-put-fmt (st value kind)
  (loop
    (let ((d (%f7-next-desc st)))
      (cond ((eq d :end) (%f7-revert st))
            ((%f7-data-desc-p d)
             (%f7-out-text st (%f7-format-value st d value kind))
             (setf (fst-since-rev st) (+ 1 (fst-since-rev st)))
             (return))
            (t (%f7-control st d))))))

;;; ---- input ----------------------------------------------------------------------------------------------------------------

(defun %f7-field-text (st w)
  "The W characters of the record at the current column (blank padded), advancing the column."
  (let* ((buf (fst-buf st)) (col (fst-col st)) (end (+ col w))
         (s (cond ((>= col (length buf)) "") (t (subseq buf col (min end (length buf)))))))
    (setf (fst-col st) end)
    (if (< (length s) w) (%string-concat s (make-string (- w (length s)) :initial-element #\Space)) s)))

(defun %f7-convert-input (kind v)
  (case kind ((:real) (%f7-single v)) ((:complex) v) (t v)))

(defun %f7-get (st kind setter &optional len)
  "Input an item of KIND, giving the value to SETTER; LEN is the length of a CHARACTER item."
  (case (fst-kind st)
    (:list (%f7-ld-get st kind setter len))
    (:unf
     (when (null (fst-vals st)) (%f7-ioerror 8 "The unformatted record has no more values"))
     (let ((v (pop (fst-vals st))))
       (cond ((and (eq kind :character)) (unless (stringp v) (%f7-ioerror 7 "The record holds ~S where a CHARACTER value is read" v)) (funcall setter (%f7-cfit v len)))
             ((and (member kind '(:real :double :integer :complex :dcomplex)) (not (numberp v))) (%f7-ioerror 7 "The record holds ~S where a number is read" v))
             (t (funcall setter (if (eq kind :real) (%f7-single v) v))))))
    (t
     (if (member kind '(:complex :dcomplex))
         (let ((re nil) (im nil))
           (%f7-get-fmt st :real (lambda (v) (setq re v)) nil)
           (%f7-get-fmt st :real (lambda (v) (setq im v)) nil)
           (funcall setter (let ((z (complex re im))) (if (eq kind :complex) (%f7-cxs z) z))))
         (%f7-get-fmt st kind setter len)))))

(defun %f7-get-fmt (st kind setter len)
  (loop
    (let ((d (%f7-next-desc st)))
      (cond
        ((eq d :end) (%f7-revert st))
        ((%f7-data-desc-p d)
         (setf (fst-since-rev st) (+ 1 (fst-since-rev st)))
         (let ((blank (fst-blank st)) (w (caddr d)))
           (ecase (car d)
             (:i (unless (member kind '(:integer)) (%f7-ioerror 7 "Edit descriptor I cannot read a ~(~A~) item" kind))
              (funcall setter (%f7-read-int (%f7-field-text st w) blank)))
             ((:z :o :b)
              (unless (eq kind :integer) (%f7-ioerror 7 "Edit descriptor ~A cannot read a ~(~A~) item" (car d) kind))
              (funcall setter (%f7-read-radix (%f7-field-text st w) (case (car d) (:z 16) (:o 8) (t 2)))))
             ((:f :e :d :g)
              (unless (member kind '(:real :double)) (%f7-ioerror 7 "Edit descriptor ~A cannot read a ~(~A~) item" (car d) kind))
              (funcall setter (%f7-convert-input kind (%f7-read-real (%f7-field-text st w) (cadddr d) blank))))
             (:a (unless (eq kind :character) (%f7-ioerror 7 "Edit descriptor A cannot read a ~(~A~) item" kind))
              (let* ((n (or w len)) (text (%f7-field-text st n)))
                (funcall setter (cond ((= n len) text)
                                      ((> n len) (subseq text (- n len)))
                                      (t (%f7-cfit text len))))))
             (:l (unless (eq kind :logical) (%f7-ioerror 7 "Edit descriptor L cannot read a ~(~A~) item" kind))
              (funcall setter (%f7-read-logical (%f7-field-text st w))))))
         (return))
        (t (%f7-control st d))))))

;;; ---- list-directed input --------------------------------------------------------------------------------------------------

(defun %f7-ld-blank-p (c) (member c '(#\Space #\Tab #\Return)))

(defun %f7-ld-skip (st)
  "Skip blanks and record ends to the next character of the data; NIL when only blanks remain on the record."
  (loop
    (let ((buf (fst-buf st)) (col (fst-col st)))
      (cond ((>= col (length buf)) (return nil))
            ((%f7-ld-blank-p (char buf col)) (setf (fst-col st) (+ col 1)))
            (t (return t))))))

(defun %f7-ld-token (st)
  "Read the next token of a list-directed record: (values token quoted-p)."
  (let* ((buf (fst-buf st)) (col (fst-col st)) (n (length buf)) (c (char buf col)))
    (cond
      ((or (char= c #\') (char= c #\"))
       (let ((out (make-string-output-stream)) (i (+ col 1)))
         (loop
           (when (>= i n) (return))
           (let ((d (char buf i)))
             (cond ((and (char= d c) (< (+ i 1) n) (char= (char buf (+ i 1)) c)) (write-char c out) (setq i (+ i 2)))
                   ((char= d c) (setq i (+ i 1)) (return))
                   (t (write-char d out) (setq i (+ i 1))))))
         (setf (fst-col st) i)
         (values (get-output-stream-string out) t)))
      ((char= c #\()
       (let ((e (position #\) buf :start col)))
         (unless e (%f7-ioerror 9 "Unterminated complex constant in list-directed input"))
         (setf (fst-col st) (+ e 1))
         (values (subseq buf col (+ e 1)) nil)))
      (t
       (let ((i col))
         (loop (when (or (>= i n) (%f7-ld-blank-p (char buf i)) (member (char buf i) '(#\, #\/))) (return)) (setq i (+ i 1)))
         (setf (fst-col st) i)
         (values (subseq buf col i) nil))))))

(defun %f7-ld-convert (token quoted kind len)
  (declare (ignorable quoted))
  (case kind
    (:integer (let ((s (string-upcase token)))
                (unless (and (> (length s) 0) (let ((b (if (member (char s 0) '(#\+ #\-)) 1 0))) (and (< b (length s)) (every #'digit-char-p (subseq s b)))))
                  (%f7-bad-input "integer" token))
                (parse-integer s)))
    ((:real :double) (let ((v (%f7-read-real token nil :null))) (if (eq kind :real) (%f7-single v) v)))
    ((:complex :dcomplex)
     (let* ((s (string-trim "()" token)) (comma (position #\, s)))
       (unless comma (%f7-bad-input "complex number" token))
       (let ((z (complex (%f7-read-real (subseq s 0 comma) nil :null) (%f7-read-real (subseq s (+ comma 1)) nil :null))))
         (if (eq kind :complex) (%f7-cxs z) z))))
    (:logical (%f7-read-logical token))
    (:character (%f7-cfit token len))
    (t token)))

(defun %f7-ld-get (st kind setter len)
  (when (fst-done st) (return-from %f7-ld-get nil))
  (when (> (fst-rep-left st) 0)
    (setf (fst-rep-left st) (- (fst-rep-left st) 1))
    (unless (eq (fst-rep-val st) :null) (funcall setter (fst-rep-val st)))
    (return-from %f7-ld-get nil))
  (loop
    ;; find the next character of data, going on to later records
    (loop
      (if (%f7-ld-skip st) (return)
          (progn (setf (fst-buf st) (%f7-next-record (fst-io st)) (fst-col st) 0))))
    (let* ((buf (fst-buf st)) (c (char buf (fst-col st))))
      (cond
        ((char= c #\/) (setf (fst-done st) t) (return))
        ((char= c #\,)
         (setf (fst-col st) (+ 1 (fst-col st)))
         (if (fst-after-value st)
             (setf (fst-after-value st) nil)
             (return)))                         ; a null value
        (t
         (multiple-value-bind (token quoted) (%f7-ld-token st)
           (setf (fst-after-value st) t)
           (let ((star (and (not quoted) (position #\* token))))
             (cond
               ((and star (> star 0) (every #'digit-char-p (subseq token 0 star)))
                (let ((r (parse-integer token :end star)) (rest (subseq token (+ star 1))))
                  (when (< r 1) (%f7-bad-input "repeat count" token))
                  (setf (fst-rep-left st) (- r 1))
                  (if (string= rest "")
                      (setf (fst-rep-val st) :null)
                      (let ((v (%f7-ld-convert rest nil kind len)))
                        (setf (fst-rep-val st) v) (funcall setter v)))))
               (t (funcall setter (%f7-ld-convert token quoted kind len))))))
         (return))))))

;;; ---- finishing and guarding a statement ---------------------------------------------------------------------------------

(defun %f7-end (st)
  "Finish the statement: output the rest of the format and the last record."
  (case (fst-kind st)
    (:list (when (eq (fst-mode st) :write) (%f7-emit st)))
    (:unf (when (eq (fst-mode st) :write) (%f7-write-record (fst-io st) (reverse (fst-vals st)))))
    (t
     (when (eq (fst-mode st) :write)
       (loop
         (let ((d (%f7-next-desc st)))
           (cond ((eq d :end) (return))
                 ((eq (car d) :colon) (return))
                 ((%f7-data-desc-p d) (return))
                 (t (%f7-control st d)))))
       (%f7-emit st))))
  nil)

(defun %f7-guard (thunk iostat-setter end-p err-p)
  "Run THUNK, an input/output statement, handling IOSTAT=, END= and ERR=: returns :OK, :END or :ERR."
  (if (not (or iostat-setter end-p err-p))
      (progn (funcall thunk) :ok)
      (handler-case (progn (funcall thunk) (when iostat-setter (funcall iostat-setter 0)) :ok)
        (f77-io-error (c)
          (let ((code (f77-error-code c)))
            (when iostat-setter (funcall iostat-setter code))
            (cond ((and (= code -1) end-p) :end)
                  (err-p :err)
                  (iostat-setter :ok)
                  (t (error c))))))))
