;;;; 60-dataframe.lisp -- data frames: a named collection of equal-length columns, with CSV/TSV input and output,
;;;; selection, filtering, sorting, grouping, joins and summaries.  Missing values are NIL.

(in-package :cl)

(defstruct (dframe (:copier nil) (:constructor %make-dframe (names columns))) names columns)

(defun make-dframe (names columns)
  "A data frame with the column NAMES (strings) and COLUMNS (sequences of equal length)."
  (let ((cols (mapcar #'%seq->vec columns)))
    (unless (= (length names) (length cols)) (error "Need one name per column"))
    (when (and cols (some (lambda (c) (/= (length c) (length (car cols)))) cols)) (error "Columns must have equal length"))
    (%make-dframe (mapcar (lambda (n) (if (stringp n) n (string-downcase (princ-to-string n)))) names) cols)))

(defun df-nrow (d) "Number of rows of the data frame D." (if (dframe-columns d) (length (car (dframe-columns d))) 0))
(defun df-ncol (d) "Number of columns of the data frame D." (length (dframe-names d)))
(defun df-names (d) "Column names of D." (dframe-names d))

(defun df-col (d name)
  "The column NAME of D as a vector."
  (let ((i (position name (dframe-names d) :test #'string=)))
    (unless i (error "No column named ~A" name))
    (nth i (dframe-columns d))))

(defun df-ref (d row name) "The value in ROW of column NAME." (aref (df-col d name) row))

(defun df-row (d i) "Row I of D as a list of values." (mapcar (lambda (c) (aref c i)) (dframe-columns d)))

(defun df-select (d names)
  "A data frame with only the columns NAMES."
  (make-dframe names (mapcar (lambda (n) (df-col d n)) names)))

(defun df-take-rows (d indices)
  "A data frame with the rows at INDICES (in that order)."
  (make-dframe (dframe-names d) (mapcar (lambda (c) (map 'vector (lambda (i) (aref c i)) indices)) (dframe-columns d))))

(defun df-filter (d predicate)
  "Rows of D for which PREDICATE is true.  PREDICATE receives a function mapping a column name to the row's value."
  (let ((keep nil))
    (dotimes (i (df-nrow d))
      (when (funcall predicate (lambda (name) (aref (df-col d name) i))) (setq keep (cons i keep))))
    (df-take-rows d (reverse keep))))

(defun df-add-column (d name values)
  "D with the extra column NAME; VALUES is a sequence or a function of a row accessor."
  (let ((vals (if (functionp values)
                  (let ((out (make-array (df-nrow d) :initial-element nil)))
                    (dotimes (i (df-nrow d)) (setf (aref out i) (funcall values (lambda (n) (aref (df-col d n) i))))) out)
                  (%seq->vec values))))
    (make-dframe (append (dframe-names d) (list name)) (append (dframe-columns d) (list vals)))))

(defun df-sort (d name &key descending)
  "D sorted by column NAME (missing values last)."
  (let* ((c (df-col d name)) (idx (loop for i from 0 below (df-nrow d) collect i))
         (less (lambda (a b) (let ((x (aref c a)) (y (aref c b)))
                               (cond ((null x) nil) ((null y) t)
                                     ((stringp x) (if descending (string> x y) (string< x y)))
                                     (t (if descending (> x y) (< x y))))))))
    (df-take-rows d (stable-sort idx less))))

(defun df-head (d &optional (n 6)) "The first N rows of D." (df-take-rows d (loop for i from 0 below (min n (df-nrow d)) collect i)))

(defun df-group-by (d names)
  "Groups of D by the columns NAMES: a list of (key-values . row-indices) in order of first appearance."
  (let ((groups nil) (table (make-hash-table :test 'equal)))
    (dotimes (i (df-nrow d))
      (let* ((key (mapcar (lambda (n) (df-ref d i n)) names)) (cell (gethash key table)))
        (if cell (setf (cdr cell) (cons i (cdr cell)))
            (let ((new (cons key (list i)))) (setf (gethash key table) new) (setq groups (cons new groups))))))
    (mapcar (lambda (g) (cons (car g) (reverse (cdr g)))) (reverse groups))))

(defun df-summarize (d group-names &rest specs)
  "Aggregate D by GROUP-NAMES.  Each spec is (output-name function column-name); FUNCTION receives the column's values
(missing values removed) for the group as a list."
  (let ((groups (df-group-by d group-names)))
    (make-dframe (append group-names (mapcar #'first specs))
                 (append (loop for k from 0 below (length group-names) collect (map 'vector (lambda (g) (nth k (car g))) groups))
                         (mapcar (lambda (spec)
                                   (map 'vector (lambda (g)
                                                  (funcall (second spec) (remove nil (mapcar (lambda (i) (df-ref d i (third spec))) (cdr g)))))
                                        groups))
                                 specs)))))

(defun df-join (a b key &key (type :inner))
  "Join A and B on the column KEY.  TYPE is :inner or :left.  Columns of B that clash with A's are suffixed \"_y\"."
  (let* ((index (make-hash-table :test 'equal)) (bnames (remove key (dframe-names b) :test #'string=))
         (out-rows nil))
    (dotimes (j (df-nrow b)) (let ((k (df-ref b j key))) (setf (gethash k index) (append (gethash k index) (list j)))))
    (dotimes (i (df-nrow a))
      (let ((matches (gethash (df-ref a i key) index)))
        (cond (matches (dolist (j matches) (setq out-rows (cons (cons i j) out-rows))))
              ((eq type :left) (setq out-rows (cons (cons i nil) out-rows))))))
    (setq out-rows (reverse out-rows))
    (make-dframe (append (dframe-names a) (mapcar (lambda (n) (if (member n (dframe-names a) :test #'string=) (%string-concat n "_y") n)) bnames))
                 (append (mapcar (lambda (c) (map 'vector (lambda (r) (aref c (car r))) out-rows)) (dframe-columns a))
                         (mapcar (lambda (n) (let ((c (df-col b n))) (map 'vector (lambda (r) (and (cdr r) (aref c (cdr r)))) out-rows))) bnames)))))

(defun df-column-stats (d)
  "A data frame summarising the numeric columns of D (n, mean, sd, min, median, max)."
  (let* ((names (remove-if-not (lambda (n) (let ((c (remove nil (coerce (df-col d n) 'list)))) (and c (every #'realp c)))) (dframe-names d)))
         (stats (mapcar (lambda (n) (describe-data (remove nil (coerce (df-col d n) 'list)))) names)))
    (make-dframe (list "column" "n" "mean" "sd" "min" "median" "max")
                 (list names (mapcar (lambda (s) (getf s :n)) stats) (mapcar (lambda (s) (getf s :mean)) stats) (mapcar (lambda (s) (getf s :sd)) stats)
                       (mapcar (lambda (s) (getf s :min)) stats) (mapcar (lambda (s) (getf s :median)) stats) (mapcar (lambda (s) (getf s :max)) stats)))))

(defun df->matrix (d &optional names)
  "The numeric columns NAMES (default all) of D as a matrix."
  (matrix-from-columns (mapcar (lambda (n) (df-col d n)) (or names (dframe-names d)))))

(defun df-string (d &optional (max-rows 20))
  "A text rendering of D with aligned columns."
  (let* ((n (min max-rows (df-nrow d)))
         (cells (mapcar (lambda (name col) (cons name (loop for i from 0 below n collect (let ((v (aref col i))) (cond ((null v) "NA") ((floatp v) (%fmt-num v)) (t (format nil "~A" v)))))))
                        (dframe-names d) (dframe-columns d)))
         (widths (mapcar (lambda (c) (reduce #'max (mapcar #'length c))) cells)))
    (with-output-to-string (s)
      (dotimes (r (+ n 1))
        (loop for c in cells for w in widths
              do (let ((txt (nth r c))) (format s "~A~A " (make-string (- w (length txt)) :initial-element #\Space) txt)))
        (terpri s))
      (when (> (df-nrow d) n) (format s "... ~D more rows~%" (- (df-nrow d) n))))))

(defmethod print-object ((d dframe) stream)
  (format stream "#<DFRAME ~D rows x ~D columns>~%~A" (df-nrow d) (df-ncol d) (df-string d 10)))

;;; ---- CSV / TSV ---------------------------------------------------------------------------------------------------------

(defun parse-delimited (text separator)
  "Parse TEXT into a list of records (lists of strings), handling quoted fields, doubled quotes and CRLF."
  (let ((records nil) (fields nil) (cur (make-string-output-stream)) (in-quotes nil) (n (length text)) (i 0) (has nil))
    (flet ((end-field () (setq fields (cons (get-output-stream-string cur) fields) has nil))
           (end-record () (setq records (cons (reverse fields) records) fields nil)))
      (loop while (< i n)
            do (let ((c (char text i)))
                 (cond (in-quotes
                        (cond ((char= c #\") (if (and (< (+ i 1) n) (char= (char text (+ i 1)) #\"))
                                                 (progn (write-char #\" cur) (setq i (+ i 1)))
                                                 (setq in-quotes nil)))
                              (t (write-char c cur))))
                       ((char= c #\") (setq in-quotes t has t))
                       ((char= c separator) (end-field) (setq has t))
                       ((char= c #\Return) nil)
                       ((char= c #\Newline) (when (or has fields) (end-field) (end-record)))
                       (t (write-char c cur) (setq has t)))
                 (setq i (+ i 1))))
      (when (or has fields) (end-field) (end-record)))
    (reverse records)))

(defun %convert-column (strings)
  (let ((vals (mapcar (lambda (s) (if (member s '("" "NA" "N/A" "NaN" "na") :test #'string=) :na s)) strings)))
    (flet ((all (pred) (every (lambda (v) (or (eq v :na) (funcall pred v))) vals))
           (num-p (v) (let ((start (if (and (> (length v) 0) (member (char v 0) '(#\- #\+))) 1 0)) (dots 0) (digits 0) (ok t))
                        (loop for i from start below (length v)
                              do (let ((c (char v i)))
                                   (cond ((digit-char-p c) (incf digits)) ((char= c #\.) (incf dots))
                                         ((member c '(#\e #\E #\- #\+)) (when (= digits 0) (setq ok nil)))
                                         (t (setq ok nil)))))
                        (and ok (> digits 0) (<= dots 1)))))
      (cond ((all (lambda (v) (let ((s (if (member (char v 0) '(#\- #\+)) 1 0))) (and (> (length v) s) (every #'digit-char-p (subseq v s))))))
             (map 'vector (lambda (v) (if (eq v :na) nil (parse-integer v))) vals))
            ((all #'num-p)
             (map 'vector (lambda (v) (if (eq v :na) nil (float (read-from-string (if (char= (char v 0) #\+) (subseq v 1) v))))) vals))
            (t (map 'vector (lambda (v) (if (eq v :na) nil v)) vals))))))

(defun dframe-from-records (records &key (header t))
  "A data frame from parsed RECORDS; the first record names the columns when HEADER is true."
  (let* ((names (if header (car records) (loop for i from 1 to (length (car records)) collect (format nil "V~D" i))))
         (rows (if header (cdr records) records)) (ncol (length names)))
    (make-dframe names (loop for j from 0 below ncol collect (%convert-column (mapcar (lambda (r) (or (nth j r) "")) rows))))))

(defun read-csv (path &key (separator #\,) (header t))
  "Read a delimited file into a data frame.  Columns that look numeric become numbers; empty and NA cells become NIL."
  (dframe-from-records (parse-delimited (join-lines (read-file-lines path)) separator) :header header))

(defun read-tsv (path &key (header t)) "Read a tab-separated file into a data frame." (read-csv path :separator #\Tab :header header))

(defun %csv-field (v separator)
  (let ((s (cond ((null v) "NA") ((floatp v) (format nil "~A" v)) (t (format nil "~A" v)))))
    (if (or (find separator s) (find #\" s) (find #\Newline s))
        (with-output-to-string (o) (write-char #\" o) (loop for c across s do (when (char= c #\") (write-char #\" o)) (write-char c o)) (write-char #\" o))
        s)))

(defun dframe-csv-string (d &key (separator #\,))
  "D rendered as delimited text with a header line."
  (with-output-to-string (s)
    (format s "~A~%" (%join-strings (mapcar (lambda (n) (%csv-field n separator)) (dframe-names d)) (string separator)))
    (dotimes (i (df-nrow d))
      (format s "~A~%" (%join-strings (mapcar (lambda (v) (%csv-field v separator)) (df-row d i)) (string separator))))))

(defun write-csv (d path &key (separator #\,))
  "Write the data frame D to PATH as delimited text."
  (with-open-file (s path :direction :output :if-exists :supersede)
    (write-string (dframe-csv-string d :separator separator) s))
  path)
