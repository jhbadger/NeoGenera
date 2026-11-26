;;;; 00-docs.lisp -- the documentation system: document format, Markdown import, symbol
;;;; documents, full-text search and coverage.
;;;;
;;;; A DOCUMENT is a structure holding a list of NODES written as s-expressions:
;;;;
;;;;   block nodes   (:h level inline...)  (:p inline...)  (:example "code" [language])  (:ul item...)  (:ol item...)
;;;;                 (:table header-row row...)  (:see-also topic...)         item/cell = list of inlines
;;;;   inline nodes  "text"  (:code "x")  (:em inline...)  (:strong inline...)
;;;;                 (:ref topic "text")  (:fn symbol)  (:link "url" "text")
;;;;
;;;; The same nodes are rendered by the viewer (a presentation window), to HTML and to PDF.
;;;; Documents are written with DEFINE-DOCUMENT or imported from Markdown (IMPORT-MARKDOWN reads the
;;;; file at compile time, so the text becomes part of the system image).

(in-package :cl)

;;; ---- DOCUMENTATION -------------------------------------------------------------------------------

(defun %doc-indicator (type)
  (case type ((function compiler-macro) '%doc-function) (variable '%doc-variable) ((type structure) '%doc-type) (t nil)))

(defun documentation (x type)
  (let ((i (%doc-indicator type))) (and i (symbolp x) (get x i))))

(defun set-documentation (x type value)
  (let ((i (%doc-indicator type))) (when (and i (symbolp x)) (%put x i value)) value))
(%def-setf 'documentation 'set-documentation)

(defun %doc-bulk (indicator entries)
  "ENTRIES is a flat list: symbol \"docstring\" symbol \"docstring\" ..."
  (do-list-tails (e entries) (%put (car e) indicator (cadr e)) (setq e (cdr e)))
  nil)

(defun symbol-documented-p (sym)
  (and (symbolp sym) sym (or (get sym '%doc-function) (get sym '%doc-variable) (get sym '%doc-type)) t))

;;; ---- registry ---------------------------------------------------------------------------------------

(defstruct (doc (:constructor %make-doc) (:copier nil)) name title keywords category nodes file)
(defvar *documents* nil)
(defvar *doc-index* nil)

(defun find-document (name) (find name *documents* :key #'doc-name))

(defun %register-document (name title keywords category nodes &optional file)
  (let ((d (%make-doc :name name :title title :keywords keywords :category category :nodes nodes :file file)))
    (setq *documents* (append (remove name *documents* :key #'doc-name) (list d)))
    (setq *doc-index* nil)
    d))

(defmacro define-document (name (&key title keywords category) &body nodes)
  `(%register-document ',name ,(or title (string-capitalize (substitute #\Space #\- (symbol-name name)))) ',keywords ',category ',nodes))

(defun document-names () (mapcar #'doc-name *documents*))

;;; ---- Markdown import ----------------------------------------------------------------------------------

(defun %md-link-node (text target)
  (cond ((%starts-with target "topic:") (list :ref (intern (string-upcase (subseq target 6)) "KEYWORD") text))
        ((or (%starts-with target "fn:") (%starts-with target "function:"))
         (let ((sym (%find-symbol-named (subseq target (+ 1 (%string-position-char target #\: 0))))))
           (if sym (list :fn sym) text)))
        ((or (%starts-with target "http://") (%starts-with target "https://")) (list :link target text))
        (t (list :ref (intern (string-upcase target) "KEYWORD") text))))

(defun %md-inline (s)
  "Parse the inline markup of S: `code`, **strong**, *em*, [text](target)."
  (let ((nodes nil) (buf (make-string-output-stream)) (i 0) (n (length s)))
    (flet ((flush ()
             (let ((str (get-output-stream-string buf))) (when (> (length str) 0) (setq nodes (cons str nodes))))))
      (loop
        (when (>= i n) (return))
        (let ((c (char s i)))
          (cond
            ((char= c #\`)
             (let ((e (%string-position-char s #\` (+ i 1))))
               (if e (progn (flush) (setq nodes (cons (list :code (subseq s (+ i 1) e)) nodes)) (setq i (+ e 1)))
                   (progn (write-char c buf) (setq i (+ i 1))))))
            ((and (char= c #\*) (< (+ i 1) n) (char= (char s (+ i 1)) #\*))
             (let ((e (%string-search "**" s (+ i 2))))
               (if e (progn (flush) (setq nodes (cons (cons :strong (%md-inline (subseq s (+ i 2) e))) nodes)) (setq i (+ e 2)))
                   (progn (write-char c buf) (setq i (+ i 1))))))
            ((and (char= c #\*) (< (+ i 1) n) (not (member (char s (+ i 1)) '(#\Space #\*))))
             (let ((e (%string-position-char s #\* (+ i 1))))
               (if (and e (> e (+ i 1)) (char/= (char s (- e 1)) #\Space))
                   (progn (flush) (setq nodes (cons (cons :em (%md-inline (subseq s (+ i 1) e))) nodes)) (setq i (+ e 1)))
                   (progn (write-char c buf) (setq i (+ i 1))))))
            ((char= c #\[)
             (let* ((close (%string-search "](" s (+ i 1))) (end (and close (%string-position-char s #\) (+ close 2)))))
               (if (and close end)
                   (progn (flush) (setq nodes (cons (%md-link-node (subseq s (+ i 1) close) (subseq s (+ close 2) end)) nodes))
                          (setq i (+ end 1)))
                   (progn (write-char c buf) (setq i (+ i 1))))))
            (t (write-char c buf) (setq i (+ i 1))))))
      (flush))
    (nreverse nodes)))

(defun %md-trim (s) (string-trim '(#\Space #\Tab) s))
(defun %md-blank-p (line) (= (length (%md-trim line)) 0))
(defun %md-fence-p (line) (and (>= (length line) 3) (string= (subseq line 0 3) "```")))

(defun %md-heading-level (line)
  (let ((n 0))
    (loop (if (and (< n (length line)) (char= (char line n) #\#)) (setq n (+ n 1)) (return)))
    (and (> n 0) (<= n 6) (< n (length line)) (char= (char line n) #\Space) n)))

(defun %md-bullet-p (line)
  (let ((tr (string-left-trim " " line)))
    (and (>= (length tr) 2) (member (char tr 0) '(#\- #\*)) (char= (char tr 1) #\Space))))

(defun %md-ordered-p (line)
  (let* ((tr (string-left-trim " " line)) (i 0))
    (loop (if (and (< i (length tr)) (digit-char-p (char tr i))) (setq i (+ i 1)) (return)))
    (and (> i 0) (< (+ i 1) (length tr)) (char= (char tr i) #\.) (char= (char tr (+ i 1)) #\Space))))

(defun %md-item-text (line)
  (let* ((tr (string-left-trim " " line)) (p (%string-position-char tr #\Space 0))) (%md-trim (subseq tr (+ p 1)))))

(defun %md-table-cells (line)
  (let* ((tr (%md-trim line)) (parts (%split-string tr #\|)))
    (when (and parts (string= (car parts) "")) (setq parts (cdr parts)))
    (when (and parts (string= (car (last parts)) "")) (setq parts (butlast parts)))
    (mapcar (lambda (c) (%md-inline (%md-trim c))) parts)))

(defun %md-separator-row-p (line)
  (let ((tr (%md-trim line)))
    (and (> (length tr) 0) (char= (char tr 0) #\|) (every (lambda (c) (member c '(#\| #\- #\: #\Space))) tr))))

(defun markdown->nodes (text)
  "Parse Markdown TEXT into document nodes."
  (let ((lines (split-lines text)) (nodes nil) (para nil))
    (flet ((flush-para ()
             (when para
               (setq nodes (cons (cons :p (%md-inline (%join-strings (reverse para) " "))) nodes))
               (setq para nil))))
      (loop
        (when (null lines) (return))
        (let ((line (car lines)))
          (cond
            ((%md-fence-p line)
             (flush-para)
             (let ((code nil) (lang (string-downcase (string-trim '(#\Space #\Tab #\`) line))))
               (setq lines (cdr lines))
               (loop (when (or (null lines) (%md-fence-p (car lines))) (return))
                     (setq code (cons (car lines) code)) (setq lines (cdr lines)))
               ;; (:example code) or, after a fence naming a language, (:example code "pascal")
               (setq nodes (cons (list* :example (join-lines (reverse code)) (if (string= lang "") nil (list lang))) nodes))))
            ((%md-blank-p line) (flush-para))
            ((%md-heading-level line)
             (flush-para)
             (let ((lv (%md-heading-level line)))
               (setq nodes (cons (list* :h lv (%md-inline (%md-trim (subseq line (+ lv 1))))) nodes))))
            ((or (%md-bullet-p line) (%md-ordered-p line))
             (flush-para)
             (let ((ordered (%md-ordered-p line)) (items nil))
               (loop
                 (when (null lines) (return))
                 (let ((l (car lines)))
                   (cond ((if ordered (%md-ordered-p l) (%md-bullet-p l))
                          (setq items (cons (%md-item-text l) items)))
                         ((and items (> (length l) 1) (char= (char l 0) #\Space) (not (%md-blank-p l)))
                          (setf (car items) (%string-concat (car items) " " (%md-trim l))))
                         (t (setq lines (cons l (cdr lines))) (return))))
                 (setq lines (cdr lines)))
               (setq nodes (cons (cons (if ordered :ol :ul) (mapcar #'%md-inline (reverse items))) nodes))
               (setq lines (cons "" lines))))
            ((and (> (length (%md-trim line)) 0) (char= (char (%md-trim line) 0) #\|))
             (flush-para)
             (let ((rows nil))
               (loop (when (or (null lines) (%md-blank-p (car lines)) (char/= (char (%md-trim (car lines)) 0) #\|)) (return))
                     (unless (%md-separator-row-p (car lines)) (setq rows (cons (%md-table-cells (car lines)) rows)))
                     (setq lines (cdr lines)))
               (setq nodes (cons (cons :table (reverse rows)) nodes))
               (setq lines (cons "" lines))))
            (t (setq para (cons (%md-trim line) para)))))
        (setq lines (cdr lines)))
      (flush-para))
    (nreverse nodes)))

(defun markdown-title (nodes)
  (dolist (n nodes "Untitled") (when (and (eq (car n) :h) (eql (cadr n) 1)) (return (inline-text (cddr n))))))

(defmacro import-markdown (name path &key category keywords)
  "Define document NAME from the Markdown file PATH, read when the form is compiled."
  (let* ((nodes (markdown->nodes (join-lines (read-file-lines (resolve-source-path path))))))
    `(%register-document ',name ,(markdown-title nodes) ',keywords ',category ',nodes ,path)))

(defun load-markdown-document (name path &key category keywords)
  "Read Markdown from PATH at run time and register it as document NAME."
  (let ((nodes (markdown->nodes (join-lines (read-file-lines path)))))
    (%register-document name (markdown-title nodes) keywords category nodes path)))

;;; ---- plain text rendering (used by PDF export and the line-mode Listener) ----------------------------------

(defun inline-text (nodes)
  (apply #'%string-concat
         (mapcar (lambda (n)
                   (cond ((stringp n) n)
                         ((eq (car n) :code) (cadr n))
                         ((member (car n) '(:em :strong)) (inline-text (cdr n)))
                         ((eq (car n) :ref) (caddr n))
                         ((eq (car n) :fn) (string-downcase (symbol-name (cadr n))))
                         ((eq (car n) :link) (caddr n))
                         (t "")))
                 nodes)))

(defun wrap-words (text width &optional (indent "") (first-indent indent))
  "Wrap TEXT to WIDTH columns; returns a list of lines."
  (let ((words (remove "" (%split-string (substitute #\Space #\Newline text) #\Space) :test #'string=))
        (lines nil) (cur first-indent) (fresh t))
    (dolist (w words)
      (cond (fresh (setq cur (%string-concat cur w) fresh nil))
            ((<= (+ (length cur) 1 (length w)) width) (setq cur (%string-concat cur " " w)))
            (t (setq lines (cons cur lines)) (setq cur (%string-concat indent w)))))
    (unless fresh (setq lines (cons cur lines)))
    (nreverse lines)))

(defun %table-lines (rows)
  (let* ((cells (mapcar (lambda (r) (mapcar #'inline-text r)) rows))
         (ncols (reduce #'max (mapcar #'length cells) :initial-value 0))
         (widths (let ((acc nil)) (dotimes (c ncols) (setq acc (append acc (list (reduce #'max (mapcar (lambda (r) (length (or (nth c r) ""))) cells) :initial-value 0)))))  acc)))
    (mapcar (lambda (r) (let ((parts nil) (c 0))
                          (dolist (w widths) (setq parts (cons (%pad-to (or (nth c r) "") w) parts)) (setq c (+ c 1)))
                          (string-right-trim " " (%join-strings (nreverse parts) "  "))))
            cells)))

(defun doc->lines (doc &optional (width 78))
  "The document as a list of (style . text) lines; styles: :title :h1 :h2 :h3 :text :code :blank."
  (let ((out nil))
    (flet ((emit (style text) (setq out (cons (cons style text) out))))
      (emit :title (doc-title doc)) (emit :blank "")
      (dolist (n (doc-nodes doc))
        (case (car n)
          (:h (unless (and (eql (cadr n) 1) (string= (inline-text (cddr n)) (doc-title doc)))
                (emit :blank "") (emit (case (cadr n) (1 :h1) (2 :h2) (t :h3)) (inline-text (cddr n))) (emit :blank "")))
          (:p (dolist (l (wrap-words (inline-text (cdr n)) width)) (emit :text l)) (emit :blank ""))
          (:example (dolist (l (split-lines (cadr n))) (emit :code (%string-concat "    " l))) (emit :blank ""))
          (:ul (dolist (item (cdr n)) (dolist (l (wrap-words (inline-text item) width "    " "  * ")) (emit :text l))) (emit :blank ""))
          (:ol (let ((i 0)) (dolist (item (cdr n)) (setq i (+ i 1))
                              (dolist (l (wrap-words (inline-text item) width "     " (format nil " ~D. " i))) (emit :text l))))
               (emit :blank ""))
          (:table (dolist (l (%table-lines (cdr n))) (emit :code (%string-concat "  " l))) (emit :blank ""))
          (:see-also (emit :text (%string-concat "See also: " (%join-strings (mapcar (lambda (x) (princ-to-string x)) (cdr n)) ", "))) (emit :blank ""))
          (t nil))))
    (nreverse out)))

(defun doc-text (doc &optional (width 78))
  (join-lines (mapcar #'cdr (doc->lines doc width))))

;;; ---- symbol documents -------------------------------------------------------------------------------------------

(defun %doc-paragraphs (string)
  (let ((paras nil) (cur nil))
    (dolist (l (split-lines string))
      (if (%md-blank-p l)
          (when cur (setq paras (cons (%join-strings (reverse cur) " ") paras)) (setq cur nil))
          (setq cur (cons (%md-trim l) cur))))
    (when cur (setq paras (cons (%join-strings (reverse cur) " ") paras)))
    (nreverse paras)))

(defun symbol-kinds (sym)
  (let ((kinds nil))
    (when (special-operator-p sym) (setq kinds (cons :special-operator kinds)))
    (when (macro-function sym) (setq kinds (cons :macro kinds)))
    (when (%find-gf sym) (setq kinds (cons :generic-function kinds)))
    (when (and (fboundp sym) (not (macro-function sym)) (not (%find-gf sym)) (not (special-operator-p sym))) (setq kinds (cons :function kinds)))
    (when (and (boundp sym) (not (keywordp sym))) (setq kinds (cons :variable kinds)))
    (when (and (find-class sym nil) (not (eq sym t))) (setq kinds (cons :class kinds)))
    (nreverse kinds)))

(defun symbol-document (sym)
  "A generated document describing SYM: signatures, docstrings, value, class structure, location."
  (let ((nodes nil) (kinds (symbol-kinds sym)))
    (flet ((add (n) (setq nodes (append nodes (list n)))))
      (add (list :h 1 (%fname sym)))
      (dolist (k kinds)
        (case k
          ((:function :macro :generic-function :special-operator)
           (add (list :p (list :strong (case k (:function "Function") (:macro "Macro") (:generic-function "Generic function") (t "Special operator")))
                      " " (list :code (or (arglist-string sym) (%string-concat "(" (string-downcase (symbol-name sym)) " ...)")))))
           (dolist (p (%doc-paragraphs (or (documentation sym 'function) ""))) (add (cons :p (%md-inline p))))
           (when (eq k :generic-function)
             (add (list :p (format nil "~D methods." (length (gf-methods (%find-gf sym))))))))
          (:variable
           (add (list :p (list :strong "Variable") " " (list :code (%fname sym)) (format nil " = ~A" (%abbrev (symbol-value sym) 70))))
           (dolist (p (%doc-paragraphs (or (documentation sym 'variable) ""))) (add (cons :p (%md-inline p)))))
          (:class
           (let ((c (find-class sym)))
             (add (list :p (list :strong "Class") " " (list :code (%fname sym))))
             (dolist (p (%doc-paragraphs (or (documentation sym 'type) ""))) (add (cons :p (%md-inline p))))
             (add (list :p "Superclasses: " (inline-text-list (mapcar (lambda (s) (list :fn (%cls-name s))) (%cls-supers c)))))
             (when (%cls-slots c) (add (cons :ul (mapcar (lambda (sd) (list (list :code (string-downcase (symbol-name (slotd-name sd)))))) (%cls-slots c)))))))))
      (when (null kinds) (add (list :p "No definition is recorded for this symbol.")))
      (dolist (l (definition-locations sym))
        (add (list :p "Defined in " (list :code (cadr l)) (format nil " (~A)." (string-downcase (symbol-name (car l))))))))
    (%make-doc :name sym :title (%fname sym) :keywords nil :category :symbol :nodes nodes)))

(defun inline-text-list (nodes)
  (let ((acc nil))
    (dolist (n nodes) (setq acc (append acc (if acc (list ", " n) (list n)))))
    (or acc (list "none"))))

(defun resolve-topic (x)
  "A document for X (a document, document name, symbol, or string), or NIL."
  (cond ((doc-p x) x)
        ((and (symbolp x) x (find-document x)) (find-document x))
        ((stringp x) (let ((k (intern (string-upcase x) "KEYWORD")))
                       (or (find-document k) (let ((s (%find-symbol-named x))) (and s (symbol-document s))))))
        ((and (symbolp x) x (or (symbol-kinds x) (symbol-documented-p x))) (symbol-document x))
        (t nil)))

(defun doc-summary (d)
  "First sentence of the first paragraph."
  (let ((p (find :p (doc-nodes d) :key #'car)))
    (if p
        (let* ((text (inline-text (cdr p))) (e (%string-search ". " text 0)))
          (let ((s (if e (subseq text 0 (+ e 1)) text)))
            (if (> (length s) 100) (%string-concat (subseq s 0 97) "...") s)))
        "")))

;;; ---- search ----------------------------------------------------------------------------------------------------------

(defparameter *doc-stop-words* '("the" "and" "for" "with" "that" "this" "are" "was" "not" "you" "can" "from" "has" "have" "its" "any" "all" "one" "may"))

(defun doc-tokenize (str)
  (let ((words nil) (cur (make-string-output-stream)) (n 0))
    (flet ((flush () (when (> n 0) (let ((w (get-output-stream-string cur))) (when (and (>= (length w) 2) (not (member w *doc-stop-words* :test #'string=))) (setq words (cons w words)))) (setq n 0))))
      (dotimes (i (length str))
        (let ((c (char str i)))
          (if (and (< (char-code c) 128) (alphanumericp c)) (progn (write-char (char-downcase c) cur) (setq n (+ n 1))) (flush))))
      (flush))
    (nreverse words)))

(defun %node-words (node)
  (case (car node)
    (:p (doc-tokenize (inline-text (cdr node))))
    (:h (doc-tokenize (inline-text (cddr node))))
    (:example (doc-tokenize (cadr node)))
    ((:ul :ol) (let ((acc nil)) (dolist (i (cdr node)) (setq acc (append acc (doc-tokenize (inline-text i))))) acc))
    (:table (let ((acc nil)) (dolist (r (cdr node)) (dolist (c r) (setq acc (append acc (doc-tokenize (inline-text c)))))) acc))
    (t nil)))

(defun %index-add (table word weight)
  (setf (gethash word table) (+ weight (gethash word table 0))))

(defun %make-entry (key kind title table) (list key kind title table))

(defstruct (doc-search-index (:copier nil) (:constructor %make-doc-search-index (entries postings vocabulary)))
  entries postings vocabulary)

(defun doc-index-build ()
  "Index every document (title 5, keywords 3, headings 3, body 1) and every documented symbol, as an inverted index:
word -> postings (entry . weight), plus a sorted vocabulary for prefix matching."
  (let ((entries nil))
    (dolist (d *documents*)
      (let ((table (make-hash-table :test 'equal)))
        (dolist (w (doc-tokenize (doc-title d))) (%index-add table w 5))
        (dolist (k (doc-keywords d)) (dolist (w (doc-tokenize (if (symbolp k) (symbol-name k) k))) (%index-add table w 3)))
        (dolist (n (doc-nodes d))
          (dolist (w (%node-words n)) (%index-add table w (if (eq (car n) :h) 3 1))))
        (setq entries (cons (%make-entry (doc-name d) :document (doc-title d) table) entries))))
    (dolist (p (list-all-packages))
      (dolist (sym (%package-symbols p nil))
        (when (and (eq (symbol-package sym) p) (symbol-documented-p sym))
          (let ((table (make-hash-table :test 'equal)) (name (symbol-name sym)))
            (%index-add table (string-downcase name) 6)
            (dolist (w (doc-tokenize name)) (%index-add table w 4))
            (dolist (ind '(%doc-function %doc-variable %doc-type))
              (let ((d (get sym ind))) (when d (dolist (w (doc-tokenize d)) (%index-add table w 1)))))
            (setq entries (cons (%make-entry sym :symbol (%fname sym) table) entries))))))
    (let* ((vec (coerce (nreverse entries) 'vector)) (postings (make-hash-table :test 'equal)) (vocab nil))
      (dotimes (i (length vec))
        (maphash (lambda (word weight) (setf (gethash word postings) (cons (cons i weight) (gethash word postings)))) (fourth (aref vec i))))
      (maphash (lambda (word list) (declare (ignore list)) (push word vocab)) postings)
      (setq *doc-index* (%make-doc-search-index vec postings (coerce (sort vocab #'string<) 'vector))))))

(defun %vocab-prefix-range (vocab prefix)
  "Indices [lo, hi) of the words of the sorted vector VOCAB that start with PREFIX."
  (let ((n (length vocab)) (lo 0) (hi 0) (plen (length prefix)))
    (let ((a 0) (b n))
      (loop while (< a b) do (let ((mid (floor (+ a b) 2))) (if (string< (aref vocab mid) prefix) (setq a (+ mid 1)) (setq b mid))))
      (setq lo a hi a))
    (loop while (and (< hi n) (let ((w (aref vocab hi))) (and (>= (length w) plen) (string= (subseq w 0 plen) prefix)))) do (setq hi (+ hi 1)))
    (values lo hi)))

(defun search-documentation (query &optional (limit 30))
  "Ranked results for QUERY: a list of (score key kind title).  An entry scores the capped weight of each query word
(an exact word, else the best longer word with that prefix at half weight); matching every word earns a bonus."
  (unless *doc-index* (doc-index-build))
  (let* ((words (doc-tokenize query)) (index *doc-index*) (entries (doc-search-index-entries index)) (vocab (doc-search-index-vocabulary index))
         (postings (doc-search-index-postings index)) (n (length entries))
         (scores (make-array n :initial-element 0)) (hits (make-array n :initial-element 0)) (touched nil) (results nil))
    (dolist (w words)
      (let ((exact (make-hash-table :test 'eql)) (prefix (make-hash-table :test 'eql)))
        (dolist (p (gethash w postings)) (setf (gethash (car p) exact) (min (cdr p) 12)))
        (multiple-value-bind (lo hi) (%vocab-prefix-range vocab w)
          (loop for k from lo below hi
                do (let ((word (aref vocab k)))
                     (when (> (length word) (length w))
                       (dolist (p (gethash word postings))
                         (let ((half (floor (min (cdr p) 12) 2)))
                           (when (> half (gethash (car p) prefix -1)) (setf (gethash (car p) prefix) half))))))))
        (flet ((credit (e v) (when (zerop (aref hits e)) (push e touched)) (incf (aref scores e) v) (incf (aref hits e))))
          (maphash (lambda (e v) (credit e v)) exact)
          (maphash (lambda (e v) (unless (gethash e exact) (credit e v))) prefix))))
    (when words
      (dolist (e touched)
        (let* ((h (aref hits e)) (score (if (= h (length words)) (+ (aref scores e) (* 2 h)) (floor (aref scores e) 2))))
          (when (> score 0)
            (let ((entry (aref entries e))) (push (list score (first entry) (second entry) (third entry)) results))))))
    (let ((sorted (sort results (lambda (a b) (or (> (car a) (car b)) (and (= (car a) (car b)) (string< (fourth a) (fourth b))))))))
      (if (> (length sorted) limit) (subseq sorted 0 limit) sorted))))

;;; ---- coverage ------------------------------------------------------------------------------------------------------------

(defun %manual-symbols ()
  "Symbols mentioned (as (:fn sym) or (:code \"name\")) in any document."
  (let ((table (make-hash-table :test 'eq)))
    (labels ((scan (nodes)
               (dolist (n nodes)
                 (when (consp n)
                   (cond ((eq (car n) :fn) (setf (gethash (cadr n) table) t))
                         ((eq (car n) :code) (let ((s (ignore-errors (%find-symbol-named (cadr n))))) (when s (setf (gethash s table) t))))
                         ((member (car n) '(:p :em :strong)) (scan (cdr n)))
                         ((eq (car n) :h) (scan (cddr n)))
                         ((member (car n) '(:ul :ol)) (dolist (i (cdr n)) (scan i)))
                         ((eq (car n) :table) (dolist (r (cdr n)) (dolist (c r) (scan c)))))))))
      (dolist (d *documents*) (scan (doc-nodes d))))
    table))

(defvar *core-symbols* nil "Hash set of the COMMON-LISP symbols that were defined when the system was built.")

(defun snapshot-core-symbols ()
  (let ((table (make-hash-table :test 'eq)))
    (dolist (s (%package-symbols (find-package "COMMON-LISP") t)) (setf (gethash s table) t))
    (setq *core-symbols* table)))

(defun doc-coverage ()
  "Documentation coverage of the core library: exported COMMON-LISP symbols that name a function, macro,
variable or class (excluding the window system, editor and tools).  A symbol is covered when it has a
docstring or is mentioned in a manual.  Returns (values covered total undocumented-symbols)."
  (let ((mentioned (%manual-symbols)) (covered 0) (total 0) (missing nil))
    (dolist (s (%package-symbols (find-package "COMMON-LISP") t))
      (let ((locs (definition-locations s)))
        (when (and (or (null *core-symbols*) (gethash s *core-symbols*))
                   (not (char= (char (symbol-name s) 0) #\%)) (not (get s '%internal))
                   (or (fboundp s) (macro-function s) (find-class s nil) (and (boundp s) (not (keywordp s)) (not (eq s t)) (not (eq s nil))))
                   (or (null locs) (some (lambda (l) (%starts-with (cadr l) "lisp/core/")) locs)))
          (setq total (+ total 1))
          (if (or (symbol-documented-p s) (gethash s mentioned)) (setq covered (+ covered 1)) (setq missing (cons s missing))))))
    (values covered total (sort missing (lambda (a b) (string< (symbol-name a) (symbol-name b)))))))

(defun doc (topic &optional (stream *standard-output*))
  "Print the documentation for TOPIC (a document name, symbol or string) as text."
  (let ((d (resolve-topic topic)))
    (if d (progn (write-string (doc-text d) stream) (terpri stream)) (format stream "No documentation for ~S~%" topic))
    (values)))
