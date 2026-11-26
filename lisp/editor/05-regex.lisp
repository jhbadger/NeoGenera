;;;; 05-regex.lisp -- regular expressions on UTF-8 strings.
;;;;
;;;; A backtracking matcher with the usual (Perl/POSIX extended) syntax:
;;;;   .  [abc] [^a-z] [[:alpha:]]  \d \D \w \W \s \S  \b \B  ^ $
;;;;   ( )  (?: )  |  * + ? {n} {n,} {n,m}  and lazy *? +? ?? {n,m}?   \1..\9 back-references
;;;;   \t \n \\ \. and any other escaped character stands for itself.
;;;; Patterns and subjects are UTF-8: . and classes match a whole character.  Matching works within one
;;;; string (the editor searches line by line), so ^ and $ are the ends of the string and . does not match
;;;; a newline.  With :CASE-FOLD, letters match regardless of case (Unicode-aware, see UNICODE-FOLD).
;;;; Results are byte offsets.

(in-package :cl)

(defstruct (regex (:constructor %make-regex) (:copier nil)) source node (ngroups 0) fold)

(define-condition regex-syntax-error (simple-error) ())

(defun %rx-fail (fmt &rest args) (error 'regex-syntax-error :format-control fmt :format-arguments args))

;;; ---- parsing ---------------------------------------------------------------------------------------------

(defvar *rx-codes* nil)
(defvar *rx-pos* 0)
(defvar *rx-ngroups* 0)

(defun %rx-peek (&optional (k 0)) (let ((i (+ *rx-pos* k))) (and (< i (length *rx-codes*)) (aref *rx-codes* i))))
(defun %rx-next () (let ((c (%rx-peek))) (setq *rx-pos* (+ *rx-pos* 1)) c))
(defun %rx-at-p (char) (eql (%rx-peek) (char-code char)))

(defun regex-compile (pattern &key case-fold)
  "Compile the regular expression PATTERN (a UTF-8 string).  With CASE-FOLD, matching ignores case.
Signals REGEX-SYNTAX-ERROR for an invalid pattern."
  (let ((*rx-codes* (coerce (utf8-codes pattern) 'vector)) (*rx-pos* 0) (*rx-ngroups* 0))
    (let ((node (%rx-parse-alt)))
      (when (%rx-peek) (%rx-fail "Unmatched ) in regexp ~S" pattern))
      (%make-regex :source pattern :node node :ngroups *rx-ngroups* :fold case-fold))))

(defun %rx-parse-alt ()
  (let ((alts (list (%rx-parse-seq))))
    (loop (unless (%rx-at-p #\|) (return)) (%rx-next) (setq alts (cons (%rx-parse-seq) alts)))
    (if (cdr alts) (cons :alt (nreverse alts)) (car alts))))

(defun %rx-parse-seq ()
  (let ((items nil))
    (loop
      (let ((c (%rx-peek)))
        (when (or (null c) (= c 124) (= c 41)) (return))       ; | or )
        (setq items (cons (%rx-parse-quantified) items))))
    (if (and items (null (cdr items))) (car items) (cons :seq (nreverse items)))))

(defun %rx-parse-int ()
  (let ((n nil))
    (loop (let ((c (%rx-peek))) (if (and c (<= 48 c 57)) (progn (%rx-next) (setq n (+ (* 10 (or n 0)) (- c 48)))) (return))))
    n))

(defun %rx-parse-quantified ()
  (let ((atom (%rx-parse-atom)))
    (loop
      (let ((c (%rx-peek)) (min nil) (max nil))
        (cond ((eql c 42) (%rx-next) (setq min 0 max nil))       ; *
              ((eql c 43) (%rx-next) (setq min 1 max nil))       ; +
              ((eql c 63) (%rx-next) (setq min 0 max 1))         ; ?
              ((eql c 123)                                       ; {n,m}
               (let ((save *rx-pos*))
                 (%rx-next)
                 (let ((a (%rx-parse-int)))
                   (cond ((and a (%rx-at-p #\})) (%rx-next) (setq min a max a))
                         ((and a (%rx-at-p #\,))
                          (%rx-next)
                          (let ((b (%rx-parse-int)))
                            (if (%rx-at-p #\}) (progn (%rx-next) (setq min a max b)) (setq *rx-pos* save))))
                         (t (setq *rx-pos* save))))
                 (when (null min) (return atom))))
              (t (return atom)))
        (when (and max (< max min)) (%rx-fail "Bad repetition {~D,~D}" min max))
        (when (member (car atom) '(:bol :eol :wordb :nwordb)) (%rx-fail "Nothing to repeat"))
        (let ((greedy t))
          (when (%rx-at-p #\?) (%rx-next) (setq greedy nil))
          (setq atom (list :rep min max greedy atom)))))))

(defun %rx-escape-class (c)
  "The class keyword for \\d \\w \\s and their negations, or NIL."
  (case c (100 :digit) (68 '(:not :digit)) (119 :word) (87 '(:not :word)) (115 :space) (83 '(:not :space)) (t nil)))

(defun %rx-escaped-char (c)
  (case c (116 9) (110 10) (114 13) (102 12) (118 11) (101 27) (t c)))

(defun %rx-parse-atom ()
  (let ((c (%rx-next)))
    (cond ((null c) (%rx-fail "Regexp ends unexpectedly"))
          ((= c 40)                                              ; (
           (let ((group nil))
             (if (and (%rx-at-p #\?) (eql (%rx-peek 1) 58)) (progn (%rx-next) (%rx-next))
                 (setq group (setq *rx-ngroups* (+ *rx-ngroups* 1))))
             (let ((inner (%rx-parse-alt)))
               (unless (%rx-at-p #\)) (%rx-fail "Unmatched ( in regexp"))
               (%rx-next)
               (if group (list :group group inner) inner))))
          ((= c 41) (%rx-fail "Unmatched )"))
          ((= c 91) (%rx-parse-class))
          ((= c 46) (list :any))
          ((= c 94) (list :bol))
          ((= c 36) (list :eol))
          ((member c '(42 43 63)) (%rx-fail "Nothing to repeat"))
          ((= c 92)                                              ; backslash
           (let ((e (%rx-next)))
             (cond ((null e) (%rx-fail "Regexp ends with a backslash"))
                   ((%rx-escape-class e) (list :set nil (list (%rx-escape-class e))))
                   ((= e 98) (list :wordb)) ((= e 66) (list :nwordb))
                   ((<= 49 e 57) (list :backref (- e 48)))
                   (t (list :char (%rx-escaped-char e))))))
          (t (list :char c)))))

(defparameter *rx-named-classes*
  '(("alpha" . :alpha) ("digit" . :digit) ("alnum" . :alnum) ("space" . :space) ("upper" . :upper)
    ("lower" . :lower) ("punct" . :punct) ("word" . :word) ("blank" . :blank) ("xdigit" . :xdigit)))

(defun %rx-parse-class ()
  (let ((neg nil) (items nil) (first t))
    (when (%rx-at-p #\^) (%rx-next) (setq neg t))
    (loop
      (let ((c (%rx-next)))
        (cond ((null c) (%rx-fail "Unmatched [ in regexp"))
              ((and (= c 93) (not first)) (return))              ; ]
              ((and (= c 91) (%rx-at-p #\:))                     ; [:name:]
               (%rx-next)
               (let ((name nil))
                 (loop (let ((d (%rx-next)))
                         (cond ((null d) (%rx-fail "Unterminated [: in regexp"))
                               ((and (= d 58) (%rx-at-p #\])) (%rx-next) (return))
                               (t (setq name (cons d name))))))
                 (let ((cls (cdr (assoc (utf8-from-codes (nreverse name)) *rx-named-classes* :test #'string=))))
                   (unless cls (%rx-fail "Unknown character class in regexp"))
                   (setq items (cons cls items)))))
              (t
               (let ((lo c))
                 (when (= c 92)
                   (let ((e (%rx-next)))
                     (when (null e) (%rx-fail "Regexp ends with a backslash"))
                     (if (%rx-escape-class e) (setq lo (%rx-escape-class e)) (setq lo (%rx-escaped-char e)))))
                 (if (and (integerp lo) (%rx-at-p #\-) (%rx-peek 1) (not (eql (%rx-peek 1) 93)))
                     (progn (%rx-next)
                            (let ((hi (%rx-next)))
                              (when (= hi 92) (setq hi (%rx-escaped-char (%rx-next))))
                              (when (< hi lo) (%rx-fail "Bad range in character class"))
                              (setq items (cons (cons lo hi) items))))
                     (setq items (cons lo items))))))
        (setq first nil)))
    (list :set neg (nreverse items))))

;;; ---- matching -----------------------------------------------------------------------------------------------

(defvar *rx-fold* nil)
(defvar *rx-groups* nil)
(defvar *rx-end* 0)

(defun %rx-space-p (c) (or (member c '(32 9 10 11 12 13 #xA0 #x1680 #x2028 #x2029 #x202F #x205F #x3000)) (<= #x2000 c #x200A)))

(defun %rx-class-p (cls c)
  (cond ((consp cls) (if (eq (car cls) :not) (not (%rx-class-p (cadr cls) c)) (<= (car cls) c (cdr cls))))
        ((integerp cls) (= cls c))
        (t (case cls
             (:digit (<= 48 c 57))
             (:word (unicode-word-char-p c))
             (:space (%rx-space-p c))
             (:blank (or (= c 32) (= c 9)))
             (:alpha (and (unicode-word-char-p c) (not (<= 48 c 57)) (/= c 95)))
             (:alnum (and (unicode-word-char-p c) (/= c 95)))
             (:upper (unicode-upper-case-p c))
             (:lower (unicode-lower-case-p c))
             (:xdigit (or (<= 48 c 57) (<= 65 c 70) (<= 97 c 102)))
             (:punct (and (not (unicode-word-char-p c)) (not (%rx-space-p c)) (>= c 33)))
             (t nil)))))

(defun %rx-set-p (neg items c)
  (let ((in (or (some (lambda (it) (%rx-class-p it c)) items)
                (and *rx-fold*
                     (let ((u (unicode-upcase c)) (d (unicode-downcase c)))
                       (some (lambda (it) (or (%rx-class-p it u) (%rx-class-p it d))) items))))))
    (if neg (not in) in)))

(defun %rx-char-eq (a b) (or (= a b) (and *rx-fold* (= (unicode-fold a) (unicode-fold b)))))

(defun %rx-single-p (node) (member (car node) '(:char :any :set)))

(defun %rx-single-match (node s i)
  "If the one-character NODE matches at I, the index after the character, else NIL."
  (when (< i *rx-end*)
    (multiple-value-bind (c next) (utf8-char-at s i)
      (and (case (car node)
             (:char (%rx-char-eq c (cadr node)))
             (:any (/= c 10))
             (:set (%rx-set-p (cadr node) (caddr node) c)))
           next))))

(defun %rx-word-before-p (s i)
  (and (> i 0) (let ((j (- i 1))) (loop (unless (and (> j 0) (= (logand (char-code (char s j)) #xC0) #x80)) (return)) (setq j (- j 1)))
                 (unicode-word-char-p (utf8-char-at s j)))))
(defun %rx-word-after-p (s i) (and (< i *rx-end*) (unicode-word-char-p (utf8-char-at s i))))

(defun %rx (node s i k)
  "Match NODE at I in S, then call K with the index after it; returns what K returns, or NIL."
  (case (car node)
    ((:char :any :set) (let ((j (%rx-single-match node s i))) (and j (funcall k j))))
    (:seq (%rx-seq (cdr node) s i k))
    (:alt (dolist (a (cdr node) nil) (let ((r (%rx a s i k))) (when r (return r)))))
    (:bol (and (= i 0) (funcall k i)))
    (:eol (and (= i *rx-end*) (funcall k i)))
    (:wordb (and (not (eq (%rx-word-before-p s i) (%rx-word-after-p s i))) (funcall k i)))
    (:nwordb (and (eq (%rx-word-before-p s i) (%rx-word-after-p s i)) (funcall k i)))
    (:group
     (let* ((n (cadr node)) (old-s (aref *rx-groups* (* 2 n))) (old-e (aref *rx-groups* (+ 1 (* 2 n)))))
       (or (%rx (caddr node) s i
                (lambda (j)
                  (let ((save-s (aref *rx-groups* (* 2 n))) (save-e (aref *rx-groups* (+ 1 (* 2 n)))))
                    (setf (aref *rx-groups* (* 2 n)) i (aref *rx-groups* (+ 1 (* 2 n))) j)
                    (or (funcall k j)
                        (progn (setf (aref *rx-groups* (* 2 n)) save-s (aref *rx-groups* (+ 1 (* 2 n))) save-e) nil)))))
           (progn (setf (aref *rx-groups* (* 2 n)) old-s (aref *rx-groups* (+ 1 (* 2 n))) old-e) nil))))
    (:backref
     (let* ((n (cadr node)) (gs (and (< (* 2 n) (length *rx-groups*)) (aref *rx-groups* (* 2 n)))))
       (if (null gs)
           (funcall k i)                                         ; an unset group matches the empty string
           (let ((ge (aref *rx-groups* (+ 1 (* 2 n)))) (j i) (p gs))
             (loop
               (when (>= p ge) (return (funcall k j)))
               (when (>= j *rx-end*) (return nil))
               (multiple-value-bind (a pn) (utf8-char-at s p)
                 (multiple-value-bind (b jn) (utf8-char-at s j)
                   (unless (%rx-char-eq a b) (return nil))
                   (setq p pn j jn))))))))
    (:rep (%rx-rep (cadr node) (caddr node) (cadddr node) (car (cddddr node)) s i k))
    (t (error "Bad regexp node ~S" node))))

(defun %rx-seq (nodes s i k)
  (if (null nodes) (funcall k i) (%rx (car nodes) s i (lambda (j) (%rx-seq (cdr nodes) s j k)))))

(defun %rx-rep (min max greedy node s i k)
  (if (and greedy (%rx-single-p node))
      ;; one character at a time: collect the positions, then try the longest first (no deep recursion)
      (let ((stops (list i)) (count 0) (j i))
        (loop (when (and max (>= count max)) (return))
              (let ((nj (%rx-single-match node s j))) (unless nj (return)) (setq j nj count (+ count 1) stops (cons j stops))))
        (loop (when (< count min) (return nil))
              (let ((r (funcall k (car stops)))) (when r (return r)))
              (setq stops (cdr stops) count (- count 1))))
      (labels ((try (count i)
                 (if greedy
                     (or (and (or (null max) (< count max))
                              (%rx node s i (lambda (j) (and (or (> j i) (< count min)) (try (+ count 1) j)))))
                         (and (>= count min) (funcall k i)))
                     (or (and (>= count min) (funcall k i))
                         (and (or (null max) (< count max))
                              (%rx node s i (lambda (j) (and (or (> j i) (< count min)) (try (+ count 1) j)))))))))
        (try 0 i))))

(defun %rx-groups-result (re start end)
  (let ((v (make-array (+ 1 (regex-ngroups re)) :initial-element nil)))
    (setf (aref v 0) (cons start end))
    (dotimes (n (regex-ngroups re))
      (let ((gs (aref *rx-groups* (* 2 (+ n 1)))))
        (when gs (setf (aref v (+ n 1)) (cons gs (aref *rx-groups* (+ 1 (* 2 (+ n 1)))))))))
    v))

(defun regex-match (re string start &optional (end (length string)))
  "If RE matches STRING at byte START (not going past END), return (values match-end groups), where GROUPS
is a vector of (start . end) byte ranges (element 0 is the whole match; NIL for a group that did not match)."
  (let ((*rx-fold* (regex-fold re)) (*rx-end* end)
        (*rx-groups* (make-array (* 2 (+ 1 (regex-ngroups re)))) :initial-element nil))
    (let ((e (%rx (regex-node re) string start (lambda (j) j))))
      (and e (values e (%rx-groups-result re start e))))))

(defun regex-find (re string &optional (start 0) (end (length string)))
  "Find the first match of RE in STRING at or after byte START.  Returns (values match-start match-end groups),
or NIL."
  (let ((i start))
    (loop
      (when (> i end) (return nil))
      (multiple-value-bind (e groups) (regex-match re string i end)
        (when e (return (values i e groups))))
      (when (>= i end) (return nil))
      (setq i (nth-value 1 (utf8-char-at string i))))))

(defun regex-find-backward (re string limit)
  "Find the last match of RE in STRING that ends at or before byte LIMIT.  Returns (values start end groups)."
  (let ((i (min limit (length string))))
    (loop
      (multiple-value-bind (e groups) (regex-match re string i limit)
        (when e (return (values i e groups))))
      (when (<= i 0) (return nil))
      (setq i (- i 1))
      (loop (unless (and (> i 0) (= (logand (char-code (char string i)) #xC0) #x80)) (return)) (setq i (- i 1))))))

(defun regex-expand (template string groups)
  "The replacement TEMPLATE with \\& or \\0 replaced by the whole match in STRING and \\1 .. \\9 by the groups
(from REGEX-SEARCH); \\\\ is a backslash."
  (let ((out (make-string-output-stream)) (i 0) (n (length template)))
    (loop
      (when (>= i n) (return))
      (let ((c (char template i)))
        (if (and (char= c #\\) (< (+ i 1) n))
            (let ((d (char template (+ i 1))))
              (cond ((or (char= d #\&) (digit-char-p d))
                     (let* ((k (if (char= d #\&) 0 (digit-char-p d))) (g (and (< k (length groups)) (aref groups k))))
                       (when g (write-string (subseq string (car g) (cdr g)) out))))
                    ((char= d #\n) (write-char #\Newline out))
                    ((char= d #\t) (write-char (code-char 9) out))
                    (t (write-char d out)))
              (setq i (+ i 2)))
            (progn (write-char c out) (setq i (+ i 1))))))
    (get-output-stream-string out)))

(defun regex-replace-all (re string template)
  "STRING with every match of RE replaced by TEMPLATE (see REGEX-EXPAND).  Returns (values new-string count)."
  (let ((out (make-string-output-stream)) (pos 0) (count 0) (n (length string)))
    (loop
      (multiple-value-bind (ms me groups) (regex-find re string pos)
        (unless ms (write-string (subseq string pos) out) (return))
        (write-string (subseq string pos ms) out)
        (write-string (regex-expand template string groups) out)
        (setq count (+ count 1))
        (cond ((> me ms) (setq pos me))
              ((>= me n) (setq pos n) (write-string "" out) (return))
              (t (let ((nx (nth-value 1 (utf8-char-at string me)))) (write-string (subseq string me nx) out) (setq pos nx))))))
    (values (get-output-stream-string out) count)))

(defun string-match-p (pattern string &key case-fold)
  "True if the regular expression PATTERN matches somewhere in STRING."
  (and (regex-find (regex-compile pattern :case-fold case-fold) string) t))
