;;;; 10-parser.lisp -- the FORTRAN Tool Kit: statements and program units.
;;;;
;;;; FORTRAN has no reserved words, so a statement is classified from its squeezed text (blanks removed): first as
;;;; a DO statement or an assignment, then by its leading keyword.  Each statement becomes an FNODE (kind, label,
;;;; source line and byte offsets, arguments).  Expressions are lists: (:int n) (:real x) (:dreal x) (:str s)
;;;; (:logical b) (:complex re im) (:name n) (:ref n items) (:substr base lo hi) (:paren e) (:bin op a b) (:un op a).
;;;; A source text is a sequence of program units (PROGRAM, SUBROUTINE, FUNCTION, BLOCK DATA), each ending with END;
;;;; text before the first unit header is the unnamed main program.  Syntax errors are collected per statement so
;;;; one parse reports many.

(in-package :cl)

(defstruct (fnode (:conc-name fn-) (:constructor %make-fnode) (:copier nil)) kind label line start end args)

(defmethod print-object ((n fnode) s) (format s "#<Fortran ~(~A~) line ~D>" (fn-kind n) (fn-line n)))

(defstruct (fast (:conc-name fu-) (:constructor %make-fast) (:copier nil))
  kind name params type stmts start end line)

(defmethod print-object ((u fast) s) (format s "#<Fortran ~(~A~) ~A>" (fu-kind u) (or (fu-name u) "")))

(defvar *f7t* (vector) "The tokens of the statement being parsed.")
(defvar *f7p* 0 "The position in *F7T*.")
(defvar *f7stmt* nil "The FSTMT being parsed.")

(defun %f7-error (fmt &rest args)
  (let ((s *f7stmt*))
    (error 'f77-syntax-error :message (let ((m (apply #'format nil fmt args)))
                                        (if (and s (fs-trunc s)) (%string-concat m " (text beyond column 72 is ignored)") m))
                             :start (and s (fs-start s)) :end (and s (fs-end s)) :line (and s (fs-line s)) :col (and s (fs-col s))
                             :file (and s (fs-file s)))))

(defun %f7-cur () (if (< *f7p* (length *f7t*)) (svref *f7t* *f7p*) nil))
(defun %f7-at (k) (if (< (+ *f7p* k) (length *f7t*)) (svref *f7t* (+ *f7p* k)) nil))
(defun %f7-adv () (let ((tk (%f7-cur))) (setq *f7p* (+ *f7p* 1)) tk))
(defun %f7-op-p (s &optional (k 0)) (let ((tk (%f7-at k))) (and tk (eq (car tk) :op) (string= (cdr tk) s))))
(defun %f7-dot-p (kw) (let ((tk (%f7-cur))) (and tk (eq (car tk) :dot) (eq (cdr tk) kw))))
(defun %f7-end-p () (>= *f7p* (length *f7t*)))
(defun %f7-describe (tk)
  (cond ((null tk) "the end of the statement")
        ((eq (car tk) :str) (format nil "the character constant ~S" (cdr tk)))
        ((eq (car tk) :dot) (format nil ".~A." (cdr tk)))
        (t (format nil "~A" (cdr tk)))))
(defun %f7-expect-op (s)
  (if (%f7-op-p s) (%f7-adv) (%f7-error "\"~A\" expected, not ~A" s (%f7-describe (%f7-cur)))))
(defun %f7-name ()
  (let ((tk (%f7-cur)))
    (if (and tk (eq (car tk) :name)) (progn (%f7-adv) (cdr tk)) (%f7-error "A name was expected, not ~A" (%f7-describe tk)))))
(defun %f7-expect-end ()
  (unless (%f7-end-p) (%f7-error "Unexpected ~A at the end of the statement" (%f7-describe (%f7-cur)))))

(defmacro %f7-with-tokens ((str) &body body)
  `(let ((*f7t* (coerce (f77-tokenize ,str) 'vector)) (*f7p* 0)) ,@body))

;;; ---- expressions -----------------------------------------------------------------------------------------

(defun %f7-parse-expr () (%f7-parse-eqv))

(defmacro %f7-left-assoc (name next kinds)
  `(defun ,name ()
     (let ((a (,next)))
       (loop
         (let ((tk (%f7-cur)))
           (if (and tk (eq (car tk) :dot) (member (cdr tk) ',kinds))
               (progn (%f7-adv) (setq a (list :bin (cdr tk) a (,next))))
               (return a)))))))

(%f7-left-assoc %f7-parse-eqv %f7-parse-or (:eqv :neqv))
(%f7-left-assoc %f7-parse-or %f7-parse-and (:or :xor))
(%f7-left-assoc %f7-parse-and %f7-parse-not (:and))

(defun %f7-parse-not ()
  (if (%f7-dot-p :not) (progn (%f7-adv) (list :un :not (%f7-parse-not))) (%f7-parse-rel)))

(defun %f7-parse-rel ()
  (let ((a (%f7-parse-concat)) (tk (%f7-cur)))
    (cond ((and tk (eq (car tk) :dot) (member (cdr tk) '(:eq :ne :lt :le :gt :ge)))
           (%f7-adv) (list :bin (cdr tk) a (%f7-parse-concat)))
          (t a))))

(defun %f7-parse-concat ()
  (let ((a (%f7-parse-add)))
    (loop (if (%f7-op-p "//") (progn (%f7-adv) (setq a (list :bin :// a (%f7-parse-add)))) (return a)))))

(defun %f7-parse-add ()
  (let ((a (cond ((%f7-op-p "-") (%f7-adv) (list :un :- (%f7-parse-term)))
                 ((%f7-op-p "+") (%f7-adv) (%f7-parse-term))
                 (t (%f7-parse-term)))))
    (loop
      (cond ((%f7-op-p "+") (%f7-adv) (setq a (list :bin :+ a (%f7-parse-term))))
            ((%f7-op-p "-") (%f7-adv) (setq a (list :bin :- a (%f7-parse-term))))
            (t (return a))))))

(defun %f7-parse-term ()
  (let ((a (%f7-parse-factor)))
    (loop
      (cond ((%f7-op-p "*") (%f7-adv) (setq a (list :bin :* a (%f7-parse-factor))))
            ((%f7-op-p "/") (%f7-adv) (setq a (list :bin :/ a (%f7-parse-factor))))
            (t (return a))))))

(defun %f7-parse-factor ()
  (let ((a (%f7-parse-primary)))
    (if (%f7-op-p "**")
        (progn (%f7-adv)
               (list :bin :** a (cond ((%f7-op-p "-") (%f7-adv) (list :un :- (%f7-parse-factor)))
                                      ((%f7-op-p "+") (%f7-adv) (%f7-parse-factor))
                                      (t (%f7-parse-factor)))))
        a)))

(defun %f7-parse-subscripts ()
  "At \"(\": the items up to the matching \")\": expressions and (:slice lo hi)."
  (%f7-expect-op "(")
  (let ((items nil))
    (unless (%f7-op-p ")")
      (loop
        (let ((item (if (%f7-op-p ":")
                        (progn (%f7-adv) (list :slice nil (if (or (%f7-op-p ",") (%f7-op-p ")")) nil (%f7-parse-expr))))
                        (let ((e (%f7-parse-expr)))
                          (if (%f7-op-p ":")
                              (progn (%f7-adv) (list :slice e (if (or (%f7-op-p ",") (%f7-op-p ")")) nil (%f7-parse-expr))))
                              e)))))
          (push item items))
        (if (%f7-op-p ",") (%f7-adv) (return))))
    (%f7-expect-op ")")
    (nreverse items)))

(defun %f7-parse-primary ()
  (let ((tk (%f7-cur)))
    (unless tk (%f7-error "An expression was expected at the end of the statement"))
    (case (car tk)
      (:int (%f7-adv) (list :int (cdr tk)))
      (:real (%f7-adv) (list :real (cdr tk)))
      (:dreal (%f7-adv) (list :dreal (cdr tk)))
      (:str (%f7-adv) (list :str (cdr tk)))
      (:logical (%f7-adv) (list :logical (cdr tk)))
      (:name
       (%f7-adv)
       (let ((base (if (%f7-op-p "(") (list :ref (cdr tk) (%f7-parse-subscripts)) (list :name (cdr tk)))))
         (if (and (%f7-op-p "(") (eq (car base) :ref))
             ;; a substring of an array element or function value: A(I)(2:3)
             (let ((items (%f7-parse-subscripts)))
               (unless (and items (null (cdr items)) (eq (car (car items)) :slice)) (%f7-error "A substring range lo:hi was expected"))
               (list :substr base (cadr (car items)) (caddr (car items))))
             base)))
      (:op
       (cond ((string= (cdr tk) "(")
              (%f7-adv)
              (let ((e (%f7-parse-expr)))
                (if (%f7-op-p ",")
                    (progn (%f7-adv) (let ((im (%f7-parse-expr))) (%f7-expect-op ")") (list :complex e im)))
                    (progn (%f7-expect-op ")") (list :paren e)))))
             (t (%f7-error "An expression was expected, not ~A" (%f7-describe tk)))))
      (t (%f7-error "An expression was expected, not ~A" (%f7-describe tk))))))

(defun %f7-parse-lvalue ()
  (let ((e (%f7-parse-primary)))
    (unless (member (car e) '(:name :ref :substr)) (%f7-error "A variable was expected"))
    e))

;;; ---- text helpers ----------------------------------------------------------------------------------------

(defun %f7-match-paren (s i)
  "The index of the \")\" matching the \"(\" at index I of S (outside character constants), or NIL."
  (let ((depth 0) (q nil) (n (length s)))
    (do ((k i (+ k 1))) ((>= k n) nil)
      (let ((c (char s k)))
        (cond (q (when (char= c #\') (setq q nil)))
              ((char= c #\') (setq q t))
              ((char= c #\() (setq depth (+ depth 1)))
              ((char= c #\)) (setq depth (- depth 1)) (when (= depth 0) (return k))))))))

(defun %f7-find-depth0 (s ch &optional (start 0))
  "The index of the first CH at parenthesis depth 0 (outside character constants) at or after START."
  (let ((depth 0) (q nil) (n (length s)))
    (do ((k start (+ k 1))) ((>= k n) nil)
      (let ((c (char s k)))
        (cond (q (when (char= c #\') (setq q nil)))
              ((char= c #\') (setq q t))
              ((char= c #\() (setq depth (+ depth 1)))
              ((char= c #\)) (setq depth (- depth 1)))
              ((and (= depth 0) (char= c ch)) (return k)))))))

(defun %f7-starts (s kw) (and (>= (length s) (length kw)) (string= (subseq s 0 (length kw)) kw)))
(defun %f7-name-string-p (s)
  (and (> (length s) 0) (alpha-char-p (char s 0)) (every #'%f7-name-char-p s)))
(defun %f7-digits-p (s) (and (> (length s) 0) (every #'digit-char-p s)))

;;; ---- assignment and DO detection -------------------------------------------------------------------------

(defun %f7-assignment-p (s)
  "True when the squeezed statement S is an assignment: a variable, array element or substring before a depth-0 =."
  (let ((eq (%f7-find-depth0 s #\=)))
    (and eq (> eq 0)
         (alpha-char-p (char s 0))
         (let ((i 0) (n eq))
           (loop (when (or (>= i n) (not (%f7-name-char-p (char s i)))) (return)) (setq i (+ i 1)))
           (let ((groups 0))
             (loop
               (cond ((= i n) (return t))
                     ((and (char= (char s i) #\() (< groups 2))
                      (let ((m (%f7-match-paren s i)))
                        (if (and m (< m n)) (setq i (+ m 1) groups (+ groups 1)) (return nil))))
                     (t (return nil)))))))))

(defun %f7-do-parts (s)
  "For a DO statement: (values kind label rest) with KIND :DO or :DOWHILE; NIL for anything else."
  (when (and (> (length s) 2) (string= (subseq s 0 2) "DO"))
    (let* ((n (length s)) (i 2) (j i))
      (loop (when (or (>= j n) (not (digit-char-p (char s j)))) (return)) (setq j (+ j 1)))
      (let ((label (if (> j i) (parse-integer s :start i :end j) nil)))
        (when (and (< j n) (char= (char s j) #\,)) (setq j (+ j 1)))
        (let ((rest (subseq s j)))
          (cond
            ((and (>= (length rest) 6) (string= (subseq rest 0 6) "WHILE(") (eql (%f7-match-paren rest 5) (- (length rest) 1)))
             (values :dowhile label rest))
            (t
             (let ((eq (%f7-find-depth0 rest #\=)))
               (when (and eq (%f7-name-string-p (subseq rest 0 eq)) (%f7-find-depth0 rest #\, eq))
                 (values :do label rest))))))))))

;;; ---- declarations ----------------------------------------------------------------------------------------

(defparameter *f77-type-keywords*
  '(("DOUBLEPRECISION" . :double) ("DOUBLECOMPLEX" . :dcomplex) ("INTEGER" . :integer) ("REAL" . :real)
    ("COMPLEX" . :complex) ("LOGICAL" . :logical) ("CHARACTER" . :character)))

(defun %f7-type-keyword (s)
  "(values kind keyword-length) when S begins with a type keyword."
  (dolist (k *f77-type-keywords* nil)
    (when (%f7-starts s (car k)) (return (values (cdr k) (length (car k)))))))

(defun %f7-length-spec ()
  "After a `*`: an integer, (expr) or (*): the length or kind."
  (cond ((%f7-op-p "(")
         (%f7-adv)
         (if (%f7-op-p "*")
             (progn (%f7-adv) (%f7-expect-op ")") :star)
             (let ((e (%f7-parse-expr))) (%f7-expect-op ")") e)))
        ((and (%f7-cur) (eq (car (%f7-cur)) :int)) (list :int (cdr (%f7-adv))))
        (t (%f7-error "A length was expected after *"))))

(defun %f7-resolve-kind (kind size)
  "The kind for REAL*8, COMPLEX*16 and so on."
  (let ((n (and size (listp size) (eq (car size) :int) (cadr size))))
    (case kind
      (:real (if (and n (>= n 8)) :double :real))
      (:complex (if (and n (>= n 16)) :dcomplex :complex))
      (t kind))))

(defun %f7-parse-typespec (kind rest)
  "Rest of a type statement after the type keyword: (values (:type kind len) remainder-string)."
  (let ((len nil) (r rest))
    (when (and (> (length r) 0) (char= (char r 0) #\*))
      ;; a length or kind: tokenize just the part we need
      (let ((end (cond ((and (> (length r) 1) (char= (char r 1) #\()) (+ 1 (%f7-match-paren r 1)))
                       (t (let ((j 1)) (loop (when (or (>= j (length r)) (not (digit-char-p (char r j)))) (return)) (setq j (+ j 1))) j)))))
        (%f7-with-tokens ((subseq r 1 end)) (setq len (%f7-length-spec)))
        (setq r (subseq r end))))
    (values (list :type (if (eq kind :character) :character (%f7-resolve-kind kind len)) (if (eq kind :character) (or len '(:int 1)) nil))
            r)))

(defun %f7-parse-dims ()
  "At \"(\": the dimension declarators, a list of (lo hi), HI being an expression or :STAR."
  (%f7-expect-op "(")
  (let ((dims nil))
    (loop
      (let ((d (if (%f7-op-p "*")
                   (progn (%f7-adv) (list nil :star))
                   (let ((e (%f7-parse-expr)))
                     (if (%f7-op-p ":")
                         (progn (%f7-adv) (list e (if (%f7-op-p "*") (progn (%f7-adv) :star) (%f7-parse-expr))))
                         (list nil e))))))
        (push d dims))
      (if (%f7-op-p ",") (%f7-adv) (return)))
    (%f7-expect-op ")")
    (nreverse dims)))

(defun %f7-parse-declarators (&optional (allow-init t))
  "name[(dims)][*len][/values/] items separated by commas: a list of (name dims len values)."
  (let ((items nil))
    (loop
      (let ((name (%f7-name)) (dims nil) (len nil) (vals nil))
        (when (%f7-op-p "(") (setq dims (%f7-parse-dims)))
        (when (%f7-op-p "*") (%f7-adv) (setq len (%f7-length-spec)))
        (when (and allow-init (%f7-op-p "/")) (%f7-adv) (setq vals (%f7-parse-data-values)))
        (push (list name dims len vals) items))
      (if (%f7-op-p ",") (%f7-adv) (return)))
    (nreverse items)))

(defun %f7-parse-implicit-stmt (r)
  "The IMPLICIT statement with text R after the keyword: (:implicit ((kind len ((from . to)...))...))."
  (let ((specs nil) (rest r))
    (loop
      (when (string= rest "") (return))
      (multiple-value-bind (kind klen) (%f7-type-keyword rest)
        (unless kind (%f7-error "A type was expected in the IMPLICIT statement"))
        (multiple-value-bind (ty after) (%f7-parse-typespec kind (subseq rest klen))
          (unless (and (> (length after) 0) (char= (char after 0) #\())
            (%f7-error "A letter range (a-z) was expected in the IMPLICIT statement"))
          (let* ((close (%f7-match-paren after 0)) (ranges nil))
            (unless close (%f7-error "Unbalanced parentheses in the IMPLICIT statement"))
            (dolist (part (%split-string (subseq after 1 close) #\,))
              (cond ((and (= (length part) 1) (alpha-char-p (char part 0))) (push (cons (char part 0) (char part 0)) ranges))
                    ((and (= (length part) 3) (char= (char part 1) #\-) (alpha-char-p (char part 0)) (alpha-char-p (char part 2)))
                     (push (cons (char part 0) (char part 2)) ranges))
                    (t (%f7-error "Bad letter range ~A in the IMPLICIT statement" part))))
            (push (list (cadr ty) (caddr ty) (nreverse ranges)) specs)
            (setq rest (subseq after (+ close 1)))
            (when (and (> (length rest) 0) (char= (char rest 0) #\,)) (setq rest (subseq rest 1)))))))
    (list :implicit (nreverse specs))))

;;; ---- DATA, PARAMETER, COMMON, EQUIVALENCE ----------------------------------------------------------------------

(defun %f7-implied-do-ahead-p ()
  "True when the \"(\" at the current token opens an implied DO: a top-level \", NAME =\" occurs inside it."
  (let ((depth 0) (k 0))
    (loop
      (let ((tk (%f7-at k)))
        (when (null tk) (return nil))
        (cond ((and (eq (car tk) :op) (string= (cdr tk) "(")) (setq depth (+ depth 1)))
              ((and (eq (car tk) :op) (string= (cdr tk) ")")) (setq depth (- depth 1)) (when (<= depth 0) (return nil)))
              ((and (= depth 1) (eq (car tk) :op) (string= (cdr tk) ",")
                    (%f7-at (+ k 1)) (eq (car (%f7-at (+ k 1))) :name) (%f7-op-p "=" (+ k 2)))
               (return t)))
        (setq k (+ k 1))))))

(defun %f7-parse-implied-do (item-parser)
  "At \"(\": (:implied-do items var from to step)."
  (%f7-expect-op "(")
  (let ((items nil))
    (loop
      (if (and (%f7-at 0) (eq (car (%f7-at 0)) :name) (%f7-op-p "=" 1))
          (return)
          (progn (push (funcall item-parser) items)
                 (%f7-expect-op ","))))
    (let ((var (%f7-name)))
      (%f7-expect-op "=")
      (let ((from (%f7-parse-expr)) to step)
        (%f7-expect-op ",")
        (setq to (%f7-parse-expr))
        (when (%f7-op-p ",") (%f7-adv) (setq step (%f7-parse-expr)))
        (%f7-expect-op ")")
        (list :implied-do (nreverse items) var from to step)))))

(defun %f7-parse-data-target ()
  (if (and (%f7-op-p "(") (%f7-implied-do-ahead-p))
      (%f7-parse-implied-do #'%f7-parse-data-target)
      (%f7-parse-lvalue)))

(defun %f7-parse-data-values ()
  "Values of a DATA list up to the closing /: a list of (repeat . node)."
  (let ((vals nil))
    (loop
      (let ((repeat 1))
        (when (and (%f7-at 0) (eq (car (%f7-at 0)) :int) (%f7-op-p "*" 1))
          (setq repeat (cdr (%f7-adv))) (%f7-adv))
        (let ((v (cond ((%f7-op-p "-") (%f7-adv) (list :un :- (%f7-parse-primary)))
                       ((%f7-op-p "+") (%f7-adv) (%f7-parse-primary))
                       (t (%f7-parse-primary)))))
          (push (cons repeat v) vals)))
      (cond ((%f7-op-p ",") (%f7-adv))
            ((%f7-op-p "/") (%f7-adv) (return))
            (t (%f7-error "\",\" or \"/\" expected in a DATA value list, not ~A" (%f7-describe (%f7-cur))))))
    (nreverse vals)))

(defun %f7-parse-data ()
  (let ((groups nil))
    (loop
      (let ((targets nil))
        (loop (push (%f7-parse-data-target) targets)
              (if (%f7-op-p ",") (%f7-adv) (return)))
        (%f7-expect-op "/")
        (push (list (nreverse targets) (%f7-parse-data-values)) groups))
      (when (%f7-op-p ",") (%f7-adv))
      (when (%f7-end-p) (return)))
    (list :data (nreverse groups))))

(defun %f7-parse-parameter ()
  (let ((paren (%f7-op-p "(")) (items nil))
    (when paren (%f7-adv))
    (loop
      (let ((name (%f7-name)))
        (%f7-expect-op "=")
        (push (list name (%f7-parse-expr)) items))
      (if (%f7-op-p ",") (%f7-adv) (return)))
    (when paren (%f7-expect-op ")"))
    (list :parameter (nreverse items))))

(defun %f7-parse-common ()
  "COMMON /name/ list, /name/ list: (:common ((blockname . declarators)...)); blank common has the name \"\"."
  (let ((blocks nil) (name ""))
    (loop
      (cond ((%f7-op-p "//") (%f7-adv) (setq name ""))
            ((%f7-op-p "/")
             (%f7-adv)
             (setq name (if (%f7-op-p "/") "" (%f7-name)))
             (%f7-expect-op "/")))
      (let ((items (%f7-parse-declarators nil)))
        (let ((cell (assoc name blocks :test #'string=)))
          (if cell (setf (cdr cell) (append (cdr cell) items)) (push (cons name items) blocks))))
      (when (%f7-op-p ",") (%f7-adv))
      (when (%f7-end-p) (return)))
    (list :common (nreverse blocks))))

(defun %f7-parse-equivalence ()
  (let ((groups nil))
    (loop
      (%f7-expect-op "(")
      (let ((items nil))
        (loop (push (%f7-parse-lvalue) items) (if (%f7-op-p ",") (%f7-adv) (return)))
        (%f7-expect-op ")")
        (when (< (length items) 2) (%f7-error "An EQUIVALENCE group needs at least two names"))
        (push (nreverse items) groups))
      (if (%f7-op-p ",") (%f7-adv) (return)))
    (list :equivalence (nreverse groups))))

(defun %f7-parse-name-list ()
  (let ((names nil))
    (loop (push (%f7-name) names) (if (%f7-op-p ",") (%f7-adv) (return)))
    (nreverse names)))

;;; ---- input/output ----------------------------------------------------------------------------------------------

(defun %f7-parse-io-item ()
  (if (and (%f7-op-p "(") (%f7-implied-do-ahead-p))
      (%f7-parse-implied-do #'%f7-parse-io-item)
      (%f7-parse-expr)))

(defun %f7-parse-io-items ()
  (let ((items nil))
    (unless (%f7-end-p)
      (loop (push (%f7-parse-io-item) items) (if (%f7-op-p ",") (%f7-adv) (return))))
    (nreverse items)))

(defun %f7-io-value ()
  (if (%f7-op-p "*") (progn (%f7-adv) :star) (%f7-parse-expr)))

(defun %f7-parse-control (positional)
  "A parenthesized control list: a plist, POSITIONAL naming the keys of leading unnamed items."
  (%f7-expect-op "(")
  (let ((plist nil) (pos positional))
    (loop
      (let ((key (if (and (%f7-at 0) (eq (car (%f7-at 0)) :name) (%f7-op-p "=" 1))
                     (prog1 (intern (cdr (%f7-adv)) "KEYWORD") (%f7-adv))
                     (or (pop pos) (%f7-error "Too many items in the control list")))))
        (setf (getf plist key) (if (member key '(:unit :fmt)) (%f7-io-value) (%f7-parse-expr))))
      (if (%f7-op-p ",") (%f7-adv) (return)))
    (%f7-expect-op ")")
    plist))

(defun %f7-parse-simple-control ()
  "REWIND u, REWIND (UNIT=u, IOSTAT=i): a plist."
  (if (%f7-op-p "(") (%f7-parse-control '(:unit)) (list :unit (%f7-parse-expr))))

;;; ---- statements -------------------------------------------------------------------------------------------------

(defun %f7-parse-label-list ()
  "(10,20,30) -> (10 20 30)"
  (%f7-expect-op "(")
  (let ((labels nil))
    (loop
      (let ((tk (%f7-cur)))
        (unless (and tk (eq (car tk) :int)) (%f7-error "A statement label was expected, not ~A" (%f7-describe tk)))
        (push (cdr (%f7-adv)) labels))
      (if (%f7-op-p ",") (%f7-adv) (return)))
    (%f7-expect-op ")")
    (nreverse labels)))

(defun %f7-parse-call-args ()
  "Arguments of CALL: expressions and (:altret n) for *n or &n."
  (let ((args nil))
    (when (%f7-op-p "(")
      (%f7-adv)
      (unless (%f7-op-p ")")
        (loop
          (if (or (%f7-op-p "*") (%f7-op-p "&"))
              (progn (%f7-adv)
                     (let ((tk (%f7-cur)))
                       (unless (and tk (eq (car tk) :int)) (%f7-error "A label was expected after * in the argument list"))
                       (%f7-adv) (push (list :altret (cdr tk)) args)))
              (push (%f7-parse-expr) args))
          (if (%f7-op-p ",") (%f7-adv) (return))))
      (%f7-expect-op ")"))
    (nreverse args)))

(defun %f7-parse-params ()
  "(a, b, *) after a SUBROUTINE or FUNCTION name: names, with :STAR for alternate returns."
  (let ((params nil))
    (when (%f7-op-p "(")
      (%f7-adv)
      (unless (%f7-op-p ")")
        (loop (push (if (%f7-op-p "*") (progn (%f7-adv) :star) (%f7-name)) params)
              (if (%f7-op-p ",") (%f7-adv) (return))))
      (%f7-expect-op ")"))
    (nreverse params)))

(defun %f7-parse-if (s)
  "IF statements: block IF, arithmetic IF and logical IF."
  (let ((close (%f7-match-paren s 2)))
    (unless (and (> (length s) 2) (char= (char s 2) #\() close) (%f7-error "Unbalanced parentheses in the IF statement"))
    (let ((cond-text (subseq s 2 (+ close 1))) (rest (subseq s (+ close 1))))
      (let ((cexpr (%f7-with-tokens (cond-text)
                     (%f7-expect-op "(") (prog1 (%f7-parse-expr) (%f7-expect-op ")") (%f7-expect-end)))))
        (cond ((string= rest "THEN") (list :if cexpr))
              ((= (length rest) 0) (%f7-error "The IF statement has nothing to do"))
              ((let ((parts (%split-string rest #\,))) (and (= (length parts) 3) (every #'%f7-digits-p parts)))
               (let ((parts (%split-string rest #\,)))
                 (list :aif cexpr (parse-integer (first parts)) (parse-integer (second parts)) (parse-integer (third parts)))))
              (t (list :lif cexpr (%f7-classify rest))))))))

(defun %f7-parse-goto (r)
  (cond ((%f7-digits-p r) (list :goto (parse-integer r)))
        ((char= (char r 0) #\()
         (%f7-with-tokens (r)
           (let ((labels (%f7-parse-label-list)))
             (when (%f7-op-p ",") (%f7-adv))
             (let ((e (%f7-parse-expr))) (%f7-expect-end) (list :cgoto labels e)))))
        (t (%f7-with-tokens (r)
             (let ((name (%f7-name)) (labels nil))
               (when (%f7-op-p ",") (%f7-adv))
               (when (%f7-op-p "(") (setq labels (%f7-parse-label-list)))
               (%f7-expect-end)
               (list :agoto name labels))))))

(defun %f7-parse-read-write (kind r)
  "READ, WRITE, PRINT: (kind control items)."
  (cond
    ((eq kind :print)
     (%f7-with-tokens (r)
       (let ((fmt (%f7-io-value)))
         (let ((items (if (%f7-op-p ",") (progn (%f7-adv) (%f7-parse-io-items)) nil)))
           (%f7-expect-end)
           (list :print (list :unit :star :fmt fmt) items)))))
    ((and (> (length r) 0) (char= (char r 0) #\())
     (let ((close (%f7-match-paren r 0)))
       (unless close (%f7-error "Unbalanced parentheses in the ~(~A~) statement" kind))
       (let ((control (%f7-with-tokens ((subseq r 0 (+ close 1))) (%f7-parse-control '(:unit :fmt)))))
         (let ((items (%f7-with-tokens ((subseq r (+ close 1))) (prog1 (%f7-parse-io-items) (%f7-expect-end)))))
           (list kind control items)))))
    (t ;; READ fmt, items
     (%f7-with-tokens (r)
       (let ((fmt (%f7-io-value)))
         (let ((items (if (%f7-op-p ",") (progn (%f7-adv) (%f7-parse-io-items)) nil)))
           (%f7-expect-end)
           (list kind (list :unit :star :fmt fmt) items)))))))

(defun %f7-parse-header (kind r)
  "SUBROUTINE, FUNCTION, PROGRAM and BLOCK DATA header lines."
  (%f7-with-tokens (r)
    (ecase kind
      (:subroutine (let* ((name (%f7-name)) (params (%f7-parse-params))) (%f7-expect-end) (list :subroutine name params)))
      (:program (let ((name (%f7-name))) (%f7-expect-end) (list :program name)))
      (:blockdata (let ((name (if (%f7-end-p) nil (%f7-name)))) (%f7-expect-end) (list :blockdata name))))))

(defun %f7-parse-function-header (type r)
  (%f7-with-tokens (r)
    (let* ((name (%f7-name)) (params (%f7-parse-params)))
      (%f7-expect-end)
      (list :function type name params))))

(defun %f7-classify (s)
  "The statement node (kind . args) for the squeezed statement text S."
  (let ((n (length s)))
    (when (= n 0) (return-from %f7-classify (list :continue)))
    (multiple-value-bind (dkind dlabel drest) (%f7-do-parts s)
      (when dkind
        (return-from %f7-classify
          (if (eq dkind :dowhile)
              (%f7-with-tokens ((subseq drest 5))
                (%f7-expect-op "(") (let ((e (%f7-parse-expr))) (%f7-expect-op ")") (%f7-expect-end) (list :dowhile dlabel e)))
              (%f7-with-tokens (drest)
                (let ((var (%f7-name)))
                  (%f7-expect-op "=")
                  (let ((from (%f7-parse-expr)) to step)
                    (%f7-expect-op ",")
                    (setq to (%f7-parse-expr))
                    (when (%f7-op-p ",") (%f7-adv) (setq step (%f7-parse-expr)))
                    (%f7-expect-end)
                    (list :do dlabel var from to step))))))))
    (when (%f7-assignment-p s)
      (let ((eq (%f7-find-depth0 s #\=)))
        (return-from %f7-classify
          (let ((lhs (%f7-with-tokens ((subseq s 0 eq)) (prog1 (%f7-parse-lvalue) (%f7-expect-end))))
                (rhs (%f7-with-tokens ((subseq s (+ eq 1))) (prog1 (%f7-parse-expr) (%f7-expect-end)))))
            (list :assign lhs rhs)))))
    ;; leading keyword
    (macrolet ((kw (word &body body) `(when (%f7-starts s ,word) (let ((r (subseq s ,(length word)))) (declare (ignorable r)) (return-from %f7-classify (progn ,@body))))))
      (kw "ENDFILE" (list :endfile (%f7-with-tokens (r) (prog1 (%f7-parse-simple-control) (%f7-expect-end)))))
      (kw "ENDIF" (list :endif))
      (kw "ENDDO" (list :enddo))
      (kw "ELSEIF"
          (let ((close (and (> (length r) 0) (char= (char r 0) #\() (%f7-match-paren r 0))))
            (unless (and close (string= (subseq r (+ close 1)) "THEN")) (%f7-error "ELSE IF (condition) THEN expected"))
            (list :elseif (%f7-with-tokens ((subseq r 0 (+ close 1))) (%f7-expect-op "(") (prog1 (%f7-parse-expr) (%f7-expect-op ")"))))))
      (kw "ELSE" (if (string= r "") (list :else) (%f7-error "Unexpected text after ELSE")))
      (when (and (%f7-starts s "END")
                 (member (subseq s 3) '("" "SUBROUTINE" "FUNCTION" "PROGRAM" "BLOCKDATA") :test #'string=))
        (return-from %f7-classify (list :end)))
      (when (and (%f7-starts s "END")
                 (some (lambda (w) (%f7-starts (subseq s 3) w)) '("SUBROUTINE" "FUNCTION" "PROGRAM" "BLOCKDATA")))
        (return-from %f7-classify (list :end)))
      (kw "PROGRAM" (%f7-parse-header :program r))
      (kw "SUBROUTINE" (%f7-parse-header :subroutine r))
      (kw "BLOCKDATA" (%f7-parse-header :blockdata r))
      (kw "FUNCTION" (%f7-parse-function-header nil r))
      (kw "ENTRY" (%f7-error "The ENTRY statement is not supported"))
      (kw "IMPLICIT" (if (string= r "NONE") (list :implicit-none) (%f7-parse-implicit-stmt r)))
      (kw "DIMENSION" (%f7-with-tokens (r) (prog1 (list :dimension (%f7-parse-declarators)) (%f7-expect-end))))
      (kw "COMMON" (%f7-with-tokens (r) (prog1 (%f7-parse-common) (%f7-expect-end))))
      (kw "EQUIVALENCE" (%f7-with-tokens (r) (prog1 (%f7-parse-equivalence) (%f7-expect-end))))
      (kw "EXTERNAL" (%f7-with-tokens (r) (prog1 (list :external (%f7-parse-name-list)) (%f7-expect-end))))
      (kw "INTRINSIC" (%f7-with-tokens (r) (prog1 (list :intrinsic (%f7-parse-name-list)) (%f7-expect-end))))
      (kw "SAVE"
          (%f7-with-tokens (r)
            (let ((items nil))
              (loop
                (when (%f7-end-p) (return))
                (if (%f7-op-p "/")
                    (progn (%f7-adv) (push (list :common (%f7-name)) items) (%f7-expect-op "/"))
                    (push (%f7-name) items))
                (if (%f7-op-p ",") (%f7-adv) (return)))
              (%f7-expect-end)
              (list :save (nreverse items)))))
      (kw "PARAMETER" (%f7-with-tokens (r) (prog1 (%f7-parse-parameter) (%f7-expect-end))))
      (kw "DATA" (%f7-with-tokens (r) (%f7-parse-data)))
      (kw "FORMAT"
          (let ((close (and (> (length r) 0) (char= (char r 0) #\() (%f7-match-paren r 0))))
            (unless (and close (= close (- (length r) 1))) (%f7-error "FORMAT needs a parenthesized list of edit descriptors"))
            (list :format r)))
      (multiple-value-bind (tkind tlen) (%f7-type-keyword s)
        (when tkind
          (multiple-value-bind (ty after) (%f7-parse-typespec tkind (subseq s tlen))
            (return-from %f7-classify
              (if (and (%f7-starts after "FUNCTION") (> (length after) 8) (alpha-char-p (char after 8)))
                  (%f7-parse-function-header ty (subseq after 8))
                  (%f7-with-tokens (after)
                    (when (and (%f7-op-p ",") (eq tkind :character)) (%f7-adv))
                    (prog1 (list :type ty (%f7-parse-declarators)) (%f7-expect-end))))))))
      (when (%f7-starts s "IF") (return-from %f7-classify (%f7-parse-if s)))
      (kw "GOTO" (%f7-parse-goto r))
      (kw "ASSIGN"
          (let ((to (search "TO" r)))
            (unless (and to (%f7-digits-p (subseq r 0 to)) (%f7-name-string-p (subseq r (+ to 2)))) (%f7-error "ASSIGN label TO variable expected"))
            (list :assign-label (parse-integer (subseq r 0 to)) (subseq r (+ to 2)))))
      (kw "CALL" (%f7-with-tokens (r) (let* ((name (%f7-name)) (args (%f7-parse-call-args))) (%f7-expect-end) (list :call name args))))
      (kw "CONTINUE" (if (string= r "") (list :continue) (%f7-error "Unexpected text after CONTINUE")))
      (kw "RETURN" (if (string= r "") (list :return nil) (%f7-with-tokens (r) (prog1 (list :return (%f7-parse-expr)) (%f7-expect-end)))))
      (kw "STOP" (if (string= r "") (list :stop nil) (%f7-with-tokens (r) (prog1 (list :stop (%f7-parse-expr)) (%f7-expect-end)))))
      (kw "PAUSE" (if (string= r "") (list :pause nil) (%f7-with-tokens (r) (prog1 (list :pause (%f7-parse-expr)) (%f7-expect-end)))))
      (kw "PRINT" (%f7-parse-read-write :print r))
      (kw "READ" (%f7-parse-read-write :read r))
      (kw "WRITE" (%f7-parse-read-write :write r))
      (kw "OPEN" (%f7-with-tokens (r) (prog1 (list :open (%f7-parse-control '(:unit))) (%f7-expect-end))))
      (kw "CLOSE" (%f7-with-tokens (r) (prog1 (list :close (%f7-parse-control '(:unit))) (%f7-expect-end))))
      (kw "INQUIRE" (%f7-with-tokens (r) (prog1 (list :inquire (%f7-parse-control '(:unit))) (%f7-expect-end))))
      (kw "REWIND" (list :rewind (%f7-with-tokens (r) (prog1 (%f7-parse-simple-control) (%f7-expect-end)))))
      (kw "BACKSPACE" (list :backspace (%f7-with-tokens (r) (prog1 (%f7-parse-simple-control) (%f7-expect-end)))))
      (%f7-error "Unrecognized statement ~A" (if (> n 24) (%string-concat (subseq s 0 24) "...") s)))))

(defun f77-parse-statement (fs)
  "Parse the FSTMT FS into an FNODE; signals F77-SYNTAX-ERROR."
  (let ((*f7stmt* fs))
    (let ((node (handler-case (%f7-classify (%f7-squeeze (fs-text fs)))
                  (f77-syntax-error (c)
                    (if (f77-error-line c) (error c) (%f7-error "~A" (f77-error-message c))))
                  (error (e) (%f7-error "~A" e)))))
      (%make-fnode :kind (car node) :args (cdr node) :label (fs-label fs) :line (fs-line fs) :start (fs-start fs) :end (fs-end fs)))))

;;; ---- program units ---------------------------------------------------------------------------------------------

(defun f77-parse (source &key file (directory ""))
  "Parse the fixed-format FORTRAN SOURCE: (values units errors).  UNITS are FAST structures (kind :PROGRAM, :SUBROUTINE,
:FUNCTION or :BLOCKDATA, name, parameters, type, statements, byte range) and ERRORS F77-SYNTAX-ERRORs in source order."
  (multiple-value-bind (stmts errors) (f77-read-statements source :file file :directory directory)
    (let ((units nil) (cur nil))
      (labels ((close-unit (end-node)
                 (when cur
                   (setf (fu-stmts cur) (nreverse (fu-stmts cur)))
                   (setf (fu-end cur) (if end-node (fn-end end-node) (let ((l (car (last (fu-stmts cur))))) (if l (fn-end l) (fu-start cur)))))
                   (push cur units) (setq cur nil)))
               (new-unit (node kind name params type)
                 (setq cur (%make-fast :kind kind :name name :params params :type type :stmts nil :start (fn-start node) :line (fn-line node)))))
        (dolist (fs stmts)
          (handler-case
              (let* ((node (f77-parse-statement fs)) (kind (fn-kind node)) (args (fn-args node)))
                (case kind
                  ((:program :subroutine :function :blockdata)
                   (when cur
                     (push (make-condition 'f77-syntax-error :message "Missing END statement before this program unit" :start (fn-start node) :end (fn-end node)
                                                             :line (fn-line node) :col 7 :file file) errors)
                     (close-unit nil))
                   (ecase kind
                     (:program (new-unit node :program (car args) nil nil))
                     (:subroutine (new-unit node :subroutine (car args) (cadr args) nil))
                     (:function (new-unit node :function (cadr args) (caddr args) (car args)))
                     (:blockdata (new-unit node :blockdata (car args) nil nil)))
                   (push node (fu-stmts cur)))
                  (:end (if cur
                            (progn (push node (fu-stmts cur)) (close-unit node))
                            (progn (new-unit node :program nil nil nil) (push node (fu-stmts cur)) (close-unit node))))
                  (t (unless cur (new-unit node :program nil nil nil))
                     (push node (fu-stmts cur)))))
            (f77-syntax-error (c) (push c errors))))
        (when cur
          (push (make-condition 'f77-syntax-error :message "Missing END statement at the end of the source"
                                                  :start (fu-start cur) :end (fu-start cur) :line (fu-line cur) :col 7 :file file) errors)
          (close-unit nil)))
      (values (nreverse units) (sort (copy-list errors) #'< :key (lambda (c) (or (f77-error-start c) 0)))))))
