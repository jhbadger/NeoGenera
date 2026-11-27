;;;; fuzz-tests.lisp -- deterministic fuzzing of the reader, printer, parsers and numeric kernels.
;;;;
;;;; Random and mutated inputs are fed to every parser and C primitive.  A function may succeed or signal an
;;;; ordinary error; what it must never do is crash the process (run this file under the ASan build:
;;;; `make asan` then see CONTRIBUTING.md), loop forever, or violate a mathematical identity.

(in-package :cl)

(defvar *fuzz-rng* (make-rng 20260101))
(defvar *fuzz-chars* (coerce "abcxyz ABCXYZ 0123456789 ()'`,;\"#|\\:.-+*/<>=@[]{}~%&^$!?_
" 'list))

(defun fuzz-string (&optional (max 60))
  (let* ((n (rand-int (+ max 1) *fuzz-rng*)) (s (make-string n #\a)))
    (dotimes (i n s)
      (setf (char s i) (if (< (rand-uniform *fuzz-rng*) 0.9) (rand-choice *fuzz-chars* *fuzz-rng*) (code-char (+ 1 (rand-int 126 *fuzz-rng*))))))))

(defun fuzz-mutate (s)
  "A copy of S with a few random edits (delete, insert, replace, truncate, duplicate a slice)."
  (let ((out (copy-seq s)))
    (dotimes (k (+ 1 (rand-int 4 *fuzz-rng*)) out)
      (let ((n (length out)))
        (when (> n 0)
          (let ((p (rand-int n *fuzz-rng*)))
            (case (rand-int 5 *fuzz-rng*)
              (0 (setq out (%string-concat (subseq out 0 p) (subseq out (min n (+ p 1))))))
              (1 (setq out (%string-concat (subseq out 0 p) (string (rand-choice *fuzz-chars* *fuzz-rng*)) (subseq out p))))
              (2 (setf (char out p) (rand-choice *fuzz-chars* *fuzz-rng*)))
              (3 (setq out (subseq out 0 p)))
              (t (setq out (%string-concat out (subseq out p (min n (+ p 8)))))))))))))

(defun fuzz-lines (text) (split-lines text))

(defmacro fuzz-each ((var count generator) &body body)
  "Run BODY COUNT times with VAR bound to fresh inputs from GENERATOR; ordinary errors are fine, crashes are not."
  `(let ((ok 0) (errors 0))
     (dotimes (i ,count)
       (let ((,var ,generator))
         (handler-case (progn ,@body (incf ok)) (error (e) (declare (ignore e)) (incf errors)))))
     (values ok errors)))

(defun seed-text (name) (join-lines (read-file-lines (fixture name))))

(deftest fuzz-reader-and-printer
  (let ((*read-eval* nil))
    (multiple-value-bind (ok errors) (fuzz-each (s 4000 (fuzz-string 50)) (read-from-string s))
      (is (> (+ ok errors) 3999)) (is (> ok 100)))
    (multiple-value-bind (ok errors) (fuzz-each (s 2000 (fuzz-mutate "(defun f (x &key (y 2)) \"doc\" #'(lambda (z) `(a ,b ,@c)) #(1 2 3) #\\a 1/2 -1.5e3 'sym :key |Odd Sym| #x1F)")) (read-from-string s))
      (is (> (+ ok errors) 1999)))
    (multiple-value-bind (ok errors) (fuzz-each (s 1500 (fuzz-string 40)) (with-input-from-string (in s) (loop (let ((x (read in nil :eof))) (when (eq x :eof) (return))))))
      (is (> (+ ok errors) 1499)))
    (multiple-value-bind (ok errors) (fuzz-each (s 1500 (fuzz-string 40)) (format nil s))
      (is (> (+ ok errors) 1499)))
    (multiple-value-bind (ok errors) (fuzz-each (s 800 (fuzz-string 12)) (parse-integer s :junk-allowed (evenp (length s))))
      (is (> (+ ok errors) 799))))
  ;; print/read round trip of random data
  (labels ((gen (depth)
             (case (rand-int (if (> depth 3) 6 8) *fuzz-rng*)
               (0 (- (rand-int 2000000 *fuzz-rng*) 1000000)) (1 (rand-int (expt 10 25) *fuzz-rng*)) (2 (/ (+ 1 (rand-int 99 *fuzz-rng*)) (+ 2 (rand-int 99 *fuzz-rng*))))
               (3 (rand-choice '("" "abc" "with \"quote\"" "back\\slash" "line
break" "ünï") *fuzz-rng*)) (4 (rand-choice '(foo bar |Hello World| :key nil t) *fuzz-rng*)) (5 (rand-choice '(#\a #\Space #\Newline #\λ) *fuzz-rng*))
               (6 (loop repeat (rand-int 4 *fuzz-rng*) collect (gen (+ depth 1)))) (t (coerce (loop repeat (rand-int 4 *fuzz-rng*) collect (gen (+ depth 1))) 'vector)))))
    (dotimes (i 600)
      (let ((x (gen 0))) (is (equalp x (read-from-string (prin1-to-string x))))))))

(deftest fuzz-text-parsers
  (let ((fa (seed-text "tiny.fa")) (fq (seed-text "tiny.fq")) (gb (seed-text "tiny.gb")) (gff (seed-text "tiny.gff3")) (vcf (seed-text "tiny.vcf")) (sam (seed-text "tiny.sam")) (bed (seed-text "tiny.bed")))
    (dolist (case (list (cons "fasta" (cons fa #'parse-fasta)) (cons "fastq" (cons fq #'parse-fastq)) (cons "genbank" (cons gb #'parse-genbank)) (cons "gff" (cons gff #'parse-gff))
                        (cons "vcf" (cons vcf #'parse-vcf)) (cons "sam" (cons sam #'parse-sam)) (cons "bed" (cons bed #'parse-bed))))
      (multiple-value-bind (ok errors) (fuzz-each (s 500 (fuzz-mutate (cadr case))) (funcall (cddr case) (fuzz-lines s)))
        (is-equal 500 (+ ok errors)) (is (> ok 20))))
    (multiple-value-bind (ok errors) (fuzz-each (s 500 (fuzz-string 80)) (parse-fasta (fuzz-lines s)) (parse-fastq (fuzz-lines s)) (parse-genbank (fuzz-lines s)) (parse-sam (fuzz-lines s)))
      (is-equal 500 (+ ok errors))))
  (multiple-value-bind (ok errors) (fuzz-each (s 800 (fuzz-mutate "((A:0.1,B:0.2):0.3,(C:0.4,D:0.5):0.6);")) (parse-newick s)) (is-equal 800 (+ ok errors)) (is (> ok 10)))
  (multiple-value-bind (ok errors) (fuzz-each (s 800 (fuzz-mutate "a,\"b,c\",d
1,2.5,\"x \"\"y\"\"\"
")) (parse-delimited s #\,)) (is-equal 800 (+ ok errors)) (is (> ok 400)))
  (multiple-value-bind (ok errors) (fuzz-each (s 800 (fuzz-string 40)) (parse-delimited s #\Tab) (dframe-from-records (parse-delimited s #\,))) (is-equal 800 (+ ok errors)))
  (multiple-value-bind (ok errors) (fuzz-each (s 800 (fuzz-mutate (seed-text "../../docs/manual/user-guide.md"))) (markdown->nodes (subseq s 0 (min 400 (length s))))) (is-equal 800 (+ ok errors)) (is (> ok 400)))
  (multiple-value-bind (ok errors) (fuzz-each (s 800 (fuzz-string 30)) (parse-genbank-location s) (parse-cigar s)) (is-equal 800 (+ ok errors))))

(deftest fuzz-binary-parsers
  (let ((good (read-gzip-string (fixture "tiny.bam"))))
    (multiple-value-bind (ok errors) (fuzz-each (s 600 (fuzz-mutate good)) (parse-bam-bytes s)) (is-equal 600 (+ ok errors)) (is (>= ok 0)))
    (multiple-value-bind (ok errors) (fuzz-each (s 400 (let ((n (rand-int (length good) *fuzz-rng*))) (subseq good 0 n))) (parse-bam-bytes s)) (is-equal 400 (+ ok errors)) (is (> errors 300)))
    (multiple-value-bind (ok errors) (fuzz-each (s 400 (let ((m (copy-seq good))) (dotimes (k 6) (setf (char m (rand-int (length m) *fuzz-rng*)) (code-char (rand-int 256 *fuzz-rng*)))) m)) (parse-bam-bytes s)) (is-equal 400 (+ ok errors)))))

(deftest fuzz-regex-and-motifs
  (multiple-value-bind (ok errors) (fuzz-each (s 1500 (fuzz-string 12)) (regex-find-all s "ACGTACGTTTGACAGGATCC")) (is-equal 1500 (+ ok errors)) (is (> ok 100)))
  (multiple-value-bind (ok errors) (fuzz-each (s 800 (let ((a (coerce (loop repeat 6 collect (rand-choice '("A" "C" "G" "T" "." "[AC]" "(A|C)" "A*" "G+" "T?" "N" "{2}" "^" "$") *fuzz-rng*)) 'list))) (apply #'%string-concat a)))
      (regex-find-all s "ACGTAGCTAGCTAGGATCGATCGATCGTAGCTAGCTAGC")) (is-equal 800 (+ ok errors)))
  (multiple-value-bind (ok errors) (fuzz-each (s 800 (random-string (rand-int 8 *fuzz-rng*) "ACGTRYSWKMBDHVN-X" *fuzz-rng*)) (find-iupac-motif s "ACGTAGCTAGCTAGGATC" :both-strands t)) (is-equal 800 (+ ok errors))))

(deftest fuzz-kernels
  ;; the C alignment fill and suffix array with arbitrary strings, modes and gap parameters
  (let ((alphabets '("ACGT" "ACGTN" "AC" "A" "ARNDCQEGHILKMFPSTWYV" "acgt")))
    (multiple-value-bind (ok errors)
        (fuzz-each (job 1500 (list (random-string (rand-int 40 *fuzz-rng*) (rand-choice alphabets *fuzz-rng*) *fuzz-rng*) (random-string (rand-int 40 *fuzz-rng*) (rand-choice alphabets *fuzz-rng*) *fuzz-rng*)
                                   (rand-choice '(:global :local :semiglobal) *fuzz-rng*) (+ 1 (rand-int 15 *fuzz-rng*)) (+ 1 (rand-int 5 *fuzz-rng*)) (rand-choice '(-1 -1 0 1 3 10) *fuzz-rng*)))
          (let* ((al (align (first job) (second job) :mode (third job) :gap-open (fourth job) :gap-extend (fifth job) :band (sixth job))))
            (assert (= (length (alignment-aligned-a al)) (length (alignment-aligned-b al))))
            (assert (string= (remove #\- (alignment-aligned-a al)) (subseq (string-upcase (first job)) (alignment-a-start al) (alignment-a-end al))))))
      (is-equal 1500 (+ ok errors)) (is (> ok 1000))))
  (dolist (text (list "" "a" "aaaa" "abab" (random-string 500 "AC" *fuzz-rng*) (random-string 300 "ACGTN" *fuzz-rng*) (coerce (loop for i from 0 below 100 collect (code-char (+ 1 (mod (* i 37) 250)))) 'string)))
    (let* ((sa (coerce (%suffix-array text) 'list)) (naive (sort (loop for i from 0 below (length text) collect i) (lambda (a b) (string< (subseq text a) (subseq text b))))))
      (is-equal naive sa)))
  ;; bignum identities across the schoolbook / Karatsuba boundary
  (let ((bad 0))
    (dotimes (i 300)
      (let* ((na (+ 1 (rand-int 90 *fuzz-rng*))) (nb (+ 1 (rand-int 90 *fuzz-rng*)))
             (a (let ((n 0)) (dotimes (k na n) (setq n (+ (ash n 30) (rand-int (ash 1 30) *fuzz-rng*)))))) (b (let ((n 0)) (dotimes (k nb n) (setq n (+ (ash n 30) (rand-int (ash 1 30) *fuzz-rng*))))))
             (a (if (zerop (rand-int 5 *fuzz-rng*)) (- a) a)) (ab (* a b)))
        (unless (and (= (* a b) (* b a)) (or (zerop b) (= (floor ab b) a)) (= (* (+ a 1) b) (+ ab b)) (= (- (* (+ a b) (+ a b)) (* a a) (* b b)) (* 2 ab))
                     (= (mod ab 1000000007) (mod (* (mod a 1000000007) (mod b 1000000007)) 1000000007)))
          (incf bad))))
    (is-equal 0 bad))
  ;; numeric kernels with extreme arguments must give an answer or an error
  (multiple-value-bind (ok errors) (fuzz-each (x 400 (rand-choice (list 0.0 1.0 -1.0 1e300 -1e300 1e-300 +infinity+ -infinity+ +nan+ 12345678.9 (expt 2 70) 1/3) *fuzz-rng*))
                                              (sqrt x) (exp x) (log x) (sin x) (atan x) (floor x) (round x) (float x) (* x x) (/ x 3))
    (is-equal 400 (+ ok errors))))

(deftest fuzz-tools-and-settings
  ;; arbitrary text through the data-oriented helpers must never crash
  (multiple-value-bind (ok errors) (fuzz-each (s 400 (fuzz-string 30)) (parse-blast-tabular (list s)) (parse-paf (list s)) (parse-samtools-depth (list s))) (is-equal 400 (+ ok errors)))
  (multiple-value-bind (ok errors) (fuzz-each (s 200 (fuzz-string 30)) (run-tool s nil)) (is-equal 200 (+ ok errors)) (is-equal 0 ok))
  (multiple-value-bind (ok errors) (fuzz-each (s 300 (fuzz-string 40)) (search-documentation s) (resolve-topic s)) (is-equal 300 (+ ok errors)))
  (multiple-value-bind (ok errors) (fuzz-each (s 300 (fuzz-mutate "(+ x (* 3 y) (expt z 2) (sin (* 2 x)))")) (let ((e (read-from-string s))) (simplify e) (differentiate e 'x) (expr->string e))) (is-equal 300 (+ ok errors))))

(deftest fuzz-fortran
  ;; the FORTRAN Tool Kit: card reading, squeezing, tokens, parser, FORMAT, translation and input
  (let ((seed "      PROGRAM P
      INTEGER I, A(10)
      REAL X
      CHARACTER*8 S
      COMMON /B/ X
      DATA A /10*0/
      DO 10 I = 1, 10
         A(I) = I**2 + MOD(I, 3)
   10 CONTINUE
      IF (X .GT. 1.5) THEN
         WRITE (*, 100) A(1), X
      ELSE
         S = 'it''s' // 'ok'
      END IF
  100 FORMAT (1X, I5, F8.3, 2(A, ' '), /)
      END
"))
    (multiple-value-bind (ok errors) (fuzz-each (s 1500 (fuzz-mutate seed)) (f77-parse s))
      (is (> (+ ok errors) 1499)))
    (multiple-value-bind (ok errors) (fuzz-each (s 800 (fuzz-mutate seed)) (f77-translate s))
      (is (> (+ ok errors) 799)) (is (> ok 100)))
    (multiple-value-bind (ok errors) (fuzz-each (s 2000 (fuzz-string 50)) (f77-tokenize (%f7-squeeze s)))
      (is (> (+ ok errors) 1999)))
    (multiple-value-bind (ok errors) (fuzz-each (s 2000 (fuzz-string 70)) (f77-read-statements s))
      (is (> (+ ok errors) 1999)))
    (multiple-value-bind (ok errors) (fuzz-each (s 2000 (fuzz-mutate "(1X,'x=',F8.3,3I5,/,2(A,I3),E12.4E2,2PF6.2,T3,TL1,Z4,BZ,5HHello)")) (f77-parse-format s))
      (is (> (+ ok errors) 1999)))
    ;; huge counts and sizes are errors, not allocations
    (signals-error (f77-parse-format "(I99999999999)"))
    (is (null (f77-translate (format nil "      INTEGER A(2000000000)~%      A(1) = 1~%      END~%"))))
    (is (null (f77-translate (format nil "      PARAMETER (N = 9**9**9)~%      END~%"))))
    (is (null (f77-translate (format nil "      INTEGER A(3)~%      DATA (A(I), I=1,1000000000) /1/~%      END~%"))))
    (is (stringp (%f7-squeeze "FORMAT(99999999999HX)")))
    (signals-error (%f7-ipow 3 100000000))
    (signals-error (%f7-ishft 1 1000000000))
    ;; every edit descriptor on odd numbers
    (dolist (x (list 0d0 1d300 -1d-300 123456789.123456789d0 1d0 0.5d0 9.9999d0 99999.5d0 -2.5d0))
      (dolist (w '(0 1 3 8 20))
        (dolist (d '(0 1 3 8))
          (%f7-edit-f x w d 0 nil) (%f7-edit-e x w d nil 0 nil #\E) (%f7-edit-e x w d 2 1 t #\D) (%f7-edit-g x w d nil 0 nil))))
    ;; formatted and list-directed input from random text
    (flet ((reading (fmt text)
             (let ((*f77-env* (%make-fenv :input (make-string-input-stream text))))
               (let ((st (%f7-begin :read 5 fmt)))
                 (%f7-get st :integer #'identity) (%f7-get st :double #'identity)
                 (%f7-get st :character #'identity 5) (%f7-get st :logical #'identity) (%f7-end st)))))
      (multiple-value-bind (ok errors) (fuzz-each (s 1500 (fuzz-string 30)) (reading nil s))
        (is (> (+ ok errors) 1499)))
      (let ((fmt (f77-parse-format "(I3,F5.1,A4,L2)")))
        (multiple-value-bind (ok errors) (fuzz-each (s 1500 (fuzz-string 30)) (reading fmt s))
          (is (> (+ ok errors) 1499)))))))

;;; ---- the bytecode assembler ----------------------------------------------------------------------------------------------

(defun fuzz-asm-atom ()
  (rand-choice (list 0 1 2 3 -1 100 1.5 "s" nil t 'foo 'bar :key 'l0 'l1 'l2 'x 'y (list 1 2) '(a . b) (expt 2 70) #\a) *fuzz-rng*))

(defun fuzz-asm-item ()
  "A random assembly item: a label, a junk atom, or an instruction with random operands."
  (case (rand-int 10 *fuzz-rng*)
    (0 (rand-choice '(l0 l1 l2 done top) *fuzz-rng*))
    (1 (fuzz-asm-atom))
    (t (let ((head (if (< (rand-uniform *fuzz-rng*) 0.95)
                       (intern (string-upcase (rand-choice (asm-mnemonics) *fuzz-rng*)) :cl)
                       (rand-choice (list :unsafe :var :free :lambda 'nonsense 3 nil) *fuzz-rng*)))
             (n (case (rand-int 5 *fuzz-rng*) (0 0) (1 2) (t 1))))
         (cons head (let ((ops nil)) (dotimes (i n) (push (if (< (rand-uniform *fuzz-rng*) 0.1) (list :lambda nil '(const 1) '(ret)) (fuzz-asm-atom)) ops)) ops))))))

(defvar *fuzz-asm-label* 0)

(defun fuzz-asm-snippet ()
  "A well-formed run of instructions, mostly leaving the stack one deeper: the building block of generated programs."
  (case (rand-int 8 *fuzz-rng*)
    (0 (list (list 'const (rand-choice '(0 1 2 5 -3 1.5 nil) *fuzz-rng*))))
    (1 (list '(lref 0)))
    (2 (list '(lref 0) (rand-choice '((car) (cdr) (not) (box) (chkfix) (chktype cons) (chktype list) (chkrange 0 9) (dup) (pop) (unbox)) *fuzz-rng*)))
    (3 (list '(lref 0) '(lref 0) (list (rand-choice '(add sub mul lt eq cons numeq ge addfx ltfx addfl svref schar svset remfx) *fuzz-rng*))))
    (4 (list '(lref 0) '(chkfix) '(lset 0) '(lref 0) '(const 3) (list (rand-choice '(addfx subfx mulfx logandfx ltfx remfx) *fuzz-rng*))))
    (5 (list '(lref 0) '(chktype double-float) '(lset 0) '(lref 0) '(lref 0) (list (rand-choice '(addfl subfl mulfl ltfl) *fuzz-rng*))))
    (6 (list '(lref 0) '(box) '(chktype cons) '(unbox)))
    (t (let ((l (intern (format nil "G~D" (incf *fuzz-asm-label*)) :cl)))
         (list '(lref 0) (list (rand-choice '(jnil jnnil) *fuzz-rng*) l) '(const 7) '(pop) l '(const 8))))))

(defun fuzz-asm-program ()
  "Items for a random but mostly sound routine of one parameter.  Forward jumps only, so it always terminates."
  (let ((items nil))
    (dotimes (i (+ 1 (rand-int 6 *fuzz-rng*)))
      (dolist (it (fuzz-asm-snippet)) (push it items)))
    (push '(ret) items)
    (nreverse items)))

(deftest fuzz-asm
  ;; the assembler's contract: any items assemble or signal ASM-ERROR -- nothing else, never a crash
  (let ((other 0) (accepted 0))
    (dotimes (i 4000)
      (let ((items (if (evenp i)
                       (let ((acc nil)) (dotimes (k (rand-int 9 *fuzz-rng*)) (push (fuzz-asm-item) acc)) (nreverse acc))
                       ;; a sound program with one item replaced, deleted or added
                       (let* ((p (fuzz-asm-program)) (k (rand-int (length p) *fuzz-rng*)))
                         (case (rand-int 3 *fuzz-rng*)
                           (0 (append (subseq p 0 k) (list (fuzz-asm-item)) (nthcdr (+ k 1) p)))
                           (1 (append (subseq p 0 k) (nthcdr (+ k 1) p)))
                           (t (append (subseq p 0 k) (list (fuzz-asm-item)) (nthcdr k p)))))))
            (ll (if (evenp i)
                    (rand-choice '(() (x) (x y) (x &optional y) (x &rest r) (&optional a b) (&key a) (x x) (1) foo) *fuzz-rng*)
                    '(x))))
        (handler-case (progn (asm-assemble 'fuzz-routine ll items) (incf accepted))
          (asm-error () nil)
          (error () (incf other)))))
    (is-equal 0 other)
    (is (> accepted 100)))
  ;; and what assembles runs without taking the process down, whatever the argument
  (let ((other 0) (accepted 0) (ran 0))
    (dotimes (i 3000)
      (let ((items (fuzz-asm-program)))
        (handler-case
            (let ((fn (asm-assemble 'fuzz-routine '(x) items)))
              (incf accepted)
              (dolist (arg (list 0 1 -5 1.5 nil '(1 2 3) "str" (vector 1 2) 'sym (expt 2 70)))
                (handler-case (progn (funcall fn arg) (incf ran)) (error () (incf ran)))))
          (asm-error () nil)
          (error () (incf other)))))
    (is-equal 0 other)
    (is (> accepted 500))
    (is (> ran 5000))))
