;;;; 20-align.lisp -- pairwise alignment, scoring matrices, indexes and seeded search, motifs.
;;;;
;;;; The dynamic-programming fill (affine gaps, global/local/semi-global, optional band) is the C primitive
;;;; %DP-ALIGN; traceback and everything else is Lisp.

(in-package :cl)

;;; ---- scoring matrices -------------------------------------------------------------------------------------------------

(defstruct (scoring-matrix (:copier nil) (:constructor %make-scoring-matrix (name alphabet table lookup)))
  name alphabet table lookup)

(defun %build-lookup (alphabet cells default)
  "A 65536-element vector indexed by (byte-a * 256 + byte-b) holding scores (case-insensitive)."
  (let ((v (make-array 65536 :initial-element default)))
    (dotimes (i (length alphabet))
      (dotimes (j (length alphabet))
        (let ((s (aref cells (+ (* i (length alphabet)) j))))
          (dolist (ci (list (char-code (char alphabet i)) (char-code (char-downcase (char alphabet i)))))
            (dolist (cj (list (char-code (char alphabet j)) (char-code (char-downcase (char alphabet j)))))
              (setf (aref v (+ (* ci 256) cj)) s))))))
    v))

(defun make-scoring-matrix (name alphabet rows &optional (default -4))
  "A scoring matrix over ALPHABET (a string) from ROWS, a list of lists of integers; unknown residues score DEFAULT."
  (let ((cells (coerce (apply #'append rows) 'vector)))
    (unless (= (length cells) (* (length alphabet) (length alphabet))) (error "Scoring matrix must be ~Dx~D" (length alphabet) (length alphabet)))
    (%make-scoring-matrix name alphabet cells (%build-lookup alphabet cells default))))

(defun nucleotide-matrix (&optional (match 5) (mismatch -4) (n-score 0))
  "A DNA scoring matrix: MATCH for identical bases, MISMATCH otherwise, N-SCORE against N."
  (let* ((alpha "ACGTN") (rows (loop for a across alpha collect (loop for b across alpha collect (cond ((or (char= a #\N) (char= b #\N)) n-score) ((char= a b) match) (t mismatch))))))
    (make-scoring-matrix "NUC" alpha rows mismatch)))

(defun score-pair (m a b)
  "The score of aligning residue characters A and B under the scoring matrix M."
  (aref (scoring-matrix-lookup m) (+ (* 256 (char-code a)) (char-code b))))

(defvar *blosum62-alphabet* "ARNDCQEGHILKMFPSTWYV")

(defvar *blosum62-rows*
  '(( 4 -1 -2 -2  0 -1 -1  0 -2 -1 -1 -1 -1 -2 -1  1  0 -3 -2  0)
    (-1  5  0 -2 -3  1  0 -2  0 -3 -2  2 -1 -3 -2 -1 -1 -3 -2 -3)
    (-2  0  6  1 -3  0  0  0  1 -3 -3  0 -2 -3 -2  1  0 -4 -2 -3)
    (-2 -2  1  6 -3  0  2 -1 -1 -3 -4 -1 -3 -3 -1  0 -1 -4 -3 -3)
    ( 0 -3 -3 -3  9 -3 -4 -3 -3 -1 -1 -3 -1 -2 -3 -1 -1 -2 -2 -1)
    (-1  1  0  0 -3  5  2 -2  0 -3 -2  1  0 -3 -1  0 -1 -2 -1 -2)
    (-1  0  0  2 -4  2  5 -2  0 -3 -3  1 -2 -3 -1  0 -1 -3 -2 -2)
    ( 0 -2  0 -1 -3 -2 -2  6 -2 -4 -4 -2 -3 -3 -2  0 -2 -2 -3 -3)
    (-2  0  1 -1 -3  0  0 -2  8 -3 -3 -1 -2 -1 -2 -1 -2 -2  2 -3)
    (-1 -3 -3 -3 -1 -3 -3 -4 -3  4  2 -3  1  0 -3 -2 -1 -3 -1  3)
    (-1 -2 -3 -4 -1 -2 -3 -4 -3  2  4 -2  2  0 -3 -2 -1 -2 -1  1)
    (-1  2  0 -1 -3  1  1 -2 -1 -3 -2  5 -1 -3 -1  0 -1 -3 -2 -2)
    (-1 -1 -2 -3 -1  0 -2 -3 -2  1  2 -1  5  0 -2 -1 -1 -1 -1  1)
    (-2 -3 -3 -3 -2 -3 -3 -3 -1  0  0 -3  0  6 -4 -2 -2  1  3 -1)
    (-1 -2 -2 -1 -3 -1 -1 -2 -2 -3 -3 -1 -2 -4  7 -1 -1 -4 -3 -2)
    ( 1 -1  1  0 -1  0  0  0 -1 -2 -2  0 -1 -2 -1  4  1 -3 -2 -2)
    ( 0 -1  0 -1 -1 -1 -1 -2 -2 -1 -1 -1 -1 -2 -1  1  5 -2 -2  0)
    (-3 -3 -4 -4 -2 -2 -3 -2 -2 -3 -2 -3 -1  1 -4 -3 -2 11  2 -3)
    (-2 -2 -2 -3 -2 -1 -2 -3  2 -1 -1 -2 -1  3 -3 -2 -2  2  7 -1)
    ( 0 -3 -3 -3 -1 -2 -2 -3 -3  3  1 -2  1 -1 -2 -2  0 -3 -1  4)))

(defvar *pam250-rows*
  '(( 2 -2  0  0 -2  0  0  1 -1 -1 -2 -1 -1 -3  1  1  1 -6 -3  0)
    (-2  6  0 -1 -4  1 -1 -3  2 -2 -3  3  0 -4  0  0 -1  2 -4 -2)
    ( 0  0  2  2 -4  1  1  0  2 -2 -3  1 -2 -3  0  1  0 -4 -2 -2)
    ( 0 -1  2  4 -5  2  3  1  1 -2 -4  0 -3 -6 -1  0  0 -7 -4 -2)
    (-2 -4 -4 -5 12 -5 -5 -3 -3 -2 -6 -5 -5 -4 -3  0 -2 -8  0 -2)
    ( 0  1  1  2 -5  4  2 -1  3 -2 -2  1 -1 -5  0 -1 -1 -5 -4 -2)
    ( 0 -1  1  3 -5  2  4  0  1 -2 -3  0 -2 -5 -1  0  0 -7 -4 -2)
    ( 1 -3  0  1 -3 -1  0  5 -2 -3 -4 -2 -3 -5  0  1  0 -7 -5 -1)
    (-1  2  2  1 -3  3  1 -2  6 -2 -2  0 -2 -2  0 -1 -1 -3  0 -2)
    (-1 -2 -2 -2 -2 -2 -2 -3 -2  5  2 -2  2  1 -2 -1  0 -5 -1  4)
    (-2 -3 -3 -4 -6 -2 -3 -4 -2  2  6 -3  4  2 -3 -3 -2 -2 -1  2)
    (-1  3  1  0 -5  1  0 -2  0 -2 -3  5  0 -5 -1  0  0 -3 -4 -2)
    (-1  0 -2 -3 -5 -1 -2 -3 -2  2  4  0  6  0 -2 -2 -1 -4 -2  2)
    (-3 -4 -3 -6 -4 -5 -5 -5 -2  1  2 -5  0  9 -5 -3 -3  0  7 -1)
    ( 1  0  0 -1 -3  0 -1  0  0 -2 -3 -1 -2 -5  6  1  0 -6 -5 -1)
    ( 1  0  1  0  0 -1  0  1 -1 -1 -3  0 -2 -3  1  2  1 -2 -3 -1)
    ( 1 -1  0  0 -2 -1  0  0 -1  0 -2  0 -1 -3  0  1  3 -5 -3  0)
    (-6  2 -4 -7 -8 -5 -7 -7 -3 -5 -2 -3 -4  0 -6 -2 -5 17  0 -6)
    (-3 -4 -2 -4  0 -4 -4 -5  0 -1 -1 -4 -2  7 -5 -3 -3  0 10 -2)
    ( 0 -2 -2 -2 -2 -2 -2 -1 -2  4  2 -2  2 -1 -1 -1  0 -6 -2  4)))

(defvar *blosum62* (make-scoring-matrix "BLOSUM62" *blosum62-alphabet* *blosum62-rows* -1) "The BLOSUM62 protein scoring matrix.")
(defvar *pam250* (make-scoring-matrix "PAM250" *blosum62-alphabet* *pam250-rows* -1) "The PAM250 protein scoring matrix.")
(defvar *dna-matrix* (nucleotide-matrix) "The default DNA scoring matrix (+5 / -4).")

;;; ---- alignment -------------------------------------------------------------------------------------------------------------

(defstruct (alignment (:copier nil) (:constructor %make-alignment (score a b aligned-a aligned-b a-start a-end b-start b-end mode)))
  score a b aligned-a aligned-b a-start a-end b-start b-end mode)

(defun alignment-length (al) "Number of columns in the alignment AL." (length (alignment-aligned-a al)))
(defun alignment-identity (al)
  "Fraction of alignment columns with identical residues."
  (let ((n (alignment-length al)) (same 0))
    (dotimes (i n) (when (char= (char (alignment-aligned-a al) i) (char (alignment-aligned-b al) i)) (unless (char= (char (alignment-aligned-a al) i) #\-) (incf same))))
    (if (zerop n) 0.0 (/ (float same) n))))
(defun alignment-gaps (al) "Number of gap characters in AL." (+ (count #\- (alignment-aligned-a al)) (count #\- (alignment-aligned-b al))))

(defun alignment-midline (al &optional (matrix nil))
  "The match line of AL: | for identities, + for positive scores (when MATRIX is given), blank otherwise."
  (let* ((n (alignment-length al)) (out (make-string n #\Space)))
    (dotimes (i n)
      (let ((x (char (alignment-aligned-a al) i)) (y (char (alignment-aligned-b al) i)))
        (cond ((or (char= x #\-) (char= y #\-)) nil)
              ((char= x y) (setf (char out i) #\|))
              ((and matrix (> (score-pair matrix x y) 0)) (setf (char out i) #\+)))))
    out))

(defun alignment-string (al &key (width 60) matrix)
  "A readable rendering of AL in blocks of WIDTH columns with coordinates."
  (let* ((n (alignment-length al)) (mid (alignment-midline al matrix)) (pa (alignment-a-start al)) (pb (alignment-b-start al)))
    (with-output-to-string (o)
      (format o "Score ~D, identity ~,1F%, ~D gaps~%" (alignment-score al) (* 100.0 (alignment-identity al)) (alignment-gaps al))
      (loop for i from 0 below n by width
            do (let* ((e (min n (+ i width))) (sa (subseq (alignment-aligned-a al) i e)) (sb (subseq (alignment-aligned-b al) i e))
                      (na (- (length sa) (count #\- sa))) (nb (- (length sb) (count #\- sb))))
                 (format o "~%~8D ~A ~D~%         ~A~%~8D ~A ~D~%" (+ pa 1) sa (+ pa na) (subseq mid i e) (+ pb 1) sb (+ pb nb))
                 (incf pa na) (incf pb nb))))))

(defmethod print-object ((al alignment) stream)
  (format stream "#<ALIGNMENT ~A score ~D ~D columns ~,1F% identity>" (alignment-mode al) (alignment-score al) (alignment-length al) (* 100.0 (alignment-identity al))))

(defun align (a b &key (mode :global) matrix (gap-open 10) (gap-extend 1) (band -1))
  "Align sequences A and B (strings or BIOSEQs) with affine gap penalties: a gap of length k costs GAP-OPEN + (k-1) * GAP-EXTEND.
MODE is :global (Needleman-Wunsch), :local (Smith-Waterman) or :semiglobal (free end gaps).  MATRIX defaults to BLOSUM62 for
proteins and a +5/-4 matrix for nucleotides.  BAND >= 0 restricts the fill to a diagonal band of that half-width."
  (let* ((x (string-upcase (%residues a))) (y (string-upcase (%residues b)))
         (protein (or (and (bioseq-p a) (eq (bioseq-alphabet a) :protein)) (eq (%detect-alphabet x) :protein)))
         (m (or matrix (if protein *blosum62* *dna-matrix*)))
         (res (%dp-align x y (scoring-matrix-lookup m) gap-open gap-extend (ecase mode (:global 0) (:local 1) (:semiglobal 2)) band))
         (score (first res)) (ei (second res)) (ej (third res)) (state (fourth res)) (tr (fifth res)) (w (+ (length y) 1))
         (ra nil) (rb nil) (i ei) (j ej) (done nil))
    (loop until done
          do (let ((byte (char-code (char tr (+ (* i w) j)))))
               (cond
                 ((= state 0)
                  (when (or (zerop i) (zerop j)) (return))
                  (let ((from (logand byte 3)))
                    (push (char x (- i 1)) ra) (push (char y (- j 1)) rb) (decf i) (decf j)
                    (cond ((= from 3) (setq done t)) (t (setq state from)))
                    (when (and (eq mode :local) (= from 3)) (setq done t))))
                 ((= state 1)
                  (let ((from (logand (ash byte -2) 3)))
                    (push (char x (- i 1)) ra) (push #\- rb) (decf i)
                    (if (= from 3) (setq done t) (setq state from))))
                 (t (let ((from (logand (ash byte -4) 3)))
                      (push #\- ra) (push (char y (- j 1)) rb) (decf j)
                      (if (= from 3) (setq done t) (setq state from)))))
               (when (and (not done) (eq mode :global) (zerop i) (zerop j)) (setq done t))
               (when (and (not done) (not (eq mode :global)) (or (zerop i) (zerop j)) (= state 0)) (setq done t))))
    (%make-alignment score a b (coerce ra 'string) (coerce rb 'string) i ei j ej mode)))

(defun align-score (a b &rest keys)
  "The score of aligning A and B (see ALIGN for the keywords)."
  (alignment-score (apply #'align a b keys)))

(defun percent-identity (a b &rest keys)
  "Percent identity of the best alignment of A and B."
  (* 100.0 (alignment-identity (apply #'align a b keys))))

(defun alignment-from-strings (aligned-a aligned-b &key (score 0))
  "An ALIGNMENT object from two already aligned, gap-containing strings."
  (%make-alignment score aligned-a aligned-b aligned-a aligned-b 0 (- (length aligned-a) (count #\- aligned-a)) 0 (- (length aligned-b) (count #\- aligned-b)) :given))

;;; ---- suffix array search ------------------------------------------------------------------------------------------------

(defstruct (suffix-index (:copier nil) (:constructor %make-suffix-index (text array))) text array)

(defun build-suffix-index (s)
  "A suffix array index of the sequence S (built in C by prefix doubling)."
  (let ((text (%residues s))) (%make-suffix-index text (%suffix-array text))))

(defun %suffix-compare (text sa k pattern)
  "Compare the suffix SA[K] of TEXT with PATTERN over the pattern length: -1, 0 (the pattern is a prefix) or 1."
  (let* ((p (aref sa k)) (m (length pattern)) (len (min m (- (length text) p))))
    (dotimes (i len (if (< len m) -1 0))
      (let ((a (char text (+ p i))) (b (char pattern i)))
        (unless (char= a b) (return (if (char< a b) -1 1)))))))

(defun suffix-index-search (index pattern)
  "Sorted positions at which PATTERN occurs in the indexed text (binary search on the suffix array)."
  (let* ((text (suffix-index-text index)) (sa (suffix-index-array index)) (n (length sa)) (lo 0) (hi n))
    (loop while (< lo hi) do (let ((mid (floor (+ lo hi) 2))) (if (< (%suffix-compare text sa mid pattern) 0) (setq lo (+ mid 1)) (setq hi mid))))
    (let ((first lo) (hi2 n) (lo2 lo))
      (loop while (< lo2 hi2) do (let ((mid (floor (+ lo2 hi2) 2))) (if (<= (%suffix-compare text sa mid pattern) 0) (setq lo2 (+ mid 1)) (setq hi2 mid))))
      (sort (loop for k from first below lo2 collect (aref sa k)) #'<))))

(defun longest-repeat (index)
  "The longest substring that occurs at least twice in the indexed text: (values string positions)."
  (let* ((text (suffix-index-text index)) (sa (suffix-index-array index)) (best 0) (bp 0))
    (loop for k from 1 below (length sa)
          do (let* ((a (aref sa (- k 1))) (b (aref sa k)) (l 0))
               (loop while (and (< (+ a l) (length text)) (< (+ b l) (length text)) (char= (char text (+ a l)) (char text (+ b l)))) do (incf l))
               (when (> l best) (setq best l bp a))))
    (values (subseq text bp (+ bp best)) (suffix-index-search index (subseq text bp (+ bp best))))))

;;; ---- k-mer index and seeded (BLAST-like) search -------------------------------------------------------------------------

(defstruct (kmer-index (:copier nil) (:constructor %make-kmer-index (k table subjects))) k table subjects)

(defun build-kmer-index (subjects &key (k 11))
  "An index of every k-mer of SUBJECTS (a list of BIOSEQ objects) for seeded search."
  (let ((table (make-hash-table :test 'equal)) (id 0))
    (dolist (s subjects)
      (let ((r (string-upcase (%residues s))))
        (loop for i from 0 to (- (length r) k) do (let ((km (subseq r i (+ i k)))) (setf (gethash km table) (cons (cons id i) (gethash km table))))))
      (incf id))
    (%make-kmer-index k table (coerce subjects 'vector))))

(defstruct (hit (:copier nil) (:constructor %make-hit (subject score query-start query-end subject-start subject-end alignment))) subject score query-start query-end subject-start subject-end alignment)

(defmethod print-object ((h hit) stream)
  (format stream "#<HIT ~A score ~D query ~D-~D subject ~D-~D>" (bioseq-id (hit-subject h)) (hit-score h) (hit-query-start h) (hit-query-end h) (hit-subject-start h) (hit-subject-end h)))

(defun seeded-search (query index &key (min-score 30) (flank 30) matrix (max-hits 20))
  "Find local alignments of QUERY against the k-mer INDEX: exact k-mer seeds are collected per diagonal, each promising
diagonal is extended by a banded Smith-Waterman alignment, and hits scoring at least MIN-SCORE are returned best first."
  (let* ((q (string-upcase (%residues query))) (k (kmer-index-k index)) (diag (make-hash-table :test 'equal)) (hits nil))
    (loop for i from 0 to (- (length q) k)
          do (dolist (p (gethash (subseq q i (+ i k)) (kmer-index-table index)))
               (let ((key (cons (car p) (- (cdr p) i)))) (setf (gethash key diag) (+ 1 (gethash key diag 0))))))
    (let ((cands nil)) (maphash (lambda (key v) (push (cons v key) cands)) diag)
      (setq cands (sort cands (lambda (a b) (> (car a) (car b)))))
      (let ((done nil))
        (dolist (c (if (> (length cands) 200) (subseq cands 0 200) cands))
          (let* ((sid (car (cdr c))) (d (cdr (cdr c))) (subject (aref (kmer-index-subjects index) sid)) (sr (string-upcase (%residues subject)))
                 (sstart (max 0 (+ d (- flank)))) (send (min (length sr) (+ d (length q) flank))))
            (unless (or (>= sstart send) (member (cons sid (floor d (max 1 flank))) done :test #'equal))
              (push (cons sid (floor d (max 1 flank))) done)
              (let ((al (align q (subseq sr sstart send) :mode :local :matrix matrix :band (+ flank 10))))
                (when (>= (alignment-score al) min-score)
                  (push (%make-hit subject (alignment-score al) (alignment-a-start al) (alignment-a-end al) (+ sstart (alignment-b-start al)) (+ sstart (alignment-b-end al)) al) hits))))))))
    (let ((sorted (sort hits (lambda (a b) (> (hit-score a) (hit-score b))))))
      (if (> (length sorted) max-hits) (subseq sorted 0 max-hits) sorted))))

;;; ---- motifs: IUPAC, PWMs, a small regular-expression engine -----------------------------------------------------------

(defvar *iupac-codes* '((#\A . "A") (#\C . "C") (#\G . "G") (#\T . "T") (#\U . "T") (#\R . "AG") (#\Y . "CT") (#\S . "CG") (#\W . "AT") (#\K . "GT") (#\M . "AC")
                        (#\B . "CGT") (#\D . "AGT") (#\H . "ACT") (#\V . "ACG") (#\N . "ACGT")))

(defun iupac-match-p (code base) "True when the nucleotide BASE is covered by the IUPAC CODE." (let ((e (assoc (char-upcase code) *iupac-codes*))) (and e (find (char-upcase base) (cdr e)) t)))

(defun find-iupac-motif (pattern s &key both-strands)
  "Zero-based start positions of the IUPAC PATTERN in S; with BOTH-STRANDS a list of (position . strand)."
  (let* ((r (string-upcase (%residues s))) (m (length pattern)) (out nil))
    (flet ((scan (text strand) (loop for i from 0 to (- (length text) m)
                                     do (when (every (lambda (k) (iupac-match-p (char pattern k) (char text (+ i k)))) (loop for k from 0 below m collect k))
                                          (push (if both-strands (cons (if (> strand 0) i (- (length text) i m)) strand) i) out)))))
      (scan r 1)
      (when both-strands (scan (reverse-complement r) -1)))
    (nreverse out)))

(defstruct (pwm (:copier nil) (:constructor %make-pwm (alphabet columns counts))) alphabet columns counts)

(defun make-pwm (sites &key (alphabet "ACGT") (pseudocount 0.5) (background 0.25))
  "A log-odds position weight matrix from aligned SITES (equal-length strings); columns hold log2 (frequency / BACKGROUND)."
  (let* ((w (length (first sites))) (n (length sites)) (cols nil))
    (dotimes (j w)
      (push (map 'vector (lambda (a) (log (/ (/ (+ pseudocount (count a sites :key (lambda (s) (char-upcase (char s j))))) (+ n (* pseudocount (length alphabet)))) background) 2.0)) alphabet) cols))
    (%make-pwm alphabet (coerce (nreverse cols) 'vector) n)))

(defun pwm-width (p) "Number of columns of the PWM P." (length (pwm-columns p)))
(defun pwm-score (p s &optional (start 0))
  "Log-odds score of the window of S beginning at START under P."
  (let ((r (%residues s)) (total 0.0))
    (dotimes (j (pwm-width p) total)
      (let ((idx (position (char-upcase (char r (+ start j))) (pwm-alphabet p)))) (setq total (+ total (if idx (aref (aref (pwm-columns p) j) idx) -2.0)))))))
(defun pwm-max-score (p) "The best possible score under P." (loop for col across (pwm-columns p) sum (reduce #'max (coerce col 'list))))
(defun pwm-scan (p s &key (threshold 0.0) both-strands)
  "A list of (position score strand) for windows of S scoring at least THRESHOLD, best first."
  (let* ((r (string-upcase (%residues s))) (w (pwm-width p)) (out nil))
    (flet ((scan (text strand) (loop for i from 0 to (- (length text) w)
                                     do (let ((sc (pwm-score p text i))) (when (>= sc threshold) (push (list (if (> strand 0) i (- (length text) i w)) sc strand) out))))))
      (scan r 1) (when both-strands (scan (reverse-complement r) -1)))
    (sort out (lambda (a b) (> (second a) (second b))))))

(defun pwm-consensus (p) "The consensus string of P (the best symbol per column)." (let ((out (make-string (pwm-width p) #\N))) (dotimes (j (pwm-width p) out) (let* ((col (aref (pwm-columns p) j)) (best (position (reduce #'max (coerce col 'list)) col))) (setf (char out j) (char (pwm-alphabet p) best))))))

;;; regular expressions: literals, ., [set], [^set], groups (), alternation |, quantifiers * + ? {m,n}, anchors ^ $; backtracking matcher

(defun %re-parse (pattern)
  "Parse PATTERN into a tree: (:seq ...) (:alt ...) (:star node min max) (:char c) (:any) (:set negate chars) (:bol) (:eol)."
  (let ((pos 0) (n (length pattern)))
    (labels ((peek () (and (< pos n) (char pattern pos)))
             (next () (prog1 (char pattern pos) (incf pos)))
             (parse-alt () (let ((branches (list (parse-seq)))) (loop while (eql (peek) #\|) do (next) (push (parse-seq) branches)) (if (cdr branches) (cons :alt (nreverse branches)) (car branches))))
             (parse-seq () (let ((items nil)) (loop while (and (peek) (not (member (peek) '(#\| #\))))) do (push (parse-quant) items)) (cons :seq (nreverse items))))
             (parse-quant ()
               (let ((atom (parse-atom)))
                 (loop (case (peek)
                         (#\* (next) (setq atom (list :star atom 0 nil)))
                         (#\+ (next) (setq atom (list :star atom 1 nil)))
                         (#\? (next) (setq atom (list :star atom 0 1)))
                         (#\{ (next) (let ((lo 0) (hi nil) (comma nil))
                                       (loop while (and (peek) (digit-char-p (peek))) do (setq lo (+ (* lo 10) (digit-char-p (next)))))
                                       (when (eql (peek) #\,) (next) (setq comma t) (when (and (peek) (digit-char-p (peek))) (setq hi 0) (loop while (and (peek) (digit-char-p (peek))) do (setq hi (+ (* hi 10) (digit-char-p (next)))))))
                                       (next) (setq atom (list :star atom lo (if comma hi lo)))))
                         (t (return atom))))))
             (parse-atom ()
               (let ((c (next)))
                 (case c
                   (#\. (list :any)) (#\^ (list :bol)) (#\$ (list :eol))
                   (#\( (prog1 (parse-alt) (next)))
                   (#\[ (let ((neg nil) (chars nil))
                          (when (eql (peek) #\^) (next) (setq neg t))
                          (loop until (eql (peek) #\]) do (let ((a (next))) (if (and (eql (peek) #\-) (< (+ pos 1) n) (char/= (char pattern (+ pos 1)) #\]))
                                                                                 (progn (next) (let ((b (next))) (loop for code from (char-code a) to (char-code b) do (push (code-char code) chars))))
                                                                                 (push a chars))))
                          (next) (list :set neg chars)))
                   (#\\ (list :char (next)))
                   (t (list :char c))))))
      (parse-alt))))

(defun %re-match (node text i k)
  "Match NODE at I of TEXT; call K with the end position on success (continuation passing, backtracking)."
  (ecase (car node)
    (:char (and (< i (length text)) (char= (char text i) (cadr node)) (funcall k (+ i 1))))
    (:any (and (< i (length text)) (funcall k (+ i 1))))
    (:set (and (< i (length text)) (let ((in (member (char text i) (caddr node)))) (if (cadr node) (not in) in)) (funcall k (+ i 1))))
    (:bol (and (= i 0) (funcall k i)))
    (:eol (and (= i (length text)) (funcall k i)))
    (:seq (labels ((run (items j) (if (null items) (funcall k j) (%re-match (car items) text j (lambda (e) (run (cdr items) e)))))) (run (cdr node) i)))
    (:alt (some (lambda (b) (%re-match b text i k)) (cdr node)))
    (:star (let ((sub (cadr node)) (lo (caddr node)) (hi (cadddr node)))
             (labels ((try (count j)
                        (or (and (or (null hi) (< count hi))
                                 (%re-match sub text j (lambda (e) (and (> e j) (try (+ count 1) e)))))
                            (and (>= count lo) (funcall k j)))))
               (try 0 i))))))

(defun regex-match-at (pattern text start)
  "The end index of the longest match of the regular expression PATTERN at START in TEXT (greedy), or NIL."
  (let ((tree (%re-parse pattern)) (best nil))
    (%re-match tree text start (lambda (e) (when (or (null best) (> e best)) (setq best e)) nil))
    best))

(defun regex-find-all (pattern s)
  "All non-overlapping matches of PATTERN in S as a list of (start end), scanning left to right."
  (let* ((text (%residues s)) (tree (%re-parse pattern)) (i 0) (out nil))
    (loop while (<= i (length text))
          do (let ((best nil))
               (%re-match tree text i (lambda (e) (when (or (null best) (> e best)) (setq best e)) nil))
               (if (and best (> best i)) (progn (push (list i best) out) (setq i best)) (incf i))))
    (nreverse out)))

(defun regex-search (pattern s)
  "The first match of PATTERN in S as (start end), or NIL."
  (first (regex-find-all pattern s)))
