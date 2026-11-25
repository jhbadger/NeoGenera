;;;; 60-workflows.lisp -- worked analyses that combine the pieces: genome annotation, protein search, variant
;;;; summaries, read-quality control, primer design and enrichment.  Each is documented in the Bioinformatics manual.

(in-package :cl)

;;; ---- genome annotation and protein search ------------------------------------------------------------------------

(defun annotate-orfs (genome &key (min-length 60) (table 11) (id nil))
  "Annotate GENOME (a BIOSEQ or string) with CDS features for every ORF of at least MIN-LENGTH amino acids on both strands.
Each feature carries a /translation and a /locus_tag; features are sorted by start."
  (let* ((seqid (or id (and (bioseq-p genome) (bioseq-id genome)) "genome")) (orfs (find-orfs genome :min-length min-length :table table :both-strands t)) (k 0))
    (mapcar (lambda (o)
              (incf k)
              (%make-bio-feature seqid "CDS" (list (cons (orf-start o) (orf-end o))) (orf-strand o)
                                 (list (cons "locus_tag" (format nil "ORF~4,'0D" k)) (cons "translation" (orf-protein o)) (cons "codon_start" (format nil "~D" (+ 1 (orf-frame o)))))))
            orfs)))

(defun cds-proteins (features)
  "The protein sequences (BIOSEQs) of CDS FEATURES carrying a translation qualifier."
  (let ((out nil))
    (dolist (f features) (let ((p (feature-qualifier f "translation"))) (when (and p (string= (bio-feature-type f) "CDS")) (push (make-bioseq p :id (feature-name f) :alphabet :protein) out))))
    (nreverse out)))

(defun rank-by-alignment (query subjects &key (matrix *blosum62*) (mode :local) (gap-open 11) (gap-extend 1))
  "Align the QUERY against every sequence in SUBJECTS and return a list of (subject alignment bit-score) sorted by score.
The bit score uses the BLOSUM62 gapped parameters lambda = 0.267 and K = 0.041."
  (let ((rows (mapcar (lambda (s) (let ((al (align query s :mode mode :matrix matrix :gap-open gap-open :gap-extend gap-extend)))
                                    (list s al (/ (- (* 0.267 (alignment-score al)) (log 0.041)) (log 2.0)))))
                      subjects)))
    (sort rows (lambda (a b) (> (alignment-score (second a)) (alignment-score (second b)))))))

(defun genome-report (genome &key (min-length 60))
  "A plist summary of GENOME: length, GC content, GC skew, ORF counts, coding fraction and the longest ORF."
  (let* ((features (annotate-orfs genome :min-length min-length)) (n (seq-length genome))
         (coding (reduce #'+ (mapcar #'feature-length features)))
         (longest (and features (reduce (lambda (a b) (if (> (feature-length a) (feature-length b)) a b)) features))))
    (list :length n :gc (gc-content genome) :gc-skew (gc-skew genome) :orfs (length features)
          :forward (count 1 features :key #'bio-feature-strand) :reverse (count -1 features :key #'bio-feature-strand)
          :coding-fraction (/ (float coding) (max 1 n)) :longest-orf (and longest (feature-length longest)))))

;;; ---- variant summary --------------------------------------------------------------------------------------------------

(defun variant-summary (records)
  "A plist summarising VCF-RECORDs: counts of SNPs, insertions, deletions and others, the transition/transversion ratio,
variants per chromosome and passing variants."
  (let ((snp 0) (ins 0) (del 0) (other 0) (ts 0) (tv 0) (pass 0) (chroms (make-hash-table :test 'equal)) (order nil))
    (dolist (v records)
      (let ((ref (vcf-record-ref v)))
        (dolist (alt (vcf-record-alt v))
          (cond ((and (= (length ref) 1) (= (length alt) 1))
                 (incf snp)
                 (if (or (and (find (char ref 0) "AG") (find (char alt 0) "AG")) (and (find (char ref 0) "CT") (find (char alt 0) "CT"))) (incf ts) (incf tv)))
                ((and (= (length ref) 1) (> (length alt) 1)) (incf ins))
                ((and (> (length ref) 1) (= (length alt) 1)) (incf del))
                (t (incf other)))))
      (when (member (vcf-record-filter v) '("PASS" ".") :test #'string=) (incf pass))
      (unless (gethash (vcf-record-chrom v) chroms) (push (vcf-record-chrom v) order))
      (setf (gethash (vcf-record-chrom v) chroms) (+ 1 (gethash (vcf-record-chrom v) chroms 0))))
    (list :variants (length records) :snps snp :insertions ins :deletions del :other other
          :ts-tv (if (zerop tv) nil (/ (float ts) tv)) :pass pass
          :by-chromosome (mapcar (lambda (c) (cons c (gethash c chroms))) (nreverse order)))))

;;; ---- read quality control ----------------------------------------------------------------------------------------------

(defun fastq-qc (reads)
  "A plist of quality statistics for READS (BIOSEQs with qualities): read count, mean length, mean quality, fraction of bases
at Q30 or above, mean GC, N fraction, duplicate fraction and the mean quality at each position."
  (let* ((n (length reads)) (total 0) (q30 0) (nbases 0) (gc 0) (qsum 0) (maxlen (reduce #'max (mapcar #'seq-length reads)))
         (pos-sum (make-array maxlen :initial-element 0)) (pos-n (make-array maxlen :initial-element 0)) (seen (make-hash-table :test 'equal)) (dups 0))
    (dolist (r reads)
      (let ((res (bioseq-residues r)) (q (bioseq-quality r)))
        (if (gethash res seen) (incf dups) (setf (gethash res seen) t))
        (dotimes (i (length res))
          (incf total) (incf qsum (aref q i)) (when (>= (aref q i) 30) (incf q30))
          (let ((c (char res i))) (cond ((or (char= c #\G) (char= c #\C)) (incf gc)) ((char= c #\N) (incf nbases))))
          (incf (aref pos-sum i) (aref q i)) (incf (aref pos-n i)))))
    (list :reads n :mean-length (/ (float total) (max 1 n)) :mean-quality (/ (float qsum) (max 1 total)) :q30-fraction (/ (float q30) (max 1 total))
          :gc (/ (float gc) (max 1 total)) :n-fraction (/ (float nbases) (max 1 total)) :duplicate-fraction (/ (float dups) (max 1 n))
          :per-position-quality (coerce (loop for i from 0 below maxlen collect (if (zerop (aref pos-n i)) 0.0 (/ (float (aref pos-sum i)) (aref pos-n i)))) 'vector))))

(defun coverage-summary (depth)
  "Summary of a depth vector: mean, median, fraction of positions at depth >= 1 and >= 10."
  (let ((l (coerce depth 'list)) (n (length depth)))
    (list :mean (mean l) :median (median l) :covered (/ (float (count-if (lambda (d) (>= d 1)) l)) n) :covered-10x (/ (float (count-if (lambda (d) (>= d 10)) l)) n))))

;;; ---- primer design ---------------------------------------------------------------------------------------------------------

(defun %primer-ok-p (p)
  (let ((gc (gc-content p)) (tm (melting-temperature p)))
    (and (<= 0.40 gc) (<= gc 0.60) (<= 52.0 tm) (<= tm 66.0)
         (find (char p (- (length p) 1)) "GC")
         (not (some (lambda (run) (%string-search run p 0)) '("AAAA" "CCCC" "GGGG" "TTTT")))
         (<= (%self-complementarity p) 4))))

(defun %self-complementarity (p)
  "The longest stretch of P that is the reverse complement of another part of P (a crude hairpin/self-dimer test)."
  (let ((best 0) (n (length p)))
    (loop for k from 3 to (floor n 2)
          do (loop for i from 0 to (- n k) do (when (%string-search (reverse-complement (subseq p i (+ i k))) p (+ i k)) (setq best (max best k)))))
    best))

(defstruct (primer-pair (:copier nil) (:constructor %make-primer-pair (forward reverse forward-start reverse-end product-size tm-difference score)))
  forward reverse forward-start reverse-end product-size tm-difference score)

(defmethod print-object ((p primer-pair) stream)
  (format stream "#<PRIMER-PAIR ~A / ~A product ~Dbp>" (primer-pair-forward p) (primer-pair-reverse p) (primer-pair-product-size p)))

(defun design-primers (template target-start target-end &key (min-length 18) (max-length 25) (max-product 600) (count 3))
  "Design PCR primer pairs flanking the target region [TARGET-START, TARGET-END) of TEMPLATE.  Candidates are 18-25 mers with 40-60%
GC, a G/C 3' end, Tm within 52-66, no homopolymer runs and little self-complementarity; pairs are ranked by Tm difference."
  (let* ((r (string-upcase (%residues template))) (fwd nil) (rev nil))
    (loop for len from min-length to max-length
          do (loop for s from (max 0 (- target-start 150)) to (- target-start len)
                   do (let ((p (subseq r s (+ s len)))) (when (%primer-ok-p p) (push (cons s p) fwd))))
             (loop for e from target-end to (min (length r) (+ target-end 150))
                   do (when (>= (- e len) 0) (let ((p (reverse-complement (subseq r (- e len) e)))) (when (%primer-ok-p p) (push (cons e p) rev))))))
    (let ((pairs nil))
      (dolist (f fwd)
        (dolist (v rev)
          (let ((product (- (car v) (car f))))
            (when (and (> product 0) (<= product max-product))
              (let ((dt (abs (- (melting-temperature (cdr f)) (melting-temperature (cdr v))))))
                (push (%make-primer-pair (cdr f) (cdr v) (car f) (car v) product dt (+ dt (* 0.001 product))) pairs))))))
      (let ((sorted (sort pairs (lambda (a b) (< (primer-pair-score a) (primer-pair-score b))))))
        (if (> (length sorted) count) (subseq sorted 0 count) sorted)))))

;;; ---- enrichment (uses the statistics library) ----------------------------------------------------------------------

(defun enrichment (selected background categories &key (adjust :bh))
  "Over-representation of annotation categories among SELECTED genes.  BACKGROUND is the list of all genes and CATEGORIES an alist
(category . list-of-genes).  Returns a list of (category overlap size odds-ratio p adjusted-p) sorted by p, from one-sided Fisher exact tests."
  (let* ((sel (remove-duplicates (remove-if-not (lambda (g) (member g background :test #'equal)) selected) :test #'equal)) (rows nil))
    (dolist (c categories)
      (let* ((members (remove-duplicates (remove-if-not (lambda (g) (member g background :test #'equal)) (cdr c)) :test #'equal))
             (a (length (intersection sel members :test #'equal))) (b (- (length sel) a)) (cc (- (length members) a)) (d (- (length background) a b cc))
             (h (fisher-exact a b cc d :alternative :greater)))
        (push (list (car c) a (length members) (htest-estimate h) (htest-p-value h)) rows)))
    (let* ((sorted (sort rows (lambda (x y) (< (fifth x) (fifth y))))) (adj (p-adjust (mapcar #'fifth sorted) adjust)))
      (loop for r in sorted for q in adj collect (append r (list q))))))
