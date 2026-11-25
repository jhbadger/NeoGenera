;;;; bio-tests.lisp -- sequences, formats, alignment, search, phylogeny, intervals, tools, presentations, workflows.

(in-package :cl)

(defun fixture (name) (%string-concat "tests/data/" name))

;;; ---- reference implementations used to cross-check the C kernels ----------------------------------------------------

(defun ref-align-score (x y mode open ext matrix)
  "A straightforward Gotoh H/E/F dynamic program in Lisp (no traceback) for cross-checking %DP-ALIGN."
  (let* ((n (length x)) (m (length y)) (w (+ m 1)) (neg -1000000) (size (* (+ n 1) w))
         (h (make-array size :initial-element 0)) (e (make-array size :initial-element neg)) (f (make-array size :initial-element neg)) (best 0))
    (flet ((ix (i j) (+ (* i w) j)))
      (dotimes (i (+ n 1))
        (dotimes (j (+ m 1))
          (cond ((and (= i 0) (= j 0)) (setf (aref h 0) 0))
                ((= i 0) (setf (aref h (ix 0 j)) (if (eq mode :global) (- (+ open (* (- j 1) ext))) 0)) (setf (aref f (ix 0 j)) (aref h (ix 0 j))))
                ((= j 0) (setf (aref h (ix i 0)) (if (eq mode :global) (- (+ open (* (- i 1) ext))) 0)) (setf (aref e (ix i 0)) (aref h (ix i 0))))
                (t (setf (aref e (ix i j)) (max (- (aref e (ix (- i 1) j)) ext) (- (aref h (ix (- i 1) j)) open)))
                   (setf (aref f (ix i j)) (max (- (aref f (ix i (- j 1))) ext) (- (aref h (ix i (- j 1))) open)))
                   (let ((v (max (+ (aref h (ix (- i 1) (- j 1))) (score-pair matrix (char x (- i 1)) (char y (- j 1)))) (aref e (ix i j)) (aref f (ix i j)))))
                     (setf (aref h (ix i j)) (if (eq mode :local) (max 0 v) v)))))))
      (ecase mode
        (:global (aref h (ix n m)))
        (:local (dotimes (i (+ n 1)) (dotimes (j (+ m 1)) (setq best (max best (aref h (ix i j)))))) best)
        (:semiglobal (let ((b -1000000)) (dotimes (i (+ n 1)) (setq b (max b (aref h (ix i m))))) (dotimes (j (+ m 1)) (setq b (max b (aref h (ix n j))))) b))))))

(defun rescore-alignment (al matrix open ext)
  "Recompute the score of the aligned strings of AL from first principles."
  (let ((a (alignment-aligned-a al)) (b (alignment-aligned-b al)) (total 0) (prev nil))
    (dotimes (i (length a) total)
      (let ((x (char a i)) (y (char b i)))
        (cond ((char= x #\-) (decf total (if (eq prev :a) ext open)) (setq prev :a))
              ((char= y #\-) (decf total (if (eq prev :b) ext open)) (setq prev :b))
              (t (incf total (score-pair matrix x y)) (setq prev nil)))))))

(defun random-string (n alphabet rng) (let ((s (make-string n #\A))) (dotimes (i n s) (setf (char s i) (char alphabet (rand-int (length alphabet) rng))))))

;;; ---- sequences ----------------------------------------------------------------------------------------------------------

(deftest sequences-and-translation
  (is-equal "GCAT" (reverse-complement "ATGC")) (is-equal "GAATTC" (reverse-complement "GAATTC")) (is-equal "RYKM" (complement-seq "YRMK"))
  (is-equal "AUGC" (transcribe "ATGC")) (is-equal "ATGC" (reverse-transcribe "AUGC"))
  (is-equal :dna (bioseq-alphabet (make-bioseq "acgtn"))) (is-equal :rna (bioseq-alphabet (make-bioseq "ACGU"))) (is-equal :protein (bioseq-alphabet (make-bioseq "MKVLAAG")))
  (is-equal "ACGT" (bioseq-residues (make-bioseq "acgt")))
  (let ((s (make-bioseq "ATGCGC" :id "x"))) (is-equal "GCGCAT" (bioseq-residues (reverse-complement s))) (is-equal "x" (bioseq-id (reverse-complement s))) (is-equal 3 (seq-length (seq-subseq s 3))))
  (is (near (gc-content "ATGC") 0.5)) (is (near (gc-content "GGGG") 1.0)) (is (near (gc-skew "GGGC") 0.5)) (is-equal 3 (length (gc-windows "ACGTACGTAC" 4 3)))
  (is-equal '((#\A . 2) (#\C . 1) (#\G . 1)) (base-counts "AGAC"))
  ;; the standard code: Wikipedia's example sequence
  (is-equal "MAIVMGR*KGAR*" (translate "ATGGCCATTGTAATGGGCCGCTGAAAGGGTGCCCGATAG"))
  (is-equal "MAIVMGR" (translate "ATGGCCATTGTAATGGGCCGCTGAAAGGGTGCCCGATAG" :to-stop t))
  (is-equal "AIVMGR*KGAR*" (translate "xATGGCCATTGTAATGGGCCGCTGAAAGGGTGCCCGATAG" :frame 4))
  (is-equal "MAIV" (translate "AUGGCCAUUGUA"))
  (is-equal "MKX" (translate "ATGAAANNN"))
  (is-equal #\W (codon-amino "TGA" 2)) (is-equal #\* (codon-amino "TGA" 1)) (is-equal #\M (codon-amino "ATA" 2)) (is-equal #\I (codon-amino "ATA" 11)) (is-equal #\S (codon-amino "AGA" 5)) (is-equal #\* (codon-amino "AGA" 2))
  (is-equal 5 (length *genetic-codes*)) (signals-error (find-genetic-code 99))
  ;; all 64 codons are covered and the standard code has exactly three stops
  (is-equal 3 (count #\* (genetic-code-aminos (find-genetic-code 1)))) (is-equal 64 (length (genetic-code-aminos (find-genetic-code 11))))
  (is-equal '("ACG" "CGT" "GTA") (kmers "ACGTA" 3)) (is-equal 2 (gethash "AC" (kmer-counts "ACACG" 2)))
  (is-equal 2 (gethash "AC" (kmer-counts "ACGT" 2 :canonical t))) (is-equal "ACG" (canonical-kmer "CGT")) (is-equal '("AC" . 3) (first (kmer-spectrum "ACACAC" 2)))
  (is-equal 2 (hamming-distance "ACGT" "ACCA")) (is-equal 3 (edit-distance "kitten" "sitting")) (is-equal 0 (edit-distance "" "")) (is-equal 4 (edit-distance "" "abcd"))
  (is (near (protein-mass "ACDEFGHIKLMNPQRSTVWY") 2395.6 1e-3)) (is (near (protein-mass "G") 75.067 1e-4))
  (is (> (first (hydropathy-profile "IIIIIIIIIDDDDDDDDD" 5)) 4.0)) (is (< (car (last (hydropathy-profile "IIIIIIIIIDDDDDDDDD" 5))) -3.0))
  (is (near (melting-temperature "ATGC") 12.0)) (is (near (melting-temperature "ACGTACGTACGTACGTACGT") 51.8 0.01))
  (let ((d (random-dna 1000 :gc 0.7 :rng (make-rng 1)))) (is-equal 1000 (length d)) (is (near (gc-content d) 0.7 0.1)))
  (let* ((d (random-dna 1000 :rng (make-rng 2))) (m (mutate-dna d 0.1 :rng (make-rng 3)))) (is (< 50 (hamming-distance d m) 160))))

(deftest packed-sequences
  (let* ((s (random-dna 1001 :rng (make-rng 4))) (p (pack-dna s)))
    (is-equal 1001 (packed-seq-length p)) (is-equal s (unpack-seq p)) (is-equal (char s 500) (packed-ref p 500)) (is-equal (subseq s 100 130) (packed-subseq p 100 130))
    (is (< (packed-bytes p) 260)) (is-equal 2 (packed-seq-bits p)))
  (let ((p (pack-dna "ACGTNACGTRA"))) (is-equal "ACGTNACGTRA" (unpack-seq p)) (is-equal '((4 . #\N) (9 . #\R)) (packed-seq-exceptions p)))
  (let ((p (pack-iupac "ACGTRYSWKMBDHVN-"))) (is-equal "ACGTRYSWKMBDHVN-" (unpack-seq p)) (is-equal 4 (packed-seq-bits p)))
  (let ((p (pack-protein "MKVLAAGIVGLLLAQPSWA*"))) (is-equal "MKVLAAGIVGLLLAQPSWA*" (unpack-seq p)) (is-equal 5 (packed-seq-bits p)))
  (signals-error (pack-seq "ACGU" "ACGT")) (is-equal "" (unpack-seq (pack-dna ""))))

(deftest open-reading-frames
  (let* ((rng (make-rng 11)) (codons (let ((acc nil)) (dotimes (i 64) (let ((c (map 'string (lambda (k) (char "TCAG" k)) (list (floor i 16) (mod (floor i 4) 4) (mod i 4))))) (unless (find (codon-amino c) "*") (push c acc)))) acc))
         (gene (lambda (naa) (apply #'%string-concat "ATG" (append (loop repeat (- naa 1) collect (rand-choice codons rng)) (list (rand-choice '("TAA" "TAG" "TGA")))))))
         (g1 (funcall gene 120)) (g2 (funcall gene 90)) (filler (lambda (n) (random-string n "ACGT" rng)))
         (genome (%string-concat (funcall filler 10) "TAA" g1 (funcall filler 40) (reverse-complement (%string-concat "TAA" g2)) (funcall filler 10)))
         (orfs (find-orfs genome :min-length 85 :both-strands t)))
    (let ((f (find 1 orfs :key #'orf-strand :test #'eql)) (r (find -1 orfs :key #'orf-strand :test #'eql)))
      (is f) (is r)
      (is-equal 13 (orf-start f)) (is-equal (+ 13 (length g1)) (orf-end f)) (is-equal (translate g1 :to-stop t) (orf-protein f)) (is-equal 120 (orf-length f))
      (is-equal (+ 13 (length g1) 40) (orf-start r)) (is-equal (+ 13 (length g1) 40 (length g2)) (orf-end r))
      (is-equal (translate g2 :to-stop t) (orf-protein r))
      (is-equal (subseq genome (orf-start r) (orf-end r)) (reverse-complement (%string-concat g2))))
    (is-equal 1 (length (find-orfs genome :min-length 85))) ; forward strand only: the second gene is on the reverse strand
    (is (>= (length (find-orfs genome :min-length 85 :both-strands t)) 2))
    (let ((starts (mapcar #'orf-start (find-orfs genome :min-length 85 :both-strands t)))) (is-equal starts (sort (copy-list starts) #'<)))))

;;; ---- file formats -------------------------------------------------------------------------------------------------------

(deftest fasta-fastq
  (let ((seqs (read-fasta (fixture "tiny.fa"))))
    (is-equal 3 (length seqs)) (is-equal "seq1" (bioseq-id (first seqs))) (is-equal "first test sequence" (bioseq-description (first seqs))) (is-equal 42 (seq-length (first seqs)))
    (is-equal :protein (bioseq-alphabet (third seqs))) (is-equal nil (bioseq-description (second seqs))) (is-equal "MAIVMGR*KGAR*" (bioseq-residues (translate (second seqs)))))
  (let ((gz (read-fasta (fixture "tiny.fa.gz")))) (is-equal (mapcar #'bioseq-residues (read-fasta (fixture "tiny.fa"))) (mapcar #'bioseq-residues gz)))
  (let ((n 0) (len 0)) (map-fasta (lambda (s) (incf n) (incf len (seq-length s))) (fixture "tiny.fa")) (is-equal 3 n) (is-equal (+ 42 39 19) len))
  (let* ((seqs (list (make-bioseq (random-dna 130 :rng (make-rng 5)) :id "a" :description "d e") (make-bioseq "ACGT" :id "b"))) (text (fasta-string seqs :width 50)))
    (is (%string-search ">a d e" text 0)) (is-equal 2 (count-substring (format nil "~%>") (format nil "~%~A" text)))
    (let ((back (parse-fasta (split-lines text)))) (is-equal (mapcar #'bioseq-residues seqs) (mapcar #'bioseq-residues back)) (is-equal "d e" (bioseq-description (first back)))))
  (let ((path "/tmp/ng-bio.fa")) (write-fasta (list (make-bioseq "ACGTACGT" :id "w")) path) (is-equal "w" (bioseq-id (first (read-fasta path)))) (delete-file path))
  (is-equal nil (parse-fasta nil)) (is-equal 1 (length (parse-fasta '(">only" "ACGT" "" "TTTT"))))
  (let ((reads (read-fastq (fixture "tiny.fq"))))
    (is-equal 2 (length reads)) (is-equal "read1" (bioseq-id (first reads))) (is-equal "sample A" (bioseq-description (first reads)))
    (is-equal '(40 40 40 40 40 40 40 40 2 20) (coerce (bioseq-quality (first reads)) 'list)) (is (near (mean-quality (second reads)) 32.0 1e-9))
    (is-equal 8 (seq-length (quality-trim (first reads) 30))) (is (near (error-probability 20) 0.01 1e-12))
    (is-equal (format nil "@read1 sample A~%ACGTNACGTA~%+~%IIIIIIII#5~%@read2~%GGGGCCCCAA~%+~%5555IIIIII~%") (fastq-string reads)))
  (signals-error (parse-fastq '("@r" "ACGT" "+" "II"))) (signals-error (parse-fastq '("r" "ACGT" "+" "IIII"))))

(deftest genbank-gff-bed
  (let* ((rec (first (read-genbank (fixture "tiny.gb")))) (fs (gb-record-features rec)))
    (is-equal "TESTSEQ" (gb-record-locus rec)) (is-equal "TS000001" (gb-record-accession rec)) (is-equal "Testus organismus" (gb-record-organism rec))
    (is-equal "Test sequence with two genes." (gb-record-definition rec)) (is-equal 60 (seq-length (gb-record-sequence rec))) (is-equal 4 (length fs))
    (let ((a (second fs)) (cds-a (third fs)) (b (fourth fs)))
      (is-equal "gene" (bio-feature-type a)) (is-equal '((0 . 18)) (bio-feature-intervals a)) (is-equal "geneA" (feature-name a)) (is-equal 1 (bio-feature-strand a))
      (is-equal "hypothetical protein A" (feature-qualifier cds-a "product")) (is-equal "MKVLAA" (feature-qualifier cds-a "translation"))
      (is-equal "MKVLAA" (translate (feature-sequence cds-a (gb-record-sequence rec))))
      (is-equal '((24 . 36) (39 . 51)) (bio-feature-intervals b)) (is-equal -1 (bio-feature-strand b)) (is-equal 24 (feature-start b)) (is-equal 51 (feature-end b)) (is-equal 24 (feature-length b))
      (is-equal 24 (length (feature-sequence b (gb-record-sequence rec))))))
  (multiple-value-bind (iv strand) (parse-genbank-location "complement(join(1..10,20..30))") (is-equal '((0 . 10) (19 . 30)) iv) (is-equal -1 strand))
  (is-equal '((4 . 5)) (parse-genbank-location "5")) (is-equal '((0 . 10)) (parse-genbank-location "<1..>10"))
  (let ((fs (read-gff (fixture "tiny.gff3"))))
    (is-equal 4 (length fs)) (is-equal '((100 . 200)) (bio-feature-intervals (first fs))) (is-equal "alpha" (feature-name (first fs))) (is-equal -1 (bio-feature-strand (third fs)))
    (is-equal "0" (feature-qualifier (second fs) "phase")) (is-equal "g1" (feature-qualifier (second fs) "Parent")) (is-equal "5.5" (feature-qualifier (fourth fs) "score")))
  (let ((fs (read-gff (fixture "tiny.gtf") :gtf t))) (is-equal 2 (length fs)) (is-equal "G1" (feature-qualifier (first fs) "gene_id")) (is-equal "T1" (feature-qualifier (second fs) "transcript_id")))
  (let ((fs (read-bed (fixture "tiny.bed"))))
    (is-equal 3 (length fs)) (is-equal '((100 . 200)) (bio-feature-intervals (first fs))) (is-equal "featA" (feature-name (first fs))) (is-equal -1 (bio-feature-strand (second fs))) (is-equal 0 (bio-feature-strand (third fs))))
  (is (stringp (princ-to-string (first (read-bed (fixture "tiny.bed")))))))

(deftest vcf-sam-bam
  (multiple-value-bind (recs samples) (read-vcf (fixture "tiny.vcf"))
    (is-equal '("S1" "S2") samples) (is-equal 5 (length recs)) (is-equal 100 (vcf-record-pos (first recs))) (is-equal '("G") (vcf-record-alt (first recs))) (is (near (vcf-record-qual (first recs)) 50.0))
    (is (vcf-snp-p (first recs))) (is (not (vcf-snp-p (third recs)))) (is-equal "30" (vcf-info (first recs) "DP")) (is-equal t (vcf-info (third recs) "DB")) (is-equal nil (vcf-info (fifth recs) "DP"))
    (is-equal '(0 1) (vcf-genotype (first recs) 0)) (is-equal '(1 1) (vcf-genotype (first recs) 1)) (is-equal '(0 1) (vcf-genotype (second recs) 1)) (is-equal '(nil nil) (vcf-genotype (third recs) 1))
    (let ((s (variant-summary recs)))
      (is-equal 5 (getf s :variants)) (is-equal 3 (getf s :snps)) (is-equal 1 (getf s :insertions)) (is-equal 1 (getf s :deletions)) (is (near (getf s :ts-tv) 2.0)) (is-equal 4 (getf s :pass))
      (is-equal '(("chr1" . 3) ("chr2" . 2)) (getf s :by-chromosome))))
  (multiple-value-bind (recs header) (read-sam (fixture "tiny.sam"))
    (is-equal 4 (length recs)) (is-equal 3 (length header)) (let ((r (second recs)))
      (is-equal "r2" (sam-record-qname r)) (is-equal '((4 . #\M) (2 . #\I) (4 . #\M)) (sam-record-cigar r)) (is-equal "4M2I4M" (cigar-string (sam-record-cigar r))) (is (sam-reverse-p r)) (is-equal 8 (cigar-reference-length (sam-record-cigar r)))
      (is-equal 10 (cigar-query-length (sam-record-cigar r))) (is-equal 17 (sam-end r)) (is-equal 2 (sam-tag r "NM")))
    (is-equal "grp1" (sam-tag (first recs) "RG")) (is (not (sam-mapped-p (fourth recs)))) (is (sam-mapped-p (first recs))) (is-equal nil (parse-cigar "*")) (is-equal 14 (sam-end (third recs))))
  ;; the BAM file holds the same records: two independent parsers must agree
  (multiple-value-bind (bam text refs) (read-bam (fixture "tiny.bam"))
    (let ((sam (read-sam (fixture "tiny.sam"))))
      (is-equal 4 (length bam)) (is-equal '(("chr1" . 100) ("chr2" . 50)) refs) (is (%string-search "@SQ" text 0))
      (loop for a in bam for b in sam
            do (is-equal (sam-record-qname b) (sam-record-qname a)) (is-equal (sam-record-flag b) (sam-record-flag a)) (is-equal (sam-record-rname b) (sam-record-rname a))
               (is-equal (sam-record-pos b) (sam-record-pos a)) (is-equal (sam-record-mapq b) (sam-record-mapq a)) (is-equal (sam-record-cigar b) (sam-record-cigar a))
               (is-equal (sam-record-seq b) (sam-record-seq a)) (is-equal (sam-record-qual b) (sam-record-qual a)) (is-equal (sam-record-tags b) (sam-record-tags a)))))
  (let ((depth (pileup-depth (read-sam (fixture "tiny.sam")) "chr1" 100)))
    (is-equal 1 (aref depth 4)) (is-equal 2 (aref depth 10)) (is-equal 0 (aref depth 20)) (is-equal 16 (reduce #'+ (coerce depth 'list)))
    (is (near (getf (coverage-summary depth) :covered) 0.13 1e-9)))
  (signals-error (parse-bam-bytes "not a bam file")))

;;; ---- alignment ---------------------------------------------------------------------------------------------------------

(deftest scoring-matrices
  (dolist (m (list *blosum62* *pam250*))
    (let ((a (scoring-matrix-alphabet m)))
      (dotimes (i 20) (dotimes (j 20) (is-equal (score-pair m (char a i) (char a j)) (score-pair m (char a j) (char a i)))))))
  (is-equal '(4 5 6 6 9 5 5 6 8 4 4 5 5 6 7 4 5 11 7 4) (loop for c across *blosum62-alphabet* collect (score-pair *blosum62* c c)))
  (is-equal '(2 6 2 4 12 4 4 5 6 5 6 5 6 9 6 2 3 17 10 4) (loop for c across *blosum62-alphabet* collect (score-pair *pam250* c c)))
  (is-equal -2 (score-pair *blosum62* #\W #\C)) (is-equal 2 (score-pair *blosum62* #\I #\L)) (is-equal 2 (score-pair *blosum62* #\E #\D)) (is-equal -3 (score-pair *blosum62* #\W #\A)) (is-equal 3 (score-pair *blosum62* #\I #\V))
  (is-equal -6 (score-pair *pam250* #\W #\A)) (is-equal 7 (score-pair *pam250* #\F #\Y)) (is-equal -1 (score-pair *blosum62* #\X #\A)) (is-equal 4 (score-pair *blosum62* #\a #\a))
  (let ((n (nucleotide-matrix 2 -3))) (is-equal 2 (score-pair n #\A #\A)) (is-equal -3 (score-pair n #\A #\C)) (is-equal 0 (score-pair n #\N #\A)))
  (signals-error (make-scoring-matrix "bad" "AC" '((1 2 3)))))

(deftest pairwise-alignment
  ;; Needleman-Wunsch (Wikipedia): GATTACA / GCATGCU with match 1, mismatch -1, gap -1 scores 0
  (let ((al (align "GATTACA" "GCATGCU" :mode :global :matrix (nucleotide-matrix 1 -1) :gap-open 1 :gap-extend 1)))
    (is-equal 0 (alignment-score al)) (is-equal 0 (rescore-alignment al (nucleotide-matrix 1 -1) 1 1)) (is-equal "GATTACA" (remove #\- (alignment-aligned-a al))) (is-equal "GCATGCU" (remove #\- (alignment-aligned-b al))))
  ;; Smith-Waterman (Wikipedia): TGTTACGG / GGTTGACTA, match 3, mismatch -3, gap -2: score 13, GTT-AC / GTTGAC
  (let ((al (align "TGTTACGG" "GGTTGACTA" :mode :local :matrix (nucleotide-matrix 3 -3) :gap-open 2 :gap-extend 2)))
    (is-equal 13 (alignment-score al)) (is-equal "GTT-AC" (alignment-aligned-a al)) (is-equal "GTTGAC" (alignment-aligned-b al)) (is-equal 1 (alignment-a-start al)) (is-equal 6 (alignment-a-end al)) (is-equal 1 (alignment-b-start al)))
  ;; identical sequences: the score is the sum of the diagonal
  (is-equal (+ 11 9 11) (alignment-score (align "WCW" "WCW" :mode :local :matrix *blosum62*))) (is-equal (+ 11 9 11) (alignment-score (align (make-bioseq "WCW" :alphabet :protein) "WCW" :mode :global)))
  (is (near (alignment-identity (align "ACGTACGT" "ACGTACGT")) 1.0)) (is-equal 0 (alignment-gaps (align "ACGTACGT" "ACGTACGT")))
  ;; an inserted block costs one gap opening plus extensions
  (let ((al (align "ACGTACGTAC" "ACGTTTTACGTAC" :mode :global :matrix (nucleotide-matrix 5 -4) :gap-open 6 :gap-extend 1)))
    (is-equal (- (* 5 10) (+ 6 2)) (alignment-score al)) (is-equal 3 (alignment-gaps al)))
  ;; semi-global: a read inside a longer sequence aligns without end-gap penalty
  (let ((al (align "ACGTAGCTAGC" "TTTTTACGTAGCTAGCTTTTT" :mode :semiglobal :matrix (nucleotide-matrix 5 -4) :gap-open 6 :gap-extend 1)))
    (is-equal 55 (alignment-score al)) (is-equal 5 (alignment-b-start al)) (is-equal 16 (alignment-b-end al)) (is-equal 0 (alignment-gaps al)))
  ;; local alignment of unrelated sequences can be empty
  (is-equal 0 (alignment-score (align "AAAA" "TTTT" :mode :local)))
  (is (stringp (alignment-string (align "HEAGAWGHEE" "PAWHEAE" :mode :local) :matrix *blosum62*))) (is (stringp (princ-to-string (align "ACGT" "ACGT"))))
  (is (near (percent-identity "ACGTACGT" "ACGTACGA") 87.5 1e-9)) (is-equal 40 (align-score "ACGTACGT" "ACGTACGT")) (is-equal (alignment-aligned-a (alignment-from-strings "AC-T" "ACGT")) "AC-T")
  (is-equal "|||" (alignment-midline (align "ACG" "ACG")))
  ;; the C kernel agrees with an independent Lisp implementation, and the traceback reproduces the score
  (let ((rng (make-rng 77)) (bad 0))
    (dolist (mode '(:global :local :semiglobal))
      (dolist (gaps '((10 1) (5 5) (3 2) (11 1)))
        (dotimes (trial 12)
          (let* ((x (random-string (+ 3 (rand-int 25 rng)) "ACGT" rng)) (y (random-string (+ 3 (rand-int 25 rng)) "ACGT" rng)) (m (nucleotide-matrix 5 -4))
                 (al (align x y :mode mode :matrix m :gap-open (first gaps) :gap-extend (second gaps))))
            (unless (and (= (alignment-score al) (ref-align-score x y mode (first gaps) (second gaps) m))
                         (= (alignment-score al) (rescore-alignment al m (first gaps) (second gaps))))
              (incf bad) (format t "MISMATCH ~A ~A ~A ~A~%" mode gaps x y))))))
    (dotimes (trial 10)
      (let* ((x (random-string (+ 5 (rand-int 25 rng)) "ARNDCQEGHILKMFPSTWYV" rng)) (y (random-string (+ 5 (rand-int 25 rng)) "ARNDCQEGHILKMFPSTWYV" rng)) (al (align x y :mode :local :gap-open 11 :gap-extend 1)))
        (unless (and (= (alignment-score al) (ref-align-score x y :local 11 1 *blosum62*)) (= (alignment-score al) (rescore-alignment al *blosum62* 11 1))) (incf bad))))
    (is-equal 0 bad))
  ;; a band wide enough gives the full result; a narrow band cannot do better
  (let* ((rng (make-rng 78)) (x (random-dna 80 :rng rng)) (y (mutate-dna x 0.05 :rng rng)))
    (is-equal (alignment-score (align x y)) (alignment-score (align x y :band 40))) (is (<= (alignment-score (align x y :band 2)) (alignment-score (align x y)))) (is (> (alignment-score (align x y :band 3)) 300))))

;;; ---- indexes and search --------------------------------------------------------------------------------------------------

(deftest suffix-arrays-and-search
  (let ((rng (make-rng 21)))
    (dolist (text (list "banana" "mississippi" "aaaaaaaa" "" "a" (random-dna 200 :rng rng) (random-string 300 "AC" rng)))
      (let* ((idx (build-suffix-index text)) (naive (sort (loop for i from 0 below (length text) collect i) (lambda (a b) (string< (subseq text a) (subseq text b))))))
        (is-equal naive (coerce (suffix-index-array idx) 'list))))
    (let* ((text (random-dna 2000 :rng rng)) (idx (build-suffix-index text)))
      (dolist (pat (list "ACGT" "GG" "TTTTT" (subseq text 700 712) "ACGTACGTACGTACGTACGTAGGG"))
        (let ((naive (loop for i from 0 to (- (length text) (length pat)) when (string= (subseq text i (+ i (length pat))) pat) collect i)))
          (is-equal naive (suffix-index-search idx pat))))))
  (is-equal '(1 3) (suffix-index-search (build-suffix-index "banana") "ana")) (is-equal '(0) (suffix-index-search (build-suffix-index "banana") "banana")) (is-equal nil (suffix-index-search (build-suffix-index "banana") "nab"))
  (multiple-value-bind (s positions) (longest-repeat (build-suffix-index "xxabcdyyabcdzz")) (is-equal "abcd" s) (is-equal '(2 8) positions))
  ;; a larger index builds quickly in C
  (let ((idx (build-suffix-index (random-dna 200000 :rng (make-rng 22))))) (is-equal 200000 (length (suffix-index-array idx)))))

(deftest seeded-search
  (let* ((rng (make-rng 31)) (genome (random-dna 5000 :rng rng)) (decoy (random-dna 5000 :rng rng))
         (subjects (list (make-bioseq genome :id "genome") (make-bioseq decoy :id "decoy")))
         (index (build-kmer-index subjects :k 11)) (query (mutate-dna (subseq genome 2000 2200) 0.03 :rng rng))
         (hits (seeded-search query index :min-score 100)))
    (is hits) (is-equal "genome" (bioseq-id (hit-subject (first hits)))) (is (< (abs (- (hit-subject-start (first hits)) 2000)) 6)) (is (> (hit-score (first hits)) 700))
    (is (every (lambda (h) (string= (bioseq-id (hit-subject h)) "genome")) hits)) (is (stringp (princ-to-string (first hits))))
    (is (null (seeded-search (random-dna 100 :rng (make-rng 32)) index :min-score 200))))
  ;; a reverse-complemented query is found by searching its reverse complement
  (let* ((genome (random-dna 3000 :rng (make-rng 33))) (index (build-kmer-index (list (make-bioseq genome :id "g")) :k 12)) (q (reverse-complement (subseq genome 1000 1100))))
    (is (null (seeded-search q index :min-score 300))) (is (seeded-search (reverse-complement q) index :min-score 300))))

(deftest motifs
  (is (iupac-match-p #\R #\A)) (is (iupac-match-p #\R #\G)) (is (not (iupac-match-p #\R #\C))) (is (iupac-match-p #\N #\T)) (is (iupac-match-p #\y #\c))
  (is-equal '(3 9) (find-iupac-motif "GATC" "AAAGATCAAGATCA")) (is-equal '(1) (find-iupac-motif "RCW" "AACTG")) (is-equal '((0 . 1) (0 . -1)) (find-iupac-motif "GAATTC" "GAATTC" :both-strands t))
  (let* ((sites '("TATAAT" "TATAAT" "TATGAT" "TACAAT" "TATAAT" "AATAAT")) (p (make-pwm sites)))
    (is-equal 6 (pwm-width p)) (is-equal "TATAAT" (pwm-consensus p)) (is (> (pwm-score p "TATAAT") (pwm-score p "GCGCGC"))) (is (near (pwm-max-score p) (pwm-score p "TATAAT") 1e-9))
    (let ((hits (pwm-scan p "GGGGGGGGGGTATAATGGGGGGG" :threshold 5.0))) (is-equal 10 (first (first hits))) (is-equal 1 (third (first hits))))
    (is-equal '(10) (mapcar #'first (pwm-scan p "GGGGGGGGGGTATAATGGGGGGG" :threshold 5.0)))
    (let ((hits (pwm-scan p (reverse-complement "GGGGGGGGGGTATAATGGGGGGG") :threshold 5.0 :both-strands t))) (is (find -1 hits :key #'third))))
  ;; regular expressions
  (is-equal '((2 5)) (regex-find-all "ACG" "TTACGTT")) (is-equal '((0 3) (4 7)) (regex-find-all "A.G" "AAG ACG")) (is-equal '((0 6)) (regex-find-all "(AC)+G*T*" "ACACGG"))
  (is-equal '((1 4)) (regex-find-all "[CG]{3}" "ACGCAT")) (is-equal '((0 1) (1 2) (3 4) (4 5)) (regex-find-all "A|T" "AT AT" )) (is-equal 3 (regex-match-at "A+" "AAAB" 0)) (is-equal nil (regex-match-at "B" "AAAB" 0))
  (is-equal '((0 4)) (regex-find-all "^ATGC" "ATGCATGC")) (is-equal '((4 8)) (regex-find-all "ATGC$" "ATGCATGC")) (is-equal '((0 3)) (regex-find-all "[^T]+" "ACGT")) (is-equal '((0 2) (3 5)) (regex-find-all "A?T" "AT AT"))
  (is-equal '(0 6) (regex-search "N?AT[GC]+" "NATGCC")) (is-equal nil (regex-search "XYZ" "ACGT"))
  (is-equal '((0 5)) (regex-find-all "A{2,3}C{1,}G" "AACCG")) (is-equal '((0 11)) (regex-find-all "(AT|GC)+A+" "ATGCATGCAAA"))
  ;; a promoter-style search: -35 box, 15-19 spacer, -10 box
  (is-equal 1 (length (regex-find-all "TTGACA[ACGT]{15,19}TATAAT" (%string-concat "GGG" "TTGACA" "ACGTACGTACGTACGTA" "TATAAT" "CCC"))))
  (is-equal 0 (length (regex-find-all "TTGACA[ACGT]{15,19}TATAAT" (%string-concat "GGG" "TTGACA" "ACGTACGTACGTAC" "TATAAT" "CCC")))))

;;; ---- phylogeny ---------------------------------------------------------------------------------------------------------

(deftest trees
  (let ((tree (parse-newick "((A:0.1,B:0.2):0.3,(C:0.4,D:0.5):0.6);")))
    (is-equal '("A" "B" "C" "D") (tree-leaf-names tree)) (is-equal 7 (tree-node-count tree)) (is-equal "((A:0.1,B:0.2):0.3,(C:0.4,D:0.5):0.6);" (tree-newick tree)) (is-equal "((A,B),(C,D));" (tree-newick tree nil))
    (is (near (tree-height tree) 1.1 1e-12)) (is (near (tree-total-length tree) 2.1 1e-12)) (is (phylo-leaf-p (tree-find tree "C"))) (is-equal nil (tree-find tree "Z"))
    (let ((d (tree-patristic-distances tree))) (is (near (dmatrix-ref d "A" "B") 0.3)) (is (near (dmatrix-ref d "A" "C") 1.4)) (is (near (dmatrix-ref d "D" "B") 1.6)) (is (near (dmatrix-ref d "C" "C") 0.0)))
    (let ((text (tree-ascii tree))) (is (%string-search "A (0.1)" text 0)) (is-equal 7 (length (split-lines text))))
    (is-equal 0 (robinson-foulds tree (parse-newick "((B,A),(D,C));"))) (is-equal 2 (robinson-foulds tree (parse-newick "((A,C),(B,D));"))) (is-equal '(("A" "B")) (tree-bipartitions tree)))
  (is-equal '("a" "b c") (tree-leaf-names (parse-newick "('a','b c');"))) (is-equal 3 (length (tree-leaves (parse-newick "(A,B,C);")))) (is-equal "A" (phylo-node-name (parse-newick "A;"))) (signals-error (parse-newick "((A,B;"))
  (is-equal "(A:1.5,B:2);" (tree-newick (make-phylo-node (list (make-phylo-leaf "A" 1.5) (make-phylo-leaf "B" 2.0))))))

(deftest tree-building
  (let* ((true (parse-newick "((A:0.1,B:0.2):0.3,(C:0.4,D:0.5):0.6);")) (d (tree-patristic-distances true)) (nj (neighbor-joining d)))
    ;; neighbor joining recovers an additive tree: same topology and the same path lengths
    (is-equal 0 (robinson-foulds true nj))
    (let ((d2 (tree-patristic-distances nj))) (dolist (a '("A" "B" "C" "D")) (dolist (b '("A" "B" "C" "D")) (is (near (dmatrix-ref d a b) (dmatrix-ref d2 a b) 1e-9))))))
  ;; UPGMA recovers an ultrametric tree
  (let* ((true (parse-newick "((A:1,B:1):1,C:2);")) (tree (upgma (tree-patristic-distances true))))
    (is-equal 0 (robinson-foulds true tree)) (is (near (tree-height tree) 2.0 1e-9)) (is (near (dmatrix-ref (tree-patristic-distances tree) "A" "C") 4.0 1e-9)))
  (let ((d (make-dmatrix '("x" "y" "z") '((0 2 6) (2 0 6) (6 6 0))))) (is (near (dmatrix-ref d "x" "z") 6.0)) (is-equal 3 (dmatrix-size d)) (is-equal 3 (length (tree-leaves (upgma d)))) (is-equal 3 (length (tree-leaves (neighbor-joining d)))))
  (is (near (sequence-distance "ACGTACGTAC" "ACGTACGTAC" :model :p) 0.0)) (is (near (sequence-distance "AAAAAAAAAA" "AAAAAAAAAC" :model :p) 0.1))
  (is (near (sequence-distance "AAAAAAAAAA" "AAAAAAAAAC" :model :jc69) (* -0.75 (log (- 1.0 (* (/ 4.0 3.0) 0.1)))) 1e-12)) (is (> (sequence-distance "AAAAAAAAAA" "AAAAAAAAAC" :model :k2p) 0.1)) (is (float-infinity-p (sequence-distance "AC" "CA" :model :jc69)))
  (is (near (sequence-distance "AC-T" "ACGT" :model :p) 0.0))
  ;; simulate evolution along ((A,B),(C,D)) and recover the topology from sequences
  (let* ((rng (make-rng 41)) (root (random-dna 800 :rng rng)) (left (mutate-dna root 0.10 :rng rng)) (right (mutate-dna root 0.10 :rng rng))
         (a (mutate-dna left 0.03 :rng rng)) (b (mutate-dna left 0.03 :rng rng)) (c (mutate-dna right 0.03 :rng rng)) (dd (mutate-dna right 0.03 :rng rng))
         (seqs (list (make-bioseq a :id "A") (make-bioseq b :id "B") (make-bioseq c :id "C") (make-bioseq dd :id "D"))) (truth (parse-newick "((A,B),(C,D));")))
    (is-equal 0 (robinson-foulds truth (tree-from-sequences seqs :method :nj))) (is-equal 0 (robinson-foulds truth (tree-from-sequences seqs :method :upgma)))
    (is (< (dmatrix-ref (distance-matrix seqs) "A" "B") (dmatrix-ref (distance-matrix seqs) "A" "C")))))

(deftest intervals
  (let* ((rng (make-rng 51)) (items (loop for i from 0 below 300 collect (let ((s (rand-int 5000 rng))) (list s (+ s 1 (rand-int 200 rng)) i)))) (tree (make-interval-tree items)))
    (is-equal 300 (interval-tree-size tree))
    (dotimes (trial 40)
      (let* ((qs (rand-int 5000 rng)) (qe (+ qs 1 (rand-int 300 rng)))
             (naive (sort (mapcar #'third (remove-if-not (lambda (it) (and (< (first it) qe) (> (second it) qs))) items)) #'<))
             (got (sort (mapcar #'third (interval-overlaps tree qs qe)) #'<)))
        (is-equal naive got))))
  (is-equal nil (interval-overlaps (make-interval-tree nil) 0 10)) (is-equal '((5 10 x)) (interval-at (make-interval-tree '((5 10 x) (20 30 y))) 7)) (is-equal nil (interval-at (make-interval-tree '((5 10 x))) 10))
  (is-equal '((1 . 6) (8 . 10)) (merge-intervals '((8 . 10) (1 . 3) (2 . 6)))) (is-equal '((0 . 5)) (merge-intervals '((0 . 2) (2 . 5)))) (is-equal '((0 . 1) (3 . 8) (10 . 12)) (interval-complement '((1 . 3) (8 . 10)) 12))
  (is-equal '(0 1 2 2 0 0) (coerce (coverage-depth '((1 . 4) (2 . 4)) 6) 'list)) (is-equal '((0 1 0) (1 2 1) (2 4 2) (4 6 0)) (coverage-segments (coverage-depth '((1 . 4) (2 . 4)) 6)))
  (is-equal '((3 . 5)) (intersect-intervals '((0 . 5)) '((3 . 9)))) (is-equal nil (intersect-intervals '((0 . 2)) '((2 . 4))))
  (let* ((fs (read-gff (fixture "tiny.gff3"))) (index (build-feature-index fs)))
    (is-equal 2 (length (features-overlapping index "chr1" 150 160))) (is-equal 1 (length (features-overlapping index "chr1" 400 410))) (is-equal nil (features-overlapping index "chrX" 0 100))
    (multiple-value-bind (f d) (nearest-feature fs "chr1" 250) (is-equal "g2" (feature-qualifier f "ID")) (is-equal 50 d))
    (multiple-value-bind (f d) (nearest-feature fs "chr1" 150) (declare (ignore f)) (is-equal 0 d))))

;;; ---- external tools -----------------------------------------------------------------------------------------------------------

(deftest external-tools
  (is (find-executable "sh")) (is-equal nil (find-executable "no-such-program-xyz")) (is-equal nil (find-executable "a;b")) (is (tool-available-p "cat"))
  (let ((r (run-tool "cat" nil :input "hello"))) (is (tool-ok-p r)) (is-equal "hello" (tool-result-output r)) (is-equal '("hello") (tool-lines r)))
  (signals-error (run-tool "no-such-program-xyz" nil)) (is-equal 127 (tool-result-status (run-tool "no-such-program-xyz" nil :check nil)))
  (signals-error (run-tool "echo; rm -rf /" nil)) (signals-error (run-tool "sh" '(1 2))) (signals-error (run-tool "sh" '("-c" "exit 3")))
  (is-equal 3 (tool-result-status (run-tool "sh" '("-c" "exit 3") :check nil)))
  ;; arguments are not interpreted by a shell
  (is-equal "a;b $HOME `x`" (string-trim (list #\Newline) (tool-result-output (run-tool "echo" '("a;b $HOME `x`")))))
  ;; a stand-in for BLAST, called through an absolute path
  (let ((path "/tmp/ng-fake-blast.sh"))
    (write-file-lines path (list "#!/bin/sh" "printf 'q1\\ts1\\t99.5\\t100\\t1\\t0\\t1\\t100\\t11\\t110\\t1e-30\\t200\\n'" "printf 'q1\\ts2\\t80.0\\t90\\t10\\t1\\t5\\t95\\t1\\t90\\t1e-10\\t100\\n'" "printf 'q2\\ts3\\t70.0\\t50\\t15\\t0\\t1\\t50\\t1\\t50\\t0.01\\t30\\n'"))
    (run-tool "chmod" (list "+x" path))
    (let ((hits (parse-blast-tabular (tool-lines (run-tool path nil)))))
      (is-equal 3 (length hits)) (is-equal "s1" (blast-hit-subject (first hits))) (is (near (blast-hit-identity (first hits)) 99.5)) (is (< (blast-hit-evalue (first hits)) 1e-29)) (is-equal 11 (blast-hit-sstart (first hits)))
      (is-equal '("s1" "s3") (mapcar #'blast-hit-subject (blast-best-hits hits))) (is (stringp (princ-to-string (first hits)))))
    (delete-file path))
  (is-equal nil (parse-blast-tabular '("# comment" "short\tline"))) (signals-error (run-blast "rm" "q" "db"))
  (let ((recs (parse-paf '("r1	1000	10	990	+	chr1	50000	100	1080	950	990	60"))))
    (is-equal 1 (length recs)) (is (near (paf-identity (first recs)) (/ 950.0 990))) (is-equal 1 (paf-record-strand (first recs))) (is-equal "chr1" (paf-record-tname (first recs))))
  (is-equal '(("chr1" 100 5) ("chr1" 101 7)) (parse-samtools-depth (list (format nil "chr1~C100~C5" #\Tab #\Tab) (format nil "chr1~C101~C7" #\Tab #\Tab)))))

;;; ---- presentations -------------------------------------------------------------------------------------------------------------

(deftest biology-presentations
  (is (default-pcommand 'bioseq)) (is-equal 'view-sequence (pcmd-name (default-pcommand 'bioseq))) (is-equal 'view-alignment (pcmd-name (default-pcommand 'alignment)))
  (is-equal 'view-tree (pcmd-name (default-pcommand 'phylo-node))) (is-equal 'describe-residue (pcmd-name (default-pcommand 'residue))) (is-equal 'zoom-clade (pcmd-name (default-pcommand 'clade)))
  (is (find 'translate-sequence (applicable-pcommands 'bioseq) :key #'pcmd-name)) (is (find 'describe (applicable-pcommands 'bioseq) :key #'pcmd-name))
  (is (find 'clade-newick (applicable-pcommands 'clade) :key #'pcmd-name))
  (let ((text (residue-description (make-bioseq "ACGTACGTAC" :id "s") 4)))
    (is (%string-search "Position 5 of 10: A" text 0)) (is (%string-search "codon starting here: ACG = T" text 0)) (is (%string-search "complement T" text 0)))
  (is (%string-search "hydropathy 4.5" (residue-description (make-bioseq "MKVLIAG" :id "p" :alphabet :protein) 4) 0))
  (is (%string-search "identical" (alignment-column-description (align "ACGT" "ACGT") 1) 0))
  (multiple-value-bind (wm lw) (make-test-wm)
    (declare (ignore lw))
    (let ((seq (make-bioseq "ATGAAAGTACTGGCGGCGTAAGGCATCGATTGGATCC" :id "demo" :description "demo gene")))
      (show-sequence seq :wm wm)
      (wm-draw wm)
      (is (screen-has wm "demo")) (is (screen-has wm "ATGAAAGTAC")) (is (screen-has wm "37 bases"))
      (let ((pos (screen-has wm "ATGAAAGTAC")))
        (click-at wm (cons (car pos) (+ (cdr pos) 2)) 0)
        (wm-draw wm)
        (is (screen-has wm "Position 3 of 37: G"))))
    (let ((al (align "HEAGAWGHEE" "PAWHEAE" :mode :local)))
      (show-alignment al :wm wm) (wm-draw wm) (is (screen-has wm "alignment, score")) (is (screen-has wm "AW")))
    (let ((tree (parse-newick "((A:0.1,B:0.2):0.3,(C:0.4,D:0.5):0.6);")))
      (show-tree tree :wm wm) (wm-draw wm) (is (screen-has wm "clade of 2")) (is (screen-has wm "A (0.1)"))
      (let ((pos (screen-has wm "clade of 2")))
        (click-at wm pos 1) (wm-draw wm)
        (is (screen-has wm "B (0.2)")) (is (null (screen-has wm "C (0.4)")))))
    (show-features (read-gff (fixture "tiny.gff3")) :wm wm) (wm-draw wm) (is (screen-has wm "4 features")) (is (screen-has wm "FEATURE gene")))
  (is (not (null (with-output-to-string (*standard-output*) (show-sequence "ACGT" :wm nil))))))

;;; ---- workflows and the exit criterion -----------------------------------------------------------------------------------

(defun synthetic-genome (ngenes seed)
  "A random genome with NGENES planted genes on both strands; returns (values genome genes) where each gene is (start end strand protein)."
  (let* ((rng (make-rng seed)) (codons (let ((acc nil)) (dotimes (i 64) (let ((c (map 'string (lambda (k) (char "TCAG" k)) (list (floor i 16) (mod (floor i 4) 4) (mod i 4))))) (unless (find (codon-amino c) "*") (push c acc)))) acc))
         (parts nil) (genes nil) (pos 0))
    (dotimes (k ngenes)
      (let* ((naa (+ 80 (rand-int 150 rng)))
             (gene (apply #'%string-concat "ATG" (append (loop repeat (- naa 1) collect (rand-choice codons rng)) (list (rand-choice '("TAA" "TAG" "TGA"))))))
             (filler (random-dna (+ 30 (rand-int 150 rng)) :rng rng)) (strand (if (evenp (rand-int 2 rng)) 1 -1)) (unit (%string-concat "TAA" gene)))
        (push filler parts) (incf pos (length filler))
        (if (> strand 0)
            (progn (push unit parts) (push (list (+ pos 3) (+ pos (length unit)) 1 (translate gene :to-stop t)) genes))
            (progn (push (reverse-complement unit) parts) (push (list pos (+ pos (length gene)) -1 (translate gene :to-stop t)) genes)))
        (incf pos (length unit))))
    (values (apply #'%string-concat (reverse parts)) (reverse genes))))

(deftest annotation-and-protein-search
  (multiple-value-bind (genome genes) (synthetic-genome 60 2025)
    (let* ((bs (make-bioseq genome :id "chrSynth")) (features (annotate-orfs bs :min-length 75)) (found 0))
      (dolist (g genes)
        (when (find-if (lambda (f) (and (= (feature-start f) (first g)) (= (feature-end f) (second g)) (= (bio-feature-strand f) (third g)) (string= (feature-qualifier f "translation") (fourth g)))) features) (incf found)))
      (is-equal 60 found)
      (is (>= (length features) 60)) (is (< (length features) 200))
      (is-equal "chrSynth" (bio-feature-seqid (first features))) (is (feature-qualifier (first features) "locus_tag"))
      (let ((f (first features))) (is-equal (feature-qualifier f "translation") (translate (feature-sequence f bs) :to-stop t)))
      (let ((r (genome-report bs :min-length 75)))
        (is-equal (length genome) (getf r :length)) (is (near (getf r :gc) 0.5 0.1)) (is (> (getf r :forward) 10)) (is (> (getf r :reverse) 10)) (is (> (getf r :coding-fraction) 0.5)))
      ;; align a protein against the proteome: a 15% diverged copy of gene 7 must find gene 7
      (let* ((proteins (cds-proteins features)) (target (fourth (nth 7 genes))) (rng (make-rng 6))
             (query (let ((s (copy-seq target))) (dotimes (i (length s) s) (when (< (rand-uniform rng) 0.15) (setf (char s i) (rand-choice "ARNDCQEGHILKMFPSTWYV" rng))))))
             (ranked (rank-by-alignment query (subseq proteins 0 (min 40 (length proteins))))))
        (is (string= (bioseq-residues (first (first ranked))) target))
        (is (> (alignment-identity (second (first ranked))) 0.75)) (is (> (third (first ranked)) 100.0)) (is (> (alignment-score (second (first ranked))) (* 2 (alignment-score (second (second ranked))))))))))

(deftest read-qc-and-primers
  (let* ((reads (read-fastq (fixture "tiny.fq"))) (qc (fastq-qc reads)))
    (is-equal 2 (getf qc :reads)) (is (near (getf qc :mean-length) 10.0)) (is (near (getf qc :n-fraction) 0.05)) (is (near (getf qc :gc) 0.5 0.1)) (is (near (getf qc :duplicate-fraction) 0.0)) (is-equal 10 (length (getf qc :per-position-quality)))
    (is (near (getf qc :q30-fraction) 0.7 0.01)))
  (let* ((rng (make-rng 91)) (template (random-dna 600 :rng rng)) (pairs (design-primers template 250 350)))
    (is pairs) (is (<= (length pairs) 3))
    (dolist (p pairs)
      (is (<= 18 (length (primer-pair-forward p)) 25)) (is (<= 0.4 (gc-content (primer-pair-forward p)) 0.6)) (is (<= 0.4 (gc-content (primer-pair-reverse p)) 0.6))
      (is (<= (primer-pair-forward-start p) 250)) (is (>= (primer-pair-reverse-end p) 350)) (is (<= (primer-pair-product-size p) 600))
      (is-equal (primer-pair-forward p) (subseq template (primer-pair-forward-start p) (+ (primer-pair-forward-start p) (length (primer-pair-forward p)))))
      (is-equal (reverse-complement (primer-pair-reverse p)) (subseq template (- (primer-pair-reverse-end p) (length (primer-pair-reverse p))) (primer-pair-reverse-end p)))
      (is (< (primer-pair-tm-difference p) 6.0)))
    (is (apply #'<= (mapcar #'primer-pair-score pairs))))
  (is-equal nil (design-primers "ACGTACGTAC" 3 6)))

(deftest enrichment-analysis
  (let* ((background (loop for i from 1 to 100 collect (format nil "g~D" i)))
         (kinase (loop for i from 1 to 10 collect (format nil "g~D" i))) (other (loop for i from 11 to 60 collect (format nil "g~D" i)))
         (selected (append (loop for i from 1 to 8 collect (format nil "g~D" i)) '("g70" "g71")))
         (rows (enrichment selected background (list (cons "kinase" kinase) (cons "other" other)))))
    (is-equal "kinase" (first (first rows))) (is-equal 8 (second (first rows))) (is-equal 10 (third (first rows)))
    (is (< (fifth (first rows)) 1e-6)) (is (>= (sixth (first rows)) (fifth (first rows)))) (is (> (fifth (second rows)) 0.5))
    (is (near (fifth (first rows)) (htest-p-value (fisher-exact 8 2 2 88 :alternative :greater)) 1e-12))))

(deftest exit-criterion-genome
  ;; load a genome, annotate its ORFs, align a protein against a set, and display a tree -- all in one session
  (multiple-value-bind (genome genes) (synthetic-genome 40 777)
    (let* ((path "/tmp/ng-genome.fa") (gz "/tmp/ng-genome.fa.gz"))
      (write-fasta (list (make-bioseq genome :id "chrExit" :description "synthetic")) path)
      (run-tool "gzip" (list "-kf" path))
      (let* ((loaded (first (read-fasta gz))) (features (annotate-orfs loaded :min-length 75)) (proteins (cds-proteins features)))
        (is-equal (length genome) (seq-length loaded)) (is (>= (length features) 40))
        (is (>= (count-if (lambda (g) (find-if (lambda (f) (and (= (feature-start f) (first g)) (= (feature-end f) (second g)))) features)) genes) 40))
        (let* ((query (fourth (first genes))) (best (first (rank-by-alignment query proteins)))) (is-equal query (bioseq-residues (first best))))
        (let* ((variants (loop for k from 0 below 5 collect (make-bioseq (mutate-dna (subseq genome 1000 1400) (* 0.02 (+ k 1)) :rng (make-rng (+ 500 k))) :id (format nil "v~D" k))))
               (tree (tree-from-sequences variants :method :nj)))
          (is-equal 5 (length (tree-leaves tree))) (is (> (length (tree-ascii tree)) 20)) (is-equal 5 (length (tree-leaf-names (parse-newick (tree-newick tree)))))))
      (delete-file path) (delete-file gz))))
