;;;; 10-formats.lisp -- bioinformatics file formats: FASTA, FASTQ, GenBank, GFF3/GTF, BED, VCF, SAM and BAM.
;;;;
;;;; Parsers take lines (PARSE-*) or a path (READ-*).  Paths ending in .gz or .bgz are decompressed with the
;;;; system gzip (no shell: the program is run with an argument list).  Genomic coordinates in the objects
;;;; returned are zero-based and half-open [start, end), whatever the file format uses.

(in-package :cl)

;;; ---- reading lines, with gzip support ----------------------------------------------------------------------------

(defun gzip-path-p (path)
  "True when PATH names a gzip-compressed file (by suffix)."
  (let ((n (length path))) (or (and (> n 3) (string= (subseq path (- n 3)) ".gz")) (and (> n 4) (string= (subseq path (- n 4)) ".bgz")))))

(defun read-gzip-string (path)
  "The decompressed bytes of the gzip (or BGZF) file PATH as a string."
  (multiple-value-bind (out status) (run-program "gzip" (list "-dc" "--" path))
    (unless (eql status 0) (error "gzip could not decompress ~A" path))
    out))

(defun read-lines (path)
  "The lines of the text file PATH (decompressed first when it is gzip-compressed)."
  (if (gzip-path-p path) (split-lines (read-gzip-string path)) (read-file-lines path)))

(defun map-lines (function path)
  "Call FUNCTION on each line of PATH.  Plain files are streamed; compressed files are decompressed first."
  (if (gzip-path-p path)
      (dolist (l (split-lines (read-gzip-string path))) (funcall function l))
      (with-open-file (s path)
        (loop (let ((l (read-line s nil nil))) (if l (funcall function (string-right-trim (string #\Return) l)) (return)))))))

(defun %split-on (string ch)
  (let ((parts nil) (start 0))
    (loop (let ((p (position ch string :start start)))
            (if p (progn (push (subseq string start p) parts) (setq start (+ p 1)))
                (progn (push (subseq string start) parts) (return)))))
    (nreverse parts)))

(defun %trim (s) (string-trim (list #\Space #\Tab #\Return) s))
(defun %starts-with-p (s prefix) (and (>= (length s) (length prefix)) (string= (subseq s 0 (length prefix)) prefix)))

(defun %id-and-description (header)
  (let ((p (position-if (lambda (c) (or (char= c #\Space) (char= c #\Tab))) header)))
    (if p (values (subseq header 0 p) (%trim (subseq header p))) (values header nil))))

;;; ---- FASTA -------------------------------------------------------------------------------------------------------------

(defun parse-fasta (lines &key (alphabet nil))
  "A list of BIOSEQ objects from FASTA LINES."
  (let ((seqs nil) (header nil) (parts nil))
    (flet ((finish () (when header
                        (multiple-value-bind (id desc) (%id-and-description header)
                          (push (make-bioseq (apply #'%string-concat (reverse parts)) :id id :description desc :alphabet alphabet) seqs)))))
      (dolist (l lines)
        (let ((line (%trim l)))
          (cond ((zerop (length line)) nil)
                ((char= (char line 0) #\>) (finish) (setq header (subseq line 1) parts nil))
                ((char= (char line 0) #\;) nil)
                (t (push line parts)))))
      (finish))
    (nreverse seqs)))

(defun read-fasta (path &key alphabet) "All sequences of the FASTA file PATH." (parse-fasta (read-lines path) :alphabet alphabet))

(defun map-fasta (function path)
  "Call FUNCTION on each BIOSEQ of the FASTA file PATH, reading one record at a time."
  (let ((header nil) (parts nil))
    (flet ((finish () (when header
                        (multiple-value-bind (id desc) (%id-and-description header)
                          (funcall function (make-bioseq (apply #'%string-concat (reverse parts)) :id id :description desc))))))
      (map-lines (lambda (l)
                   (let ((line (%trim l)))
                     (cond ((zerop (length line)) nil)
                           ((char= (char line 0) #\>) (finish) (setq header (subseq line 1) parts nil))
                           ((char= (char line 0) #\;) nil)
                           (t (push line parts)))))
                 path)
      (finish))))

(defun fasta-string (seqs &key (width 60))
  "FASTA text for a list of sequences (BIOSEQ objects), wrapped at WIDTH columns."
  (with-output-to-string (o)
    (dolist (s (if (listp seqs) seqs (list seqs)))
      (format o ">~A~@[ ~A~]~%" (or (bioseq-id s) "seq") (bioseq-description s))
      (let ((r (bioseq-residues s)))
        (loop for i from 0 below (length r) by width do (format o "~A~%" (subseq r i (min (length r) (+ i width)))))))))

(defun write-fasta (seqs path &key (width 60))
  "Write SEQS to PATH in FASTA format."
  (with-open-file (o path :direction :output :if-exists :supersede) (write-string (fasta-string seqs :width width) o))
  path)

;;; ---- FASTQ -------------------------------------------------------------------------------------------------------------

(defun phred-scores (quality-string &optional (offset 33))
  "A vector of Phred quality scores decoded from QUALITY-STRING."
  (map 'vector (lambda (c) (- (char-code c) offset)) quality-string))

(defun parse-fastq (lines)
  "A list of BIOSEQ objects with Phred qualities (a vector of integers in BIOSEQ-QUALITY) from FASTQ LINES."
  (let ((out nil) (ls (remove-if (lambda (l) (zerop (length (%trim l)))) lines)))
    (loop while ls
          do (let ((h (first ls)) (r (second ls)) (q (fourth ls)))
               (unless (and h r q (char= (char h 0) #\@)) (error "Malformed FASTQ record near ~S" h))
               (unless (= (length (%trim r)) (length (%trim q))) (error "FASTQ sequence and quality lengths differ for ~A" h))
               (multiple-value-bind (id desc) (%id-and-description (subseq h 1))
                 (push (make-bioseq (%trim r) :id id :description desc :quality (phred-scores (%trim q))) out))
               (setq ls (nthcdr 4 ls))))
    (nreverse out)))

(defun read-fastq (path) "All reads of the FASTQ file PATH." (parse-fastq (read-lines path)))

(defun fastq-string (seqs)
  "FASTQ text for reads with qualities."
  (with-output-to-string (o)
    (dolist (s seqs)
      (format o "@~A~@[ ~A~]~%~A~%+~%~A~%" (bioseq-id s) (bioseq-description s) (bioseq-residues s)
              (map 'string (lambda (q) (code-char (+ q 33))) (bioseq-quality s))))))

(defun mean-quality (s) "Mean Phred quality of the read S." (mean (coerce (bioseq-quality s) 'list)))

(defun quality-trim (s threshold)
  "Trim the 3' end of the read S while the base quality is below THRESHOLD."
  (let ((q (bioseq-quality s)) (n (seq-length s)))
    (loop while (and (> n 0) (< (aref q (- n 1)) threshold)) do (decf n))
    (seq-subseq s 0 n)))

(defun error-probability (phred) "The probability that a base with Phred score PHRED is wrong." (expt 10.0 (/ (- phred) 10.0)))

;;; ---- features: shared by GenBank, GFF, BED --------------------------------------------------------------------------------

(defstruct (bio-feature (:copier nil) (:constructor %make-bio-feature (seqid type intervals strand qualifiers)))
  seqid type intervals strand qualifiers)

(defun feature-start (f) "Start (zero-based) of feature F." (reduce #'min (mapcar #'car (bio-feature-intervals f))))
(defun feature-end (f) "End (exclusive) of feature F." (reduce #'max (mapcar #'cdr (bio-feature-intervals f))))
(defun feature-length (f) "Total length of the intervals of F." (reduce #'+ (mapcar (lambda (i) (- (cdr i) (car i))) (bio-feature-intervals f))))
(defun feature-qualifier (f key) "The value of qualifier KEY (a string) of F, or NIL." (cdr (assoc key (bio-feature-qualifiers f) :test #'string=)))
(defun feature-name (f)
  "A display name for F from its gene, locus_tag, Name or ID qualifier."
  (or (feature-qualifier f "gene") (feature-qualifier f "locus_tag") (feature-qualifier f "Name") (feature-qualifier f "ID") (feature-qualifier f "gene_id")))

(defmethod print-object ((f bio-feature) stream)
  (format stream "#<FEATURE ~A ~A:~D-~D~A~@[ ~A~]>" (bio-feature-type f) (bio-feature-seqid f) (feature-start f) (feature-end f)
          (case (bio-feature-strand f) (1 "(+)") (-1 "(-)") (t "")) (feature-name f)))

(defun feature-sequence (f seq)
  "The sequence of feature F extracted from SEQ (a string or BIOSEQ): its intervals joined, reverse-complemented for the minus strand."
  (let* ((r (%residues seq)) (joined (apply #'%string-concat (mapcar (lambda (i) (subseq r (car i) (cdr i))) (bio-feature-intervals f)))))
    (if (eql (bio-feature-strand f) -1) (reverse-complement joined) joined)))

;;; ---- GenBank ---------------------------------------------------------------------------------------------------------------

(defun parse-genbank-location (text)
  "Parse a GenBank location such as complement(join(1..10,20..30)).  Returns (values intervals strand)."
  (let ((strand 1) (s (remove #\Space text)))
    (loop while (%starts-with-p s "complement(") do (setq strand (- strand) s (subseq s 11 (- (length s) 1))))
    (when (%starts-with-p s "join(") (setq s (subseq s 5 (- (length s) 1))))
    (when (%starts-with-p s "order(") (setq s (subseq s 6 (- (length s) 1))))
    (let ((intervals nil))
      (dolist (part (%split-on s #\,))
        (let* ((p (if (%starts-with-p part "complement(") (progn (setq strand -1) (subseq part 11 (- (length part) 1))) part))
               (p (remove-if (lambda (c) (or (char= c #\<) (char= c #\>))) p)) (dots (%string-search ".." p 0)))
          (if dots
              (push (cons (- (parse-integer (subseq p 0 dots)) 1) (parse-integer (subseq p (+ dots 2)))) intervals)
              (let ((v (parse-integer p))) (push (cons (- v 1) v) intervals)))))
      (values (nreverse intervals) strand))))

(defstruct (gb-record (:copier nil) (:constructor %make-gb-record (locus definition accession organism features sequence)))
  locus definition accession organism features sequence)

(defmethod print-object ((r gb-record) stream)
  (format stream "#<GENBANK ~A ~D features ~D bp>" (gb-record-locus r) (length (gb-record-features r)) (seq-length (gb-record-sequence r))))

(defun parse-genbank (lines)
  "A list of GB-RECORD objects (locus, definition, accession, organism, features, sequence) from GenBank LINES."
  (let ((records nil) (locus nil) (definition nil) (accession nil) (organism nil) (features nil) (seqparts nil)
        (section nil) (cur-type nil) (cur-loc nil) (cur-quals nil) (cur-qual nil))
    (labels ((flush-qual () (when cur-qual (push (cons (car cur-qual) (string-trim "\"" (cdr cur-qual))) cur-quals) (setq cur-qual nil)))
             (flush-feature ()
               (flush-qual)
               (when cur-type
                 (multiple-value-bind (intervals strand) (parse-genbank-location cur-loc)
                   (push (%make-bio-feature locus cur-type intervals strand (reverse cur-quals)) features)))
               (setq cur-type nil cur-loc nil cur-quals nil))
             (finish ()
               (flush-feature)
               (when locus
                 (push (%make-gb-record locus definition accession organism (reverse features)
                                        (make-bioseq (apply #'%string-concat (reverse seqparts)) :id (or accession locus) :description definition))
                       records))
               (setq locus nil definition nil accession nil organism nil features nil seqparts nil section nil)))
      (dolist (raw lines)
        (let ((l (string-right-trim (string #\Return) raw)))
          (cond ((%starts-with-p l "LOCUS") (finish) (setq locus (second (remove "" (%split-on l #\Space) :test #'string=))))
                ((%starts-with-p l "DEFINITION") (setq section :definition definition (%trim (subseq l 10))))
                ((%starts-with-p l "ACCESSION") (setq section :accession accession (first (remove "" (%split-on (%trim (subseq l 9)) #\Space) :test #'string=))))
                ((%starts-with-p l "  ORGANISM") (setq organism (%trim (subseq l 10))))
                ((%starts-with-p l "FEATURES") (setq section :features))
                ((%starts-with-p l "ORIGIN") (flush-feature) (setq section :origin))
                ((%starts-with-p l "//") (finish))
                ((and (eq section :definition) (%starts-with-p l "            ")) (setq definition (%string-concat definition " " (%trim l))))
                ((eq section :origin)
                 (push (remove-if-not #'alpha-char-p l) seqparts))
                ((eq section :features)
                 (cond ((and (> (length l) 21) (char/= (char l 5) #\Space))
                        (flush-feature)
                        (setq cur-type (%trim (subseq l 5 21)) cur-loc (%trim (subseq l 21))))
                       ((and (> (length l) 21) (char= (char l 21) #\/))
                        (flush-qual)
                        (let* ((q (subseq l 22)) (eq-pos (position #\= q)))
                          (setq cur-qual (if eq-pos (cons (subseq q 0 eq-pos) (subseq q (+ eq-pos 1))) (cons q "true")))))
                       ((and (> (length l) 21) cur-qual) (setq cur-qual (cons (car cur-qual) (%string-concat (cdr cur-qual) " " (%trim l)))))
                       ((and (> (length l) 21) cur-type (null cur-qual)) (setq cur-loc (%string-concat cur-loc (%trim l))))))))
        )
      (finish))
    (nreverse records)))

(defun read-genbank (path) "All records of the GenBank file PATH." (parse-genbank (read-lines path)))

;;; ---- GFF3 / GTF / BED -------------------------------------------------------------------------------------------------

(defun %parse-attributes (text gtf)
  (let ((acc nil))
    (dolist (item (%split-on text #\;))
      (let ((it (%trim item)))
        (when (> (length it) 0)
          (if gtf
              (let ((sp (position #\Space it)))
                (when sp (push (cons (subseq it 0 sp) (string-trim "\"" (%trim (subseq it sp)))) acc)))
              (let ((eq-pos (position #\= it)))
                (when eq-pos (push (cons (subseq it 0 eq-pos) (subseq it (+ eq-pos 1))) acc)))))))
    (nreverse acc)))

(defun %strand-number (s) (cond ((string= s "+") 1) ((string= s "-") -1) (t 0)))

(defun parse-gff (lines &key gtf)
  "A list of BIO-FEATURE objects from GFF3 (or, with GTF true, GTF) LINES.  Source, score and phase are stored as qualifiers
named \"source\", \"score\" and \"phase\"."
  (let ((out nil))
    (dolist (l lines)
      (let ((line (string-right-trim (string #\Return) l)))
        (unless (or (zerop (length line)) (char= (char line 0) #\#))
          (let ((f (%split-on line #\Tab)))
            (when (>= (length f) 8)
              (push (%make-bio-feature (nth 0 f) (nth 2 f) (list (cons (- (parse-integer (nth 3 f)) 1) (parse-integer (nth 4 f)))) (%strand-number (nth 6 f))
                                       (append (list (cons "source" (nth 1 f)) (cons "score" (nth 5 f)) (cons "phase" (nth 7 f)))
                                               (if (> (length f) 8) (%parse-attributes (nth 8 f) gtf) nil)))
                    out))))))
    (nreverse out)))

(defun read-gff (path &key gtf) "All features of the GFF3 (or GTF) file PATH." (parse-gff (read-lines path) :gtf gtf))

(defun parse-bed (lines)
  "A list of BIO-FEATURE objects (type \"bed\") from BED LINES; BED coordinates are already zero-based half-open."
  (let ((out nil))
    (dolist (l lines)
      (let ((line (string-right-trim (string #\Return) l)))
        (unless (or (zerop (length line)) (%starts-with-p line "#") (%starts-with-p line "track") (%starts-with-p line "browser"))
          (let ((f (remove "" (%split-on line #\Tab) :test #'string=)))
            (when (>= (length f) 3)
              (push (%make-bio-feature (nth 0 f) "bed" (list (cons (parse-integer (nth 1 f)) (parse-integer (nth 2 f))))
                                       (if (> (length f) 5) (%strand-number (nth 5 f)) 0)
                                       (append (if (> (length f) 3) (list (cons "Name" (nth 3 f))) nil) (if (> (length f) 4) (list (cons "score" (nth 4 f))) nil)))
                    out))))))
    (nreverse out)))

(defun read-bed (path) "All features of the BED file PATH." (parse-bed (read-lines path)))

;;; ---- VCF --------------------------------------------------------------------------------------------------------------------

(defstruct (vcf-record (:copier nil) (:constructor %make-vcf-record (chrom pos id ref alt qual filter info format samples)))
  chrom pos id ref alt qual filter info format samples)

(defmethod print-object ((v vcf-record) stream)
  (format stream "#<VCF ~A:~D ~A>~A" (vcf-record-chrom v) (vcf-record-pos v) (vcf-record-ref v) (format nil "~{~A~^,~}" (mapcar (lambda (a) (%string-concat ">" a)) (vcf-record-alt v)))))

(defun vcf-snp-p (v) "True when V is a single-nucleotide variant." (and (= (length (vcf-record-ref v)) 1) (every (lambda (a) (= (length a) 1)) (vcf-record-alt v))))
(defun vcf-info (v key) "The INFO field KEY of V (T for flags), or NIL." (let ((c (assoc key (vcf-record-info v) :test #'string=))) (and c (cdr c))))
(defun vcf-genotype (v sample-index)
  "The genotype of sample SAMPLE-INDEX as a list of allele indices (NIL for missing)."
  (let* ((s (nth sample-index (vcf-record-samples v))) (gt (cdr (assoc "GT" s :test #'string=))))
    (and gt (mapcar (lambda (a) (if (string= a ".") nil (parse-integer a))) (%split-on (substitute #\/ #\| gt) #\/)))))

(defun parse-vcf (lines)
  "Parse VCF LINES; returns (values records sample-names).  POS is one-based as in the file; ALT is a list; INFO is an alist."
  (let ((records nil) (samples nil))
    (dolist (l lines)
      (let ((line (string-right-trim (string #\Return) l)))
        (cond ((zerop (length line)) nil)
              ((%starts-with-p line "##") nil)
              ((%starts-with-p line "#CHROM") (setq samples (nthcdr 9 (%split-on line #\Tab))))
              (t (let* ((f (%split-on line #\Tab)) (fmt (and (> (length f) 8) (%split-on (nth 8 f) #\:))))
                   (push (%make-vcf-record (nth 0 f) (parse-integer (nth 1 f)) (nth 2 f) (nth 3 f) (%split-on (nth 4 f) #\,)
                                           (if (string= (nth 5 f) ".") nil (read-from-string (nth 5 f))) (nth 6 f)
                                           (mapcar (lambda (kv) (let ((e (position #\= kv))) (if e (cons (subseq kv 0 e) (subseq kv (+ e 1))) (cons kv t))))
                                                   (if (string= (nth 7 f) ".") nil (%split-on (nth 7 f) #\;)))
                                           fmt
                                           (mapcar (lambda (s) (mapcar #'cons fmt (%split-on s #\:))) (nthcdr 9 f)))
                         records))))))
    (values (nreverse records) samples)))

(defun read-vcf (path) "Records and sample names of the VCF file PATH." (parse-vcf (read-lines path)))

;;; ---- SAM / BAM ---------------------------------------------------------------------------------------------------------------

(defstruct (sam-record (:copier nil) (:constructor %make-sam-record (qname flag rname pos mapq cigar rnext pnext tlen seq qual tags)))
  qname flag rname pos mapq cigar rnext pnext tlen seq qual tags)

(defmethod print-object ((r sam-record) stream)
  (format stream "#<SAM ~A ~A:~D ~A>" (sam-record-qname r) (sam-record-rname r) (sam-record-pos r) (cigar-string (sam-record-cigar r))))

(defun parse-cigar (text)
  "A list of (length . operation-character) from a CIGAR string; \"*\" gives NIL."
  (if (string= text "*") nil
      (let ((acc nil) (n 0))
        (loop for c across text do (if (digit-char-p c) (setq n (+ (* n 10) (digit-char-p c))) (progn (push (cons n c) acc) (setq n 0))))
        (nreverse acc))))

(defun cigar-string (cigar) "The CIGAR list rendered as text." (if cigar (apply #'%string-concat (mapcar (lambda (e) (format nil "~D~A" (car e) (cdr e))) cigar)) "*"))
(defun cigar-reference-length (cigar) "Number of reference bases covered by CIGAR (M, D, N, =, X)." (loop for e in cigar when (find (cdr e) "MDN=X") sum (car e)))
(defun cigar-query-length (cigar) "Number of query bases in CIGAR (M, I, S, =, X)." (loop for e in cigar when (find (cdr e) "MIS=X") sum (car e)))
(defun sam-end (r) "End (zero-based, exclusive) of the alignment of R on the reference." (+ (- (sam-record-pos r) 1) (cigar-reference-length (sam-record-cigar r))))
(defun sam-flag-p (r bit) "True when flag BIT (1, 2, 4, 16, 256, ...) is set in R." (not (zerop (logand (sam-record-flag r) bit))))
(defun sam-mapped-p (r) "True when R is mapped." (not (sam-flag-p r 4)))
(defun sam-reverse-p (r) "True when R aligns to the reverse strand." (sam-flag-p r 16))
(defun sam-tag (r name) "The value of optional field NAME (a two-letter string) of R." (cdr (assoc name (sam-record-tags r) :test #'string=)))

(defun %parse-sam-tag (s)
  (let ((type (char s 3)) (v (subseq s 5)))
    (cons (subseq s 0 2) (case type (#\i (parse-integer v)) (#\f (float (read-from-string v))) (t v)))))

(defun parse-sam (lines)
  "Parse SAM LINES; returns (values records header-lines)."
  (let ((records nil) (header nil))
    (dolist (l lines)
      (let ((line (string-right-trim (string #\Return) l)))
        (cond ((zerop (length line)) nil)
              ((char= (char line 0) #\@) (push line header))
              (t (let ((f (%split-on line #\Tab)))
                   (push (%make-sam-record (nth 0 f) (parse-integer (nth 1 f)) (nth 2 f) (parse-integer (nth 3 f)) (parse-integer (nth 4 f)) (parse-cigar (nth 5 f))
                                           (nth 6 f) (parse-integer (nth 7 f)) (parse-integer (nth 8 f)) (nth 9 f) (nth 10 f) (mapcar #'%parse-sam-tag (nthcdr 11 f)))
                         records))))))
    (values (nreverse records) (nreverse header))))

(defun read-sam (path) "Records and header lines of the SAM file PATH." (parse-sam (read-lines path)))

(defun %u8 (s i) (char-code (char s i)))
(defun %u16 (s i) (+ (%u8 s i) (ash (%u8 s (+ i 1)) 8)))
(defun %u32 (s i) (+ (%u16 s i) (ash (%u16 s (+ i 2)) 16)))
(defun %i32 (s i) (let ((v (%u32 s i))) (if (>= v 2147483648) (- v 4294967296) v)))
(defun %i16 (s i) (let ((v (%u16 s i))) (if (>= v 32768) (- v 65536) v)))
(defun %i8 (s i) (let ((v (%u8 s i))) (if (>= v 128) (- v 256) v)))

(defun %bam-tags (s i end)
  (let ((tags nil))
    (loop while (< i end)
          do (let ((name (subseq s i (+ i 2))) (type (char s (+ i 2))) (p (+ i 3)))
               (multiple-value-bind (val next)
                   (case type
                     (#\A (values (string (char s p)) (+ p 1)))
                     (#\c (values (%i8 s p) (+ p 1))) (#\C (values (%u8 s p) (+ p 1)))
                     (#\s (values (%i16 s p) (+ p 2))) (#\S (values (%u16 s p) (+ p 2)))
                     (#\i (values (%i32 s p) (+ p 4))) (#\I (values (%u32 s p) (+ p 4)))
                     (#\f (values 0.0 (+ p 4)))
                     ((#\Z #\H) (let ((z (position (code-char 0) s :start p))) (values (subseq s p z) (+ z 1))))
                     (#\B (let* ((sub (char s p)) (cnt (%u32 s (+ p 1))) (w (case sub ((#\c #\C) 1) ((#\s #\S) 2) (t 4)))) (values nil (+ p 5 (* cnt w)))))
                     (t (values nil end)))
                 (push (cons name val) tags) (setq i next))))
    (nreverse tags)))

(defun parse-bam-bytes (data)
  "Parse the decompressed bytes of a BAM file; returns (values records header-text references) where references is a list of (name . length)."
  (unless (string= (subseq data 0 4) (format nil "BAM~C" (code-char 1))) (error "Not a BAM file"))
  (let* ((l-text (%u32 data 4)) (n-data (length data)))
    (when (> (+ 12 l-text) n-data) (error "BAM: header text exceeds the file"))
    (parse-bam-body data l-text n-data)))

(defun parse-bam-body (data l-text n-data)
  (let* ((text (subseq data 8 (+ 8 l-text))) (p (+ 8 l-text)) (n-ref (%u32 data p)) (refs nil) (records nil))
    (incf p 4)
    (when (> (* n-ref 9) (- n-data p)) (error "BAM: reference count exceeds the file"))
    (dotimes (k n-ref)
      (let* ((ln (%u32 data p)) (name (subseq data (+ p 4) (+ p 4 ln -1))) (len (%u32 data (+ p 4 ln))))
        (push (cons name len) refs) (setq p (+ p 8 ln))))
    (setq refs (nreverse refs))
    (loop while (< (+ p 4) (length data))
          do (let* ((bs (let ((v (%u32 data p))) (when (or (< v 32) (> (+ p 4 v) n-data)) (error "BAM: record exceeds the file")) v))
                    (q (+ p 4)) (ref-id (%i32 data q)) (pos (%i32 data (+ q 4))) (lname (%u8 data (+ q 8))) (mapq (%u8 data (+ q 9)))
                    (ncig (%u16 data (+ q 12))) (flag (%u16 data (+ q 14))) (lseq (%i32 data (+ q 16))) (nref (%i32 data (+ q 20))) (npos (%i32 data (+ q 24)))
                    (tlen (%i32 data (+ q 28))) (r (+ q 32)) (name (subseq data r (+ r lname -1))) (c (+ r lname))
                    (check (when (or (> (+ r lname) (+ p 4 bs)) (> (+ r lname (* 4 ncig)) (+ p 4 bs)) (< lseq 0) (> (+ r lname (* 4 ncig) (floor (+ lseq 1) 2) lseq) (+ p 4 bs)))
                             (error "BAM: record fields exceed the record")))
                    (cigar (loop for k from 0 below ncig collect (let ((v (%u32 data (+ c (* 4 k))))) (cons (ash v -4) (char "MIDNSHP=X" (logand v 15))))))
                    (soff (+ c (* 4 ncig))) (qq (+ soff (floor (+ lseq 1) 2))) (tg (+ qq lseq))
                    (seq (let ((o (make-string lseq #\N))) (dotimes (k lseq) (let* ((b (%u8 data (+ soff (floor k 2)))) (nib (if (evenp k) (ash b -4) (logand b 15)))) (setf (char o k) (char "=ACMGRSVTWYHKDBN" nib)))) o))
                    (qual (if (and (> lseq 0) (= (%u8 data qq) 255)) "*" (let ((o (make-string lseq #\!))) (dotimes (k lseq) (setf (char o k) (code-char (+ 33 (%u8 data (+ qq k)))))) o))))
               (push (%make-sam-record name flag (if (< ref-id 0) "*" (car (nth ref-id refs))) (+ pos 1) mapq cigar
                                       (cond ((< nref 0) "*") ((= nref ref-id) "=") (t (car (nth nref refs)))) (+ npos 1) tlen seq qual (%bam-tags data tg (+ p 4 bs)))
                     records)
               (setq p (+ p 4 bs))))
    (values (nreverse records) text refs)))

(defun read-bam (path)
  "Read a BAM file (decompressed with the system gzip); returns (values records header-text references)."
  (parse-bam-bytes (read-gzip-string path)))

(defun pileup-depth (records ref-name ref-length)
  "A vector of read depth per reference position (zero-based) from mapped SAM-RECORDs on REF-NAME."
  (let ((d (make-array ref-length :initial-element 0)))
    (dolist (r records d)
      (when (and (sam-mapped-p r) (string= (sam-record-rname r) ref-name))
        (let ((pos (- (sam-record-pos r) 1)))
          (dolist (e (sam-record-cigar r))
            (cond ((find (cdr e) "M=X") (loop for k from pos below (min ref-length (+ pos (car e))) do (incf (aref d k))) (incf pos (car e)))
                  ((find (cdr e) "DN") (incf pos (car e))))))))))
