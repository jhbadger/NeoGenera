;;;; 00-seq.lisp -- biological sequences: alphabets, packing, complement and translation, k-mers, ORFs.
;;;;
;;;; A BIOSEQ holds an id, a description, an alphabet (:dna :rna or :protein), upper-case residues and optional
;;;; Phred qualities.  Most functions accept either a BIOSEQ or a plain string.

(in-package :cl)

(defstruct (bioseq (:copier nil) (:constructor %make-bioseq (id description alphabet residues quality)))
  id description alphabet residues quality)

(defun %residues (x) (if (bioseq-p x) (bioseq-residues x) x))

(defun %detect-alphabet (s)
  (cond ((%string-all-in s "ACGTNRYSWKMBDHV-.") :dna)
        ((and (not (find #\T s)) (%string-all-in s "ACGUNRYSWKMBDHV-.")) :rna)
        (t :protein)))

(defun make-bioseq (residues &key id description alphabet quality)
  "A sequence object from the string RESIDUES (upper-cased); the alphabet is detected unless given."
  (let ((s (string-upcase residues)))
    (%make-bioseq id description (or alphabet (%detect-alphabet s)) s quality)))

(defmethod print-object ((s bioseq) stream)
  (let ((r (bioseq-residues s)))
    (format stream "#<~A~@[ ~A~] ~D ~A~A>" (string-upcase (symbol-name (bioseq-alphabet s))) (bioseq-id s) (length r)
            (if (eq (bioseq-alphabet s) :protein) "aa" "bp")
            (if (<= (length r) 24) (format nil " ~A" r) (format nil " ~A..." (subseq r 0 20))))))

(defun seq-length (s) "Number of residues in the sequence S." (length (%residues s)))

(defun seq-subseq (s start &optional end)
  "The residues START..END (zero-based, END exclusive) of S; a BIOSEQ gives a BIOSEQ."
  (if (bioseq-p s)
      (%make-bioseq (bioseq-id s) (bioseq-description s) (bioseq-alphabet s) (subseq (bioseq-residues s) start end)
                    (and (bioseq-quality s) (subseq (bioseq-quality s) start end)))
      (subseq s start end)))

(defun seq-concat (a b) "Concatenation of two sequences of the same kind."
  (make-bioseq (%string-concat (%residues a) (%residues b)) :id (and (bioseq-p a) (bioseq-id a)) :alphabet (and (bioseq-p a) (bioseq-alphabet a))))

;;; ---- complement, transcription -----------------------------------------------------------------------------------

(defun %translate-string (s from to) (%string-translate s from to))
(defun %reverse-string (s) (%string-reverse s))

(defvar *dna-complement-from* "ACGTRYSWKMBDHVNacgtn")
(defvar *dna-complement-to*   "TGCAYRSWMKVHDBNtgcan")

(defun complement-seq (s)
  "The complement of a DNA (or RNA) sequence, keeping IUPAC ambiguity codes consistent."
  (let ((r (%residues s)))
    (if (and (bioseq-p s) (eq (bioseq-alphabet s) :rna))
        (%translate-string r "ACGURYSWKMBDHVN" "UGCAYRSWMKVHDBN")
        (%translate-string r *dna-complement-from* *dna-complement-to*))))

(defun reverse-complement (s)
  "The reverse complement of the nucleotide sequence S (a string for a string, a BIOSEQ for a BIOSEQ)."
  (let ((rc (%reverse-string (complement-seq s))))
    (if (bioseq-p s) (%make-bioseq (bioseq-id s) (bioseq-description s) (bioseq-alphabet s) rc nil) rc)))

(defun transcribe (s) "DNA to RNA (T becomes U)." (let ((r (substitute #\U #\T (%residues s)))) (if (bioseq-p s) (make-bioseq r :id (bioseq-id s) :alphabet :rna) r)))
(defun reverse-transcribe (s) "RNA to DNA (U becomes T)." (let ((r (substitute #\T #\U (%residues s)))) (if (bioseq-p s) (make-bioseq r :id (bioseq-id s) :alphabet :dna) r)))

(defun base-counts (s)
  "An alist of (residue . count) for the sequence S, in order of first appearance."
  (let ((acc nil))
    (loop for c across (%residues s) do (let ((cell (assoc c acc))) (if cell (incf (cdr cell)) (setq acc (cons (cons c 1) acc)))))
    (sort acc (lambda (a b) (char< (car a) (car b))))))

(defun gc-content (s &optional (start 0) end)
  "Fraction of G and C among the unambiguous residues of S[START, END)."
  (let* ((r (%residues s)) (end (or end (length r))) (gc (%count-chars r "GCgc" start end)) (n (+ gc (%count-chars r "ATUatu" start end))))
    (if (zerop n) 0.0 (/ (float gc) n))))

(defun gc-skew (s &optional (start 0) end)
  "(G - C) / (G + C) over S[START, END)."
  (let ((r (%residues s)) (g 0) (c 0))
    (loop for i from start below (or end (length r)) do (let ((ch (char r i))) (cond ((char= ch #\G) (incf g)) ((char= ch #\C) (incf c)))))
    (if (zerop (+ g c)) 0.0 (/ (float (- g c)) (+ g c)))))

(defun gc-windows (s window &optional (step window))
  "A list of (start . gc-fraction) for windows of WINDOW residues advancing by STEP."
  (let ((n (seq-length s)))
    (loop for i from 0 to (- n window) by step collect (cons i (gc-content s i (+ i window))))))

;;; ---- packed sequences --------------------------------------------------------------------------------------------------

(defstruct (packed-seq (:copier nil) (:constructor %make-packed-seq (length bits alphabet words exceptions))) length bits alphabet words exceptions)

(defun %per-word (bits) (floor 30 bits))

(defun pack-seq (string alphabet)
  "Pack STRING into fixnum words using ceil(log2 |ALPHABET|) bits per symbol; every character must be in ALPHABET."
  (let* ((bits (max 1 (integer-length (- (length alphabet) 1)))) (per (%per-word bits)) (n (length string))
         (words (make-array (ceiling n per) :initial-element 0)))
    (dotimes (i n)
      (let ((code (position (char string i) alphabet)))
        (unless code (error "Character ~S is not in the alphabet ~A" (char string i) alphabet))
        (multiple-value-bind (w k) (floor i per)
          (setf (aref words w) (logior (aref words w) (ash code (* k bits)))))))
    (%make-packed-seq n bits alphabet words nil)))

(defun pack-dna (string)
  "Pack a DNA string at 2 bits per base.  Characters other than ACGT (N, IUPAC codes) are kept as exceptions
(an alist of position . character) and read back correctly."
  (let* ((n (length string)) (per 15) (words (make-array (ceiling n per) :initial-element 0)) (exc nil))
    (dotimes (i n)
      (let* ((c (char string i)) (code (position c "ACGT")))
        (unless code (setq exc (cons (cons i c) exc) code 0))
        (multiple-value-bind (w k) (floor i per) (setf (aref words w) (logior (aref words w) (ash code (* k 2)))))))
    (%make-packed-seq n 2 "ACGT" words (reverse exc))))

(defun pack-iupac (string)
  "Pack a nucleotide string at 4 bits per symbol (all IUPAC codes and the gap)."
  (pack-seq string "ACGTRYSWKMBDHVN-"))

(defun pack-protein (string)
  "Pack a protein string at 5 bits per residue."
  (pack-seq string "ACDEFGHIKLMNPQRSTVWYXBZUO*-"))

(defun packed-ref (p i)
  "The character at position I of the packed sequence P."
  (let ((e (and (packed-seq-exceptions p) (assoc i (packed-seq-exceptions p)))))
    (if e (cdr e)
        (let* ((bits (packed-seq-bits p)) (per (%per-word bits)))
          (multiple-value-bind (w k) (floor i per)
            (char (packed-seq-alphabet p) (logand (ash (aref (packed-seq-words p) w) (- (* k bits))) (- (ash 1 bits) 1))))))))

(defun packed-subseq (p start &optional end)
  "The characters START..END of the packed sequence P as a string."
  (let* ((end (or end (packed-seq-length p))) (out (make-string (- end start) #\N)))
    (loop for i from start below end do (setf (char out (- i start)) (packed-ref p i)))
    out))

(defun unpack-seq (p) "The packed sequence P as a string." (packed-subseq p 0))
(defun packed-bytes (p) "Approximate storage used by the packed words of P, in bytes." (ceiling (* (length (packed-seq-words p)) 30) 8))

;;; ---- genetic codes ----------------------------------------------------------------------------------------------------------

(defstruct (genetic-code (:copier nil) (:constructor %make-genetic-code (id name aminos starts))) id name aminos starts)

(defvar *standard-aminos* "FFLLSSSSYY**CC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG")

(defun %codon-index (codon)
  (let ((r 0))
    (dotimes (i 3 r)
      (let ((p (position (char codon i) "TCAG")))
        (unless p (return-from %codon-index nil))
        (setq r (+ (* r 4) p))))))

(defun %modified-aminos (changes)
  (let ((s (copy-seq *standard-aminos*)))
    (dolist (c changes s) (setf (char s (%codon-index (car c))) (cdr c)))))

(defvar *genetic-codes*
  (list (%make-genetic-code 1 "Standard" *standard-aminos* '("TTG" "CTG" "ATG"))
        (%make-genetic-code 2 "Vertebrate Mitochondrial" (%modified-aminos '(("AGA" . #\*) ("AGG" . #\*) ("ATA" . #\M) ("TGA" . #\W))) '("ATT" "ATC" "ATA" "ATG" "GTG"))
        (%make-genetic-code 4 "Mold, Protozoan, Mycoplasma" (%modified-aminos '(("TGA" . #\W))) '("TTA" "TTG" "CTG" "ATT" "ATC" "ATA" "ATG" "GTG"))
        (%make-genetic-code 5 "Invertebrate Mitochondrial" (%modified-aminos '(("AGA" . #\S) ("AGG" . #\S) ("ATA" . #\M) ("TGA" . #\W))) '("TTG" "ATT" "ATC" "ATA" "ATG" "GTG"))
        (%make-genetic-code 11 "Bacterial, Archaeal and Plant Plastid" *standard-aminos* '("TTG" "CTG" "ATT" "ATC" "ATA" "ATG" "GTG"))))

(defun find-genetic-code (table)
  "The genetic code with NCBI id TABLE (1, 2, 4, 5 or 11), or the code object itself."
  (if (genetic-code-p table) table
      (or (find table *genetic-codes* :key #'genetic-code-id) (error "Unknown genetic code table ~A" table))))

(defun codon-amino (codon &optional (table 1))
  "The amino acid (a character) for CODON, a three-letter DNA or RNA string; X for ambiguous codons."
  (let* ((c (substitute #\T #\U (string-upcase codon))) (i (%codon-index c)))
    (if i (char (genetic-code-aminos (find-genetic-code table)) i) #\X)))

(defun translate (s &key (table 1) (frame 0) to-stop)
  "Translate nucleotide sequence S in reading FRAME (0, 1 or 2) with genetic code TABLE.  With TO-STOP translation ends
at the first stop codon.  Incomplete trailing codons are ignored."
  (let* ((code (genetic-code-aminos (find-genetic-code table)))
         (protein (%translate-codons (%residues s) frame code to-stop)))
    (if (bioseq-p s) (make-bioseq protein :id (bioseq-id s) :alphabet :protein) protein)))

;;; ---- k-mers -----------------------------------------------------------------------------------------------------------------------

(defun kmers (s k)
  "A list of every k-mer of S in order."
  (let ((r (%residues s))) (loop for i from 0 to (- (length r) k) collect (subseq r i (+ i k)))))

(defun kmer-counts (s k &key canonical)
  "A hash table (EQUAL) mapping each k-mer of S to its count; CANONICAL merges a k-mer with its reverse complement."
  (let ((h (make-hash-table :test 'equal)) (r (%residues s)))
    (loop for i from 0 to (- (length r) k)
          do (let* ((km (subseq r i (+ i k))) (km (if canonical (canonical-kmer km) km))) (setf (gethash km h) (+ 1 (gethash km h 0)))))
    h))

(defun canonical-kmer (km) "The lexicographically smaller of KM and its reverse complement." (let ((rc (reverse-complement km))) (if (string< rc km) rc km)))

(defun kmer-spectrum (s k) "Alist (kmer . count) of the k-mers of S, most frequent first." (let ((acc nil)) (maphash (lambda (key v) (push (cons key v) acc)) (kmer-counts s k)) (sort acc (lambda (a b) (or (> (cdr a) (cdr b)) (and (= (cdr a) (cdr b)) (string< (car a) (car b))))))))

;;; ---- ORFs -----------------------------------------------------------------------------------------------------------------------------

(defstruct (orf (:copier nil) (:constructor %make-orf (start end strand frame protein))) start end strand frame protein)

(defmethod print-object ((o orf) stream)
  (format stream "#<ORF ~D..~D ~A frame ~D ~Daa>" (orf-start o) (orf-end o) (if (> (orf-strand o) 0) "+" "-") (orf-frame o) (length (orf-protein o))))

(defun orf-length (o) "Length of the ORF in amino acids." (length (orf-protein o)))

(defun %start-flags (code start-codons)
  "A 64-character string with 1 at the index of every start codon (TCAG order)."
  (declare (ignore code))
  (let ((flags (make-string 64 #\0)))
    (dolist (c start-codons flags) (let ((i (%codon-index (substitute #\T #\U (string-upcase c))))) (when i (setf (char flags i) #\1))))))

(defun %scan-orfs (r strand total table min-length start-codons)
  (let* ((orfs nil) (code (find-genetic-code table)) (aminos (genetic-code-aminos code)) (flags (%start-flags code start-codons)))
    (dotimes (frame 3)
      (dolist (span (%orf-scan r frame aminos flags min-length))
        (let* ((open (car span)) (i (cdr span)) (protein (translate (subseq r open i) :table code)))
          (setq protein (%string-concat "M" (subseq protein 1)))
          (push (if (> strand 0)
                    (%make-orf open (+ i 3) 1 frame protein)
                    (%make-orf (- total (+ i 3)) (- total open) -1 frame protein))
                orfs))))
    orfs))

(defun find-orfs (s &key (min-length 30) (table 11) both-strands (start-codons '("ATG")))
  "Open reading frames of S (start codon through stop codon) of at least MIN-LENGTH amino acids, as ORF objects
sorted by start; coordinates are zero-based on the forward strand, END exclusive and including the stop codon."
  (let* ((r (substitute #\T #\U (%residues s))) (total (length r))
         (fwd (%scan-orfs r 1 total table min-length start-codons))
         (rev (if both-strands (%scan-orfs (reverse-complement r) -1 total table min-length start-codons) nil)))
    (sort (append fwd rev) (lambda (a b) (or (< (orf-start a) (orf-start b)) (and (= (orf-start a) (orf-start b)) (< (orf-end a) (orf-end b))))))))

;;; ---- distances and protein properties ----------------------------------------------------------------------------------------

(defun hamming-distance (a b) "Number of positions at which two equal-length sequences differ."
  (let ((x (%residues a)) (y (%residues b))) (unless (= (length x) (length y)) (error "Sequences differ in length")) (let ((d 0)) (dotimes (i (length x)) (unless (char= (char x i) (char y i)) (incf d))) d)))

(defun edit-distance (a b)
  "Levenshtein distance between two sequences."
  (let* ((x (%residues a)) (y (%residues b)) (n (length x)) (m (length y)) (prev (make-array (+ m 1) :initial-element 0)) (cur (make-array (+ m 1) :initial-element 0)))
    (dotimes (j (+ m 1)) (setf (aref prev j) j))
    (dotimes (i n)
      (setf (aref cur 0) (+ i 1))
      (dotimes (j m) (setf (aref cur (+ j 1)) (min (+ 1 (aref prev (+ j 1))) (+ 1 (aref cur j)) (+ (aref prev j) (if (char= (char x i) (char y j)) 0 1)))))
      (rotatef prev cur))
    (aref prev m)))

(defvar *residue-masses*
  '((#\A . 71.0788) (#\R . 156.1875) (#\N . 114.1038) (#\D . 115.0886) (#\C . 103.1388) (#\E . 129.1155) (#\Q . 128.1307) (#\G . 57.0519)
    (#\H . 137.1411) (#\I . 113.1594) (#\L . 113.1594) (#\K . 128.1741) (#\M . 131.1926) (#\F . 147.1766) (#\P . 97.1167) (#\S . 87.0782)
    (#\T . 101.1051) (#\W . 186.2132) (#\Y . 163.1760) (#\V . 99.1326)))

(defvar *hydropathy*
  '((#\A . 1.8) (#\R . -4.5) (#\N . -3.5) (#\D . -3.5) (#\C . 2.5) (#\Q . -3.5) (#\E . -3.5) (#\G . -0.4) (#\H . -3.2) (#\I . 4.5)
    (#\L . 3.8) (#\K . -3.9) (#\M . 1.9) (#\F . 2.8) (#\P . -1.6) (#\S . -0.8) (#\T . -0.7) (#\W . -0.9) (#\Y . -1.3) (#\V . 4.2)))

(defun protein-mass (s)
  "Average molecular mass of the protein S in daltons (residue masses plus one water)."
  (+ 18.01528 (loop for c across (%residues s) sum (or (cdr (assoc c *residue-masses*)) 0.0))))

(defun hydropathy-profile (s &optional (window 9))
  "Kyte-Doolittle hydropathy averaged over a sliding WINDOW: a list of values, one per window position."
  (let* ((r (%residues s)) (vals (map 'list (lambda (c) (or (cdr (assoc c *hydropathy*)) 0.0)) r)) (n (length vals)) (v (coerce vals 'vector)))
    (loop for i from 0 to (- n window) collect (/ (loop for j from i below (+ i window) sum (aref v j)) window))))

(defun melting-temperature (s)
  "Estimated melting temperature (Celsius) of the oligonucleotide S: the Wallace rule below 14 bases, the basic GC formula above."
  (let* ((r (%residues s)) (n (length r)) (gc (count-if (lambda (c) (or (char= c #\G) (char= c #\C))) r)) (at (- n gc)))
    (if (< n 14) (+ (* 2.0 at) (* 4.0 gc)) (+ 64.9 (/ (* 41.0 (- gc 16.4)) n)))))

(defun random-dna (n &key (gc 0.5) (rng *rng*))
  "A random DNA string of N bases with expected GC fraction GC."
  (let ((out (make-string n #\A)))
    (dotimes (i n)
      (let ((g (< (rand-uniform rng) gc)) (h (< (rand-uniform rng) 0.5)))
        (setf (char out i) (if g (if h #\G #\C) (if h #\A #\T)))))
    out))

(defun mutate-dna (s rate &key (rng *rng*))
  "A copy of the DNA string S with each base replaced by a different one with probability RATE."
  (let ((out (copy-seq s)))
    (dotimes (i (length out))
      (when (< (rand-uniform rng) rate)
        (let ((c (char out i))) (setf (char out i) (let ((n (char "ACGT" (rand-int 4 rng)))) (if (char= n c) (char "ACGT" (mod (+ 1 (position c "ACGT")) 4)) n))))))
    out))
