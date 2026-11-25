;;; Annotate a synthetic bacterial-style genome, search a protein against its proteome, and build a tree.
;;; expect: ORFs found
(rng-seed 7)
(defun make-gene (naa)
  (let ((codons nil))
    (dotimes (i 64)
      (let ((c (map 'string (lambda (k) (char "TCAG" k)) (list (floor i 16) (mod (floor i 4) 4) (mod i 4)))))
        (unless (char= (codon-amino c) #\*) (push c codons))))
    (apply #'concatenate 'string "ATG" (append (loop repeat (- naa 1) collect (rand-choice codons)) (list (rand-choice '("TAA" "TAG" "TGA")))))))
(defun make-genome (ngenes)
  (apply #'concatenate 'string
         (loop repeat ngenes
               collect (random-dna 80)
               collect (let ((unit (concatenate 'string "TAA" (make-gene (+ 90 (rand-int 120)))))) (if (evenp (rand-int 2)) unit (reverse-complement unit))))))
(let* ((genome (make-bioseq (make-genome 30) :id "chrDemo" :description "synthetic genome"))
       (features (annotate-orfs genome :min-length 75))
       (report (genome-report genome :min-length 75)))
  (format t "~D bp, GC ~,1F%, ~D ORFs found (~D forward, ~D reverse), ~,0F% coding~%"
          (getf report :length) (* 100 (getf report :gc)) (getf report :orfs) (getf report :forward) (getf report :reverse) (* 100 (getf report :coding-fraction)))
  (format t "first features:~%")
  (dolist (f (subseq features 0 (min 4 (length features)))) (format t "  ~A~%" f))
  (let* ((proteins (cds-proteins features)) (query (bioseq-residues (nth 3 proteins)))
         (diverged (let ((s (copy-seq query))) (dotimes (i (length s) s) (when (< (rand-uniform) 0.2) (setf (char s i) (rand-choice "ARNDCQEGHILKMFPSTWYV")))))))
    (let ((best (first (rank-by-alignment diverged (subseq proteins 0 10)))))
      (format t "best hit ~A, identity ~,0F%, bit score ~,0F~%" (bioseq-id (first best)) (* 100 (alignment-identity (second best))) (third best))))
  (let* ((a (subseq (bioseq-residues genome) 500 800))
         (taxa (list (make-bioseq a :id "root-ish") (make-bioseq (mutate-dna a 0.03) :id "sp1") (make-bioseq (mutate-dna a 0.06) :id "sp2")
                     (make-bioseq (mutate-dna a 0.12) :id "sp3") (make-bioseq (mutate-dna a 0.2) :id "sp4"))))
    (format t "~%neighbor-joining tree:~%~A~%" (tree-ascii (tree-from-sequences taxa)))))
