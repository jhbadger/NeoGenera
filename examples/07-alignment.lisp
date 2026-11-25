;;; Pairwise protein and DNA alignment.
;;; expect: identity
(let ((al (align "HEAGAWGHEE" "PAWHEAE" :mode :local :matrix *blosum62* :gap-open 8 :gap-extend 8)))
  (princ (alignment-string al :matrix *blosum62*)))
(terpri)
(let ((al (align "GATTACAGATTACA" "GCATGCUGATTACA" :mode :global :matrix (nucleotide-matrix 1 -1) :gap-open 1 :gap-extend 1)))
  (princ (alignment-string al)))
(terpri)
(format t "identity of two homologs: ~,1F%~%"
        (percent-identity "MKVLAAGIVGLLLAQPSWAHHHH" "MKVLSAGIVGLLAQPSWAHHH" :matrix *blosum62*))
