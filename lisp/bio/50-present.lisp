;;;; 50-present.lisp -- presentations for biology: sequences, residues, alignments, trees and features are
;;;; mouse-sensitive objects in the Listener and in dedicated viewer windows.
;;;;
;;;; Structures already present under their own type (BIOSEQ, ALIGNMENT, PHYLO-NODE, BIO-FEATURE, ORF, ...), so the
;;;; value of any form that returns one is clickable.  The viewers add finer-grained types: RESIDUE (a sequence
;;;; position), ALIGNMENT-COLUMN and CLADE.

(in-package :cl)

(define-presentation-type 'residue '(t))
(define-presentation-type 'alignment-column '(t))
(define-presentation-type 'clade '(t))
(define-presentation-type 'bio-feature '(t))

(defclass bio-window (report-window) ())

(defmacro %with-style ((out style) &body body)
  "Like WITH-STYLE, but also accepts an ordinary stream (the text is written unstyled)."
  `(if (typep ,out 'pane-stream) (with-style (,out ,style) ,@body) (progn ,@body)))

(defmacro %with-pres ((out object type) &body body)
  "Like WITH-OUTPUT-AS-PRESENTATION, but also accepts an ordinary stream."
  `(if (typep ,out 'pane-stream) (with-output-as-presentation (,out ,object ,type) ,@body) (progn ,@body)))

(defun %count-char (ch string start end) (let ((n 0)) (loop for i from start below end do (when (char= (char string i) ch) (incf n))) n))

(defun %bio-window (title producer subject &key (w 82) (h 28) (wm *wm*))
  "Open (or reuse) a window titled TITLE showing PRODUCER; without a window system the producer writes to standard output."
  (if wm
      (open-report wm title producer :class 'bio-window :subject subject :w w :h h)
      (progn (funcall producer nil *standard-output*) nil)))

;;; ---- sequence viewer ----------------------------------------------------------------------------------------------------

(defun %feature-mask (len features)
  (let ((mask (make-array len :initial-element nil)))
    (dolist (f features) (when (member (bio-feature-type f) '("CDS" "gene" "exon" "bed") :test #'string=)
                           (dolist (i (bio-feature-intervals f)) (loop for k from (max 0 (car i)) below (min len (cdr i)) do (setf (aref mask k) t)))))
    mask))

(defun render-sequence (seq out &key (start 0) end (width 60) features)
  "Write SEQ to OUT in rows of WIDTH residues in blocks of ten; every residue is a RESIDUE presentation."
  (let* ((r (%residues seq)) (end (min (length r) (or end (length r)))) (mask (and features (%feature-mask (length r) features))))
    (when (bioseq-p seq) (%with-style (out :bold) (format out "~A  ~A~%" (or (bioseq-id seq) "sequence") (or (bioseq-description seq) ""))))
    (%with-style (out :dim) (format out "~D ~A, ~D-~D shown~%~%" (length r) (if (eq (and (bioseq-p seq) (bioseq-alphabet seq)) :protein) "residues" "bases") (+ start 1) end))
    (loop for row from start below end by width
          do (%with-style (out :dim) (format out "~8D " (+ row 1)))
             (loop for i from row below (min end (+ row width))
                   do (when (and (> i row) (zerop (mod (- i row) 10))) (write-char #\Space out))
                      (%with-pres (out (list seq i) 'residue)
                        (if (and mask (aref mask i)) (%with-style (out :bold) (write-char (char r i) out)) (write-char (char r i) out))))
             (terpri out))))

(defun show-sequence (seq &key (start 0) end features (wm *wm*))
  "Show SEQ in a viewer window; click a residue for commands.  Without the window system the text is printed."
  (let ((s (if (stringp seq) (make-bioseq seq :id "sequence") seq)))
    (%bio-window (format nil "Sequence: ~A" (or (bioseq-id s) "")) (lambda (w out) (declare (ignore w)) (render-sequence s out :start start :end (or end (min (seq-length s) (+ start 3000))) :features features)) s :wm wm)))

(defun residue-description (seq index)
  "A multi-line description of residue INDEX of SEQ."
  (let* ((r (%residues seq)) (c (char r index)) (prot (eq (and (bioseq-p seq) (bioseq-alphabet seq)) :protein)))
    (with-output-to-string (o)
      (format o "Position ~D of ~D: ~A~%" (+ index 1) (length r) c)
      (if prot
          (progn (format o "  hydropathy ~A, residue mass ~A~%" (let ((h (assoc c *hydropathy*))) (if h (cdr h) "n/a")) (let ((m (assoc c *residue-masses*))) (if m (cdr m) "n/a")))
                 (format o "  window (+-5): ~A~%" (subseq r (max 0 (- index 5)) (min (length r) (+ index 6)))))
          (progn (format o "  complement ~A, local GC (+-50) ~,2F~%" (char (complement-seq (string c)) 0) (gc-content r (max 0 (- index 50)) (min (length r) (+ index 51))))
                 (when (<= (+ index 3) (length r))
                   (let ((codon (subseq r index (+ index 3)))) (format o "  codon starting here: ~A = ~A~%" codon (codon-amino codon))))
                 (format o "  context: ~A[~A]~A~%" (subseq r (max 0 (- index 8)) index) c (subseq r (+ index 1) (min (length r) (+ index 9)))))))))

(define-presentation-command (describe-residue residue :documentation "Describe residue" :default t) (p)
  (format t "~&~A" (residue-description (first p) (second p))))
(define-presentation-command (translate-from-here residue :documentation "Translate from here") (p)
  (let* ((r (%residues (first p))) (sub (subseq r (second p) (min (length r) (+ (second p) 90)))))
    (format t "~&Translation from position ~D: ~A~%" (+ (second p) 1) (translate sub))))
(define-presentation-command (reverse-complement-region residue :documentation "Reverse complement of next 30") (p)
  (let* ((r (%residues (first p))) (sub (subseq r (second p) (min (length r) (+ (second p) 30))))) (format t "~&~A~%" (reverse-complement sub))))

(define-presentation-command (view-sequence bioseq :documentation "View sequence" :default t) (s) (show-sequence s))
(define-presentation-command (translate-sequence bioseq :documentation "Translate (frame 0)") (s) (present (translate s)) (terpri))
(define-presentation-command (reverse-complement-sequence bioseq :documentation "Reverse complement") (s) (present (reverse-complement s)) (terpri))
(define-presentation-command (sequence-gc bioseq :documentation "GC content") (s) (format t "~&GC content ~,2F%~%" (* 100.0 (gc-content s))))
(define-presentation-command (sequence-orfs bioseq :documentation "Find ORFs") (s) (let ((orfs (find-orfs s :both-strands t))) (format t "~&~D ORFs:~%" (length orfs)) (dolist (o orfs) (format t "  ") (present o) (terpri))))

(define-presentation-command (describe-orf orf :documentation "Show protein" :default t) (o) (format t "~&~A~%~A~%" o (orf-protein o)))
(define-presentation-command (describe-feature bio-feature :documentation "Show qualifiers" :default t) (f)
  (format t "~&~A~%" f) (dolist (q (bio-feature-qualifiers f)) (format t "  ~A = ~A~%" (car q) (cdr q))))

;;; ---- alignment viewer ----------------------------------------------------------------------------------------------------------

(defun render-alignment (al out &key (width 60) matrix)
  "Write AL to OUT in blocks; every column's two residues are presentations of type ALIGNMENT-COLUMN."
  (let* ((n (alignment-length al)) (mid (alignment-midline al matrix)) (pa (alignment-a-start al)) (pb (alignment-b-start al)))
    (%with-style (out :bold) (format out "~A alignment, score ~D, ~,1F% identity, ~D gaps~%" (alignment-mode al) (alignment-score al) (* 100.0 (alignment-identity al)) (alignment-gaps al)))
    (loop for i from 0 below n by width
          do (let ((e (min n (+ i width))))
               (terpri out)
               (flet ((row (label str pos)
                        (%with-style (out :dim) (format out "~8D " (+ pos 1)))
                        (loop for k from i below e do (%with-pres (out (list al k) 'alignment-column) (write-char (char str k) out)))
                        (format out " ~D~%" (+ pos (- (- e i) (%count-char #\- str i e))))
                        label))
                 (row 'a (alignment-aligned-a al) pa)
                 (format out "         ~A~%" (subseq mid i e))
                 (row 'b (alignment-aligned-b al) pb))
               (incf pa (- (- e i) (%count-char #\- (alignment-aligned-a al) i e)))
               (incf pb (- (- e i) (%count-char #\- (alignment-aligned-b al) i e)))))))

(defun show-alignment (al &key matrix (wm *wm*))
  "Show the alignment AL in a viewer window."
  (%bio-window "Alignment" (lambda (w out) (declare (ignore w)) (render-alignment al out :matrix matrix)) al :wm wm))

(defun alignment-column-description (al k)
  "Describe column K of AL: residues, positions in both sequences, identity."
  (let* ((x (char (alignment-aligned-a al) k)) (y (char (alignment-aligned-b al) k))
         (pa (+ (alignment-a-start al) (- k (%count-char #\- (alignment-aligned-a al) 0 k)) (if (char= x #\-) 0 1)))
         (pb (+ (alignment-b-start al) (- k (%count-char #\- (alignment-aligned-b al) 0 k)) (if (char= y #\-) 0 1))))
    (format nil "Column ~D: ~A vs ~A (~A)~%  position in first sequence: ~A, in second: ~A~%" (+ k 1) x y (cond ((char= x y) "identical") ((or (char= x #\-) (char= y #\-)) "gap") (t "mismatch"))
            (if (char= x #\-) "-" pa) (if (char= y #\-) "-" pb))))

(define-presentation-command (describe-alignment-column alignment-column :documentation "Describe column" :default t) (p)
  (format t "~&~A" (alignment-column-description (first p) (second p))))
(define-presentation-command (view-alignment alignment :documentation "View alignment" :default t) (al) (show-alignment al))
(define-presentation-command (alignment-text alignment :documentation "Print as text") (al) (format t "~&~A" (alignment-string al)))

;;; ---- tree viewer ---------------------------------------------------------------------------------------------------------------

(defun render-tree (tree out)
  "Write TREE to OUT as an indented outline in which every node is a CLADE presentation."
  (labels ((label (n) (%string-concat (or (phylo-node-name n) (if (phylo-leaf-p n) "?" (format nil "clade of ~D" (length (tree-leaves n)))))
                                      (if (phylo-node-branch-length n) (format nil " (~A)" (%fmt-length (phylo-node-branch-length n))) "")))
           (walk (n prefix last root)
             (%with-style (out :dim) (write-string (if root "" (%string-concat prefix (if last (%string-concat (%cp-string 9492) (%cp-string 9472) (%cp-string 9472) " ") (%string-concat (%cp-string 9500) (%cp-string 9472) (%cp-string 9472) " ")))) out))
             (%with-pres (out n 'clade) (if (phylo-leaf-p n) (write-string (label n) out) (%with-style (out :bold) (write-string (label n) out))))
             (terpri out)
             (let ((child-prefix (if root "" (%string-concat prefix (if last "    " (%string-concat (%cp-string 9474) "   "))))))
               (loop for c in (phylo-node-children n) for rest on (phylo-node-children n) do (walk c child-prefix (null (cdr rest)) nil)))))
    (walk tree "" t t)))

(defun show-tree (tree &key (wm *wm*))
  "Show TREE in a viewer window; click a clade for commands."
  (%bio-window "Tree" (lambda (w out) (declare (ignore w)) (render-tree tree out)) tree :wm wm))

(define-presentation-command (zoom-clade clade :documentation "Zoom to this clade" :default t) (n)
  (let ((w *current-listener*))
    (if (typep w 'bio-window) (report-navigate w (lambda (win out) (declare (ignore win)) (render-tree n out)) n "Tree") (show-tree n))))
(define-presentation-command (clade-newick clade :documentation "Newick") (n) (format t "~&~A~%" (tree-newick n)))
(define-presentation-command (clade-leaves clade :documentation "List leaves") (n) (format t "~&~{~A~^, ~}~%" (tree-leaf-names n)))
(define-presentation-command (clade-height clade :documentation "Height") (n) (format t "~&~D leaves, height ~A, total branch length ~A~%" (length (tree-leaves n)) (%fmt-length (tree-height n)) (%fmt-length (tree-total-length n))))
(define-presentation-command (view-tree phylo-node :documentation "View tree" :default t) (n) (show-tree n))
(define-presentation-command (tree-newick-cmd phylo-node :documentation "Newick") (n) (format t "~&~A~%" (tree-newick n)))

;;; ---- feature list and commands --------------------------------------------------------------------------------------------

(defun show-features (features &key (wm *wm*))
  "List FEATURES (BIO-FEATURE objects) in a window; click one to see its qualifiers."
  (%bio-window "Features"
               (lambda (w out) (declare (ignore w))
                 (%with-style (out :bold) (format out "~D features~%~%" (length features)))
                 (dolist (f features) (write-string "  " out) (present f :stream out) (terpri out)))
               features :wm wm))

(defun bio-view (thing)
  "Show THING in the appropriate viewer: a sequence, alignment, tree or a list of features."
  (cond ((or (bioseq-p thing) (stringp thing)) (show-sequence thing))
        ((alignment-p thing) (show-alignment thing))
        ((phylo-node-p thing) (show-tree thing))
        ((and (consp thing) (bio-feature-p (car thing))) (show-features thing))
        (t (error "Do not know how to view ~S" thing))))

(define-command (bio-view :documentation "Show a sequence, alignment, tree or feature list returned by an expression") ((expr string :prompt "Expression"))
  (bio-view (eval (read-from-string expr))))
