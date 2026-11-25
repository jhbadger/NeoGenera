;;;; 30-phylo.lisp -- distance matrices, UPGMA and neighbor-joining, trees, Newick, text rendering.

(in-package :cl)

(defstruct (phylo-node (:copier nil) (:constructor %make-phylo-node (name children branch-length)))
  name children branch-length)

(defun make-phylo-leaf (name &optional branch-length) "A leaf node called NAME." (%make-phylo-node name nil branch-length))
(defun make-phylo-node (children &key name branch-length) "An internal node with CHILDREN." (%make-phylo-node name children branch-length))
(defun phylo-leaf-p (n) "True when N has no children." (null (phylo-node-children n)))

(defmethod print-object ((n phylo-node) stream)
  (format stream "#<TREE ~D leaves~@[ ~A~]>" (length (tree-leaves n)) (phylo-node-name n)))

(defun tree-leaves (tree) "The leaf nodes of TREE from left to right."
  (if (phylo-leaf-p tree) (list tree) (apply #'append (mapcar #'tree-leaves (phylo-node-children tree)))))
(defun tree-leaf-names (tree) "The leaf names of TREE from left to right." (mapcar #'phylo-node-name (tree-leaves tree)))
(defun tree-node-count (tree) "Number of nodes in TREE." (+ 1 (loop for c in (phylo-node-children tree) sum (tree-node-count c))))
(defun tree-height (tree) "The longest root-to-leaf path length (sum of branch lengths)."
  (+ (or (phylo-node-branch-length tree) 0.0) (if (phylo-leaf-p tree) 0.0 (reduce #'max (mapcar #'tree-height (phylo-node-children tree))))))
(defun tree-total-length (tree) "The sum of all branch lengths in TREE." (+ (or (phylo-node-branch-length tree) 0.0) (loop for c in (phylo-node-children tree) sum (tree-total-length c))))

(defun tree-find (tree name)
  "The node of TREE named NAME, or NIL."
  (cond ((equal (phylo-node-name tree) name) tree)
        (t (dolist (c (phylo-node-children tree) nil) (let ((f (tree-find c name))) (when f (return f)))))))

(defun %fmt-length (x) (let ((s (format nil "~,6F" x))) (string-right-trim "." (string-right-trim "0" s))))

(defun tree-newick (tree &optional (with-lengths t))
  "TREE in Newick format."
  (labels ((walk (n)
             (%string-concat
              (if (phylo-leaf-p n) "" (%string-concat "(" (%join-strings (mapcar #'walk (phylo-node-children n)) ",") ")"))
              (or (phylo-node-name n) "")
              (if (and with-lengths (phylo-node-branch-length n)) (%string-concat ":" (%fmt-length (phylo-node-branch-length n))) ""))))
    (%string-concat (walk tree) ";")))

(defun parse-newick (text)
  "Parse a Newick string into a PHYLO-NODE tree."
  (let ((pos 0) (s (remove-if (lambda (c) (member c (list #\Newline #\Return #\Tab))) text)))
    (labels ((peek () (and (< pos (length s)) (char s pos)))
             (name-token () (let ((start pos)) (loop while (and (peek) (not (find (peek) "(),:;"))) do (incf pos)) (%trim (subseq s start pos))))
             (parse-node ()
               (let ((children nil) (name nil) (len nil))
                 (when (eql (peek) #\()
                   (incf pos)
                   (push (parse-node) children)
                   (loop while (eql (peek) #\,) do (incf pos) (push (parse-node) children))
                   (unless (eql (peek) #\)) (error "Newick: expected ) at ~D" pos))
                   (incf pos))
                 (let ((tok (name-token))) (when (> (length tok) 0) (setq name (string-trim "'" tok))))
                 (when (eql (peek) #\:) (incf pos) (setq len (float (read-from-string (name-token)))))
                 (%make-phylo-node name (nreverse children) len))))
      (parse-node))))

(defun tree-ascii (tree &key (lengths t))
  "A text rendering of TREE with box-drawing characters, one node per line."
  (let ((lines nil))
    (labels ((label (n) (%string-concat (or (phylo-node-name n) (if (phylo-leaf-p n) "?" "+"))
                                        (if (and lengths (phylo-node-branch-length n)) (format nil " (~A)" (%fmt-length (phylo-node-branch-length n))) "")))
             (walk (n prefix last root)
               (push (if root (label n) (%string-concat prefix (if last (%string-concat (%cp-string 9492) (%cp-string 9472) (%cp-string 9472) " ") (%string-concat (%cp-string 9500) (%cp-string 9472) (%cp-string 9472) " ")) (label n))) lines)
               (let ((kids (phylo-node-children n)) (child-prefix (if root "" (%string-concat prefix (if last "    " (%string-concat (%cp-string 9474) "   "))))))
                 (loop for c in kids for rest on kids do (walk c child-prefix (null (cdr rest)) nil)))))
      (walk tree "" t t))
    (join-lines (reverse lines))))

(defun tree-patristic-distances (tree)
  "A DMATRIX of the path-length distances between the leaves of TREE."
  (let* ((names (tree-leaf-names tree)) (n (length names)) (m (make-matrix n n)) (index (make-hash-table :test 'equal)))
    (loop for nm in names for i from 0 do (setf (gethash nm index) i))
    (labels ((walk (node) ; returns an alist (leaf-index . distance-to-node)
               (if (phylo-leaf-p node) (list (cons (gethash (phylo-node-name node) index) (or (phylo-node-branch-length node) 0.0)))
                   (let ((sets (mapcar #'walk (phylo-node-children node))))
                     (loop for a in sets for ra on sets
                           do (dolist (b (cdr ra))
                                (dolist (x a) (dolist (y b) (let ((d (+ (cdr x) (cdr y)))) (setf (mref m (car x) (car y)) d) (setf (mref m (car y) (car x)) d))))))
                     (mapcar (lambda (p) (cons (car p) (+ (cdr p) (or (phylo-node-branch-length node) 0.0)))) (apply #'append sets))))))
      (walk tree))
    (%make-dmatrix names m)))

(defun tree-bipartitions (tree)
  "The nontrivial leaf bipartitions of TREE as sorted lists of the smaller side's leaf names (for tree comparison)."
  (let* ((all (sort (copy-list (tree-leaf-names tree)) #'string<)) (n (length all)) (out nil))
    (labels ((walk (node) (if (phylo-leaf-p node) (list (phylo-node-name node))
                              (let ((leaves (sort (copy-list (apply #'append (mapcar #'walk (phylo-node-children node)))) #'string<)))
                                (when (and (> (length leaves) 1) (< (length leaves) (- n 1)))
                                  (let ((other (set-difference all leaves :test #'string=)))
                                    (push (if (or (< (length leaves) (length other)) (and (= (length leaves) (length other)) (string< (car leaves) (car (sort (copy-list other) #'string<)))))
                                              leaves (sort (copy-list other) #'string<))
                                          out)))
                                leaves))))
      (walk tree))
    (remove-duplicates out :test #'equal)))

(defun robinson-foulds (a b)
  "The Robinson-Foulds distance between two trees on the same leaves: the number of bipartitions found in only one."
  (let ((pa (tree-bipartitions a)) (pb (tree-bipartitions b)))
    (+ (length (set-difference pa pb :test #'equal)) (length (set-difference pb pa :test #'equal)))))

;;; ---- distance matrices -------------------------------------------------------------------------------------------------

(defstruct (dmatrix (:copier nil) (:constructor %make-dmatrix (names matrix))) names matrix)

(defun dmatrix-ref (d a b) "The distance between taxa A and B (names or indices)."
  (let ((i (if (integerp a) a (position a (dmatrix-names d) :test #'equal))) (j (if (integerp b) b (position b (dmatrix-names d) :test #'equal)))) (mref (dmatrix-matrix d) i j)))
(defun dmatrix-size (d) "Number of taxa in D." (length (dmatrix-names d)))

(defmethod print-object ((d dmatrix) stream) (format stream "#<DMATRIX ~D taxa>" (dmatrix-size d)))

(defun make-dmatrix (names rows) "A distance matrix from taxon NAMES and a list of ROWS." (%make-dmatrix names (mat rows)))

(defun sequence-distance (a b &key (model :jc69))
  "The evolutionary distance between aligned sequences A and B of equal length: :p (proportion of differences),
:jc69 (Jukes-Cantor) or :k2p (Kimura two-parameter, nucleotides only).  Gap columns are skipped."
  (let ((x (%residues a)) (y (%residues b)) (diff 0) (ts 0) (tv 0) (n 0))
    (dotimes (i (length x))
      (let ((p (char x i)) (q (char y i)))
        (unless (or (char= p #\-) (char= q #\-) (char= p #\N) (char= q #\N))
          (incf n)
          (unless (char= p q) (incf diff)
                  (if (or (and (find p "AG") (find q "AG")) (and (find p "CT") (find q "CT"))) (incf ts) (incf tv))))))
    (let ((p (if (zerop n) 0.0 (/ (float diff) n))))
      (ecase model
        (:p p)
        (:jc69 (if (>= p 0.75) +infinity+ (* -0.75 (log (- 1.0 (* (/ 4.0 3.0) p))))))
        (:k2p (let* ((s (/ (float ts) (max n 1))) (v (/ (float tv) (max n 1))) (t1 (- 1.0 (* 2.0 s) v)) (t2 (- 1.0 (* 2.0 v))))
                (if (or (<= t1 0.0) (<= t2 0.0)) +infinity+ (+ (* -0.5 (log t1)) (* -0.25 (log t2))))))))))

(defun distance-matrix (seqs &key (model :jc69))
  "The DMATRIX of pairwise distances between SEQS (BIOSEQ objects).  Sequences of different lengths are first aligned globally."
  (let* ((n (length seqs)) (m (make-matrix n n)) (names (mapcar (lambda (s) (or (bioseq-id s) "?")) seqs)) (v (coerce seqs 'vector)))
    (dotimes (i n)
      (loop for j from (+ i 1) below n
            do (let* ((a (aref v i)) (b (aref v j))
                      (d (if (= (seq-length a) (seq-length b)) (sequence-distance a b :model model)
                             (let ((al (align a b :mode :global))) (sequence-distance (alignment-aligned-a al) (alignment-aligned-b al) :model model)))))
                 (setf (mref m i j) d) (setf (mref m j i) d))))
    (%make-dmatrix names m)))

;;; ---- UPGMA and neighbor joining -----------------------------------------------------------------------------------------

(defun upgma (d)
  "Build an ultrametric tree from the DMATRIX D by average linkage (UPGMA)."
  (let* ((n (dmatrix-size d)) (clusters (loop for i from 0 below n collect (list :tree (make-phylo-leaf (nth i (dmatrix-names d))) :size 1 :height 0.0 :id i)))
         (dist (make-hash-table :test 'equal)))
    (dotimes (i n) (dotimes (j n) (setf (gethash (cons i j) dist) (mref (dmatrix-matrix d) i j))))
    (let ((next n))
      (loop while (> (length clusters) 1)
            do (let ((bi nil) (bj nil) (best nil))
                 (loop for a in clusters for ra on clusters
                       do (dolist (b (cdr ra))
                            (let ((dd (gethash (cons (getf a :id) (getf b :id)) dist)))
                              (when (or (null best) (< dd best)) (setq best dd bi a bj b)))))
                 (let* ((h (/ best 2.0))
                        (ti (getf bi :tree)) (tj (getf bj :tree)))
                   (setf (phylo-node-branch-length ti) (- h (getf bi :height)) (phylo-node-branch-length tj) (- h (getf bj :height)))
                   (let ((new (list :tree (make-phylo-node (list ti tj)) :size (+ (getf bi :size) (getf bj :size)) :height h :id next)))
                     (dolist (c clusters)
                       (unless (or (eq c bi) (eq c bj))
                         (let ((dn (/ (+ (* (getf bi :size) (gethash (cons (getf bi :id) (getf c :id)) dist)) (* (getf bj :size) (gethash (cons (getf bj :id) (getf c :id)) dist))) (+ (getf bi :size) (getf bj :size)))))
                           (setf (gethash (cons next (getf c :id)) dist) dn (gethash (cons (getf c :id) next) dist) dn))))
                     (setq clusters (cons new (remove bj (remove bi clusters))))
                     (incf next))))))
    (getf (first clusters) :tree)))

(defun neighbor-joining (d)
  "Build an unrooted tree (returned with a trifurcating root) from the DMATRIX D by neighbor joining (Saitou and Nei)."
  (let* ((n (dmatrix-size d)) (nodes (loop for i from 0 below n collect (cons i (make-phylo-leaf (nth i (dmatrix-names d)))))) (dist (make-hash-table :test 'equal)) (next n))
    (dotimes (i n) (dotimes (j n) (setf (gethash (cons i j) dist) (mref (dmatrix-matrix d) i j))))
    (flet ((dd (a b) (if (= a b) 0.0 (gethash (cons a b) dist))))
      (loop while (> (length nodes) 3)
            do (let* ((r (length nodes)) (ids (mapcar #'car nodes))
                      (rsum (mapcar (lambda (i) (cons i (loop for j in ids sum (dd i j)))) ids))
                      (best nil) (bi nil) (bj nil))
                 (loop for i in ids for ri on ids
                       do (dolist (j (cdr ri))
                            (let ((q (- (* (- r 2) (dd i j)) (cdr (assoc i rsum)) (cdr (assoc j rsum)))))
                              (when (or (null best) (< q best)) (setq best q bi i bj j)))))
                 (let* ((dij (dd bi bj)) (li (+ (/ dij 2.0) (/ (- (cdr (assoc bi rsum)) (cdr (assoc bj rsum))) (* 2.0 (- r 2)))))
                        (lj (- dij li)) (ni (cdr (assoc bi nodes))) (nj (cdr (assoc bj nodes))))
                   (setf (phylo-node-branch-length ni) (max 0.0 li) (phylo-node-branch-length nj) (max 0.0 lj))
                   (let ((u next)) (incf next)
                     (dolist (k ids) (unless (or (= k bi) (= k bj))
                                       (let ((dk (/ (- (+ (dd bi k) (dd bj k)) dij) 2.0))) (setf (gethash (cons u k) dist) dk (gethash (cons k u) dist) dk))))
                     (setq nodes (cons (cons u (make-phylo-node (list ni nj))) (remove-if (lambda (e) (or (= (car e) bi) (= (car e) bj))) nodes)))))))
      (if (= (length nodes) 3)
          (let* ((a (car (first nodes))) (b (car (second nodes))) (c (car (third nodes)))
                 (la (/ (+ (dd a b) (dd a c) (- (dd b c))) 2.0)) (lb (- (dd a b) la)) (lc (- (dd a c) la))
                 (na (cdr (first nodes))) (nb (cdr (second nodes))) (nc (cdr (third nodes))))
            (setf (phylo-node-branch-length na) (max 0.0 la) (phylo-node-branch-length nb) (max 0.0 lb) (phylo-node-branch-length nc) (max 0.0 lc))
            (make-phylo-node (list na nb nc)))
          (if (= (length nodes) 2)
              (let ((a (cdr (first nodes))) (b (cdr (second nodes))))
                (setf (phylo-node-branch-length a) (/ (dd (car (first nodes)) (car (second nodes))) 2.0) (phylo-node-branch-length b) (/ (dd (car (first nodes)) (car (second nodes))) 2.0))
                (make-phylo-node (list a b)))
              (cdr (first nodes)))))))

(defun tree-from-sequences (seqs &key (method :nj) (model :jc69))
  "Infer a tree from SEQS: distances by MODEL, then :nj (neighbor joining) or :upgma."
  (let ((d (distance-matrix seqs :model model))) (if (eq method :upgma) (upgma d) (neighbor-joining d))))

;;; ---- genomic intervals ------------------------------------------------------------------------------------------------------

(defstruct (interval-tree (:copier nil) (:constructor %make-interval-tree (root size))) root size)
(defstruct (itnode (:copier nil) (:constructor %make-itnode (start end data left right max-end))) start end data left right max-end)

(defun make-interval-tree (items)
  "An interval tree over ITEMS, each a list (start end data) with half-open [start, end); queries run in O(log n + k)."
  (let* ((v (coerce (sort (copy-list items) (lambda (a b) (< (first a) (first b)))) 'vector)))
    (labels ((build (lo hi)
               (if (>= lo hi) nil
                   (let* ((mid (floor (+ lo hi) 2)) (it (aref v mid)) (l (build lo mid)) (r (build (+ mid 1) hi)))
                     (%make-itnode (first it) (second it) (third it) l r (max (second it) (if l (itnode-max-end l) -1) (if r (itnode-max-end r) -1)))))))
      (%make-interval-tree (build 0 (length v)) (length v)))))

(defun interval-overlaps (tree start end)
  "All items (start end data) of TREE overlapping [START, END), in order of start."
  (let ((out nil))
    (labels ((walk (n)
               (when (and n (> (itnode-max-end n) start))
                 (walk (itnode-left n))
                 (when (and (< (itnode-start n) end) (> (itnode-end n) start)) (push (list (itnode-start n) (itnode-end n) (itnode-data n)) out))
                 (when (< (itnode-start n) end) (walk (itnode-right n))))))
      (walk (interval-tree-root tree)))
    (nreverse out)))

(defun interval-at (tree point) "Items of TREE containing the position POINT." (interval-overlaps tree point (+ point 1)))

(defun merge-intervals (intervals)
  "Merge overlapping or touching (start . end) pairs into a sorted list of disjoint pairs."
  (let ((sorted (sort (copy-list intervals) (lambda (a b) (< (car a) (car b))))) (out nil))
    (dolist (i sorted)
      (if (and out (<= (car i) (cdr (car out)))) (setf (cdr (car out)) (max (cdr (car out)) (cdr i))) (push (cons (car i) (cdr i)) out)))
    (nreverse out)))

(defun interval-complement (intervals length)
  "The gaps of the disjoint merged INTERVALS within [0, LENGTH)."
  (let ((pos 0) (out nil))
    (dolist (i (merge-intervals intervals)) (when (> (car i) pos) (push (cons pos (car i)) out)) (setq pos (max pos (cdr i))))
    (when (< pos length) (push (cons pos length) out))
    (nreverse out)))

(defun coverage-depth (intervals length)
  "A vector of the number of (start . end) intervals covering each position of [0, LENGTH), computed with a difference array."
  (let ((diff (make-array (+ length 1) :initial-element 0)) (depth (make-array length :initial-element 0)) (run 0))
    (dolist (i intervals) (when (< (car i) length) (incf (aref diff (max 0 (car i)))) (decf (aref diff (min length (cdr i))))))
    (dotimes (k length) (incf run (aref diff k)) (setf (aref depth k) run))
    depth))

(defun coverage-segments (depth)
  "Run-length encode a depth vector as a list of (start end depth) with constant depth."
  (let ((out nil) (n (length depth)) (i 0))
    (loop while (< i n) do (let ((j i)) (loop while (and (< (+ j 1) n) (= (aref depth (+ j 1)) (aref depth i))) do (incf j)) (push (list i (+ j 1) (aref depth i)) out) (setq i (+ j 1))))
    (nreverse out)))

(defun intersect-intervals (a b)
  "Intersection of two lists of (start . end) pairs, as a merged list."
  (let ((ma (merge-intervals a)) (mb (merge-intervals b)) (out nil))
    (dolist (x ma) (dolist (y mb) (let ((s (max (car x) (car y))) (e (min (cdr x) (cdr y)))) (when (< s e) (push (cons s e) out)))))
    (merge-intervals out)))

(defun build-feature-index (features)
  "A hash table from sequence id to an interval tree of the BIO-FEATURE objects on it (data = the feature)."
  (let ((by (make-hash-table :test 'equal)) (index (make-hash-table :test 'equal)))
    (dolist (f features) (push (list (feature-start f) (feature-end f) f) (gethash (bio-feature-seqid f) by)))
    (maphash (lambda (k items) (setf (gethash k index) (make-interval-tree items))) by)
    index))

(defun features-overlapping (index seqid start end)
  "The features in the INDEX (from BUILD-FEATURE-INDEX) on SEQID overlapping [START, END)."
  (let ((tree (gethash seqid index))) (and tree (mapcar #'third (interval-overlaps tree start end)))))

(defun nearest-feature (features seqid position)
  "The feature on SEQID closest to POSITION, with its distance (0 when inside)."
  (let ((best nil) (bd nil))
    (dolist (f features)
      (when (equal (bio-feature-seqid f) seqid)
        (let ((d (cond ((< position (feature-start f)) (- (feature-start f) position)) ((>= position (feature-end f)) (- position (feature-end f) -1)) (t 0))))
          (when (or (null bd) (< d bd)) (setq bd d best f)))))
    (values best bd)))
