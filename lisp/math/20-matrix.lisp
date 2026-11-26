;;;; 20-matrix.lisp -- dense matrices, vectors and linear algebra.
;;;;
;;;; A MATRIX is a row-major simple vector of floats with row and column counts.  Vectors are ordinary Lisp
;;;; vectors (lists are accepted where noted).  Factorisations: LU (partial pivoting), Householder QR, Cholesky,
;;;; Jacobi eigen-decomposition (symmetric), one-sided Jacobi SVD, shifted-QR eigenvalues (general).

(in-package :cl)

(defstruct (matrix (:constructor %make-matrix (rows cols data)) (:copier nil)) rows cols data)

(defun make-matrix (rows cols &optional (initial 0.0))
  "A ROWS x COLS matrix filled with INITIAL."
  (%make-matrix rows cols (make-array (* rows cols) :initial-element initial)))

(defun mat (rows)
  "A matrix from a list (or vector) of rows, each a list or vector of numbers."
  (let* ((rs (coerce rows 'list)) (r (length rs)) (c (length (coerce (car rs) 'list))) (m (make-matrix r c)) (i 0))
    (dolist (row rs)
      (let ((j 0)) (dolist (x (coerce row 'list)) (setf (aref (matrix-data m) (+ (* i c) j)) (float x)) (setq j (+ j 1))))
      (setq i (+ i 1)))
    m))

(defun identity-matrix (n)
  "The N x N identity matrix."
  (let ((m (make-matrix n n))) (dotimes (i n) (setf (aref (matrix-data m) (+ (* i n) i)) 1.0)) m))

(defun diagonal-matrix (values)
  "A square matrix with VALUES on the diagonal."
  (let* ((v (coerce values 'vector)) (n (length v)) (m (make-matrix n n)))
    (dotimes (i n) (setf (aref (matrix-data m) (+ (* i n) i)) (float (aref v i)))) m))

(defun mref (m i j)
  "Element (I, J) of the matrix M (zero-based)."
  (aref (matrix-data m) (+ (* i (matrix-cols m)) j)))

(defun (setf mref) (value m i j)
  (setf (aref (matrix-data m) (+ (* i (matrix-cols m)) j)) value))

(defun matrix-copy (m)
  "A copy of the matrix M."
  (%make-matrix (matrix-rows m) (matrix-cols m) (copy-seq (matrix-data m))))

(defun matrix->lists (m)
  "The matrix M as a list of row lists."
  (let ((rows nil))
    (dotimes (i (matrix-rows m)) (setq rows (cons (let ((r nil)) (dotimes (j (matrix-cols m)) (setq r (cons (mref m i j) r))) (reverse r)) rows)))
    (reverse rows)))

(defun matrix-row (m i) "Row I of M as a vector." (let ((v (make-array (matrix-cols m) :initial-element 0.0))) (dotimes (j (matrix-cols m)) (setf (aref v j) (mref m i j))) v))
(defun matrix-col (m j) "Column J of M as a vector." (let ((v (make-array (matrix-rows m) :initial-element 0.0))) (dotimes (i (matrix-rows m)) (setf (aref v i) (mref m i j))) v))

(defun matrix-from-columns (columns)
  "A matrix whose columns are the given vectors."
  (let* ((cs (coerce columns 'list)) (c (length cs)) (r (length (coerce (car cs) 'list))) (m (make-matrix r c)) (j 0))
    (dolist (col cs) (let ((i 0)) (dolist (x (coerce col 'list)) (setf (mref m i j) (float x)) (setq i (+ i 1)))) (setq j (+ j 1)))
    m))

(defun submatrix (m r0 r1 c0 c1)
  "Rows R0..R1-1 and columns C0..C1-1 of M."
  (let ((s (make-matrix (- r1 r0) (- c1 c0))))
    (loop for i from r0 below r1 do (loop for j from c0 below c1 do (setf (mref s (- i r0) (- j c0)) (mref m i j)))) s))

(defun matrix-transpose (m)
  "The transpose of M."
  (let ((tm (make-matrix (matrix-cols m) (matrix-rows m))))
    (dotimes (i (matrix-rows m)) (dotimes (j (matrix-cols m)) (setf (mref tm j i) (mref m i j)))) tm))

(defun %check-same-shape (a b)
  (unless (and (= (matrix-rows a) (matrix-rows b)) (= (matrix-cols a) (matrix-cols b)))
    (error "Matrix shapes differ: ~Dx~D and ~Dx~D" (matrix-rows a) (matrix-cols a) (matrix-rows b) (matrix-cols b))))

(defun mat+ (a b) "Elementwise sum of two matrices."
  (%check-same-shape a b)
  (let ((r (matrix-copy a))) (dotimes (k (length (matrix-data r))) (incf (aref (matrix-data r) k) (aref (matrix-data b) k))) r))
(defun mat- (a b) "Elementwise difference of two matrices."
  (%check-same-shape a b)
  (let ((r (matrix-copy a))) (dotimes (k (length (matrix-data r))) (decf (aref (matrix-data r) k) (aref (matrix-data b) k))) r))
(defun mat-scale (a s) "The matrix A multiplied by the number S."
  (let ((r (matrix-copy a))) (dotimes (k (length (matrix-data r))) (setf (aref (matrix-data r) k) (* s (aref (matrix-data r) k)))) r))

(defun mat* (a b)
  "Matrix product A B (inner dimensions must agree)."
  (unless (= (matrix-cols a) (matrix-rows b)) (error "Cannot multiply ~Dx~D by ~Dx~D" (matrix-rows a) (matrix-cols a) (matrix-rows b) (matrix-cols b)))
  (let* ((n (matrix-rows a)) (m (matrix-cols b)) (kk (matrix-cols a)) (r (make-matrix n m))
         (ad (matrix-data a)) (bd (matrix-data b)) (rd (matrix-data r)))
    (dotimes (i n)
      (dotimes (k kk)
        (let ((aik (aref ad (+ (* i kk) k))))
          (unless (zerop aik)
            (let ((bo (* k m)) (ro (* i m)))
              (dotimes (j m) (setf (aref rd (+ ro j)) (+ (aref rd (+ ro j)) (* aik (aref bd (+ bo j)))))))))))
    r))

(defun mat-vec (a v)
  "The product of matrix A and vector V, a vector."
  (let* ((v (coerce v 'vector)) (out (make-array (matrix-rows a) :initial-element 0.0)))
    (dotimes (i (matrix-rows a))
      (let ((s 0.0)) (dotimes (j (matrix-cols a)) (setq s (+ s (* (mref a i j) (aref v j))))) (setf (aref out i) s)))
    out))

(defun matrix-trace (m) "Sum of the diagonal of M." (let ((s 0.0)) (dotimes (i (min (matrix-rows m) (matrix-cols m))) (setq s (+ s (mref m i i)))) s))
(defun matrix-norm (m) "Frobenius norm of M." (sqrt (reduce #'+ (map 'list (lambda (x) (* x x)) (matrix-data m)) :initial-value 0.0)))
(defun matrix-equal (a b &optional (tol 1e-9))
  "True when A and B have the same shape and all elements agree within TOL."
  (and (= (matrix-rows a) (matrix-rows b)) (= (matrix-cols a) (matrix-cols b))
       (every (lambda (x y) (<= (abs (- x y)) tol)) (matrix-data a) (matrix-data b))))

(defmethod print-object ((m matrix) stream)
  (format stream "#<MATRIX ~Dx~D" (matrix-rows m) (matrix-cols m))
  (when (<= (* (matrix-rows m) (matrix-cols m)) 25) (format stream " ~S" (matrix->lists m)))
  (format stream ">"))

(defun matrix-string (m &optional (digits 4))
  "A multi-line, right-aligned rendering of M."
  (let* ((cells (mapcar (lambda (row) (mapcar (lambda (x) (format nil "~,vF" digits x)) row)) (matrix->lists m)))
         (w (reduce #'max (mapcar (lambda (row) (reduce #'max (mapcar #'length row) :initial-value 0)) cells) :initial-value 0)))
    (with-output-to-string (s)
      (dolist (row cells)
        (dolist (c row) (format s " ~A~A" (make-string (- w (length c)) :initial-element #\Space) c))
        (terpri s)))))

;;; ---- vectors ------------------------------------------------------------------------------------------------------

(defun dot (a b) "Dot product of two vectors." (let ((s 0.0) (a (coerce a 'vector)) (b (coerce b 'vector))) (dotimes (i (length a)) (setq s (+ s (* (aref a i) (aref b i))))) s))
(defun vnorm (v) "Euclidean norm of the vector V." (sqrt (dot v v)))
(defun v+ (a b) "Sum of two vectors." (map 'vector #'+ a b))
(defun v- (a b) "Difference of two vectors." (map 'vector #'- a b))
(defun vscale (v s) "Vector V multiplied by S." (map 'vector (lambda (x) (* x s)) v))

;;; ---- LU decomposition and solving -------------------------------------------------------------------------------

(defstruct (lu (:constructor %make-lu (matrix perm sign singular)) (:copier nil)) matrix perm sign singular)

(defun lu-decompose (a)
  "LU decomposition with partial pivoting of the square matrix A; returns an LU object (L and U packed in one matrix)."
  (let* ((n (matrix-rows a)) (m (matrix-copy a)) (perm (make-array n :initial-element 0)) (sign 1) (singular nil))
    (unless (= n (matrix-cols a)) (error "LU decomposition needs a square matrix"))
    (dotimes (i n) (setf (aref perm i) i))
    (dotimes (k n)
      (let ((p k) (best (abs (mref m k k))))
        (loop for i from (+ k 1) below n do (when (> (abs (mref m i k)) best) (setq best (abs (mref m i k)) p i)))
        (when (< best 1e-300) (setq singular t))
        (when (/= p k)
          (dotimes (j n) (let ((tmp (mref m k j))) (setf (mref m k j) (mref m p j)) (setf (mref m p j) tmp)))
          (let ((tmp (aref perm k))) (setf (aref perm k) (aref perm p)) (setf (aref perm p) tmp))
          (setq sign (- sign)))
        (unless (zerop (mref m k k))
          (loop for i from (+ k 1) below n
                do (let ((f (/ (mref m i k) (mref m k k))))
                     (setf (mref m i k) f)
                     (loop for j from (+ k 1) below n do (decf (mref m i j) (* f (mref m k j)))))))))
    (%make-lu m perm sign singular)))

(defun lu-solve (lu b)
  "Solve A x = b for the vector B using the LU object of A."
  (let* ((m (lu-matrix lu)) (n (matrix-rows m)) (b (coerce b 'vector)) (x (make-array n :initial-element 0.0)))
    (when (lu-singular lu) (error "Matrix is singular"))
    (dotimes (i n) (setf (aref x i) (float (aref b (aref (lu-perm lu) i)))))
    (dotimes (i n) (dotimes (j i) (decf (aref x i) (* (mref m i j) (aref x j)))))
    (loop for i from (- n 1) downto 0
          do (loop for j from (+ i 1) below n do (decf (aref x i) (* (mref m i j) (aref x j))))
             (setf (aref x i) (/ (aref x i) (mref m i i))))
    x))

(defun matrix-det (a)
  "Determinant of the square matrix A."
  (let* ((lu (lu-decompose a)) (d (float (lu-sign lu))))
    (dotimes (i (matrix-rows a)) (setq d (* d (mref (lu-matrix lu) i i))))
    d))

(defun matrix-solve (a b)
  "Solve A X = B for X; B is a vector (result a vector) or a matrix (result a matrix)."
  (let ((lu (lu-decompose a)))
    (if (matrix-p b)
        (let ((cols (loop for j from 0 below (matrix-cols b) collect (lu-solve lu (matrix-col b j)))))
          (matrix-from-columns cols))
        (lu-solve lu b))))

(defun matrix-inverse (a)
  "The inverse of the square matrix A."
  (matrix-solve a (identity-matrix (matrix-rows a))))

;;; ---- Householder QR and least squares -----------------------------------------------------------------------------

(defun qr-decompose (a)
  "Householder QR of the m x n matrix A (m >= n); returns (values Q R) with Q m x m orthogonal and R m x n upper triangular."
  (let* ((m (matrix-rows a)) (n (matrix-cols a)) (r (matrix-copy a)) (q (identity-matrix m)))
    (dotimes (k (min (- m 1) n))
      (let ((norm 0.0))
        (loop for i from k below m do (setq norm (+ norm (* (mref r i k) (mref r i k)))))
        (setq norm (sqrt norm))
        (unless (zerop norm)
          (let* ((alpha (if (> (mref r k k) 0) (- norm) norm)) (v (make-array m :initial-element 0.0)))
            (loop for i from k below m do (setf (aref v i) (mref r i k)))
            (decf (aref v k) alpha)
            (let ((vn 0.0)) (loop for i from k below m do (setq vn (+ vn (* (aref v i) (aref v i)))))
              (unless (zerop vn)
                (dotimes (j n)
                  (let ((s 0.0)) (loop for i from k below m do (setq s (+ s (* (aref v i) (mref r i j)))))
                    (let ((f (/ (* 2.0 s) vn))) (loop for i from k below m do (decf (mref r i j) (* f (aref v i)))))))
                (dotimes (i m)
                  (let ((s 0.0)) (loop for j from k below m do (setq s (+ s (* (mref q i j) (aref v j)))))
                    (let ((f (/ (* 2.0 s) vn))) (loop for j from k below m do (decf (mref q i j) (* f (aref v j)))))))))))))
    (dotimes (j n) (loop for i from (+ j 1) below m do (setf (mref r i j) 0.0)))
    (values q r)))

(defun %qr-reduce (a rhs)
  "Householder reduction of the m x n matrix A (m >= n) without forming Q: returns (values R Q^T*RHS) where R is the
n x n upper triangle and the vector is the first n entries of Q^T RHS.  Memory and time are O(m n) and O(m n^2)."
  (let* ((m (matrix-rows a)) (n (matrix-cols a)) (r (matrix-copy a)) (y (when rhs (map 'vector #'float rhs))) (rd (matrix-data r)) (v (make-array m :initial-element 0.0)))
    (dotimes (k (min (- m 1) n))
      (let ((norm 0.0))
        (loop for i from k below m do (let ((x (aref rd (+ (* i n) k)))) (setq norm (+ norm (* x x)))))
        (setq norm (sqrt norm))
        (unless (zerop norm)
          (let ((alpha (if (> (aref rd (+ (* k n) k)) 0) (- norm) norm)) (vn 0.0))
            (loop for i from k below m do (setf (aref v i) (aref rd (+ (* i n) k))))
            (decf (aref v k) alpha)
            (loop for i from k below m do (setq vn (+ vn (* (aref v i) (aref v i)))))
            (unless (zerop vn)
              (loop for j from k below n
                    do (let ((sum 0.0)) (loop for i from k below m do (setq sum (+ sum (* (aref v i) (aref rd (+ (* i n) j))))))
                         (let ((f (/ (* 2.0 sum) vn))) (loop for i from k below m do (decf (aref rd (+ (* i n) j)) (* f (aref v i)))))))
              (when y
                (let ((sum 0.0)) (loop for i from k below m do (setq sum (+ sum (* (aref v i) (aref y i)))))
                  (let ((f (/ (* 2.0 sum) vn))) (loop for i from k below m do (decf (aref y i) (* f (aref v i))))))))))))
    (let ((top (make-matrix n n)))
      (dotimes (i n) (loop for j from i below n do (setf (mref top i j) (aref rd (+ (* i n) j)))))
      (values top y))))

(defun least-squares (a b)
  "The vector x minimising |A x - b| (A is m x n, m >= n) using Householder QR (Q is never formed)."
  (multiple-value-bind (r qtb) (%qr-reduce a b)
    (let* ((n (matrix-cols a)) (x (make-array n :initial-element 0.0)))
      (loop for i from (- n 1) downto 0
            do (let ((s (aref qtb i)))
                 (loop for j from (+ i 1) below n do (decf s (* (mref r i j) (aref x j))))
                 (when (< (abs (mref r i i)) 1e-300) (error "Matrix is rank deficient"))
                 (setf (aref x i) (/ s (mref r i i)))))
      x)))

;;; ---- Cholesky ----------------------------------------------------------------------------------------------------

(defun cholesky (a)
  "Lower-triangular L with A = L L^T for the symmetric positive-definite matrix A."
  (let* ((n (matrix-rows a)) (l (make-matrix n n)))
    (dotimes (i n)
      (dotimes (j (+ i 1))
        (let ((s (mref a i j)))
          (dotimes (k j) (decf s (* (mref l i k) (mref l j k))))
          (if (= i j)
              (progn (when (<= s 0.0) (error "Matrix is not positive definite")) (setf (mref l i i) (sqrt s)))
              (setf (mref l i j) (/ s (mref l j j)))))))
    l))

(defun cholesky-solve (l b)
  "Solve A x = b given L = (cholesky A)."
  (let* ((n (matrix-rows l)) (y (make-array n :initial-element 0.0)) (x (make-array n :initial-element 0.0)) (b (coerce b 'vector)))
    (dotimes (i n) (let ((s (float (aref b i)))) (dotimes (k i) (decf s (* (mref l i k) (aref y k)))) (setf (aref y i) (/ s (mref l i i)))))
    (loop for i from (- n 1) downto 0
          do (let ((s (aref y i))) (loop for k from (+ i 1) below n do (decf s (* (mref l k i) (aref x k)))) (setf (aref x i) (/ s (mref l i i)))))
    x))

;;; ---- symmetric eigenproblem (Jacobi) ----------------------------------------------------------------------------

(defun eigen-symmetric (a)
  "Eigen-decomposition of the symmetric matrix A by cyclic Jacobi rotations.
Returns (values eigenvalues eigenvectors): a vector sorted in decreasing order and a matrix whose columns are the vectors."
  (let* ((n (matrix-rows a)) (m (matrix-copy a)) (v (identity-matrix n)))
    (dotimes (sweep 100)
      (let ((off 0.0))
        (dotimes (i n) (dotimes (j i) (setq off (+ off (* (mref m i j) (mref m i j))))))
        (when (< off 1e-30) (return))
        (dotimes (p n)
          (loop for q from (+ p 1) below n
                do (let ((apq (mref m p q)))
                     (when (> (abs apq) 1e-300)
                       (let* ((theta (/ (- (mref m q q) (mref m p p)) (* 2.0 apq)))
                              (tt (/ (if (>= theta 0) 1.0 -1.0) (+ (abs theta) (sqrt (+ (* theta theta) 1.0)))))
                              (c (/ 1.0 (sqrt (+ (* tt tt) 1.0)))) (s (* tt c)))
                         (dotimes (k n)
                           (let ((akp (mref m k p)) (akq (mref m k q)))
                             (setf (mref m k p) (- (* c akp) (* s akq))) (setf (mref m k q) (+ (* s akp) (* c akq)))))
                         (dotimes (k n)
                           (let ((apk (mref m p k)) (aqk (mref m q k)))
                             (setf (mref m p k) (- (* c apk) (* s aqk))) (setf (mref m q k) (+ (* s apk) (* c aqk)))))
                         (dotimes (k n)
                           (let ((vkp (mref v k p)) (vkq (mref v k q)))
                             (setf (mref v k p) (- (* c vkp) (* s vkq))) (setf (mref v k q) (+ (* s vkp) (* c vkq))))))))))))
    (let* ((order (sort (loop for i from 0 below n collect i) (lambda (i j) (> (mref m i i) (mref m j j)))))
           (vals (make-array n :initial-element 0.0)) (vecs (make-matrix n n)) (c 0))
      (dolist (i order)
        (setf (aref vals c) (mref m i i))
        (dotimes (k n) (setf (mref vecs k c) (mref v k i)))
        (setq c (+ c 1)))
      (values vals vecs))))

;;; ---- SVD (one-sided Jacobi) ---------------------------------------------------------------------------------------

(defun svd (a)
  "Singular value decomposition of the m x n matrix A.  Returns (values U S V) with A = U diag(S) V^T,
S a vector in decreasing order, U m x n and V n x n.  (For m < n the transpose is decomposed and swapped.)
One-sided Jacobi on column vectors: each rotation touches two contiguous vectors."
  (if (< (matrix-rows a) (matrix-cols a))
      (multiple-value-bind (u s v) (svd (matrix-transpose a)) (values v s u))
      (let* ((m (matrix-rows a)) (n (matrix-cols a)) (u (make-array n :initial-element nil)) (v (make-array n :initial-element nil)))
        (dotimes (j n)
          (setf (aref u j) (matrix-col a j))
          (let ((e (make-array n :initial-element 0.0))) (setf (aref e j) 1.0) (setf (aref v j) e)))
        (dotimes (sweep 60)
          (let ((rotated nil))
            (dotimes (p n)
              (loop for q from (+ p 1) below n
                    do (let ((up (aref u p)) (uq (aref u q)) (alpha 0.0) (beta 0.0) (gamma 0.0))
                         (dotimes (i m) (let ((x (aref up i)) (y (aref uq i))) (setq alpha (+ alpha (* x x)) beta (+ beta (* y y)) gamma (+ gamma (* x y)))))
                         (when (> (abs gamma) (* 1e-15 (sqrt (* alpha beta))))
                           (setq rotated t)
                           (let* ((zeta (/ (- beta alpha) (* 2.0 gamma)))
                                  (tt (/ (if (>= zeta 0) 1.0 -1.0) (+ (abs zeta) (sqrt (+ 1.0 (* zeta zeta))))))
                                  (c (/ 1.0 (sqrt (+ 1.0 (* tt tt))))) (s (* c tt)) (vp (aref v p)) (vq (aref v q)))
                             (dotimes (i m) (let ((x (aref up i)) (y (aref uq i))) (setf (aref up i) (- (* c x) (* s y))) (setf (aref uq i) (+ (* s x) (* c y)))))
                             (dotimes (i n) (let ((x (aref vp i)) (y (aref vq i))) (setf (aref vp i) (- (* c x) (* s y))) (setf (aref vq i) (+ (* s x) (* c y))))))))))
            (unless rotated (return))))
        (let ((sv (make-array n :initial-element 0.0)))
          (dotimes (j n) (let ((col (aref u j)) (sum 0.0)) (dotimes (i m) (setq sum (+ sum (* (aref col i) (aref col i))))) (setf (aref sv j) (sqrt sum))))
          (let* ((order (sort (loop for i from 0 below n collect i) (lambda (i j) (> (aref sv i) (aref sv j)))))
                 (s (make-array n :initial-element 0.0)) (ucols nil) (vcols nil) (c 0))
            (dolist (j order)
              (setf (aref s c) (aref sv j))
              (push (let ((col (aref u j)) (nv (aref sv j))) (map 'vector (lambda (x) (if (> nv 1e-300) (/ x nv) 0.0)) col)) ucols)
              (push (aref v j) vcols)
              (setq c (+ c 1)))
            (values (matrix-from-columns (reverse ucols)) s (matrix-from-columns (reverse vcols))))))))

(defun matrix-rank (a &optional (tol nil))
  "Numerical rank of A: the number of singular values above TOL (default eps * max(m,n) * largest)."
  (multiple-value-bind (u s v) (svd a)
    (declare (ignore u v))
    (let ((tol (or tol (* +double-epsilon+ (max (matrix-rows a) (matrix-cols a)) (if (> (length s) 0) (aref s 0) 0.0)))))
      (count-if (lambda (x) (> x tol)) s))))

(defun pseudo-inverse (a)
  "The Moore-Penrose pseudo-inverse of A."
  (multiple-value-bind (u s v) (svd a)
    (let* ((tol (* +double-epsilon+ (max (matrix-rows a) (matrix-cols a)) (if (> (length s) 0) (aref s 0) 0.0)))
           (sinv (map 'vector (lambda (x) (if (> x tol) (/ 1.0 x) 0.0)) s)))
      (mat* (mat* v (diagonal-matrix sinv)) (matrix-transpose u)))))

(defun condition-number (a)
  "Ratio of the largest to the smallest singular value of A."
  (multiple-value-bind (u s v) (svd a)
    (declare (ignore u v))
    (let ((smin (aref s (- (length s) 1)))) (if (< smin 1e-300) +infinity+ (/ (aref s 0) smin)))))

;;; ---- general eigenvalues (Hessenberg + shifted QR) ------------------------------------------------------

(defun %hessenberg (a)
  (let* ((n (matrix-rows a)) (h (matrix-copy a)))
    (loop for k from 1 below (- n 1)
          do (let ((alpha 0.0))
               (loop for i from k below n do (setq alpha (+ alpha (* (mref h i (- k 1)) (mref h i (- k 1))))))
               (setq alpha (sqrt alpha))
               (unless (< alpha 1e-300)
                 (when (> (mref h k (- k 1)) 0) (setq alpha (- alpha)))
                 (let ((v (make-array n :initial-element 0.0)) (vn 0.0))
                   (loop for i from k below n do (setf (aref v i) (mref h i (- k 1))))
                   (decf (aref v k) alpha)
                   (loop for i from k below n do (setq vn (+ vn (* (aref v i) (aref v i)))))
                   (unless (zerop vn)
                     (dotimes (j n) (let ((s 0.0)) (loop for i from k below n do (setq s (+ s (* (aref v i) (mref h i j))))) (let ((f (/ (* 2.0 s) vn))) (loop for i from k below n do (decf (mref h i j) (* f (aref v i)))))))
                     (dotimes (i n) (let ((s 0.0)) (loop for j from k below n do (setq s (+ s (* (mref h i j) (aref v j))))) (let ((f (/ (* 2.0 s) vn))) (loop for j from k below n do (decf (mref h i j) (* f (aref v j))))))))))))
    h))

(defun eigenvalues (a)
  "All eigenvalues of the square matrix A as a list (real numbers, or complex numbers for complex pairs),
by Hessenberg reduction and shifted QR iteration."
  (let* ((n (matrix-rows a)) (h (%hessenberg a)) (hi (- n 1)) (out nil) (iter 0))
    (loop while (>= hi 0)
          do (cond
               ((= hi 0) (setq out (cons (mref h 0 0) out) hi -1))
               (t (let ((l hi))
                    (loop while (and (> l 0) (> (abs (mref h l (- l 1))) (* 1e-14 (+ (abs (mref h l l)) (abs (mref h (- l 1) (- l 1))))))) do (setq l (- l 1)))
                    (cond
                      ((= l hi) (setq out (cons (mref h hi hi) out) hi (- hi 1) iter 0))
                      ((= l (- hi 1))
                       (let* ((a11 (mref h (- hi 1) (- hi 1))) (a12 (mref h (- hi 1) hi)) (a21 (mref h hi (- hi 1))) (a22 (mref h hi hi))
                              (tr (+ a11 a22)) (det (- (* a11 a22) (* a12 a21))) (disc (- (* tr tr 0.25) det)))
                         (if (>= disc 0)
                             (let ((root (sqrt disc))) (setq out (cons (+ (* 0.5 tr) root) (cons (- (* 0.5 tr) root) out))))
                             (let ((root (sqrt (- disc)))) (setq out (cons (complex (* 0.5 tr) root) (cons (complex (* 0.5 tr) (- root)) out)))))
                         (setq hi (- hi 2) iter 0)))
                      (t
                       (setq iter (+ iter 1))
                       (when (> iter 2000) (error "Eigenvalue iteration did not converge"))
                       ;; shifted QR step on rows/cols l..hi using Givens rotations
                       (let* ((mu (if (zerop (mod iter 11)) (+ (mref h hi hi) (abs (mref h hi (- hi 1)))) (mref h hi hi)))
                              (cs nil))
                         (loop for i from l to hi do (decf (mref h i i) mu))
                         (loop for k from l below hi
                               do (let* ((x (mref h k k)) (y (mref h (+ k 1) k)) (r (sqrt (+ (* x x) (* y y)))))
                                    (let ((c (if (zerop r) 1.0 (/ x r))) (s (if (zerop r) 0.0 (/ y r))))
                                      (setq cs (cons (list k c s) cs))
                                      (loop for j from k to hi
                                            do (let ((t1 (mref h k j)) (t2 (mref h (+ k 1) j)))
                                                 (setf (mref h k j) (+ (* c t1) (* s t2))) (setf (mref h (+ k 1) j) (- (* c t2) (* s t1))))))))
                         (dolist (g (reverse cs))
                           (let ((k (first g)) (c (second g)) (s (third g)))
                             (loop for i from l to (min hi (+ k 2))
                                   do (let ((t1 (mref h i k)) (t2 (mref h i (+ k 1))))
                                        (setf (mref h i k) (+ (* c t1) (* s t2))) (setf (mref h i (+ k 1)) (- (* c t2) (* s t1)))))))
                         (loop for i from l to hi do (incf (mref h i i) mu)))))))))
    (sort out (lambda (x y) (let ((rx (realpart x)) (ry (realpart y))) (or (< rx ry) (and (= rx ry) (< (imagpart x) (imagpart y)))))))))
