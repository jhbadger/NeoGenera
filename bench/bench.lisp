;;;; bench.lisp -- repeatable workloads for the performance pass.  Run with `make bench`.
;;;; Each line reports wall-clock seconds; results vary with the machine, ratios between runs on one machine matter.

(in-package :cl)

(defvar *bench-results* nil)
(defun now-seconds () (/ (float (get-internal-real-time)) internal-time-units-per-second))
(defmacro bench (name &body body)
  `(let ((t0 (now-seconds)))
     ,@body
     (let ((dt (- (now-seconds) t0)))
       (push (cons ,name dt) *bench-results*)
       (format t "~28A ~8,3F s~%" ,name dt) (finish-output))))

(defun fib (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(defun tak (x y z) (if (not (< y x)) z (tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y))))

;; the same loops with and without declarations (the typed ones at safety 0, where declarations are trusted).
;; --load runs the stage-0 evaluator; EVAL is the compiler, which is what declarations are for.
(eval '(progn
(defun bench-sum (n) (let ((s 0)) (dotimes (i n) (setq s (+ s (logand (* i 3) 255)))) s))
(defun bench-sum-fx (n) (declare (fixnum n) (optimize (safety 0)))
  (let ((s 0)) (declare (fixnum s))
    (dotimes (i n) (declare (fixnum i)) (setq s (the fixnum (+ s (logand (the fixnum (* i 3)) 255))))) s))
(defun bench-dot (v w n) (let ((s 0.0)) (dotimes (i n) (setq s (+ s (* (svref v i) (svref w i))))) s))
(defun bench-dot-fl (v w n) (declare (simple-vector v w) (fixnum n) (optimize (safety 0)))
  (let ((s 0.0d0)) (declare (double-float s))
    (dotimes (i n) (declare (fixnum i)) (setq s (+ s (* (the double-float (svref v i)) (the double-float (svref w i)))))) s))))

(rng-seed 1)
(let ((genome (random-dna 300000)) (dna1m (random-dna 1000000)) (a (random-dna 1500)) (b (random-dna 1500)))
  (bench "fib 27" (fib 27))
  (bench "tak 18 12 6 x20" (dotimes (i 20) (tak 18 12 6)))
  (bench "fixnum loop 3M, no declarations" (bench-sum 3000000))
  (bench "fixnum loop 3M, fixnum safety 0" (bench-sum-fx 3000000))
  (let ((v (make-array 200000 :initial-element 1.5d0)) (w (make-array 200000 :initial-element 2.0d0)))
    (bench "dot product 20x200k, untyped" (dotimes (k 20) (bench-dot v w 200000)))
    (bench "dot product 20x200k, double-float" (dotimes (k 20) (bench-dot-fl v w 200000))))
  (bench "factorial 3000" (let ((f 1)) (loop for i from 1 to 3000 do (setq f (* f i)))))
  (bench "bignum multiply 1M bits" (let ((x (expt 3 600000)) (y (expt 7 400000))) (* x y)))
  (bench "hash table 200k" (let ((h (make-hash-table :test 'equal))) (dotimes (i 200000) (setf (gethash i h) i)) (dotimes (i 200000) (gethash i h))))
  (bench "sort 200k" (sort (let ((l nil)) (dotimes (i 200000) (push (rand-int 1000000) l)) l) #'<))
  (bench "string concat/format 20k" (dotimes (i 20000) (format nil "~D:~A:~,2F" i "abc" 3.14159)))
  (bench "matrix multiply 120x120" (let ((m (make-matrix 120 120 1.5))) (mat* m m)))
  (bench "lm 3000 x 4" (let ((y (loop repeat 3000 collect (rand-normal))) (x (loop repeat 4 collect (loop repeat 3000 collect (rand-normal))))) (lm y x)))
  (bench "svd 60x60" (svd (let ((m (make-matrix 60 60))) (dotimes (i 3600) (setf (aref (matrix-data m) i) (rand-normal))) m)))
  (bench "reverse-complement 1 Mbp" (reverse-complement dna1m))
  (bench "translate 1 Mbp" (translate dna1m))
  (bench "gc-content 1 Mbp" (gc-content dna1m))
  (bench "kmer-counts 300k k=11" (kmer-counts genome 11))
  (bench "find-orfs 300 kbp both" (find-orfs genome :min-length 60 :both-strands t))
  (bench "align 1500x1500 (C)" (align a b))
  (bench "align local 1500x1500 (C)" (align a b :mode :local))
  (bench "suffix array 1 Mbp (C)" (%suffix-array dna1m))
  (bench "pack-dna 300 kbp" (pack-dna genome))
  (bench "fasta write+read 1 Mbp" (progn (write-fasta (list (make-bioseq dna1m :id "g")) "/tmp/ng-bench.fa") (read-fasta "/tmp/ng-bench.fa") (delete-file "/tmp/ng-bench.fa")))
  (bench "documentation search x200" (dotimes (i 200) (search-documentation "alignment matrix")))
  (bench "compile+load 200 defuns" (dotimes (i 200) (eval `(defun ,(intern (format nil "BENCH-FN-~D" i)) (x) (let ((y (* x ,i))) (if (> y 10) (+ y 1) (- y 1))))))))

(format t "~%total ~,2F s~%" (reduce #'+ (mapcar #'cdr *bench-results*)))
