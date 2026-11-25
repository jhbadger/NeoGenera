;;;; soak.lisp -- a long-running mixed workload that watches the heap.
;;;;
;;;; Usage:  NG_SOAK_SECONDS=600 build/neogenera -i build/neogenera.img --no-toplevel --load tests/soak/soak.lisp
;;;; (default 20 seconds).  Each round allocates and discards lists, strings, hash tables, bignums, structures, CLOS
;;;; instances, conditions, parsed files, alignments, trees and subprocess output.  After every round the live heap
;;;; is measured (after a forced collection); the run fails if it grows steadily instead of levelling off, or if any
;;;; round signals an unexpected error.

(in-package :cl)

(defstruct soak-point x y)
(defclass soak-animal () ((name :initarg :name :accessor soak-name)))
(defclass soak-dog (soak-animal) ())
(defgeneric soak-speak (a))
(defmethod soak-speak ((a soak-animal)) (soak-name a))
(defmethod soak-speak ((a soak-dog)) (%string-concat "dog " (call-next-method)))

(defun soak-round (rng k)
  (let ((junk nil))
    ;; lists, strings, vectors, hash tables
    (dotimes (i 2000) (push (list i (format nil "item ~D" i) (make-soak-point :x i :y (* i i))) junk))
    (let ((h (make-hash-table :test 'equal))) (dotimes (i 3000) (setf (gethash (format nil "k~D" i) h) i)) (setq junk (cons h junk)))
    (setq junk (cons (sort (loop repeat 3000 collect (rand-int 100000 rng)) #'<) junk))
    ;; numbers
    (let ((n 1)) (loop for i from 1 to 400 do (setq n (* n i))) (setq junk (cons (* n n) junk)))
    (setq junk (cons (expt 7 5000) junk))
    ;; CLOS, conditions, dynamic extent
    (dotimes (i 300) (soak-speak (make-instance (if (evenp i) 'soak-dog 'soak-animal) :name "rex")))
    (dotimes (i 200) (handler-case (error "soak ~D" i) (error (e) (princ-to-string e))))
    (dotimes (i 100) (catch 'out (throw 'out i)) (block b (return-from b i)) (unwind-protect (+ i 1) (+ i 2)))
    ;; text processing and formats
    (let* ((dna (random-dna 3000 :rng rng)) (a (random-dna 120 :rng rng)) (b (mutate-dna a 0.1 :rng rng)))
      (setq junk (cons (list (reverse-complement dna) (translate dna) (kmer-counts dna 6) (find-orfs dna :min-length 20 :both-strands t) (align a b) (align a b :mode :local)) junk))
      (setq junk (cons (parse-fasta (split-lines (fasta-string (list (make-bioseq dna :id "s"))))) junk))
      (setq junk (cons (suffix-index-search (build-suffix-index dna) "ACG") junk))
      (setq junk (cons (tree-from-sequences (loop for j from 0 below 4 collect (make-bioseq (mutate-dna a 0.05 :rng rng) :id (format nil "t~D" j)))) junk)))
    (setq junk (cons (parse-delimited (format nil "a,b,c~%1,2,\"x,y\"~%") #\,) junk))
    (setq junk (cons (regex-find-all "A[CG]+T" (random-dna 500 :rng rng)) junk))
    (setq junk (cons (read-from-string "(a (b c) \"str\" #(1 2) 1/3 4.5)") junk))
    ;; statistics and linear algebra
    (let* ((x (loop repeat 100 collect (rand-normal 0.0 1.0 rng))) (y (mapcar (lambda (v) (+ 1.0 (* 2.0 v) (rand-normal 0.0 0.1 rng))) x)))
      (setq junk (cons (list (lm y x) (t-test x :y y) (matrix-inverse (mat '((4 3) (3 5)))) (svd (mat '((1 2) (3 4) (5 6))))) junk)))
    ;; occasionally: a subprocess, a file, documentation search, the compiler
    (when (zerop (mod k 5)) (run-program "echo" (list (format nil "round ~D" k))))
    (when (zerop (mod k 7)) (write-file-lines "/tmp/ng-soak.txt" (list "line one" "line two")) (read-file-lines "/tmp/ng-soak.txt") (delete-file "/tmp/ng-soak.txt"))
    (when (zerop (mod k 3)) (search-documentation "alignment of sequences"))
    (when (zerop (mod k 4)) (eval `(defun soak-fn (x) (let ((y (* x ,k))) (if (> y 5) (+ y 1) (- y 1))))) (soak-fn 3))
    (length junk)))

(let* ((seconds (let ((e (getenv "NG_SOAK_SECONDS"))) (if e (parse-integer e) 20))) (rng (make-rng 99))
       (start (get-internal-real-time)) (limit (* seconds internal-time-units-per-second)) (rounds 0) (samples nil) (errors 0))
  (loop while (< (- (get-internal-real-time) start) limit)
        do (handler-case (soak-round rng rounds) (error (e) (incf errors) (format t "round ~D failed: ~A~%" rounds e)))
           (incf rounds)
           (when (zerop (mod rounds 10)) (gc) (push (%heap-used) samples)))
  (let* ((s (reverse samples)) (n (length s)) (third (max 1 (floor n 3)))
         (early (mean (mapcar #'float (subseq s 0 third)))) (late (mean (mapcar #'float (subseq s (- n third))))))
    (format t "soak: ~D rounds in ~D s, ~D collections, ~D errors~%live heap early ~,1F MB, late ~,1F MB (ratio ~,2F)~%"
            rounds seconds (%gc-count) errors (/ early 1048576.0) (/ late 1048576.0) (/ late early))
    (finish-output)
    (exit (if (and (zerop errors) (>= n 6) (< (/ late early) 1.25)) 0 1))))
