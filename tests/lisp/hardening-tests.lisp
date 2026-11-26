;;;; hardening-tests.lisp -- image integrity, crash-safe saves, subprocess I/O, allocation limits, string kernels.

(in-package :cl)

(defvar *ng-binary* (or (getenv "NG_BIN") "build/neogenera"))
(defvar *ng-image* (or (getenv "NG_IMAGE") "build/neogenera.img"))

(defun ng-run (args) (multiple-value-list (run-program *ng-binary* args)))

(deftest image-integrity-and-safe-save
  (let ((good "/tmp/ng-hard-good.img") (flip "/tmp/ng-hard-flip.img") (cut "/tmp/ng-hard-cut.img") (bogus "/tmp/ng-hard-bogus.img"))
    ;; save an image with a marker, reload it, and check no temporary file is left behind
    (let ((r (ng-run (list "-i" *ng-image* "--no-toplevel" "--eval" "(defvar *hard-marker* 4242)" "--save-image" good))))
      (is-equal 0 (second (first (list r))) ))
    (is (probe-file good))
    (let ((r (ng-run (list "-i" good "--no-toplevel" "--eval" "(princ *hard-marker*)"))))
      (is (%string-search "4242" (first r) 0)) (is-equal 0 (second r)))
    (is-equal "" (string-trim (list #\Newline) (tool-result-output (run-tool "sh" (list "-c" "ls /tmp/ng-hard-good.img.tmp.* 2>/dev/null; true"))))))
  ;; a flipped byte anywhere in the payload is detected; so is truncation and a file that is not an image
  (run-tool "sh" (list "-c" "head -c 1000000 /tmp/ng-hard-good.img > /tmp/ng-hard-flip.img; printf X >> /tmp/ng-hard-flip.img; tail -c +1000002 /tmp/ng-hard-good.img >> /tmp/ng-hard-flip.img"))
  (run-tool "sh" (list "-c" "head -c 3000000 /tmp/ng-hard-good.img > /tmp/ng-hard-cut.img"))
  (run-tool "sh" (list "-c" "echo not an image > /tmp/ng-hard-bogus.img"))
  (let ((r (ng-run (list "-i" "/tmp/ng-hard-flip.img" "--no-toplevel" "--eval" "(princ 1)")))) (is-equal 2 (second r)) (is (%string-search "cannot load image" (first r) 0)) (is (%string-search "code -4" (first r) 0)))
  (let ((r (ng-run (list "-i" "/tmp/ng-hard-cut.img" "--no-toplevel" "--eval" "(princ 1)")))) (is-equal 2 (second r)) (is (%string-search "code -2" (first r) 0)))
  (let ((r (ng-run (list "-i" "/tmp/ng-hard-bogus.img" "--no-toplevel" "--eval" "(princ 1)")))) (is-equal 2 (second r)))
  (let ((r (ng-run (list "-i" "/tmp/ng-hard-good.img" "--no-toplevel" "--eval" "(princ 1)")))) (is-equal 0 (second r)))
  ;; a failed save (unwritable destination) leaves an existing image intact and reports failure
  (let ((before (tool-result-output (run-tool "sh" (list "-c" "cksum < /tmp/ng-hard-good.img")))))
    (let ((r (ng-run (list "-i" *ng-image* "--no-toplevel" "--save-image" "/tmp/no-such-dir-xyz/ng.img")))) (is (/= 0 (second r))))
    (is-equal before (tool-result-output (run-tool "sh" (list "-c" "cksum < /tmp/ng-hard-good.img")))))
  (dolist (f '("/tmp/ng-hard-good.img" "/tmp/ng-hard-flip.img" "/tmp/ng-hard-cut.img" "/tmp/ng-hard-bogus.img")) (when (probe-file f) (delete-file f))))

(deftest subprocess-io
  ;; large input with large output, a child that never reads stdin, and one that exits early: none may block
  (let ((big (make-string 2500000 #\a)))
    (multiple-value-bind (out status) (run-program "cat" nil big) (is-equal 2500000 (length out)) (is-equal 0 status))
    (multiple-value-bind (out status) (run-program "sh" '("-c" "head -c 1500000 /dev/zero | tr '\\0' x") big) (is-equal 1500000 (length out)) (is-equal 0 status))
    (multiple-value-bind (out status) (run-program "true" nil big) (is-equal 0 (length out)) (is-equal 0 status)))
  (multiple-value-bind (out status) (run-program "sh" '("-c" "echo oops >&2; exit 4")) (is (%string-search "oops" out 0)) (is-equal 4 status))
  (multiple-value-bind (out status) (run-program "/no/such/program" nil) (declare (ignore out)) (is-equal 127 status))
  (multiple-value-bind (out status) (run-program "echo" '("a b" "c;d" "$HOME" "*")) (is-equal (format nil "a b c;d $HOME *~%") out) (is-equal 0 status)))

(deftest allocation-limits
  (is (%string-search "Heap exhausted" (handler-case (make-string 4000000000) (error (e) (princ-to-string e))) 0))
  (is (%string-search "Heap exhausted" (handler-case (make-array 900000000) (error (e) (princ-to-string e))) 0))
  (is (stringp (handler-case (make-string -1) (error (e) (princ-to-string e)))))
  (is (%string-search "Heap exhausted" (handler-case (%string-concat (make-string 300000000) (make-string 300000000) (make-string 300000000)) (error (e) (princ-to-string e))) 0))
  ;; after an allocation error the system is still usable
  (is-equal 6 (length (make-string 6 #\x))) (is-equal 3 (length (make-list 3))))

(deftest string-kernels
  (is-equal "TGCA" (%string-translate "ACGT" "ACGT" "TGCA")) (is-equal "xbc" (%string-translate "abc" "a" "x")) (is-equal "" (%string-translate "" "a" "b")) (is-equal "abc" (%string-translate "abc" "" ""))
  (is-equal "xb" (%string-translate "ab" "aa" "xy")) ; the first pair wins for a repeated source character
  (signals-error (%string-translate "a" "ab" "x"))
  (is-equal "cba" (%string-reverse "abc")) (is-equal "" (%string-reverse "")) (is-equal "a" (%string-reverse "a"))
  (is (%string-all-in "ACGT" "ACGTN")) (is (not (%string-all-in "ACGU" "ACGTN"))) (is (%string-all-in "" "A"))
  (is-equal 2 (%count-chars "AACGT" "A" 0 5)) (is-equal 1 (%count-chars "AACGT" "A" 1 2)) (is-equal 2 (%count-chars "AACGT" "GC" 0 5)) (is-equal 0 (%count-chars "AACGT" "Z" 0 5))
  (signals-error (%count-chars "AAC" "A" 0 9)) (signals-error (%count-chars "AAC" "A" 2 1))
  (let ((std (genetic-code-aminos (find-genetic-code 1))))
    (is-equal "MAIVMGR*KGAR*" (%translate-codons "ATGGCCATTGTAATGGGCCGCTGAAAGGGTGCCCGATAG" 0 std nil))
    (is-equal "MAIVMGR" (%translate-codons "ATGGCCATTGTAATGGGCCGCTGAAAGGGTGCCCGATAG" 0 std t))
    (is-equal "KX" (%translate-codons "AAANNN" 0 std nil)) (is-equal "" (%translate-codons "AC" 0 std nil)) (is-equal "M" (%translate-codons "xxATG" 2 std nil)) (is-equal "M" (%translate-codons "augu" 0 std nil))
    (signals-error (%translate-codons "ATG" 0 "short" nil))
    (is-equal '((0 . 9)) (%orf-scan "ATGAAATTTTAGCC" 0 std (%start-flags nil '("ATG")) 3))
    (is-equal nil (%orf-scan "ATGAAATTTTAGCC" 0 std (%start-flags nil '("ATG")) 5))
    (is-equal '((0 . 6) (9 . 15)) (%orf-scan "ATGAAATAAATGCCCTGA" 0 std (%start-flags nil '("ATG")) 1))
    (is-equal nil (%orf-scan "ATGAAA" 0 std (%start-flags nil '("ATG")) 0))))

(deftest search-index
  (is (find :editor-manual (mapcar #'second (search-documentation "undo editor")))) (is-equal nil (search-documentation "qzqzqz")) (is-equal nil (search-documentation ""))
  (is (<= (length (search-documentation "function" 7)) 7))
  ;; prefix words are found: "inspect" reaches the Inspector, "align" reaches alignment functions
  (is (find 'align (mapcar #'second (search-documentation "align")))) (is (find :bio-manual (mapcar #'second (search-documentation "aligned sequences"))))
  (define-document :hard-doc (:title "Quokka Taxonomy") (:p "Marsupial of Rottnest."))
  (is-equal :hard-doc (second (car (search-documentation "quokka")))) (is-equal :hard-doc (second (car (search-documentation "quok"))))
  (is (< (length (search-documentation "document")) 31)))

(deftest core-regressions
  ;; found by the example gallery, the fuzzers and the benchmarks
  (is-equal '(1 a 2 a 3 a) (loop for i in '(1 2 3) collect i collect 'a))
  (is-equal '(1 2 10 20) (loop for i in '(1 2) collect i into xs append (list (* i 10)) into ys finally (return (append xs ys))))
  (is-equal '(1 :b 2 :b) (loop for i in '(1 2) collect i collect :b))
  (is-equal "3.594e-12" (format nil "~,3E" 3.594004038909079e-12)) (is-equal "1.23e-4" (format nil "~,2E" 0.000123)) (is-equal "1.000e+1" (format nil "~,3E" 9.9996)) (is-equal "-2.5e+0" (format nil "~,1E" -2.5))
  (is-equal "0.00e+0" (format nil "~,2E" 0.0)) (is-equal "5.0e-8" (format nil "~E" 5.0e-8)) (is-equal "x:y" (format nil "~@[~A:~]~A" "x" "y")) (is-equal "y" (format nil "~@[~A:~]~A" nil "y"))
  (is-equal 'fixnum (type-of 1)) (is-equal 'null (type-of nil)) (is-equal 'string (type-of "s")) (is-equal 'float (type-of 1.5)) (is-equal 'cons (type-of '(1))) (is-equal 'symbol (type-of 'a))
  (is (%string-search "ret" (with-output-to-string (*standard-output*) (disassemble 'car-ish-test-fn)) 0))
  (is-equal "aa" (make-string 2 :initial-element #\a)))

(defun car-ish-test-fn (x) (car x))

(defun %exhausted-p (thunk) (handler-case (progn (funcall thunk) nil) (error (e) (and (%string-search "exhausted" (princ-to-string e) 0) t))))

(deftest stack-safety
  ;; runaway recursion is an ordinary, recoverable error -- interpreted or compiled, repeated -- never a crash
  (is (%exhausted-p (lambda () (labels ((f (n) (+ 1 (f (+ n 1))))) (f 0)))))
  (is (%exhausted-p (lambda () (labels ((a (n) (b (+ n 1))) (b (n) (+ 1 (a n)))) (a 0)))))
  (is (%exhausted-p (funcall (compile nil '(lambda () (lambda () (labels ((f (n) (+ 1 (f (+ n 1))))) (f 0))))))))
  (is (%exhausted-p (lambda () (labels ((f (n) (cons n (f (+ n 1))))) (f 0)))))
  (is-equal 3 (+ 1 2))
  (dotimes (i 3) (is (%exhausted-p (lambda () (labels ((f (n) (+ 1 (f (+ n 1))))) (f 0))))))
  (is-equal 10000 (labels ((g (n) (if (= n 0) 0 (+ 1 (g (- n 1)))))) (g 10000)))
  ;; very deeply nested data: reading, printing, comparing and copying either work or signal an error
  (flet ((nest (n) (concatenate 'string (make-string n #\() "1" (make-string n #\))))
         (build (n) (let ((x 1)) (dotimes (i n x) (setq x (list x))))))
    (is (handler-case (progn (read-from-string (nest 100000)) t) (error () t)))
    (is (stringp (prin1-to-string (build 100000))))
    (is (handler-case (equal (build 300000) (build 300000)) (error () t)))
    (is (handler-case (progn (copy-tree (build 100000)) t) (error () t))))
  (is-equal 4 (+ 2 2)))
