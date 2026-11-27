;;;; specialize-tests.lisp -- declarations, OPTIMIZE policy, type specialisation and the binary fasl format.

(in-package :cl)

(defun st-ops (fn)
  "The names of the instructions of the compiled function FN, in order."
  (let ((instrs (nth 4 (%code-parts fn))) (acc nil))
    (do () ((null instrs))
      (let ((e (find (car instrs) *opcodes* :key #'cadr)))
        (push (car e) acc)
        (setq instrs (nthcdr (+ 1 (caddr e)) instrs))))
    (nreverse acc)))

(defun st-has (fn op) (if (member op (st-ops fn)) t nil))
(defun st-comp (form) (compile nil form))

(defvar *st-rng* (make-rng 424242))
(defun st-rand-fix (bits) (- (rand-int (ash 1 (+ bits 1)) *st-rng*) (ash 1 bits)))

;;; ---- the type lattice ---------------------------------------------------------------------------------

(deftest type-lattice
  (is-equal '(:int 0 255) (parse-type '(unsigned-byte 8)))
  (is-equal *ty-fixnum* (parse-type 'fixnum))
  (is-equal :flonum (parse-type 'double-float))
  (is-equal :flonum (parse-type '(single-float 0.0 1.0)))
  (is-equal nil (parse-type 'string))                      ; adjustable strings are not simple strings
  (is-equal nil (parse-type 'some-user-deftype))
  (is-equal '(:int -5 7) (parse-type '(integer -5 7)))
  (is-equal '(:int 0 nil) (parse-type '(integer 0 *)))
  (is-equal '(:int 0 9) (parse-type '(mod 10)))
  (is-equal '(:int -128 127) (parse-type '(signed-byte 8)))
  (is-equal :svec (parse-type '(simple-array t (*))))
  (is-equal :svec (parse-type 'simple-vector))
  (is-equal :string (parse-type '(simple-array character (*))))
  (is-equal nil (parse-type '(simple-array t (* *))))
  (is-equal :real (parse-type '(or fixnum double-float)))
  (is-equal :list (parse-type '(or null cons)))
  (is-equal nil (parse-type '(or fixnum string)))
  (is (ty-subtype-p '(:int 1 5) '(:int 0 10)))
  (is (not (ty-subtype-p '(:int 1 50) '(:int 0 10))))
  (is (ty-subtype-p :null :list))
  (is (ty-subtype-p '(:int 1 1) :number))
  (is (not (ty-subtype-p nil :cons)))
  (is (ty-subtype-p :cons nil))
  (is-equal '(:int 0 10) (ty-join '(:int 0 3) '(:int 5 10)))
  (is-equal '(:int 2 3) (ty-meet '(:int 0 3) '(:int 2 9)))
  (is-equal '((type fixnum x) (type fixnum y) (optimize (speed 3)))
            (parse-declarations-specs '((fixnum x y) (optimize (speed 3))))))

(defun st-adjustable (items)
  (let ((v (make-array 4 :adjustable t :fill-pointer 0)))
    (dolist (x items v) (vector-push-extend x v))))

(defun st-same (a b)
  "Structural equality that looks inside vectors, with EQL on atoms (EQUAL does not descend into vectors)."
  (cond ((consp a) (and (consp b) (st-same (car a) (car b)) (st-same (cdr a) (cdr b))))
        ((stringp a) (and (stringp b) (string= a b)))
        ((vectorp a) (and (vectorp b) (= (length a) (length b)) (every #'st-same a b)))
        (t (eql a b))))

(defun parse-declarations-specs (decls) (values (parse-declarations (list (cons 'declare decls)))))

;;; ---- specialised instructions ---------------------------------------------------------------------------

(deftest fixnum-specialisation
  (let ((f (st-comp '(lambda (a b) (declare (fixnum a b) (optimize (safety 0))) (the fixnum (+ a (* b 2)))))))
    (is (st-has f :addfx)) (is (st-has f :mulfx)) (is (not (st-has f :chkfix)))
    (is-equal 11 (funcall f 3 4))
    (is-equal (+ most-negative-fixnum 1) (funcall f most-positive-fixnum 1)))      ; safety 0: wraps (maxfix + 2)
  (let ((f (st-comp '(lambda (a b) (declare (fixnum a b)) (the fixnum (+ a (* b 2)))))))
    (is (st-has f :chkfix)) (is (not (st-has f :addfx)))                        ; the sum may not fit: generic ADD
    (is-equal 11 (funcall f 3 4))
    (signals-error (funcall f most-positive-fixnum 1))
    (signals-error (funcall f 'a 1)))
  (let ((f (st-comp '(lambda (a b) (declare (type (integer 0 1000) a b)) (+ a (* b 2))))))
    (is (st-has f :addfx)) (is (st-has f :mulfx))                                ; the range fits a fixnum: no check needed
    (is-equal 2003 (funcall f 3 1000))
    (signals-error (funcall f 1001 0))
    (signals-error (funcall f -1 0)))
  (let ((f (st-comp '(lambda (a b) (declare (fixnum a b)) (list (< (logand a 255) (rem b 7)) (= a b) (<= a b) (>= a b) (> a b))))))
    (is (st-has f :ltfx)) (is (st-has f :numeqfx)) (is (st-has f :logandfx)) (is (st-has f :remfx))
    (is-equal '(nil nil t nil nil) (funcall f 300 400))
    (is-equal '(nil t t t nil) (funcall f 0 0)))
  (let ((f (st-comp '(lambda (a) (declare (fixnum a) (optimize (safety 0))) (list (ash a 3) (ash a -2) (mod a 10) (logior a 1) (logxor a 5) (the fixnum (1+ a)) (the fixnum (1- a)))))))
    (is (st-has f :ashfx)) (is (st-has f :modfx)) (is (st-has f :incfx)) (is (st-has f :decfx))
    (is-equal (list 80 2 0 11 15 11 9) (funcall f 10))
    (is-equal (list -80 -3 0 -9 -13 -9 -11) (funcall f -10))))

(deftest fixnum-specialisation-agrees-with-generic
  ;; typed code at safety 0 on in-range values must agree with the untyped (generic) code
  (let ((typed (st-comp '(lambda (a b) (declare (fixnum a b) (optimize (safety 0)))
                          (the fixnum (+ (* a 3) (- b a) (logxor a b) (ash a -3) (logand a #xffff))))))
        (plain (st-comp '(lambda (a b) (+ (* a 3) (- b a) (logxor a b) (ash a -3) (logand a #xffff)))))
        (mods-typed (st-comp '(lambda (a b) (declare (fixnum a b) (optimize (safety 0))) (list (mod a b) (rem a b)))))
        (mods-plain (st-comp '(lambda (a b) (list (mod a b) (rem a b))))))
    (dotimes (i 300)
      (let ((a (st-rand-fix 40)) (b (st-rand-fix 40)))
        (is-equal (funcall plain a b) (funcall typed a b))
        (when (/= b 0) (is-equal (funcall mods-plain a b) (funcall mods-typed a b)))))
    (signals-error (funcall mods-typed 5 0))))

(deftest float-specialisation
  (let ((f (st-comp '(lambda (x y) (declare (double-float x y)) (/ (+ (* x y) 1) 4)))))
    (is (st-has f :mulfl)) (is (st-has f :addfl)) (is (st-has f :divfl))
    (is-equal 1.75 (funcall f 2.0 3.0))
    (signals-error (funcall f 2 3.0))
    (signals-error (funcall f 2.0 nil)))
  (let ((f (st-comp '(lambda (x n) (declare (double-float x) (fixnum n)) (list (+ x n) (< x n) (- n x) (float n) (zerop x))))))
    (is (st-has f :fixtofl)) (is (st-has f :ltfl))
    (is-equal '(5.5 t 2.5 4.0 nil) (funcall f 1.5 4))
    (is-equal '(0.0 nil 0.0 0.0 t) (funcall f 0.0 0)))
  (let ((f (st-comp '(lambda (x y) (declare (double-float x y)) (/ x y)))))
    (is-equal 0.5 (funcall f 1.0 2.0))
    (signals-error (funcall f 1.0 0.0))))

(deftest car-cdr-and-vectors
  (let ((f (st-comp '(lambda (x) (declare (cons x) (optimize (safety 0))) (cons (car x) (cdr x))))))
    (is (st-has f :carx)) (is (st-has f :cdrx))
    (is-equal '(1 . 2) (funcall f '(1 . 2))))
  (let ((f (st-comp '(lambda (x) (declare (cons x)) (car x)))))
    (is (st-has f :carx))                                                  ; the entry check makes the CAR safe
    (is-equal 1 (funcall f '(1 . 2)))
    (signals-error (funcall f 5)))
  (let ((f (st-comp '(lambda (v i) (svref v i)))))
    (is (st-has f :svref))
    (is-equal 20 (funcall f (vector 10 20 30) 1))
    (signals-error (funcall f (vector 1) 5))
    (signals-error (funcall f (vector 1) -1))
    (signals-error (funcall f (vector 1) 'a))
    (signals-error (funcall f 'x 0))
    (is-equal 2 (funcall f (st-adjustable '(1 2 3)) 1)))                                  ; generic fallback
  (let ((f (st-comp '(lambda (v i) (declare (simple-vector v) (fixnum i) (optimize (safety 0))) (svref v i)))))
    (is (st-has f :svrefx))
    (is-equal 30 (funcall f (vector 10 20 30) 2)))
  (let ((f (st-comp '(lambda (a i) (aref a i)))) (g (st-comp '(lambda (s i) (declare (simple-string s)) (aref s i)))))
    (is (st-has f :svref)) (is (st-has g :schar))
    (is-equal #\b (funcall f "abc" 1))
    (is-equal #\c (funcall g "abc" 2))
    (signals-error (funcall g "abc" 3)))
  (let ((f (st-comp '(lambda (s i) (char s i)))))
    (is (st-has f :schar))
    (is-equal #\é (funcall f "aé" 1))
    (is-equal #\→ (funcall f "a→" 1))
    (is-equal #\x (funcall f (let ((s (make-array 5 :element-type 'character :fill-pointer 0 :adjustable t))) (vector-push-extend #\y s) (vector-push-extend #\x s) s) 1))
    (signals-error (funcall f "ab" -1)))
  (let ((f (st-comp '(lambda (v i x) (setf (svref v i) x)))) (v (vector 1 2 3)))
    (is (st-has f :svset))
    (is-equal 9 (funcall f v 1 9))
    (is (st-same #(1 9 3) v))
    (signals-error (funcall f v 3 0))
    (let ((s (copy-seq "abc")))
      (is-equal #\z (funcall f s 0 #\z))
      (is-equal "zbc" s))
    (let ((a (make-array '(2 2) :initial-element 0)))
      (signals-error (funcall f a 9 1)))))

;;; ---- checks, declarations and the policy --------------------------------------------------------------------

(deftest declared-types-are-checked
  (let ((f (st-comp '(lambda (x) (declare (type (integer 0 10) x)) x))))
    (is-equal 5 (funcall f 5))
    (signals-error (funcall f 11)) (signals-error (funcall f -1)) (signals-error (funcall f 2.0)))
  (let ((f (st-comp '(lambda (x) (declare (type (unsigned-byte 70) x)) x))))
    (is-equal (expt 2 65) (funcall f (expt 2 65)))
    (signals-error (funcall f 'a)))
  (let ((f (st-comp '(lambda (x) (the (integer 0 5) x)))))
    (is-equal 3 (funcall f 3))
    (signals-error (funcall f 6)))
  (let ((f (st-comp '(lambda (x) (the (integer 0 *) x)))))
    (is-equal (expt 2 70) (funcall f (expt 2 70)))
    (signals-error (funcall f -1)))
  (let ((f (st-comp '(lambda (x) (declare (fixnum x)) (let ((y 0)) (declare (fixnum y)) (setq y x) (setq y (* y 4)) y)))))
    (is-equal 40 (funcall f 10))
    (signals-error (funcall f most-positive-fixnum)))
  (let ((f (st-comp '(lambda (x) (let ((y x)) (declare (cons y)) y)))))
    (is-equal '(1) (funcall f '(1)))
    (signals-error (funcall f 3)))
  ;; at safety 0 the declarations are trusted: no checks at all
  (let ((f (st-comp '(lambda (x) (declare (fixnum x) (optimize (safety 0))) x))))
    (is (not (st-has f :chkfix)))))

(deftest typed-optional-key-and-aux-variables
  (let ((f (st-comp '(lambda (a &optional (b 5) &key (c 7) &aux (d (+ a b)))
                      (declare (fixnum a b c d)) (list a b c d)))))
    (is-equal '(1 5 7 6) (funcall f 1))
    (is-equal '(1 2 7 3) (funcall f 1 2))
    (is-equal '(1 2 9 3) (funcall f 1 2 :c 9))
    (signals-error (funcall f 1 'x))
    (signals-error (funcall f 1 2 :c 'z))
    (signals-error (funcall f 'a))))

(deftest let-star-and-do-declarations
  (let ((f (st-comp '(lambda (x) (let* ((a x) (b (+ a 1))) (declare (fixnum a) (type (integer 0 100) b)) (list a b))))))
    (is-equal '(5 6) (funcall f 5))
    (signals-error (funcall f 'q))
    (signals-error (funcall f 100)))
  (let ((f (st-comp '(lambda (n) (declare (fixnum n))
                      (let ((s 0)) (declare (fixnum s)) (dotimes (i n) (declare (fixnum i)) (setq s (+ s i))) s)))))
    (is (st-has f :gefx)) (is (st-has f :incfx))
    (is-equal 45 (funcall f 10))
    (is-equal 0 (funcall f 0)))
  (let ((f (st-comp '(lambda (n) (declare (fixnum n))
                      (do ((i 0 (1+ i)) (s 0)) ((>= i n) s) (declare (fixnum i s)) (setq s (+ s i)))))))
    (is (st-has f :gefx))
    (is-equal 45 (funcall f 10)))
  (is-equal 6 (st-dotimes-result 6)))

(defun st-dotimes-result (n) (let ((r 0)) (dotimes (i n r) (declare (fixnum i)) (setq r (1+ i)))))

(deftest policy
  (let ((f (st-comp '(lambda (n) (declare (optimize (debug 3))) (st-undefined-fn n))))
        (g (st-comp '(lambda (n) (st-undefined-fn n)))))
    (is (not (st-has f :tailcall))) (is (st-has f :retv))
    (is (st-has g :tailcall)))
  (let ((f (st-comp '(lambda (x) (declare (notinline car)) (car x)))))
    (is (not (st-has f :car)))
    (is-equal 1 (funcall f '(1 2))))
  (let ((f (st-comp '(lambda (x) (car x)))))
    (is (st-has f :car)))
  (let ((*policy* *policy*))
    (%declaim '((optimize (safety 0))))
    (let ((f (st-comp '(lambda (a) (declare (fixnum a)) (the fixnum (1+ a))))))
      (is (st-has f :incfx)) (is (not (st-has f :chkfix))))
    (%declaim '((optimize (safety 1))))
    (let ((f (st-comp '(lambda (a) (declare (fixnum a)) (the fixnum (1+ a))))))
      (is (st-has f :chkfix))))
  (is-equal 1 (policy-quality 'safety))
  (let ((f (st-comp '(lambda (a) (declare (fixnum a)) (let ((b a)) (declare (optimize (safety 0))) (the fixnum (+ b 1)))))))
    (is (st-has f :incfx)) (is (st-has f :chkfix)))              ; the parameter check stays: it is outside the LET
  (let ((f (st-comp '(lambda (a) (declare (fixnum a) (optimize (safety 0))) a))))
    (is (not (st-has f :chkfix))))
  (is-equal '((speed . 3) (safety . 0)) (parse-policy-delta '((optimize speed (safety 0))))))

(defvar *st-g* 5)
(defun st-id-fn (x) x)

(deftest declaimed-types
  (%declaim '((ftype (function (fixnum) fixnum) st-id-fn)))
  (let ((f (st-comp '(lambda (y) (+ (st-id-fn y) 1)))) (g (st-comp '(lambda (y) (st-id-fn y)))))
    (is (st-has f :chkfix))
    (is-equal 4 (funcall f 3))
    (signals-error (funcall f 'a))                                ; the declared result is checked
    (is (st-has g :tailcall)) (is (not (st-has g :chkfix))))     ; ... except in a tail call, which stays one
  (%put 'st-id-fn '%ftype-result nil)
  (%declaim '((type fixnum *st-g*)))
  (let ((f (st-comp '(lambda () (+ *st-g* 1)))) (g (st-comp '(lambda (v) (setq *st-g* v)))))
    (is (st-has f :chkfix))
    (is-equal 6 (funcall f))
    (is-equal 7 (funcall g 7))
    (signals-error (funcall g 'a)))
  (%put '*st-g* '%type nil)
  (setq *st-g* 5)
  (proclaim '(optimize (speed 1))))

;;; ---- the binary fasl format ----------------------------------------------------------------------------------

(defvar *st-consts-text*
  "(0 -7 1152921504606846975 -1152921504606846976 1152921504606846976 -99999999999999999999999 1/3 -7/9 #C(1 2) #C(1.5 -2.5) 3.14159 -0.0 1.0e300 \"hello\" \"héllo→✓\" #\\a #\\λ (a b . c) #(1 2 (3)) :key st-sym nil t \"\")")

(defmacro st-twice () (let ((g (make-symbol "X"))) `(quote (,g ,g))))

(defun st-write-source (path)
  (with-open-file (s path :direction :output)
    (write-line "(in-package :cl)" s)
    (write-line (format nil "(defvar *st-fasl-consts* '~A)" *st-consts-text*) s)
    (write-line "(defvar *st-fasl-u* (st-twice))" s)
    (write-line "(defun st-fasl-fn (x &optional (y 2) &key z) (declare (fixnum x)) (list x y z (the fixnum (+ x 1))))" s)
    (write-line "(defvar *st-fasl-count* 0)" s)
    (write-line "(setq *st-fasl-count* (+ *st-fasl-count* 1))" s)))

(deftest binary-fasl-roundtrip
  (let ((*package* (find-package "COMMON-LISP")) (src "build/st-src.lisp") (bin "build/st-a.nfasl") (bin2 "build/st-b.nfasl") (txt "build/st-t.nfasl"))
    (st-write-source src)
    (compile-file src :output-file bin)
    (let ((bytes (%fasl-read-file bin)))
      (is-equal '(127 78 70 65 83 76 13 10) (mapcar #'char-code (coerce (subseq bytes 0 8) 'list)))
      (is (%binary-fasl-p bytes))
      (is (> (%fasl-entries bytes) 5)))
    (compile-file src :output-file bin2)                          ; deterministic output
    (is (string= (%fasl-read-file bin) (%fasl-read-file bin2)))
    (setq *st-fasl-count* 0)
    (makunbound '*st-fasl-consts*)
    (load bin)
    (is-equal 1 *st-fasl-count*)
    (is (st-same (read-from-string *st-consts-text*) *st-fasl-consts*))
    (is (eql -0.0 (nth 11 *st-fasl-consts*)))
    (is (eq (first *st-fasl-u*) (second *st-fasl-u*)))
    (is (null (symbol-package (first *st-fasl-u*))))
    (is-equal "X" (symbol-name (first *st-fasl-u*)))
    (is-equal '(4 2 nil 5) (st-fasl-fn 4))
    (is-equal '(4 9 7 5) (st-fasl-fn 4 9 :z 7))
    (signals-error (st-fasl-fn 'a))
    (is (definition-locations 'st-fasl-fn))
    ;; the text format is still written and still loads; load-fasl recognises either
    (compile-file src :output-file txt :format :text)
    (is (%starts-with (with-open-file (s txt) (read-line s)) *fasl-header*))
    (setq *st-fasl-count* 0)
    (makunbound '*st-fasl-consts*)
    (load-fasl txt)
    (is-equal 1 *st-fasl-count*)
    (is (st-same (read-from-string *st-consts-text*) *st-fasl-consts*))
    (is-equal '(4 2 nil 5) (st-fasl-fn 4))
    (signals-error (compile-file src :output-file txt :format :nonsense))
    (let ((out (with-output-to-string (s) (is-equal (%fasl-entries (%fasl-read-file bin)) (dump-fasl bin s)))))
      (is (search "entry 0" out)) (is (search "const" out)))
    (dolist (f (list bin bin2 txt src)) (delete-file f))))

(deftest binary-fasl-rejects-damage
  (let* ((*package* (find-package "COMMON-LISP")) (src "build/st-src2.lisp") (bin "build/st-d.nfasl") (bad "build/st-bad.nfasl"))
    (st-write-source src)
    (compile-file src :output-file bin)
    (let* ((bytes (%fasl-read-file bin)) (n (length bytes)))
      (flet ((damaged (new) (%fasl-write-raw bad new) (signals-error (load-fasl bad))))
        (let ((c (copy-seq bytes))) (setf (char c (floor n 2)) (code-char (logxor 1 (char-code (char c (floor n 2)))))) (damaged c))   ; checksum
        (damaged (subseq bytes 0 (- n 1)))                                      ; truncated
        (damaged (subseq bytes 0 20))
        (damaged (subseq bytes 0 3))
        (damaged "")
        (damaged "garbage that is not a fasl file")
        (let ((c (copy-seq bytes))) (setf (char c 8) (code-char 99)) (damaged c))     ; unknown format version
        (damaged (%string-concat bytes "x"))))
    (signals-error (load-fasl "build/st-no-such-file.nfasl"))
    (is (handler-case (progn (load-fasl bad) nil) (error (e) (search "fasl" (princ-to-string e)))))
    (dolist (f (list bin bad src)) (delete-file f))))

(defun st-bad-payload (instrs consts)
  (%fasl-encode (%make-code 'st-bad 0 0 nil instrs consts nil)))

(defun st-decode-payload (payload)
  (let ((path "build/st-p.nfasl"))
    (%fasl-write-file path (list payload))
    (let ((bytes (%fasl-read-file path)))
      (%fasl-entries bytes)
      (car (%fasl-decode bytes +fasl-first-entry+)))))

(deftest binary-fasl-validates-code
  (is (%codep (st-decode-payload (st-bad-payload '(0 0 14) (vector 5)))))                   ; const 0, ret: fine
  (signals-error (st-decode-payload (st-bad-payload '(0 5 14) (vector))))                   ; constant out of range
  (signals-error (st-decode-payload (st-bad-payload '(0 -1 14) (vector 1))))
  (signals-error (st-decode-payload (st-bad-payload '(255 14) (vector))))                   ; bad opcode
  (signals-error (st-decode-payload (st-bad-payload '(-3) (vector))))
  (signals-error (st-decode-payload (st-bad-payload '() (vector))))                         ; empty
  (signals-error (st-decode-payload (st-bad-payload '(0 0) (vector 1))))                    ; falls off the end
  (signals-error (st-decode-payload (st-bad-payload '(0) (vector 1))))                      ; truncated operand
  (signals-error (st-decode-payload (st-bad-payload '(9 99) (vector))))                     ; jump out of range
  (signals-error (st-decode-payload (st-bad-payload '(9 1 14) (vector))))                   ; jump into an operand
  (signals-error (st-decode-payload (st-bad-payload '(70 99 14) (vector))))                 ; bad type code
  (signals-error (st-decode-payload (st-bad-payload '(15 0 0 14) (vector 7))))              ; CLOSURE of a non-code constant
  (signals-error (st-decode-payload (st-bad-payload '(71 0 14) (vector nil))))              ; CHKRANGE without a range
  (signals-error (st-decode-payload (st-bad-payload '(4 0 14) (vector 5))))                 ; GREF of a non-symbol
  (is (%codep (st-decode-payload (st-bad-payload (list 71 0 14) (vector (cons 1 5))))))
  (ignore-errors (delete-file "build/st-p.nfasl")))

(deftest binary-fasl-fuzz
  ;; Mutated payloads must either decode (never executed here) or signal an ordinary error: no crash, no hang.
  (let* ((fn (st-comp '(lambda (a &optional (b "str") &key (c 1/3)) (declare (fixnum a))
                        (list a b c 2.5 #\x '(1 2 . 3) #(1 2) (lambda (x) (+ x a)) 123456789012345678901234567890 (cons a :k)))))
         (code (let ((parts (%code-parts fn))) (%make-code (nth 0 parts) (nth 1 parts) (nth 2 parts) (nth 3 parts) (nth 4 parts) (nth 5 parts) (nth 6 parts))))
         (payload (%fasl-encode code))
         (ok 0) (errors 0) (path "build/st-fuzz.nfasl"))
    (dotimes (i 1500)
      (let ((s (copy-seq payload)) (n (length payload)))
        (dotimes (k (+ 1 (rand-int 4 *st-rng*)))
          (let ((p (rand-int n *st-rng*)))
            (case (rand-int 4 *st-rng*)
              (0 (setf (char s p) (code-char (rand-int 256 *st-rng*))))
              (1 (setf (char s p) (code-char (logxor (char-code (char s p)) (ash 1 (rand-int 8 *st-rng*))))))
              (2 (setq s (subseq s 0 (max 1 p))))
              (t (setq s (%string-concat s (subseq s p (min (length s) (+ p 6)))))))
            (setq n (length s))))
        (handler-case
            (progn (%fasl-write-file path (list s))
                   (let ((bytes (%fasl-read-file path)))
                     (%fasl-entries bytes)
                     (is (%codep (car (%fasl-decode bytes +fasl-first-entry+)))))
                   (setq ok (+ ok 1)))
          (error () (setq errors (+ errors 1))))))
    (is (> errors 100))
    (is (> ok 0))
    (dolist (len '(1 2 3 5 8 13 40))                               ; random garbage, and prefixes of a real payload
      (let ((s (make-string len #\a)))
        (dotimes (k 40)
          (dotimes (j len) (setf (char s j) (code-char (rand-int 256 *st-rng*))))
          (handler-case (progn (%fasl-write-file path (list s)) (let ((b (%fasl-read-file path))) (%fasl-entries b) (%fasl-decode b +fasl-first-entry+)))
            (error () nil)))))
    (ignore-errors (delete-file path))))
