;;;; asm-tests.lisp -- the bytecode assembler (DEFASM), its verifier, the listing/decompiler, ASM mode and the manual.
;;;; Uses helpers from tools-tests.lisp (make-editor-wm, ed-keys, ed-type).

(in-package :cl)

;;; Earlier tests leave *PACKAGE* and the editor's package changed, and these tests read and print symbols, so each runs in CL.
(defmacro defasm-test (name &body body)
  `(deftest ,name (let ((*package* (find-package "CL")) (*ed-package* nil)) ,@body)))

(defvar *asm-test-special* 1)
(defun asm-test-reader () *asm-test-special*)

(defun asm-run (ll items &rest args)
  "Assemble ITEMS as a routine with lambda list LL and call it with ARGS."
  (apply (asm-assemble 'asm-test-routine ll items) args))

(defun asm-fail-message (ll items)
  "The message of the assembly error ITEMS provoke, or NIL if they assemble."
  (handler-case (progn (asm-assemble 'asm-test-routine ll items) nil)
    (asm-error (e) (asm-error-message e))))

(defun asm-fails-with (part ll items)
  (let ((m (asm-fail-message ll items)))
    (and m (%string-search part m 0) t)))

(defun asm-test-line (code &optional comment)
  (if comment (%string-concat (%asm-pad-to code *asm-comment-column*) comment) code))

;;; ---- the instruction table ----------------------------------------------------------------------------------------

(defasm-test asm-instruction-table
  (is-equal (length *opcodes*) (length *asm-instruction-info*))
  (dolist (e *opcodes*)
    (let ((info (%asm-info-entry (car e))))
      (is info)
      (when info
        ;; operands described exactly when the instruction has operands
        (is (eq (= (caddr e) 0) (string= (cadr info) ""))))))
  (is-equal (asm-mnemonics) (mapcar (lambda (e) (string-downcase (symbol-name (car e)))) *opcodes*))
  (is (%string-search "stack" (asm-instruction-help 'dup) 0))
  (is-equal nil (asm-instruction-help "nonesuch"))
  (is (%string-search "( a b -- sum )" (asm-instruction-help "ADD") 0))
  (dolist (g *asm-instruction-info*) (is (assoc (cadddr g) *asm-instruction-groups*))))

;;; ---- assembling and running ---------------------------------------------------------------------------------------

(defasm-test asm-basic-routines
  (is-equal 42 (asm-run '() '((const 42) (ret))))
  (is-equal 7 (asm-run '(a b) '((lref a) (lref b) (add) (ret)) 3 4))
  (is-equal 7 (asm-run '(a b) '((lref 0) (lref 1) (add) (ret)) 3 4))
  (is-equal nil (asm-run '() '((const nil) (ret))))
  (is-equal "hi" (asm-run '() '((const "hi") (ret))))
  (is-equal '(a b) (asm-run '() '((const (a b)) (ret))))
  (is-equal 'foo (asm-run '() '((const foo) (ret))))
  (is-equal '(1 . 2) (asm-run '(x) '((const 1) (lref x) (cons) (ret)) 2))
  (is-equal 3 (asm-run '() '((const 1) (const 2) (const 3) (slide 2) (ret))))
  (is-equal '(1 2) (asm-run '() '((gfun list) (const 1) (const 2) (call 2) (ret))))
  (is-equal 5 (asm-run '(x) '((lref x) (dup) (pop) (ret)) 5))
  (is-equal '(t nil) (list (asm-run '(a b) '((lref a) (lref b) (eq) (ret)) 'x 'x) (asm-run '(a) '((lref a) (not) (not) (ret)) nil)))
  (is-equal '(2 3) (list (asm-run '(l) '((lref l) (car) (ret)) '(2 3)) (asm-run '(l) '((lref l) (cdr) (car) (ret)) '(2 3))))
  (is-equal '(t nil t t) (list (asm-run '(a b) '((lref a) (lref b) (lt) (ret)) 1 2) (asm-run '(a b) '((lref a) (lref b) (gt) (ret)) 1 2)
                               (asm-run '(a b) '((lref a) (lref b) (le) (ret)) 2 2) (asm-run '(a b) '((lref a) (lref b) (numeq) (ret)) 2.0 2)))
  (is-equal 6 (asm-run '(a b) '((lref a) (lref b) (mul) (ret)) 2 3))
  (is-equal -1 (asm-run '(a b) '((lref a) (lref b) (sub) (ret)) 2 3))
  (is-equal (expt 2 70) (asm-run '(a) '((lref a) (lref a) (mul) (ret)) (expt 2 35))))

(defasm-test asm-defasm-form
  (defasm asm-test-fact (n)
    "Factorial by a loop."
    (const 1)
    (:var acc)
    top
    (lref n) (const 1) (le) (jnnil done)
    (lref acc) (lref n) (mul) (lset acc)
    (lref n) (const 1) (sub) (lset n)
    (jmp top)
    done
    (lref acc) (ret))
  (is-equal 3628800 (asm-test-fact 10))
  (is-equal 1 (asm-test-fact 0))
  (is-equal 15511210043330985984000000 (asm-test-fact 25))
  (is-equal "Factorial by a loop." (documentation 'asm-test-fact 'function))
  (is-equal '(n) (nth 6 (%code-parts (symbol-function 'asm-test-fact))))
  (is (signals-error (asm-test-fact)))                       ; the VM checks the argument count
  (is (signals-error (asm-test-fact 1 2)))
  (is (eq 'asm-test-fact (defasm asm-test-fact (n) (lref n) (ret)))))

(defasm-test asm-tail-calls-and-recursion
  (defasm asm-test-count (n acc)
    (lref n) (const 0) (numeq) (jnil more)
    (lref acc) (ret)
    more
    (gfun asm-test-count) (lref n) (const 1) (sub) (lref acc) (lref n) (add) (tailcall 2))
  (is-equal 5000050000 (asm-test-count 100000 0))             ; no stack growth: the frame is reused
  (defasm asm-test-fib (n)
    (lref n) (const 2) (lt) (jnil rec)
    (lref n) (ret)
    rec
    (gfun asm-test-fib) (lref n) (const 1) (sub) (call 1)
    (gfun asm-test-fib) (lref n) (const 2) (sub) (call 1)
    (add) (ret))
  (is-equal 610 (asm-test-fib 15)))

(defasm-test asm-optional-and-rest
  (let ((f (asm-assemble 'asm-test-opt '(a &optional b &rest r)
                         '((lref r) (ret)))))
    (is-equal nil (funcall f 1))
    (is-equal '(3 4) (funcall f 1 2 3 4))
    (is (signals-error (funcall f))))
  (let ((f (asm-assemble 'asm-test-unsupplied '(a &optional b)
                         '((gfun %unsupplied) (call 0) (lref b) (eq) (ret)))))
    (is-equal t (funcall f 1))
    (is-equal nil (funcall f 1 2)))
  (is-equal 1 (nth 1 (%code-parts (asm-assemble 'f '(a &optional b &rest r) '((const 1) (ret))))))
  (is-equal '(2 1 t) (let ((p (%code-parts (asm-assemble 'f '(a &optional b &rest r) '((const 1) (ret)))))) (list (+ (nth 1 p) 1) (nth 2 p) (and (nth 3 p) t)))))

(defasm-test asm-closures-and-boxes
  (defasm asm-test-make-counter ()
    (const 0) (box)
    (closure (:lambda ()
               (:free cell)
               (fref cell) (chktype cons)
               (fref cell) (chktype cons) (unbox) (const 1) (add)
               (boxset) (ret))
             1)
    (ret))
  (let ((c (asm-test-make-counter)) (d (asm-test-make-counter)))
    (is-equal '(1 2 3) (list (funcall c) (funcall c) (funcall c)))
    (is-equal 1 (funcall d)))
  ;; captured values by position, no :free names
  (defasm asm-test-adder (n)
    (lref n)
    (closure (:lambda (x) (lref x) (fref 0) (add) (ret)) 1)
    (ret))
  (is-equal 15 (funcall (asm-test-adder 10) 5))
  ;; nested twice
  (defasm asm-test-curry3 (a)
    (lref a)
    (closure (:lambda (b)
               (lref b) (fref 0)
               (closure (:lambda (c) (lref c) (fref 0) (fref 1) (add) (add) (ret)) 2)
               (ret))
             1)
    (ret))
  (is-equal 6 (funcall (funcall (asm-test-curry3 1) 2) 3)))

(defasm-test asm-special-bindings-and-globals
  (defasm asm-test-bound ()
    (const 5) (bind *asm-test-special*)
    (gfun asm-test-reader) (call 0)
    (unbind 1) (ret))
  (is-equal 5 (asm-test-bound))
  (is-equal 1 *asm-test-special*)                              ; unbound again
  (is-equal 5 (let ((*asm-test-special* 9)) (asm-test-bound)))
  (is-equal 9 (let ((*asm-test-special* 9)) (asm-run '() '((gref *asm-test-special*) (ret)))))
  (defasm asm-test-setter (v) (lref v) (gset *asm-test-special*) (gref *asm-test-special*) (ret))
  (is-equal 77 (let ((*asm-test-special* 0)) (asm-test-setter 77)))
  (is (signals-error (asm-run '() '((gref asm-test-never-bound-xyz) (ret)))))
  (is (signals-error (asm-run '() '((const 1) (gset t) (const 2) (ret))))))

(defasm-test asm-multiple-values
  (let ((f (asm-assemble 'asm-test-mv '() '((gfun values) (const 1) (const 2) (const 3) (call 3) (retv)))))
    (is-equal '(1 2 3) (multiple-value-list (funcall f))))
  (let ((f (asm-assemble 'asm-test-mv1 '() '((gfun values) (const 1) (const 2) (call 2) (ret)))))
    (is-equal '(1) (multiple-value-list (funcall f))))
  (let ((f (asm-assemble 'asm-test-mvt '() '((gfun floor) (const 7) (const 2) (tailcall 2)))))
    (is-equal '(3 1) (multiple-value-list (funcall f)))))

(defasm-test asm-fixnum-and-flonum-operations
  (let ((f (asm-assemble 'asm-test-fx '(a b)
                         '((lref a) (chkfix) (lset a) (lref b) (chkfix) (lset b)
                           (lref a) (lref b) (addfx) (ret)))))
    (is-equal 7 (funcall f 3 4))
    (is-equal most-negative-fixnum (funcall f most-positive-fixnum 1))   ; wraps modulo 2^61
    (is (signals-error (funcall f 1.5 2)))
    (is (signals-error (funcall f 1 'x))))
  (flet ((fx2 (op) (asm-assemble 'f '(a b) `((lref a) (chkfix) (lset a) (lref b) (chkfix) (lset b) (lref a) (lref b) (,op) (ret)))))
    (is-equal -1 (funcall (fx2 'subfx) 3 4))
    (is-equal 12 (funcall (fx2 'mulfx) 3 4))
    (is-equal '(t nil) (list (funcall (fx2 'ltfx) 3 4) (funcall (fx2 'gtfx) 3 4)))
    (is-equal '(t t t) (list (funcall (fx2 'lefx) 3 3) (funcall (fx2 'gefx) 3 3) (funcall (fx2 'numeqfx) 3 3)))
    (is-equal '(2 7 5) (list (funcall (fx2 'logandfx) 6 3) (funcall (fx2 'logiorfx) 6 3) (funcall (fx2 'logxorfx) 6 3)))
    (is-equal '(1 -1 2 3) (list (funcall (fx2 'remfx) 7 3) (funcall (fx2 'remfx) -7 3) (funcall (fx2 'modfx) -7 3) (funcall (fx2 'modfx) 8 5)))
    (is (signals-error (funcall (fx2 'remfx) 1 0)))
    (is (signals-error (funcall (fx2 'modfx) 1 0))))
  (flet ((fx1 (op) (asm-assemble 'f '(a) `((lref a) (chkfix) ,@op (ret)))))
    (is-equal 6 (funcall (fx1 '((incfx))) 5))
    (is-equal 4 (funcall (fx1 '((decfx))) 5))
    (is-equal 40 (funcall (fx1 '((ashfx 3))) 5))
    (is-equal 2 (funcall (fx1 '((ashfx -1))) 5))
    (is-equal -3 (funcall (fx1 '((ashfx -1))) -5))
    (is-equal 5.0 (funcall (fx1 '((fixtofl))) 5)))
  (flet ((fl2 (op) (asm-assemble 'f '(a b) `((lref a) (chktype double-float) (lset a) (lref b) (chktype double-float) (lset b) (lref a) (lref b) (,op) (ret)))))
    (is-equal 3.5 (funcall (fl2 'addfl) 1.5 2.0))
    (is-equal -0.5 (funcall (fl2 'subfl) 1.5 2.0))
    (is-equal 3.0 (funcall (fl2 'mulfl) 1.5 2.0))
    (is-equal 0.75 (funcall (fl2 'divfl) 1.5 2.0))
    (is (signals-error (funcall (fl2 'divfl) 1.5 0.0)))
    (is (signals-error (funcall (fl2 'addfl) 1 2.0)))
    (is-equal '(t nil t t t) (list (funcall (fl2 'ltfl) 1.0 2.0) (funcall (fl2 'gtfl) 1.0 2.0) (funcall (fl2 'lefl) 2.0 2.0)
                                   (funcall (fl2 'gefl) 2.0 2.0) (funcall (fl2 'numeqfl) 2.0 2.0)))))

(defasm-test asm-checks-and-vectors
  (let ((f (asm-assemble 'f '(x) '((lref x) (chkrange 1 10) (ret)))))
    (is-equal 5 (funcall f 5))
    (is (signals-error (funcall f 11)))
    (is (signals-error (funcall f 0)))
    (is (signals-error (funcall f 2.0))))
  (dolist (tp '(fixnum integer double-float real number cons list symbol null string simple-vector character))
    (let ((f (asm-assemble 'f '(x) `((lref x) (chktype ,tp) (ret)))))
      (is-equal 'ok (handler-case (progn (funcall f (case tp (fixnum 1) (integer 2) (double-float 1.5) (real 1) (number 1) (cons '(1)) (list nil)
                                                       (symbol 'a) (null nil) (string "s") (simple-vector (vector 1)) (character #\a)))
                                         'ok)
                          (error () 'wrong)))))
  (is (signals-error (asm-run '(x) '((lref x) (chktype cons) (ret)) 1)))
  (is (signals-error (asm-run '(x) '((lref x) (chktype string) (ret)) 'sym)))
  (let ((v (vector 10 20 30)))
    (is-equal 20 (asm-run '(v i) '((lref v) (lref i) (svref) (ret)) v 1))
    (is-equal 99 (asm-run '(v i) '((lref v) (lref i) (const 99) (svset) (ret)) v 2))
    (is-equal 99 (svref v 2))
    (is (signals-error (asm-run '(v i) '((lref v) (lref i) (svref) (ret)) v 3)))      ; out of range: an error, not a crash
    (is (signals-error (asm-run '(v i) '((lref v) (lref i) (svref) (ret)) 5 0)))
    (is-equal 20 (asm-run '(v i) '((:unsafe) (lref v) (lref i) (svrefx) (ret)) v 1))
    (is-equal 7 (asm-run '(v i) '((:unsafe) (lref v) (lref i) (const 7) (svsetx) (ret)) v 0))
    (is-equal 7 (svref v 0)))
  (is-equal #\b (asm-run '(s i) '((lref s) (lref i) (schar) (ret)) "abc" 1))
  (is-equal 1 (asm-run '(c) '((lref c) (chktype cons) (carx) (ret)) '(1 2)))
  (is-equal '(2) (asm-run '(c) '((lref c) (chktype cons) (cdrx) (ret)) '(1 2)))
  (is-equal 5 (asm-run '(c) '((lref c) (chktype cons) (lset c) (lref c) (const 5) (ret)) '(1))))

(defasm-test asm-unsafe-directive
  ;; the stack and control checks still run under (:unsafe); only the type proofs are skipped
  (is-equal 7 (asm-run '(a b) '((:unsafe) (lref a) (lref b) (addfx) (ret)) 3 4))
  (is-equal 7 (asm-run '(a b) '((:unsafe t) (lref a) (lref b) (addfx) (ret)) 3 4))
  (is (asm-fail-message '(a b) '((:unsafe nil) (lref a) (lref b) (addfx) (ret))))
  (is (asm-fails-with "underflow" '(a) '((:unsafe) (addfx) (ret))))
  (is (asm-fails-with "slots 0 to" '(a) '((:unsafe) (lref 5) (ret))))
  (is (asm-fails-with "depth" '(a) '((:unsafe) (lref a) (jnil l) (const 1) l (ret)))))

;;; ---- the verifier -------------------------------------------------------------------------------------------------

(defasm-test asm-verifier-stack-and-slots
  (is (asm-fails-with "underflow" '() '((pop) (const 1) (ret))))
  (is (asm-fails-with "underflow" '() '((add) (ret))))
  (is (asm-fails-with "underflow" '() '((const 1) (add) (ret))))
  (is (asm-fails-with "underflow" '() '((ret))))
  (is (asm-fails-with "underflow" '() '((const 1) (call 1) (ret))))
  (is (asm-fails-with "underflow" '() '((const 1) (slide 1) (ret))))
  (is (asm-fails-with "underflow" '() '((svset) (ret))))
  (is (asm-fails-with "only slots 0 to 0" '(x) '((lref 1) (ret))))
  (is (asm-fails-with "only slots 0 to -1" '() '((lref 0) (ret))))
  (is (asm-fails-with "lset" '(x) '((const 1) (lset 1) (const 2) (ret))))      ; slot 1 is the value being stored
  (is (asm-fails-with "lset" '(x) '((const 1) (lset 5) (const 2) (ret))))
  (is (asm-fails-with "not a variable" '(x) '((lref y) (ret))))
  (is (asm-fails-with "negative" '(x) '((lref -1) (ret))))
  (is (asm-fails-with "there are only 1" '() '((const 1) (add) (ret))))
  (is (asm-fails-with "there is nothing" '() '((pop) (ret))))
  ;; (:var) names the top slot at that point
  (is-equal 3 (asm-run '() '((const 1) (:var a) (const 2) (:var b) (lref a) (lref b) (add) (ret))))
  (is (asm-fails-with "nothing on the stack" '() '((:var a) (const 1) (ret))))
  (is (asm-fails-with "already stands" '() '((const 1) (:var a) (const 2) (:var a) (ret))))
  (is (asm-fails-with "twice" '(a a) '((const 1) (ret)))))

(defasm-test asm-verifier-control-flow
  (is (asm-fails-with "run off the end" '(x) '((lref x))))
  (is (asm-fails-with "run off the end" '(x) '((lref x) (jnil l) (const 1) (ret) l)))
  (is (asm-fails-with "run off the end" '() '((const 1) (pop))))
  (is (asm-fails-with "not defined" '(x) '((lref x) (jmp nowhere))))
  (is (asm-fails-with "defined twice" '(x) '(a (const 1) a (ret))))
  (is (asm-fails-with "depth does not agree" '(x) '((lref x) (jnil l) (const 1) l (const 2) (ret))))
  (is (asm-fails-with "depth does not agree" '(x) '(top (const 1) (jmp top))))                ; a loop that grows the stack
  (is (asm-fails-with "no instructions" '() '()))
  (is (asm-fails-with "no instructions" '() '("only documentation")))
  (is (asm-fails-with "no instructions" '() '(here (:unsafe))))
  (is (asm-fails-with "must be a label" '() '((jmp 3))))
  ;; loops, joins and dead code are fine
  (is-equal 10 (asm-run '(n) '((const 0) (:var s) top (lref n) (const 0) (numeq) (jnnil end)
                              (lref s) (lref n) (add) (lset s) (lref n) (const 1) (sub) (lset n) (jmp top) end (lref s) (ret)) 4))
  (is-equal 1 (asm-run '() '((const 1) (ret) (const 2) (ret))))
  (is-equal 1 (asm-run '() '((jmp over) (const 2) (ret) over (const 1) (ret)))))

(defasm-test asm-verifier-dynamic-bindings
  (is (asm-fails-with "dynamic binding" '() '((const 1) (bind *asm-test-special*) (const 2) (ret))))
  (is (asm-fails-with "dynamic binding" '() '((const 1) (bind *asm-test-special*) (gfun list) (tailcall 0))))
  (is (asm-fails-with "only 0 dynamic" '() '((unbind 1) (const 2) (ret))))
  (is (asm-fails-with "only 1 dynamic" '() '((const 1) (bind *asm-test-special*) (unbind 2) (const 2) (ret))))
  (is (asm-fails-with "dynamic bindings does not agree" '(x) '((lref x) (jnil l) (const 1) (bind *asm-test-special*) l (const 3) (ret))))
  (is-equal 2 (asm-run '() '((const 1) (bind *asm-test-special*) (const 2) (bind *asm-test-special*) (unbind 1) (unbind 1) (const 2) (ret)))))

(defasm-test asm-verifier-types
  (is (asm-fails-with "chkfix" '(a b) '((lref a) (lref b) (addfx) (ret))))
  (is (asm-fails-with "chkfix" '(a) '((lref a) (incfx) (ret))))
  (is (asm-fails-with "chkfix" '(a) '((lref a) (ashfx 1) (ret))))
  (is (asm-fails-with "chkfix" '(a) '((lref a) (fixtofl) (ret))))
  (is (asm-fails-with "chkfix" '(a b) '((lref a) (lref b) (ltfx) (ret))))
  (is (asm-fails-with "double-float" '(a b) '((lref a) (lref b) (addfl) (ret))))
  (is (asm-fails-with "double-float" '(a b) '((lref a) (chktype integer) (lref b) (chktype double-float) (addfl) (ret))))
  (is (asm-fails-with "cons" '(a) '((lref a) (carx) (ret))))
  (is (asm-fails-with "cons" '(a) '((lref a) (cdrx) (ret))))
  (is (asm-fails-with "cons" '(a) '((lref a) (unbox) (ret))))
  (is (asm-fails-with "cons" '(a b) '((lref a) (lref b) (boxset) (ret))))
  (is (asm-fails-with "svrefx" '(a b) '((lref a) (lref b) (svrefx) (ret))))
  (is (asm-fails-with "svsetx" '(a b) '((lref a) (lref b) (const 1) (svsetx) (ret))))
  ;; a check anywhere on the value's path proves it; reading it into a slot keeps the type
  (is-equal 4 (asm-run '(a) '((lref a) (chkfix) (lset a) (lref a) (const 3) (addfx) (ret)) 1))
  (is-equal 8 (asm-run '(a) '((lref a) (chkrange 0 10) (lset a) (lref a) (lref a) (addfx) (ret)) 4))
  (is-equal 3 (asm-run '() '((const 1) (const 2) (addfx) (ret))))                      ; literals are known fixnums
  (is-equal 3.5 (asm-run '() '((const 1.5) (const 2.0) (addfl) (ret))))
  (is-equal 2 (asm-run '() '((const 1) (box) (unbox) (const 1) (add) (ret))))           ; BOX gives a known cons
  (is-equal 5 (asm-run '() '((const 5) (dup) (box) (pop) (chkfix) (const 0) (addfx) (ret))))
  ;; types join at labels: both paths fixnum is fine, one unknown path is not
  (is-equal 4 (asm-run '(x) '((lref x) (jnil a) (const 1) (jmp b) a (const 2) b (const 3) (addfx) (ret)) t))
  (is-equal 5 (asm-run '(x) '((lref x) (jnil a) (const 1) (jmp b) a (const 2) b (const 3) (addfx) (ret)) nil))
  (is (asm-fails-with "chkfix" '(x) '((lref x) (jnil a) (const 1) (jmp b) a (lref x) b (const 3) (addfx) (ret))))
  ;; a loop that assigns an unknown value into a slot loses the proof
  (is (asm-fails-with "chkfix" '(x) '((const 0) (:var i) top (lref i) (const 1) (addfx) (lset i) (lref x) (lset i) (jmp top))))
  (is-equal 3 (asm-run '() '((const 0) (:var i) top (lref i) (const 3) (numeqfx) (jnnil done)
                            (lref i) (incfx) (lset i) (jmp top) done (lref i) (ret)))))

(defasm-test asm-verifier-syntax
  (is (asm-fails-with "not an instruction" '() '((frobnicate) (ret))))
  (is (asm-fails-with "Unknown directive" '() '((:nonsense) (ret))))
  (is (asm-fails-with "neither a label" '() '(42 (ret))))
  (is (asm-fails-with "neither a label" '() '(nil (ret))))
  (is (asm-fails-with "neither a label" '() '(("x") (ret))))
  (is (asm-fails-with "keyword" '() '(:top (ret))))
  (is (asm-fails-with "first item" '() '((const 1) "doc" (ret))))
  (is (asm-fails-with "takes 0 operand" '() '((ret 1))))
  (is (asm-fails-with "takes 1 operand" '() '((const) (ret))))
  (is (asm-fails-with "takes 1 operand" '() '((const 1 2) (ret))))
  (is (asm-fails-with "takes 2 operand" '() '((const 1) (chkrange 1) (ret))))
  (is (asm-fails-with "slot number or a name" '(x) '((lref "x") (ret))))
  (is (asm-fails-with "must be a symbol" '() '((gref 1) (ret))))
  (is (asm-fails-with "non-negative" '() '((const 1) (slide -1) (ret))))
  (is (asm-fails-with "non-negative" '() '((gfun list) (call x) (ret))))
  (is (asm-fails-with "integer" '(a) '((lref a) (ashfx 1.5) (ret))))
  (is (asm-fails-with "chktype knows" '(a) '((lref a) (chktype widget) (ret))))
  (is (asm-fails-with "chktype knows" '(a) '((lref a) (chktype 3) (ret))))
  (is (asm-fails-with "fixnum bounds" '(a) '((lref a) (chkrange a b) (ret))))
  (is (asm-fails-with "lower bound" '(a) '((lref a) (chkrange 5 1) (ret))))
  (is (asm-fails-with "(:lambda" '() '((const 1) (closure foo 1) (ret))))
  (is (asm-fails-with "(:lambda" '() '((closure (lambda (x) x) 0) (ret))))
  (is (asm-fails-with "non-negative" '() '((closure (:lambda () (const 1) (ret)) -1) (ret))))
  (is (asm-fails-with "nested lambda" '() '((const 1) (closure (:lambda () (lref 3) (ret)) 1) (ret))))
  (is (asm-fails-with "nested lambda" '() '((closure (:lambda (x) (fref 0) (ret)) 0) (ret))))
  (is (asm-fails-with "captures 2" '() '((const 1) (const 2) (closure (:lambda () (:free a) (fref a) (ret)) 2) (ret))))
  (is (asm-fails-with "captured variable" '() '((const 1) (fref 0) (ret))))
  (is (asm-fails-with "only meaningful" '() '((:free a) (const 1) (ret))))
  (is (asm-fails-with "name for a captured" '() '((closure (:lambda () (:free 1) (const 1) (ret)) 0) (ret))))
  (is (asm-fails-with "takes one variable" '() '((const 1) (:var) (ret))))
  (is (asm-fails-with "takes one variable" '() '((const 1) (:var 3) (ret)))))

(defasm-test asm-lambda-list-errors
  (dolist (ll '((&key a) (&aux a) (a &allow-other-keys) ((a 1)) (&optional (a 1)) (&rest) (&rest a b) (&optional a &optional b) (a &rest b &optional c) (:a) (1) (nil)))
    (is (handler-case (progn (asm-assemble 'f ll '((const 1) (ret))) nil) (asm-error () t))))
  (is (handler-case (progn (asm-assemble 'f 'not-a-list '((const 1) (ret))) nil) (asm-error () t))))

(defasm-test asm-errors-carry-positions
  (let ((e (handler-case (asm-assemble 'asm-test-bad '(x) '((lref x) (pop) (pop) (pop) (ret))) (asm-error (e) e))))
    (is-equal 3 (asm-error-index e))
    (is-equal '(pop) (asm-error-item e))
    (is-equal 'asm-test-bad (asm-error-routine e))
    (is (%string-search "Assembly error in ASM-TEST-BAD" (princ-to-string e) 0)))
  ;; a documentation string counts as item 0
  (is-equal 4 (asm-error-index (handler-case (asm-assemble 'f '(x) '("doc" (lref x) (pop) (pop) (pop) (ret))) (asm-error (e) e))))
  ;; an error in a nested lambda points at the closure instruction
  (let ((e (handler-case (asm-assemble 'f '() '((const 1) (closure (:lambda () (pop) (pop) (ret)) 1) (ret))) (asm-error (e) e))))
    (is-equal 1 (asm-error-index e))
    (is (%string-search "nested lambda" (asm-error-message e) 0)))
  (is-equal nil (asm-error-index (handler-case (asm-assemble 'f '(&key a) '((const 1) (ret))) (asm-error (e) e)))))

(defasm-test asm-warnings
  (multiple-value-bind (fn warns) (asm-assemble 'f '() '((const 1) (ret) (const 2) (ret)))
    (is-equal 1 (funcall fn))
    (is-equal '(2 3) (mapcar #'car warns))
    (is (%string-search "Unreachable" (cdar warns) 0)))
  (multiple-value-bind (fn warns) (asm-assemble 'f '() '((const 1) (const 2) (ret)))
    (is-equal 2 (funcall fn))
    (is (%string-search "extra value" (cdar warns) 0)))
  (multiple-value-bind (fn warns) (asm-assemble 'f '() '((const 1) (ret)))
    (declare (ignore fn))
    (is-equal nil warns))
  (multiple-value-bind (fn warns) (asm-assemble 'f '() '(cons (const 1) (ret)))
    (declare (ignore fn))
    (is (%string-search "name of an instruction" (cdar warns) 0))))

;;; ---- listing and decompiling --------------------------------------------------------------------------------------------

(defun asm-listing-round-trip-p (fn)
  "True if the listing of FN assembles to a function with the same listing."
  (let* ((txt (asm-listing fn nil :addresses nil))
         (form (let ((*package* (find-package "CL"))) (read-from-string txt))))
    (and (eq (car form) 'defasm)
         (string-equal txt (asm-listing (asm-assemble (cadr form) (caddr form) (cdddr form)) nil :addresses nil)))))

(defasm-test asm-listing-and-decompile
  (defasm asm-test-sum (n)
    "Sum 1..n."
    (const 0) (:var s)
    top (lref n) (const 0) (le) (jnnil done)
    (lref s) (lref n) (add) (lset s)
    (lref n) (const 1) (sub) (lset n)
    (jmp top)
    done (lref s) (ret))
  (let ((txt (asm-listing 'asm-test-sum)))
    (is (%string-search "(defasm ASM-TEST-SUM (N)" txt 0))
    (is (%string-search "\"Sum 1..n.\"" txt 0))
    (is (%string-search "(jnnil l25)" txt 0))
    (is (%string-search "; 0007" txt 0))                           ; addresses as comments
    (is (not (%string-search "(:unsafe)" txt 0))))
  (is (not (%string-search "; 0007" (asm-listing 'asm-test-sum nil :addresses nil) 0)))
  (is (stringp (asm-listing 'asm-test-sum)))
  (is-equal nil (asm-listing 'asm-test-sum (make-string-output-stream)))
  (let* ((form (asm-decompile 'asm-test-sum)))
    (is-equal 'defasm (car form))
    (is-equal 'asm-test-sum (cadr form))
    (is-equal '(n) (caddr form))
    (is-equal "Sum 1..n." (cadddr form))
    ;; the decompiled form assembles to a working function
    (is-equal 55 (funcall (asm-assemble (cadr form) (caddr form) (cdddr form)) 10)))
  ;; code the compiler produced
  (compile nil '(lambda (x) x))
  (let ((f (compile nil '(lambda (x &optional (y 3) &rest z) (let ((k (lambda (a) (+ a x y)))) (funcall k (length z)))))))
    (is (asm-listing-round-trip-p f))
    (is (%string-search "&OPTIONAL" (asm-listing f) 0))
    (is (%string-search "(closure (:lambda" (asm-listing f) 0))
    (is-equal 6 (funcall f 1 2 3 4 5)))
  (is (asm-listing-round-trip-p (compile nil '(lambda (n) (let ((s 0)) (dotimes (i n s) (incf s i)))))))
  (is (asm-listing-round-trip-p (compile nil '(lambda (a b) (declare (fixnum a b) (optimize (safety 0))) (the fixnum (+ a (* b 2)))))))   ; needs (:unsafe)
  (is (%string-search "(:unsafe)" (asm-listing (compile nil '(lambda (a b) (declare (fixnum a b) (optimize (safety 0))) (the fixnum (+ a (* b 2)))))) 0))
  (is (asm-listing-round-trip-p (compile nil '(lambda (l) (mapcar (lambda (e) (* e e)) l)))))
  (is (asm-listing-round-trip-p (symbol-function 'asm-test-sum)))
  (is (signals-error (asm-listing 42)))
  (is (signals-error (asm-decompile '(lambda (x) x)))))

(defasm-test asm-listing-of-many-system-functions
  ;; a deterministic sample of the compiled functions in the system must decompile and assemble back to the same thing
  (let ((n 0) (bad nil) (i 0))
    (do-all-symbols (s)
      (when (and (fboundp s) (not (macro-function s)) (not (special-operator-p s)) (functionp (symbol-function s)) (ignore-errors (%code-parts (symbol-function s))))
        (setq i (+ i 1))
        (when (= 0 (mod i 9))
          (let ((txt (asm-listing (symbol-function s) nil :addresses nil)))
            (unless (%string-search "captures" txt 0)
              (setq n (+ n 1))
              (unless (asm-listing-round-trip-p (symbol-function s)) (push s bad)))))))
    (is (> n 300))
    (is-equal nil bad)))

;;; ---- ASM mode in the editor ---------------------------------------------------------------------------------------

(defun asm-ed ()
  (multiple-value-bind (wm ed) (make-editor-wm)
    (setf (buffer-mode (ed-buffer ed)) 'asm-mode)
    (values wm ed (ed-buffer ed))))

(defun asm-set-text (b text) (buffer-set-text b text) (buf-goto b 0 0))
(defun asm-buffer-lines (b) (coerce (buffer-lines b) 'list))

(defasm-test asm-mode-basics
  (is-equal 'asm-mode (file-mode-for "/tmp/x/prog.ngasm"))
  (is-equal 'asm-mode (file-mode-for "PROG.NGASM"))
  (is-equal 'lisp-mode (file-mode-for "prog.lisp"))
  (is-equal "Assembly" (mode-display-name 'asm-mode))
  (is (eq *asm-mode-map* (mode-keymap 'asm-mode)))
  (is-equal '((0 3 :comment)) (asm-line-syntax "; x" nil))
  (is-equal '((4 5 :comment)) (asm-line-syntax "    ;" nil))
  (is-equal '((1 7 :builtin) (8 11 :defname)) (asm-line-syntax "(defasm foo (x)" nil))
  (is-equal '((1 6 :keyword)) (asm-line-syntax "(:free" nil))
  (is-equal '((1 5 :builtin) (6 7 :constant) (10 21 :comment))
            (asm-line-syntax "(lref 0)  ; a comment" nil))
  (is-equal '((2 6 :defname)) (asm-line-syntax "  loop" nil))
  (is-equal '((1 6 :builtin) (7 12 :string) (14 23 :comment)) (asm-line-syntax "(const \"a b\") ; comment" nil))
  (is-equal '((0 2 :string)) (asm-line-syntax "x\" y" :string))
  (multiple-value-bind (spans state) (asm-line-syntax "(const \"open" nil)
    (is-equal :string state)
    (is-equal '(:string) (mapcar #'caddr (remove :builtin spans :key #'caddr))))
  (is-equal '((1 4 :builtin) (5 6 :constant)) (asm-line-syntax "(jmp 3)" nil))
  (is-equal '((1 4 :builtin) (5 8 :constant)) (asm-line-syntax "(add 1.5" nil))
  (multiple-value-bind (wm ed b) (asm-ed)
    (declare (ignore wm b))
    (run-editor-command ed 'lisp-mode nil)
    (run-editor-command ed 'asm-mode nil)
    (is-equal 'asm-mode (buffer-mode (ed-buffer ed)))
    (is (%string-search "Assembly" (ed-message ed) 0))))

(defparameter *asm-demo-text*
  "(defasm sq (x)
\"Square.\"
  (lref x)   ; the argument
          (lref    x )
 (mul)  ; multiply
    top
(ret))
")

(defasm-test asm-mode-formatting
  (multiple-value-bind (wm ed b) (asm-ed)
    (declare (ignore wm))
    (asm-set-text b *asm-demo-text*)
    (run-editor-command ed 'asm-format-region nil)
    (is-equal (list "(defasm sq (x)"
                    "  \"Square.\""
                    (asm-test-line "    (lref x)" "; the argument")
                    "    (lref x)"
                    (asm-test-line "    (mul)" "; multiply")
                    "  top"
                    "    (ret))"
                    "")
              (asm-buffer-lines b))
    ;; formatting is idempotent
    (let ((once (buffer-string b)))
      (run-editor-command ed 'asm-format-region nil)
      (is-equal once (buffer-string b)))
    (is (%string-search "Formatted" (ed-message ed) 0))
    ;; the formatted routine still assembles
    (run-editor-command ed 'compile-buffer nil)
    (is-equal 49 (sq 7)))
  ;; a long instruction keeps two spaces before its comment; comment-only lines are indented like instructions; ;;; goes to column 0
  (multiple-value-bind (wm ed b) (asm-ed)
    (declare (ignore wm))
    (asm-set-text b "(defasm f (x)
(const \"a string that is quite long indeed\") ; c
; note
;;; section
(ret))")
    (run-editor-command ed 'asm-format-region nil)
    (is-equal (list "(defasm f (x)"
                    "    (const \"a string that is quite long indeed\")  ; c"
                    "    ; note"
                    ";;; section"
                    "    (ret))")
              (asm-buffer-lines b)))
  ;; strings keep their spacing; blanks inside are not collapsed, and a string spanning lines is left alone
  (multiple-value-bind (wm ed b) (asm-ed)
    (declare (ignore wm))
    (asm-set-text b "(defasm f (x)
\"a    doc
   with  two lines\"
(const \"a    b\")
(ret))")
    (run-editor-command ed 'asm-format-region nil)
    (is-equal (list "(defasm f (x)"
                    "  \"a    doc"
                    "   with  two lines\""
                    "    (const \"a    b\")"
                    "    (ret))")
              (asm-buffer-lines b)))
  ;; a region formats only its lines
  (multiple-value-bind (wm ed b) (asm-ed)
    (declare (ignore wm))
    (asm-set-text b "(defasm f (x)
(lref x)
(ret))")
    (buf-goto b 1 0) (setf (buffer-mark b) (cons 1 0)) (buf-goto b 1 3)
    (run-editor-command ed 'asm-format-region nil)
    (is-equal (list "(defasm f (x)" "    (lref x)" "(ret))") (asm-buffer-lines b))))

(defasm-test asm-mode-keys-and-indentation
  (multiple-value-bind (wm ed b) (asm-ed)
    ;; typing a routine: RET indents the next line by its role
    (ed-type wm "(defasm cube (x)")
    (ed-keys wm "<return>")
    (ed-type wm "\"Cube.\"")
    (ed-keys wm "<return>")
    (is-equal 4 (point-col b))                                           ; the next line starts as an instruction
    (ed-type wm "(lref x)")
    (ed-keys wm "<return>")
    (ed-type wm "(dup)")
    (ed-keys wm "<return>")
    (ed-type wm "(dup)")
    (ed-keys wm "<return>")
    (ed-type wm "(mul)")
    (ed-keys wm "<return>")
    (ed-type wm "(mul)")
    (ed-keys wm "<return>")
    (ed-type wm "(ret))")
    (is-equal (list "(defasm cube (x)" "  \"Cube.\"" "    (lref x)" "    (dup)" "    (dup)" "    (mul)" "    (mul)" "    (ret))")
              (asm-buffer-lines b))
    ;; TAB on a mis-indented line, and on a label
    (buf-goto b 3 4) (buf-delete b 3 0 3 4) (buf-goto b 3 0)
    (ed-keys wm "<tab>")
    (is-equal "    (dup)" (buf-line b 3))
    (is-equal 4 (point-col b))
    (buf-goto b 0 0) (buf-goto b 7 0)
    (buf-goto b 5 0) (buf-insert b "loop") (buf-insert b (string #\Newline))
    (buf-goto b 5 2)
    (ed-keys wm "<tab>")
    (is-equal "  loop" (buf-line b 5))
    ;; M-; starts a comment in the comment column, then aligns it
    (buf-goto b 3 0)
    (ed-keys wm "M-;")
    (is-equal *asm-comment-column* (position #\; (buf-line b 3)))
    (ed-type wm "duplicate")
    (is-equal (asm-test-line "    (dup)" "; duplicate") (buf-line b 3))
    ;; C-j works like RET
    (buf-goto b 7 0)
    (buf-goto b (point-line b) (length (buf-current-line b)))
    (ed-keys wm "C-j")
    (is-equal 4 (point-col b))))

(defasm-test asm-mode-completion-and-help
  (multiple-value-bind (wm ed b) (asm-ed)
    (declare (ignore wm))
    (asm-set-text b "    (lre")
    (buf-goto b 0 8)
    (run-editor-command ed 'asm-complete-mnemonic nil)
    (is-equal "    (lref" (buf-line b 0))
    (is (%string-search "slot" (ed-message ed) 0))
    ;; an ambiguous prefix completes as far as it can and lists the candidates
    (asm-set-text b "    (addf")
    (buf-goto b 0 9)
    (run-editor-command ed 'asm-complete-mnemonic nil)
    (is (%string-search "addfl" (ed-message ed) 0))
    (is (%string-search "addfx" (ed-message ed) 0))
    (asm-set-text b "    (lt")
    (buf-goto b 0 7)
    (run-editor-command ed 'asm-complete-mnemonic nil)
    (is (%string-search "ltfl" (ed-message ed) 0))
    (asm-set-text b "    (zzz")
    (buf-goto b 0 8)
    (run-editor-command ed 'asm-complete-mnemonic nil)
    (is (%string-search "No instruction starts with" (ed-message ed) 0))
    ;; C-c ? describes the instruction at point
    (asm-set-text b "    (chkrange 1 5)")
    (buf-goto b 0 8)
    (run-editor-command ed 'asm-describe-instruction nil)
    (is (%string-search "chkrange lo hi" (ed-message ed) 0))
    (is (%string-search "fixnum" (ed-message ed) 0))
    (asm-set-text b "    (not-an-op)")
    (buf-goto b 0 8)
    (run-editor-command ed 'asm-describe-instruction nil)
    (is (ed-minibuf ed))))

(defparameter *asm-two-routines*
  "(defasm asm-ed-double (x)
  \"Twice x.\"
    (lref x) (lref x) (add) (ret))

(defasm asm-ed-triple (x)
    (lref x) (lref x) (add) (lref x) (add) (ret))
")

(defasm-test asm-mode-compiling
  (multiple-value-bind (wm ed b) (asm-ed)
    (declare (ignore wm))
    (asm-set-text b *asm-two-routines*)
    ;; C-c C-c compiles the routine at point
    (buf-goto b 1 5)
    (run-editor-command ed 'compile-region nil)
    (is (%string-search "asm-ed-double assembled" (ed-message ed) 0))
    (is-equal 8 (funcall 'asm-ed-double 4))
    (is-equal "Twice x." (documentation 'asm-ed-double 'function))
    (buf-goto b 5 5)
    (run-editor-command ed 'asm-compile-routine nil)
    (is-equal 12 (funcall 'asm-ed-triple 4))
    ;; a region compiles the routines inside it
    (fmakunbound 'asm-ed-double) (fmakunbound 'asm-ed-triple)
    (buf-goto b 0 0) (setf (buffer-mark b) (cons 0 0)) (buf-goto b 7 0)
    (run-editor-command ed 'compile-region nil)
    (is (%string-search "asm-ed-double, asm-ed-triple assembled" (ed-message ed) 0))
    (is-equal 6 (funcall 'asm-ed-double 3))
    ;; the whole buffer, with ordinary forms evaluated as they come
    (fmakunbound 'asm-ed-double)
    (asm-set-text b (%string-concat "(defvar *asm-ed-var* 5)
" *asm-two-routines*))
    (run-editor-command ed 'compile-buffer nil)
    (is-equal 9 (funcall 'asm-ed-triple 3))
    (is-equal 5 (symbol-value '*asm-ed-var*))
    (is-equal nil *asm-diagnostics*)))

(defasm-test asm-mode-diagnostics
  (multiple-value-bind (wm ed b) (asm-ed)
    (declare (ignore wm))
    (asm-set-text b "(defasm asm-ed-bad (x)
  \"Doc.\"
    (lref x)
    (pop)
    (pop)
    (pop)
    (ret))

(defasm asm-ed-ok (x)
    (lref x) (ret))
")
    (run-editor-command ed 'compile-buffer nil)
    (is (%string-search "1 error" (ed-message ed) 0))
    (is (%string-search "line 6" (ed-message ed) 0))                     ; the offending instruction, not the routine's first line
    (is (%string-search "underflow" (ed-message ed) 0))
    (is-equal 1 (asm-ed-ok 1))                                           ; the good routine was assembled all the same
    (is (not (fboundp 'asm-ed-bad)))
    (is-equal 1 (length *asm-diagnostics*))
    ;; C-c C-w goes to the instruction
    (buf-goto b 0 0)
    (run-editor-command ed 'edit-compiler-warnings nil)
    (is-equal 5 (point-line b))
    (is-equal 4 (point-col b))
    ;; fixing the routine clears the diagnostics
    (buf-goto b 5 0) (buf-delete b 5 0 5 9) (buf-goto b 5 0) (buf-insert b "    (const 1)")
    (run-editor-command ed 'compile-buffer nil)
    (is (%string-search "assembled" (ed-message ed) 0))
    (is-equal nil *asm-diagnostics*)
    (run-editor-command ed 'edit-compiler-warnings nil)
    (is (%string-search "No assembler diagnostics" (ed-message ed) 0)))
  ;; a syntax error in the form is reported where the reader stopped; other forms still compile
  (multiple-value-bind (wm ed b) (asm-ed)
    (declare (ignore wm))
    (asm-set-text b "(defasm asm-ed-reader (x)
    (lref x)
    (const #<unreadable>)
    (ret))
(defasm asm-ed-fine () (const 1) (ret))
")
    (run-editor-command ed 'compile-buffer nil)
    (is (%string-search "error" (ed-message ed) 0))
    (is-equal 1 (funcall 'asm-ed-fine))
    (is-equal 1 (length *asm-diagnostics*)))
  ;; warnings are listed but do not stop the routine being assembled
  (multiple-value-bind (wm ed b) (asm-ed)
    (declare (ignore wm))
    (asm-set-text b "(defasm asm-ed-dead ()
    (const 1)
    (ret)
    (const 2)
    (ret))
")
    (run-editor-command ed 'compile-buffer nil)
    (is (%string-search "assembled" (ed-message ed) 0))
    (is (%string-search "2 warnings" (ed-message ed) 0))
    (is-equal 1 (funcall 'asm-ed-dead))
    (is-equal :warning (cadddr (car *asm-diagnostics*)))
    (run-editor-command ed 'edit-compiler-warnings nil)
    (is-equal 3 (point-line b))
    (setq *asm-diagnostics* nil))
  ;; in a Lisp buffer the same commands do what they did before
  (multiple-value-bind (wm ed) (make-editor-wm)
    (declare (ignore wm))
    (setf (buffer-mode (ed-buffer ed)) 'lisp-mode)
    (asm-set-text (ed-buffer ed) "(defun asm-ed-lisp-fn () 77)")
    (run-editor-command ed 'compile-buffer nil)
    (is-equal 77 (funcall 'asm-ed-lisp-fn))))

(defasm-test asm-mode-listing-run-and-find
  (multiple-value-bind (wm ed b) (asm-ed)
    (declare (ignore wm))
    (asm-set-text b *asm-two-routines*)
    (buf-goto b 5 5)
    ;; not yet assembled
    (fmakunbound 'asm-ed-triple)
    (run-editor-command ed 'asm-disassemble-routine nil)
    (is (%string-search "not assembled yet" (ed-message ed) 0))
    (run-editor-command ed 'asm-run-routine nil)
    (is (%string-search "not assembled yet" (ed-message ed) 0))
    (run-editor-command ed 'compile-buffer nil)
    ;; C-c C-a shows the listing in an ASM mode buffer
    (buf-goto b 5 5)
    (run-editor-command ed 'asm-disassemble-routine nil)
    (is-equal "*Assembly Listing*" (buffer-name (ed-buffer ed)))
    (is-equal 'asm-mode (buffer-mode (ed-buffer ed)))
    (is (%string-search "(defasm ASM-ED-TRIPLE (X)" (buffer-string (ed-buffer ed)) 0))
    (ed-switch-to-buffer ed b)
    ;; C-c C-x asks for arguments and calls the routine
    (buf-goto b 1 5)
    (run-editor-command ed 'asm-run-routine nil)
    (is (ed-minibuf ed))
    (funcall (mb-callback (ed-minibuf ed)) "21")
    (is (%string-search "42" (ed-message ed) 0))
    ;; M-. finds a routine in the buffer and labels in the routine
    (asm-set-text b "(defasm asm-ed-find-a (x)
  top
    (lref x) (jnil top) (const 1) (ret))

(defasm asm-ed-find-b (x)
    (gfun asm-ed-find-a) (lref x) (tailcall 1))
")
    (buf-goto b 5 10)
    (run-editor-command ed 'asm-edit-definition nil)
    (is-equal 0 (point-line b))
    (buf-goto b 2 20)
    (run-editor-command ed 'asm-edit-definition nil)
    (is-equal 1 (point-line b))
    (is-equal 2 (point-col b))))

(defasm-test asm-mode-bindings
  (dolist (k '(("<tab>" asm-tab) ("<return>" asm-newline-and-indent) ("C-j" asm-newline-and-indent) ("M-;" asm-align-comment)
               ("C-c C-f" asm-format-region) ("C-M-q" asm-format-routine) ("M-<tab>" asm-complete-mnemonic) ("C-c ?" asm-describe-instruction)
               ("C-c C-c" compile-region) ("C-c C-b" compile-buffer) ("C-M-x" asm-compile-routine) ("C-c C-w" edit-compiler-warnings)
               ("C-c C-a" asm-disassemble-routine) ("C-c C-x" asm-run-routine) ("M-." asm-edit-definition)
               ("C-M-a" asm-beginning-of-routine) ("C-M-e" asm-end-of-routine)))
    (is-equal (cadr k) (lookup-key (list *asm-mode-map*) (parse-key-sequence (car k)))))
  (dolist (c '(asm-mode asm-tab asm-newline-and-indent asm-align-comment asm-format-region asm-format-routine asm-complete-mnemonic
               asm-describe-instruction asm-compile-routine asm-disassemble-routine asm-run-routine asm-edit-definition
               asm-beginning-of-routine asm-end-of-routine))
    (is (get c '%ecommand))
    (is (member c *editor-commands*))))

(defasm-test asm-mode-format-routine
  (multiple-value-bind (wm ed b) (asm-ed)
    (declare (ignore wm))
    (asm-set-text b "(defasm a (x)
(lref x)
(ret))

(defasm b (x)
(lref x)
(ret))
")
    (buf-goto b 5 2)
    (run-editor-command ed 'asm-format-routine nil)
    (is-equal (list "(defasm a (x)" "(lref x)" "(ret))" "" "(defasm b (x)" "    (lref x)" "    (ret))" "") (asm-buffer-lines b))
    ;; C-M-a and C-M-e find the routine even when its instructions start in column 0
    (buf-goto b 1 3)
    (run-editor-command ed 'asm-beginning-of-routine nil)
    (is-equal '(0 . 0) (cons (point-line b) (point-col b)))
    (run-editor-command ed 'asm-end-of-routine nil)
    (is-equal '(2 . 6) (cons (point-line b) (point-col b)))))

;;; ---- the manual ---------------------------------------------------------------------------------------------------------------

(defasm-test asm-manual
  (let ((d (find-document :bytecode-manual)))
    (is d)
    (when d
      (let ((text (doc-text d 200)))
        ;; every instruction is documented, and so are the language's directives and the editor mode
        (dolist (m (asm-mnemonics))
          (is (%string-search (%string-concat "(" m) text 0))
          (is (%string-search m text 0)))
        (dolist (word '("defasm" "asm-assemble" "asm-listing" "asm-decompile" ":unsafe" ":var" ":free" "verifier" "asm-error"
                        "ASM mode" "C-c C-c" "bytecode-manual" "tail call" "dynamic binding" "multiple values"))
          (is (%string-search word text 0)))))))
