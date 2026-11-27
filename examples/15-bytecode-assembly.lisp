;;; The bytecode assembler: routines written as VM instructions, checked by a verifier, listed and edited as source.
;;; expect: gcd 1071 462 = 21
(load "examples/asm/numeric.ngasm")

(format t "~&gcd 1071 462 = ~D~%" (gcd2 1071 462))
(format t "fib 88 = ~D~%" (fib-fixnum 88))
(format t "sum = ~D~%" (vector-sum (vector 1 2 3 4 5.5)))
(let ((acc (make-accumulator 100)))
  (funcall acc 5)
  (format t "accumulator: ~D~%" (funcall acc 10)))
(format t "scaled-by 7 3 = ~D~%" (scaled-by 7 3))

;; A routine can be shown as source again, with the address of every instruction.
(format t "~%The listing of gcd2:~%")
(asm-listing 'gcd2 *standard-output*)

;; Mistakes are found when the routine is assembled, before it can run.  The unchecked fixnum add is refused
;; for operands the assembler cannot prove are fixnums, and a path that leaves the stack deeper than the
;; other is refused at the label where they meet.
(format t "~%What the verifier says:~%")
(dolist (bad '((() ((const 1) (add) (ret)))
               ((a b) ((lref a) (lref b) (addfx) (ret)))
               ((x) ((lref x) (jnil skip) (const 1) skip (ret)))
               ((x) ((lref x) (jmp nowhere)))))
  (handler-case (asm-assemble 'bad (first bad) (second bad))
    (asm-error (e) (format t "  ~A~%" (asm-error-message e)))))

;; Code the compiler produced can be read the same way, and edited into a routine of your own.
(defun triple (x) (* 3 x))
(compile 'triple)
(format t "~%The compiler's own code for (defun triple (x) (* 3 x)):~%")
(asm-listing 'triple *standard-output*)
