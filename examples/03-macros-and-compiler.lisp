;;; Macros, the compiler and the disassembler.
;;; expect: 3628800
(defmacro while (test &body body) `(loop (unless ,test (return)) ,@body))
(defun count-down (n) (let ((acc nil)) (while (> n 0) (push n acc) (decf n)) (nreverse acc)))
(format t "~A~%" (count-down 5))
(defun fact (n) (if (<= n 1) 1 (* n (fact (- n 1)))))
(format t "10! = ~D~%" (fact 10))
(format t "macroexpansion: ~S~%" (macroexpand-1 '(while (< i 3) (incf i))))
(disassemble 'fact)
