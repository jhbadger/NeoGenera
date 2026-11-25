;;; Numbers that never overflow, exact fractions, and formatted output.
;;; expect: 2432902008176640000
(defun fact (n) (if (<= n 1) 1 (* n (fact (- n 1)))))
(format t "20! = ~D~%" (fact 20))
(format t "100! has ~D digits~%" (length (princ-to-string (fact 100))))
(format t "2^100 = ~D~%" (expt 2 100))
(format t "1/3 + 1/6 = ~A~%" (+ 1/3 1/6))
(format t "pi to 10 places: ~,10F~%" pi)
(format t "~{~D~^, ~}~%" (loop for i from 1 to 10 collect (* i i)))
