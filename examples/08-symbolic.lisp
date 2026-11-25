;;; Differentiation, integration and polynomial arithmetic.
;;; expect: d/dx
(dolist (e '((expt x 3) (* (sin x) (exp x)) (/ 1 (+ 1 (expt x 2))) (log (* 2 x))))
  (format t "d/dx ~A = ~A~%" (expr->string e) (expr->string (differentiate e 'x))))
(format t "integral of x^2 + 2 = ~A~%" (expr->string (integrate '(+ (expt x 2) 2) 'x)))
(format t "integral of sin(x) from 0 to pi = ~,6F~%" (definite-integral '(sin x) 'x 0 pi))
(format t "expand (x+1)^3 = ~A~%" (expr->string (expand '(expt (+ x 1) 3))))
(format t "taylor series of exp(x): ~A~%" (expr->string (taylor-series '(exp x) 'x 0 4)))
(multiple-value-bind (q r) (poly-divide '(-1 0 0 1) '(-1 1)) (format t "(x^3 - 1) / (x - 1) = ~A remainder ~A~%" q r))
