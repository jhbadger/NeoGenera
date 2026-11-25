;;; Solving, decomposing and checking.
;;; expect: reconstruction error
(let* ((a (mat '((4 -2 1) (-2 4 -2) (1 -2 4)))) (b #(11 -16 17)))
  (format t "solution: ~A~%" (matrix-solve a b))
  (format t "determinant: ~,3F~%" (matrix-det a))
  (multiple-value-bind (values vectors) (eigen-symmetric a)
    (format t "eigenvalues: ~A~%" values)
    (format t "check A v = lambda v: ~A~%" (matrix-equal (mat* a vectors) (mat* vectors (diagonal-matrix values)) 1e-9)))
  (multiple-value-bind (u s v) (svd a)
    (format t "singular values: ~A~%" s)
    (format t "reconstruction error ~,2E~%" (matrix-norm (mat- a (mat* (mat* u (diagonal-matrix s)) (matrix-transpose v))))))
  (format t "~%inverse:~%~A" (matrix-string (matrix-inverse a) 4))
  (format t "roots of x^3 - 6x^2 + 11x - 6: ~A~%" (polynomial-roots '(1 -6 11 -6))))
