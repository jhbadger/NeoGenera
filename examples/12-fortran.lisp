;;; The FORTRAN Tool Kit: ANSI FORTRAN 77 compiled into Lisp, sharing its integers and its functions.
;;; expect: Factorial 30 is 265252859812191058636308480000000
(f77-load-file "examples/fortran/factor.f")
(f77-load-file "examples/fortran/quadratic.f")
(f77-load-file "examples/fortran/sieve.f")

;; Run main programs; :input gives unit 5 (here a string stream instead of the keyboard).
(f77:execute 'ftn-user::factor)
(f77:execute 'ftn-user::quadratic :input (make-string-input-stream "1 0 -9"))
(f77:execute 'ftn-user::quadratic :input (make-string-input-stream "1 2 5"))
(f77:execute "sieve")

;; Subroutines and functions are Lisp functions in the package FTN-USER.  Arguments are references,
;; so a variable passed from Lisp is a cell made with F77:MAKE-REF; plain values are boxed for you.
(f77:compile-string "      SUBROUTINE ADDTO(X, Y)
      X = X + Y
      END
      INTEGER FUNCTION TWICE(K)
      INTEGER K
      TWICE = 2*K
      END
      SUBROUTINE FILL(V, N)
      INTEGER V(N)
      DO 1 I = 1, N
    1 V(I) = I*I
      END
")
(format t "~&From Lisp: twice 21 = ~D~%" (ftn-user::twice 21))
(let ((x (f77:make-ref 1.5)))
  (ftn-user::addto x 2.0)
  (format t "From Lisp: addto gave ~A~%" (f77:ref-value x)))
;; A region of FORTRAN number space, addressed by FORTRAN as an array.
(f77:with-fortran-number-data (squares 6)
  (ftn-user::fill squares 6)
  (format t "From Lisp: squares ~A~%" (loop for i below 6 collect (f77-ref-element squares i))))

;; Variables that were never given a value are detected; Init to Zero turns that off.
(f77:compile-string "      PROGRAM UNINIT
      INTEGER N
      PRINT *, N
      END
")
(handler-case (f77:execute 'ftn-user::uninit)
  (f77-runtime-error (e) (format t "~&Error: ~A~%" e)))
(f77:execute 'ftn-user::uninit :init-to-zero t)

;; Stronger type checks, reported with the line of the statement.
(multiple-value-bind (names errors)
    (f77:compile-string "      INTEGER I
      CHARACTER*5 S
      I = S
      END
")
  (declare (ignore names))
  (dolist (e errors) (format t "~&Compile error: ~A~%" e)))

;; Binary patterns and the Department of Defense bit functions.
(f77:compile-string "      PROGRAM BITS
      INTEGER I
      I = Z'F0'
      PRINT *, IAND(I, 255), IOR(I, 1), ISHFT(1, 70), IBITS(255, 2, 3)
      I(0:3) = 5
      PRINT *, I
      WRITE (*, '(Z8, O4, B6)') 255, 8, 5
      END
")
(f77:execute 'ftn-user::bits)
