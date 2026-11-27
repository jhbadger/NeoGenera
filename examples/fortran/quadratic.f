C -*- Mode: FORTRAN; Package: FTN-USER -*-
C     The roots of A*X**2 + B*X + C, real or complex.
      PROGRAM QUADRATIC
      REAL A, B, C, D
      COMPLEX X1, X2
      READ (*, *) A, B, C
      D = B*B - 4.0*A*C
      IF (D .GE. 0.0) THEN
         X1 = CMPLX((-B + SQRT(D)) / (2.0*A), 0.0)
         X2 = CMPLX((-B - SQRT(D)) / (2.0*A), 0.0)
      ELSE
         X1 = CMPLX(-B / (2.0*A), SQRT(-D) / (2.0*A))
         X2 = CONJG(X1)
      END IF
      PRINT *, 'The roots are', X1, ' and', X2
      END
