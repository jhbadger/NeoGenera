C -*- Mode: FORTRAN; Package: FTN-USER -*-
C     Factorials: FORTRAN integers here have no size limit.
      PROGRAM FACTOR
      INTEGER N, F, I
      DO 20 N = 5, 30, 25
         F = 1
         DO 10 I = 2, N
            F = F * I
   10    CONTINUE
         WRITE (*, 100) N, F
   20 CONTINUE
  100 FORMAT (' Factorial ', I2, ' is ', I33)
      END
