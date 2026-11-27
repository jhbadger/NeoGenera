C -*- Mode: FORTRAN; Package: FTN-USER -*-
C     The sieve of Eratosthenes, with DO WHILE and a logical array.
      PROGRAM SIEVE
      PARAMETER (N = 50)
      LOGICAL PRIME(N)
      INTEGER P(N), I, J, K
      DO 10 I = 1, N
         PRIME(I) = .TRUE.
   10 CONTINUE
      PRIME(1) = .FALSE.
      I = 2
      DO WHILE (I*I .LE. N)
         IF (PRIME(I)) THEN
            DO 20 J = I*I, N, I
               PRIME(J) = .FALSE.
   20       CONTINUE
         END IF
         I = I + 1
      END DO
      K = 0
      DO 30 I = 1, N
         IF (PRIME(I)) THEN
            K = K + 1
            P(K) = I
         END IF
   30 CONTINUE
      WRITE (*, 100) N, (P(I), I = 1, K)
  100 FORMAT (' Primes up to ', I2, ':', /, (1X, 10I4))
      PRINT *, K, ' primes'
      END
