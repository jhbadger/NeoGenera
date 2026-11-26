{-*- Mode: PASCAL; Dialect: ISO; Package: PASCAL-USER -*- }
PROGRAM sieve;
{ Sets are Lisp integers used as bit masks, so a set may be as large as you like. }
CONST limit = 1000;
VAR primes : SET OF 2..limit;
    i, j, count : integer;
BEGIN
  primes := [2..limit];
  FOR i := 2 TO limit DO
    IF i IN primes THEN
    BEGIN
      j := i * i;
      WHILE j <= limit DO BEGIN primes := primes - [j]; j := j + i END
    END;
  count := 0;
  FOR i := 2 TO limit DO IF i IN primes THEN count := count + 1;
  writeln('There are ', count:1, ' primes below ', limit:1)
END.
