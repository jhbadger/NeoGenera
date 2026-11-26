{-*- Mode: PASCAL; Dialect: ISO; Package: PASCAL-USER -*- }
PROGRAM factor;
{ Integers are Lisp integers: the factorial never overflows. }
VAR x : integer;
  FUNCTION factorial (n : integer) : integer;
  VAR i : integer;
  BEGIN
    factorial := 1;
    FOR i := n DOWNTO 1 DO
      factorial := factorial * i
  END; { function factorial }
BEGIN
  writeln('Program calculates the factorial of a number.  Enter 0 to halt program.');
  REPEAT
    write('Find the factorial of what number? ');
    readln(x);
    writeln('Factorial ', x:1, ' is ', factorial(x):1)
  UNTIL x = 0;
  writeln('Leaving factorial program.')
END.
