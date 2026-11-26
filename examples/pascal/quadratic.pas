{-*- Mode: PASCAL; Dialect: ISO; Package: PASCAL-USER -*- }
PROGRAM quadratic;
VAR a,b,c,discriminant,x1,x2 : real;
BEGIN
  writeln;
  write('Solves the equation: ');
  writeln('A*X**2 + B*X + C = 0 for X');
  write('Enter values for A, B, and C:  ');
  readln(a,b,c);
  discriminant := b*b - 4*a*c;
  x1 := (-b + sqrt(discriminant)) / (2*a);
  x2 := (-b - sqrt(discriminant)) / (2*a);
  writeln('The roots are:');
  writeln(x1,' and ',x2)
END.
