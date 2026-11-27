% expr.pl -- a definite clause grammar for arithmetic: precedence, parentheses and evaluation.

expr(V) --> term(T), expr_rest(T, V).

expr_rest(Acc, V) --> "+", !, term(T), { A1 is Acc + T }, expr_rest(A1, V).
expr_rest(Acc, V) --> "-", !, term(T), { A1 is Acc - T }, expr_rest(A1, V).
expr_rest(V, V) --> [].

term(V) --> factor(F), term_rest(F, V).

term_rest(Acc, V) --> "*", !, factor(F), { A1 is Acc * F }, term_rest(A1, V).
term_rest(Acc, V) --> "/", !, factor(F), { A1 is Acc / F }, term_rest(A1, V).
term_rest(V, V) --> [].

factor(V) --> "(", expr(V), ")".
factor(V) --> digits(Ds), { Ds \== [], number_codes(V, Ds) }.

digits([D|T]) --> digit(D), !, digits(T).
digits([]) --> [].

digit(D) --> [D], { code_type(D, digit) }.

calc(Text, Value) :-
    string_codes(Text, Codes),
    phrase(expr(Value), Codes).
