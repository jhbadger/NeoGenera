% queens.pl -- N queens by generate and test, placing one queen per column.

queens(N, Qs) :-
    numlist(1, N, Rows),
    place(Rows, [], Qs).

place([], Qs, Qs).
place(Rows, Placed, Qs) :-
    select(Q, Rows, Rest),
    safe(Q, Placed, 1),
    place(Rest, [Q|Placed], Qs).

safe(_, [], _).
safe(Q, [Q1|Qs], D) :-
    Q =\= Q1 + D,
    Q =\= Q1 - D,
    D1 is D + 1,
    safe(Q, Qs, D1).

count_solutions(N, Count) :-
    aggregate_all(count, queens(N, _), Count).

print_board(Qs) :-
    length(Qs, N),
    forall(member(Q, Qs),
           ( forall(between(1, N, C),
                    ( C =:= Q -> write(' Q') ; write(' .') )),
             nl )).
