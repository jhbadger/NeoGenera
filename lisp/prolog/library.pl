% library.pl -- the Prolog Tool Kit's library: lists, apply, pairs, ordered sets, assoc, error checking, DCG phrase.
% These predicates are ordinary Prolog; a user program may define its own versions, which replace them.

% ---- lists -------------------------------------------------------------------------------------------------------

append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).

append(ListOfLists, List) :- '$append_lists'(ListOfLists, List).
'$append_lists'([], []).
'$append_lists'([L|Ls], As) :- append(L, Ws, As), '$append_lists'(Ls, Ws).

member(X, [Y|Ys]) :- '$member_'(Ys, X, Y).
'$member_'(_, X, X).
'$member_'([Y|Ys], X, _) :- '$member_'(Ys, X, Y).

select(X, [X|T], T).
select(X, [H|T], [H|R]) :- select(X, T, R).

selectchk(X, L, R) :- select(X, L, R), !.

select(X, Xs, Y, Ys) :- '$select4'(Xs, X, Y, Ys).
'$select4'([X|T], X, Y, [Y|T]).
'$select4'([H|T], X, Y, [H|T2]) :- '$select4'(T, X, Y, T2).

exclude(_, [], []).
exclude(P, [X|Xs], R) :- ( call(P, X) -> R = R1 ; R = [X|R1] ), exclude(P, Xs, R1).

include(_, [], []).
include(P, [X|Xs], R) :- ( call(P, X) -> R = [X|R1] ; R = R1 ), include(P, Xs, R1).

partition(_, [], [], []).
partition(P, [H|T], I, E) :-
    (   call(P, H) -> I = [H|I1], E = E1 ; I = I1, E = [H|E1] ),
    partition(P, T, I1, E1).

partition(_, [], [], [], []).
partition(P, [H|T], L, E, G) :-
    call(P, Diff, H),
    '$partition6'(Diff, H, P, T, L, E, G).
'$partition6'(<, H, P, T, [H|L], E, G) :- partition(P, T, L, E, G).
'$partition6'(=, H, P, T, L, [H|E], G) :- partition(P, T, L, E, G).
'$partition6'(>, H, P, T, L, E, [H|G]) :- partition(P, T, L, E, G).

maplist(G, L) :- '$maplist1'(L, G).
'$maplist1'([], _).
'$maplist1'([X|Xs], G) :- call(G, X), '$maplist1'(Xs, G).

maplist(G, L1, L2) :- '$maplist2'(L1, L2, G).
'$maplist2'([], [], _).
'$maplist2'([X|Xs], [Y|Ys], G) :- call(G, X, Y), '$maplist2'(Xs, Ys, G).

maplist(G, L1, L2, L3) :- '$maplist3'(L1, L2, L3, G).
'$maplist3'([], [], [], _).
'$maplist3'([X|Xs], [Y|Ys], [Z|Zs], G) :- call(G, X, Y, Z), '$maplist3'(Xs, Ys, Zs, G).

maplist(G, L1, L2, L3, L4) :- '$maplist4'(L1, L2, L3, L4, G).
'$maplist4'([], [], [], [], _).
'$maplist4'([X|Xs], [Y|Ys], [Z|Zs], [W|Ws], G) :- call(G, X, Y, Z, W), '$maplist4'(Xs, Ys, Zs, Ws, G).

foldl(G, L, V0, V) :- '$foldl1'(L, G, V0, V).
'$foldl1'([], _, V, V).
'$foldl1'([X|Xs], G, V0, V) :- call(G, X, V0, V1), '$foldl1'(Xs, G, V1, V).

foldl(G, L1, L2, V0, V) :- '$foldl2'(L1, L2, G, V0, V).
'$foldl2'([], [], _, V, V).
'$foldl2'([X|Xs], [Y|Ys], G, V0, V) :- call(G, X, Y, V0, V1), '$foldl2'(Xs, Ys, G, V1, V).

foldl(G, L1, L2, L3, V0, V) :- '$foldl3'(L1, L2, L3, G, V0, V).
'$foldl3'([], [], [], _, V, V).
'$foldl3'([X|Xs], [Y|Ys], [Z|Zs], G, V0, V) :- call(G, X, Y, Z, V0, V1), '$foldl3'(Xs, Ys, Zs, G, V1, V).

nth0(I, L, E) :- integer(I), !, I >= 0, '$nth_det'(I, L, E).
nth0(I, L, E) :- var(I), !, '$nth_gen'(L, E, 0, I).
nth0(I, _, _) :- throw(error(type_error(integer, I), context(nth0/3, _))).

nth1(I, L, E) :- integer(I), !, I >= 1, I0 is I - 1, '$nth_det'(I0, L, E).
nth1(I, L, E) :- var(I), !, '$nth_gen'(L, E, 1, I).
nth1(I, _, _) :- throw(error(type_error(integer, I), context(nth1/3, _))).

'$nth_det'(0, [E|_], E) :- !.
'$nth_det'(I, [_|T], E) :- I1 is I - 1, '$nth_det'(I1, T, E).

'$nth_gen'([E|_], E, B, B).
'$nth_gen'([_|T], E, B0, B) :- B1 is B0 + 1, '$nth_gen'(T, E, B1, B).

nth0(I, L, E, R) :- integer(I), !, I >= 0, '$nth_del'(I, L, E, R).
nth0(I, L, E, R) :- var(I), '$nth_gen_del'(L, E, R, 0, I).
nth1(I, L, E, R) :- integer(I), !, I >= 1, I0 is I - 1, '$nth_del'(I0, L, E, R).
nth1(I, L, E, R) :- var(I), '$nth_gen_del'(L, E, R, 1, I).
'$nth_del'(0, [E|T], E, T) :- !.
'$nth_del'(I, [H|T], E, [H|R]) :- I1 is I - 1, '$nth_del'(I1, T, E, R).
'$nth_gen_del'([E|T], E, T, B, B).
'$nth_gen_del'([H|T], E, [H|R], B0, B) :- B1 is B0 + 1, '$nth_gen_del'(T, E, R, B1, B).

last([X|Xs], Last) :- '$last_'(Xs, X, Last).
'$last_'([], Last, Last).
'$last_'([X|Xs], _, Last) :- '$last_'(Xs, X, Last).

proper_length(List, Length) :- is_list(List), length(List, Length).

delete([], _, []).
delete([H|T], X, R) :- ( H \= X -> R = [H|R1] ; R = R1 ), delete(T, X, R1).

subtract([], _, []).
subtract([H|T], L, R) :- ( memberchk(H, L) -> R = R1 ; R = [H|R1] ), subtract(T, L, R1).

intersection([], _, []).
intersection([H|T], L, R) :- ( memberchk(H, L) -> R = [H|R1] ; R = R1 ), intersection(T, L, R1).

union([], L, L).
union([H|T], L, R) :- ( memberchk(H, L) -> R = R1 ; R = [H|R1] ), union(T, L, R1).

permutation([], []).
permutation(L, [H|T]) :- select(H, L, R), permutation(R, T).

flatten(List, Flat) :- '$flatten'(List, [], Flat0), !, Flat = Flat0.
'$flatten'(Var, Tl, [Var|Tl]) :- var(Var), !.
'$flatten'([], Tl, Tl) :- !.
'$flatten'([Hd|Tl], Tail, List) :- !, '$flatten'(Hd, FlatHeadTail, List), '$flatten'(Tl, Tail, FlatHeadTail).
'$flatten'(NonList, Tl, [NonList|Tl]).

max_member(Max, [H|T]) :- '$max_member'(T, H, Max).
'$max_member'([], M, M).
'$max_member'([H|T], M0, M) :- ( H @> M0 -> '$max_member'(T, H, M) ; '$max_member'(T, M0, M) ).

min_member(Min, [H|T]) :- '$min_member'(T, H, Min).
'$min_member'([], M, M).
'$min_member'([H|T], M0, M) :- ( H @< M0 -> '$min_member'(T, H, M) ; '$min_member'(T, M0, M) ).

nextto(X, Y, [X,Y|_]).
nextto(X, Y, [_|Zs]) :- nextto(X, Y, Zs).

predsort(P, L, Sorted) :-
    length(L, N),
    '$predsort'(P, N, L, _, Sorted1), !,
    Sorted = Sorted1.
'$predsort'(P, 2, [X1,X2|L], L, R) :- !,
    call(P, Delta, X1, X2),
    '$sort2'(Delta, X1, X2, R).
'$predsort'(_, 1, [X|L], L, [X]) :- !.
'$predsort'(_, 0, L, L, []) :- !.
'$predsort'(P, N, L1, L3, R) :-
    N1 is N // 2, plus(N1, N2, N),
    '$predsort'(P, N1, L1, L2, R1),
    '$predsort'(P, N2, L2, L3, R2),
    '$predmerge'(P, R1, R2, R).
'$sort2'(<, X1, X2, [X1,X2]).
'$sort2'(=, X1, _, [X1]).
'$sort2'(>, X1, X2, [X2,X1]).
'$predmerge'(_, [], R, R) :- !.
'$predmerge'(_, R, [], R) :- !.
'$predmerge'(P, [H1|T1], [H2|T2], Result) :-
    call(P, Delta, H1, H2), !,
    '$predmerge_'(Delta, P, H1, H2, T1, T2, Result).
'$predmerge_'(<, P, H1, H2, T1, T2, [H1|R]) :- '$predmerge'(P, T1, [H2|T2], R).
'$predmerge_'(=, P, H1, _, T1, T2, [H1|R]) :- '$predmerge'(P, T1, T2, R).
'$predmerge_'(>, P, H1, H2, T1, T2, [H2|R]) :- '$predmerge'(P, [H1|T1], T2, R).

% ---- pairs -------------------------------------------------------------------------------------------------------

pairs_keys_values([], [], []).
pairs_keys_values([K-V|T], [K|Ks], [V|Vs]) :- pairs_keys_values(T, Ks, Vs).
pairs_keys([], []).
pairs_keys([K-_|T], [K|Ks]) :- pairs_keys(T, Ks).
pairs_values([], []).
pairs_values([_-V|T], [V|Vs]) :- pairs_values(T, Vs).

% ---- ordered sets ------------------------------------------------------------------------------------------------

list_to_ord_set(L, S) :- sort(L, S).
ord_union(A, B, C) :- append(A, B, AB), sort(AB, C).
ord_union(ListOfSets, Set) :- append(ListOfSets, L), sort(L, Set).
ord_subtract(A, B, C) :- '$ord_subtract'(A, B, C).
'$ord_subtract'([], _, []).
'$ord_subtract'([H|T], B, R) :- ( '$ord_memberchk'(H, B) -> R = R1 ; R = [H|R1] ), '$ord_subtract'(T, B, R1).
ord_intersection(A, B, C) :- '$ord_intersection'(A, B, C).
'$ord_intersection'([], _, []).
'$ord_intersection'([H|T], B, R) :- ( '$ord_memberchk'(H, B) -> R = [H|R1] ; R = R1 ), '$ord_intersection'(T, B, R1).
ord_memberchk(X, L) :- '$ord_memberchk'(X, L).
'$ord_memberchk'(X, [Y|T]) :- compare(O, X, Y), ( O = (=) -> true ; O = (>) -> '$ord_memberchk'(X, T) ).
ord_insert(S, E, S2) :- ord_union(S, [E], S2).
ord_add_element(S, E, S2) :- ord_union(S, [E], S2).
ord_del_element(S, E, S2) :- ord_subtract(S, [E], S2).
ord_subset(A, B) :- ord_subtract(A, B, []).
ord_empty([]).

% ---- control and meta --------------------------------------------------------------------------------------------

apply(G, Args) :- G =.. L0, append(L0, Args, L1), G2 =.. L1, call(G2).

setup_call_cleanup(Setup, Goal, Cleanup) :-
    once(Setup),
    (   catch(Goal, E, true)
    ->  (   ignore(Cleanup), ( nonvar(E) -> throw(E) ; true ) )
    ;   (   ignore(Cleanup), fail )
    ).

call_cleanup(Goal, Cleanup) :- setup_call_cleanup(true, Goal, Cleanup).

phrase(G, L) :- phrase(G, L, []).
phrase(G, L, R) :- '$dcg_body'(G, S0, S, Goal), S0 = L, S = R, call(Goal).

subsumes_term(General, Specific) :-
    \+ \+ ( term_variables(Specific, V1), General = Specific, term_variables(V1, V2), V1 == V2 ).

dif(X, Y) :- X \== Y.

concat_atom(L, R) :- atomic_list_concat(L, R).
concat_atom(L, Sep, R) :- atomic_list_concat(L, Sep, R).

assertion(G) :- ( \+ \+ catch(G, _, fail) -> true ; throw(error(assertion_failed(G), _)) ).

% ---- error checking ----------------------------------------------------------------------------------------------

must_be(Type, X) :-
    (   '$has_type'(Type, X) -> true
    ;   var(X), Type \== var -> throw(error(instantiation_error, _))
    ;   '$must_be_error'(Type, X)
    ).

'$must_be_error'(positive_integer, X) :- integer(X), !, throw(error(type_error(positive_integer, X), _)).
'$must_be_error'(nonneg, X) :- integer(X), !, throw(error(type_error(nonneg, X), _)).
'$must_be_error'(positive_integer, X) :- !, throw(error(type_error(integer, X), _)).
'$must_be_error'(nonneg, X) :- !, throw(error(type_error(integer, X), _)).
'$must_be_error'(Type, X) :- throw(error(type_error(Type, X), _)).

is_of_type(Type, X) :- '$has_type'(Type, X).

'$has_type'(any, _).
'$has_type'(integer, X) :- integer(X).
'$has_type'(atom, X) :- atom(X).
'$has_type'(atomic, X) :- atomic(X).
'$has_type'(callable, X) :- callable(X).
'$has_type'(var, X) :- var(X).
'$has_type'(nonvar, X) :- nonvar(X).
'$has_type'(boolean, X) :- ( X == true ; X == false ), !.
'$has_type'(list, X) :- is_list(X).
'$has_type'(proper_list, X) :- is_list(X).
'$has_type'(list(Type), X) :- is_list(X), '$all_have_type'(X, Type).
'$has_type'(number, X) :- number(X).
'$has_type'(float, X) :- float(X).
'$has_type'(positive_integer, X) :- integer(X), X >= 1.
'$has_type'(nonneg, X) :- integer(X), X >= 0.
'$has_type'(string, X) :- string(X).
'$has_type'(compound, X) :- compound(X).
'$has_type'(ground, X) :- ground(X).
'$has_type'(between(L, U), X) :- number(X), X >= L, X =< U.

'$all_have_type'([], _).
'$all_have_type'([X|Xs], Type) :- '$has_type'(Type, X), '$all_have_type'(Xs, Type).

% ---- association lists (AVL trees) -------------------------------------------------------------------------------

empty_assoc(t).

get_assoc(Key, t(K, V, _, L, R), Val) :-
    compare(O, Key, K),
    '$get_assoc'(O, Key, V, L, R, Val).
'$get_assoc'(=, _, V, _, _, V).
'$get_assoc'(<, Key, _, L, _, V) :- get_assoc(Key, L, V).
'$get_assoc'(>, Key, _, _, R, V) :- get_assoc(Key, R, V).

put_assoc(Key, T, Val, New) :- '$put_assoc'(T, Key, Val, New).

'$put_assoc'(t, K, V, t(K, V, 1, t, t)).
'$put_assoc'(t(K0, V0, H, L, R), K, V, New) :-
    compare(O, K, K0),
    '$put_assoc'(O, K0, V0, H, L, R, K, V, New).

'$put_assoc'(=, K0, _, H, L, R, _, V, t(K0, V, H, L, R)).
'$put_assoc'(<, K0, V0, _, L, R, K, V, New) :-
    '$put_assoc'(L, K, V, L1),
    '$avl_balance'(K0, V0, L1, R, New).
'$put_assoc'(>, K0, V0, _, L, R, K, V, New) :-
    '$put_assoc'(R, K, V, R1),
    '$avl_balance'(K0, V0, L, R1, New).

'$avl_height'(t, 0).
'$avl_height'(t(_, _, H, _, _), H).

'$avl_node'(K, V, L, R, t(K, V, H, L, R)) :-
    '$avl_height'(L, HL), '$avl_height'(R, HR),
    H is max(HL, HR) + 1.

'$avl_balance'(K, V, L, R, T) :-
    '$avl_height'(L, HL), '$avl_height'(R, HR),
    D is HL - HR,
    (   D > 1
    ->  L = t(LK, LV, _, LL, LR),
        '$avl_height'(LL, HLL), '$avl_height'(LR, HLR),
        (   HLL >= HLR
        ->  '$avl_node'(K, V, LR, R, N1),
            '$avl_node'(LK, LV, LL, N1, T)
        ;   LR = t(LRK, LRV, _, LRL, LRR),
            '$avl_node'(LK, LV, LL, LRL, N1),
            '$avl_node'(K, V, LRR, R, N2),
            '$avl_node'(LRK, LRV, N1, N2, T)
        )
    ;   D < -1
    ->  R = t(RK, RV, _, RL, RR),
        '$avl_height'(RL, HRL), '$avl_height'(RR, HRR),
        (   HRR >= HRL
        ->  '$avl_node'(K, V, L, RL, N1),
            '$avl_node'(RK, RV, N1, RR, T)
        ;   RL = t(RLK, RLV, _, RLL, RLR),
            '$avl_node'(K, V, L, RLL, N1),
            '$avl_node'(RK, RV, RLR, RR, N2),
            '$avl_node'(RLK, RLV, N1, N2, T)
        )
    ;   '$avl_node'(K, V, L, R, T)
    ).

list_to_assoc(L, A) :- '$list_to_assoc'(L, t, A).
'$list_to_assoc'([], A, A).
'$list_to_assoc'([K-V|T], A0, A) :- put_assoc(K, A0, V, A1), '$list_to_assoc'(T, A1, A).

assoc_to_list(T, L) :- '$assoc_list'(T, L, []).
'$assoc_list'(t, L, L).
'$assoc_list'(t(K, V, _, Lt, Rt), L, T) :-
    '$assoc_list'(Lt, L, [K-V|M]),
    '$assoc_list'(Rt, M, T).

assoc_to_keys(A, Ks) :- assoc_to_list(A, L), pairs_keys(L, Ks).
assoc_to_values(A, Vs) :- assoc_to_list(A, L), pairs_values(L, Vs).

max_assoc(A, K, V) :- assoc_to_list(A, L), last(L, K-V).
min_assoc(A, K, V) :- assoc_to_list(A, [K-V|_]).

del_assoc(K, A, V, NA) :-
    get_assoc(K, A, V),
    assoc_to_list(A, L),
    '$del_pair'(L, K, L2),
    list_to_assoc(L2, NA).
'$del_pair'([K0-V0|T], K, R) :- ( K0 == K -> R = T ; R = [K0-V0|R1], '$del_pair'(T, K, R1) ).

% ---- yall lambda expressions --------------------------------------------------------------------------------------

'>>'(Ps, L) :- '$lambda'(Ps, L, []).
'>>'(Ps, L, A1) :- '$lambda'(Ps, L, [A1]).
'>>'(Ps, L, A1, A2) :- '$lambda'(Ps, L, [A1,A2]).
'>>'(Ps, L, A1, A2, A3) :- '$lambda'(Ps, L, [A1,A2,A3]).
'>>'(Ps, L, A1, A2, A3, A4) :- '$lambda'(Ps, L, [A1,A2,A3,A4]).
'>>'(Ps, L, A1, A2, A3, A4, A5) :- '$lambda'(Ps, L, [A1,A2,A3,A4,A5]).
'>>'(Ps, L, A1, A2, A3, A4, A5, A6) :- '$lambda'(Ps, L, [A1,A2,A3,A4,A5,A6]).

'$lambda'(Ps0, L0, Args) :-
    '$lambda_copy'(Ps0, L0, Ps, L),
    '$lambda_bind'(Ps, Args, Rest),
    '$lambda_call'(Rest, L).

'$lambda_copy'(Free/Ps0, L0, Ps, L) :- !, copy_term(Free/Ps0>>L0, Free/Ps>>L).
'$lambda_copy'(Ps0, L0, Ps, L) :- copy_term(Ps0>>L0, Ps>>L).

'$lambda_bind'([], Rest, Rest) :- !.
'$lambda_bind'([P|Ps], [A|As], Rest) :- !, P = A, '$lambda_bind'(Ps, As, Rest).
'$lambda_bind'(_, [], []).

'$lambda_call'([], L) :- !, call(L).
'$lambda_call'(Rest, L) :- G =.. [call, L|Rest], call(G).
