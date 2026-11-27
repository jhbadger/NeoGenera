% family.pl -- family relations: facts, rules, recursion and findall.

parent(tom, bob).
parent(tom, liz).
parent(bob, ann).
parent(bob, pat).
parent(pat, jim).

male(tom).  male(bob).  male(jim).
female(liz).  female(ann).  female(pat).

father(X, Y) :- parent(X, Y), male(X).
mother(X, Y) :- parent(X, Y), female(X).

grandparent(X, Z) :-
    parent(X, Y),
    parent(Y, Z).

sibling(X, Y) :-
    parent(P, X),
    parent(P, Y),
    X \== Y.

ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

descendants(X, Ds) :- findall(D, ancestor(X, D), Ds).

% How many generations separate an ancestor from a descendant?
generations(X, X, 0).
generations(X, Y, N) :-
    parent(X, Z),
    generations(Z, Y, N0),
    N is N0 + 1.
