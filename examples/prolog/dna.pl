% dna.pl -- translating DNA with a grammar.  The genetic code is one string indexed by the bases
% in the order T C A G; reverse_complement is the Lisp function of the bioinformatics extensions.

genetic_code("FFLLSSSSYY**CC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG").

base_index(t, 0).  base_index(c, 1).  base_index(a, 2).  base_index(g, 3).

codon_amino([B1, B2, B3], Amino) :-
    base_index(B1, I1), base_index(B2, I2), base_index(B3, I3),
    N is 16*I1 + 4*I2 + I3,
    genetic_code(Table),
    sub_atom(Table, N, 1, _, Amino).

protein([A|As]) --> codon(A), !, protein(As).
protein([]) --> [].

codon(Amino) --> [B1, B2, B3], { codon_amino([B1, B2, B3], Amino) }.

% translate(+DnaText, -ProteinAtom): the first reading frame, stopping at a stop codon.
translate(Dna, Protein) :-
    string_lower(Dna, Lower),
    atom_chars(Lower, Bases),
    phrase(protein(Aminos), Bases, _),
    take_until_stop(Aminos, Kept),
    atomic_list_concat(Kept, Protein).

take_until_stop([], []).
take_until_stop(['*'|_], []) :- !.
take_until_stop([A|As], [A|Bs]) :- take_until_stop(As, Bs).

% the other strand, through the Lisp function
reverse_strand(Dna, Reverse) :-
    lisp_call(reverse_complement, [Dna], Reverse).

gc_percent(Dna, Percent) :-
    atom_chars(Dna, Bases),
    include([B]>>memberchk(B, ['G', 'C', g, c]), Bases, Gc),
    length(Gc, N), length(Bases, Total),
    Percent is 100 * N / Total.
