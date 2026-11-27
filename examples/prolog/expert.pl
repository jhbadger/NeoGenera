% expert.pl -- a small rule-based identifier: observed features are asserted as facts and the
% rules chain backwards from the conclusions.

:- dynamic observed/1.

identify(Features, Animal) :-
    retractall(observed(_)),
    forall(member(F, Features), assertz(observed(F))),
    animal(Animal), !.

animal(cheetah)   :- mammal, carnivore, has(tawny_colour), has(dark_spots).
animal(tiger)     :- mammal, carnivore, has(tawny_colour), has(black_stripes).
animal(giraffe)   :- ungulate, has(long_neck), has(dark_spots).
animal(zebra)     :- ungulate, has(black_stripes).
animal(ostrich)   :- bird, \+ flies, has(long_neck).
animal(penguin)   :- bird, \+ flies, swims, has(black_and_white).
animal(albatross) :- bird, flies_well.

mammal    :- has(hair) ; has(gives_milk).
bird      :- has(feathers) ; flies, lays_eggs.
carnivore :- has(eats_meat) ; has(pointed_teeth), has(claws), has(forward_eyes).
ungulate  :- mammal, ( has(hooves) ; chews_cud ).
chews_cud :- has(chews_cud).
flies     :- has(flies).
flies_well :- has(flies_well).
lays_eggs :- has(lays_eggs).
swims     :- has(swims).

has(F) :- observed(F).
