%%%%%%%%%%%%%%%%%%%%%%%%%
%                       %
%   !   : Negation      %
%   &   : Conjuction    %
%   |   : Disjunction   %
%   >   : Implication   %
%   =   : Equivalence   %
%                       %
%%%%%%%%%%%%%%%%%%%%%%%%%

convert(Sentence, Res) :-
    atom_chars(Sentence, Atoms),
    filter_space(Atoms, NoSpaces),
    eliminate(NoSpaces, Res).

eliminate(Atoms, Res) :-
    translate_parens(Atoms, Parens),
    translate_equivalence(Parens, Equiv),
    translate_implication(Equiv, Impl),
    translate_disjunction(Impl, Dis),
    translate_conjunction(Dis, Con),
    translate_negation(Con, Negations),
    reconstruct(Negations, Res).

translate_parens([], []).
translate_parens(['(' | []], Res) :- throw(mismatched_oparen).
translate_parens([')' | []], Res) :- throw(mismatched_cparen).
translate_parens([Head | Tail], [Head | Res]) :- translate_parens(Tail, Res).
translate_parens(['(' | Tail], Res) :-
    translate_parens_(Tail, ParensContent, Rest),
    translate_parens(Rest, SubRes),
    append([ParensContent], SubRes, Res).

paren_content
translate_parens_(Atoms, ParensContent, Rest) :-


filter_space([], []).
filter_space([' ' | Tail], Res) :- filter_space(Tail, Res).
filter_space([Head | Tail], [Head | Res]) :- filter_space(Tail, Res).


