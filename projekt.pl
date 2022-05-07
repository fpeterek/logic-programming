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
    write(Equiv), nl,
    Res = Equiv.
    % translate_implication(Equiv, Impl),
    % translate_disjunction(Impl, Dis),
    % translate_conjunction(Dis, Con),
    % translate_negation(Con, Negations),
    % reconstruct(Negations, Res).

% translate_equivalence(Terms, Res)

translate_parens([], []).
translate_parens(['(' | []], _) :- throw(mismatched_oparen).
translate_parens([')' | _], _) :- throw(mismatched_cparen).

translate_parens(['(' | Tail], Res) :-
    translate_inner_parens(Tail, ParensContent, Rest),
    translate_parens(Rest, SubRes),
    append([ParensContent], SubRes, Res).

translate_parens([Head | Tail], [Head | Res]) :- translate_parens(Tail, Res).

translate_inner_parens([], _, _) :- throw(missing_paren).

translate_inner_parens(['(' | Tail], ParensContent, Rest) :-
    translate_inner_parens(Tail, SubContent, SubRest),
    translate_inner_parens(SubRest, SubContent2, Rest),
    append([SubContent], SubContent2, ParensContent).

translate_inner_parens([')' | Tail], [], Tail).

translate_inner_parens([Head | Atoms], [Head | ParensContent], Rest) :-
    translate_inner_parens(Atoms, ParensContent, Rest).


filter_space([], []).
filter_space([' ' | Tail], Res) :- filter_space(Tail, Res).
filter_space([Head | Tail], [Head | Res]) :- filter_space(Tail, Res).


