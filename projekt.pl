%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                         %
%    !   : Negation       %
%    &   : Conjuction     %
%    |   : Disjunction    %
%    >   : Implication    %
%    =   : Equivalence    %
%                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Start the program
run :-
    write('Input: '), nl,
    read(In),
    process(In).

% Print true on EOF to avoid making the user sad
process(end_of_file).

% Process one line of user input, print the result to stdout
% process('x & b > c').
process(In) :-
    In \== end_of_file,
    catch(convert(In, Res), Err, write(Err)),
    write(Res), nl, nl,
    run.

% Eliminate implication and equivalence operators, convert the result to a string
% return the result using the Res variable
% convert('x & b > c', Res).
convert(Sentence, Res) :-
    atom_chars(Sentence, Atoms),
    eliminate(Atoms, Tree),
    flatten(Tree, AtomRes),
    atomics_to_string(AtomRes, Res).

% Eliminates implications and equivalences, however, unlike convert, eliminate
% accepts a list of atoms, not a string, a returns a tree representation
% eliminate(['a', '&', 'b'], Res).
eliminate(Atoms, Res) :-
    filter_space(Atoms, NoSpaces),
    translate_parens(NoSpaces, Parens),
    translate_equivalence(Parens, ParseTree),
    rm_unnecessary_nesting(ParseTree, SimplifiedTree),
    validate(SimplifiedTree),
    elim_opers(SimplifiedTree, Res).

% Accepts a tree representation and flattens it to atomic chars
% flatten(['&', ['>', 'a', 'b'], 'c'], Flat)

flatten(['(', Tree], Res) :-
    flatten(Tree, FlatTree),
    append(['('], FlatTree, LeftPar),
    append(LeftPar, [')'], Res).

flatten(['!', Tree], Res) :-
    flatten(Tree, FlatTree),
    append(['!'], FlatTree, Res).

flatten([Oper, LeftTree, RightTree], Res) :-
    flatten(LeftTree, FlatLeft),
    flatten(RightTree, FlatRight),

    append(FlatLeft, [' ', Oper, ' '], SubRes),
    append(SubRes, FlatRight, Res).

flatten(Atom, [Atom]).

/***********************************************/
/*                                             */
/*                 Equivalence                 */
/*                                             */
/***********************************************/

% Parses a list of atoms into a tree representation
% translate_equivalence(['a', '&', 'b', '>', 'c'], Res).

translate_equivalence(Terms, Res) :- translate_equivalence_([], Terms, Res).

% Inner helper function with a helper aggregator, should not be called by itself
% Expects to be provided with an empty list as its firts argument

translate_equivalence_([], ['=' | _], _) :- throw(missing_operand_equiv).
translate_equivalence_(_, ['='], _) :- throw(missing_operand_equiv).

translate_equivalence_(Left, ['=' | Right], Res) :-
    translate_implication(Left, LeftRes),
    eliminate(Right, RightRes),
    append(['='], [LeftRes], SubRes),
    append(SubRes, [RightRes], Res).

translate_equivalence_(Left, [Head | Right], Res) :-
    append(Left, [Head], L),
    translate_equivalence_(L, Right, Res).

translate_equivalence_(Left, [], Res) :-
    translate_implication(Left, Res).

/***********************************************/
/*                                             */
/*                 Implication                 */
/*                                             */
/***********************************************/

% Parses a list of atoms into a tree representation - does not parse equivalences
% Assumes all equivalence operators have been removed by now
% translate_implication(['a', '&', 'b', '>', 'c'], Res).

translate_implication(Terms, Res) :- translate_implication_([], Terms, Res).

% Inner helper function with a helper aggregator, should not be called by itself
% Expects to be provided with an empty list as its firts argument

translate_implication_([], ['>' | _], _) :- throw(missing_operand_impl).
translate_implication_(_, ['>'], _) :- throw(missing_operand_impl).

translate_implication_(Left, ['>' | Right], Res) :-
    translate_disjunction(Left, LeftRes),
    eliminate(Right, RightRes),
    append(['>'], [LeftRes], SubRes),
    append(SubRes, [RightRes], Res).

translate_implication_(Left, [Head | Right], Res) :-
    append(Left, [Head], L),
    translate_implication_(L, Right, Res).

translate_implication_(Left, [], Res) :-
    translate_disjunction(Left, Res).

/***********************************************/
/*                                             */
/*                 Disjunction                 */
/*                                             */
/***********************************************/

% Parses a list of atoms into a tree representation
% Assumes all equivalence and implication operators have been removed by now
% translate_disjunction(['a', '&', 'b', '>', 'c'], Res).

translate_disjunction(Terms, Res) :- translate_disjunction_([], Terms, Res).

% Inner helper function with a helper aggregator, should not be called by itself
% Expects to be provided with an empty list as its firts argument

translate_disjunction_([], ['|' | _], _) :- throw(missing_operand_disj).
translate_disjunction_(_, ['|'], _) :- throw(missing_operand_disj).

translate_disjunction_(Left, ['|' | Right], Res) :-
    translate_conjunction(Left, LeftRes),
    eliminate(Right, RightRes),
    append(['|'], [LeftRes], SubRes),
    append(SubRes, [RightRes], Res).

translate_disjunction_(Left, [Head | Right], Res) :-
    append(Left, [Head], L),
    translate_disjunction_(L, Right, Res).

translate_disjunction_(Left, [], Res) :-
    translate_conjunction(Left, Res).

/***********************************************/
/*                                             */
/*                 Conjunction                 */
/*                                             */
/***********************************************/

% Parses a list of atoms into a tree representation
% Assumes all operators of lower precedence have already been parsed
% translate_conjunction(['a', '&', 'b', '>', 'c'], Res).

translate_conjunction(Terms, Res) :- translate_conjunction_([], Terms, Res).

% Inner helper function with a helper aggregator, should not be called by itself
% Expects to be provided with an empty list as its firts argument

translate_conjunction_([], ['&' | _], _) :- throw(missing_operand_conj).
translate_conjunction_(_, ['&'], _) :- throw(missing_operand_conj).

translate_conjunction_(Left, ['&' | Right], Res) :-
    translate_negation(Left, LeftRes),
    eliminate(Right, RightRes),
    append(['&'], [LeftRes], SubRes),
    append(SubRes, [RightRes], Res).

translate_conjunction_(Left, [Head | Right], Res) :-
    append(Left, [Head], L),
    translate_conjunction_(L, Right, Res).

translate_conjunction_(Left, [], Res) :-
    translate_negation(Left, Res).

/***********************************************/
/*                                             */
/*                  Negation                   */
/*                                             */
/***********************************************/

% Parses a list of atoms into a tree representation
% Assumes all operators of lower precedence have already been parsed
% translate_negation(['a', '&', 'b', '>', 'c'], Res).

translate_negation(Terms, Res) :- translate_negation_(Terms, Res).

% Inner helper function, should not be called by itself

translate_negation_([], []).

translate_negation_(['!'], _) :- throw(missing_operand_neg).

translate_negation_(['!', Oper], Res) :-
    append(['!'], [Oper], Res).

translate_negation_([Head | Right], Res) :-
    translate_negation_(Right, SubRes),
    append([Head], SubRes, Res).


/***********************************************/
/*                                             */
/*                 Parenthesis                 */
/*                                             */
/***********************************************/

% Parses a list of atoms into a tree representation
% Handles statements in parenthesis. Parenthesis are represented in the tree
% using a special construction to provide the ability to reparenthesise the statements
% which the user wrapped in parenthesis in the input statement
% translate_parens(['(', 'a', '&', 'b', ')', '>', 'c'], Res).

translate_parens([], []).
translate_parens(['(' | []], _) :- throw(mismatched_oparen).
translate_parens([')' | _], _) :- throw(mismatched_cparen).

translate_parens(['(' | Tail], Res) :-
    translate_inner_parens(Tail, ParensContent, Rest),
    eliminate(ParensContent, ParsedParens),
    translate_parens(Rest, SubRes),

    append(['('], [ParsedParens], ParenObject),

    append([ParenObject], SubRes, Res).

translate_parens([Head | Tail], [Head | Res]) :- translate_parens(Tail, Res).

% Inner function which handles nested parenthesis - should not be called
% by itself

translate_inner_parens([], _, _) :- throw(missing_paren).

translate_inner_parens(['(' | Tail], ParensContent, Rest) :-
    translate_inner_parens(Tail, SubContent, SubRest),
    eliminate(SubContent, ParsedSub),
    translate_inner_parens(SubRest, SubContent2, Rest),

    append(['('], [ParsedSub], ParenObject),

    append([ParenObject], SubContent2, ParensContent).

translate_inner_parens([')' | Tail], [], Tail).

translate_inner_parens([Head | Atoms], [Head | ParensContent], Rest) :-
    translate_inner_parens(Atoms, ParensContent, Rest).

/***********************************************/
/*                                             */
/*                 Validator                   */
/*                                             */
/***********************************************/

% Validates syntax and ensures the input is valid
% validate(['+', 'a', 'b']).

validate([]) :- throw(empty_statement).

validate([Oper, Left, Right]) :- 
    (
        Oper = '=';
        Oper = '>';
        Oper = '|';
        Oper = '&'
    ),
    validate(Left),
    validate(Right).

validate([Oper, _, _, _ | _]) :- 
    (
        Oper = '=';
        Oper = '>';
        Oper = '|';
        Oper = '&'
    ),
    throw(extraneous_operand).


validate(['!', Right]) :- validate(Right).
validate(['(', Right]) :- validate(Right).

validate(['=', _]) :- throw(missing_operand_equiv).
validate(['>', _]) :- throw(missing_operand_impl).
validate(['|', _]) :- throw(missing_operand_disj).
validate(['&', _]) :- throw(missing_operand_conj).

validate(['=']) :- throw(missing_operand_equiv).
validate(['>']) :- throw(missing_operand_impl).
validate(['|']) :- throw(missing_operand_disj).
validate(['&']) :- throw(missing_operand_conj).
validate(['!']) :- throw(missing_operand_neg).
validate(['(']) :- throw(empty_statement).

validate('=') :- throw(invalid_atom_name).
validate('>') :- throw(invalid_atom_name).
validate('|') :- throw(invalid_atom_name).
validate('&') :- throw(invalid_atom_name).
validate('!') :- throw(invalid_atom_name).
validate('(') :- throw(invalid_atom_name).

validate([_, _ | _]) :- throw(missing_operator).

validate(_).

/***********************************************/
/*                                             */
/*            Operator Elimination             */
/*                                             */
/***********************************************/

% Accepts a tree representation of the input statement and eliminates
% equivalence and implication operators
% elim_opers(['=', ['!', 'a'], 'b'], Result).

elim_opers(['=', Left, Right], Res) :-
    elim_opers(Left, EL),
    elim_opers(Right, ER),

    append(['('], [EL], ParEL),
    append(['('], [ER], ParER),

    append(['&'], [ParEL], Sub1),
    append(Sub1, [ParER], And),
    append(['('], [And], AndPar),

    append(['|'], [ParEL], Non1),
    append(Non1, [ParER], Non2),
    append(['('], [Non2], NonPar),
    append(['!'], [NonPar], Non),


    append(['|'], [AndPar], SubRes),
    append(SubRes, [Non], Elim),
    append(['('], [Elim], Res).

elim_opers(['>', Left, Right], Res) :-
    elim_opers(Left, Ant),
    elim_opers(Right, Cons),

    append(['('], [Cons], ParenCons),
    append(['('], [Ant], ParenAnt),

    append(['!'], [ParenAnt], NonAnt),

    append(['|'], [NonAnt], SubRes),
    append(SubRes, [ParenCons], Elim),

    append(['('], [Elim], Res).

elim_opers(Any, Any).

/***********************************************/
/*                                             */
/*              Helper Functions               */
/*                                             */
/***********************************************/

% Recursively flattens lists which only contain one element into the only element
% Otherwise, returns the input list
% rm_unnecessary_nesting([['a']], 'a')
% rm_unnecessary_nesting([['a']], Flattened)

rm_unnecessary_nesting([], _) :- throw(empty_statement).
rm_unnecessary_nesting([Tree], Res) :- rm_unnecessary_nesting(Tree, Res).
rm_unnecessary_nesting(Tree, Res) :- rm_unnecessary_nesting_(Tree, Res).

rm_unnecessary_nesting_([], []).
rm_unnecessary_nesting_([Head | Tail], Res) :-
    rm_unnecessary_nesting(Head, Left),
    rm_unnecessary_nesting_(Tail, Right),
    append([Left], Right, Res).

rm_unnecessary_nesting_(Atom, Atom).

% Filters space characters from input as whitespace characters have no effect
% on the meaning of a term
% Input is a list of atomic chars
% filter_space(['a', ' ', '=', ' ', ' ', 'b', ' '], NoSpaces)

filter_space([], []).
filter_space([' ' | Tail], Res) :- filter_space(Tail, Res).
filter_space([Head | Tail], [Head | Res]) :- filter_space(Tail, Res).


