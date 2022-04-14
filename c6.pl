
%varia_rep(Počet prvků, [Z čeho vznikne], variace)
%?- varia_rep(4, [1, 2, 3, 4, 5, 6], X).

varia_rep(0, _, []).
varia_rep(N, L, [H | Tail]) :- 
    N > 0,
    N1 is N-1,
    del(H, L, _),
    varia_rep(N1, L, Tail).

%del(prvek, list, list bez prvku)

del(X, [X | T], T).
del(X, [H | T], [H | NT]) :- del(X, T, NT).

solution(Secret) :- 
    generate(S), 
    solution_(Secret, S, [1,1,2,2]).

solution_(Secret, _, Guess) :- 
    get_eval(Secret, Guess, 4, 0), 
    write('Secret odhalen: '), write(Guess), nl.

solution_(Secret, S, Guess) :-
    get_eval(Secret, Guess, B, W),
    filterS(S, Guess, B, W, NewS),
    length(NewS, L),
    write('Délka S je: '), write(L), nl,
    get_evals(Guess, NewS, E),
    next_guess(NewS, E, NextGuess),
    solution_(Secret, NewS, NextGuess).

get_evals(_, [], []).
get_evals(Guess, [Hs | Ts], [[B, W] | Te]) :-
    get_eval(Guess, Hs, B, W),
    get_evals(Guess, Ts, Te).

% --- Score ------------------------------------------------

% minimum deleted for every evaluation
% zjisti score, což je minimalni pocet odstranenych prvku z S pro kazdou
% evaluaci
score(Code,S,E,MinDeleted):- dfae(Code,S,E,Deleted),
                             sort(Deleted,[MinDeleted|_]).

%--- Next guess ------------------------------------------------

% vybere prvek s maximalnim poctem odstranenych prvku z S
next_guess(S,E,NextGuess):-next_guess_(S,S,E,ScoreList),
                           sort(ScoreList,Sorted),
                           reverse(Sorted,[[NextGuess|_]|_]).

next_guess_([],_,_,[]).
next_guess_([H|Ss],S,E,[[H,MinDeleted]|ScoreList]):- score(H,S,E,MinDeleted),
                                                     next_guess_(Ss,S,E,ScoreList).


% deleted for one evaluation
% Pocet prvku odstranenych z S, kde prvek je odstranen pokud nema stejne
% B&W score vyci kodu, jako je zade B&W score
dfe(Code, S, B, W, Deleted):- filterS(S, Code, B, W, NewS),
                              length(NewS,L1),
                              length(S,L2),
                              Deleted is L2-L1.


% deleted for all evaluations
% pocet odstranenych z S pro všechny mozné evaluace
dfae(_,_,[],[]).
dfae(Code,S,[[B,W]|Es],[Count|Deleted]):- dfe(Code, S, B, W, Count),
                                          dfae(Code,S,Es,Deleted).

generate(Vars) :- findall(X, varia_rep(4, [1, 2, 3, 4, 5, 6], X), Vars).

filterS([], _, _, _, []).

filterS([H | T], Guess, Blacks, Whites, [H | Res]) :-
    get_eval(H, Guess, Blacks, Whites),
    filterS(T, Guess, Blacks, Whites, Res).

filterS([_ | T], Guess, Blacks, Whites, Res) :- filterS(T, Guess, Blacks, Whites, Res).

get_eval(Secret,Guess,B,W):-get_b(Secret,Guess,B,0,[],[],Gw,Sw), get_w(Gw,Sw,W,0),!. %zavolá pomocný predikát s akumulatory

%ziska pocet spravnych cisel an spravnych pozicich
get_b([],[],Acc,Acc,Gw,Sw,Gw,Sw).
get_b([S|Ss],[G|Gs],B, Acc, Sacc, Gacc,Gw,Sw):- S =:=G, %porovna, zda cislo v S je stejne jako cislo v G
                                         Acc1 is Acc + 1, %akumulator se zvetsi o 1
                                         get_b(Ss, Gs, B, Acc1, Sacc, Gacc,Gw,Sw). %pokracuje se dalsim prvkem seznamu


get_b([S|Ss],[G|Gs],B, Acc,Sacc,Gacc,Gw,Sw):- S =\= G, %porovna, zda cislo v S neni stejne jako cislo v G
                                                get_b(Ss, Gs, B, Acc, [S|Sacc],[G|Gacc],Gw,Sw). %do akumulatoru se vlozi prvky a pokracuje se na dalsi prvek seznamu



%získá pocest spravnych cisel an spatnych pozicich
get_w([],_,Acc,Acc).
get_w([S|Ss],Gs,W,Acc):- member(S,Gs), % Existuje prvek v guessu, na spatne pozici
                         Acc1 is Acc + 1, %prictu jedna k bilym kolikum
                         remover(S,Gs,Gs1), %odstranim z akumulatoru jednou prvek S
                         get_w(Ss,Gs1,W,Acc1).

get_w([S|Ss],Gs,W,Acc):- not(member(S,Gs)),
                         get_w(Ss,Gs,W,Acc).


%odstraní první výskyt prvku R se seznamu
%?-remover(1, [2,1,3],X).
%X = [2, 3] .
remover( _, [], []).
remover( R, [R|T], T).
remover( R, [H|T], [H|T2]) :- H \= R, remover( R, T, T2).

/* Úkol z body z jiného cvičení */

p(0) :- nl.
p(N) :- N1 is N-1, write('*'), p(N1).

processfile :-
   read(Line),
   Line \== end_of_file,
   process(Line).

processfile :- !.

process(Line) :-
    call(Line),
    processfile.

