%prechodova funkce
d(1, a, 1).
d(1, a, 2).
d(1, b, 1).
d(2, b, 3).

%pocatecni stav
i(1).

%koncovy stav
f(3).

startQ(I) :- findall(S, i(S), I).

convert(States, Alphabet, Delta, I) :-
    startQ(I),
    convert_(States, [I], [], Alphabet, [], Delta),
    getF(States, F),
    output_delta(Delta),
    print_to_file(Delta),
    !.

convert_(P, [], P, _, D, D) :- !.
convert_(States, [Q | Qs], Acc, Alphabet, Prechody, Delta) :-
    not(member(Q, Acc)),
    chars(Q, Alphabet, D),
    getQs(D, Q1s),
    append(Qs, Q1s, UpdatedStates),
    append(Prechody, D, UpdatedPrechody),
    append(Acc, [Q], UpdatedAcc),
    convert_(States, UpdatedStates, UpdatedAcc, Alphabet, UpdatedPrechody, Delta).
convert_(States, [Q | Qs], Acc, Alphabet, Prechody, Delta) :-
    member(Q, Acc),
    convert_(States, Qs, Acc, Alphabet, Prechody, Delta).

prechod_([], _, Acc, Acc) :- !.
prechod_([X | Xs], Char, Acc, NewState) :-
    findall(Y, d(X, Char, Y), Y1),
    append(Acc, Y1, Y2),
    sort(Y2, Y3),
    prechod_(Xs, Char, Y3, NewState).

prechod(State, Char, NewState) :- prechod_(State, Char, [], NewState).

chars(State, Alphabet, Prechody) :- chars_(State, Alphabet, [], Prechody).

chars_(_, [], Acc, Acc) :- !.
chars_(State, [Char | Alphabet], Acc, Prechody) :- 
    prechod(State, Char, NewState),
    append(Acc, [[State, Char, NewState]], Acc2),
    chars_(State, Alphabet, Acc2, Prechody).

getQs([], []) :- !.
getQs([[_, _, New] | T], [New | Qs1]) :- getQs(T, Qs), sort(Qs, Qs1).

isF([H | _]) :- f(H).
isF([_ | Tail]) :- isF(Tail).

getF([], []).
getF([H | States], [H | F]) :- isF(H), getF(States, F).
getF([_ | States], F) :- getF(States, F).

output_delta([]) :- !.
output_delta([ [State, Char, NewState] | Rest ]) :-
    write('d('),
    write(State),
    write(','),
    write(Char),
    write(','),
    write(NewState),
    write(').'),
    nl,
    output_delta(Rest).


/*
getF([],[]). % pokud sjme zpracovali vsechny stavy, jsme na konci listu F
getF([Q|Qs],[Q|Fs]):- findall(X,f(X),F), % získá množinu koncových stavu NKF
match(Q,F), % zjsití, zda stav DKA obsahuje alespon jeden z koncových stavu NKF
getF(Qs,Fs). % jestli ano, prida tento stav do F (v hlave predikatu)



getF([Q|Qs],Fs):- findall(X,f(X),F), % získá množinu koncových stavu NKF
\+ match(Q,F), % zjsití, zda stav DKA obsahuje alespon jeden z koncových stavu NKF
getF(Qs,Fs). % jestli ano, preskoci tento stav do F (v hlave predikatu)



match(Qs,Fs):- member(Q,Qs),member(Q,Fs). %porovná, zda dva listy mají alespon jeden spolecný prvek
*/

%File IO

print_to_file(Delta) :- tell('delta.txt'), output_delta(Delta), told.

%see('delta.txt'), process_file, seen.

process_file :- 
    read(Line),
    Line \== end_of_file,
    process(Line).
process_file :- !.

process(Line) :-
    write(Line), nl,
    process_file.
