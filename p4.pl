
graph(a, b).
graph(a, c).
graph(a, d).

graph(b, a).
graph(b, c).
graph(b, d).

graph(c, a).
graph(c, b).
graph(c, d).
graph(c, e).

graph(d, a).
graph(d, b).
graph(d, c).
graph(d, e).

graph(e, c).
graph(e, d).

euler(R) :- 
    findall([X, Y], graph(X, Y), El), 
    member(E, El),
    path(E, El, [], R).

path(E, [], T, R) :- reverse([E | T], R).
path([X, Y], El, T, R) :- 
    member([Y, Z], El), 
    Z \= X, 
    remove(El, [[X, Y], [Y, Z], [Y, X], [Z, Y]], El1),
    path([Y, Z], El1, [[X, Y] | T], R).

remove([], _, []).
remove([H | Rest], ToRem, [H | Dst]) :- not(member(H, ToRem)), remove(Rest, ToRem, Dst).
remove([H | Rest], ToRem, Dst) :- member(H, ToRem), remove(Rest, ToRem, Dst).

output([]).
output([H | T]) :- write(H), nl, output(T).

containsDuplicates(L, Res) :- sort(L, Sorted), length(Sorted, SrtLen), length(L, Len), SrtLen = Len.

%NFA

sigma([a, b]).

d(1, a, 2).
d(2, a, 3).
d(3, a, 1).

f([3]).

i([1, 2]).

startProgram :-
    write('Input: '), 
    read(W), 
    atom_chars(W, Ws),
    i(Il),
    member(I, Il),
    write('Init.'), 
    nl,
    nfa(I, Ws).

nfa(S, []) :- f(Fl), member(S, Fl), nl, write('Ok').
nfa(S, [C | Ws]) :- 
    d(S, C, S1), 
    write(S), 
    atom_chars(W, [C | Ws]), 
    write(': '), write(W), write(' -> '), write(S1), nl,
    nfa(S1, Ws).


