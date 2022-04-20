
/* Directed graph */

graph(a, b, 1).
graph(b, c, 2).
graph(c, d, 3).
graph(a, e, 3).
graph(e, d, 4).

path(X, Y, C) :- graph(X, Y, C).
path(X, Y, C) :- graph(X, Z, C1), path(Z, Y, C2), C is C1 + C2.

path(X, Y, C, []) :- graph(X, Y, C).
path(X, Y, C, [ Z | P ]) :- graph(X, Z, C1), path(Z, Y, C2, P), C is C1 + C2.

/* Checkerboard */

board([0, 2]). board([1, 2]). board([2, 2]).
board([0, 1]). board([1, 1]). board([2, 1]).
board([0, 0]). board([1, 0]). board([2, 0]). 

move([X, Y], [X1, Y1]) :- board([X, Y]), X1 is X-1, Y1 is Y+2, board([X1, Y1]).
move([X, Y], [X1, Y1]) :- board([X, Y]), X1 is X+1, Y1 is Y+2, board([X1, Y1]).
move([X, Y], [X1, Y1]) :- board([X, Y]), X1 is X-1, Y1 is Y-2, board([X1, Y1]).
move([X, Y], [X1, Y1]) :- board([X, Y]), X1 is X+1, Y1 is Y-2, board([X1, Y1]).

move([X, Y], [X1, Y1]) :- board([X, Y]), X1 is X-2, Y1 is Y+1, board([X1, Y1]).
move([X, Y], [X1, Y1]) :- board([X, Y]), X1 is X+2, Y1 is Y+1, board([X1, Y1]).
move([X, Y], [X1, Y1]) :- board([X, Y]), X1 is X-2, Y1 is Y-1, board([X1, Y1]).
move([X, Y], [X1, Y1]) :- board([X, Y]), X1 is X+2, Y1 is Y-1, board([X1, Y1]).

cost([0,0],1).
cost([1,0],2).
cost([2,0],3).

cost([0,1],3).
cost([1,1],2).
cost([2,1],1).

cost([0,2],1).
cost([1,2],2).
cost([2,2],3).

cheapest(X, Y, P, C) :-
    findall(Path, knightpath(X, Y, Path), Paths),
    find_cheapest(Paths, P, C).

find_cheapest(Paths, P, C) :-
    find_cheapest_(Paths, [], 10000000000000000, P, C).

find_cheapest_([], BP, BC, BP, BC).

find_cheapest_([P | Paths], CP, CC, BP, BC) :-
    calc_cost(P, NC),
    NC < CC,
    find_cheapest_(Paths, P, NC, BP, BC).

find_cheapest_([P | Paths], CP, CC, BP, BC) :- find_cheapest_(Paths, CP, CC, BP, BC).

calc_cost([], 0).
calc_cost([H | Path], Total) :-
    cost(H, C),
    calc_cost(Path, Subtotal),
    Total is C + Subtotal.

kpath(X, X, P, P).
kpath(X, Y, T, P) :- move(X, Z), not(member(Z, T)), kpath(Z, Y, [Z | T], P).

knightpath(X, Y, P) :- kpath(X, Y, [X], P1), reverse(P1, P).

writepath([]).
writepath([H | T]) :- write(H), nl, writepath(T).

/* Visit each corner exactly once */
corners(Paths) :-
    findall(Path, cornerpath([0, 0], [2, 1], Path), Paths),
    length(Paths, Len),
    write('Number of paths: '), write(Len), nl,
    printall(Paths).

printall([]).
printall([H | T]) :- write(H), nl, printall(T).
     
cpath(X, X, P, P) :- member([0, 0], P), member([2, 0], P), member([0, 2], P), member([2, 2], P).
cpath(X, Y, T, P) :- move(X, Z), not(member(Z, T)), cpath(Z, Y, [Z | T], P).

cornerpath(X, Y, P) :- cpath(X, Y, [X], P1), reverse(P1, P).

/* String operations */

readln(Len, Str, R) :- atom_chars(Str, C), append(Pref, _, C), length(Pref, Len), atom_chars(R, Pref).

remove(Len, Str, R) :- atom_chars(Str, C), append(Pref, Suffix, C), length(Pref, Len), atom_chars(R, Suffix).

contains(Str, Substr, P) :- 
    atom_chars(Substr, Strl), 
    length(Strl, StrLen),
    contains(Str, Substr, P, StrLen).

contains(Str, Substr, 1, D) :- readln(D, Str, Substr).
contains(Str, Substr, Pos, D) :- 
    remove(1, Str, Str2),
    contains(Str2, Substr, P1, D),
    Pos is P1+1.

