swap(e, w).
swap(w, e).

% move(Config, CarriedItem, NextConfig)

move([X, X, Goat, Cabbage], wolf, [Y, Y, Goat, Cabbage]) :- swap(X, Y).
move([X, Wolf, X, Cabbage], goat, [Y, Wolf, Y, Cabbage]) :- swap(X, Y).
move([X, Wolf, Goat, X], cabbage, [Y, Wolf, Goat, Y]) :- swap(X, Y).
move([X, Wolf, Goat, Cabbage], nothing, [Y, Wolf, Goat, Cabbage]) :- swap(X, Y).

safe([Farmer, Wolf, Goat, Cabbage]) :- safe_(Farmer, Wolf, Goat), safe_(Farmer, Goat, Cabbage).

safe_(X, X, _).
safe_(X, _, X).

solve([e, e, e, e], _).
solve(Config, [Move | Rest]) :- 
    move(Config, CarriedItem, NextConfig),
    safe(NextConfig),
    solve(NextConfig, Rest).

puzzle(Config, X) :- length(X, 7), solve(Config, X). 

/*
  ?- puzzle([w, w, w, w], X).
*/

dropAt(X, [X | Tail], 1, Tail).
dropAt(X, [Head | Tail], K, [Head | Rtail]) :- K>1, K1 is K-1, dropAt(X, Tail, K1, Rtail).

/*
dropAt(Item, List, Index, Res) :- dropAt_(Item, List, Index, 1, Res).

dropAt_(Item, [Item | List], Index, Index, Res) :- append(Res, List, R2), Res is R2.
dropAt_(Item, [H | List], Index, CI, Res) :- 
    NI is CI+1, 
    append(Res, [H], R2), 
    dropAt_(Item, List, Index, NI, R3), 
    Res is R3.
*/


duplicate([], []).
duplicate([H | Tail], [H, H | X]) :- duplicate(Tail, X).


/*
  List compression - compress repeating sequences
*/


compress([], []).
compress([H | Tail], [H | Res]) :- compress_(H, Tail, Res).

compress_(_, [], []).
compress_(Fst, [Fst | Tail], Res) :- compress_(Fst, Tail, Res).
compress_(_, [H | Tail], [H | Res]) :- compress_(H, Tail, Res).

/*
%compress([H, H | Tail], [H | R]) :- compress(Tail, R).
%compress([H | Tail], [H | R]) :- compress(Tail, R).
*/


sum(X, X, X).
sum(From, To, Sum) :- F1 is From + 1, sum(F1, To, Subres), Sum is Subres + From.












