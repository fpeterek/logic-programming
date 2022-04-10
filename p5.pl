palindrom(Word) :- atom_chars(Word, LWord), reverse(LWord, LWord).

board([0, 2]). board([1, 2]). board([2, 2]).
board([0, 1]). board([1, 1]). board([2, 1]).
board([0, 0]). board([1, 0]). board([2, 0]).

%object(ID, S1, S2, S3).

object(1, [X, Y], [X, Y1], [X, Y2])  :- Y1 is Y+1, Y2 is Y+2 .
object(2, [X, Y], [X1, Y], [X2, Y])  :- X1 is X+1, X2 is X+2 .
object(3, [X, Y], [X, Y1], [X1, Y])  :- X1 is X+1, Y1 is Y+1 .
object(4, [X, Y], [X1, Y], [X1, Y0]) :- X1 is X+1, Y0 is Y-1 .
object(5, [X, Y], [X1, Y], [X1, Y1]) :- X1 is X+1, Y1 is Y+1 .
object(6, [X, Y], [X, Y1], [X1, Y1]) :- X1 is X+1, Y1 is Y+1 .

fill(X) :- findall([XC, YC], board([XC, YC]), S), put(S, X).

put([], []).
put(S, [[O, S1, S2, S3] | V]) :- 
    member(S1, S),
    object(O, S1, S2, S3),
    member(S2, S),
    member(S3, S),
    remove(S, [S1, S2, S3], SNew),
    put(SNew, V).

output([]).
output([H | T]) :- write(H), nl, output(T).

remove([], _, []).
remove([H | Rest], ToRem, [H | Dst]) :- not(member(H, ToRem)), remove(Rest, ToRem, Dst).
remove([H | Rest], ToRem, Dst) :- member(H, ToRem), remove(Rest, ToRem, Dst).

%---- Hamming distance

:- dynamic d/5.

d(1, 0, a, 3, 0).
%d(1, b, 2).

