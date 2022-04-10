info(A, B) :- write('Move disk from '), write(A), write(' to '), write(B), write('.'), nl.

% move(Number of disks, from, to, using).
% move(3, l, c, r).
move(0, _, _, _).
move(N, A, B, C) :- M is N-1, move(M, A, C, B), info(A, B), move(M, C, B, A).

% sum(n, result)
% sum(3, X).

sum_(0, Acc, Acc).
sum_(N, Acc, Res) :- N1 is N-1, Acc2 is Acc + N, sum_(N1, Acc2, Res).

sum(N, Res) :- N > -1, sum_(N, 0, Res).


