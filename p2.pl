/* List length */

/* built-in eq: length */

listLen(L, Res) :- listLen_(L, 0, Res).

listLen_([], Acc, Acc).
listLen_([_ | Tail], Acc, Res) :- A2 is Acc+1, listLen_(Tail, A2, Res).


/* List sum */

listSum(L, Res) :- listSum_(L, 0, Res).

listSum_([], Acc, Acc).
listSum_([H | Tail], Acc, Res) :- A2 is Acc+H, listSum_(Tail, A2, Res).

/* Contains */

/* built-in eq: member */

contains([], _) :- 0 = 1. %Redundant
contains([X | _], X).
contains([_ | Tail], X) :- contains(Tail, X).

/*
  ?- contains([3,1,2,5,0,-1,6], X), X>1.
  ?- contains([3,1,2,5,0,-1,6], X), X>1.
  ?- findall(X, (contains([3,1,2,5,0,-1,6], X), X>1), List).
*/

/* Find even numbers */

findEven([], []).
findEven([Head | Tail], [Head | X]) :- 0 is Head mod 2, findEven(Tail, X).
findEven([Head | Tail], X) :- 1 is Head mod 2, findEven(Tail, X).

/* Find numbers on odd positions */

oddPositions([], []).
oddPositions([H1], [H1]).
oddPositions([H1, _ | Tail], [H1 | Res]) :- oddPositions(Tail, Res).

/* Find even numbers on odd positions */

/*evenOnOdd([], []).
evenOnOdd([H], [H]) :- 0 is H mod 2 .
evenOnOdd([H], []) :- 1 is H mod 2 .
evenOnOdd([H1, H2 | Tail], [H1 | X]) :- 0 is H1 mod 2, evenOnOdd(Tail, X).
evenOnOdd([H1, H2 | Tail], X) :- 1 is H1 mod 2, evenOnOdd(Tail, X).*/

/*
  ?- oddPositions([2, 3, 3, 3, 4, 6, 8, 10, 10], Res), findEven(Res, R2).
*/

/* Merge Lists */

% built-in eq: append

mergeLists([], L2, L2).
mergeLists(L1, [], L1).
mergeLists([H | L1], L2, [H | L3]) :- mergeLists(L1, L2, L3).

/* Reverse List */

reverseList([], []).
reverseList([H | T], L2) :- reverseList(T, L3), append(L3, [H], L2).




