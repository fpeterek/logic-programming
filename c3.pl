solve(Qs) :- 
    permutation([1, 2, 3, 4, 5, 6, 7, 8], Qs),
    test_permutation(Qs, 1, [], []).

test_permutation([], _, _, _).
test_permutation([Y | Ys], X, Cs, Ds) :- 
    C is X - Y, 
    not(member(C, Cs)),
    D is X + Y,
    not(member(D, Ds)),
    X1 is X+1,
    test_permutation(Ys, X1, [C | Cs], [D | Ds]).


swap([H1, H2 | _], [H2, H1 | _]) :- H > H2.
swap([H1 | T1], [H1 | T2]) :- swap(T1, T2).

bubblesort(List, Sorted) :- swap(List, List1),!, bubblesort(List1, Sorted).
bubblesort(Sorted, Sorted).

bigger(X, [], []).
bigger(X, [H | Tail], [H | Rest]) :- H > X, bigger(X, Tail, Rest).
bigger(X, [H | Tail], Res) :- bigger(X, Tail, Res).

smaller(X, [], []).
smaller(X, [H | Tail], [H | Rest]) :- H =< X, smaller(X, Tail, Rest).
smaller(X, [H | Tail], Res) :- smaller(X, Tail, Res).

quicksort([], []).
quicksort([X | Tail], Res) :- 
    smaller(X, Tail, Smaller), 
    bigger(X, Tail, Bigger),
    quicksort(Smaller, SmallerSorted),
    quicksort(Bigger, BiggerSorted),
    append([X], BiggerSorted, BiggerSorted1),
    append(SmallerSorted, BiggerSorted1, Res).


is_even(0).
is_even(X) :- X > 0, X1 is X-2, is_even(X1).

split_list([], [], []).
split_list([H | List], [H | Even], Odd) :- is_even(H), split_list(List, Even, Odd).
split_list([H | List], Even, [H | Odd]) :- split_list(List, Even, Odd).

evenOdd(List, Res) :- split_list(List, Even, Odd), append(Even, Odd, Res).

insert([], X, [X]).
insert([H1 | []], X, [H1, X]) :- X >= H1.
insert([H1 | []], X, [X | H1]) :- X < H1.
insert([H1, H2 | Sorted], X, [H1, X, H2 | Res]) :- H1 =< X, X =< H2.
insert([H1, H2 | Sorted], X, [H1, H2 | Res]) :- insert(Sorted, X, Res).

sort3([], []).
sort3([H | List], Sorted) :- sort3(List, Sorted2), insert(Sorted2, H, Sorted).

%----- N Queens

lst(0, Res, Res).
lst(N, Acc, Res) :- N > 0, N1 is N-1, lst(N1, [N | Acc], Res).

solve_n(N, Qs) :- 
    lst(N, [], Lst),
    permutation(Lst, Qs),
    test_permutation(Qs, 1, [], []).

