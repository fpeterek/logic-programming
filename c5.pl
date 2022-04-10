balik(List) :- 
    sort_by_size(List, Small, Medium, Large),
    S is 69, M is 420, L is 911,
    calc_price(S, Small, SmallPrice),
    calc_price(M, Medium, MedPrice),
    calc_price(L, Large, LargePrice),
    Total is SmallPrice + MedPrice + LargePrice,
    output(Small, Medium, Large, SmallPrice, MedPrice, LargePrice, Total).


output(Small, Medium, Large, SmallPrice, MedPrice, LargePrice, Total) :-
    write('Prices: '), nl,
    write('Small: 69,-'), nl,
    write('Medium: 420,-'), nl,
    write('Large: 911,-'), nl,
    write('Small: '), write(Small), write(', price: '), write(SmallPrice), write(',-'), nl,
    write('Medium: '), write(Medium), write(', price: '), write(MedPrice), write(',-'), nl,
    write('Large: '), write(Large), write(', price: '), write(LargePrice), write(',-'), nl,
    write('Total: '), write(Total), nl.



calc_price(PerPackage, Packages, Total) :- length(Packages, Len), Total is Len * PerPackage.

isSmall([D1, D2, D3]) :- D1 =< 50, D2 =< 50, D3 =< 50 .

isMedium([D1, D2, D3]) :- D1 =< 100, D2 =< 100, D3 =< 100 .

sort_by_size([], [], [], []).
sort_by_size([P | Rest], [P | S], M, L) :- isSmall(P), sort_by_size(Rest, S, M, L).
sort_by_size([P | Rest], S, [P | M], L) :- isMedium(P), sort_by_size(Rest, S, M, L).
sort_by_size([P | Rest], S, M, [P | L]) :- sort_by_size(Rest, S, M, L).


