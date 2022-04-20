h(Nationality, Pet, Beverage, Cigarettes, House).

houses(Hs) :-
    length(Hs, 5),
    member(h(british, _, _, _, red), Hs),                             % 1
    member(h(spanish, dog, _, _, _), Hs),                             % 2
    member(h(_, _, coffee, _, green), Hs),                            % 3
    member(h(ukrainian, _, tea, _, _), Hs),                           % 4
    neighbours(h(_, _, _, _, green), h(_, _, _, _, white), Hs),       % 5
    member(h(_, snake, _, winston, _), Hs),                           % 6
    member(h(_, _, _, kool, yellow), Hs),                             % 7
    Hs = [h(norwegian, _, _, _, _), _, h(_, _, milk, _, _), _, _],    % 8, 9
    neighbours(h(_, _, _, chesterfield, _), h(_, fox, _, _, _), Hs),  % 10
    neighbours(h(_, _, _, kool, _), h(_, horse, _, _, _), Hs),        % 11
    member(h(_, _, juice, luckystrike, _), Hs),                       % 12
    member(h(japanese, _, _, kent, _), Hs),                           % 13
    neighbours(h(norwegian, _, _, _, _), h(_, _, _, _, blue), Hs).    % 14

zebra(P) :-
    houses(Hs),
    member(h(P, zebra, _, _, _), Hs).

water(P) :-
    houses(Hs),
    member(h(P, _, water, _, _), Hs).

neighbours(A, B, List) :- 
    append(_, [A, B | _], List).
neighbours(A, B, List) :- 
    append(_, [B, A | _], List).

