:- dynamic s/2.
:- dynamic is_empty_flag/1.

reset :- retract(s(_, _)), fail.

% Tah na prostřední pole, pokud je celé herní pole prázdné
is_empty_flag(true).

is_empty(V) :-
    is_empty_flag(V),
    V,
    findall(Coord, s(Coord, ' '), Empty),
    length(Empty, 100),
    retract(is_empty_flag(true)),
    assert(is_empty_flag(false)).

is_empty(false).

tp :- is_empty(V), V, s(S, ' '), retract(s(S, ' ')), assert(s(S, x)), vypis_p, test_v(x).

% Pětice xxxxx

% Offense xxxxx

%Tah počítače : pravidlo 1 Xxxxx
tp :-
    s(S1, ' '),
    o(S1, S2, S3, S4, S5),
    s(S2, x), s(S3, x), s(S4, x), s(S5, x),
    retract(s(S1, ' ')), assert(s(S1, x)),
    write([S1, 1]), nl,
    vypis_p,
    test_v(x).

%Tah počítače : pravidlo 1 xXxxx
tp :-
    s(S1, x),
    o(S1, S2, S3, S4, S5),
    s(S1, x), s(S2, ' '), s(S3, x), s(S4, x), s(S5, x),
    retract(s(S2, ' ')), assert(s(S2, x)),
    write([S2, 1]), nl,
    vypis_p,
    test_v(x).

%Tah počítače : pravidlo 1 xxXxx
tp :-
    s(S1, x),
    o(S1, S2, S3, S4, S5),
    s(S1, x), s(S2, x), s(S3, ' '), s(S4, x), s(S5, x),
    retract(s(S3, ' ')), assert(s(S3, x)),
    write([S3, 1]), nl,
    vypis_p,
    test_v(x).

%Tah počítače : pravidlo 1 xxxXx
tp :-
    s(S1, x),
    o(S1, S2, S3, S4, S5),
    s(S1, x), s(S2, x), s(S3, x), s(S4, ' '), s(S5, x),
    retract(s(S4, ' ')), assert(s(S4, x)),
    write([S4, 1]), nl,
    vypis_p,
    test_v(x).

%Tah počítače : pravidlo 1 xxxxX
tp :-
    s(S1, x),
    o(S1, S2, S3, S4, S5),
    s(S1, x), s(S2, x), s(S3, x), s(S4, x), s(S5, ' '),
    retract(s(S5, ' ')), assert(s(S5, x)),
    write([S5, 1]), nl,
    vypis_p,
    test_v(x).

% Defense xxxxx

%Tah počítače : pravidlo 1 Xxxxx
tp :-
    s(S1, ' '),
    o(S1, S2, S3, S4, S5),
    s(S2, o), s(S3, o), s(S4, o), s(S5, o),
    retract(s(S1, ' ')), assert(s(S1, x)),
    write([S1, 1]), nl,
    vypis_p,
    test_v(x).

%Tah počítače : pravidlo 1 xXxxx
tp :-
    s(S1, o),
    o(S1, S2, S3, S4, S5),
    s(S1, o), s(S2, ' '), s(S3, o), s(S4, o), s(S5, o),
    retract(s(S2, ' ')), assert(s(S2, x)),
    write([S2, 1]), nl,
    vypis_p,
    test_v(x).

%Tah počítače : pravidlo 1 xxXxx
tp :-
    s(S1, o),
    o(S1, S2, S3, S4, S5),
    s(S2, o), s(S3, ' '), s(S4, o), s(S5, o),
    retract(s(S3, ' ')), assert(s(S3, x)),
    write([S3, 1]), nl,
    vypis_p,
    test_v(x).

%Tah počítače : pravidlo 1 xxxXx
tp :-
    s(S1, o),
    o(S1, S2, S3, S4, S5),
    s(S2, o), s(S3, o), s(S4, ' '), s(S5, o),
    retract(s(S4, ' ')), assert(s(S4, x)),
    write([S4, 1]), nl,
    vypis_p,
    test_v(x).

%Tah počítače : pravidlo 1 xxxxX
tp :-
    s(S1, o), 
    o(S1, S2, S3, S4, S5),
    s(S2, o), s(S3, o), s(S4, o), s(S5, ' '),
    retract(s(S5, ' ')), assert(s(S5, x)),
    write([S5, 1]), nl,
    vypis_p,
    test_v(x).

% Čtveřice

% Offense xxxx

%Tah počítače : pravidlo 4 Xxxx
%Předpoklad - oponent není debil a umí blokovat trojice/čtveřice - tedy čtveřici je třeba tvořit pouze pokud je trojice volná z obou stran.
tp :-
    (s(S1, ' '); s(S1, x)),
    o6(S1, S2, S3, S4, S5, S6),
    s(S2, ' '), s(S3, x), s(S4, x), s(S5, x),
    (s(S6, ' '); s(S6, x)),
    retract(s(S2, ' ')), assert(s(S2, x)),
    write([S2, 4]), nl,
    vypis_p,
    test_v(x).

%Tah počítače : pravidlo 4 xXxx
tp :-
    (s(S1, ' '), s(S1, x)),
    o6(S1, S2, S3, S4, S5, S6),
    s(S2, x), s(S3, ' '), s(S4, x), s(S5, x),
    (s(S6, ' '); s(S6, x)),
    retract(s(S3, ' ')), assert(s(S3, x)),
    write([S3, 4]), nl,
    vypis_p,
    test_v(x).

%Tah počítače : pravidlo 4 xxXx
tp :-
    (s(S1, x); s(S1, ' ')),
    o6(S1, S2, S3, S4, S5, S6),
    s(S2, x), s(S3, x), s(S4, ' '), s(S5, x),
    (s(S6, x); s(S6, ' ')),
    retract(s(S4, ' ')), assert(s(S4, x)),
    write([S4, 4]), nl,
    vypis_p,
    test_v(x).

%Tah počítače : pravidlo 4 xxxX
tp :-
    (s(S1, ' '); s(S1, x)),
    o6(S1, S2, S3, S4, S5, S6),
    s(S2, x), s(S3, x), s(S4, x), s(S5, ' '), 
    (s(S6, x); s(S6, ' ')),
    retract(s(S5, ' ')), assert(s(S5, x)),
    write([S5, 4]), nl,
    vypis_p,
    test_v(x).

% Defense xxxx

%Tah počítače : pravidlo 4 Xxxx
%Předpoklad - oponent není debil a umí blokovat trojice/čtveřice - tedy čtveřici je třeba tvořit pouze pokud je trojice volná z obou stran.
tp :-
    (s(S1, ' '); s(S1, o)),
    o6(S1, S2, S3, S4, S5, S6),
    s(S2, ' '), s(S3, o), s(S4, o), s(S5, o),
    (s(S6, ' '); s(S6, o)),
    retract(s(S2, ' ')), assert(s(S2, x)),
    write([S2, 4]), nl,
    vypis_p,
    test_v(x).

%Tah počítače : pravidlo 4 xXxx
tp :-
    (s(S1, ' '), s(S1, o)),
    o6(S1, S2, S3, S4, S5, S6),
    s(S2, o), s(S3, ' '), s(S4, o), s(S5, o),
    (s(S6, ' '); s(S6, o)),
    retract(s(S3, ' ')), assert(s(S3, x)),
    write([S3, 4]), nl,
    vypis_p,
    test_v(x).

%Tah počítače : pravidlo 4 xxXx
tp :-
    (s(S1, o); s(S1, ' ')),
    o6(S1, S2, S3, S4, S5, S6),
    s(S2, o), s(S3, o), s(S4, ' '), s(S5, o),
    (s(S6, o); s(S6, ' ')),
    retract(s(S4, ' ')), assert(s(S4, x)),
    write([S4, 4]), nl,
    vypis_p,
    test_v(x).

%Tah počítače : pravidlo 4 xxxX
tp :-
    (s(S1, ' '); s(S1, o)),
    o6(S1, S2, S3, S4, S5, S6),
    s(S2, o), s(S3, o), s(S4, o), s(S5, ' '), 
    (s(S6, o); s(S6, ' ')),
    retract(s(S5, ' ')), assert(s(S5, x)),
    write([S5, 4]), nl,
    vypis_p,
    test_v(x).

% Kříže

% Offense - kříž

% Tah počítače - pravidlo 30 - zákeřnější kříž - nahoru doleva
% TODO: Nefunguje úplně asi

% Útok

tp :-
    (s(S1, ' '); s(S1, x)), o(S1, S2, S3, S4, S5),
    s(S4, ' '),
    (
        (s(S2, x), s(S3, x)); 
        (s(S3, x), s(S5, x))
    ),
    (s(S5, x); s(S5, ' ')),

    (s(S6, ' '); s(S6, x)),
    o6(S6, S7, S8, S9, S4, SA),
    S7 \= S1,
    
    (s(S7, ' '); s(S7, x)),
    (s(S8, ' '); s(S8, x)),
    (s(S9, ' '); s(S9, x)),
    (s(SA, ' '); s(SA, x)),
    s(S9, ' '),
    (
        (s(S7, x), s(S8, x));
        (s(S8, x), s(SA, x))
    ),

    retract(s(S4, ' ')), assert(s(S4, x)),
    write([S4, 30]), nl,
    vypis_p,
    test_v(x).

% Tah počítače - pravidlo 30 - zákeřnější kříž - dolů doleva

tp :-
    (s(S1, ' '); s(S1, x)), o(S1, S2, S3, S4, S5),
    s(S2, ' '),
    (
        (s(S1, x), s(S3, x)); 
        (s(S3, x), s(S4, x))
    ),
    (s(S5, x); s(S5, ' ')),

    (s(S6, ' '); s(S6, x)),
    o6(S6, S7, S8, S9, S2, SA),
    S9 \= S1,
    
    (s(S7, ' '); s(S7, x)),
    (s(S8, ' '); s(S8, x)),
    (s(S9, ' '); s(S9, x)),
    (s(SA, ' '); s(SA, x)),
    s(S9, ' '),
    (
        (s(S7, x), s(S8, x));
        (s(S8, x), s(SA, x))
    ),

    retract(s(S2, ' ')), assert(s(S2, x)),
    write([S2, 30]), nl,
    vypis_p,
    test_v(x).

% Tah počítače - pravidlo 30 - zákeřnější kříž - nahoru doprava

tp :-
    (s(S1, ' '); s(S1, x)), o(S1, S2, S3, S4, S5),
    s(S4, ' '),
    (
        (s(S2, x), s(S3, x)); 
        (s(S3, x), s(S5, x))
    ),
    (s(S5, x); s(S5, ' ')),

    (s(S6, ' '); s(S6, x)),
    o6(S6, S4, S7, S8, S9, SA),
    S7 \= S5,
    
    (s(S7, ' '); s(S7, x)),
    (s(S8, ' '); s(S8, x)),
    (s(S9, ' '); s(S9, x)),
    (s(SA, ' '); s(SA, x)),
    s(S9, ' '),
    (
        (s(S6, x), s(S8, x));
        (s(S8, x), s(S9, x))
    ),

    retract(s(S4, ' ')), assert(s(S4, x)),
    write([S4, 30]), nl,
    vypis_p,
    test_v(x).

% Obrana

tp :-
    (s(S1, ' '); s(S1, o)), o(S1, S2, S3, S4, S5),
    s(S4, ' '),
    (
        (s(S2, o), s(S3, o)); 
        (s(S3, o), s(S5, o))
    ),
    (s(S5, o); s(S5, ' ')),

    (s(S6, ' '); s(S6, o)),
    o6(S6, S7, S8, S9, S4, SA),
    S7 \= S1,
    
    (s(S7, ' '); s(S7, o)),
    (s(S8, ' '); s(S8, o)),
    (s(S9, ' '); s(S9, o)),
    (s(SA, ' '); s(SA, o)),
    s(S9, ' '),
    (
        (s(S7, o), s(S8, o));
        (s(S8, o), s(SA, o))
    ),

    retract(s(S4, ' ')), assert(s(S4, x)),
    write([S4, 30]), nl,
    vypis_p,
    test_v(x).

% Tah počítače - pravidlo 30 - zákeřnější kříž - dolů doleva

tp :-
    (s(S1, ' '); s(S1, o)), o(S1, S2, S3, S4, S5),
    s(S2, ' '),
    (
        (s(S1, o), s(S3, o)); 
        (s(S3, o), s(S4, o))
    ),
    (s(S5, o); s(S5, ' ')),

    (s(S6, ' '); s(S6, o)),
    o6(S6, S7, S8, S9, S2, SA),
    S9 \= S1,
    
    (s(S7, ' '); s(S7, o)),
    (s(S8, ' '); s(S8, o)),
    (s(S9, ' '); s(S9, o)),
    (s(SA, ' '); s(SA, o)),
    s(S9, ' '),
    (
        (s(S7, o), s(S8, o));
        (s(S8, o), s(SA, o))
    ),

    retract(s(S2, ' ')), assert(s(S2, x)),
    write([S2, 30]), nl,
    vypis_p,
    test_v(x).

% Tah počítače - pravidlo 30 - zákeřnější kříž - nahoru doprava

tp :-
    (s(S1, ' '); s(S1, o)), o(S1, S2, S3, S4, S5),
    s(S4, ' '),
    (
        (s(S2, o), s(S3, o)); 
        (s(S3, o), s(S5, o))
    ),
    (s(S5, o); s(S5, ' ')),

    (s(S6, ' '); s(S6, o)),
    o6(S6, S4, S7, S8, S9, SA),
    S7 \= S5,
    
    (s(S7, ' '); s(S7, o)),
    (s(S8, ' '); s(S8, o)),
    (s(S9, ' '); s(S9, o)),
    (s(SA, ' '); s(SA, o)),
    s(S9, ' '),
    (
        (s(S6, o), s(S8, o));
        (s(S8, o), s(S9, o))
    ),

    retract(s(S4, ' ')), assert(s(S4, x)),
    write([S4, 30]), nl,
    vypis_p,
    test_v(x).

% Tah počítače - pravidlo 30 - zákeřnější kříž - dolů doprava

tp :-
    (s(S1, ' '); s(S1, o)), o(S1, S2, S3, S4, S5),
    s(S2, ' '),
    (
        (s(S1, o), s(S3, o));
        (s(S3, o), s(S4, o)) 
    ),
    (s(S5, o); s(S5, ' ')),

    (s(S6, ' '); s(S6, o)),
    o6(S6, S2, S7, S8, S9, SA),
    S6 \= S1,
    
    (s(S7, ' '); s(S7, o)),
    (s(S8, ' '); s(S8, o)),
    (s(S9, ' '); s(S9, o)),
    (s(SA, ' '); s(SA, o)),
    s(S9, ' '),
    (
        (s(S6, o), s(S8, o));
        (s(S8, o), s(S9, o))
    ),

    retract(s(S2, ' ')), assert(s(S2, x)),
    write([S2, 30]), nl,
    vypis_p,
    test_v(x).

%Tah počítače - pravidlo 2 kříž
tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, x), s(S3, ' '), s(S4, x), s(S5, ' '),
    s(S6, ' '), S1 \= S6, o(S6, S7, S3, S8, S9),
    s(S7, x), s(S8, x), s(S9, ' '),
    retract(s(S3, ' ')), assert(s(S3, x)),
    write([S3, 2]), nl,
    vypis_p,
    test_v(x).

%Tah počítače - pravidlo 2 kříž
tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, ' '), s(S3, x), s(S4, x), s(S5, ' '),
    s(S6, ' '), S1 \= S6, o(S6, S2, S7, S8, S9),
    s(S7, x), s(S8, x), s(S9, ' '),
    retract(s(S3, ' ')), assert(s(S3, x)),
    write([S3, 2]), nl,
    vypis_p,
    test_v(x).

%Tah počítače - pravidlo 2 kříž ukazující doprava
tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, ' '), s(S3, x), s(S4, x), s(S5, ' '),
    s(S6, ' '), o(S6, S7, S2, S8, S9),
    s(S7, x), s(S8, x), s(S9, ' '),
    retract(s(S3, ' ')), assert(s(S3, x)),
    write([S3, 2]), nl,
    vypis_p,
    test_v(x).

%Tah počítače - pravidlo 2 kříž ukazující doleva
tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, x), s(S3, x), s(S4, ' '), s(S5, ' '),
    s(S6, ' '), o(S6, S7, S4, S8, S9),
    s(S7, x), s(S8, x), s(S9, ' '),
    retract(s(S3, ' ')), assert(s(S3, x)),
    write([S3, 2]), nl,
    vypis_p,
    test_v(x).

% Defense - kříž

%Tah počítače : pravidlo 3 kříž
tp :-
    (s(S1, ' '); s(S1, o)), o(S1, S2, S3, S4, S5),
    s(S2, o), s(S3, ' '), s(S4, o), (s(S5, ' '); s(S5, o)),
    (s(S6, ' '); s(S6, o)), S1 \= S6, o(S6, S7, S3, S8, S9),
    s(S7, o), s(S8, o), (s(S9, ' '); s(S9, o)),
    retract(s(S3, ' ')), assert(s(S3, x)),
    write([S3, 3]), nl,
    vypis_p,
    test_v(x).

%Tah počítače : pravidlo 3 kříž
tp :-
    (s(S1, ' '); s(S1, o)), o(S1, S2, S3, S4, S5),
    s(S2, ' '), s(S3, o), s(S4, o), (s(S5, ' '); s(S5, o)),
    (s(S6, ' '); s(S6, o)), S1 \= S6, o(S6, S2, S7, S8, S9),
    s(S7, o), s(S8, o), (s(S9, ' '); s(S9, o)),
    retract(s(S3, ' ')), assert(s(S3, x)),
    write([S3, 3]), nl,
    vypis_p,
    test_v(x).

%Tah počítače - pravidlo 3 kříž ukazující doprava
tp :-
    (s(S1, ' '); s(S1, o)), o(S1, S2, S3, S4, S5),
    s(S2, ' '), s(S3, o), s(S4, o), (s(S5, ' '); s(S5, o)),
    (s(S6, ' '); s(S6, o)), o(S6, S7, S2, S8, S9),
    s(S7, o), s(S8, o), (s(S9, ' '); s(S9, o)),
    retract(s(S3, ' ')), assert(s(S3, x)),
    write([S3, 3]), nl,
    vypis_p,
    test_v(x).

%Tah počítače - pravidlo 3 kříž ukazující doleva
tp :-
    (s(S1, ' '); s(S1, o)), o(S1, S2, S3, S4, S5),
    s(S2, o), s(S3, o), s(S4, ' '), (s(S5, ' '); s(S5, o)),
    (s(S6, ' '); s(S6, o)), o(S6, S7, S4, S8, S9),
    s(S7, o), s(S8, o), (s(S9, ' '); s(S9, o)),
    retract(s(S3, ' ')), assert(s(S3, x)),
    write([S3, 3]), nl,
    vypis_p,
    test_v(x).

% Robíme paralelní trojice - položení pátého pole, pravidlo 14

tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    (s(S2, x); s(S4, x)),
    (s(S2, ' '); s(S2, x)), s(S3, x), (s(S4, x); s(S4, ' ')), s(S5, ' '),

    [S1X, S1Y] = S1, [S5X, S5Y] = S5, (S1X = S5X; S1Y = S5Y),
    (
        (S1X = S6X, (S6Y is S1Y-2; S6Y is S1Y+2));
        (S1Y = S6Y, (S6X is S1X-2; S6X is S1X+2))
    ),
    S6 = [S6X, S6Y],

    s(S6, _), o(S6, S7, S8, S9, _),
    (s(S7, x); s(S9, x)), s(S8, x),

    s(SB, ' '), o(SB, SC, SD, SE, SF),
    SB \= S1, SB \= S6,
    (SC = S2; SC = S7), (SE = S4; SE = S9),
    s(SB, ' '), s(SD, ' '), s(SF, ' '),

    (
        (retract(s(S2, ' ')), assert(s(S2, x)), write([S2, 14]), nl);
        (retract(s(S4, ' ')), assert(s(S4, x)), write([S4, 14]), nl)
    ),
    vypis_p,
    test_v(x).

tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, x), s(S3, ' '), s(S4, x), s(S5, ' '),

    [S1X, S1Y] = S1, [S5X, S5Y] = S5, (S1X = S5X; S1Y = S5Y),
    (
        (S1X = S6X, (S6Y is S1Y-2; S6Y is S1Y+2));
        (S1Y = S6Y, (S6X is S1X-2; S6X is S1X+2))
    ),
    S6 = [S6X, S6Y],

    s(S6, _), o(S6, S7, S8, S9, _),
    (s(S7, x); s(S9, x)), s(S8, x),

    s(SB, ' '), o(SB, SC, SD, SE, SF),
    SB \= S1, SB \= S6,
    (SC = S2; SC = S7), (SE = S4; SE = S9),
    s(SB, ' '), s(SD, ' '), s(SF, ' '),

    (
        (retract(s(S3, ' ')), assert(s(S3, x)), write([S3, 14]), nl)
    ),
    vypis_p,
    test_v(x).

% Obrana před paralelníma dvojicema/trojicema/jak tomu chcu říkat - pro dvě
% paralelní dvojice, ale to už je trochu pozdě
tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    (s(S2, o); s(S4, o)),
    (s(S2, ' '); s(S2, o)), s(S3, o), (s(S4, o); s(S4, ' ')), s(S5, ' '),

    [S1X, S1Y] = S1, [S5X, S5Y] = S5, (S1X = S5X; S1Y = S5Y),
    (
        (S1X = S6X, (S6Y is S1Y-2; S6Y is S1Y+2));
        (S1Y = S6Y, (S6X is S1X-2; S6X is S1X+2))
    ),
    S6 = [S6X, S6Y],

    s(S6, _), o(S6, S7, S8, S9, _),
    (s(S7, o); s(S9, o)), s(S8, o),

    s(SB, ' '), o(SB, SC, SD, SE, SF),
    SB \= S1, SB \= S6,
    (SC = S2; SC = S7), (SE = S4; SE = S9),
    s(SB, ' '), s(SD, ' '), s(SF, ' '),

    (
        (retract(s(SD, ' ')), assert(s(SD, x)), write([SD, 14]), nl)
    ),
    vypis_p,
    test_v(x).

% Robíme paralelní trojice - položení čtvrtého pole, pravidlo 15

tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, x), s(S3, ' '), s(S4, ' '), s(S5, ' '),

    [S1X, S1Y] = S1, [S5X, S5Y] = S5, (S1X = S5X; S1Y = S5Y),
    (
        (S1X = S6X, (S6Y is S1Y-2; S6Y is S1Y+2));
        (S1Y = S6Y, (S6X is S1X-2; S6X is S1X+2))
    ),
    S6 = [S6X, S6Y],

    s(S6, _), o(S6, S7, S8, S9, _),
    s(S7, x), s(S8, x),

    s(SB, ' '), o(SB, SC, SD, SE, SF),
    SB \= S1, SB \= S6,
    (SC = S2; SC = S7), (SE = S4; SE = S9),
    s(SB, ' '), s(SD, ' '), s(SF, ' '),

    (
        (retract(s(S3, ' ')), assert(s(S3, x)), write([S3, 15]), nl)
    ),
    vypis_p,
    test_v(x).

tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, ' '), s(S3, x), s(S4, ' '), s(S5, ' '),

    [S1X, S1Y] = S1, [S5X, S5Y] = S5, (S1X = S5X; S1Y = S5Y),
    (
        (S1X = S6X, (S6Y is S1Y-2; S6Y is S1Y+2));
        (S1Y = S6Y, (S6X is S1X-2; S6X is S1X+2))
    ),
    S6 = [S6X, S6Y],

    s(S6, _), o(S6, S7, S8, S9, _),
    s(S7, x), s(S8, x),

    s(SB, ' '), o(SB, SC, SD, SE, SF),
    SB \= S1, SB \= S6,
    (SC = S2; SC = S7), (SE = S4; SE = S9),
    s(SB, ' '), s(SD, ' '), s(SF, ' '),

    (
        (retract(s(S2, ' ')), assert(s(S2, x)), write([S2, 15]), nl)
    ),
    vypis_p,
    test_v(x).

tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, ' '), s(S3, x), s(S4, ' '), s(S5, ' '),

    [S1X, S1Y] = S1, [S5X, S5Y] = S5, (S1X = S5X; S1Y = S5Y),
    (
        (S1X = S6X, (S6Y is S1Y-2; S6Y is S1Y+2));
        (S1Y = S6Y, (S6X is S1X-2; S6X is S1X+2))
    ),
    S6 = [S6X, S6Y],

    s(S6, _), o(S6, S7, S8, S9, _),
    s(S8, x), s(S9, x),

    s(SB, ' '), o(SB, SC, SD, SE, SF),
    SB \= S1, SB \= S6,
    (SC = S2; SC = S7), (SE = S4; SE = S9),
    s(SB, ' '), s(SD, ' '), s(SF, ' '),

    (
        (retract(s(S4, ' ')), assert(s(S4, x)), write([S4, 15]), nl)
    ),
    vypis_p,
    test_v(x).

tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, ' '), s(S3, ' '), s(S4, x), s(S5, ' '),

    [S1X, S1Y] = S1, [S5X, S5Y] = S5, (S1X = S5X; S1Y = S5Y),
    (
        (S1X = S6X, (S6Y is S1Y-2; S6Y is S1Y+2));
        (S1Y = S6Y, (S6X is S1X-2; S6X is S1X+2))
    ),
    S6 = [S6X, S6Y],

    s(S6, _), o(S6, S7, S8, S9, _),
    s(S8, x), s(S9, x),

    s(SB, ' '), o(SB, SC, SD, SE, SF),
    SB \= S1, SB \= S6,
    (SC = S2; SC = S7), (SE = S4; SE = S9),
    s(SB, ' '), s(SD, ' '), s(SF, ' '),

    (
        (retract(s(S3, ' ')), assert(s(S3, x)), write([S3, 15]), nl)
    ),
    vypis_p,
    test_v(x).

tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, x), s(S3, x), s(S4, ' '), s(S5, ' '),

    [S1X, S1Y] = S1, [S5X, S5Y] = S5, (S1X = S5X; S1Y = S5Y),
    (
        (S1X = S6X, (S6Y is S1Y-2; S6Y is S1Y+2));
        (S1Y = S6Y, (S6X is S1X-2; S6X is S1X+2))
    ),
    S6 = [S6X, S6Y],

    s(S6, _), o(S6, S7, S8, S9, _),
    s(S7, ' '), s(S8, x),

    s(SB, ' '), o(SB, SC, SD, SE, SF),
    SB \= S1, SB \= S6,
    (SC = S2; SC = S7), (SE = S4; SE = S9),
    s(SB, ' '), s(SD, ' '), s(SF, ' '),

    (
        (retract(s(S7, ' ')), assert(s(S7, x)), write([S7, 15]), nl)
    ),
    vypis_p,
    test_v(x).

tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, x), s(S3, x), s(S4, ' '), s(S5, ' '),

    [S1X, S1Y] = S1, [S5X, S5Y] = S5, (S1X = S5X; S1Y = S5Y),
    (
        (S1X = S6X, (S6Y is S1Y-2; S6Y is S1Y+2));
        (S1Y = S6Y, (S6X is S1X-2; S6X is S1X+2))
    ),
    S6 = [S6X, S6Y],

    s(S6, _), o(S6, S7, S8, S9, _),
    s(S7, x), s(S8, ' '),

    s(SB, ' '), o(SB, SC, SD, SE, SF),
    SB \= S1, SB \= S6,
    (SC = S2; SC = S7), (SE = S4; SE = S9),
    s(SB, ' '), s(SD, ' '), s(SF, ' '),

    (
        (retract(s(S8, ' ')), assert(s(S8, x)), write([S8, 15]), nl)
    ),
    vypis_p,
    test_v(x).

tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, ' '), s(S3, x), s(S4, x), s(S5, ' '),

    [S1X, S1Y] = S1, [S5X, S5Y] = S5, (S1X = S5X; S1Y = S5Y),
    (
        (S1X = S6X, (S6Y is S1Y-2; S6Y is S1Y+2));
        (S1Y = S6Y, (S6X is S1X-2; S6X is S1X+2))
    ),
    S6 = [S6X, S6Y],

    s(S6, _), o(S6, S7, S8, S9, _),
    s(S8, x), s(S9, ' '),

    s(SB, ' '), o(SB, SC, SD, SE, SF),
    SB \= S1, SB \= S6,
    (SC = S2; SC = S7), (SE = S4; SE = S9),
    s(SB, ' '), s(SD, ' '), s(SF, ' '),

    (
        (retract(s(S9, ' ')), assert(s(S9, x)), write([S9, 15]), nl)
    ),
    vypis_p,
    test_v(x).

tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, ' '), s(S3, x), s(S4, x), s(S5, ' '),

    [S1X, S1Y] = S1, [S5X, S5Y] = S5, (S1X = S5X; S1Y = S5Y),
    (
        (S1X = S6X, (S6Y is S1Y-2; S6Y is S1Y+2));
        (S1Y = S6Y, (S6X is S1X-2; S6X is S1X+2))
    ),
    S6 = [S6X, S6Y],

    s(S6, _), o(S6, S7, S8, S9, _),
    s(S8, ' '), s(S9, x),

    s(SB, ' '), o(SB, SC, SD, SE, SF),
    SB \= S1, SB \= S6,
    (SC = S2; SC = S7), (SE = S4; SE = S9),
    s(SB, ' '), s(SD, ' '), s(SF, ' '),

    (
        (retract(s(S8, ' ')), assert(s(S8, x)), write([S8, 15]), nl)
    ),
    vypis_p,
    test_v(x).

% Obrana před paralelními dvojicemi/trojicemi, pravidlo 20 - je třeba provést, dokud
% soupeř položil pouze tři pole, jinak je pozdě
% TODO: Perf issues

tp :-
    (s(S1, ' '); s(S1, o)), o(S1, S2, S3, S4, S5),
    s(S3, o),
    (s(S2, ' '); s(S2, o)), (s(S4, ' '); s(S4, o)), (s(S5, ' '); s(S5, o)),

    [S1X, S1Y] = S1, [S5X, S5Y] = S5, (S1X = S5X; S1Y = S5Y),
    (
        (S1X = S6X, (S6Y is S1Y-2; S6Y is S1Y+2));
        (S1Y = S6Y, (S6X is S1X-2; S6X is S1X+2))
    ), 
    S6 = [S6X, S6Y],

    s(S6, _), o(S6, S7, S8, S9, _),
    (s(S7, ' '); s(S7, o)), (s(S8, ' '); s(S8, o)), (s(S9, ' '); s(S9, o)),

    (
        (s(S2, o), s(S7, o)); 
        (s(S4, o), s(S9, o))
    ),

    (s(SB, ' '); s(SB, o)), o(SB, SC, SD, SE, SF),
    SB \= S1, SB \= S6,
    (SC = S2; SC = S7), (SE = S4; SE = S9),
    (s(SD, ' '); s(SD, o)), (s(SF, ' '); s(SF, o)),

    s(ST, _), s(SU, _), s(SV, _), s(SW, _),
    (o(ST, S7, S27, S2, _); o(SU, S2, S27, S7, _)),
    (o(SV, S4, S49, S9, _); o(SW, S9, S49, S4, _)),

    (
        (s(S2, o), s(S27, ' '), retract(s(S27, ' ')), assert(s(S27, x)), write([S27, 20]), nl);
        (s(S4, o), s(S49, ' '), retract(s(S49, ' ')), assert(s(S49, x)), write([S49, 20]), nl)
    ),
    vypis_p,
    test_v(x).

% Robíme paralelní trojice - položení třetího pole, pravidlo 16

tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, ' '), s(S3, ' '), s(S4, x), s(S5, ' '),

    [S1X, S1Y] = S1, [S5X, S5Y] = S5, (S1X = S5X; S1Y = S5Y),
    (
        (S1X = S6X, (S6Y is S1Y-2; S6Y is S1Y+2));
        (S1Y = S6Y, (S6X is S1X-2; S6X is S1X+2))
    ),
    S6 = [S6X, S6Y],

    s(S6, _), o(S6, S7, S8, S9, _),
    s(S8, ' '), s(S9, x),

    s(SB, ' '), o(SB, SC, SD, SE, SF),
    SB \= S1, SB \= S6,
    (SC = S2; SC = S7), (SE = S4; SE = S9),
    s(SB, ' '), s(SD, ' '), s(SF, ' '),

    (
        (retract(s(S3, ' ')), assert(s(S3, x)), write([S3, 16]), nl)
    ),
    vypis_p,
    test_v(x).

tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, ' '), s(S3, x), s(S4, ' '), s(S5, ' '),

    [S1X, S1Y] = S1, [S5X, S5Y] = S5, (S1X = S5X; S1Y = S5Y),
    (
        (S1X = S6X, (S6Y is S1Y-2; S6Y is S1Y+2));
        (S1Y = S6Y, (S6X is S1X-2; S6X is S1X+2))
    ),
    S6 = [S6X, S6Y],

    s(S6, _), o(S6, S7, S8, S9, _),
    s(S8, ' '), (s(S9, x); s(S7, x)),

    s(SB, ' '), o(SB, SC, SD, SE, SF),
    SB \= S1, SB \= S6,
    (SC = S2; SC = S7), (SE = S4; SE = S9),
    s(SB, ' '), s(SD, ' '), s(SF, ' '),

    (
        (retract(s(S8, ' ')), assert(s(S8, x)), write([S8, 16]), nl)
    ),
    vypis_p,
    test_v(x).

tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    (s(S2, ' '); s(S2, x)), s(S3, ' '), (s(S4, ' '); s(S4, x)), s(S5, ' '),
    (s(S2, x); s(S4, x)),

    [S1X, S1Y] = S1, [S5X, S5Y] = S5, (S1X = S5X; S1Y = S5Y),
    (
        (S1X = S6X, (S6Y is S1Y-2; S6Y is S1Y+2));
        (S1Y = S6Y, (S6X is S1X-2; S6X is S1X+2))
    ),
    S6 = [S6X, S6Y],

    s(S6, _), o(S6, S7, S8, S9, _),
    s(S8, ' '), ( (s(S9, x), s(S4, x)); (s(S7, x), s(S2, x)) ),

    s(SB, ' '), o(SB, SC, SD, SE, SF),
    SB \= S1, SB \= S6,
    (SC = S2; SC = S7), (SE = S4; SE = S9),
    s(SB, ' '), s(SD, ' '), s(SF, ' '),

    (
        (retract(s(S3, ' ')), assert(s(S3, x)), write([S3, 16]), nl)
    ),

    vypis_p,
    test_v(x).

tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, x), s(S3, ' '), s(S4, ' '), s(S5, ' '),

    [S1X, S1Y] = S1, [S5X, S5Y] = S5, (S1X = S5X; S1Y = S5Y),
    (
        (S1X = S6X, (S6Y is S1Y-2; S6Y is S1Y+2));
        (S1Y = S6Y, (S6X is S1X-2; S6X is S1X+2))
    ),
    S6 = [S6X, S6Y],

    s(S6, _), o(S6, S7, S8, S9, _),
    s(S7, x), s(S8, ' '),

    s(SB, ' '), o(SB, SC, SD, SE, SF),
    SB \= S1, SB \= S6,
    (SC = S2; SC = S7), (SE = S4; SE = S9),
    s(SB, ' '), s(SD, ' '), s(SF, ' '),

    (
        (retract(s(S3, ' ')), assert(s(S3, x)), write([S3, 16]), nl)
    ),
    vypis_p,
    test_v(x).

% Nahoře x
tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, x), s(S3, x), s(S4, ' '), s(S5, ' '),

    [S1X, S1Y] = S1, [S5X, S5Y] = S5, (S1X = S5X; S1Y = S5Y),
    (
        (S1X = S6X, (S6Y is S1Y-2; S6Y is S1Y+2));
        (S1Y = S6Y, (S6X is S1X-2; S6X is S1X+2))
    ),
    S6 = [S6X, S6Y],

    s(S6, _), o(S6, S7, S8, S9, _),
    s(S7, ' '), s(S8, ' '),

    s(SB, ' '), o(SB, SC, SD, SE, SF),
    SB \= S1, SB \= S6,
    (SC = S2; SC = S7), (SE = S4; SE = S9),
    s(SB, ' '), s(SD, ' '), s(SF, ' '),

    (
        (retract(s(S7, ' ')), assert(s(S7, x)), write([S7, 16]), nl)
    ),
    vypis_p,
    test_v(x).

tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, ' '), s(S3, x), s(S4, x), s(S5, ' '),

    [S1X, S1Y] = S1, [S5X, S5Y] = S5, (S1X = S5X; S1Y = S5Y),
    (
        (S1X = S6X, (S6Y is S1Y-2; S6Y is S1Y+2));
        (S1Y = S6Y, (S6X is S1X-2; S6X is S1X+2))
    ),
    S6 = [S6X, S6Y],

    s(S6, _), o(S6, S7, S8, S9, _),
    s(S8, ' '), s(S9, ' '),

    s(SB, ' '), o(SB, SC, SD, SE, SF),
    SB \= S1, SB \= S6,
    (SC = S2; SC = S7), (SE = S4; SE = S9),
    s(SB, ' '), s(SD, ' '), s(SF, ' '),

    (
        (retract(s(S8, ' ')), assert(s(S8, x)), write([S8, 16]), nl)
    ),
    vypis_p,
    test_v(x).

% Dole x
tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, ' '), s(S3, ' '), s(S4, ' '), s(S5, ' '),

    [S1X, S1Y] = S1, [S5X, S5Y] = S5, (S1X = S5X; S1Y = S5Y),
    (
        (S1X = S6X, (S6Y is S1Y-2; S6Y is S1Y+2));
        (S1Y = S6Y, (S6X is S1X-2; S6X is S1X+2))
    ),
    S6 = [S6X, S6Y],

    s(S6, _), o(S6, S7, S8, S9, _),
    s(S8, x), s(S9, x),

    s(SB, ' '), o(SB, SC, SD, SE, SF),
    SB \= S1, SB \= S6,
    (SC = S2; SC = S7), (SE = S4; SE = S9),
    s(SB, ' '), s(SD, ' '), s(SF, ' '),

    (
        (retract(s(S3, ' ')), assert(s(S3, x)), write([S3, 16]), nl)
    ),
    vypis_p,
    test_v(x).

tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, ' '), s(S3, ' '), s(S4, ' '), s(S5, ' '),

    [S1X, S1Y] = S1, [S5X, S5Y] = S5, (S1X = S5X; S1Y = S5Y),
    (
        (S1X = S6X, (S6Y is S1Y-2; S6Y is S1Y+2));
        (S1Y = S6Y, (S6X is S1X-2; S6X is S1X+2))
    ),
    S6 = [S6X, S6Y],

    s(S6, _), o(S6, S7, S8, S9, _),
    s(S7, x), s(S8, x),

    s(SB, ' '), o(SB, SC, SD, SE, SF),
    SB \= S1, SB \= S6,
    (SC = S2; SC = S7), (SE = S4; SE = S9),
    s(SB, ' '), s(SD, ' '), s(SF, ' '),

    (
        (retract(s(S3, ' ')), assert(s(S3, x)), write([S3, 16]), nl)
    ),
    vypis_p,
    test_v(x).


% Robíme zákeřné kříže - položení čtvrtého pole, pravidlo 31

% Útok

tp :-
    (s(S1, ' '); s(S1, x)), o(S1, S2, S3, S4, S5),
    s(S4, ' '),
    (
        (s(S2, x), s(S3, x)); 
        (s(S3, x), s(S5, x))
    ),
    (s(S5, x); s(S5, ' ')),

    (s(S6, ' '); s(S6, x)),
    o6(S6, S7, S8, S9, S4, SA),
    S7 \= S1,
    
    (s(S7, ' '); s(S7, x)),
    (s(S8, ' '); s(S8, x)),
    (s(S9, ' '); s(S9, x)),
    (s(SA, ' '); s(SA, x)),
    s(S9, ' '),
    (
        (s(S7, x), s(S8, x));
        (s(S8, x), s(SA, x))
    ),

    retract(s(S4, ' ')), assert(s(S4, x)),
    write([S4, 30]), nl,
    vypis_p,
    test_v(x).

% Tah počítače - pravidlo 30 - zákeřnější kříž - dolů doleva

tp :-
    (s(S1, ' '); s(S1, x)), o(S1, S2, S3, S4, S5),
    s(S2, ' '),
    (
        (s(S1, x), s(S3, x)); 
        (s(S3, x), s(S4, x))
    ),
    (s(S5, x); s(S5, ' ')),

    (s(S6, ' '); s(S6, x)),
    o6(S6, S7, S8, S9, S2, SA),
    S9 \= S1,
    
    (s(S7, ' '); s(S7, x)),
    (s(S8, ' '); s(S8, x)),
    (s(S9, ' '); s(S9, x)),
    (s(SA, ' '); s(SA, x)),
    s(S9, ' '),
    (
        (s(S7, x), s(S8, x));
        (s(S8, x), s(SA, x))
    ),

    retract(s(S2, ' ')), assert(s(S2, x)),
    write([S2, 30]), nl,
    vypis_p,
    test_v(x).

% Tah počítače - pravidlo 30 - zákeřnější kříž - nahoru doprava

tp :-
    (s(S1, ' '); s(S1, x)), o(S1, S2, S3, S4, S5),
    s(S4, ' '),
    (
        (s(S2, x), s(S3, x)); 
        (s(S3, x), s(S5, x))
    ),
    (s(S5, x); s(S5, ' ')),

    (s(S6, ' '); s(S6, x)),
    o6(S6, S4, S7, S8, S9, SA),
    S7 \= S5,
    
    (s(S7, ' '); s(S7, x)),
    (s(S8, ' '); s(S8, x)),
    (s(S9, ' '); s(S9, x)),
    (s(SA, ' '); s(SA, x)),
    s(S9, ' '),
    (
        (s(S6, x), s(S8, x));
        (s(S8, x), s(S9, x))
    ),

    retract(s(S4, ' ')), assert(s(S4, x)),
    write([S4, 30]), nl,
    vypis_p,
    test_v(x).
% Robíme kříže - položení čtvrtého pole, pravidlo 10

% Kříž doprava
tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, ' '), s(S3, ' '), s(S4, x), s(S5, ' '),
    s(S6, ' '), o(S6, S7, S2, S8, S9), S8 \= S3,
    s(S7, x), s(S8, x), s(S9, ' '),
    retract(s(S3, ' ')), assert(s(S3, x)),
    write([S3, 10]), nl,
    vypis_p,
    test_v(x).

tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, ' '), s(S3, x), s(S4, ' '), s(S5, ' '),
    s(S6, ' '), o(S6, S7, S2, S8, S9), S8 \= S3,
    s(S7, x), s(S8, x), s(S9, ' '),
    retract(s(S4, ' ')), assert(s(S4, x)),
    write([S4, 10]), nl,
    vypis_p,
    test_v(x).

tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, x), s(S3, x), s(S4, ' '), s(S5, ' '),
    s(S6, ' '), o(S6, S7, S4, S8, S9), S8 \= S5,
    s(S7, ' '), s(S8, x), s(S9, ' '),
    retract(s(S7, ' ')), assert(s(S7, x)),
    write([S7, 10]), nl,
    vypis_p,
    test_v(x).

tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, x), s(S3, x), s(S4, ' '), s(S5, ' '),
    s(S6, ' '), o(S6, S7, S4, S8, S9), S8 \= S5,
    s(S7, x), s(S8, ' '), s(S9, ' '),
    retract(s(S8, ' ')), assert(s(S8, x)),
    write([S8, 10]), nl,
    vypis_p,
    test_v(x).

% To samé, ale kříž doleva

tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, x), s(S3, ' '), s(S4, ' '), s(S5, ' '),
    s(S6, ' '), o(S6, S7, S4, S8, S9), S7 \= S3,
    s(S7, x), s(S8, x), s(S9, ' '),
    retract(s(S3, ' ')), assert(s(S3, x)),
    write([S3, 10]), nl,
    vypis_p,
    test_v(x).

tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, ' '), s(S3, x), s(S4, ' '), s(S5, ' '),
    s(S6, ' '), o(S6, S7, S4, S8, S9), S7 \= S3,
    s(S7, x), s(S8, x), s(S9, ' '),
    retract(s(S2, ' ')), assert(s(S2, x)),
    write([S2, 10]), nl,
    vypis_p,
    test_v(x).

tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, x), s(S3, x), s(S4, ' '), s(S5, ' '),
    s(S6, ' '), o(S6, S7, S4, S8, S9), S7 \= S3,
    s(S7, ' '), s(S8, x), s(S9, ' '),
    retract(s(S7, ' ')), assert(s(S7, x)),
    write([S7, 10]), nl,
    vypis_p,
    test_v(x).

tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, x), s(S3, x), s(S4, ' '), s(S5, ' '),
    s(S6, ' '), o(S6, S7, S4, S8, S9), S7 \= S3,
    s(S7, x), s(S8, ' '), s(S9, ' '),
    retract(s(S8, ' ')), assert(s(S8, x)),
    write([S8, 10]), nl,
    vypis_p,
    test_v(x).

% Robíme paralelní trojice - položení druhého pole, pravidlo 17

tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, ' '), s(S3, ' '), s(S4, ' '), s(S5, ' '),

    [S1X, S1Y] = S1, [S5X, S5Y] = S5, (S1X = S5X; S1Y = S5Y), [S5X, S5Y] = S5, (S1X = S5X; S1Y = S5Y),
    (
        (S1X = S6X, (S6Y is S1Y-2; S6Y is S1Y+2));
        (S1Y = S6Y, (S6X is S1X-2; S6X is S1X+2))
    ),
    S6 = [S6X, S6Y],

    s(S6, _), o(S6, S7, S8, S9, _),
    (
        (s(S7, ' '), s(S8, x));
        (s(S7, x), s(S8, ' '));
        (s(S8, x), s(S9, ' '));
        (s(S8, ' '), s(S9, x))
    ),

    s(SB, ' '), o(SB, SC, SD, SE, SF),
    SB \= S1, SB \= S6,
    (SC = S2; SC = S7), (SE = S4; SE = S9),
    s(SB, ' '), s(SD, ' '), s(SF, ' '),

    (
        (retract(s(S3, ' ')), assert(s(S3, x)), write([S3, 17.1]), nl)
    ),
    vypis_p,
    test_v(x).

tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    (s(S2, ' '); s(S2, x)), (s(S3, ' '), s(S3, x)), (s(S4, ' '), s(S4, x)), s(S5, ' '),
    (s(S2, x); s(S3, x); s(S4, x)),

    [S1X, S1Y] = S1, [S5X, S5Y] = S5, (S1X = S5X; S1Y = S5Y),
    (
        (S1X = S6X, (S6Y is S1Y-2; S6Y is S1Y+2));
        (S1Y = S6Y, (S6X is S1X-2; S6X is S1X+2))
    ),
    S6 = [S6X, S6Y],

    s(S6, _), o(S6, S7, S8, S9, _),
    s(S8, ' '),
    (
        (s(S2, x), s(S7, ' ')),
        (s(S3, x), (s(S7, ' '); s(S9, ' '))),
        (s(S4, x), s(S9, ' '))
    ),

    s(SB, ' '), o(SB, SC, SD, SE, SF),
    SB \= S1, SB \= S6,
    (SC = S2; SC = S7), (SE = S4; SE = S9),
    s(SB, ' '), s(SD, ' '), s(SF, ' '),

    (
        (retract(s(S8, ' ')), assert(s(S8, x)), write([S8, 17.2]), nl)
    ),
    vypis_p,
    test_v(x).

% Robíme kříže - položení třetího pole, pravidlo 11
tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, ' '), s(S3, x), s(S4, x), s(S5, ' '),
    s(S6, ' '), o(S6, S7, S2, S8, S9), S8 \= S3,
    s(S7, ' '), s(S8, ' '), s(S9, ' '),
    retract(s(S8, ' ')), assert(s(S8, x)),
    write([S8, 11]), nl,
    vypis_p,
    test_v(x).

tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, x), s(S3, x), s(S4, ' '), s(S5, ' '),
    s(S6, ' '), o(S6, S7, S4, S8, S9), S8 \= S5,
    s(S7, ' '), s(S8, ' '), s(S9, ' '),
    retract(s(S8, ' ')), assert(s(S8, x)),
    write([S8, 11]), nl,
    vypis_p,
    test_v(x).

tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, ' '), s(S3, ' '), s(S4, ' '), s(S5, ' '),
    s(S6, ' '), o(S6, S7, S2, S8, S9), S8 \= S3,
    s(S7, x), s(S8, x), s(S9, ' '),
    retract(s(S3, ' ')), assert(s(S3, x)),
    write([S3, 11]), nl,
    vypis_p,
    test_v(x).

tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, ' '), s(S3, ' '), s(S4, ' '), s(S5, ' '),
    s(S6, ' '), o(S6, S7, S4, S8, S9), S8 \= S5,
    s(S7, x), s(S8, x), s(S9, ' '),
    retract(s(S3, ' ')), assert(s(S3, x)),
    write([S3, 11]), nl,
    vypis_p,
    test_v(x).

% Robíme paralelní trojice - položení prvního pole, pravidlo 18

tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, ' '), s(S3, ' '), s(S4, ' '), s(S5, ' '),

    [S1X, S1Y] = S1, [S5X, S5Y] = S5, (S1X = S5X; S1Y = S5Y),
    (
        (S1X = S6X, (S6Y is S1Y-2; S6Y is S1Y+2));
        (S1Y = S6Y, (S6X is S1X-2; S6X is S1X+2))
    ),
    S6 = [S6X, S6Y],

    s(S6, _), o(S6, S7, S8, S9, _),
    s(S7, ' '), (s(S6, ' '); s(S8, ' ')),

    s(SB, ' '), o(SB, SC, SD, SE, SF),
    SB \= S1, SB \= S6,
    (SC = S2; SC = S7), (SE = S4; SE = S9),
    s(SB, ' '), s(SD, ' '), s(SF, ' '),

    (
        (retract(s(S3, ' ')), assert(s(S3, x)), write([S3, 18]), nl)
    ),
    vypis_p,
    test_v(x).

% Robíme kříže - položení druhého pole, pravidlo 12

% Doprava
tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, ' '), s(S3, x), s(S4, ' '), s(S5, ' '),
    s(S6, ' '), o(S6, S7, S2, S8, S9), S8 \= S3,
    s(S7, ' '), s(S8, ' '), s(S9, ' '),
    retract(s(S4, ' ')), assert(s(S4, x)),
    write([S4, 12]), nl,
    vypis_p,
    test_v(x).

tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, ' '), s(S3, ' '), s(S4, x), s(S5, ' '),
    s(S6, ' '), o(S6, S7, S2, S8, S9), S8 \= S3,
    s(S7, ' '), s(S8, ' '), s(S9, ' '),
    retract(s(S3, ' ')), assert(s(S3, x)),
    write([S3, 12]), nl,
    vypis_p,
    test_v(x).

% Doleva
tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, ' '), s(S3, x), s(S4, ' '), s(S5, ' '),
    s(S6, ' '), o(S6, S7, S4, S8, S9), S8 \= S5,
    s(S7, ' '), s(S8, ' '), s(S9, ' '),
    retract(s(S2, ' ')), assert(s(S2, x)),
    write([S2, 12]), nl,
    vypis_p,
    test_v(x).

tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, x), s(S3, ' '), s(S4, ' '), s(S5, ' '),
    s(S6, ' '), o(S6, S7, S4, S8, S9), S8 \= S5,
    s(S7, ' '), s(S8, ' '), s(S9, ' '),
    retract(s(S3, ' ')), assert(s(S3, x)),
    write([S3, 12]), nl,
    vypis_p,
    test_v(x).

% Robíme kříže - položení prvního pole, pravidlo 13
% Doprava
tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, ' '), s(S3, ' '), s(S4, ' '), s(S5, ' '),
    s(S6, ' '), o(S6, S7, S2, S8, S9), S8 \= S3,
    s(S7, ' '), s(S8, ' '), s(S9, ' '),
    retract(s(S3, ' ')), assert(s(S3, x)),
    write([S3, 13]), nl,
    vypis_p,
    test_v(x).

% Doleva
tp :-
    s(S1, ' '), o(S1, S2, S3, S4, S5),
    s(S2, ' '), s(S3, ' '), s(S4, ' '), s(S5, ' '),
    s(S6, ' '), o(S6, S7, S4, S8, S9), S8 \= S5,
    s(S7, ' '), s(S8, ' '), s(S9, ' '),
    retract(s(S3, ' ')), assert(s(S3, x)),
    write([S3, 13]), nl,
    vypis_p,
    test_v(x).

% Tah na volné pole, x - počítač, o - hráč
tp :-
    s(S, ' '),
    retract(s(S, ' ')), assert(s(S, x)),
    write([S, nahodny_tah]), nl,
    vypis_p,
    test_v(x).

% Tah hráče
tah(X, Y) :- tah([X, Y]).
tah(S) :-
    s(S, ' '),
    retract(s(S, ' ')), assert(s(S, o)),
    vypis_p,
    test_v(o),
    true.
    % tp.

% Objekty - 5 polí
o([X, Y], [X1, Y], [X2, Y], [X3, Y], [X4, Y]) :-
    X1 is X+1, X2 is X+2, X3 is X+3, X4 is X+4.

o([X, Y], [X, Y1], [X, Y2], [X, Y3], [X, Y4]) :-
    Y1 is Y+1, Y2 is Y+2, Y3 is Y+3, Y4 is Y+4.

o([X, Y], [X1, Y1], [X2, Y2], [X3, Y3], [X4, Y4]) :-
    X1 is X+1, X2 is X+2, X3 is X+3, X4 is X+4,
    Y1 is Y+1, Y2 is Y+2, Y3 is Y+3, Y4 is Y+4.

o([X, Y], [X1, Y1], [X2, Y2], [X3, Y3], [X4, Y4]) :-
    X1 is X+1, X2 is X+2, X3 is X+3, X4 is X+4,
    Y1 is Y-1, Y2 is Y-2, Y3 is Y-3, Y4 is Y-4.

% Objekty - 6 polí
o6([X, Y], [X1, Y], [X2, Y], [X3, Y], [X4, Y], [X5, Y]) :-
    X1 is X+1, X2 is X+2, X3 is X+3, X4 is X+4, X5 is X+5.

o6([X, Y], [X, Y1], [X, Y2], [X, Y3], [X, Y4], [X, Y5]) :-
    Y1 is Y+1, Y2 is Y+2, Y3 is Y+3, Y4 is Y+4, Y5 is Y+5.

o6([X, Y], [X1, Y1], [X2, Y2], [X3, Y3], [X4, Y4], [X5, Y5]) :-
    X1 is X+1, X2 is X+2, X3 is X+3, X4 is X+4, X5 is X+5,
    Y1 is Y+1, Y2 is Y+2, Y3 is Y+3, Y4 is Y+4, Y5 is Y+5.

o6([X, Y], [X1, Y1], [X2, Y2], [X3, Y3], [X4, Y4], [X5, Y5]) :-
    X1 is X+1, X2 is X+2, X3 is X+3, X4 is X+4, X5 is X+5,
    Y1 is Y-1, Y2 is Y-2, Y3 is Y-3, Y4 is Y-4, Y5 is Y-5.

% Test výhry
test_v(H) :-
    s(S1, H),
    o(S1, S2, S3, S4, S5),
    s(S2, H),
    s(S3, H),
    s(S4, H),
    s(S5, H),
    nl,
    write([vyhra, H, S1, S2, S3, S4, S5]).

test_v(_).

% Výpis pole
vypis_p :-
    s([0, 0], H00), s([0, 1], H01), s([0, 2], H02), s([0, 3], H03), s([0, 4], H04), s([0, 5], H05), s([0, 6], H06), s([0, 7], H07), s([0, 8], H08), s([0, 9], H09),
    s([1, 0], H10), s([1, 1], H11), s([1, 2], H12), s([1, 3], H13), s([1, 4], H14), s([1, 5], H15), s([1, 6], H16), s([1, 7], H17), s([1, 8], H18), s([1, 9], H19),
    s([2, 0], H20), s([2, 1], H21), s([2, 2], H22), s([2, 3], H23), s([2, 4], H24), s([2, 5], H25), s([2, 6], H26), s([2, 7], H27), s([2, 8], H28), s([2, 9], H29),
    s([3, 0], H30), s([3, 1], H31), s([3, 2], H32), s([3, 3], H33), s([3, 4], H34), s([3, 5], H35), s([3, 6], H36), s([3, 7], H37), s([3, 8], H38), s([3, 9], H39),
    s([4, 0], H40), s([4, 1], H41), s([4, 2], H42), s([4, 3], H43), s([4, 4], H44), s([4, 5], H45), s([4, 6], H46), s([4, 7], H47), s([4, 8], H48), s([4, 9], H49),
    s([5, 0], H50), s([5, 1], H51), s([5, 2], H52), s([5, 3], H53), s([5, 4], H54), s([5, 5], H55), s([5, 6], H56), s([5, 7], H57), s([5, 8], H58), s([5, 9], H59),
    s([6, 0], H60), s([6, 1], H61), s([6, 2], H62), s([6, 3], H63), s([6, 4], H64), s([6, 5], H65), s([6, 6], H66), s([6, 7], H67), s([6, 8], H68), s([6, 9], H69),
    s([7, 0], H70), s([7, 1], H71), s([7, 2], H72), s([7, 3], H73), s([7, 4], H74), s([7, 5], H75), s([7, 6], H76), s([7, 7], H77), s([7, 8], H78), s([7, 9], H79),
    s([8, 0], H80), s([8, 1], H81), s([8, 2], H82), s([8, 3], H83), s([8, 4], H84), s([8, 5], H85), s([8, 6], H86), s([8, 7], H87), s([8, 8], H88), s([8, 9], H89),
    s([9, 0], H90), s([9, 1], H91), s([9, 2], H92), s([9, 3], H93), s([9, 4], H94), s([9, 5], H95), s([9, 6], H96), s([9, 7], H97), s([9, 8], H98), s([9, 9], H99),

    write('   --- --- --- --- --- --- --- --- --- ---'), nl,
    write('9 | '), write(H09), write(' | '), write(H19), write(' | '), write(H29), write(' | '), write(H39), write(' | '), write(H49),
    write(' | '), write(H59), write(' | '), write(H69), write(' | '), write(H79), write(' | '), write(H89), write(' | '), write(H99), write(' |'),
    nl,
    write('   --- --- --- --- --- --- --- --- --- ---'), nl,
    write('8 | '), write(H08), write(' | '), write(H18), write(' | '), write(H28), write(' | '), write(H38), write(' | '), write(H48),
    write(' | '), write(H58), write(' | '), write(H68), write(' | '), write(H78), write(' | '), write(H88), write(' | '), write(H98), write(' |'),
    nl,
    write('   --- --- --- --- --- --- --- --- --- ---'), nl,
    write('7 | '), write(H07), write(' | '), write(H17), write(' | '), write(H27), write(' | '), write(H37), write(' | '), write(H47),
    write(' | '), write(H57), write(' | '), write(H67), write(' | '), write(H77), write(' | '), write(H87), write(' | '), write(H97), write(' |'),
    nl,
    write('   --- --- --- --- --- --- --- --- --- ---'), nl,
    write('6 | '), write(H06), write(' | '), write(H16), write(' | '), write(H26), write(' | '), write(H36), write(' | '), write(H46),
    write(' | '), write(H56), write(' | '), write(H66), write(' | '), write(H76), write(' | '), write(H86), write(' | '), write(H96), write(' |'),
    nl,
    write('   --- --- --- --- --- --- --- --- --- ---'), nl,
    write('5 | '), write(H05), write(' | '), write(H15), write(' | '), write(H25), write(' | '), write(H35), write(' | '), write(H45),
    write(' | '), write(H55), write(' | '), write(H65), write(' | '), write(H75), write(' | '), write(H85), write(' | '), write(H95), write(' |'),
    nl,
    write('   --- --- --- --- --- --- --- --- --- ---'), nl,
    write('4 | '), write(H04), write(' | '), write(H14), write(' | '), write(H24), write(' | '), write(H34), write(' | '), write(H44),
    write(' | '), write(H54), write(' | '), write(H64), write(' | '), write(H74), write(' | '), write(H84), write(' | '), write(H94), write(' |'),
    nl,
    write('   --- --- --- --- --- --- --- --- --- ---'), nl,
    write('3 | '), write(H03), write(' | '), write(H13), write(' | '), write(H23), write(' | '), write(H33), write(' | '), write(H43),
    write(' | '), write(H53), write(' | '), write(H63), write(' | '), write(H73), write(' | '), write(H83), write(' | '), write(H93), write(' |'),
    nl,
    write('   --- --- --- --- --- --- --- --- --- ---'), nl,
    write('2 | '), write(H02), write(' | '), write(H12), write(' | '), write(H22), write(' | '), write(H32), write(' | '), write(H42),
    write(' | '), write(H52), write(' | '), write(H62), write(' | '), write(H72), write(' | '), write(H82), write(' | '), write(H92), write(' |'),
    nl,
    write('   --- --- --- --- --- --- --- --- --- ---'), nl,
    write('1 | '), write(H01), write(' | '), write(H11), write(' | '), write(H21), write(' | '), write(H31), write(' | '), write(H41),
    write(' | '), write(H51), write(' | '), write(H61), write(' | '), write(H71), write(' | '), write(H81), write(' | '), write(H91), write(' |'),
    nl,
    write('   --- --- --- --- --- --- --- --- --- ---'), nl,
    write('0 | '), write(H00), write(' | '), write(H10), write(' | '), write(H20), write(' | '), write(H30), write(' | '), write(H40),
    write(' | '), write(H50), write(' | '), write(H60), write(' | '), write(H70), write(' | '), write(H80), write(' | '), write(H90), write(' |'),
    nl,
    write('   --- --- --- --- --- --- --- --- --- ---'), nl,
    write('    0   1   2   3   4   5   6   7   8   9 '), nl.

% Definice pole - vygenerováno Python skriptem
s([4, 4], ' ').
s([4, 5], ' ').
s([3, 5], ' ').
s([3, 4], ' ').
s([3, 3], ' ').
s([4, 3], ' ').
s([5, 3], ' ').
s([5, 4], ' ').
s([5, 5], ' ').
s([4, 6], ' ').
s([3, 6], ' ').
s([2, 6], ' ').
s([2, 5], ' ').
s([2, 4], ' ').
s([2, 3], ' ').
s([2, 2], ' ').
s([3, 2], ' ').
s([4, 2], ' ').
s([5, 2], ' ').
s([6, 2], ' ').
s([6, 3], ' ').
s([6, 4], ' ').
s([6, 5], ' ').
s([6, 6], ' ').
s([5, 6], ' ').
s([4, 7], ' ').
s([3, 7], ' ').
s([2, 7], ' ').
s([1, 7], ' ').
s([1, 6], ' ').
s([1, 5], ' ').
s([1, 4], ' ').
s([1, 3], ' ').
s([1, 2], ' ').
s([1, 1], ' ').
s([2, 1], ' ').
s([3, 1], ' ').
s([4, 1], ' ').
s([5, 1], ' ').
s([6, 1], ' ').
s([7, 1], ' ').
s([7, 2], ' ').
s([7, 3], ' ').
s([7, 4], ' ').
s([7, 5], ' ').
s([7, 6], ' ').
s([7, 7], ' ').
s([6, 7], ' ').
s([5, 7], ' ').
s([4, 8], ' ').
s([3, 8], ' ').
s([2, 8], ' ').
s([1, 8], ' ').
s([0, 8], ' ').
s([0, 7], ' ').
s([0, 6], ' ').
s([0, 5], ' ').
s([0, 4], ' ').
s([0, 3], ' ').
s([0, 2], ' ').
s([0, 1], ' ').
s([0, 0], ' ').
s([1, 0], ' ').
s([2, 0], ' ').
s([3, 0], ' ').
s([4, 0], ' ').
s([5, 0], ' ').
s([6, 0], ' ').
s([7, 0], ' ').
s([8, 0], ' ').
s([8, 1], ' ').
s([8, 2], ' ').
s([8, 3], ' ').
s([8, 4], ' ').
s([8, 5], ' ').
s([8, 6], ' ').
s([8, 7], ' ').
s([8, 8], ' ').
s([7, 8], ' ').
s([6, 8], ' ').
s([5, 8], ' ').
s([4, 9], ' ').
s([3, 9], ' ').
s([2, 9], ' ').
s([1, 9], ' ').
s([0, 9], ' ').
s([9, 0], ' ').
s([9, 1], ' ').
s([9, 2], ' ').
s([9, 3], ' ').
s([9, 4], ' ').
s([9, 5], ' ').
s([9, 6], ' ').
s([9, 7], ' ').
s([9, 8], ' ').
s([9, 9], ' ').
s([8, 9], ' ').
s([7, 9], ' ').
s([6, 9], ' ').
s([5, 9], ' ').


