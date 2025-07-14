# proship

% -------------------Neu----------------------

%  help track visited cells and ship segments
:- dynamic visited/2.
:- dynamic found_ship/3.

% Try to mark the whole ship starting at (R, C)
explore_ship(R, C, Dir, Length, FinalLength) :-
    \+ visited(R, C),
    cell(R, C, ship),
    assert(visited(R, C)),
    NextLen is Length + 1,
    (
        Dir = horizontal -> C1 is C + 1, explore_ship(R, C1, horizontal, NextLen, FinalLength);
        Dir = vertical -> R1 is R + 1, explore_ship(R1, C, vertical, NextLen, FinalLength);
        FinalLength = NextLen
    ).
explore_ship(_, _, _, Length, Length).


% Detect ships and store their lengths
detect_ships :-
    cell(R, C, ship),
    \+ visited(R, C),
    (
        (C1 is C + 1, cell(R, C1, ship), explore_ship(R, C, horizontal, 0, Len), assert(found_ship(R, C, Len)));
        (R1 is R + 1, cell(R1, C, ship), explore_ship(R, C, vertical, 0, Len), assert(found_ship(R, C, Len)));
        explore_ship(R, C, none, 0, Len), assert(found_ship(R, C, Len))  % submarine
    ),
    fail.
detect_ships.


% Validate ship lengths 
validate_found_ship_lengths :-
    found_ship(R, C, Len),
    \+ ship_type(_, Len),
    format('Invalid ship of length ~w at (~w,~w)~n', [Len, R, C]),
    fail.
validate_found_ship_lengths.


% Clear temporary data
clear_ship_tracking :-
    retractall(visited(_, _)),
    retractall(found_ship(_, _, _)).

% Final validation predicate to plug in
validate_ships_by_type :-
    clear_ship_tracking,
    detect_ships,
    validate_found_ship_lengths,
    clear_ship_tracking.

% -------------Neu-----------------------