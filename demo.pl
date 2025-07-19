% =====================
% Battleships Puzzle Solver
% =====================

% Dynamic Predicates
:- dynamic cell/3.  % cell(Row, Col, Type) -> Type is 'ship' or 'water'.
:- dynamic row_count/2.  % row_count(Row, NumShips)
:- dynamic col_count/2.  % col_count(Col, NumShips)
:- dynamic grid_size/2.  % grid_size(Rows, Cols)
:- dynamic fleet/2.      % fleet(ShipType, Count) -> e.g., fleet(battleship, 1).
:- dynamic temp_ship/2.  % temp_ship(Row, Col) -> Used for temporary processing during validation.

% --- Ship Types and their corresponding lengths
ship_type(submarine, 1).
ship_type(destroyer, 2).
ship_type(cruiser, 3).
ship_type(battleship, 4).

% =====================
% INITIALIZATION
% =====================

% --- Clear all facts (to reinitialize the puzzle)
clear :-
	retractall(cell(_, _, _)),
	retractall(row_count(_, _)),
	retractall(col_count(_, _)),
	retractall(grid_size(_, _)),
	retractall(fleet(_, _)),
	retractall(temp_ship(_, _)).

% --- Load Sample Game 1 (3x3 grid)
load_game(1) :-
	clear,
	assert(grid_size(3, 3)),
	% --- Number of ship segements per row
	assert(row_count(1, 2)),
	assert(row_count(2, 0)),
	assert(row_count(3, 0)),
	% --- Number of ship segements per column
	assert(col_count(1, 1)),
	assert(col_count(2, 1)),
	assert(col_count(3, 0)),
    % Fleet definition
    assert(fleet(destroyer, 1)),
    assert(fleet(submarine, 0)), % Explicitly state zero for other types
    assert(fleet(cruiser, 0)),
    assert(fleet(battleship, 0)),
	% Ship placements
	assert(cell(1, 1, ship)),
	assert(cell(1, 2, ship)).

% --- Load Sample Game 2 (4x4 grid)
load_game(2) :-
	clear,
	assert(grid_size(4, 4)),
	% --- Number of ship segements per row
	assert(row_count(1, 1)),
	assert(row_count(2, 1)),
	assert(row_count(3, 1)),
	assert(row_count(4, 1)),
	% --- Number of ship segements per column
	assert(col_count(1, 0)),
	assert(col_count(2, 3)),
	assert(col_count(3, 0)),
	assert(col_count(4, 1)),
    % Fleet definition
    assert(fleet(destroyer, 0)),
    assert(fleet(submarine, 1)),
    assert(fleet(cruiser, 1)),
    assert(fleet(battleship, 0)),
	% Ship placements
	assert(cell(1, 2, ship)),
	% assert(cell(1, 3, ship)),
	% assert(cell(3, 4, ship)),
	assert(cell(4, 4, ship)).

% --- Load Sample Game 3 (10x10 grid)
load_game(3) :-
	clear,
	assert(grid_size(10, 10)),
	% --- Number of ship segements per row
	assert(row_count(1, 4)),
	assert(row_count(2, 0)),
	assert(row_count(3, 2)),
	assert(row_count(4, 1)),
	assert(row_count(5, 2)),
	assert(row_count(6, 1)),
	assert(row_count(7, 2)),
	assert(row_count(8, 2)),
	assert(row_count(9, 5)),
	assert(row_count(10, 1)),
	% --- Number of ship segements per column
	assert(col_count(1, 1)),
	assert(col_count(2, 1)),
	assert(col_count(3, 6)),
	assert(col_count(4, 1)),
	assert(col_count(5, 4)),
	assert(col_count(6, 1)),
	assert(col_count(7, 3)),
	assert(col_count(8, 1)),
	assert(col_count(9, 1)),
	assert(col_count(10, 1)),
	% Fleet definition
    assert(fleet(submarine, 4)),
    assert(fleet(destroyer, 3)),
    assert(fleet(cruiser, 2)),
    assert(fleet(battleship, 1)),
	% Ship placements
	assert(cell(4, 7, ship)),
	assert(cell(5, 5, ship)),
	assert(cell(7, 8, water)).

% --- Load Sample Game 4 (3x3 grid)
load_game(4) :-
	clear,
	assert(grid_size(3, 3)),
	% --- Number of ship segements per row
	assert(row_count(1, 2)),
	assert(row_count(2, 0)),
	assert(row_count(3, 1)),
	% --- Number of ship segements per column
	assert(col_count(1, 2)),
	assert(col_count(2, 1)),
	assert(col_count(3, 0)),
	% Fleet definition
    assert(fleet(submarine, 1)),
    assert(fleet(destroyer, 1)),
    assert(fleet(cruiser, 0)),
    assert(fleet(battleship, 0)),
	% Ship placements
	assert(cell(1, 1, ship)),
	% assert(cell(2, 1, ship)),
	assert(cell(1, 2, ship)).

% --- Load Sample Game 5 (5x5 grid) - Valid Fleet
load_game(5) :-
	clear,
	assert(grid_size(5, 5)),
	% --- Number of ship segments per row
	assert(row_count(1, 1)),
	assert(row_count(2, 2)),
	assert(row_count(3, 1)),
	assert(row_count(4, 1)),
	assert(row_count(5, 1)),
	% --- Number of ship segments per column
	assert(col_count(1, 1)),
	assert(col_count(2, 2)),
	assert(col_count(3, 0)),
	assert(col_count(4, 1)),
	assert(col_count(5, 2)),
	% Fleet definition
	assert(fleet(destroyer, 1)),   % size 2
	assert(fleet(submarine, 4)),  % size 1
	assert(fleet(cruiser, 0)),
	assert(fleet(battleship, 0)),
	% Ship placements
	assert(cell(1, 5, ship)),       % submarine
	assert(cell(2, 1, ship)),       % destroyer start
	assert(cell(2, 2, ship)),       % destroyer end
	assert(cell(4, 2, ship)),       % submarine
	assert(cell(3, 5, ship)),       % submarine
	assert(cell(5, 4, ship)).       % submarine

% --- Load Sample Game 6 (3x3 grid) - Diagonal ship (invalid)
load_game(6) :-
	clear,
	assert(grid_size(3, 3)),
	% --- Number of ship segments per row
	assert(row_count(1, 1)),
	assert(row_count(2, 1)),
	assert(row_count(3, 0)),
	% --- Number of ship segments per column
	assert(col_count(1, 1)),
	assert(col_count(2, 0)),
	assert(col_count(3, 1)),
	% Fleet definition
	assert(fleet(submarine, 2)),
	assert(fleet(destroyer, 0)),
	assert(fleet(cruiser, 0)),
	assert(fleet(battleship, 0)),
	% Diagonally connected ships
	% assert(cell(1, 1, ship)),
	assert(cell(2, 2, ship)).  % diagonal → should be invalid

% --- Load Sample Game 7 (5x5 grid) - Too many destroyers
load_game(7) :-
	clear,
	assert(grid_size(5, 5)),
	% --- Number of ship segments per row
	assert(row_count(1, 2)),
	assert(row_count(2, 2)),
	assert(row_count(3, 0)),
	assert(row_count(4, 0)),
	assert(row_count(5, 0)),
	% --- Number of ship segments per column
	assert(col_count(1, 1)),
	assert(col_count(2, 1)),
	assert(col_count(3, 0)),
	assert(col_count(4, 1)),
	assert(col_count(5, 1)),
	% Fleet definition
	assert(fleet(destroyer, 2)), 
	assert(fleet(submarine, 0)),
	assert(fleet(cruiser, 0)),
	assert(fleet(battleship, 0)),
	% Two destroyers (invalid)
	assert(cell(1, 1, ship)),
	% assert(cell(1, 2, ship)),
	% assert(cell(2, 5, ship)),
	assert(cell(2, 4, ship)).

% --- Load Sample Game 8 (5x5) - Correct counts, but impossible fleet
load_game(8) :-
    clear,
    assert(grid_size(5, 5)),
    % --- Number of ship segments per row
    assert(row_count(1, 2)),
    assert(row_count(2, 0)),
    assert(row_count(3, 2)),
    assert(row_count(4, 0)),
    assert(row_count(5, 1)),
    % --- Number of ship segments per column
    assert(col_count(1, 1)),
    assert(col_count(2, 1)),
    assert(col_count(3, 1)),
    assert(col_count(4, 1)),
    assert(col_count(5, 1)),
    % --- Required Fleet (1 cruiser, 1 destroyer)
    assert(fleet(cruiser, 1)),      % size 3
    assert(fleet(destroyer, 1)),    % size 2
    assert(fleet(battleship, 0)),
    assert(fleet(submarine, 0)),
    % --- Deceptive ship placements
    assert(cell(1, 2, ship)),
    assert(cell(1, 4, ship)),
    assert(cell(3, 1, ship)),
    assert(cell(3, 5, ship)),
    assert(cell(5, 3, ship)).

% --- Load Sample Game 9 (6x6)
load_game(9) :-
    clear,
    assert(grid_size(6, 6)),
    % --- Number of ship segments per row
    assert(row_count(1, 4)),
    assert(row_count(2, 0)),
    assert(row_count(3, 2)),
    assert(row_count(4, 1)),
    assert(row_count(5, 2)),
    assert(row_count(6, 1)),
    % --- Number of ship segments per column
    assert(col_count(1, 1)),
    assert(col_count(2, 0)),
    assert(col_count(3, 4)),
    assert(col_count(4, 0)),
    assert(col_count(5, 3)),
    assert(col_count(6, 2)),
    % --- Required Fleet (1 cruiser, 1 destroyer)
	assert(fleet(submarine, 3)),
    assert(fleet(destroyer, 2)),    % size 2
    assert(fleet(cruiser, 1)),      % size 3
    assert(fleet(battleship, 0)),
    % --- ship placements
    assert(cell(1, 1, ship)),
    assert(cell(4, 3, ship)).

% --- Initialize the puzzle with facts and print the board ---
init(Game) :-
	clear,
	load_game(Game),
	print_board.

% =====================
% BOARD PRINTING
% =====================

% --- Print the current board state
print_board :-
	grid_size(Rows, Cols),
	write('  '),
	print_col_headers(1, Cols),
	nl,
	print_rows_with_counts(1, Rows, Cols),
	write('  '),
	print_border(1, Cols),
	nl,
	write('  '),
	print_col_counts(1, Cols),
	nl.

% --- Print Columns' headers
print_col_headers(Col, MaxCol) :-
	Col > MaxCol,
	!.
print_col_headers(Col, MaxCol) :-
	write(' '),
	write(Col),
	write(' '),
	Next is Col + 1,
	print_col_headers(Next, MaxCol).

% --- Print Rows with count
print_rows_with_counts(Row, MaxRow, _) :-
	Row > MaxRow,
	!.
print_rows_with_counts(Row, MaxRow, Cols) :-
	write(Row),
	write(' '),
	print_columns(Row, 1, Cols),
	write(' | '),
	row_count(Row, Count),
	write(Count),
	nl,
	Next is Row + 1,
	print_rows_with_counts(Next, MaxRow, Cols).

% --- Print Columns' count
print_col_counts(Col, MaxCol) :-
	Col > MaxCol,
	!.
print_col_counts(Col, MaxCol) :-
	col_count(Col, Count),
	write(' '),
	write(Count),
	write(' '),
	Next is Col + 1,
	print_col_counts(Next, MaxCol).

% --- Print lower border ---
print_border(Col, MaxCol) :-
	Col > MaxCol,
	!.
print_border(Col, MaxCol) :-
	write('---'),
	Next is Col + 1,
	print_border(Next, MaxCol).

% --- Print Rows
print_rows(Row, MaxRow, _) :-
	Row > MaxRow,
	!.
print_rows(Row, MaxRow, Cols) :-
	print_columns(Row, 1, Cols),
	nl,
	NextRow is Row + 1,
	print_rows(NextRow, MaxRow, Cols).

% --- Print Columns
print_columns(_, Col, MaxCol) :-
	Col > MaxCol,
	!.
print_columns(Row, Col, MaxCol) :-
	print_cell(Row, Col),
	NextCol is Col + 1,
	print_columns(Row, NextCol, MaxCol).

% --- Print Cell
print_cell(Row, Col) :-
	cell(Row, Col, ship),
	write(' S '),
	!.
print_cell(Row, Col) :-
	cell(Row, Col, water),
	write(' ~ '),
	!.
print_cell(_, _) :-
	write(' . ').

% =====================
% VALIDATION LOGIC
% =====================

% --- 1. Validate Row and Column Counts ---

% Base case: passed the last column
h_count_ships_in_row(_Row, Col, MaxCol, Acc, Acc) :-
	Col > MaxCol,
	!.
% Case 1: cell at (Row, Col) is ship
h_count_ships_in_row(Row, Col, MaxCol, Acc, Total) :-
	Col =< MaxCol,
	cell(Row, Col, ship),
	!,
	NewAcc is Acc + 1,
	NextCol is Col + 1,
	h_count_ships_in_row(Row, NextCol, MaxCol, NewAcc, Total).
% Case 2: cell at (Row, Col) is not ship (or unknown)
h_count_ships_in_row(Row, Col, MaxCol, Acc, Total) :-
	Col =< MaxCol,
	NextCol is Col + 1,
	h_count_ships_in_row(Row, NextCol, MaxCol, Acc, Total).
% --- Calculate the number of ship segments currently placed in a given row
count_ships_in_row(Row, Total) :-
	grid_size(_, MaxCol),
	h_count_ships_in_row(Row, 1, MaxCol, 0, Total).

% --- check if the actual count of ships (from the predicates above) matches the expected count defined by the row_count
validate_rows(Current, MaxRow) :-
	Current > MaxRow,
	!.
validate_rows(Current, MaxRow) :-
	row_count(Current, Expected),
	count_ships_in_row(Current, Actual),
	(Actual =:= Expected ->
	true;
	format('WRONG : Row ~w has ~w ship segments, expected ~w.~n', [Current, Actual, Expected]),
		fail),
	Next is Current + 1,
	validate_rows(Next, MaxRow).

% Base case: passed the last row
h_count_ships_in_col(_Col, Row, MaxRow, Acc, Acc) :-
	Row > MaxRow,
	!.
% Case 1: cell at (Row, Col) is ship
h_count_ships_in_col(Col, Row, MaxRow, Acc, Total) :-
	Row =< MaxRow,
	cell(Row, Col, ship),
	!,
	NewAcc is Acc + 1,
	NextRow is Row + 1,
	h_count_ships_in_col(Col, NextRow, MaxRow, NewAcc, Total).
% Case 2: cell at (Row, Col) is not ship (or unknown)
h_count_ships_in_col(Col, Row, MaxRow, Acc, Total) :-
	Row =< MaxRow,
	NextRow is Row + 1,
	h_count_ships_in_col(Col, NextRow, MaxRow, Acc, Total).
% --- Calculate the number of ship segments currently placed in a given Column
count_ships_in_col(Col, Total) :-
	grid_size(MaxRow, _),
	h_count_ships_in_col(Col, 1, MaxRow, 0, Total).

% --- check if the actual count of ships (from the predicates above) matches the expected count defined by the col_count
validate_cols(Current, MaxCol) :-
	Current > MaxCol,
	!.
validate_cols(Current, MaxCol) :-
	col_count(Current, Expected),
	count_ships_in_col(Current, Actual),
	(Actual =:= Expected ->
	true;
	format('WRONG : Column ~w has ~w ship segments, expected ~w.~n', [Current, Actual, Expected]),
		fail),
	Next is Current + 1,
	validate_cols(Next, MaxCol).

% --- joins both the validate predicates
validate_rows_and_cols :-
	grid_size(MaxRow, MaxCol),
	validate_rows(1, MaxRow),
	validate_cols(1, MaxCol).

% --- 2. Validate Ship Contacts (No touching ships) ---

check_ship_contacts :-
    \+ ( cell(R, C, ship), (
            % Check diagonal adjacency
            (R1 is R - 1, C1 is C - 1, cell(R1, C1, ship));
            (R1 is R - 1, C1 is C + 1, cell(R1, C1, ship));
            (R1 is R + 1, C1 is C - 1, cell(R1, C1, ship));
            (R1 is R + 1, C1 is C + 1, cell(R1, C1, ship));
            % Check for "cross" formations (part of the same ship check)
            % A segment cannot have both horizontal and vertical neighbors unless it's a single submarine.
            ( (cell(R, C-1, ship) ; cell(R, C+1, ship)) , (cell(R-1, C, ship) ; cell(R+1, C, ship)))
         )
       ), !.
check_ship_contacts :-
    format('VALIDATION FAILED: Ships are touching illegally (diagonally or forming a cross).~n'),
    fail.

% --- 3. Validate Ship Shapes and Fleet Count ---

% Entry point to validate the fleet
validate_fleet :-
	forall(cell(R, C, ship), assertz(temp_ship(R, C))),
    count_all_ships(0, 0, 0, 0, counts(B, C, D, S)),
	format('Found: ~w battleship(s), ~w cruiser(s), ~w destroyer(s), ~w submarine(s)~n', [B, C, D, S]), % debug
    fleet(battleship, ExpB), B =:= ExpB,
    fleet(cruiser,    ExpC), C =:= ExpC,
    fleet(destroyer,  ExpD), D =:= ExpD,
    fleet(submarine,  ExpS), S =:= ExpS,
    !.
validate_fleet :-
    % This clause is reached if any of the checks above fail.
    retractall(temp_ship(_, _)), % Ensure cleanup on failure
    format('VALIDATION FAILED: The count of ship types does not match the defined fleet.~n'),
    fail.

% DFS-based collection and counting of all ships
count_all_ships(B, C, D, S, counts(B, C, D, S)) :-
    \+ temp_ship(_, _), !.
count_all_ships(B_in, C_in, D_in, S_in, FinalCounts) :-
    temp_ship(R, C),
    find_full_ship(R, C, Length),
    update_counts(Length, B_in, C_in, D_in, S_in, B_out, C_out, D_out, S_out),
    count_all_ships(B_out, C_out, D_out, S_out, FinalCounts).

% Initiates DFS to retrieve full ship and its length
find_full_ship(R, C, Length) :-
    retract(temp_ship(R, C)),
    dfs(R, C, 1, Length).

% DFS traversal for connected ship cells
dfs(R, C, Acc, Length) :-
    (   adjacent_ship(R, C, R1, C1)
    ->  retract(temp_ship(R1, C1)),
        Acc1 is Acc + 1,
        dfs(R1, C1, Acc1, Length)
    ;   Length = Acc
    ).

% Orthogonally adjacent ship cell
adjacent_ship(R, C, R1, C1) :-
    (
        R1 is R - 1, C1 is C, temp_ship(R1, C1);
        R1 is R + 1, C1 is C, temp_ship(R1, C1);
        R1 is R,     C1 is C - 1, temp_ship(R1, C1);
        R1 is R,     C1 is C + 1, temp_ship(R1, C1)
    ), !.  % DFS: only explore one adjacent cell at a time

% Updates the counts of ships based on length
update_counts(4, B, C, D, S, B1, C, D, S) :- B1 is B + 1.
update_counts(3, B, C, D, S, B, C1, D, S) :- C1 is C + 1.
update_counts(2, B, C, D, S, B, C, D1, S) :- D1 is D + 1.
update_counts(1, B, C, D, S, B, C, D, S1) :- S1 is S + 1.


% --- top level validation predicates
validate_board :-
    validate_rows_and_cols,
    check_ship_contacts,
    validate_fleet.

% =====================
% SOLVER ENTRY POINT
% =====================

solve :-
	(validate_board ->
	write('SUCCESS: Board is valid and matches all constraints.'),
		nl;
	write('FAILURE: Board is invalid.'),
		nl),
	!.

% =====================
% BRUTEFORCE SOLVER
% =====================

% --- Top-level predicate to find a solution for a given game
find_solution(Game) :-
    init(Game),
    write('--- Attempting to solve Game '), write(Game), write(' ---'), nl,
    write('Initial state:'), nl,
    print_board,nl,
	pre_process_board, % Deduce obvious water placements first
    write('State after pre-processing:'), nl,
    print_board, nl,
    find_unknown_cells(Unknowns),
    ( solve_puzzle(Unknowns) ->
        write('--- SOLUTION FOUND ---'), nl,
        print_board
    ;
        write('--- NO SOLUTION FOUND ---'), nl
    ).

% --- Pre-computation: Fill in all logically required water cells
pre_process_board :-
    mark_remaining_water_after_ship_counts.

% --- mark fullfilled Rows/Columns with water ---
mark_remaining_water_after_ship_counts :-
    grid_size(MaxR, MaxC),
    mark_filled_rows(MaxR, MaxC),
    mark_filled_cols(MaxR, MaxC).

mark_filled_rows(MaxR, MaxC) :-
    forall(
        between(1, MaxR, Row),
        (
            row_count(Row, Expected),
            count_ships_in_row(Row, Actual),
            Expected =:= Actual
        ->
            mark_unknowns_as_water_in_row(Row, MaxC)
        ; true
        )
    ).

mark_unknowns_as_water_in_row(Row, MaxC) :-
    forall(
        between(1, MaxC, Col),
        (
            \+ cell(Row, Col, _)
        -> assertz(cell(Row, Col, water))
        ; true
        )
    ).

mark_filled_cols(MaxR, MaxC) :-
    forall(
        between(1, MaxC, Col),
        (
            col_count(Col, Expected),
            count_ships_in_col(Col, Actual),
            Expected =:= Actual
        ->
            mark_unknowns_as_water_in_col(Col, MaxR)
        ; true
        )
    ).

mark_unknowns_as_water_in_col(Col, MaxR) :-
    forall(
        between(1, MaxR, Row),
        (
            \+ cell(Row, Col, _)
        -> assertz(cell(Row, Col, water))
        ; true
        )
    ).

% --- Collect all cells that are not yet defined as 'ship' or 'water'
find_unknown_cells(Unknowns) :-
    grid_size(MaxR, MaxC),
    findall((R,C),
            (between(1, MaxR, R),
             between(1, MaxC, C),
             \+ cell(R, C, _)),
            Unknowns).

% --- Main backtracking predicate
% Base case: No more unknown cells, the board is full. Check final validity.
solve_puzzle([]) :- validate_board, !.

% Recursive step: Try placing 'water' or 'ship' in the next unknown cell.
solve_puzzle([(R,C)|Rest]) :-
    % Option 1: Place 'water' if is a promising move
	is_promising_water(R, C),
    assertz(cell(R, C, water)),
	format("Placing water at (~w, ~w):\n", [R, C]),
    print_board,
    ( solve_puzzle(Rest) ->
        true
    ;
        % Backtrack if placing water didn't work
        retract(cell(R, C, water)),
        fail
    ).

solve_puzzle([(R,C)|Rest]) :-
    % Option 2: Place 'ship', but only if it's a promising move
    is_promising_ship(R, C),
    assertz(cell(R, C, ship)),
	format("Placing ship segment at (~w, ~w):\n", [R, C]),
    print_board,
    ( solve_puzzle(Rest) ->
        true % If it leads to a solution, we're done.
    ;
        % Backtrack if placing ship didn't work
        retract(cell(R, C, ship)),
        fail
    ).

% --- Pruning predicates for the solver ---

% A ship placement is promising if it doesn't violate counts or contact rules.
is_promising_ship(R, C) :-
    count_ships_in_row(R, RCount), row_count(R, RMax), RCount < RMax,
    count_ships_in_col(C, CCount), col_count(C, CMax), CCount < CMax,
    \+ illegal_touch(R, C).

% A water placement is promising if it doesn't make it impossible to meet counts.
is_promising_water(R, C) :-
    row_count(R, RMax), count_ships_in_row(R, RCount), count_unknown_in_row(R, RUnknown),
    RCount + (RUnknown - 1) >= RMax, % Ships placed + remaining unknowns must be enough
    col_count(C, CMax), count_ships_in_col(C, CCount), count_unknown_in_col(C, CUnknown),
    CCount + (CUnknown - 1) >= CMax.

% --- Helpers for pruning checks ---
count_unknown_in_row(R, Count) :-
    grid_size(_, MaxC),
    findall(C, (between(1, MaxC, C), \+ cell(R, C, _)), Cs),
    length(Cs, Count).

count_unknown_in_col(C, Count) :-
    grid_size(MaxR, _),
    findall(R, (between(1, MaxR, R), \+ cell(R, C, _)), Rs),
    length(Rs, Count).

% --- Check for illegal contacts around a potential new ship cell at (R,C)
illegal_touch(R, C) :-
    % Check for diagonal neighbors
    ( R1 is R-1, C1 is C-1, cell(R1, C1, ship) );
    ( R1 is R-1, C1 is C+1, cell(R1, C1, ship) );
    ( R1 is R+1, C1 is C-1, cell(R1, C1, ship) );
    ( R1 is R+1, C1 is C+1, cell(R1, C1, ship) ).
illegal_touch(R, C) :-
    % Check if placing a ship here would form an illegal corner.
    % This happens if the new piece connects to existing pieces both vertically and horizontally.
    (cell(R-1, C, ship) ; cell(R+1, C, ship)), % Is there a vertical neighbor?
    (cell(R, C-1, ship) ; cell(R, C+1, ship)). % Is there a horizontal neighbor?