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
	assert(row_count(1, 2)),
	assert(row_count(2, 0)),
	assert(row_count(3, 1)),
	assert(row_count(4, 1)),
	% --- Number of ship segements per column
	assert(col_count(1, 1)),
	assert(col_count(2, 2)),
	assert(col_count(3, 0)),
	assert(col_count(4, 1)),
    % Fleet definition
    assert(fleet(destroyer, 1)),
    assert(fleet(submarine, 2)),
    assert(fleet(cruiser, 0)),
    assert(fleet(battleship, 0)),
	% Ship placements
	assert(cell(1, 2, ship)),
	assert(cell(1, 3, ship)),
	assert(cell(3, 4, ship)),
	assert(cell(4, 1, ship)).

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
	assert(row_count(2, 1)),
	assert(row_count(3, 0)),
	% --- Number of ship segements per column
	assert(col_count(1, 2)),
	assert(col_count(2, 1)),
	assert(col_count(3, 0)),
	% Ship placements
	assert(cell(1, 1, ship)),
	assert(cell(2, 1, ship)),
	assert(cell(1, 2, ship)).

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

% This is the main entry point for fleet validation.
validate_fleet :-
    % Copy ship cells to a temporary workspace to avoid modifying original data.
    forall(cell(R, C, ship), assertz(temp_ship(R, C))),
    % Recursively find and count ships, passing counters as accumulators.
    count_all_ships(0, 0, 0, 0, Counts),
    % Clean up the temporary data.
    retractall(temp_ship(_, _)),
    % Check the final counts against the fleet definition.
    Counts = counts(B, C, D, S),
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

% Recursive predicate to find and count all ships on the board.
% Base Case: No more ship segments left in our temporary workspace.
count_all_ships(B, C, D, S, counts(B, C, D, S)) :- \+ temp_ship(_, _), !.
count_all_ships(B_in, C_in, D_in, S_in, FinalCounts) :-
    temp_ship(R, C),
    determine_direction(R, C, DR, DC),
    ( (DR == fail; DC == fail) ->
        % skip this cell, it's part of a ship already processed
        retract(temp_ship(R, C)),
        count_all_ships(B_in, C_in, D_in, S_in, FinalCounts)
    ;
        (
            (DR =:= 0, DC =:= 0) ->  % Submarine
                Length = 1,
                retract(temp_ship(R, C))
            ;
                measure_length(R, C, DR, DC, Length),
                retract_ship(R, C, DR, DC, Length)
        ),
        update_counts(Length, B_in, C_in, D_in, S_in, B_out, C_out, D_out, S_out),
        count_all_ships(B_out, C_out, D_out, S_out, FinalCounts)
    ).

% Measures the length of a ship starting at (R,C) in direction (DR,DC).
measure_length(R, C, _DR, _DC, 0) :-
    \+ temp_ship(R, C), !. % Base case: If the cell is not a ship, its length is 0.
measure_length(R, C, DR, DC, Length) :-
    temp_ship(R, C), !,     % Condition: This cell must be a ship segment.
    NextR is R + DR,
    NextC is C + DC,
    measure_length(NextR, NextC, DR, DC, SubLength),
    Length is SubLength + 1.

% Retracts all segments of a ship from the temp_ship database.
retract_ship(_, _, _, _, 0) :- !.
retract_ship(R, C, DR, DC, N) :-
    retract(temp_ship(R, C)),
    NextR is R + DR,
    NextC is C + DC,
    N_new is N - 1,
    retract_ship(NextR, NextC, DR, DC, N_new).

% Updates the ship counters based on length.
update_counts(4, B, C, D, S, B_new, C, D, S) :- B_new is B + 1, !.
update_counts(3, B, C, D, S, B, C_new, D, S) :- C_new is C + 1, !.
update_counts(2, B, C, D, S, B, C, D_new, S) :- D_new is D + 1, !.
update_counts(1, B, C, D, S, B, C, D, S_new) :- S_new is S + 1, !.
update_counts(_, B, C, D, S, B, C, D, S). % Should not happen with valid ships.

% Horizontal ship head: has a ship to the right, but not to the left
determine_direction(R, C, 0, 1) :-
    temp_ship(R, C+1),
    \+ temp_ship(R, C-1), !.

% Vertical ship head: has a ship below, but not above
determine_direction(R, C, 1, 0) :-
    temp_ship(R+1, C),
    \+ temp_ship(R-1, C), !.

% Submarine: no ships in any adjacent orthogonal direction
determine_direction(R, C, 0, 0) :-
    \+ temp_ship(R, C+1),
    \+ temp_ship(R, C-1),
    \+ temp_ship(R+1, C),
    \+ temp_ship(R-1, C), !.

% Middle of a ship: skip
determine_direction(_, _, fail, fail).  % fallback clause if direction is ambiguous


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



