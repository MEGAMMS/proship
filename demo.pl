% =====================
% Battleships Puzzle Solver
% =====================

% Dynamic Predicates
:- dynamic cell/3.
:- dynamic row_count/2.
:- dynamic col_count/2.
:- dynamic grid_size/2.

% --- Ship Types (optional, used in more advanced solving)
ship_type(submarine, 1).
ship_type(destroyer, 2).
ship_type(cruiser, 3).
ship_type(battleship, 4).

% --- Clear all facts (to reinitialize the puzzle)
clear :-
    retractall(cell(_, _, _)),
    retractall(row_count(_, _)),
    retractall(col_count(_, _)),
    retractall(grid_size(_, _)).

% --- sample game
load_game(1) :-
    clear,
    assert(grid_size(3, 3)),
    assert(row_count(1, 2)),
    assert(row_count(2, 0)),
    assert(row_count(3, 0)),
    assert(col_count(1, 1)),
    assert(col_count(2, 1)),
    assert(col_count(3, 0)),
    assert(cell(1, 1, ship)),
    assert(cell(1, 2, ship)).

% --- sample game 2
load_game(2) :-
    clear,
    assert(grid_size(4, 4)),
    assert(row_count(1, 2)),
    assert(row_count(2, 0)),
    assert(row_count(3, 1)),
    assert(row_count(4, 1)),
    assert(col_count(1, 1)),
    assert(col_count(2, 2)),
    assert(col_count(3, 0)),
    assert(col_count(4, 1)),
    assert(cell(1, 2, ship)),
    assert(cell(1, 3, ship)),
    assert(cell(3, 4, ship)),
    assert(cell(4, 1, ship)).

% --- Initialize the puzzle with facts
initialize(Game) :-
    clear,
    load_game(Game),
    print_board.

% --- Print the current board state
print_board :-
    grid_size(Rows, Cols),
    write('  '),
    print_col_headers(1, Cols), nl,
    print_rows_with_counts(1, Rows, Cols),
    write('  '),
    print_border(1,Cols),nl,
    write('  '),
    print_col_counts(1, Cols), nl.

% --- Print Columns' headers
print_col_headers(Col, MaxCol) :-
    Col > MaxCol, !.
print_col_headers(Col, MaxCol) :-
    write(' '), write(Col), write(' '),
    Next is Col + 1,
    print_col_headers(Next, MaxCol).

% --- Print Rows with count
print_rows_with_counts(Row, MaxRow, _) :-
    Row > MaxRow, !.
print_rows_with_counts(Row, MaxRow, Cols) :-
    write(Row), write(' '),
    print_columns(Row, 1, Cols),
    write(' | '),
    row_count(Row, Count), write(Count), nl,
    Next is Row + 1,
    print_rows_with_counts(Next, MaxRow, Cols).

% --- Print Columns' count
print_col_counts(Col, MaxCol) :-
    Col > MaxCol, !.
print_col_counts(Col, MaxCol) :-
    col_count(Col, Count),
    write(' '), write(Count), write(' '),
    Next is Col + 1,
    print_col_counts(Next, MaxCol).

% --- Lower border
print_border(Col, MaxCol) :-
    Col > MaxCol, !.
print_border(Col, MaxCol) :-
    write(' '), write('-'), write(' '),
    Next is Col + 1,
    print_border(Next, MaxCol).

% --- Print Rows
print_rows(Row, MaxRow, _) :-
    Row > MaxRow, !.
print_rows(Row, MaxRow, Cols) :-
    print_columns(Row, 1, Cols),
    nl,
    NextRow is Row + 1,
    print_rows(NextRow, MaxRow, Cols).

% --- Print Columns
print_columns(_, Col, MaxCol) :-
    Col > MaxCol, !.
print_columns(Row, Col, MaxCol) :-
    print_cell(Row, Col),
    NextCol is Col + 1,
    print_columns(Row, NextCol, MaxCol).

% --- Print Cell
print_cell(Row, Col) :-
	cell(Row, Col, ship),
	ansi_format([fg(cyan)], ' S ', []), !.
print_cell(Row, Col) :-
	cell(Row, Col, water),
	ansi_format([fg(blue)], ' ~ ', []), !.
print_cell(_, _) :-
	write(' . ').


% Base case: passed the last column
h_count_ships_in_row(_Row, Col, MaxCol, Acc, Acc) :-
    Col > MaxCol, !.
% Case 1: cell at (Row, Col) is ship
h_count_ships_in_row(Row, Col, MaxCol, Acc, Total) :-
    Col =< MaxCol,
    cell(Row, Col, ship), !,
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
    Current > MaxRow, !.
validate_rows(Current, MaxRow) :-
    row_count(Current, Expected),
    count_ships_in_row(Current, Actual),
    (Actual =:= Expected ->
        true;
        format('WRONG : Row ~w has ~w ship segments, expected ~w.~n', [Current, Actual, Expected]), fail),
    Next is Current + 1,
    validate_rows(Next, MaxRow).

% Base case: passed the last row
h_count_ships_in_col(_Col, Row, MaxRow, Acc, Acc) :-
    Row > MaxRow, !.
% Case 1: cell at (Row, Col) is ship
h_count_ships_in_col(Col, Row, MaxRow, Acc, Total) :-
    Row =< MaxRow,
    cell(Row, Col, ship), !,
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
    Current > MaxCol, !.
validate_cols(Current, MaxCol) :-
    col_count(Current, Expected),
    count_ships_in_col(Current, Actual),
    (Actual =:= Expected ->
        true;
        format('WRONG : Column ~w has ~w ship segments, expected ~w.~n', [Current, Actual, Expected]), fail),
    Next is Current + 1,
    validate_cols(Next, MaxCol).

% --- joins both the validate predicates
validate_rows_and_cols :-
    grid_size(MaxRow, MaxCol),
    validate_rows(1, MaxRow),
    validate_cols(1, MaxCol).

% --- defines the eight possible relative positions for an adjacent cell.
adjacent_delta(-1, -1). % diagonally
adjacent_delta(-1,  0). % horizontally
adjacent_delta(-1,  1). % diagonally
adjacent_delta( 0, -1). % vertically
adjacent_delta( 0,  1). % vertically
adjacent_delta( 1, -1). % diagonally
adjacent_delta( 1,  0). % horizontally
adjacent_delta( 1,  1). % diagonally

% ---  finds a valid adjacent cell (R2, C2) for a given cell (Row, Col)
adjacent_cell(Row, Col, AdjRow, AdjCol) :-
    adjacent_delta(DR, DC),
    AdjRow is Row + DR,
    AdjCol is Col + DC,
    grid_size(MaxRow, MaxCol),
    AdjRow >= 1, AdjRow =< MaxRow,
    AdjCol >= 1, AdjCol =< MaxCol.

% ---  succeeds if any adjacent cell contains a ship.
adjacent_ship_exists(Row, Col) :-
    adjacent_cell(Row, Col, R2, C2),
    cell(R2, C2, ship).

% --- succeeds if it can find any ship on the board that has another ship adjacent to it.
invalid_ship_placement :-
    cell(R, C, ship),
    adjacent_ship_exists(R, C).

% --- uses negation on the previous predicate
check_no_adjacent_ships :-
    \+ invalid_ship_placement.

% --- combines validation predicates
validate_board :-
    validate_rows_and_cols,
    check_no_adjacent_ships.


% --- check the over all solution
solve :-
    validate_board,
    write('Board is valid!'), nl.
solve :-
    write('Board is invalid.'), nl.
