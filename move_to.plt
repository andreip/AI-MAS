% Tests for move.pl

:- begin_tests(move_to).
:- use_module(move_to).

test(1, [ true(Moves == [w, w, w, n, n]) ]) :-
	New = ( (0,3), [(0,0), (0,1), (0,2)] ),
	Old = ( (3,1), [(0,0), (0,1), (1,1), (2,1)] ),
	move_to(New, Old, Moves).

test(2, [ true(Moves == [n, n]) ]) :-
	New = ( (0,3), [(0,0), (0,1), (0,2)] ),
	Old = ( (0,1), [(0,0)] ),
	move_to(New, Old, Moves).

test(3, [ true(Moves == [s, s]) ]) :-
	New = ( (0,1), [(0,0)] ),
	Old = ( (0,3), [(0,0), (0,1), (0,2)] ),
	move_to(New, Old, Moves).

test(4, [ true(Moves == [s, s]) ]) :-
	New = ( (0,0), [] ),
	Old = ( (0,2), [(0,0), (0,1)] ),
	move_to(New, Old, Moves).

test(5, [ true(Moves == [n, n]) ]) :-
	New = ( (0,2), [(0,0), (0,1)] ),
	Old = ( (0,0), [] ),
	move_to(New, Old, Moves).

test(6, [ true(Moves == [n, w, s, w, w, w, s]) ]) :-
	New = ( (0,0), [] ),
	Old = ( (4,1), [(0,0), (0,1), (1,1), (2,1), (3,1), (3,2), (4,2)] ),
	move_to(New, Old, Moves).

:- end_tests(move_to).
