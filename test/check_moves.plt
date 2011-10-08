:- begin_tests(check_moves).
:- ensure_loaded([check_moves]).

test(correct, nondet) :-
	N = 2,
	Boxes = [(0,0), (2,0)],
	Obstacles = [],
	Result = [move(n), move(s), move(e), move(e), move(e), move(w),
	          pickbox, moveWithBox(w), moveWithBox(w), stackbox, pickbox,
	          stackbox],
	State = state((0,0), no),
	check_moves(Result, Boxes, Obstacles, State, N, 2).

test('test(gather, walk)', nondet) :-
	N = 1,
	Boxes = [(0,0), (2,0)],
	Obstacles = [],
	Result = [move(n), move(s), move(e), move(e), move(e), move(w),
	          pickbox, moveWithBox(w), moveWithBox(w), stackbox,
	          stackbox],
	State = state((0,0), no),
	check_moves(Result, Boxes, Obstacles, State, N, 1).

test('obstacole', nondet) :-
	N = 2,
	Boxes = [(0,0), (2,0)],
	Obstacles = [(1,0),(1,1),(2,1)],
	Result = [pickbox, move(n), move(n), move(n), move(e), move(s),
	          move(e), move(n), move(e), move(s),
	          move(s), move(s), move(w), pickbox, moveWithBox(e),
	          moveWithBox(n), moveWithBox(n), moveWithBox(n),  % (w) to correct the last one
	          moveWithBox(w), moveWithBox(w), moveWithBox(s),
	          moveWithBox(s), stackbox],
	State = state((0,0), no),
	check_moves(Result, Boxes, Obstacles, State, N, 3).
