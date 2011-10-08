% tema1 - unit testing

:- ensure_loaded(check_moves).

init(State, Boxes) :-
	get_home(Home),
	State = state(Home, no),             % initial state
	get_Global(map([Boxes, _])).

:- begin_tests(robot).

%# global 30

% test 01
%# timeout 2
%# punctaj 10
test('(01) Dumb',
     [ nondet,
       setup(load_map('test/harti/01_dummy.txt')),
       cleanup(cleanup_map)
	 ]) :-
	N = 1,
	init(State, Boxes),
	solve(N, Result),
	check_moves(Result, Boxes, State, N, 1).

% test 02
%# timeout 2
%# punctaj 10
test('(02) Dumber :)',
     [ nondet,
       setup(load_map('test/harti/01_dummy.txt')),
       cleanup(cleanup_map)
	 ]) :-
	N = 0,
	init(State, Boxes),
	solve(N, Result),
	check_moves(Result, Boxes, State, N, 2).

% test 03
%# timeout 2
%# punctaj 10
test('(03) Dumb walk',
     [ nondet,
       setup(load_map('test/harti/01_dummy.txt')),
       cleanup(cleanup_map)
	 ]) :-
	N = 2,
	init(State, Boxes),
	solve(N, Result),
	check_moves(Result, Boxes, State, N, 3).

% test 04
%# timeout 2
%# punctaj 10
test('(04) More origin boxes',
     [ nondet,
       setup(load_map('test/harti/02_origin_boxes.txt')),
       cleanup(cleanup_map)
	 ]) :-
	N = 3,
	init(State, Boxes),
	solve(N, Result),
	check_moves(Result, Boxes, State, N, 4).

% test 05
%# timeout 2
%# punctaj 10
test('(05) Dumb - box in origin',
     [ nondet,
       setup(load_map('test/harti/02_origin_boxes.txt')),
       cleanup(cleanup_map)
	 ]) :-
	N = 1,
	init(State, Boxes),
	solve(N, Result),
	check_moves(Result, Boxes, State, N, 1).

% test 06
%# timeout 2
%# punctaj 10
test('(06) One obstacle',
     [ nondet,
       setup(load_map('test/harti/03_simple_obstacle.txt')),
       cleanup(cleanup_map)
	 ]) :-
	N = 2,
	init(State, Boxes),
	solve(N, Result),
	check_moves(Result, Boxes, State, N, 6).

% test 07
%# timeout 2
%# punctaj 10
test('(07) Two obstacles',
     [ nondet,
       setup(load_map('test/harti/04_two_obstacles.txt')),
       cleanup(cleanup_map)
	 ]) :-
	N = 2,
	init(State, Boxes),
	solve(N, Result),
	check_moves(Result, Boxes, State, N, 7).

% test 08
%# timeout 3
%# punctaj 10
test('(08) Three obstacles',
     [ nondet,
       setup(load_map('test/harti/05_three_obstacles.txt')),
       cleanup(cleanup_map)
	 ]) :-
	N = 1,
	init(State, Boxes),
	solve(N, Result),
	check_moves(Result, Boxes, State, N, 8).

% test 09
%# timeout 5
%# punctaj 10
test('(09) Three obstacles bit harder',
     [ nondet,
       setup(load_map('test/harti/05_three_obstacles.txt')),
       cleanup(cleanup_map)
	 ]) :-
	N = 2,
	init(State, Boxes),
	solve(N, Result),
	check_moves(Result, Boxes, State, N, 9).

% test 10
%# timeout 10
%# punctaj 10
test('(10) "Escape" from corner',
     [ nondet,
       setup(load_map('test/harti/06_corner_hard.txt')),
       cleanup(cleanup_map)
	 ]) :-
	N = 3,
	init(State, Boxes),
	solve(N, Result),
	check_moves(Result, Boxes, State, N, 10).

% test 11
%# timeout 30
%# punctaj 10
test('(11) Medium sized Labyrinth',
     [ nondet,
       setup(load_map('test/harti/07_medium_sized_labyrinth.txt')),
       cleanup(cleanup_map)
	 ]) :-
	N = 10,
	init(State, Boxes),
	solve(N, Result),
	check_moves(Result, Boxes, State, N, 11).

- end_tests(robot).
