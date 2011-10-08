:- module(check_moves, [check_moves/5]).

:- ensure_loaded(test_lengths).

%% Test if:
%  - gathered enough the boxes
%  - the robot has an accurate walk (doesn't walk through obstacles)
%  - the robot stacks a box only when being in the Origin/Home
%  - the robot picks up a box from a room where it exists one
%  - the length of result is <= than a Maximum
%  - the robot does any more moves after has gathered enough boxes
%  - the robot moves consitent (ex. moveWithBox(Dir) until has done stackbox)
%  Overall:
%  - fail if at least one has failed
init :-
	assert(gathered_enough_boxes(fail)),
	assert(accurately_walk(ok)),
	assert(accurately_stackbox(ok)),
	assert(accurately_pickbox(ok)),
	assert(length_of_result(fail)),
	assert(moves_after_finished(ok)),
	assert(moves_consistency(ok)),
	assert(overall_status(fail)).

%	Initial_State = state(Home, no),
%	check_moves(Result, Boxes, Initial_State, N, Test_Number).
check_moves(Result, Boxes, Initial_State, N, TestNR) :-
	init,
	( test_result(Result, Boxes, Initial_State, N, TestNR); true ),
	write_summary(OS),
	cleanup,
	check_if_fail(OS).

test_result(Result, Boxes, Initial_State, N, TestNR) :-
	check_max_length(TestNR, Result),
	check_home(N, Nleft, Boxes, Initial_State),
	check_rest(Result, Boxes, Initial_State, Nleft).

check_max_length(TestNR, Result) :-
	test(TestNR, _, MaxLength),
	length(Result, TestLength),
	TestLength =< MaxLength, !,
	asserta(length_of_result(ok)).
check_max_length(_, _).

check_home(N, Nleft, Boxes, state(Home, _)) :-
	findall(Home, member(Home, Boxes), HomeBoxes),
	length(HomeBoxes, Nhome),
	Nleft is N - Nhome,
	Nleft >= 0, !.
check_home(_, 0, _, _).

check_rest([], _, _, N) :-
	N > 0, !,
	fail.
check_rest([], _, state(Home, no), 0) :- 
	get_home(Home), !,
	asserta(gathered_enough_boxes(ok)),
	check_overall.

% move without carying a box
check_rest([move(Dir) | Moves], Boxes, state(Pos, Carry), N) :-
	check_moves_consistency(no, Carry),
	get_neighbor(Pos, PosNew, Dir),
	check_accurately_walk(PosNew), !,
	check_rest(Moves, Boxes, state(PosNew, no), N).
% move with box
check_rest([moveWithBox(Dir) | Moves], Boxes, state(Pos, Carry), N) :-
	check_moves_consistency(yes, Carry),
	get_neighbor(Pos, PosNew, Dir),
	check_accurately_walk(PosNew), !,
	check_rest(Moves, Boxes, state(PosNew, yes), N).
% finds box
check_rest([pickbox | Moves], Boxes, state(Pos, Carry), N) :-
	check_moves_consistency(no, Carry),
	check_pickbox_accuracy(Pos, Boxes), !,
	delete_one(Boxes, Pos, BoxesNew),
	check_rest(Moves, BoxesNew, state(Pos, yes), N).
% put box in Home
check_rest([stackbox | Moves], Boxes, state(Pos, Carry), N) :-
	N > 0, N1 is N-1, 
	get_home(Home), 
	check_stackbox_accuracy(Home, Pos),
	check_moves_consistency(yes, Carry),
	check_moves_after_finished(N1, Moves), !,
	check_rest(Moves, Boxes, state(Pos, no), N1).

check_overall :-
	get_global(gathered_enough_boxes(X1)), X1 = ok,
	get_global(accurately_walk(X2)), X2 = ok,
	get_global(accurately_pickbox(X3)), X3 = ok,
	get_global(accurately_stackbox(X4)), X4 = ok,
	get_global(length_of_result(X5)), X5 = ok,
	get_global(moves_after_finished(X6)), X6 = ok,
	get_global(moves_consistency(X7)), X7 = ok,
	asserta(overall_status(ok)).

get_global(X) :-
	retract(X), !,
	asserta(X).

%% It is considered that there are obstacles
%  where valid_grid fails
check_accurately_walk(Pos) :-
	\+ get_type(Pos, o), !.
check_accurately_walk(_) :-
	asserta(accurately_walk(fail)).

check_pickbox_accuracy(Pos, Boxes) :-
	memberchk(Pos, Boxes), !.
check_pickbox_accuracy(_, _) :-
	asserta(accurately_pickbox(fail)).

check_stackbox_accuracy(Home, Home) :- !.
check_stackbox_accuracy(_, _) :-
	asserta(accurately_stackbox(fail)).

% check if there are more moves then needful,
% as it has gathered enough boxes.
check_moves_after_finished(0, []) :-
	asserta(gathered_enough_boxes(ok)), !.
check_moves_after_finished(0, _) :-
	asserta(moves_after_finished(fail)),
	asserta(gathered_enough_boxes(ok)), !.
check_moves_after_finished(_, _).

check_moves_consistency(Carry, Carry) :- !.
check_moves_consistency(_, _) :-
	asserta(moves_consistency(fail)).

write_summary(OS) :-
	retract(gathered_enough_boxes(GEB)),
	retract(accurately_walk(AW)),
	retract(accurately_pickbox(AP)),
	retract(accurately_stackbox(AS)),
	retract(length_of_result(LOR)),
	retract(moves_after_finished(MAF)),
	retract(moves_consistency(MC)),
	retract(overall_status(OS)), nl,
	write_LOR(LOR), nl,
	write_GEB(GEB), nl,
	write_AW(AW), nl,
	write_AS(AS), nl,
	write_AP(AP), nl,
	write_MAF(MAF), nl,
	write_MC(MC), nl,
	write_overall(OS).

write_LOR(Status) :-
	write_status(Status),
	write('Test pentru lungimea solutiei').

write_GEB(Status) :-
	write_status(Status),
	write('Test pentru numarul de cutii adunate').

write_AW(Status) :-
	write_status(Status),
	write('Test pentru a verifica faptul ca nu intra in obstacole').

write_AS(Status) :-
	write_status(Status),
	write('Test pentru a verifica faptul ca depoziteaza cutiile in origine').

write_AP(Status) :-
	write_status(Status),
	write('Test pentru a verifica faptul ca nu ridica cutii in pozitii inexistente').

write_MAF(Status) :-
	write_status(Status),
	write('Test pentru a verifica daca robotul mai efectueaza miscari dupa ce a strans suficiente cutii').

write_MC(Status) :-
	write_status(Status),
	write('Test pentru a verifica daca miscarile sunt consistente (ex. face numai moveWithBox() pana la miscarea stackbox, sau face stackbox fara sa care vreo cutie, sau pickbox cand cara deja o cutie s.a.)').

write_overall(Status) :-
	format('~`-t~15|~nOverall: ~s', Status).

% ok/fail ...:
write_status(Status) :-
	format('~s~t~4|...: ', Status).

cleanup :-
	retractall(gathered_enough_boxes(_)),
	retractall(accurately_walk(_)),
	retractall(accurately_pickbox(_)),
	retractall(accurately_stackbox(_)),
	retractall(length_of_result(_)),
	retractall(moves_after_finished(_)),
	retractall(moves_consistency(_)),
	retractall(overall_status(_)).

check_if_fail(fail) :- !, fail.
check_if_fail(_).

% delete one element of all its duplicates
delete_one([], _, []) :- !.
delete_one([X | T], X, T) :- !.
delete_one([H | T], X, [H | Rest]) :-
	H \= X,
	delete_one(T, X, Rest).
