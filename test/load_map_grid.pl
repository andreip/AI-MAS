%% load_map(+File)
%  Harta se desfasoara in semiaxele xOy pozitive;
%  Axa x este orizontala si creste spre dreapta
%  Axa y este verticala si creste in jos
%  Originea e in coltul stanga sus
%  o - obstacol
%  f - free
%  x - unde x este un numar intre [0,9],
%      inseamnand ca sunt x cutii in acel loc
%
%  Adauga lista List obtinuta ca variabila globala, pentru
%  functia de expandare get_neighbors --> map(List).
%  Unde List = [Boxes, Obstacles, ... ]
%%

init_map_bo :-
	assert(boxes([])),
	assert(obstacles([])).

cleanup_map_bo :-
	retractall(boxes(_)),
	retractall(obstacles(_)).

cleanup_map :-
	retractall(map(_)).

% List is formed of [Boxes, Obstacles .. ]
load_map(File) :-
	init_map_bo,
	open(File, read, Stream),
	read_from_file(Stream, (0,0)), !,
	close(Stream),
	global_list,
	cleanup_map_bo.
load_map(_) :-
	cleanup_map_bo, fail.

read_from_file(Stream, Pos) :-
	\+ at_end_of_stream(Stream), !,
	get0(Stream, Char0),
	atom_chars(Char, [Char0]),
	add_char(Char, Pos),
	update_pos(Char, Pos, NewPos),
	read_from_file(Stream, NewPos).
read_from_file(_, _).

add_char(o, Pos) :- !,
	retract(obstacles(Obstacles)),
	asserta(obstacles([Pos | Obstacles])).
add_char(f, _) :- !.
add_char(X, Pos) :-
	convert_to_number(X, N),
	retract(boxes(Boxes)),
	create_list_n(N, Pos, List),
	append(Boxes, List, BoxesNew),
	asserta(boxes(BoxesNew)).
add_char(_, _).

create_list_n(0, _, []) :- !.
create_list_n(N, Pos, [Pos | List]) :-
	N1 is N-1,
	create_list_n(N1, Pos, List).

update_pos('\n', (_,Y), (0,Y1)) :- Y1 is Y+1, !.
update_pos(_, (X,Y), (X1,Y)) :- X1 is X+1.

global_list :-
	retract(boxes(Boxes)),
	retract(obstacles(Obstacles)),
	[Boxes, Obstacles] = List,
	asserta(map(List)).    % lista globala pt functie expandare

convert_to_number('0', 0) :- !.
convert_to_number('1', 1) :- !.
convert_to_number('2', 2) :- !.
convert_to_number('3', 3) :- !.
convert_to_number('4', 4) :- !.
convert_to_number('5', 5) :- !.
convert_to_number('6', 6) :- !.
convert_to_number('7', 7) :- !.
convert_to_number('8', 8) :- !.
convert_to_number('9', 9).
