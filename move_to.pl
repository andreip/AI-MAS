:- module(move_to, [move_to/3]).

%% Imports
%
:- use_module(list_functions, [reverse_list/2]).

%% move_to(New, Old)
%
%  Move to a new position, from an old one,
%  having a path to home for each position
%  Return : a list of moves [ n, n, e, e, s, .. ] for grid
move_to(New, Old, Moves) :-
	New = (PosNew, PathToHomeNew),
	Old = (PosOld, PathToHomeOld),
	( PosNew == PosOld
		-> Moves = []
	;
		append(PathToHomeNew, [PosNew], PathNew),
		append(PathToHomeOld, [PosOld], PathOld),
		common_ancestor(PathNew, PathOld, Ancestor),
		move_to_ancestor(Old, Ancestor, MovesToAncestor),
		move_from_ancestor(New, Ancestor, MovesFromAncestor),
		append(MovesToAncestor, MovesFromAncestor, Moves)
	).
	
%% common_ancestor(+PathToHomeNew, +PathToHomeOld, ?Ancestor)
%
%  Find the common ancestor of two paths to home
common_ancestor([], [], []) :- !.
common_ancestor([Elem | _], [Elem], Elem) :- !.
common_ancestor([Elem], [Elem | _], Elem) :- !.
common_ancestor([X, Y | PathNew], [X, Y | PathOld], Ancestor) :- !,
	common_ancestor([Y | PathNew], [Y | PathOld], Ancestor).
common_ancestor([X, Y | _], [X, Z | _], X) :-
	Y \= Z.

%% move_to_ancestor(+Old, +Ancestor, ?Moves)
%
%  Move to a common ancestor of the two positions
move_to_ancestor(Old, Ancestor, Moves) :-
	Old = (Pos, PathToHome),
	reverse_list(PathToHome, PathRev),
	get_moves(Pos, PathRev, Ancestor, Moves).

%% move_from_ancestor(+New, +Ancestor, ?Moves)
%
%  Move from ancestor to the new position
move_from_ancestor(New, Ancestor, Moves) :-
	New = (Pos, PathToHome),
	append(PathToHome, [Pos], Path),
	truncate_path(Path, OkPath, Ancestor),
	get_moves(Ancestor, OkPath, Pos, Moves).

% Truncate the path to contain only path from ancestor
truncate_path([Element | Path], OkPath, Ancestor) :-
	Element \= Ancestor, !,
	truncate_path(Path, OkPath, Ancestor).
truncate_path([Ancestor | OkPath], OkPath, Ancestor).

get_moves(Stop, _, Stop, []) :- !.
get_moves(Pos, [NextPos | Path], Stop, [Move | Moves]) :-
	get_neighbor(Pos, NextPos, Move), !,
	get_moves(NextPos, Path, Stop, Moves).
