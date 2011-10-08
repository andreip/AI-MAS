get_Global(X) :-
	retract(X), !,
	asserta(X).

%% get_home(?Home)
%
%  Get the place where to start from.
%  For a grid in 2D, it's [(0,0), f], where
%  f stands for free.
get_home(Home) :-
	Home = (0,0).

%% get_neighbor(+Pos, NextPos, Dir)
%
%  Returns (given Pos):
%   - a neighbor, having the direction
%   - a direction, having the neighbor
get_neighbor((X,Y), (X1,Y), e) :- X1 is X+1.
get_neighbor((X,Y), (X1,Y), w) :- X1 is X-1.
get_neighbor((X,Y), (X,Y1), n) :- Y1 is Y+1.
get_neighbor((X,Y), (X,Y1), s) :- Y1 is Y-1.

%% get_type(+Id, ?Type)
%
%  Returns the Type of an Id
%  - x (a number) says how many boxes there are in Id
%      i.e. Type=3 means there are three boxes in Id
%  - o stands for obstacle
%  - f stands for free
get_type(Id, Type) :-
	get_Global(map([Boxes, Obstacles])),
	get_type(Id, Type, Boxes, Obstacles).

% obstacle
get_type((X,Y), o, _, _) :-
	not_in_valid_area(X,Y), !.
get_type(Id, o, _, Obstacles) :-
	memberchk(Id, Obstacles), !.
% box
get_type(Id, Type, Boxes, _) :-
	memberchk(Id, Boxes), !,
	findall(Id, member(Id, Boxes), IdBoxes),
	length(IdBoxes, Type0),
	number(Type0),
	Type = Type0.
% then, it's just a free id
get_type(_, f, _, _).

not_in_valid_area(X, _) :- X < 0, !.
not_in_valid_area(_, Y) :- Y < 0.
