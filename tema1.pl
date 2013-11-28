% Tema1 AI

% Observations:
% If a box appears more than once, I assume
% that its Coordonates/Position also appears
% more than once in the Boxes list

% N - number of required boxes
% Boxes/Obstacles - a list of where the boxes/obstacles are
% Result - robot's moves to gather boxes and get them 'home'


                             /************************
                             * COMPILATION & IMPORTS *
                             ************************/

% move from one position to another
:- ensure_loaded(move_to).

% grid defaults
:- ensure_loaded(test/grid).

% list functions
:- use_module(list_functions, [reverse_list/2]).


cleanup :-
	retractall(memory(_, _, _)),
	retractall(queue(_)),
	retractall(visited(_)).

                              /*********************
                              * SOLVING FOR RESULT *
                              *********************/

%% solve(+N, ?Result)
%
%  Result should return robot's moves. I will call
%  home the origin, where the robot needs to gather
%  all the boxes in order to get the bananas
solve(N, Result) :-
	search_home(N, Nnew),
	initialise(Nnew),
	search_rest(Result), !,
	cleanup.
solve(_, _) :-
	cleanup, fail.

%% search_home(+N, ?Nnew)
%
%  Deal with the case when there already are boxes at home
search_home(N, Nnew) :-
	get_home(Home),
	get_type(Home, Type),
	update_home_boxes(N, Nnew, Type).

update_home_boxes(N, N, f) :- !.
update_home_boxes(N, Nnew, Home_boxes) :-
	Nnew is N - Home_boxes.

%% initialise(+N)
%
%  The robot remembers important stuff. So its memory is
%  memory(Nb_of_boxes_left, CrtPos, PathToHome)
%  And we make the memory of the robot global.
%  Also global is the queue with the TOVisit positions and the
%  list of Visited places
initialise(N) :-
	get_home(Home),
	assert(memory(N, Home, [])),
	get_neighbors(Home, Neighbors00),
	filter_neighbors(Neighbors00, [], [], [], Neighbors0),
	fix_neighbors(Neighbors0, Home, [], Neighbors),
	assert(queue(Neighbors)),
	assert(visited([Home])).

%% search_rest(?Moves)
%
%  Find Moves by exploring the area. First, go to
%  a position from the queue, then process that position,
%  looking for boxes and for its neighbours.
search_rest(Moves) :-
	get_Global(memory(N, _, _)), N > 0, !,
	get_Global(queue([(PosNew, PathToHomeNew) | _])),
	move_to_new_position(PosNew, PathToHomeNew, Moves1),
	asserta(memory(N, PosNew, PathToHomeNew)),
	process_position(Moves2),
	search_rest(Moves3),
	append([Moves1, Moves2, Moves3], Moves).
search_rest([]).

%% move_to_new_position(+PosNew, +PathToHomeNew, ?Moves)
%
%  Move to a new position, from queue, moving from the
%  current position.
move_to_new_position(PosNew, PathToHomeNew, Moves) :-
	get_Global(memory(_, Pos, PathToHome)),
	move_to((PosNew, PathToHomeNew), (Pos, PathToHome), Moves1),
	fix_moves(Moves1, Moves, no).

%% fix_moves(+Moves1, ?Moves, +CarryBox)
%
%  Fix the moves to contain moveWithBox(Dir)/move(Dir).
fix_moves([], [], _) :- !.
fix_moves([Dir | Moves1], [moveWithBox(Dir) | Moves], yes) :-
	fix_moves(Moves1, Moves, yes).
fix_moves([Dir | Moves1], [move(Dir) | Moves], no) :-
	fix_moves(Moves1, Moves, no).

%% process_position(?Moves)
%
%  For the position on which the robot currently is,
%  search if there are boxes and find new positions to
%  explore, near the current one.
process_position(Moves) :-
	get_Global(memory(N, Pos, PathToHome)),
	search_for_boxes(Pos, PathToHome, N, Moves),
	explore_and_update_queue,
	update_visited_positions(Pos).

%% search_for_boxes(+Pos, +PathToHome, +N, ?NLeft, +Boxes, ?BoxesLeft, ?Moves)
%
%  Search current position for boxes. Retrieve each box Home and get back to
%  this position, if not gathered enough boxes. Also update the number of
%  boxes needed and the list of boxes.
search_for_boxes(Pos, PathToHome, N, Moves) :-
	get_type(Pos, Type),
	Type \= f, !,
	moves_home_and_back(Pos, PathToHome, MovesHome, MovesBack),
	gather_boxes(N, Type, MovesHome, MovesBack, Moves),
	get_Global(map([Boxes, Obstacles])),
	delete(Boxes, Pos, BoxesNew),
	asserta(map([BoxesNew, Obstacles])).
search_for_boxes(_, _, _, []).

%% moves_home_and_back(+Pos, +Path, +MovesHome, +MovesBack)
%
%  Compute MovesHome and MovesBack once and use them for all the
%  boxes in the current position.
moves_home_and_back(Pos, PathToHome, MovesHome, MovesBack) :-
	%% towards home moves
	get_home(Home),
	move_to( (Home, []), (Pos, PathToHome), Moves ),
	fix_moves(Moves, MovesHome, yes),
	%% get back moves
	reverse_list(Moves, MovesRev),
	get_opposite_list(MovesRev, MovesOpp),
	fix_moves(MovesOpp, MovesBack, no).

get_opposite_list([], []).
get_opposite_list([Dir | Moves1], [OppDir | Moves]) :-
	get_opposite_dir(Dir, OppDir),
	get_opposite_list(Moves1, Moves).

get_opposite_dir(n, s).
get_opposite_dir(s, n).
get_opposite_dir(w, e).
get_opposite_dir(e, w).

%% gather_boxes(+N, +Left, +MovesHome, +MovesBack, ?Moves)
%
%  Gather all boxes from current room. Left is how many boxes
%  are in current position.
gather_boxes(N, Left, MovesHome, MovesBack, Moves) :-
	N > 0, Left > 0, !,
	append([[pickbox], MovesHome, [stackbox]], Moves1),
	N1 is N-1, Left1 is Left-1,
	( N1 = 0  % finished
	-> Moves2 = []
	; Moves2 = MovesBack
	),
	gather_boxes(N1, Left1, MovesHome, MovesBack, Moves3),
	append([Moves1, Moves2, Moves3], Moves).
gather_boxes(N, _, _, _, []) :-
	retract(memory(_, Pos, PathToHome)), !,
	asserta(memory(N, Pos, PathToHome)).

%% explore_and_update_queue
%
%  Search for neighours of a position from queue,
%  filter them and dequeue this position; then 
%  enqueue the new neighbours found.
explore_and_update_queue :-
	get_Global(memory(N, _, _)), N > 0, !,
	get_Global(queue([(PosNew, PathToHomeNew) | List])),
	explore(PosNew, PathToHomeNew, Neighbors),
	append(List, Neighbors, ListNew),
	asserta(queue(ListNew)).
explore_and_update_queue.

%% explore( (Pos, PathToHome), Neighbours )
%
%  Returns a list of neighbors that are not in
%  queue() and are not the last element of PathToHome
explore(Pos, PathToHome, Neighbors) :-
	get_neighbors(Pos, Neighbors0),
	last(PathToHome, Parent),
	get_Global(queue(ToVisit)),
	get_Global(visited(Visited)),
	filter_neighbors(Neighbors0, Parent, ToVisit, Visited, Neighbors1),
	fix_neighbors(Neighbors1, Pos, PathToHome, Neighbors).

%% get_neighbors(+Id, ?Neighbors)
%
%  Returns all neighbors of Id
get_neighbors(Id, Neighbors) :-
	get_neighbors(Id, [], Neighbors).

get_neighbors(Id, Acc, Neighbors) :-
	get_neighbor(Id, NewId, _), 
	\+ memberchk(NewId, Acc), !,
	get_neighbors(Id, [NewId | Acc], Neighbors).
get_neighbors(_, Acc, Acc).

filter_neighbors([], _, _, _, []) :- !.
filter_neighbors([X | Neighbors0], Parent, ToVisit, Visited, [X | Neighbors]) :-
	X \= Parent,
	\+ get_type(X, o),
	\+ memberchk((X,_), ToVisit),
	\+ memberchk(X, Visited), !,
	filter_neighbors(Neighbors0, Parent, ToVisit, Visited, Neighbors).
filter_neighbors([_ | Neighbors0], Parent, ToVisit, Visited, Neighbors) :-
	filter_neighbors(Neighbors0, Parent, ToVisit, Visited, Neighbors).

fix_neighbors([], _, _, []) :- !.
fix_neighbors(Neighbors0, Pos, PathToHome, Neighbors) :-
	append(PathToHome, [Pos], Path),
	fix_neighbors(Neighbors0, Path, Neighbors).
fix_neighbors([], _, []) :- !.
fix_neighbors([X | Old], Path, [(X, Path) | New]) :-
	fix_neighbors(Old, Path, New).

update_visited_positions(IdNew) :-
	retract(visited(Old)), !,
	asserta(visited([IdNew | Old])).
