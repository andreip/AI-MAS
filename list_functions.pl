% List functions
:- module(list_functions, [reverse_list/2,
                           delete_one/3,
                           delete_last/2]).

% Reverse a list
reverse_list([], []) :- !.
reverse_list(List, RevList) :-
	reverse_list(List, RevList, []).

reverse_list([], RevList, RevList) :- !.
reverse_list([Element | List], RevList, Acc) :-
	reverse_list(List, RevList, [Element | Acc]).

% delete one element of all its duplicates
delete_one([], _, []) :- !.
delete_one([X | T], X, T) :- !.
delete_one([H | T], X, [H | Rest]) :-
	H \= X,
	delete_one(T, X, Rest).

% deletes last element of a list
delete_last([], []) :- !.
delete_last([_], []) :- !.
delete_last([X, Y | Tail], [X | List]) :-
	delete_last([Y | Tail], List).
