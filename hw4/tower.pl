% SWI-prolog implementation of transpose
% https://stackoverflow.com/questions/4280986/how-to-transpose-a-matrix-in-prolog
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

% Check if the grid is valid (contains unique numbers for every row and column)
is_length(S, L) :- length(L, S).
is_in_fd_domain(Min, Max, L) :- fd_domain(L, Min, Max).

is_valid_grid(Size, Grid) :-
	length(Grid, Size),
	maplist(is_length(Size), Grid),
	maplist(is_in_fd_domain(1, Size), Grid),
	maplist(fd_all_different, Grid).

% Helpers for accessing the counts data structure
top(counts(T, _, _, _), T).
bot(counts(_, B, _, _), B).
left(counts(_, _, L, _), L).
right(counts(_, _, _, R), R).

% Count the number of visible towers in a list (assume viewing from the left)
count_list(_, 0, []).
count_list(PrevMax, Count, [Head|Tail]) :-
	PrevMax > Head,
	count_list(PrevMax, Count, Tail).
count_list(PrevMax, Count, [Head|Tail]) :-
	PrevMax < Head,
	count_list(Head, Count_new, Tail),
	Count is Count_new + 1. 

% Count the towers in the grid row by row
count_rows([], []).
count_rows([Count_head|Count_tail], [Grid_head|Grid_tail]) :-
	count_list(0, Count_head, Grid_head),
	count_rows(Count_tail, Grid_tail).

% ... And also backwards
count_rows_back([], []).
count_rows_back([Count_head|Count_tail], [Grid_head|Grid_tail]) :-
	reverse(Grid_head, Grid_head_rev),
	count_list(0, Count_head, Grid_head_rev),
	count_rows_back(Count_tail, Grid_tail).

% Check if the grid satisfies the count constraint
count_towers(Counts, Grid, Grid_trans) :-
	top(Counts, Top),
	bot(Counts, Bot),
	left(Counts, Left),
	right(Counts, Right),
	count_rows(Left, Grid),
	count_rows_back(Right, Grid),
	count_rows(Top, Grid_trans),
	count_rows_back(Bot, Grid_trans).
	
% The solver
tower(N, T, C) :-
	is_valid_grid(N, T),
	transpose(T, T_trans),
	is_valid_grid(N, T_trans),
	maplist(fd_labeling, T),
	count_towers(C, T, T_trans).

% The plain solver
% is_in_domain(_, _, []).
% is_in_domain(Min, Max, [Head|Tail]) :-
%	between(Min, Max, Head),
%	is_in_domain(Min, Max, Tail).

are_all_different([]).
are_all_different([Head|Tail]) :-
	\+member(Head, Tail),
	are_all_different(Tail).

is_valid_grid_plain(Size, Grid) :-
	length(Grid, Size),
	maplist(is_length(Size), Grid),
	% maplist(is_in_domain(1, Size), Grid),
	maplist(are_all_different, Grid).

generate_a_row(Size, Out) :-
	findall(Num, between(1, Size, Num), Temp),
	permutation(Temp, Out).

generate_a_grid(Size, Grid) :-
	length(Grid, Size),
	maplist(generate_a_row(Size), Grid).

plain_tower(N, T, C) :-
%  	maplist(findall(Num, between(1, N, Num)), T),
	generate_a_grid(N, T),
    is_valid_grid_plain(N, T),
	transpose(T, T_trans),
	is_valid_grid_plain(N, T_trans),	
	count_towers(C, T, T_trans).

% C = counts([4,2,2,1],[1,2,2,4],[4,2,2,1],[1,2,2,4])
% T = [[1,2,3,4],[2,1,4,3],[3,4,1,2],[4,3,2,1]] ? 

add_1(0, 1).
add_1(T, T1) :-
	T1 is T.

speedup(Time) :-
	statistics(cpu_time, [Begin|_]),
	tower(4, _, counts([4,2,2,1],[1,2,2,4],[4,2,2,1],[1,2,2,4])),
	statistics(cpu_time, [End|_]),
	T1 is End - Begin,
	add_1(T1, T1_1), % To prevent division by 0
	statistics(cpu_time, [Begin_2|_]),
	plain_tower(4, _, counts([4,2,2,1],[1,2,2,4],[4,2,2,1],[1,2,2,4])),
	statistics(cpu_time, [End_2|_]),
	T2 is End_2 - Begin_2,
	!,
	Time is T2 / T1_1.

ambiguous(N, C, T1, T2) :-
	tower(N, T1, C),
	tower(N, T2, C),
	T1 \= T2.
