%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
Projekt nr 1 - Labirynt
Autor: Anna Bielawska
*/

/* Zadanie 1. */

generate_possible_moves([], []).
generate_possible_moves([A,B], [X,Y]) :- 
    X = A, (Y is B+1 ; Y is B-1);
    Y = B, (X is A+1 ; X is A-1). 

get_n_element([X|_T], 1, X).
get_n_element([_H|T], N, X) :- N > 1, N1 is N-1, get_n_element(T, N1, X).

element_not_in_list(_, []).
element_not_in_list(El, [H|T]) :-
    H \== El, element_not_in_list(El, T).

append([], [H|T], [[H|T]]).
append([X|L1], L2, [X|L3]) :- append(L1,L2,L3).

path(Labyrinth, Start, End, Path) :- path(Labyrinth, Start, End, Path, []).
path(Labyrinth, [S,StartIdx], [E,EndIdx], Path, Acc) :-
    get_n_element(Labyrinth, S, LabyrinthRow),  % get the starting row
    get_n_element(LabyrinthRow, StartIdx, o), % check the starting position
    append(Acc, [S,StartIdx], Acc1),  % append the path
    generate_possible_moves([S,StartIdx], [N,NextIdx]),  % generate the next move
    element_not_in_list([N,NextIdx], Acc1),  % check if we're not going back 
    path(Labyrinth, [N,NextIdx], [E,EndIdx], Path, Acc1).

path(Labyrinth, [End,EndIdx], [End,EndIdx], Path, Acc) :- 
    get_n_element(Labyrinth, End, LabyrinthRow),  % get the row
    get_n_element(LabyrinthRow, EndIdx, o),  % check the end position
    append(Acc, [End,EndIdx], Path).  % append the path with the end position
                                       
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* Zadanie 2. */

writelist([]). 
writelist(H) :- 
    (H == '.' ; H == x), write(H);
    H == o, tab(2).
writelist([H|T]) :- writelist(H), tab(2), writelist(T).

display_lab([]).
display_lab([H|T]) :-
    is_list(H), writelist(H), nl, display_lab(T), fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* Zadanie 3. */

tail([X], X).
tail([_H|T], X) :- tail(T, X).

head([X], X).
head([H|_T], H).

replace([_|T], 1, X, [X|T]).
replace([H|T], N, X, [H|R]) :- N > 1, N1 is N-1, replace(T, N1, X, R).

find_and_replace(L, _, [], L).
find_and_replace(L, Idx, [H|T], R1) :-
    head(H, X), X == Idx, tail(H, N), 
    replace(L, N, ., R), find_and_replace(R, Idx, T, R1);
    head(H, X), X \== Idx,
    find_and_replace(L, Idx, T, R1).

display_lab([H|T], Path, Idx) :-
    is_list(H), find_and_replace(H, Idx, Path, R),
    writelist(R), nl, IdxNext is Idx + 1, display_lab(T, Path, IdxNext), fail.
display_lab([], _, _).

display_lab([], _).
display_lab(Labyrinth, Path) :- 
    head(Path, Start),  % get the start point coords
    tail(Path, End),  % get the end point coords
    path(Labyrinth, Start, End, Path),  % check if this path is valid
    display_lab(Labyrinth, Path, 1).  % if so, start drawing the Labyrinth from the top
