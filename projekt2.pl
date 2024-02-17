/* 
 [Programowanie w logice]
 Projekt nr 2
 Autor: Anna Bielawska
*/

% Ad. 1)
fill_with_char(_, 0, []) :- !.
fill_with_char(Char, N, [Char|T]) :-
    N > 0,
    N1 is N - 1,
    fill_with_char(Char, N1, T).

canvas(_, 0, [[]]). % invalid, no width
canvas(0, _, [[]]). % invalid, no height
canvas(W, H, S) :-
    H > 0, W > 0,
    fill_with_char(' ', W, L),
    fill_with_char(L, H, S).
  

% Ad. 2)
change_character([_H|T], 0, Char, [Char|T]) :- !.
change_character([H|T], K, Char, [H|L]) :- 
    K > 0,
    K1 is K-1,
    change_character(T, K1, Char, L).
    
point([H|T], [X, 0], Char, [L|T]) :-
    change_character(H, X, Char, L), !.

point([H|T], [X, Y], Char, [H|L]) :-
    Y > 0,
    Y1 is Y-1,
    point(T, [X, Y1], Char, L).


% Ad. 3)
line(S, [K, N], [K, N], Char, L) :-
    point(S, [K, N], Char, L), !.
    
line([H|T], [N1, 0], [N2, 0], Char, S) :-
    % from left to right
    N2 > N1,
    change_character(H, N2, Char, L),
    NewN2 is N2 - 1,
    line([L|T], [N1, 0], [NewN2, 0], Char, S);
    % from right to left 
    N1 > N2,
    change_character(H, N1, Char, L),
    NewN1 is N1 - 1,
    line([L|T], [NewN1, 0], [N2, 0], Char, S).

line(S, [N, K1], [N, K2], Char, Out) :-
    % from top to the bottom
    K2 > K1,
    point(S, [N, K2], Char, L),
    K is K2-1,
    line(L, [N, K1], [N, K], Char, Out);
    % from bottom to top
    K2 < K1,
    point(S, [N, K1], Char, L),
    K is K1-1,
    line(L, [N, K], [N, K2], Char, Out).

line([H|T], [N1, K], [N2, K], Char, [H|S]) :-
    K > 0,
    K1 is K-1,
    line(T, [N1, K1], [N2, K1], Char, S).
    
line(S, [N1, K1], [N2, K2], Char, Out) :-
    % from the top right corner to the left bottom corner
    K2 < K1,
    N2 > N1,
    point(S, [N2, K2], Char, L),
    N is N2-1,
    K is K2+1,
    line(L, [N1, K1], [N, K], Char, Out);
    % from the bottom left corner to the top right corner
    K2 > K1,
    N2 < N1,
    point(S, [N2, K2], Char, L),
    N is N2-1,
    K is K2+1,
    line(L, [N1, K1], [N, K], Char, Out).

line(S, [N1, K1], [N2, K2], Char, Out) :-
    % from the top left corner to the right bottom corner
    K2 > K1,
    N2 > N1,
    point(S, [N2, K2], Char, L),
    N is N2-1,
    K is K2-1,
    line(L, [N1, K1], [N, K], Char, Out);
    % from the bottom right corner to the top left corner
    K2 < K1,
    N2 < N1,
    point(S, [N1, K1], Char, L),
    N is N1-1,
    K is K1-1,
    line(L, [N, K], [N2, K2], Char, Out).


% Ad. 4)
poly(S, [H], _, S) :- not(atomic(H)), !.
poly(S, [H1, H2|T], Char, L) :-
    line(S, H1, H2, Char, S1),
    poly(S1, [H2|T], Char, L).


% Ad. 5)
clean(S1, X1, Y, X2, Y, S2) :- 
    line(S1, [X1, Y], [X2, Y], ' ', S2).

clean(S1, X1, Y1, X2, Y2, S2) :-
    % upper corner to lower corner
    Y2 > Y1,
    line(S1, [X1,Y2], [X2, Y2], ' ', Out),
    Y is Y2 - 1,
    clean(Out, X1, Y1, X2, Y, S2);
    % lower corner to upper corner
    Y2 < Y1,
    line(S1, [X1,Y1], [X2, Y1], ' ', Out),
    Y is Y1 - 1,
    clean(Out, X1, Y, X2, Y2, S2).
    

% Ad. 6)
change_sublist(S, _, [], S) :- !.
change_sublist([_H|T], 0, [Char|Chars], [Char|S2]) :-
    change_sublist(T, 0, Chars, S2), !.

change_sublist([H|T], X, Chars, [H|S2]) :-
    X > 0,
    X1 is X-1,
    change_sublist(T, X1, Chars, S2).

copy(S, X, Y, S1, S2) :- copy(S, X, Y, S1, [], S2).
copy([H|T], X, 0, [H1|T1], Acc, S2) :-
    not(atomic(H1)),
    change_sublist(H, X, H1, L),
    append(Acc, [L], NewAcc),
    copy(T, X, 0, T1, NewAcc, S2).
    
copy([H|T], X, 0, [], Acc, S2) :-
    nth0(0, [H|T], L),
    append(Acc, [L], NewAcc),
    copy(T, X, 0, [], NewAcc, S2).

copy([H|T], X, Y, S1, Acc, S2) :-
    Y > 0,
    nth0(0, [H|T], L),
    append(Acc, [L], NewAcc),
    Y1 is Y-1,
    copy(T, X, Y1, S1, NewAcc, S2).
    
copy([], _, _, [], Acc, Acc).
