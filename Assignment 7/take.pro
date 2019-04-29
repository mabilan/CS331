% take.pro
% Michael Bilan
% 28 Apr 2019
% CS 331 Assignment 7

% take(+n, +x, ?e)
% Given positive integer n, list x, e is the first n values of x or
% all of x if n is greater than length of x
%take(N, X, E):- length(E, N), append(E, _, X).
take(0, _, []).
take(_,[],[]).
take(N,[A|X],[A|Y]) :- N > 0, S is N - 1, take(S, X, Y).