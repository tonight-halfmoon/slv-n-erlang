-module(common_patterns_fun_expressions).
-export([doubleAll/1, revAll/1]).
-export([map/2]).
-export([evens/1, palins/1]).
-export([filter/2]).
-export([foreach/2]).

doubleAll(Xs) ->
    map(fun(X) ->
		X * 2 end, Xs).
revAll([]) ->
    [];
revAll([X|Xs]) ->
    [reverse(X)|revAll(Xs)].

reverse([]) ->
    [];
reverse([H|T]) ->
    reverse(T) ++ [H].

%% All that differs in the two examples is the transformation affecting the element X, i.e.m 'X*2' and reverse(X).

map(_F, []) ->
    [];
map(F, [X|Xs]) ->
    [F(X) | map(F, Xs)].

evens([]) ->
    [];
evens([X|Xs]) when is_list(X) ->
    [filter(fun(Y) -> Y rem 2 == 0 end, X)|evens(Xs)].

palins(Xs) ->
    filter(fun(X) -> X == reverse(X) end, Xs).

filter(_P, []) ->
    [];
filter(P, [X|Xs]) ->
    case P(X) of
	true ->
	    [X| filter(P, Xs)];
	_ ->
	    filter(P, Xs)
    end.

foreach(_F, []) ->
    ok;
foreach(F, [X|Xs]) ->
    F(X),
    foreach(F, Xs).

%% Fun Expressions
%% A fun expression does the same as functions without giving the function a name.

%% fun(X) -> X * 2 end

%% fun(X,Y) -> X+Y end

%% A function to give the head of a list (and null if it is empty) is given by:
%% fun([]) -> null;
%%    ([X|_]) -> X end
