-module(sublist).
-export([sublist/2]).

sublist([],_) ->
    [];
sublist(_,0) ->
    [];
sublist([H|T],N) when N > 0 -> [H|sublist(T, N-1)].

