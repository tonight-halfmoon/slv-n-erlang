-module(evenlists).
-export([even/1]).


%% even([]) ->
%%     [];
%% even([H|T]) when H rem 2 == 0 -> [H|even(T)];
%% even([_|T]) -> even(T). 


even(L) ->
    even(L, []).

even([], R) ->
    R;
even([H|T], R) when H rem 2 == 0 ->
    even(T, [H|R]);
even([_|T], R) ->
    even(T, R).
