-module(min).
-export([min/1]).

min([H|T]) ->
    min(T, H).

min([], Min) ->
    Min;
min([H|T], Min) when H =< Min -> min(T, H);
min([H|T], Min) when H> Min ->  min(T, Min).
