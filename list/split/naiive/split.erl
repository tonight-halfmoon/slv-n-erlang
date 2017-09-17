-module(split).
-export([split/1]).

split([]) ->
    {[], []};
split([Sol|[]]) ->
    {[Sol], []};
split([H|T]) ->
    split(T, [H]).

split(L, R) when length(L) =:= length(R);
		 length(L) =:= length(R) + 1 ->
    {R, L};
split([H|T], R) ->
    split(T, [H|R]).

