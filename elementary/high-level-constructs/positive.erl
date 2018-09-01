-module(positive).

-export([positive/1]).

-define(Positive, fun(X) when X >=0 -> true; (X) when X < 0 -> false end).

positive(L) ->
    lists:filter(?Positive, L).
