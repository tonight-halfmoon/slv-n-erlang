-module(map).
-export([map/2, incr/1, decr/1]).

map(_, []) ->
    [];
map(F, [H|T]) ->
    [F(H)|map(F,T)].

incr(X) ->
     X + 1.
decr(X) ->
    X - 1.
