-module(qs).
-export([qs/1]).

qs([]) ->
    [];
qs([H|T]) -> 
    qs(T) ++ [ H ] ++ qs(T).
