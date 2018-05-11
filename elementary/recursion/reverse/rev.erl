-module(rev).
-export([rvrs/1]).

rvrs([]) ->
    [];
rvrs(List) -> 
    rvrs(List, []).
rvrs([], Rvrsd) ->
    Rvrsd;
rvrs([H|T], Rvrsd) ->
    rvrs(T, [H] ++ Rvrsd).
