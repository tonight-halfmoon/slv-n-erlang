-module(avg).
-export([cmp/1]).


cmp([]) ->
    0;
cmp(L) ->
    lists:sum(L) / length(L).
