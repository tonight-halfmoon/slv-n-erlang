-module(avg2).
-export([cmp/1]).


cmp(L) ->
    cmp(L, 0, 0).

cmp([], S, C) when C == 0 ->
    S;
cmp([], S, C) ->
    S/C;
cmp([H|T], S, C) ->
    cmp(T, S + H, C + 1).
