-module(sublist2).
-export([sblst/2]).

sblst(List, N) ->
    sblst(List, N, []).
    % rvrs(sblst(List, N, [])).
sblst([], N, Out)->
    Out;
sblst(List, 0, Out) ->
    Out;
sblst([H|T], N, Out) ->
    sblst (T, N -1 , Out ++ [H]).% [Nex|Blt]). % [Nex] ++ Blt).

rvrs(List) ->
    rvrs(List, []).

rvrs([], Rvrsd) ->
    Rvrsd;
rvrs([H|T], Rvrsd) ->
    rvrs(T, [H]++Rvrsd).
