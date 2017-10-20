-module(perms).
-export([permute/1]).
-import(dist, [dist/2]).
-import(swap, [swap/1]).

%%% Permutations - implementation with Backtracking and, divide and conquer algorithm

permute([X,Y]) ->
    [[X,Y],[Y,X]];
permute(L) ->
    [[dist:dist(H, P) || P <- permute(T)] || [H|T] <- swap(L)].
