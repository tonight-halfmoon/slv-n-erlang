-module(perms).
-export([permute/1]).
-import(swap, [swap/1]).

%%% Permutations - implementation with Backtracking and, divide and conquer algorithm

permute([]) ->
    [[]];
permute(L) ->
    [[H|P] || [H|T] <- swap(L), P <- permute(T)].

	       
%%% References:
%%% Then, I found in Erlang STD Lib documentation, the following  line
%%% [[H|T] || H <- L, T <- permute(L -- [H])].
%%% However, it still does not scale for a list greater than 10 elements. Becauase of machine mermory limitation.
