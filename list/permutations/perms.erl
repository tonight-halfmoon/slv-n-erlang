-module(perms).
-export([permute/1]).

%%% Permutations with Backtracking with divide and conquer algorithm implementation

permute([X,Y]) ->
    [[X,Y], [Y,X]];
permute(L) ->
    SwpdLists = swap(L),   
    [[[HS|Prmtd] || Prmtd <- permute(SL)] || [HS|SL] <- SwpdLists].
 
swap(L) ->
    swap(L, length(L), 1, []).

swap([], _, _, SwpdLists) ->
    SwpdLists;
swap(L, J, J, SwpdLists) ->
    [L|SwpdLists];
swap(L, M, J, SwapdLists) ->
    ElemJ = lists:nth(J, L), 
    ElemM = lists:nth(M, L),
    SwpdJ = lists:append([[ElemM], lists:delete(ElemM, lists:delete(ElemJ, L)), [ElemJ]]),
    swap(L, M -1, J, [SwpdJ|SwapdLists]).
