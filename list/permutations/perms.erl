-module(perms).
-export([permute/1, swap/1]).

%%% Permutations with Backtracking with divide and conquer algorithm implementation

permute([X,Y]) ->
    [[X,Y], [Y,X]];
permute(L) ->
    [[[HS|Prmtd] || Prmtd <- permute(SL)] || [HS|SL] <- swap(L)].
 
swap(L) ->
    swap(L, length(L), 1, []).

swap(L, J, J, Slts) ->
    [L|Slts];
swap(L, M, J, Slts) ->
    ElemJ = lists:nth(J, L), 
    ElemM = lists:nth(M, L),
    SwpdJ = lists:append([[ElemM], lists:delete(ElemM, lists:delete(ElemJ, L)), [ElemJ]]),
    swap(L, M -1, J, [SwpdJ|Slts]).
