-module(swap).
-export([swap/1]).
-include_lib("eunit/include/eunit.hrl").

swap_test_() ->
    {"swapping '[1,2,3]' must result in '[1,2,3], [2,3,1] , [3,2,1]'", ?_assertEqual([[1,2,3], [2,3,1], [3,2,1]], swap([1,2,3]))}.

swap(L) ->
    swap(L, length(L), 1, []).

swap(L, J, J, Slts) ->
    [L|Slts];
swap(L, M, J, Slts) ->
    ElemJ = lists:nth(J, L), 
    ElemM = lists:nth(M, L),
    SwpdJ = lists:append([[ElemM], lists:delete(ElemM, lists:delete(ElemJ, L)), [ElemJ]]),
    swap(L, M -1, J, [SwpdJ|Slts]).
