-module(swap).
-export([swap/1]).
-include_lib("eunit/include/eunit.hrl").

swap_test_() ->
    {"swapping '[1,2,3]' must result in '[1,2,3], [2,1,3] , [3,1,2]'", 
     ?_assertEqual([[1,2,3], [2,1,3], [3,1,2]], swap([1,2,3]))}.

swap_empty_test_() ->
    {"swapping '[]' must result in '[]'", 
     ?_assertEqual([], swap([]))}.


swap(L) ->
    swap(L, length(L), 1, []).

swap([], _, _, Slts) ->
    Slts;
swap(L, J, J, Slts) ->
    [L|Slts];
swap(L, M, J, Slts) ->
    ElemJ = lists:nth(J, L), 
    ElemM = lists:nth(M, L),
    SwpdJ = concat(ElemM, lists:delete(ElemM, lists:delete(ElemJ, L)), ElemJ),
    swap(L, M -1, J, [SwpdJ|Slts]).

concat(L1, L2, L3) when is_list(L2) ->
    [L1|[L3|L2]].
