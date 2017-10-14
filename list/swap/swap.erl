-module(swap).
-export([swap/1, swap_2500_profile/0]).
-include_lib("eunit/include/eunit.hrl").

swap_test_() ->
    {"swapping '[1,2,3]' must result in '[1,2,3], [2,1,3] , [3,1,2]'", 
     ?_assertEqual([[1,2,3], [2,1,3], [3,1,2]], swap([1,2,3]))}.

swap_empty_test_() ->
    {"swapping '[]' must result in '[]'", 
     ?_assertEqual([], swap([]))}.

swap_2500_test_() ->
    {"Swap '[1..2500]' must halt", 
    ?_assertMatch([L|_] when length(L) == 2500, swap(lists:seq(1, 2500)))}.

swap_2500_profile() ->
     {X, _O} = timer:tc(?MODULE, swap, [lists:seq(1, 2500)]), 
     io:fwrite("Profile: ~p microsends~n", [X]).

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
    swap(L, M - 1, J, [SwpdJ|Slts]).

concat(L1, L2, L3) when is_list(L2) ->
    [L1|[L3|L2]].
