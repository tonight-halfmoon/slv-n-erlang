-module(n_adjacent_swaps).
-include_lib("eunit/include/eunit.hrl").
-export([nas/2]).
-import(swap_adjacent, [swp/1]).

nas(L, 0) ->
    L;
nas(L, K) ->
    nas(L, K, 0, [L]).

nas(_L, K, K, S) ->
    S;
nas(L, K, I, S) ->
    Swpith = swap_adjacent:swp(L), 
    nas(Swpith, K - 1, I, [Swpith|S]).

nas_123_2_adjacent_swaps_test_() ->
    {"After 2 adjacent swaps '[1,2,3]' yields '[[3,1,2],[2,3,1],[1,2,3]]'",
    ?_assertEqual([[3,1,2],[2,3,1],[1,2,3]], nas([1,2,3], 2))}.

nas_123_3_adjacent_swaps_test_() ->
    {"After 3 adjacent swaps '[1,2,3]' yields '[[1,2,3],[3,1,2],[2,3,1],[1,2,3]]'",
    ?_assertEqual([[1,2,3],[3,1,2],[2,3,1],[1,2,3]], nas([1,2,3], 3))}.

nas_1234_2_adjacent_swaps_test_() ->
    {"After 2 adjacent swaps '[1,2,3]' yields '[[3,4,1,2],[2,3,4,1],[1,2,3,4]]'",
    ?_assertEqual([[3,4,1,2],[2,3,4,1],[1,2,3,4]], nas([1,2,3,4], 2))}.

nas_1234_3_adjacent_swaps_test_() ->
    {"After 3 adjacent swaps '[1,2,3]' yields '[[4,1,2,3],[3,4,1,2],[2,3,4,1],[1,2,3,4]]'",
    ?_assertEqual([[4,1,2,3],[3,4,1,2],[2,3,4,1],[1,2,3,4]], nas([1,2,3,4], 3))}.

nas_1234_4_adjacent_swaps_test_() ->
    {"After 4 adjacent swaps '[1,2,3]' yields '[[4,1,2,3],[3,4,1,2],[2,3,4,1],[1,2,3,4]]'",
    ?_assertEqual([[1,2,3,4], [4,1,2,3],[3,4,1,2],[2,3,4,1],[1,2,3,4]], nas([1,2,3,4], 4))}.

nas_test() ->
    ?assertEqual([[b,a], [a,b]], nas([a,b], 1)).

nas_empty_lst_test() ->
    ?assertEqual([[],[]], nas([], 1)).

nas_1elm_lst_test() ->
    ?assertEqual([[a],[a]], nas([a], 1)).

nas_3elm_lst_test() ->
    ?assertEqual([[b,c,a], [a,b,c]], nas([a,b,c], 1)).

nas_5elm_lst_test() ->
    ?assertEqual([[b,a,d,e,c], [a,b,c,d,e]], nas([a,b,c,d,e],1)).

swap_2500_test_() ->
    {"N Adjacent swapping '[1..2500]' must halt", 
    ?_assertMatch([L|_] when length(L) == 2500, nas(lists:seq(1, 2500), 2500))}.
