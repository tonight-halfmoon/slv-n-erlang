-module(swap).
-include_lib("eunit/include/eunit.hrl").
-export([swap_adjacent/1]).

swap_adjacent_test() ->
    ?assertEqual([b,a], swap_adjacent([a,b])).

swap_adjacent_empty_lst_test() ->
    ?assertEqual([], swap_adjacent([])).

swap_adjacent_1elm_lst_test() ->
    ?assertEqual([a], swap_adjacent([a])).

swap_adjacent_3elm_lst_test() ->
    ?assertEqual([b,a,c], swap_adjacent([a,b,c])).

swap_adjacent_5elm_lst_test() ->
    ?assertEqual([b,a,d,c,e], swap_adjacent([a,b,c,d,e])).

swap_adjacent([]) ->
    [];
swap_adjacent([A]) -> % to be able to consider non-even lists
    [A];
swap_adjacent([H, Next|T]) ->
    [Next, H] ++ swap_adjacent(T).
    
