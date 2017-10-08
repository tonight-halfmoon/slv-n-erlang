-module(swap_adjacent).
-include_lib("eunit/include/eunit.hrl").
-export([swp/1]).

swp([]) ->
    [];
swp([A]) ->
    [A];
swp([H, Next]) ->
    [Next, H];
swp([H, Hn|[T]]) ->
    [Hn, T|[H]];
swp([H, Hn|[Th, Tn]]) ->
    [Hn, Th| swp([H, Tn])];
swp([H, Hn|T]) ->
    [Hn, H| swp(T)].

    
swap_adjacent_test() ->
    ?assertEqual([b,a], swp([a,b])).

swap_adjacent_empty_lst_test() ->
    ?assertEqual([], swp([])).

swap_adjacent_1elm_lst_test() ->
    ?assertEqual([a], swp([a])).

swap_adjacent_3elm_lst_test() ->
    ?assertEqual([b,c,a], swp([a,b,c])).

swap_adjacent_5elm_lst_test() ->
    ?assertEqual([b,a,d,e,c], swp([a,b,c,d,e])).

swap_adjacent_9elm_lst_test() ->
    ?assertEqual([b,a,d,c,f,e,h,i,g], swp([a,b,c,d,e,f,g,h,i])).

swap_adjacent_ghielm_lst_test() ->
    ?assertEqual([h,i,g], swp([g,h,i])).
