-module(avg).
-include_lib("eunit/include/eunit.hrl").
-export([avg/1]).

empty_lst_avg_test() ->
    ?assertEqual(0, avg([])).

five_elm_lst_avg_test() ->
    ?assertEqual(3, avg([1,2,3,4,5])).

all_1_elm_avg_test() ->
    ?assertEqual(1, avg([1,1,1,1,1])).

avg(L) -> tail_avg(L, 0, 0).

tail_avg([], Sum, Count) when Count =/= 0->
    floor(Sum/Count);
tail_avg([], Sum, Count) when Count =:= 0 ->
    Sum;
tail_avg([H|T], Sum, Count) ->
    tail_avg(T, H+Sum, 1+Count).
 
