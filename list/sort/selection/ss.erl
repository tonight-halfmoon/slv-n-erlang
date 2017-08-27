-module(ss).
-include_lib("eunit/include/eunit.hrl").
-export([ss/1]).

ss_test()->
    Expect =[1,2,5,9,10,22,78,89,100],
    Linput =[5,1,9,2,10,78,22,100,89],
    ?assertEqual(Expect, ss(Linput)).

ss_empty_test() ->
    ?assertEqual([], ss([])).

ss_sol_test() ->
    ?assertEqual([a], ss([a])).

ss_small_list_test() ->
    ?assertEqual([1,2,4], ss([4,2,1])).

ss_small2_list_test() ->
    ?assertEqual([1,2,4,6], ss([4,2,6,1])).

ss_somequalelem_test() ->
    ?assertEqual([1,2,2,4,5,6,7,7], ss([2,1,4,5,2,7,6,7])).

ss_sorted_test() ->
    ?assertEqual([1,2,3,4,5,6], ss([1,2,3,4,5,6])).

ss_negvals_test() ->
?assertEqual([-9,-1,1,5,10], ss([-1,10,5,1,-9])).


ss(L) -> 
    ss(L, []).

ss([], L) -> 
    L;
ss([H|T], L) ->
    Min = min_([H|T]),
    ss(lists:delete(Min, [H|T]),  L ++ [Min]).

min_([]) -> 
    [];
min_([H|T]) -> 
    min_(T, H).

min_([], Min) -> 
    Min;
min_([H|T], Min) when H =< Min -> 
    min_(T, H);
min_([H|T], Min) when H > Min -> 
    min_(T, Min).
    
