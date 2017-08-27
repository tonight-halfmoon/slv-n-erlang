-module(split2).
-include_lib("eunit/include/eunit.hrl").
-export([split/1]).

split_even_list_test() ->
    ?assertEqual({[1,2], [3,4]}, split([1,2,3,4])).
split_odd_list_test() ->
    ?assertEqual({[a], [b,c]}, split([a,b,c])).
split_empty_list_test() ->
    ?assertEqual({[],[]}, split([])).
split_one_elm_list_test() ->
    ?assertEqual({[], [a]}, split([a])).
split_8_elm_list_test() ->
    ?assertEqual({[1,2,3,4],[5,6,7,8]}, split([1,2,3,4,5,6,7,8])).
split_9_elm_list_test() ->
    ?assertEqual({[0,1,2,3],[4,5,6,7,8]}, split([0,1,2,3,4,5,6,7,8])).

reverse_test() ->
    ?assertEqual([4,3,2,1], reverse([1,2,3,4])).
reverse_odd_list_test() ->
    ?assertEqual([a,b,c], reverse([c,b,a])).
reverse_empty_list_test() ->
    ?assertEqual([],reverse([])).

split(L) -> split(L, L, []).
split([], L1, L2) -> {reverse(L2), L1};
split([_], L1, L2) -> {reverse(L2), L1};
split([_,_|TT], [H|T], L2) -> split(TT, T, [H|L2]).
%% L2/L1 flipped because we do want [1,2,3] to split into {[1] , [2,3]}

reverse(L) -> reverse(L, []).
reverse([], L) -> L;
reverse([H|T], L) -> reverse(T, [H|L]). 

    
    
    
