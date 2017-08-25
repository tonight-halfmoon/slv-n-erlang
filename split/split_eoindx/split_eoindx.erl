%% split based on even/odd index 
-module(split_eoindx).
-include_lib("eunit/include/eunit.hrl").
-export([split/1]).

split_3elm_lst_test() ->
    ?assertEqual({[a,c], [b]}, split([a,b,c])).
split_test() ->
    ?assertEqual({[a,c,e], [b,d]}, split([a,b,c,d,e])).
split_2elm_list_test() ->
    ?assertEqual({[a],[b]}, split([a,b])).
split_empty_list_test() ->    
    ?assertEqual({[],[]}, split([])).
split_1elm_list_test() ->
    ?assertEqual({[a],[]}, split([a])).
split_8_elm_list_test() ->
    ?assertEqual({[1,3,5,7],[2,4,6,8]}, split([1,2,3,4,5,6,7,8])).
split_9_elm_list_test() ->
    ?assertEqual({[0,2,4,6,8],[1,3,5,7]}, split([0,1,2,3,4,5,6,7,8])).

split(L) ->
    split(L,[],[]).
split([X1,X2|T], L1, L2)->
    split(T, [X1|L1], [X2|L2]);
split([], L1,L2) ->
    {reverse(L1), reverse(L2)};
split([X], L1,L2) ->
    {reverse([X|L1]), reverse(L2)}.

reverse(L) -> reverse(L,[]).
reverse([],L) -> L;
reverse([H|T], L) -> reverse(T, [H|L]).
