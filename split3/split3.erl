-module(split3).
-include_lib("eunit/include/eunit.hrl").
-export([split3/1]).

split3_test() ->
    ?assertEqual({[a,d],[b],[c]}, split3([a,b,c,d])).

split3_empty_test() ->
    ?assertEqual({[],[],[]}, split3([])).

split3_10elm_lst_test() ->
    ?assertEqual({[a,d,g,j],[b,e,h],[c,f,i]}, split3([a,b,c,d,e,f,g,h,i,j])).

split3_7elm_lst_test() ->
    ?assertEqual({[0,3,6],[1,4,7],[2,5]}, split3([0,1,2,3,4,5,6,7])).

split32_test() ->
    ?assertEqual({[a,d],[b,e],[c]}, split3([a,b,c,d,e])).

split3_long_test() ->
    ?assertEqual({[a,d,g,j],[b,e,h,k],[c,f,i,l]}, split3([a,b,c,d,e,f,g,h,i,j,k,l])).

split3_abitlonger_test() ->
    ?assertEqual({[a,d,g,j,m,o],[b,e,h,k,n,o],[c,f,i,l,o]}, split3([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,o,o])).

split3_one_elm_lst_test() ->
    ?assertEqual({[a],[],[]}, split3([a])).

split3_allsame3_test() ->
    ?assertEqual({[a],[a],[a]}, split3([a,a,a])).

split3_has2_test() ->
    ?assertEqual({[a],[a],[]}, split3([a,a])).

split3 (L)->
    split3(L, [], [], []).

split3([], L1, L2, L3) ->
    {reverse(L1), reverse(L2), reverse(L3)};
split3([X], L1, L2, L3) ->
    {reverse([X|L1]), reverse(L2), reverse(L3)};
split3([X,Y], L1, L2, L3) ->
    {reverse([X|L1]), reverse([Y|L2]), reverse(L3)};
split3([X,Y,Z|TTT], L1, L2, L3)->
    split3(TTT, [X|L1], [Y|L2], [Z|L3]).

reverse(L)->
    reverse(L,[]).

reverse([H|T], L)->
    reverse(T, [H|L]);
reverse([],L) ->
    L.

