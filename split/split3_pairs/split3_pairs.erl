-module(split3_pairs).
-include_lib("eunit/include/eunit.hrl").
-export([split/1]).

split3_pairs_empty_lst_test() ->
    ?assertEqual({[],[],[]}, split([])).

split3_pairs_4elm_test() ->
    ?assertEqual({[a,b],[c,d],[]}, split([a,b,c,d])).

split3_pairs_5elm_test() ->
    ?assertEqual({[a,b],[c,d],[e]}, split([a,b,c,d,e])).

split3_pairs_6elm_test() ->
    ?assertEqual({[1,2],[3,4],[5,6]}, split([1,2,3,4,5,6])).

split3_pairs_7elm_test() ->
    ?assertEqual({[1,2,7],[3,4],[5,6]}, split([1,2,3,4,5,6,7])).

split3_pairs_long_test() ->
    ?assertEqual({[a,b,g,h],[c,d,i,j],[e,f,k,l]}, split([a,b,c,d,e,f,g,h,i,j,k,l])).

split3_pairs_abitlonger_test() ->
    ?assertEqual({[a,b,g,h,m,n],[c,d,i,j,o,p],[e,f,k,l,q,r]}, split([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r])).

split3_pairs_one_elm_lst_test() ->
    ?assertEqual({[a],[],[]}, split([a])).

split3_pairs_allsame3_test() ->
    ?assertEqual({[a,a],[a],[]}, split([a,a,a])).

split3_pairs_has2_test() ->
    ?assertEqual({[a,a],[],[]}, split([a,a])).

split(L)->
    split(L, [], [], []).

split([X,Y,Z,E,F,S|T], L1, L2, L3) ->
    split(T, [Y,X|L1], [E,Z|L2], [S,F|L3]);
split([X,Y,Z,E,F|T], L1,L2,L3) ->
    split(T, [Y,X|L1], [E,Z|L2], [F|L3]);
split([X,Y,Z,E|T], L1,L2,L3) ->
    split(T, [Y,X|L1], [E,Z|L2], L3);
split([X,Y,Z|T], L1, L2, L3) ->
    split(T, [Y,X|L1], [Z|L2], L3);
split([X,Y|T], L1, L2, L3) ->
    split(T, [Y,X|L1], L2, L3);
split([X|T], L1, L2, L3) ->
    split(T, [X|L1], L2, L3);
split([], L1, L2, L3) ->
    {reverse(L1), reverse(L2), reverse(L3)}.


reverse(L)->
    reverse(L, []).

reverse([], L) ->
    L;
reverse([H|T], L)->
    reverse(T, [H|L]).

