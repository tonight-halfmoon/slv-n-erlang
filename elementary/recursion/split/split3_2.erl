-module(split3_2).
-export([split3/1]).
-include_lib("eunit/include/eunit.hrl").

split3_4elm_test() ->
    ?assertEqual({[a,b],[c,d],[]}, split3([a,b,c,d])).

split3_5elm_test() ->
    ?assertEqual({[a,b],[c,d],[e]}, split3([a,b,c,d,e])).
split3_long_test() ->
    ?assertEqual({[a,b,g,h],[c,d,i,j],[e,f,k,l]},split3([a,b,c,d,e,f,g,h,i,j,k,l])).
split3_abitlonger_test() ->
    ?assertEqual({[a,b,g,h,m],[c,d,i,j],[e,f,k,l]},split3([a,b,c,d,e,f,g,h,i,j,k,l,m])).

split3_one_test() ->
    ?assertEqual({[a],[],[]}, split3([a])).

split3_allsame3_test() ->
    ?assertEqual({[a,a],[a],[]}, split3([a,a,a])).

split3_has2_test() ->
    ?assertEqual({[a,a],[],[]}, split3([a,a])).

split3(L)->
    split(L, [], [], []).
split([X,Y,Z,E,F,S|T], L1,L2,L3) ->
    split(T, [Y,X|L1], [E,Z|L2], [S,F|L3]);
split([X,Y,Z,E,F|T], L1,L2,L3) ->
    split(T, [Y,X|L1], [E,Z|L2], [F|L3]);
split([X,Y,Z,E|T], L1,L2,L3) ->
    split(T, [Y,X|L1], [E,Z|L2], L3);
split([X,Y,Z|T], L1,L2,L3) ->
    split(T, [Y,X|L1], [Z|L2], L3);
split([X,Y|T], L1,L2,L3) ->
    split(T, [Y,X|L1], [L2], L3);
split([X|T], L1,L2,L3) ->
    split(T, [X|L1], L2, L3);
split([],L1,L2,L3) ->
    {reverse(L1), reverse(L2), reverse(L3)}.


reverse(L)->
    reverse(L,[]).
reverse([H|T], L)->
    reverse(T, [H|L]);
reverse([],L) ->
    L.

