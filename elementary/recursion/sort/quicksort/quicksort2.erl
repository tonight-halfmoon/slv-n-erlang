%%% on my own on 9th Feb 2016; Jeddah, SA %%%
-module(quicksort2).
-export([qs/1, qs_tail/1]).
-include_lib("eunit/include/eunit.hrl").

qs_already_sorted_test()->
    EV = [1,2,3,4,5,6,7,8,9,10],
    L = [1,2,3,4,5,6,7,8,9,10],
    ?assertEqual(EV, qs_tail(L)).
qs_tail_test()->
    Expected_value = [1,4,5,6,7,8,9],
    Input_value = [9,4,7,1,8,6,5],
    ?assertEqual(Expected_value, qs_tail(Input_value)).
    
qs_test() ->
    Expected_value = [1,4,5,6,7,8,9],
    Input_value = [9,4,7,1,8,6,5],
    ?assertEqual(Expected_value, qs(Input_value)).

qs_empty_test ()->
    Expected_value = [],
    Input_value = [],
    ?assertEqual(Expected_value, qs(Input_value)).

qs_char_test() ->
    Expected_value = [a,b,c,g,k,l,o,r,s,t,v,w],
    Input_value = [k,b,t,g,a,s,c,v,r,w,l,o],
    ?assertEqual(Expected_value, qs(Input_value)).

qs([]) -> [];
qs([H|T]) ->
    {Left, Right} = split(H, T, [], []),
    qs(Left) ++ [H| qs(Right)].

split(_,[], Left, Right) -> {Left, Right};
split(Pivot, [H|T], Left, Right) ->
    if H =< Pivot -> split(Pivot, T, [H|Left], Right);
       H > Pivot -> split(Pivot, T, Left, [H|Right])
    end.

qs_tail([]) ->
    [];
qs_tail(L) ->
    qs_tail(L, []).
qs_tail([], S) ->
    S;
qs_tail([H|T], S) ->
    {Left, Right} = split(H,T,[],[]),
    qs_tail(Left, [H|qs_tail(Right,S)] ).

