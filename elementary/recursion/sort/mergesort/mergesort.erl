%%%% 10th Feb, 16; Jeddah, SA %%%%%
-module(mergesort).
-export([ms/1]).
-include_lib("eunit/include/eunit.hrl").

split_test()->
    EV = {[1,2], [3,4]},
    L = [1,2,3,4],
    ?assertEqual(EV, split(L)).
split_odd_list_test()->
    EV = {[1,2], [3,4,5]},
    L = [1,2,3,4,5],
    ?assertEqual(EV, split(L)).

ms_test()->
    EV=[1,2,5,9,10,22,78,89,100],
    L =[5,1,9,2,10,78,22,100,89],
    ?assertEqual(EV, ms(L)).
ms_small_list_test()->
    EV = [1,2,4],
    L = [4,2,1],
    ?assertEqual(EV, ms(L)).
ms_small2_list_test()->
    EV = [1,2,4,6],
    L = [4,2,6,1],
    ?assertEqual(EV, ms(L)).
ms_somequalelem_test()->
    ?assertEqual([1,2,2,4,5,6,7,7], ms([2,1,4,5,2,7,6,7])).

ms([]) ->
    [];
ms([X]) ->
    [X];
ms(L) ->
    {Left, Right} = split(L),
    ms(ms(Left), ms(Right)).
ms([], R)->
    R;
ms(L, []) ->
    L;
ms([H1|T1], [H2|T2]) ->
    case lte(H1,H2) of
	true ->
	    [H1|ms(T1, [H2|T2])];
	false ->
	    [H2|ms([H1|T1], T2)]
    end.
lte(X,Y)->
    X=<Y.
split([]) ->
    [];
split([_]) ->
    [];
split([H|T]) ->
    split(T, [H]).
split(L, R) when length(L) =:= length(R) ;
		  length(L) =:= length(R) + 1 ->
    {R, L};
split([H|T], R) ->
    split(T, R ++ [H]).
