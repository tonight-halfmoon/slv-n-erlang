%%%% 10th Feb, 16; Jeddah, SA %%%%%
-module(mergesort).
-include_lib("eunit/include/eunit.hrl").
-export([ms/1]).

split_test() ->
    Expect = {[1,2], [3,4]},
    Linput = [1,2,3,4],
    ?assertEqual(Expect, split(Linput)).

split_odd_list_test() ->
    Expect = {[1,2], [3,4,5]},
    Linput = [1,2,3,4,5],
    ?assertEqual(Expect, split(Linput)).

ms_test()->
    Expect =[1,2,5,9,10,22,78,89,100],
    Linput =[5,1,9,2,10,78,22,100,89],
    ?assertEqual(Expect, ms(Linput)).

ms_empty_test() ->
    ?assertEqual([], ms([])).

ms_sol_test() ->
    ?assertEqual([a], ms([a])).

ms_small_list_test() ->
    ?assertEqual([1,2,4], ms([4,2,1])).

ms_small2_list_test() ->
    ?assertEqual([1,2,4,6], ms([4,2,6,1])).

ms_somequalelem_test() ->
    ?assertEqual([1,2,2,4,5,6,7,7], ms([2,1,4,5,2,7,6,7])).

ms_sorted_test() ->
    ?assertEqual([1,2,3,4,5,6], ms([1,2,3,4,5,6])).

ms_negvals_test() ->
    ?assertEqual([-9,-1,1,5,10], ms([-1,10,5,1,-9])).

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
    case H1 =< H2 of
	true ->
	    [H1|ms(T1, [H2|T2])];
	false ->
	    [H2|ms([H1|T1], T2)]
    end.

split([]) ->
    [];
split([_]) ->
    [];
split([H|T]) ->
    split(T, [H]).

split(L, R) when length(L) =:= length(R);
		 length(L) =:= length(R) + 1 ->
    {R, L};
split([H|T], R) ->
    split(T, R ++ [H]).
