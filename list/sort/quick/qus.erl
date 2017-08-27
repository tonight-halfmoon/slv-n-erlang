-module(qus).
-include_lib("eunit/include/eunit.hrl").
-export([qs/1]).

qs_empty_test() ->
    ?assertEqual([], qs([])).

qss_sol_test() ->
    ?assertEqual([a], qs([a])).

qss_small_list_test() ->
    ?assertEqual([1,2,4], qs([4,2,1])).

qss_small2_list_test() ->
    ?assertEqual([1,2,4,6], qs([4,2,6,1])).

qss_somequalelem_test() ->
    ?assertEqual([1,2,2,4,5,6,7,7], qs([2,1,4,5,2,7,6,7])).

qss_sorted_test() ->
    ?assertEqual([1,2,3,4,5,6], qs([1,2,3,4,5,6])).

qss_negvals_test() ->
?assertEqual([-9,-1,1,5,10], qs([-1,10,5,1,-9])).


qs([]) -> 
    [];
qs([Pivot|T]) ->
    {Left, Right} = split([Pivot|T]),
    lists:append([qs(Left), [Pivot], qs(Right)]).

split([]) -> 
    [];
split([H|T]) ->
    split(T , H, [], []).

split([], _, Left, Right) ->
    {Left, Right};
split([H|T], Pivot, Left, Right) ->
    case H < Pivot of 
	true ->
	     split(T,  Pivot, Left ++ [H], Right);
	false ->
	    split(T, Pivot, Left, [H|Right])
    end.
