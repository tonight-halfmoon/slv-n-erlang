-module(remove).
-include_lib("eunit/include/eunit.hrl").
-export([remove/2]).

remove_empty_lst_test() ->
    ?assertEqual([], remove([], 9)).

remove_3_from_3elm_lst_test() ->
    ?assertEqual([], remove([1,2,3], 3)).

remove_1_from_3elm_lst_test() ->
    ?assertEqual([2,3], remove([1,2,3], 1)).

remove_0_from_3elm_lst_test() ->
    ?assertEqual([1,2,3], remove([1,2,3], 0)).

remove_0_from_empty_lst_test() ->
    ?assertEqual([], remove([], 0)).

remove_alot_from_3elm_lst_test() ->
    ?assertEqual([], remove([1,2,3], 90)).

remove_neg_n_from_3elm_lst_test() ->
    ?assertEqual('n must be non-negative', remove([1,2,3], -2)).

remove([], _) ->
    [];
remove(S, 0) ->
    S;
remove([_|T], N) ->
    case abs(N) =:= - N of
	true ->
	    'n must be non-negative';
 	false -> 
	    remove(T, N - 1)
    end.
