-module(filterwith).
-include_lib("eunit/include/eunit.hrl").
-export([filterwith/2]).

filterwith_odd_8elm_lst_test() ->
    ?assertEqual([5,3,7,9], filterwith(fun odd/1, [2,5,3,4,6,7,9,8])).

filterwith_odd_empty_lst_test() ->
    ?assertEqual([], filterwith(fun odd/1, [])).

filterwith_odd_one_test() ->
    ?assertEqual([1], filterwith(fun odd/1, [1])).

filterwith_odd_has2_test() ->
    ?assertEqual([1], filterwith(fun odd/1, [1,2])).

filterwith_odd_3elems_test() ->
    ?assertEqual([5,3], filterwith(fun odd/1, [2,5,3])).

filterwith_odd_4elems_test() ->
    ?assertEqual([5,3], filterwith(fun odd/1, [2,5,3,6])).

filterwith_fun_false_test() ->
    ?assertEqual([], filterwith(fun(_X) -> false end, [1,12,2,4,5,r,w,2,12,33,d,dd,p])).

filterwith_fun_true_test() ->
    L = [1,w,10,2,3,5,6,t,d,s],
    ?assertEqual(L, filterwith(fun(_X) -> true end, L)).

				     
odd(X) -> case X rem 2 of 0 -> false; _ -> true end.

filterwith(F, L)->
    filterwith(F, L, []).

filterwith(_F, [], L) ->
    L;
filterwith(F, [H|T], L) ->
    case F(H) of
	true ->
	    filterwith(F, T, lists:append(L, [H]));
	false ->
	    filterwith(F, T, L) 
    end.
