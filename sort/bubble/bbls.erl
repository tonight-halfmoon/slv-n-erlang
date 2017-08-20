-module(bbls).
-include_lib("eunit/include/eunit.hrl").
-export([bbls/1]).

bbls_test() ->
    ?assertEqual([1,2,4,5], bbls([5,1,4,2])).

bbls_l_test() ->
    ?assertEqual([0,1,3,5,5,6,9,10,12,19,22,100], bbls([100,12,5,0,6,1,5,19,3,10,9,22])).

bbls_empty_test() ->
    ?assertEqual([], bbls([])).

bbls_1elem_lst_test() ->
    ?assertEqual([a], bbls([a])).

bbls_reversed_lst_test()->
    ?assertEqual([6,7,8,9], bbls([9,8,7,6])).

bbls_sorted_lst_test()->
    ?assertEqual([a,b,c,d,e], bbls([a,b,c,d,e])).

bbls_3elem_lst_test()->
    ?assertEqual([a,b,c], bbls([c,a,b])).

bbls_2elem_lst_test()->
    ?assertEqual([a,b], bbls([b,a])).

bbls_Hl_test() ->
    ?assertEqual([0,1,3,5,5,6,9,9,10,12,12,12,19,21,22,32,32,32,43,43,43,45,45,54,54,56,65,67,72,76,76,78,78,87,90,98,98,100], bbls([100,12,5,0,6,1,5,19,3,10,9,22, 72, 43,12, 76,87,98,09,45,32,90,98,76,65,78,54,54,43,32,21,12,32,43,45,56,67,78])).

bbls(L) ->
    bbls(L, length(L)).

bbls(L, 0) ->
    L;
bbls([H|T], N) ->
   bbls(sngl_rnd(H, T, []), N-1).

sngl_rnd(X, [Y|T], L) ->
    case X >= Y of
	true ->
	    sngl_rnd(X, T, L ++ [Y]);
	false ->
	    sngl_rnd(Y, T, L ++ [X]) 
    end;
sngl_rnd(X, [], L) ->
    L ++ [X].
