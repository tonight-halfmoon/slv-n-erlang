-module(inss).
-include_lib("eunit/include/eunit.hrl").
-export([inss/1]).

inss_test() -> 
    ?assertEqual([1,3,4,6], inss([6,4,1,3])).

inss_lrg_lst_test() -> 
    ?assertEqual([1,4,5,6,7,9,10,11,12,14,52], inss([5,6,4,10,11,12,1,14,9,52,7])).

inss_empty_lst_test() -> 
    ?assertEqual([], inss([])).

inss_sol_test() ->
    ?assertEqual([a], inss([a])).

inss_2elem_lst_test() -> 
    ?assertEqual([a,b], inss([b,a])).

inss_somequalelem_test() ->
    ?assertEqual([1,2,2,4,5,6,7,7], inss([2,1,4,5,2,7,6,7])).

inss_sorted_lst_test() -> 
    ?assertEqual([a,b,c,d,e], inss([a,b,c,d,e])).

inss_negvals_test() ->
    ?assertEqual([-9,-1,1,5,10], inss([-1,10,5,1,-9])).

inss_small_list_test() ->
    ?assertEqual([1,2,4], inss([4,2,1])).

inss(L) -> 
    inss(L, []).

inss([], L) -> 
    L;
inss([X|T], L) -> 
    inss(T, inssx(X, L)).

inssx(X, L) -> 
    inssx(X, L, []).

inssx(X, [H|T], L) -> case X >= H of
			  true -> 
			      inssx(X, T, L ++ [H]);
			  false -> 
			      inssx(H, T, L ++ [X])
		      end;
inssx(X, [], L) -> 
    L ++ [X].
