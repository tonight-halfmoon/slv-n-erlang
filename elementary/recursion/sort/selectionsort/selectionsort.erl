-module(selectionsort).
-export([ss/1, rotate/1]).
-include_lib("eunit/include/eunit.hrl").

ss_test() ->
    ?assertEqual([1,2,3,4,5], ss([4,1,5,2,3])).
ss_msl_test() ->
    ?assertEqual([-2,-1,0,3,5,6,8,61,78,345], ss([5,3,345,8,6,-2,0,78,61,-1])).
ss_lsl_test() ->
    ?assertEqual([-123,-10,-9,-8,-6,-4,-2,-1,0,3,5,6,8,61,66,78,99,345]
, ss([-9,5,3,345,8,-8,66,-10,-123,99,6,-4,-2,0,78,61,-1,-6])).
ss_empty_lst_test() ->
    ?assertEqual([], ss([])).
ss_1elem_lst_test() ->
    ?assertEqual([a], ss([a])).
ss_abc_test() ->
    ?assertEqual([a,b,c], ss([c,a,b])).

ss(L) -> ss(L, []).
ss([H|T], L) -> 
    [Min|TT] = rotate([H|T]), 
    ss(TT, L ++ [Min]); %[Min| L]);
ss([], L) -> L.

rotate([H|T]) -> rotate(H, T, []).
rotate(Min, [X|T], L) ->  case lse(Min, X) of
			      true ->
				  rotate(Min, T, [X|L]);
			      false ->
				  rotate(X, T, [Min|L])
			  end;
rotate(Min, [], L) -> [Min|L].

lse(X,Y)-> X =< Y.
