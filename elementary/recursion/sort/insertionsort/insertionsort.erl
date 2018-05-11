-module(insertionsort).
-export([inss/1]).
-include_lib("eunit/include/eunit.hrl").

inss_test() -> ?assertEqual([1,3,4,6], inss([6,4,1,3])).
inss_lrg_lst_test() -> ?assertEqual([1,4,5,6,7,9,10,11,12,14,52], inss([5,6,4,10,11,12,1,14,9,52,7])).
inss_empty_lst_test() -> ?assertEqual([], inss([])).
inss_2elem_lst_test() -> ?assertEqual([a,b], inss([b,a])).
inss_sorted_lst_test()-> ?assertEqual([a,b,c,d,e], inss([a,b,c,d,e])).

inss(L) -> inss(L,[]).
inss([X|T], L) -> inss(T, inssx(X, L));
inss([],L) -> L.
inssx(X, L)-> inssx(X, L, []).
inssx(X, [H|T], L) -> case grte(X,H) of
			 true -> inssx(X, T, L ++ [H]);
			 false -> inssx(H, T, L ++ [X])
		     end;
inssx(X, [], L) -> L ++ [X].

grte(X,Y)-> X>=Y.
