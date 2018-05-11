-module(solution).
-export([main/0, inss/1]).
-include_lib("eunit/include/eunit.hrl").

inss_test() -> ?assertEqual([1,3,4,6], inss([6,4,1,3])).
inss_lrg_lst_test() -> ?assertEqual([1,4,5,6,7,9,10,11,12,14,52], inss([5,6,4,10,11,12,1,14,9,52,7])).
inss_empty_lst_test() -> ?assertEqual([], inss([])).
inss_2elem_lst_test() -> ?assertEqual([a,b], inss([b,a])).
inss_sorted_lst_test() -> ?assertEqual([a,b,c,d,e], inss([a,b,c,d,e])).
inss_testcase2_test() ->  ?assertEqual([1,2,3,4,5,6,7,8,9], inss([9,8,6,7,3,5,4,1,2])).
    
main () ->
    {ok, [N]} = io:fread("", "~d"),
    {ok, Xs} = io:fread("", string:join(replicate(N, "~d"), " ")),
    L = inss(Xs),
    print4hackerrank(L)
	.

inss(L) -> inss(L,[]).
inss([X|T], L) ->
    L2 = inssx(X, L),
    case L =:= [] of 
	false ->
	    print(lists:append([L2, T]));
	true ->
	    ok
    end,
    inss(T, L2);
inss([], L) -> L.

inssx(X, L) -> inssx(X, L, []).
inssx(X, [H|T], L) -> 
    case grte(X,H) of
	true -> 
	    inssx(X, T,  L ++ [H])
		;
	false -> inssx(H, T,  L ++ [X])
    end;
inssx(X, [], L) -> L ++ [X].

grte(X,Y)-> X>=Y.

print(ok) -> ok;
print([]) -> ok;
print(L) ->  print4hackerrank(L).

replicate(Bndry, _) when Bndry =< 0 -> [];
replicate(Bndry, O) -> [O|replicate(Bndry-1, O)].

print4hackerrank([]) -> ok;
print4hackerrank(ok) -> ok;
print4hackerrank(L) -> io:fwrite(string:concat(string:join(replicate(length(L),"~w"), " "), "\n"), L).

