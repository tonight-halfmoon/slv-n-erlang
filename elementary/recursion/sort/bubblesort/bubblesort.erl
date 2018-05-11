-module(bubblesort).
-export([bbls/1, reverse/1]).
-include_lib("eunit/include/eunit.hrl").

bbls_test() ->
    ?assertEqual([1,2,4,5], bbls([5,1,4,2])).
bbls_lrg_lst_test() ->
    ?assertEqual([0,1,3,5,5,6,9,10,12,19,22,100], bbls([100,12,5,0,6,1,5,19,3,10,9,22])).
bbls_empty_lst_test() ->
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

grte(X,Y)->
    X>=Y.

bbls(L) ->
    bbls(L, length(L)).% L).

bbls(L, 0)->L;
bbls([H|T], N)->%[_|TT])->
    %bbls(reverse(sngl_rnd(H,T,[])),TT);
    bbls(sngl_rnd(H,T,[]),N-1).% TT);
%%bbls(L, []) ->
%%    L.

sngl_rnd(X, [Y|T], L) ->
    io:format("X:~w, Y:~w | T:~w, PL:~w;~n", [X,Y,T,L]),
    case grte(X,Y) of
	true ->
	    sngl_rnd(X, T, L++[Y]);%[Y|L]); %
	false ->
	    sngl_rnd(Y,T, L++[X])%[X|L] ) % 
    end;
sngl_rnd(X, [], L) ->
    io:format("X:~w~n", [X]),
    L++[X]. %[X|L].%

reverse(L)->
    reverse(L, []).
reverse([H|T], L)->
    reverse(T, [H|L]);
reverse([],L) ->
    L.
