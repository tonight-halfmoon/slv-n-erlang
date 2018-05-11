-module(collect).
-export([collect/1]).
-include_lib("eunit/include/eunit.hrl").

collect_test()->
    Expected_value = [4,100],
    L = [1,2,3,5,7,10],
    ?assertEqual(Expected_value, collect(L)).

collect([])->
    [];
collect(L) ->
    collect(L,[]).

collect([H|T], Acc) ->
    case even(H) of
	true ->
	    collect(T, Acc ++[H*H])
		;
	false ->
	    collect(T, Acc)
    end;
collect([], Acc) ->
    Acc.

even(X)->
    case X rem 2 =:= 0 of
	true ->
	    true;
	false ->
	    false
    end.
