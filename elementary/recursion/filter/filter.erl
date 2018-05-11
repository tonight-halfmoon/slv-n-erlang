-module(filter).
-export([filter/2]).
-include_lib("eunit/include/eunit.hrl").

filter_test()->
    Expected_value = [2,4],
    L = [1,2,3,4],
    ?assertEqual(Expected_value, filter(fun even/1, L)).

even(X) ->
    case X rem 2 =:= 0 of 
	true->
	    true;
	false ->
	    false
    end.

filter(Pred, [H|T]) ->
    case Pred(H) of
	true->
	    [H|filter(Pred,T)];
	false ->
	    filter(Pred,T)
    end;
filter(_, []) ->
    [].
