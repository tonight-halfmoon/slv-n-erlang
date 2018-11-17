-module(fizzbuzz).

-export([fb/1]).

fb(N) ->
    case isMultipleOf(N, 3) of
	true ->
	    'Fizz';
	_ ->
	    N
    end.

isMultipleOf(N, X) ->
    0 == N rem X.
