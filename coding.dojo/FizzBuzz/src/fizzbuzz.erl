-module(fizzbuzz).

-export([fb/1]).

fb(N) ->
    case {isMultipleOf(N, 3), isMultipleOf(N, 5)} of
	{true, false} ->
	    'Fizz';
	{false, true} ->
	    'Buzz';
	{true, true} ->
	    'FizzBuzz';
	_ ->
	    N
    end.

isMultipleOf(N, X) ->
    0 == N rem X.
