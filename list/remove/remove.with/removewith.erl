-module(removewith).
-include_lib("eunit/include/eunit.hrl").
-import(primes, [isprime/1]).
-export([removewith/2]).

removewith_even_test() ->
    ?assertEqual([1,3], removewith(fun (X) -> X rem 2 =:= 0 end, [1,2,3,4])).

removewith_prime_test() ->
    ?assertEqual([1,4,8], removewith(fun (X) -> primes:isprime(X) end, [1,2,3,4,7,8])).

removewith_empty_test() ->
    ?assertEqual([], removewith(fun (_) -> true end, [])).


removewith(_Pred, []) ->
    [];
removewith(Pred, [H|T]) ->
    case Pred(H) of
	true ->
	    removewith(Pred, T);
	false ->
	    [H| removewith(Pred, T)]
    end.
