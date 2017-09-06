-module(primes_tests).
-include_lib("eunit/include/eunit.hrl").
-import(primes, [isprime/1]).

extra_test_() ->
    {"'22292374792341233' is not a prime", ?_assertEqual(false, isprime(22292374792341233))}.

extra_9007199254740991_test_() ->
    {"'9007199254740991' is not a prime", ?_assertEqual(false, isprime(9007199254740991))}.

test_900777777127769_test_() ->
    {"'900777777127769' is a prime", ?_assertEqual(true, isprime(900777777127769))}.

test_989786771731687_test_() ->
    {"'989786771731687' is a prime", ?_assertEqual(true, isprime(989786771731687))}.
