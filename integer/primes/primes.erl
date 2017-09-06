-module(primes).
-export([isprime/1]).
-include_lib("eunit/include/eunit.hrl").

run_test()->
    ?assertEqual(isprime(2), true),
    ?assertEqual(isprime(5), true),
    ?assertEqual(isprime(4), false),
    ?assertEqual(isprime(439), true),
    ?assertEqual(isprime(0), false),
    ?assertEqual(isprime(1), false),
    ?assertEqual(isprime(3), true),
    ?assertEqual(isprime(907), true),
    ?assertEqual(isprime(811),true),
    ?assertEqual(isprime(991), true),
    ?assertEqual(isprime(999), false).

isprime(2)->
    true;
isprime(N) when N < 2 ->
    false;
isprime(N) ->
    Div = round(math:sqrt(N)),
    isprime(N, Div, N rem Div).
isprime(_, _, 0) ->
    false;
isprime(_, 2, _) -> 
    true;
isprime(N, Div, _) ->
    Div2 = Div -1,
    isprime(N, Div2, N rem Div2).
