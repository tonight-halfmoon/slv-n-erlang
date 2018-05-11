-module(prime2).
-export([is_prime/1]).
-include_lib("eunit/include/eunit.hrl").

run_test()->
    ?assertEqual(is_prime(2), true),
    ?assertEqual(is_prime(5), true),
    ?assertEqual(is_prime(4), false),
    ?assertEqual(is_prime(439), true),
    ?assertEqual(is_prime(0), false),
    ?assertEqual(is_prime(1), false),
    ?assertEqual(is_prime(3), true),
    ?assertEqual(is_prime(907), true),
    ?assertEqual(is_prime(811),true),
    ?assertEqual(is_prime(991), true),
    ?assertEqual(is_prime(999), false).

is_prime(2)->
    true;
is_prime(N) when N < 2 ->
    false;
is_prime(N) ->
    Div = round(math:sqrt(N)),
    is_prime(N, Div, N rem Div).
is_prime(_, _, 0) ->
    false;
is_prime(_, 2, _) -> 
    true;
is_prime(N, Div, _) ->
    Div2 = Div -1,
    is_prime(N, Div2, N rem Div2).
