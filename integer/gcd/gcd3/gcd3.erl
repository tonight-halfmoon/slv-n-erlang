-module(gcd3).
-export([gcd3/3]).
-import(gcd, [gcd/2]).
-include_lib("eunit/include/eunit.hrl").

gcd3(0, 0, 0) ->
    infinity;
gcd3(X, X, 0) ->
    abs(X);
gcd3(X, X, X) ->
    abs(X);
gcd3(X, Y, Z) ->
    gcd:gcd(X, gcd:gcd(Y,Z)).

%%% eunit:test([gcd3, gcd3_tests], [verbose]).

gcd3_same_integers_and_zero_test() ->
    ?assertEqual(6 , gcd3(6,6,0)).

gcd3_zero_and_same_integers_test() ->
    ?assertEqual(6 , gcd3(0,6,6)).

gcd3_integer_zero_and_same_integer_test() ->
    ?assertEqual(6 , gcd3(6,0,6)).

gcd3_same_ng_integers_and_zero_test() ->
    ?assertEqual(6 , gcd3(-6,-6, 0)).

gcd3_zero_and_any_ng_integer_test() ->
    ?assertEqual(6 , gcd3(0,-6, -6)).

gcd3_6_3_3_test() ->
    ?assertEqual(3 , gcd3(6,3,3)).

gcd3_3_6_3_test() ->
    ?assertEqual(3 , gcd3(3,6,3)).

gcd3_of_zeros_test() ->
    ?assertEqual(infinity, gcd3(0,0,0)).
