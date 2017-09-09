-module(gcd).
-export([main/0, gcd/2]).
-import(mod, [mod/2]).
-include_lib("eunit/include/eunit.hrl").

gcd(0,0) ->
    infinity;
gcd(X, 0) ->
    abs(X);
gcd(X, X) ->
    abs(X);
%%gcd(X,Y) when X < Y -> if two minus integers -> infinit loop
%%    gcd(Y,X);
gcd(X, Y) ->  % when X > Y ->
    gcd(Y, mod(X, Y)).

main() ->
    {ok, {X,Y}} = io:fread("X, Y> ", "~d~d"),
    io:fwrite("gcd(~w,~w):= ~w~n", [X,Y,gcd(X,Y)]),
    true.

%%% eunit:test({inparallel, [gcd, gcd_tests]}).
%%% eunit:test({inparallel, [gcd, gcd_tests]}, [verbose]).
%%% eunit:test([gcd, gcd_tests], [{report,{eunit_surefire,[{dir,"."}]}}]).

gcd_any_integer_and_zero_test_() ->
    {"gcd(6,0) must yeild in 6", ?_assertEqual(6 , gcd(6,0))}.

gcd_any_ng_integer_and_zero_test_() ->
   {"gcd(-6,0) must yield in 6", ?_assertEqual(6 , gcd(-6,0))}.

gcd_zero_and_any_ng_integer_test_() ->
    {"gcd(0,-6) must yield in 6", ?_assertEqual(6 , gcd(0,-6))}.

gcd_6_3_test_() ->
    {"gcd(6,3) must yield in 3", ?_assertEqual(3 , gcd(6,3))}.

gcd_3_2_test_() ->
    {"gcd(3,2) must yield in 1", ?_assertEqual(1 , gcd(3,2))}.

gcd_2_3_test_() ->
    {"gcd(2,3) must yield in 1", ?_assertEqual(1 , gcd(2,3))}.

gcd_of_zeros_test_() ->
    {"gcd(0,0) must yield in infinity", ?_assertEqual(infinity, gcd(0,0))}.

gcd_of_two_negs_test_() ->
    {"'gcd(-12393, -921)' must yield in '3'", ?_assertEqual(3, gcd(-12393,-921))}.

gcd_of_two_negs_big_test_() ->
    {"gcd(-876543219988099, -1876999999998) must yield in '59'", ?_assertEqual(59, gcd(-876543219988099,-1876999999998))}.
