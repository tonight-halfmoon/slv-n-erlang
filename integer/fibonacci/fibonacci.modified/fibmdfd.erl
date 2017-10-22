%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2017, 
%%% @doc
%%% Problems Statement 
%%% Modify Fibonacci definition to implement the following sequence:
%%% Ti+2 = Ti + (Ti+1)^2
%%% 
%%% The first two terms of the sequece are T1 = 0 and T2 = 1, and the modified 
%%% Fibonacci sequence becomes {0, 1, 1, 2, 5, 27, ...}.
%%%
%%% Constraints:
%%% 0 =< T1, T2 =< 2
%%% 3 =< N =< 20
%%% Tn may far exceed the range of a 64-bit integer.
%%%
%%% @end
%%% In 2015 by  <rosemary@SCUBA>

-module(fibmdfd).
-export([main/0, fib_mdfd/3]).
-import(math, [pow/2]).
-include_lib("eunit/include/eunit.hrl").

fib_mdfd_test() ->
    ?assertEqual(26, fib_mdfd(4,1,4)).
fib_mdfd_0_1_5_test() ->
    ?assertEqual(5, fib_mdfd(0, 1, 5)).
fib_mdfd_0_1_6_test() ->
    ?assertEqual(27, fib_mdfd(0, 1, 6)).
fib_mdfd_0_1_10_test() ->
    ?assertEqual(84266613096281243382112, fib_mdfd(0,1,10)).

fib_mdfd(_F1, F2, 2) -> 
    F2;
fib_mdfd(F1, F2, N) -> 
    fib_mdfd(F2, pow2(F2) + F1,  N - 1).

pow2(X) -> 
    X * X.
main() ->
    {ok, [A, B, N]} = io:fread("", "~d~d~d"),
    io:fwrite("~w~n", [fib_mdfd(A, B, N)]),
    ok.

