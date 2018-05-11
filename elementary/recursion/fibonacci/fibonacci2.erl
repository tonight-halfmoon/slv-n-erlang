-module(fibonacci2).
-export([fib/1]).
-include_lib("eunit/include/eunit.hrl").

fib_test()->
    F0 = 0,
    F1 = 1,
    F2 = 1,
    F3 = 2,
    F4 = 3,
    F5 = 5,
    F6 = 8,
    F7 = 13,
    ?assertEqual(F0, fib(0)),
    ?assertEqual(F1, fib(1)),
    ?assertEqual(F2, fib(2)),
    ?assertEqual(F3, fib(3)),
    ?assertEqual(F4, fib(4)),
    ?assertEqual(F5, fib(5)),
    ?assertEqual(F6,fib(6)),
    ?assertEqual(F7, fib(7)).

fib(N) -> fib(N, 1, 0).
fib(0, _, Fib) -> Fib;
fib(N, F1, F0) -> fib(N-1, F0, F1+F0).

