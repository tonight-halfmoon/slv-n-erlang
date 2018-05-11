-module(fibonacci).
-export([fib/1]).

fib(N) when N =:= 0 -> 0;
fib(N) when N =:= 1 -> 1;
fib(N) when N =:= 2 -> 1;
fib(N) when N >= 2 -> fib(N -1) + fib(N -2).