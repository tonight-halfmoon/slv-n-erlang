-module(fib_tail).
-export([main/0]).

main() ->
   {ok, [N]} = io:fread("", "~d"),
    io:fwrite("~w~n", [fib(N)]),
    ok.

fib(N) ->
   fib(N, 1, 0).

fib(0, _F1, F0) -> F0;
fib(1, F1, _F0) -> F1;
fib(N, F1, F0) -> fib(N-1, F1+F0, F1).

