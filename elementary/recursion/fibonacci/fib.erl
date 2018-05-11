% 28th Dec, 15; Khobar, SA
-module(fib).
-export([fibo/1]).
-export([fib/1]).

fibo(N) -> 
    tail_fib(N, 1, 0).

tail_fib(0, _, Acc)->
    Acc;
tail_fib(1, Acc, _) ->
    Acc;
tail_fib(N, Acc, Acc2) when N >= 2 ->
    tail_fib(N-1, Acc+Acc2, Acc).
    

fib (0)->
    0;
fib (1) ->
    1;
fib (N) when N >= 2 -> 
    fib(N-1) + fib(N-2).


