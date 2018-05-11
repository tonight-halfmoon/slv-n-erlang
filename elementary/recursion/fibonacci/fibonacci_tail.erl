-module(fibonacci_tail).
-export([tail_recursive_fib/1]).

tail_recursive_fib(N) -> tail_recursive_fib(N, 0, 1).

tail_recursive_fib(0, Acc1, Acc2) -> Acc1;
tail_recursive_fib(1, Acc1, Acc2) -> Acc2;
tail_recursive_fib(N, Acc1, Acc2) when N > 1 -> tail_recursive_fib(N - 1, Acc2, Acc1+ Acc2). 